(ns fetcher.client
  "Batteries-included HTTP client."
  (:use [clojure.xml :only [parse]])
  (:require [clojure.contrib.string :as str])
  (:require [fetcher.core :as core]
            [clojure.zip :as zip])
  (:require [fetcher.util :as util])
  (:require [clojure.contrib.zip-filter.xml :as xml-zip])
  (:import (java.net URL)
	   (org.apache.commons.io IOUtils))
  (:refer-clojure :exclude (get))
  (:use [plumbing.core :only [-?>]]
	[clojure.string :only [split trim]]))

(defn if-pos [v]
  (if (and v (pos? v)) v))

(defn parse-url [url]
  (let [url-parsed (URL. url)]
    {:scheme (.getProtocol url-parsed)
     :server-name (.getHost url-parsed)
     :server-port (if-pos (.getPort url-parsed))
     :uri (.getPath url-parsed)
     :query-string (.getQuery url-parsed)}))

(defn ensure-proper-url
  [loc default-protocol default-host-port]
  (cond (.startsWith loc "/")
        (format "%s://%s%s" default-protocol default-host-port loc)
        
        (not (.startsWith loc "http"))
        (format "%s://%s/%s" default-protocol default-host-port loc)

        :default
        loc))

(defn host-and-port
  "Host and port string in the format <host>:<port>.  If no port
   is given then <host> is returned."
  [{:keys [server-name server-port]}]
  (if (and server-port (pos? server-port))
    (format "%s:%s" server-name server-port)
    server-name))

(defn redirect-req [req resp]
  (let [url (ensure-proper-url
             (get-in resp [:headers "location"])
             (:scheme req)
             (host-and-port req))]
    (merge req (parse-url url))))

(defn redirect [client req resp]
  (let [{:keys [request-method]} req
	{:keys [status]} resp]
    (cond
     (and (#{301 302 307} status) (#{:get :head} request-method))
     (client 
      (redirect-req req resp))
     (and (= 303 status) (= :head request-method))
     (client
      (redirect-req (assoc req :request-method :get) resp))
     :else
     resp)))

(defn- chunk-seq
  "lazy sequence of input-streams for each of the chunks encoded in the
   chunk input-stream. Assumes input-stream is using chunked http transport."
  [^java.io.InputStream is]
  (let [when-done #(do (.close is))
	r (-> is java.io.InputStreamReader. java.io.BufferedReader.)]
    (take-while identity
      (repeatedly
       #(let [line (.readLine r)]
	  (if (or (nil? line)
		  (.isEmpty line))
	    (do (when-done) nil)
	     (let [size (Integer/decode (str "0x" line))
		   char-data (util/read-bytes r size)]	       
	       (if (zero? size)
		 (do (when-done) nil)
		 (let [chunk (String. char-data 0 size)]
		   (.readLine r) ;after chunk line terminator
		   (-> (.getBytes chunk "UTF-8") java.io.ByteArrayInputStream.))))))))))

(defn output-coercion [req resp]
  (let [{:keys [as,chunked?]
         :or {as :string chunked? false}} req
         {:keys [headers,body]} resp
         chunked? (and chunked?
                       (= (clojure.core/get headers "transfer-encoding") "chunked"))
         as-fn (fn [^java.io.InputStream is]
                 (case as 
                       :byte-array (IOUtils/toByteArray is)
                       :string (String. (IOUtils/toByteArray is) "UTF-8")))]
    (-> resp 
        (update-in [:body]
                   (fn [is]
                     (if (not (instance? java.io.InputStream is))
                       is
                       (if chunked?
                         (map as-fn (chunk-seq is))
                         (let [r (as-fn is)]
                           r))))))))

(defn input-coercion
  [{:keys [body] :as req}]
  (if (string? body)
    (-> req (assoc :body (util/utf8-bytes body)
			    :character-encoding "UTF-8"))
    req))

(defn decompress
  [resp]
  (case (get-in resp [:headers "content-encoding"])
	"gzip"
	(update-in resp [:body]
               (fn [^java.io.InputStream is]
                 (java.util.zip.GZIPInputStream. is)))
	"deflate"
	(update-in resp [:body]
               (fn [^java.io.InputStream is]
                 (java.util.zip.InflaterInputStream. is)))
	resp))

(def gzip ["gzip" "deflate"])

(defn accept-encoding 
  [{:keys [accept-encoding] :as req}]
  (if accept-encoding
    (-> req (dissoc :accept-encoding)
		 (assoc-in [:headers "Accept-Encoding"]
			   (str/join ", " (map name accept-encoding))))
    req))

(defn query-params
 [{:keys [query-params] :as req}]
  (if query-params
     (-> req (dissoc :query-params)
		 (assoc :query-string
		   (str/join "&"
    (map (fn [[k v]] (str (util/url-encode (name k)) "="
                          (util/url-encode (str v))))
         query-params))))
     req))

(defn basic-auth [req]
  (if-let [[user password] (:basic-auth req)]
    (-> req (dissoc :basic-auth)
	(assoc-in [:headers "Authorization"]
		  (str "Basic "
       (util/base64-encode (util/utf8-bytes (str user ":" password)))))))
  req)

(defn content-type-value [type]
	       (if (keyword? type)
    (str "application/" (name type))
    type))

(defn accept
 [{:keys [accept] :as req}]
  (if accept
     (-> req (dissoc :accept)
		 (assoc-in [:headers "Accept"]
			   (content-type-value accept)))
     req))

(defn content-type [req]
  (if (not (:content-type req)) req
    (update-in req [content-type]
	       content-type-value)))

(defn xml-root [body]
  (-> body
      (.getBytes "UTF-8")
      java.io.ByteArrayInputStream.
      parse
      zip/xml-zip))

(defn charset
  "Get charset from meta tag."
  [body]
  (let [root (xml-root body)
	meta (xml-zip/xml1-> root
				:head
				:meta)]
    (if-let [content (first (filter #(= "Content-Type"
					(:http-equiv (:attrs %)))
				    meta))]
      (->> content
	   :attrs
	     :content
	     (str/split #"=")
	     last
	     str/trim))))

(defn strip-punc [s]
  (let [strip
	(some identity (map #(.endsWith s %)
			    [";" ":" "." ","]))]
    (if strip (.substring s 0 (- (.length s) 1)))))

(defn charset-body
 [resp]
 (let [{:keys [headers body]} resp
       content-type (headers "content-type")]
   (if (and content-type
	    (.startsWith content-type "text/html"))
      (let [b (IOUtils/toByteArray body)
	    charset (or (-?> (headers "content-type")
			     (split #"=")
			     second
			     strip-punc
			     trim)
			(charset (String. b "UTF-8"))
			"UTF-8")]
	(assoc resp :body (String. b charset)))
      resp)))


(defn request
  ([method url] (request #(core/basic-http-client)
                         method
                         url))
  ([client-pool method url]
     (let [req (-> (if (map? url) url (parse-url url))
                   (merge {:request-method method
                           :accept-encoding gzip})
                   content-type
                   basic-auth
                   accept-encoding
                   accept
                   query-params
                   basic-auth
                   input-coercion)
           resp (->> (core/request (client-pool) req)
                     decompress
		     charset-body
                     (output-coercion req))]
       resp)))