(ns fetcher.client
  "Batteries-included HTTP client."
  (:use [html-parse.parser :only [dom elements attr-map head]])
  (:require [clojure.contrib.string :as str])
  (:require [fetcher.core :as core]
            [clojure.zip :as zip])
  (:require [fetcher.util :as util])
  (:require [clojure.contrib.zip-filter.xml :as xml-zip])
  (:import (org.apache.commons.io IOUtils))
  (:refer-clojure :exclude (get))
  (:use [plumbing.core :only [-?>]]
        [clojure.string :only [split trim]]))

(defn ensure-proper-url
  [^String loc default-protocol default-host-port]
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
    (merge req (core/parse-url url))))

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


(defn output-coercion [req resp]
  (let [as-fn (fn [^java.io.InputStream is]
                (case (or (:as req) :string)
                      :input-stream is
                      :byte-array (IOUtils/toByteArray is)
                      :string (String. (IOUtils/toByteArray is) "UTF-8")))]
    (-> resp 
        (update-in [:body]
                   (fn [is]
		     (cond
		      (not (instance? java.io.InputStream is)) is
		      :default (as-fn is)))))))

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
		     (when is (java.util.zip.GZIPInputStream. is))))
	"deflate"
	(update-in resp [:body]
		   (fn [^java.io.InputStream is]
		     (when is (java.util.zip.InflaterInputStream. is))))
	resp))

(def gzip ["gzip" "deflate"])

(defn wrap-accept-encoding 
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
    (-> req
        (dissoc :basic-auth)
        (assoc-in [:headers "Authorization"]
                  (str "Basic "
                       (util/base64-encode (util/utf8-bytes (str user ":" password))))))
    req))

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
  (if (not (:content-type req))
    req
    (update-in req [:content-type] content-type-value)))

(defn strip-punc [^String s]
  (let [strip (some identity (map #(.endsWith s %)
                                  [";" ":" "." ","]))]
    (if (not strip) s
        (.substring s 0 (- (.length s) 1)))))

(defn charset-headers [headers]
  (-?> (headers "content-type")
	   (split #"=")
	   second
	   strip-punc
	   trim))

(defn charset-http-equiv [meta]
  (if-let [content (first (filter #(= "Content-Type"
				      (:http-equiv (attr-map %)))
				  meta))]
    (->> content
	 attr-map
	 :content
	 (str/split #"=")
	 last
	 str/trim)))

(defn charset-html5 [meta]
  (if-let [content (first (filter #(:charset (attr-map %))
				  meta))]
    (->> content
	 attr-map
	 :charset
	 str/trim)))

(defn charset
  "Get charset from meta tag."
  [{:keys [headers body]}]
  (or (charset-headers headers)
      (let [root (dom body)
	    meta (-> root
		     head
		     (elements "meta"))]
	(or (charset-http-equiv meta)
	    (charset-html5 meta)))
      "UTF-8"))

(defn ensure-parsed-url [req]
  (cond
   (string? req) (core/parse-url req)
   (:url req) (-> req
		  (merge (core/parse-url (:url req)))
		  (dissoc :url))
		      
   :default req))

(defn request
  ([method url]
     (request #(core/basic-http-client)
              method
              url))
  ([client-pool method url
    & {:keys [accept-encoding]
       :or {accept-encoding gzip}}]
     (let [req (-> url
		   ensure-parsed-url		   
                   (merge {:request-method method :accept-encoding accept-encoding})
                   content-type
                   basic-auth
                   wrap-accept-encoding
                   accept
                   query-params
                   basic-auth
                   input-coercion)
           resp (->> (core/request ^HttpClient (client-pool) req)
                     decompress
                     (output-coercion req))]
       resp)))