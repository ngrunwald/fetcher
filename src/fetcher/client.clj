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


(def unexceptional-status?
  #{200 201 202 203 204 205 206 207 300 301 302 303 307})

(defn wrap-exceptions [client]
  (fn [req]
    (let [{:keys [status] :as resp} (client req)]
      (if (or (not (clojure.core/get req :throw-exceptions true))
              (unexceptional-status? status))
        resp
        (throw (Exception.
		(format "Error status: %d. More details\n%s" status
			(String. (IOUtils/toByteArray (:body resp))))))))))


(defn follow-redirect [client req resp]
  (let [url (get-in resp [:headers "location"])]
    (client (merge req (parse-url url)))))

(defn wrap-redirects [client]
  (fn [{:keys [request-method] :as req}]
    (let [{:keys [status] :as resp} (client req)]
      (cond
        (and (#{301 302 307} status) (#{:get :head} request-method))
          (follow-redirect client req resp)
        (and (= 303 status) (= :head request-method))
          (follow-redirect client (assoc req :request-method :get) resp)
        :else
          resp))))

(defn wrap-decompression [client]
  (fn [req]
    (if (get-in req [:headers "Accept-Encoding"])
      (client req)
      (let [req-c (update-in req [:headers] assoc "Accept-Encoding" "gzip, deflate")
            resp-c (client req)]
        (case (get-in resp-c [:headers "Content-Encoding"])
          "gzip"
	  (update-in resp-c [:body]
	     (fn [^java.io.InputStream is]
	       (java.util.zip.GZIPInputStream. is)))
          "deflate"
	  (update-in resp-c [:body]
		     (fn [^java.io.InputStream is]
		       (java.util.zip.InflaterInputStream. is)))
          resp-c)))))

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

(defn wrap-output-coercion [client]
  (fn [{:keys [as,chunked?] :as req
        :or {as :string chunked? false}}]
    (let [resp (client req)
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
                             r)))))))))


(defn wrap-input-coercion [client]
  (fn [{:keys [body] :as req}]
    (if (string? body)
      (client (-> req (assoc :body (util/utf8-bytes body)
                             :character-encoding "UTF-8")))
      (client req))))


(defn content-type-value [type]
  (if (keyword? type)
    (str "application/" (name type))
    type))

(defn wrap-content-type [client]
  (fn [{:keys [content-type] :as req}]
    (if content-type
      (client (-> req (assoc :content-type
                        (content-type-value content-type))))
      (client req))))

(defn wrap-accept [client]
  (fn [{:keys [accept] :as req}]
    (if accept
      (client (-> req (dissoc :accept)
                      (assoc-in [:headers "Accept"]
                        (content-type-value accept))))
      (client req))))

(defn accept-encoding-value [accept-encoding]
  (str/join ", " (map name accept-encoding)))

(defn wrap-accept-encoding [client]
  (fn [{:keys [accept-encoding] :as req}]
    (if accept-encoding
      (client (-> req (dissoc :accept-encoding)
                      (assoc-in [:headers "Accept-Encoding"]
                        (accept-encoding-value accept-encoding))))
      (client req))))

(defn generate-query-string [params]
  (str/join "&"
    (map (fn [[k v]] (str (util/url-encode (name k)) "="
                          (util/url-encode (str v))))
         params)))

(defn wrap-query-params [client]
  (fn [{:keys [query-params] :as req}]
    (if query-params
      (client (-> req (dissoc :query-params)
                      (assoc :query-string
                             (generate-query-string query-params))))
      (client req))))

(defn basic-auth-value [user password]
  (str "Basic "
       (util/base64-encode (util/utf8-bytes (str user ":" password)))))

(defn wrap-basic-auth [client]
  (fn [req]
    (if-let [[user password] (:basic-auth req)]
      (client (-> req (dissoc :basic-auth)
                      (assoc-in [:headers "Authorization"]
                        (basic-auth-value user password))))
      (client req))))

(defn wrap-method [client]
  (fn [req]
    (if-let [m (:method req)]
      (client (-> req (dissoc :method)
                      (assoc :request-method m)))
      (client req))))

(defn wrap-url [client]
  (fn [req]
    (if-let [url (:url req)]
      (client (-> req (dissoc :url) (merge (parse-url url))))
      (client req))))

(defn wrap-client [client http-client]
  (fn [req]
    (client http-client req)))

(defn wrap-request
  "Returns a battaries-included HTTP request function coresponding to the given
   core client. See client/client."
  [request]
  (-> request
      (wrap-client (core/pooled-http-client))
      wrap-redirects
      wrap-exceptions
      wrap-decompression
      wrap-input-coercion
      wrap-output-coercion
      wrap-query-params
      wrap-basic-auth
      wrap-accept
      wrap-accept-encoding
      wrap-content-type
      wrap-method
      wrap-url))

(def #^{:doc
  "Executes the HTTP request corresponding to the given map and returns the
   response map for corresponding to the resulting HTTP response.

   In addition to the standard Ring request keys, the following keys are also
   recognized:
   * :url
   * :method
   * :query-params
   * :basic-auth
   * :content-type
   * :accept
   * :accept-encoding
   * :as

  The following additional behaviors over also automatically enabled:
   * Exceptions are thrown for status codes other than 200-207, 300-303, or 307
   * Gzip and deflate responses are accepted and decompressed
   * Input and output bodies are coerced as required and indicated by the :as
     option."}
  request
  (wrap-request #'core/request))


(defn get
  "Like #'request, but sets the :method and :url as appropriate."
  [url & [req]]
  (request (merge req {:method :get :url url})))

(defn head
  "Like #'request, but sets the :method and :url as appropriate."
  [url & [req]]
  (request (merge req {:method :head :url url})))

(defn post
  "Like #'request, but sets the :method and :url as appropriate."
  [url & [req]]
  (request (merge req {:method :post :url url})))

(defn put
  "Like #'request, but sets the :method and :url as appropriate."
  [url & [req]]
  (request (merge req {:method :put :url url})))

(defn delete
  "Like #'request, but sets the :method and :url as appropriate."
  [url & [req]]
  (request (merge req {:method :delete :url url})))

(defn strip-punc [s]
  (let [strip
	(some identity (map #(.endsWith s %)
			    [";" ":" "." ","]))]
    (if strip (.substring s 0 (- (.length s) 1)))))

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

;; (defn wrap-html-body
;;   [client]
;;   (fn [req]
;;     (let [{:keys [headers body] :as resp} (client req)]
;;       (if (.startsWith (headers "content-type") "text/html")
;;         (let [b (IOUtils/toByteArray body)
;;               charset (or (-?> (headers "content-type")
;;                                (split #"=")
;;                                second
;; 			       strip-punc
;;                                trim)
;;                           (charset (String. b "UTF-8"))
;;                           "UTF-8")]
;;           (assoc resp :body (String. b charset)))
;;         resp))))

;; (defn wrap-request
;;   [request]
;;   (-> request
;;       (clj-http.client/wrap-client (clj-http/pooled-http-client))
;;       clj-http.client/wrap-redirects
;;       clj-http.client/wrap-exceptions
;;       clj-http.client/wrap-decompression
;;       clj-http.client/wrap-query-params
;;       clj-http.client/wrap-basic-auth
;;       clj-http.client/wrap-accept
;;       clj-http.client/wrap-accept-encoding
;;       clj-http.client/wrap-content-type
;;       clj-http.client/wrap-method
;;       clj-http.client/wrap-url
;;       wrap-html-body))

;; (def request (wrap-request clj-http/request))