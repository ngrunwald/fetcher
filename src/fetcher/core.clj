(ns fetcher.core
  (:import
   (org.apache.commons.io IOUtils)
   (org.apache.http
    HttpRequest HttpEntityEnclosingRequest HttpResponse Header)
   (org.apache.http.util EntityUtils)
   (org.apache.http.entity ByteArrayEntity)
   (org.apache.http.client HttpClient RedirectStrategy)
   (org.apache.http.client.methods
	    HttpGet HttpHead HttpPut HttpPost
	    HttpDelete HttpUriRequest)
   (org.apache.http.client.params       
    CookiePolicy ClientPNames )
   (org.apache.http.params CoreConnectionPNames)
	   (org.apache.http.impl.client
	    DefaultHttpClient
	    DefaultRedirectStrategy
	    DefaultHttpRequestRetryHandler)
	   (org.apache.http.params
	    BasicHttpParams HttpConnectionParams HttpParams HttpProtocolParams)
	   (org.apache.http.conn.scheme PlainSocketFactory Scheme
					SchemeRegistry SocketFactory)
	   (org.apache.http.impl.client DefaultRedirectStrategy)
	   (org.apache.http.client.methods HttpUriRequest)
	   (org.apache.http.conn.ssl SSLSocketFactory)
	   (org.apache.http.impl.conn.tsccm ThreadSafeClientConnManager)	   
	   (java.util.concurrent TimeUnit)
	   (java.net URL)
	   [java.security.cert X509Certificate]
	   [javax.net.ssl  SSLContext TrustManager])
  (:require [clojure.contrib.logging :as log]
	    [fetcher.core :as core]
	    [fetcher.util :as util])
  (:use [html-parse.parser :only [dom elements attr-map head]]
	[plumbing.error :only [-?>]]
	[clojure.string :only [split trim join]]))

;; (class (into-array X509Certificate []))


(defn parse-url [^String url]
  (let [url-parsed (URL. url)
	ref (.getRef url-parsed)
	uri (.getPath url-parsed)]
    {:scheme (.getProtocol url-parsed)
     :server-name (.getHost url-parsed)
     :server-port (let [p (.getPort url-parsed)]
		    (when (> p 0) p))
     :uri (if (and ref (.startsWith ref "!"))
	    (str uri "#" ref)
	    uri)
     :query-string (.getQuery url-parsed)}))

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
    (-> content
	 attr-map
	 :content
	 (split #"=")
	 last
	 trim)))

(defn charset-html5 [meta]
  (if-let [content (first (filter #(:charset (attr-map %))
				  meta))]
    (-> content
	 attr-map
	 :charset
	 trim)))

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

(defn encoded [{:keys [^java.io.InputStream body] :as resp}]
  (let [bytes (IOUtils/toByteArray body)
	^String cs (charset (assoc resp :body (String. bytes "UTF-8")))]
    (String. bytes cs)))

(defn consume-response [consume {:keys [body] :as resp}]
  (cond
   (instance? java.io.InputStream body)
   (.close ^java.io.InputStream body)
   (string? body)
   (consume resp)))

(defn output-coercion
  [as {:keys [headers ^java.io.InputStream body] :as resp}]
  (if (not (instance? java.io.InputStream body))
    resp
    (try
      (assoc resp
	:body (case (or as :string)
		    :input-stream body
		    :byte-array (IOUtils/toByteArray body)
		    :string (let [^String h (headers "content-type")
				  render-string? (or (not h)
						     (.contains h "html")
						     (.contains h "xml")
						     (.contains h "text")
						     (.contains h "json")
						     (.contains h "charset"))]
			      (if render-string?
				(encoded resp)
				body))))
      (finally (when-not (= as :input-stream) (.close body))))))

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
		  (join ", " (map name accept-encoding))))
    req))

(defn query-params
  [{:keys [query-params] :as req}]
  (if query-params
    (-> req (dissoc :query-params)
	(assoc :query-string
	  (join "&"
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

(defn ensure-parsed-url [req]
  (cond
   (string? req) (parse-url req)
   (:url req) (-> req
		  (merge (parse-url (:url req)))
		  (dissoc :url))
		      
   :default req))

(defn path-redirect-strategy
  "Returns a RedirectStrategy that stores the redirect path in the provided
   atom as a nested vector of the form [[status-code-0 url-0] ... [status-code-n url-n]]."
  [a]
  (proxy [DefaultRedirectStrategy] []
    (^HttpUriRequest getRedirect [^org.apache.http.HttpRequest request
                                  ^org.apache.http.HttpResponse response
                                  ^org.apache.http.protocol.HttpContext context]
                     (let [^HttpUriRequest redirect-req (proxy-super
                                                         getRedirect
                                                         request response context)
                           status (-> response
                                      .getStatusLine
                                      .getStatusCode
                                      str
                                      keyword)
                           redirect-url (-> redirect-req
                                            .getRequestLine
                                            .getUri)]
                       (swap! a conj [status redirect-url])
                       redirect-req))))

(def default-params
  {ClientPNames/COOKIE_POLICY CookiePolicy/BROWSER_COMPATIBILITY
   ClientPNames/HANDLE_REDIRECTS true
   ClientPNames/MAX_REDIRECTS 10   
   ClientPNames/ALLOW_CIRCULAR_REDIRECTS true
   ClientPNames/REJECT_RELATIVE_REDIRECT false})

;; CoreConnectionPNames.SO_TIMEOUT
;; CoreProtocolPNames.USER_AGENT
;; CoreConnectionPNames.SOCKET_BUFFER_SIZE
;; CoreConnectionPNames.CONNECTION_TIMEOUT
(defn config-client
  [^HttpClient client 
   {:keys [params num-retries ^RedirectStrategy redirect-strategy timeout]	   
    :or {num-retries 1 timeout 30000}}]
  (let [^HttpParams client-params (.getParams client)]
    (doseq [[pk pv] params]
      (.setParameter client-params pk pv))
    (.setIntParameter client-params CoreConnectionPNames/CONNECTION_TIMEOUT
		      (int timeout))
    (.setIntParameter client-params CoreConnectionPNames/SO_TIMEOUT
		      (int timeout)))
  (doto client
    (.setRedirectStrategy redirect-strategy)
    (.setHttpRequestRetryHandler (DefaultHttpRequestRetryHandler. (int num-retries) true))))


(defn pooled-http-client
  "A threadsafe, single client using connection pools to various hosts."
  ([] (pooled-http-client {:ttl 120
                           :max-total-conns 1000
                           :max-per-route 1000
                           :params default-params
                           :redirect-strategy (DefaultRedirectStrategy.)}))
  ([{:keys [ttl max-total-conns max-per-route]}]

     (let [psf (PlainSocketFactory/getSocketFactory)
           ssf (SSLSocketFactory/getSocketFactory)
           schemes (doto (SchemeRegistry.)
                     (.register (Scheme. "http" psf 80))
                     (.register (Scheme. "https" ssf 443)))
           mgr (doto (ThreadSafeClientConnManager. schemes (long ttl) TimeUnit/SECONDS)
                 (.setMaxTotal max-total-conns)
                 (.setDefaultMaxPerRoute max-per-route))]
       (config-client (DefaultHttpClient. mgr)
		      {:params default-params
		       :redirect-strategy (DefaultRedirectStrategy.)}))))

(def no-op-trust-manager
     (reify  javax.net.ssl.X509TrustManager
	     (^void checkClientTrusted [this ^"[Ljava.security.cert.X509Certificate;" xcs ^String s])
	     (^void checkServerTrusted [this ^"[Ljava.security.cert.X509Certificate;" xcs ^String s])
	     (getAcceptedIssuers [this])))

(defn wrap-ignore-ssl [^HttpClient client]  
  (let [ctx (doto (SSLContext/getInstance "TLS")
	      (.init nil ^"[Ljavax.net.ssl.TrustManager;"
		            (into-array TrustManager [no-op-trust-manager])
			    nil))
	ssf (doto (SSLSocketFactory. ctx)
	      (.setHostnameVerifier (SSLSocketFactory/ALLOW_ALL_HOSTNAME_VERIFIER)))
	ccm (.getConnectionManager client)
	sr (.getSchemeRegistry ccm)]
    (.register sr (Scheme. "https" ssf 443))
    (DefaultHttpClient. ccm (.getParams client))))

(defn basic-http-client
  ([] (basic-http-client {:params default-params
                          :redirect-strategy (DefaultRedirectStrategy.)}))
  ([conf]
     (config-client (DefaultHttpClient.) conf)))

(defn- parse-headers [^HttpResponse http-resp]
  (into {} (map (fn [^Header h] [(.toLowerCase (.getName h)) (.getValue h)])
                (iterator-seq (.headerIterator http-resp)))))

(defn strip-query-string [query-string]
  (when query-string
    (join  "&"
	     (sort
	      (remove
	       (fn [^String x] (.startsWith x "utm_"))
	       (split query-string #"&" ))))))

(defn build-url [{:keys [scheme,
			 server-name,
			 server-port
			 uri,
			 query-string]}]
  (str scheme "://" server-name
       (when server-port
	 (str ":" server-port))
       uri
       (when (and query-string
		  (not (.isEmpty ^String query-string)))
	 (str "?" query-string))))

(defn resolved-url [{:keys [redirects,url] :as fetched}]
  (let [resolved (or
		  (->> redirects
			(filter (comp #{:301 :302 :303} first))
			last
			second)
		  url)]
     (build-url
      (update-in (parse-url resolved)
		 [:query-string] strip-query-string))))

(defn add-headers [^HttpRequest http-req
		   {:keys [headers content-type character-encoding]}]
  (when content-type
    (.addHeader http-req "Content-Type"
		(if-not character-encoding
		  content-type
		  (str content-type
		       "; charset=" character-encoding))))
  (.addHeader http-req "Connection" "close")
  (doseq [[header-n header-v] headers]
    (.addHeader http-req header-n header-v))
  http-req)

(defn add-request-body [^HttpEntityEnclosingRequest http-req
			body]
  (when body
    (.setEntity http-req (ByteArrayEntity. ^bytes body)))
  http-req)

(defn create-request [{:keys [request-method headers
			      content-type character-encoding body]
		       :as components}]
  (let [^String url (build-url components)]
    [url
     (->  (case request-method
		:get    (HttpGet. url)
		:head   (HttpHead. url)
		:put    (HttpPut. url)
		:post   (HttpPost. url)
		:delete (HttpDelete. url))
	  (add-headers components)
	  (add-request-body body))]))

(defn request
    "Executes the HTTP request corresponding to the given Ring request map and
   returns the Ring response map corresponding to the resulting HTTP response."
    ([config]
       (request (basic-http-client) config))
    ([^HttpClient http-client
      {:keys [request-method
	      headers content-type character-encoding body]
       :as components}]
       (let [[http-url ^HttpRequest http-req]
	       (create-request components)
	       redirects (atom [])
	       ^RedirectStrategy redirect-strategy
	       (path-redirect-strategy redirects)]	
	 (.setRedirectStrategy http-client redirect-strategy)	 
	   (try
	        (let [^HttpResponse http-resp (.execute http-client http-req)]
		  {:status (.getStatusCode (.getStatusLine http-resp))
		   :headers (parse-headers http-resp)
		   :body  (when-let [ent (.getEntity http-resp)]
			    (.getContent ent))		
		   :url http-url
		   :redirects @redirects})		
	     (catch Exception e
	       (.abort http-req)
	       (throw e))))))

(defn build-request [method  url-or-req]
  (-> url-or-req
      ensure-parsed-url
      (merge {:accept-encoding
	      (when-not (and (map? url-or-req) (:no-gzip? url-or-req))
		gzip)
	      :request-method method})
      content-type
      basic-auth
      wrap-accept-encoding
      accept
      query-params
      basic-auth
      input-coercion))

(defn fetch
  ([method url]
     (fetch #(basic-http-client)
	    method
	    url))
  ([get-client method url-or-req]
     (let [{:keys [as] :as req } (build-request method url-or-req)]
       (->> req
	    (request (get-client))
	    decompress
	    (output-coercion as)))))