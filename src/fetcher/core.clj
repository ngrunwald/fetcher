(ns fetcher.core
  "Core HTTP request/response implementation."
  (:import (org.apache.http HttpRequest HttpEntityEnclosingRequest HttpResponse Header))
  (:import (org.apache.http.util EntityUtils))
  (:import (org.apache.http.entity ByteArrayEntity))
  (:import (org.apache.http.client.methods HttpGet HttpHead HttpPut HttpPost HttpDelete))
  (:import (org.apache.http.client.params CookiePolicy ClientPNames))
  (:import (org.apache.http.impl.client DefaultHttpClient))
  (:import (org.apache.http.params BasicHttpParams HttpConnectionParams
                                   HttpParams HttpProtocolParams))
  (:import (org.apache.http.conn.scheme PlainSocketFactory Scheme
                                        SchemeRegistry SocketFactory))
  (:import (org.apache.http.conn.ssl SSLSocketFactory))
  (:import (org.apache.http.impl.conn.tsccm ThreadSafeClientConnManager))
  (:import (java.util.concurrent TimeUnit))
  (:use work.graph)
  (:require [http.async.client :as c]
            [http.async.client.request :as async-req]
            [webmine.urls :as wm.urls]
            [clojure.contrib.logging :as log]))

(defn status-check
  "Check if status code is 304, abort if so."
  [_ status]
  (if (= 304 (:code status))
    [status :abort]
    [status :continue]))

(defn ok? [c] (or (= c :200)
                  (= c 200)
                  (= c "200")))

(defn on-response [{s :status}]
    (-> s str keyword))

(defn with-url
  "wraps a callback in a key and url.
callback takes key, url, status code, hearders and body."
  [k u callback]
  (fn [state]
    (let [code (-> (c/status state) :code)
          headers (if (not= 304 code)
                    (c/headers state)
                    nil)
          body (when (ok? code)
                 (c/string state))]
      (callback {:key k
                 :url u
                 :status code
                 :headers headers
                 :body body})
      [true :continue])))

(defn fetch
  "Fetch a feed for updates. Responses are handled asynchronously by the provided callback.

The callback should accept five arguments: k, u, response code, headers, and body."
  [{k :key u :url headers :headers} put-done]
  (let [callbacks (merge async-req/*default-callbacks*
                         {:status status-check
                          :completed (with-url k u put-done)
                          :error (fn [_ t]
                                   (log/error
                                    (format "Error processing request for %s."
                                            k) t))})
        req (async-req/prepare-request :get u :headers headers)]
    (apply async-req/execute-request req (apply concat callbacks))))

(defn schedule-fetches
  "Schedule work to fetch with a frequency given in seconds."
  ([get-urls enqueue]
     (fn [] (doseq [{:keys [url last-modified etag]} (get-urls)]
              (let [headers {:If-Modified-Since last-modified
                             :If-None-Match etag}]
                (enqueue {:key url
                          :url url
                          :headers headers}))))))

(defn get-scheduled-fetches
  "Schedule work to fetch with a frequency given in seconds."
  ([urls]
     (for [{:keys [url last-modified etag]} urls
           :let [headers {:If-Modified-Since last-modified
                          :If-None-Match etag}]]
       {:key url
        :url url
        :headers headers})))

(defn- parse-headers [^HttpResponse http-resp]
  (into {} (map (fn [^Header h] [(.toLowerCase (.getName h)) (.getValue h)])
                (iterator-seq (.headerIterator http-resp)))))

(defn config-client
  [c]
  (doto (.getParams c)
    (.setParameter ClientPNames/COOKIE_POLICY CookiePolicy/BROWSER_COMPATIBILITY)
    (.setParameter ClientPNames/HANDLE_REDIRECTS true)
    (.setParameter ClientPNames/MAX_REDIRECTS 10)
    (.setParameter ClientPNames/ALLOW_CIRCULAR_REDIRECTS true)
    (.setParameter ClientPNames/REJECT_RELATIVE_REDIRECT false))
  c)

(defn pooled-http-client
  "A threadsafe, single client using connection pools to various hosts."
  ([] (pooled-http-client {:ttl 120
                           :max-total-conns 200
                           :max-per-route 10}))
  ([{:keys [ttl max-total-conns max-per-route]}]
     (let [psf (PlainSocketFactory/getSocketFactory)
           ssf (SSLSocketFactory/getSocketFactory)
           schemes (doto (SchemeRegistry.)
                     (.register (Scheme. "http" psf 80))
                     (.register (Scheme. "https" ssf 443)))
           mgr (doto (ThreadSafeClientConnManager. schemes (long ttl) TimeUnit/SECONDS)
                 (.setMaxTotal max-total-conns)
                 (.setDefaultMaxPerRoute max-per-route))]
       (config-client (DefaultHttpClient. mgr)))))

(defn basic-http-client
  []
  (config-client (DefaultHttpClient.)))

(defn request
  "Executes the HTTP request corresponding to the given Ring request map and
   returns the Ring response map corresponding to the resulting HTTP response."
  ([http-client {:keys [request-method scheme server-name server-port uri query-string
                        headers content-type character-encoding body]}]
     (try
       (let [http-url (str scheme "://" server-name
                           (if server-port (str ":" server-port))
                           uri
                           (if query-string (str "?" query-string)))
             ^HttpRequest
             http-req (case request-method
                            :get    (HttpGet. http-url)
                            :head   (HttpHead. http-url)
                            :put    (HttpPut. http-url)
                            :post   (HttpPost. http-url)
                            :delete (HttpDelete. http-url))]
         (if (and content-type character-encoding)
           (.addHeader http-req "Content-Type"
                       (str content-type "; charset=" character-encoding)))
         (if (and content-type (not character-encoding))
           (.addHeader http-req "Content-Type" content-type))
         (.addHeader http-req "Connection" "close")
         (doseq [[header-n header-v] headers]
           (.addHeader http-req header-n header-v))
         (if body
           (let [http-body (ByteArrayEntity. body)]
             (.setEntity ^HttpEntityEnclosingRequest http-req http-body)))
         (let [http-resp (.execute http-client http-req)
               resp {:status (.getStatusCode (.getStatusLine http-resp))
                     :headers (parse-headers http-resp)
                     :body (when-let [ent (.getEntity http-resp)]
                             (.getContent ent))}]
           resp))))
  ([config]
     (request (basic-http-client) config)))
