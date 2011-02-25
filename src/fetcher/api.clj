(ns fetcher.api
  (:require [clojure.string :as str])
  (:import
     [org.apache.http  Header HttpRequest]
     [org.apache.http.client  HttpClient]
     [org.apache.http.message BasicHeader]
     [org.apache.http.client.methods HttpRequestBase]
     [org.apache.http.impl.client DefaultHttpClient]))

;; request
;; can either be a string for a url
;; or 

;; response
;; {:keys [status, body, headers]}
;; the nature of body differs depending
;; on whether the underlying fetcher is synchronous
;; or not.

(defn- build-query-params [query-map]
  (str "?" (str/join "&"
     (map (fn [[k v]]
	    (str (if (keyword? k) (.substring (str k) 1) k) "=" v)) query-map))))

(defn- build-request-str [request]
  (if (string? request) request
    (let [{:keys [url, query-params, port]} request]
      (str url
	   (when port (str ":" port))
	   (when query-params (build-query-params query-params))))))

(defn- ^HttpRequest
  build-http-request
  [method request]
  (let [get-headers (fn [f]
		      (->> (:headers request)
			   (filter (fn [[k _]] (f k)))
			   (map (fn [[k v]] (BasicHeader.
					     (.substring (str k) 1) v)))))]
    (proxy  [HttpRequestBase] []
     (getMethod [] method)
     (getURI [] (java.net.URI. (build-request-str request)))
     (getFirstHeader [^String name]
       (into-array Header (first (get-headers #(= (keyword name) %)))))
     (getLastHeader [^String name]
       (into-array Header (last (get-headers #(= (keyword name) %)))))
     (getHeaders [^String name]
       (into-array Header (get-headers #(= (keyword name) %))))
     (getAllHeaders [] (into-array Header (get-headers (constantly true)))))))

(defprotocol ISyncFetcher
  (http-get [this request])
  (http-post [this request data])
  (http-delete [this request]))

(extend-protocol ISyncFetcher
  HttpClient
  (http-get [this request]
     (.execute this (build-http-request "GET" request))))

(defn basic-http-fetcher []
  (reify ISyncFetcher
	 (http-get [this req] (http-get (DefaultHttpClient.) req))))


(comment (def f (DefaultHttpClient.))
	 (.execute (DefaultHttpClient.) (build-http-request "GET" "http://aria42.com"))
	 (http-get (basic-http-fetcher) "http://aria42.com"))