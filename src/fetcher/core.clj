(ns fetcher.core
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

(defn with-pre
  "Preprocess arguments with pre and apply the processed arguments to the given functions."
  [pre & fs]
  (fn [args]
    (let [new-args (pre args)]
      (doseq [f fs]
        (f new-args)))))

(defn use-redirect
  [{h :headers u :url :as resp}]
  (let [url (wm.urls/expand-relative-url u (:location h))]
    (assoc resp :url url)))

(defn url-as-key
  [{u :url :as resp}]
  (assoc resp :key u))

(defn redirect
"dispatch table with redirect policy."
[update-fetch out & [update move]]
  (if move
    (table 
     :301 [move
	       (with-pre #(-> % use-redirect url-as-key)
             update-fetch
             update)
           (with-pre #(-> % use-redirect url-as-key)
             out)]
	 [:300 :302 :307]
	 [update-fetch
	  (with-pre use-redirect out)])
    (table
	 [:300 :301 :302 :307]
	 [update-fetch
	  (with-pre use-redirect out)])))

(defn response-table
  [update-fetch update put-ok]
  (table :200 [update-fetch update put-ok]
	 [:400 :304 :401 :410 :404 :408 :500 :503]
	 update-fetch))

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
  "Fetch a feed for updates.  Responses are handled asynchronously by the provided callback.

  The callback should accept five arguments: k, u, response code, headers, and body."
  [[k u & [headers]] put-done]
  (let [callbacks (merge async-req/*default-callbacks*
                         {:status status-check
                          :completed (with-url k u put-done)
                          :error (fn [_ t]
				   (log/error
				    (format "Error processing request for %s."
					    k) t))})
        req (async-req/prepare-request :get u  :headers headers)]
    (apply async-req/execute-request req (apply concat callbacks))))
    
(defn schedule-fetches
  "Schedule work to fetch with a frequency given in seconds."
  ([get-urls enqueue]
     (fn [] (doseq [{:keys [url last-modified etag]} (get-urls)]
	      (let [headers {:If-Modified-Since last-modified
			     :If-None-Match etag}]
		(enqueue [url url headers]))))))

(defn get-scheduled-fetches
  "Schedule work to fetch with a frequency given in seconds."
  ([urls]
     (for [{:keys [url last-modified etag]} urls
	    :let [headers {:If-Modified-Since last-modified
			   :If-None-Match etag}]]
       [url url headers])))
