(ns fetcher.core
  (:require [http.async.client :as c]
            [http.async.client.request :as async-req]
            [work.core :as work]
            [work.cache :as cache]
            [clojure.contrib.logging :as log])
  (:use fetcher.handler))

;; TODO: Add more status checks for bodies we don't care about?
(defn status-check
  "Check if status code is 304, abort if so."
  [_ status]
  (if (= 304 (:code status))
    :abort
    [status :continue]))

;;; Need a closure to capture the feed-url.
(defn dispatch-generator
  "Return a fn to handle a completed response."
  [k u response-callback]
  (fn [state]
    (let [code (-> (c/status state) :code)
          headers (c/headers state)
          body (c/string state)]
      (response-callback k
                         u
                         code
                         headers
                         body)
      [true :continue])))

(defn fetch
  "Fetch a feed for updates.  Responses are handled asynchronously by the provided callback.

  The callback should accept five arguments: k, u, response code, headers, and body."
  [[k u & [headers]] put-done]
  (let [callbacks (merge async-req/*default-callbacks*
                         {:status status-check
                          :completed (dispatch-generator k u put-done)})
        req (async-req/prepare-request :get
                                       u
                                       :headers headers)
        resp (apply async-req/execute-request
                    req
                    (apply concat callbacks))]
    resp))

(defn fetch-pool
  [get-work put-done & [error-handler]]
  (let [args [fetch
              get-work
              put-done
              (work/available-processors)
              :async]]
    (apply work/queue-work (if error-handler
                             (conj args error-handler)
                             args))))