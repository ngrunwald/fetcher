(ns fetcher.handler
  "Some default handlers for working with fetcher response."
  (:require [clojure.java.io :as io]
            [clojure.contrib.logging :as log]
            [clj-time.format :as time-fmt]
            [clj-time.coerce :as time-coerce])
  (:import [java.util Date]))

;;; Note that the Last-Modified HTTP header is kept
;;; in RFC 822 format since it's only used for requests.
(defprotocol DateTime
  (rfc822-str [d]))

(def rfc822-fmt (time-fmt/formatter "EEE, dd MMM yyyy HH:mm:ss Z"))

(extend-type java.util.Date
  DateTime
  (rfc822-str [d] (rfc822-str (time-coerce/from-date d))))

(extend-type org.joda.time.DateTime
  DateTime
  (rfc822-str [d]
              (time-fmt/unparse rfc822-fmt d)))

(defn fetch-args
  "Return a vector of args that can be submitted to the fetch-pool
  from a given url-info map."
  [url-info]
  (let [url (url-info :url)
        headers {:If-Modified-Since (url-info :last-modified)
                 :If-None-Match (url-info :etag)}]
    [url url headers]))

;;TODO: it is more performant to get these io operations running in different threadpool.
(defn create-handler
  [handler get-url set-url & [rm-url]]
  (fn [k u headers body]
    (let [right-now (rfc822-str (Date.))
          current-info (get-url k)
          updated-info (merge current-info
                              {:last-fetched right-now})] 
         ;; TODO: Maybe the handler fns should also receive url-info so
         ;; none of them need the get-url fn?

         ;; We assume that updating the store in a 301 response is what should be done.
         ;; Otherwise the perm-redirect handler will need the rm-url fn.
         (if rm-url
           (if-let [new-url (:location headers)]
             (let [new-key new-url
                   updated-url-info (assoc current-info
                                      :url new-url)]
               (do (set-url new-key updated-url-info)
                   (rm-url k)))
             (log/error (format "No location in permanent redirect response for url: %s"
                                u)))
           (set-url k updated-info))
         (handler k u headers body))))

(defn update-feed
  "A wrapper for a handler which should update the last-modified and etag values for a feed."
  [handler get-url set-url]
  (fn [k u headers body]
    (let [last-modified (or (get headers :last-modified nil)
                            (get headers :date nil))
          etag (or (get headers :etag nil)
                   last-modified)
          right-now (rfc822-str (Date.))
          current-info (get-url k)
          updated-info (merge current-info
                              {:last-modified (or last-modified
                                                  right-now)
                               :etag (or etag
                                         last-modified
                                         right-now)})]
      (set-url k updated-info)
      (handler k u headers body))))

(defn perm-redirect
  [get-url submit-fetch-req]
  (fn [k u headers body]
    (log/debug (format "Perm redirect for %s" k))
    (let [new-url (:location headers)
          new-key new-url
          url-info (get-url new-key)]
      (apply submit-fetch-req (fetch-args url-info)))))

(defn temp-redirect
  [get-url submit-fetch-req]
  (fn [k u headers body]
    (let [new-url (:location headers)
          new-key new-url
          url-info (get-url k)]
      (apply submit-fetch-req (assoc (fetch-args url-info)
                                :url new-url)))))

(defn nil-handler
  [k u headers body]
    nil)

(defn ok? [c] (or (= c :200)
                  (= c 200)
                  (= c "200")))

(defn create-default-handlers
  "Default map of status codes to appropriate handlers."
  [ok-handler get-url set-url rm-url put-ok put-redirect]
  (let [ok-handler (create-handler (update-feed ok-handler get-url set-url)
                                   get-url set-url)
        perm-redirect-handler (create-handler (perm-redirect get-url put-redirect)
                                              get-url set-url rm-url)
        temp-redirect-handler (create-handler (temp-redirect get-url put-redirect)
                                              get-url set-url)]
    {:301 perm-redirect-handler
     :302 temp-redirect-handler
     :300 temp-redirect-handler
     :307 temp-redirect-handler
     :410 nil-handler
     :404 nil-handler
     :200 ok-handler}))

(defn create-response-callback
  "Create callback that will be called once the async
  HTTP request has completely read the response body and
  execute the matching handler."
  [handlers]
  (fn [k u status-code headers body]
    (let [code (-> status-code str keyword)
          handler (handlers code)
          result (handler k u headers body)]
      (log/info (format "Result is %s" (str result)))
      (cons code result))))