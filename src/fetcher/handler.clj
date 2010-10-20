(ns fetcher.handler
  "Some default handlers for working with fetcher response."
  (:require [clojure.java.io :as io])
  (:import [java.util TimeZone Date]
           [javax.mail.internet MailDateFormat]))

(declare format-last-modified)

;;TODO: it is more performant to get these io operations running in different threadpool.
(defn create-handler
  [handler get-url set-url & rm-url]
  (fn [k u headers body]  
    (let [right-now (format-last-modified (Date.))
          updated-url (merge (get-url k)
                             {:last-fetched right-now})]
      (set-url k updated-url)
      (when rm-url (rm-url k))
      (handler k u headers body))))

(defn perm-redirect-handler
  [handler]
  (fn [k u headers body]
  (let [new-url (:location headers)]
    ;;new url becomes the new key
    (handler new-url new-url headers body))))

(defn temp-redirect-handler
  [handler]
  (fn [k u headers body]
  (let [new-url (:location headers)]
    (handler k new-url headers body))))

(defn nil-handler
  [k u headers body]
    nil)

(defn ok? [c] (= c :200))

(defn put-fetch [put-ok put-redirect]
    {:301 put-redirect
     :302 put-redirect
     :300 put-redirect
     :307 put-redirect
     :410 identity
     :404 identity
     :200 put-ok})

(defn create-default-handlers
  "Default map of status codes to appropriate handlers."
  [ok-handler get-url set-url rm-url put-ok put-redirect]
  (let [ok-handler (create-handler ok-handler get-url set-url)
        perm-redirect-handler (create-handler perm-redirect-handler
					      get-url set-url rm-url)]
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
	  (cons code result))))

(defn format-last-modified
  [date]
  (let [gmt (TimeZone/getTimeZone "GMT")
        formatter (doto (MailDateFormat.)
                    (.setTimeZone gmt))]
    (.format formatter date)))