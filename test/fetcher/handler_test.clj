(ns fetcher.handler-test
  (:use clojure.test
        [fetcher.handler :only [create-handler
                                update-feed
                                temp-redirect
                                perm-redirect]])
  (:import (java.util.concurrent LinkedBlockingQueue)))

(def store (atom {}))
(def fetch-req-q (LinkedBlockingQueue.))

(defn get-url
  [k] (get @store k))

(defn set-url
  [k v] (swap! store assoc k v))

(defn rm-url
  [k] (swap! store dissoc k))

(defn init
  [f]
  (swap! store (fn [_] {}))
  (.clear fetch-req-q)
  (f))

(use-fixtures :each init)

(deftest handle-ok
  (let [k "http://xkcd.com/224/"
        u "http://xkcd.com/224/"
        h {:accept-ranges "bytes"
           :content-length "9237"
           :content-type "text/html"
           :date "Sun, 28 Nov 2010 01:59:13 GMT"
           :etag "\"2525050226\""
           :last-modified "Fri, 26 Nov 2010 05:34:54 GMT"
           :server "lighttpd/1.4.19"
           :vary "Accept-Encoding"}
        b "The body text."
        handler (create-handler (update-feed (fn [k u h b] b)
                                             get-url
                                             set-url)
                                get-url
                                set-url)]
    (is (= b (handler k u h b)))
    (is (= (:etag h) (-> (get-url k) :etag)))
    (is (= (:last-modified h) (-> (get-url k) :last-modified)))))

(deftest handle-temp-redirect
  (let [new-url "http://oracle.com"
        k "http://sun.com"
        u "http://sun.com"
        h {:location new-url}
        b "The body text."
        put-redirect (fn [k u h] (.offer fetch-req-q (prn-str [k u h])))
        handler (create-handler (temp-redirect get-url
                                               put-redirect)
                                get-url
                                set-url)]
    (set-url k {:url u})
    (handler k u h b)
    (let [[req-k req-u req-h] (-> (.take fetch-req-q)
                                  read-string)]
      (is (= new-url req-u))
      (is (= u (-> (get-url k) :url)))
      (is (empty? fetch-req-q)))))

(deftest handle-perm-redirect
  (let [new-url "http://oracle.com"
        k "http://sun.com"
        u "http://sun.com"
        h {:location new-url}
        b "The body text."
        put-redirect (fn [k u h] (.offer fetch-req-q (prn-str [k u h])))
        handler (create-handler (perm-redirect get-url
                                               put-redirect)
                                get-url
                                set-url
                                rm-url)]
    (set-url k {:url u})
    (handler k u h b)
    (let [[req-k req-u req-h] (-> (.take fetch-req-q)
                                  read-string)]
      (is (= new-url req-u))
      (is (= new-url (-> (get-url new-url) :url)))
      (is (empty? fetch-req-q)))))

(deftest same-old-and-new-perm-redirect
  (let [k "http://clojure.org"
        u "http://clojure.org"
        h {:location k}
        b "The body text."
        put-redirect (fn [k u h] (.offer fetch-req-q (prn-str [k u h])))
        handler (create-handler (perm-redirect get-url
                                               put-redirect)
                                get-url
                                set-url
                                rm-url)]
    (set-url k {:url u})
    (handler k u h b)
    (is (not (nil? (get-url k))))
    (is (= u (-> (get-url k) :url)))
    (.take fetch-req-q)
    (is (empty? fetch-req-q))))