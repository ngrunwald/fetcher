(ns fetcher.core-test
  (:use clojure.test
	plumbing.core
	fetcher.core)
  (:require [http.async.client :as c]))

(defn- promise-map [m]
  (reduce
   (fn [res [k v]]
     (let [p (promise)]
       (deliver p v)
       (assoc res k p)))
   {} m))

(deftest callback-test
  (let [m (atom nil)
	callback (fn [& args]
		   (let [body (last args)]
		     (swap! m (constantly body))))
	dg (with-url "key" "http://dl.dropbox.com/u/1205228/test.txt"
	     callback)
	state (promise-map {
			    :status {:code 200}
			    :headers {:content-type "text/plain; charset=ascii"}
			    :body (let [bs (.getBytes "This is a test")
					baos (java.io.ByteArrayOutputStream.)]
				    (.write baos bs 0 (count bs))
				    baos) })]
    (dg state)
    (is (= @m "This is a test"))))

(deftest fetch-test
  (let [m (promise)
	callback (fn [& args]
		   (let [body (last args)]
		     (deliver m body)))]
    (fetch ["key"  "http://dl.dropbox.com/u/1205228/test.txt"] callback)
    (is (= @m "This is a test.\n"))))

