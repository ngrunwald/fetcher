(ns fetcher.perf-test
  (:use fetcher.client
	[clj-time.core :only [now in-msecs interval]]
	[work.core :only [map-work]]
	[fetcher.core :only [pooled-http-client]]))

(defn time-fn [f]
  (let [start (now)]
    (f)
    (in-msecs (interval start (now)))))

(defn step-threads [f urls threads]
  (into {}
	(map (fn [t] [t (time-fn #(f urls t))])
	     threads)))

(def test-urls (repeat 500 "http://www.google.com"))

(defn fetch-throughput2 [urls threads]
  (let [c (pooled-http-client)]
    (map-work 
	    #(request (constantly c) :get %)
	    threads
	    urls)))

(defn fetch-throughput [urls threads]
  (map-work 
	  #(request :get %)
	  threads
	  urls))