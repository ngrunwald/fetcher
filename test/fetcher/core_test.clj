(ns fetcher.core-test
  (:use clojure.test)
  (:require [clojure.contrib.pprint :as pp])
  (:require [clojure.contrib.io :as io])
  (:require [fetcher.core :as core])
  (:require [fetcher.util :as util])
  (:import (org.apache.http.client.params CookiePolicy ClientPNames)
           (org.apache.http.impl.client DefaultHttpClient)))

(defn handler [req]
  (pp/pprint req)
  (println) (println)
  (condp = [(:request-method req) (:uri req)]
    [:get "/get"]
      {:status 200 :body "get"}
    [:head "/head"]
      {:status 200}
    [:get "/content-type"]
      {:status 200 :body (:content-type req)}
    [:get "/header"]
      {:status 200 :body (get-in req [:headers "x-my-header"])}
    [:post "/post"]
      {:status 200 :body (io/slurp* (:body req))}
    [:get "/error"]
      {:status 500 :body "o noes"}
    [:get "/moved"]
      {:status 301 :headers {"Location" "http://localhost:8080/get"}}
    [:get "/moved-absolute"]
      {:status 301 :headers {"Location" "/get"}}
    [:get "/moved-relative"]
      {:status 301 :headers {"Location" "get"}}
    [:get "/moved-no-prot"]
      {:status 301 :headers {"Location" "localhost:8080/get"}}
    [:get "/bounce"]
      {:status 302 :headers {"Location" "http://localhost:8080/moved"}}))

(def base-req
  {:scheme "http"
   :server-name "localhost"
   :server-port 8080})

(defn request [req]
  (core/request (merge base-req req)))

(defn slurp-body [req]
  (io/slurp* (:body req)))

(deftest basic-http-client-test
  (let [c (core/basic-http-client)]
    (is (= (doto (.getParams (DefaultHttpClient.))
             (.setParameter ClientPNames/COOKIE_POLICY
                            CookiePolicy/BROWSER_COMPATIBILITY)))
        (.getParams (core/basic-http-client)))))

(deftest makes-get-request
  (let [resp (request {:request-method :get :uri "/get"})]
    (is (= 200 (:status resp)))
    (is (= "get" (slurp-body resp)))))

(deftest makes-head-request
  (let [resp (request {:request-method :head :uri "/head"})]
    (is (= 200 (:status resp)))
    (is (nil? (:body resp)))))

(deftest sets-content-type-with-charset
  (let [resp (request {:request-method :get :uri "/content-type"
                       :content-type "text/plain" :character-encoding "UTF-8"})]
    (is (= "text/plain; charset=UTF-8" (slurp-body resp)))))

(deftest sets-content-type-without-charset
  (let [resp (request {:request-method :get :uri "/content-type"
                       :content-type "text/plain"})]
    (is (= "text/plain" (slurp-body resp)))))

(deftest sets-arbitrary-headers
  (let [resp (request {:request-method :get :uri "/header"
                       :headers {"X-My-Header" "header-val"}})]
    (is (= "header-val" (slurp-body resp)))))

(deftest sends-and-returns-byte-array-body
  (let [resp (request {:request-method :post :uri "/post"
                       :body (util/utf8-bytes "contents")})]
    (is (= 200 (:status resp)))
    (is (= "contents" (slurp-body resp)))))

(deftest returns-arbitrary-headers
  (let [resp (request {:request-method :get :uri "/get"})]
    (is (string? (get-in resp [:headers "date"])))))

(deftest returns-status-on-exceptional-responses
  (let [resp (request {:request-method :get :uri "/error"})]
    (is (= 500 (:status resp)))))

(deftest redirect-moved-test
  (let [resp (request {:request-method :get :uri "/moved"})]
    (is (= 200 (:status resp))))
  (let [resp (request {:request-method :get :uri "/moved-absolute"})]
    (is (= 200 (:status resp))))
  (let [resp (request {:request-method :get :uri "/moved-relative"})]
    (is (= 200 (:status resp)))))

(deftest redirect-strategy-test
  (let [client (core/basic-http-client
                {:params core/default-params})
        resp (core/request client {:request-method :get
                                   :scheme "http"
                                   :server-name "localhost"
                                   :server-port 8080
                                   :uri "/bounce"})]
    (is (= [[:302 "http://localhost:8080/moved"]
            [:301 "http://localhost:8080/get"]]
             (:redirects resp)))))

(deftest strip-query-string-test
  (is (empty?
       (core/strip-query-string "utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+InfectiousGreed+%28Paul+Kedrosky%27s+Infectious+Greed%29")))
  (is (= "a=2&b=1" (core/strip-query-string "b=1&a=2"))))

(deftest resolved-url-test
  (is (=
       "http://paul.kedrosky.com/archives/2011/04/coachella-glish.html"
       (core/resolved-url
	{:redirects [[:301 "http://paul.kedrosky.com/archives/2011/04/coachella-glish.html?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+InfectiousGreed+%28Paul+Kedrosky%27s+Infectious+Greed%29%22"]]}))))