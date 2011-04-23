(ns fetcher.core-test
  (:require [clojure.contrib.pprint :as pp]
	    [clojure.contrib.io :as io]
	    [fetcher.util :as util]
            [clojure.contrib.io  :as io]
            [fetcher.core :as fetcher])
  (:import (org.apache.http.client.params CookiePolicy ClientPNames)
           (org.apache.http.impl.client DefaultHttpClient)
	   (java.util Arrays))
  (:use clojure.test
        ring.adapter.jetty
        ring.middleware.reload
	[plumbing.streams :only [test-stream]]))

(def base-req
  {:scheme "http"
   :server-name "localhost"
   :server-port 8080})

(deftest roundtrip
  (let [resp (fetcher/fetch :get (merge base-req {:uri "/get"}))]
    (is (= 200 (:status resp)))
    (is (= "close" (get-in resp [:headers "connection"])))
    (is (= "get" (:body resp)))))

(defn is-passed [middleware req]
  (let [client (middleware identity)]
    (is (= req (client req)))))

(defn is-applied [middleware req-in req-out]
  (let [client (middleware identity)]
    (is (= req-out (client req-in)))))

(deftest host-and-port-test
  (is (= "host:8080"
         (fetcher/host-and-port {:server-name "host"
                                :server-port 8080})))
  (is (= "noporthost"
         (fetcher/host-and-port {:server-name "noporthost"}))))

;; http://f.com:443/orig -- /target -> http://f.com:443/doh
;; http://g.com/old -- /new -> http://g.com/new
;; http://h.com:8080/old -- http://hh.com/new -> http://hh.com/new
(deftest redirect-req-test
  (let [client identity
        req (fetcher/parse-url "http://mud.com:8080/gnarl?boom=true")
        resp {:headers {"location" "/rad?arg=foo"}}
        red-req (fetcher/redirect-req req resp)]
    (is (= "http"
           (:scheme red-req)))
    (is (= "mud.com"
           (:server-name red-req)))
    (is (= 8080
           (:server-port red-req)))
    (is (= "/rad"
           (:uri red-req)))
    (is (= "arg=foo"
           (:query-string red-req)))))

(deftest redirect-on-get
  (let [client (fn [req]
                 (if (= "foo.com" (:server-name req))
                   {:status 302
                    :headers {"location" "http://bar.com/bat"}}
                   {:status 200
                    :req req}))
        req {:server-name "foo.com" :request-method :get}
        first-resp (client req)
        resp (fetcher/redirect client req first-resp)]
    (is (= 200 (:status resp)))
    (is (= :get (:request-method (:req resp))))
    (is (= "http" (:scheme (:req resp))))
    (is (= "/bat" (:uri (:req resp))))))

(deftest redirect-to-get-on-head
  (let [client (fn [req]
                 (if (= "foo.com" (:server-name req))
                   {:status 303
                    :headers {"location" "http://bar.com/bat"}}
                   {:status 200
                    :req req}))
        req {:server-name "foo.com" :request-method :head}
        first-resp (client req)
        resp (fetcher/redirect client req first-resp)]
    (is (= 200 (:status resp)))
    (is (= :get (:request-method (:req resp))))
    (is (= "http" (:scheme (:req resp))))
    (is (= "/bat" (:uri (:req resp))))))

(deftest pass-on-non-redirect
  (let [client (fn [req] {:status 200 :body (:body req)})
        req {:body "ok"}
        first-resp (client req)
        resp (fetcher/redirect client req first-resp)]
    (is (= 200 (:status resp)))
    (is (= "ok" (:body resp)))))

(deftest apply-on-compressed
  (let [resp (fetcher/decompress {:body (-> "foofoofoo"
                                           .getBytes
                                           util/gzip
                                           java.io.ByteArrayInputStream.)
                                 :headers {"content-encoding" "gzip"}})]
    (is (= "foofoofoo" (-> resp :body  io/slurp*)))))

(deftest apply-on-deflated
  (let [resp (fetcher/decompress {:body (-> "barbarbar" .getBytes
                                           util/deflate
                                           java.io.ByteArrayInputStream.)
                                 :headers {"content-encoding" "deflate"}})]
    (is (= "barbarbar" (io/slurp* (:body resp))))))

(deftest pass-on-non-compressed
  (let [resp (fetcher/decompress {:body "foo"})]
    (is (= "foo" (:body resp)))))

(deftest apply-on-accept
  (is (=
       {:headers {"Accept" "application/json"}}
       (fetcher/accept {:accept :json}))))

(deftest pass-on-no-accept
  (let [req {:uri "/foo"}]
    (is (= req (fetcher/accept req)))))

(deftest apply-on-accept-encoding
  (is (=  {:headers {"Accept-Encoding" "identity, gzip"}}
          (fetcher/wrap-accept-encoding
           {:accept-encoding [:identity :gzip]}))))

(deftest pass-on-no-accept-encoding
  (let [req {:uri "/foo"}]
    (is (= req (fetcher/wrap-accept-encoding req)))))


(deftest apply-on-output-coercion
  (let [resp (fetcher/output-coercion
              :byte-array
              {:body (io/input-stream (.getBytes "foo"))})]
    (is (Arrays/equals (.getBytes "foo") (:body resp)))))

(deftest input-stream-output-coercion
  (let [resp (fetcher/output-coercion
	      :input-stream
	      {:body (io/input-stream (.getBytes "foo"))})]
    (is (instance? java.io.InputStream (:body resp)))
    (is (Arrays/equals (.getBytes "foo")
		      (org.apache.commons.io.IOUtils/toByteArray (:body resp))))))

(deftest pass-on-no-output-coercion
  (let [resp (fetcher/output-coercion nil {:body nil})]
    (is (nil? (:body resp))))
  (let [resp (fetcher/output-coercion
              :byte-array
              {:body :thebytes})]
    (is (= :thebytes (:body resp)))))

(deftest apply-on-input-coercion
  (let [resp (fetcher/input-coercion {:body "foo"})]
    (is (= "UTF-8" (:character-encoding resp)))
    (is (Arrays/equals (util/utf8-bytes "foo") (:body resp)))))

(deftest pass-on-no-input-coercion
  (let [req {:body (util/utf8-bytes "foo")}]
    (is (= req
           (fetcher/input-coercion req)))))

(deftest apply-on-content-type
  (is (= {:content-type "application/json"}
         (fetcher/content-type
          {:content-type :json}))))

(deftest pass-on-no-content-type
  (let [req {:uri "/foo"}]
    (is (= req (fetcher/content-type req)))))

(deftest apply-on-query-params
  (is (= {:query-string "foo=bar&dir=%3C%3C"}
         (fetcher/query-params
          {:query-params {"foo" "bar" "dir" "<<"}}))))

(deftest pass-on-no-query-params
  (let [req {:uri "/foo"}]
    (is (= req
           (fetcher/query-params req)))))

(deftest apply-on-basic-auth
  (is (= {:headers {"Authorization" "Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ=="}}
         (fetcher/basic-auth
          {:basic-auth ["Aladdin" "open sesame"]}))))

(deftest pass-on-no-basic-auth
  (let [req {:uri "/foo"}]
    (is (= req
           (fetcher/basic-auth req)))))

(deftest apply-on-url
  (let [resp (fetcher/parse-url "http://google.com:8080/foo?bar=bat")]
    (is (= "http" (:scheme resp)))
    (is (= "google.com" (:server-name resp)))
    (is (= 8080 (:server-port resp)))
    (is (= "/foo" (:uri resp)))
    (is (= "bar=bat" (:query-string resp)))))

(deftest parse-url-with-hash
  (let [u (fetcher/parse-url "http://gizmodo.com/#!5789093/the-near+future-of-mobile-gaming-is-going-to-be-pretty-epic")]
    (is (= "/#!5789093/the-near+future-of-mobile-gaming-is-going-to-be-pretty-epic" (:uri  u)))))

(deftest url-with-hash
  (let [u "http://gizmodo.com/#!5789093/the-near+future-of-mobile-gaming-is-going-to-be-pretty-epic"]
    (is (= u
	    (:url (fetcher/fetch :get u))))))

(deftest strip-bad-punc-test
  (is (= "utf-8"
         (fetcher/strip-punc "utf-8;")))
  (is
   (= "iso-8859-2"
      (fetcher/strip-punc "iso-8859-2"))))

(deftest charset-in-body
  (let [body "<html>
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>
<meta name=\"author\" content=\"I Wayan Saryada\"/></head></html>"]
    (is (= "utf-8" (fetcher/charset {:body body})))))

(deftest charset-test
  (is (= "windows-1250"
         (fetcher/charset {:body  "
<html><head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=windows-1250\"/>
<meta http-equiv=\"cache-control\" content=\"no-cache\"/>
<meta name=\"robots\" content=\"all\"/>
<title>Zemřel Pavel Vondruška, muzikant a jeden z 'Cimrmanů' - www.lidovky.cz</title>
</head><body></body></html>"}))))

(deftest charset-meta-charset
  (is (= "EUC-JP"
	 (fetcher/charset {:body
			  "<html><head><META http-equiv=\"Content-Type\" content=\"text/html; charset=EUC-JP\"></head><body></body></html>"}))))

(deftest html5-charset-test
  (is (= "fake-charset"
	 ( fetcher/charset
	   {:body"<!doctype html>
<html>
 <head>
   <meta charset=\"fake-charset\">
   <title>Example document</title>
 </head>
 <body>
   <p>Example paragraph</p>
 </body>
</html>"}))))

(deftest charset-test
  (is (= "windows-1250"
         (fetcher/charset {:body "<!DOCTYPE html PUBLIC \"-//Lidovky//DTD HTML 4//EN\" \"http://g.lidovky.cz/dtd/n3_uni.dtd\">
<html><head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=windows-1250\">
<meta http-equiv=\"cache-control\" content=\"no-cache\">
<meta name=\"robots\" content=\"all\">
<title>Zemřel Pavel Vondruška, muzikant a jeden z 'Cimrmanů' - www.lidovky.cz</title>
</head><body></body></html>"}))))

;;http://www.boston.com/sports/colleges/extras/colleges_blog/2011/03/harvard_wins_sh.html
#_(deftest charset-prod-example-breakage
  (is (= "ISO-8859-1"
	 (fetcher/charset
	  {:body 
	   " <html lang=\"en\">
  <head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=\"ISO-8859-1\">"}))))

(deftest redirect-path-test
  (is (= [[:302 "http://localhost:8080/moved"]
          [:301 "http://localhost:8080/get"]]
           (:redirects (fetcher/fetch :get "http://localhost:8080/bounce")))))

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
  (fetcher/request (merge base-req req)))

(defn slurp-body [req]
  (io/slurp* (:body req)))

(deftest basic-http-client-test
  (let [c (fetcher/basic-http-client)]
    (is (= (doto (.getParams (DefaultHttpClient.))
             (.setParameter ClientPNames/COOKIE_POLICY
                            CookiePolicy/BROWSER_COMPATIBILITY)))
        (.getParams (fetcher/basic-http-client)))))

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
  (let [resp (fetcher/fetch :get {:scheme "http"
                                   :server-name "localhost"
                                   :server-port 8080
                                   :uri "/bounce"})]
    (is (= [[:302 "http://localhost:8080/moved"]
            [:301 "http://localhost:8080/get"]]
             (:redirects resp)))))

(deftest strip-query-string-test
  (is (empty?
       (fetcher/strip-query-string "utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+InfectiousGreed+%28Paul+Kedrosky%27s+Infectious+Greed%29")))
  (is (= "a=2&b=1" (fetcher/strip-query-string "b=1&a=2"))))

(deftest resolved-url-test
  (is (=
       "http://paul.kedrosky.com/archives/2011/04/coachella-glish.html"
       (fetcher/resolved-url
	{:redirects [[:301 "http://paul.kedrosky.com/archives/2011/04/coachella-glish.html?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+InfectiousGreed+%28Paul+Kedrosky%27s+Infectious+Greed%29%22"]]}))))