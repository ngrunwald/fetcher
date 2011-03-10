(ns fetcher.client-test
  (:use clojure.test
	ring.adapter.jetty
	ring.middleware.reload
	fetcher.core-test)
  (:use [plumbing.streams :only [test-stream]])
  (:require [fetcher.client :as client]
	    [fetcher.util :as util]
	    [clojure.contrib.io  :as io])
  (:import (java.util Arrays)
	   (java.util.concurrent.atomic AtomicInteger)))

(deftest rountrip
  (let [resp (client/request (merge base-req {:uri "/get" :method :get}))]
    (is (= 200 (:status resp)))
    (is (= "close" (get-in resp [:headers "connection"])))
    (is (= "get" (:body resp)))))

(defn is-passed [middleware req]
  (let [client (middleware identity)]
    (is (= req (client req)))))

(defn is-applied [middleware req-in req-out]
  (let [client (middleware identity)]
    (is (= req-out (client req-in)))))

(deftest redirect-on-get
  (let [client (fn [req]
                 (if (= "foo.com" (:server-name req))
                   {:status 302
                    :headers {"location" "http://bar.com/bat"}}
                   {:status 200
                    :req req}))
        r-client (client/wrap-redirects client)
        resp (r-client {:server-name "foo.com" :request-method :get})]
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
        r-client (client/wrap-redirects client)
        resp (r-client {:server-name "foo.com" :request-method :head})]
    (is (= 200 (:status resp)))
    (is (= :get (:request-method (:req resp))))
    (is (= "http" (:scheme (:req resp))))
    (is (= "/bat" (:uri (:req resp))))))

(deftest pass-on-non-redirect
  (let [client (fn [req] {:status 200 :body (:body req)})
        r-client (client/wrap-redirects client)
        resp (r-client {:body "ok"})]
    (is (= 200 (:status resp)))
    (is (= "ok" (:body resp)))))


(deftest throw-on-exceptional
  (let [client (fn [req] {:status 500 :body "No worky."})
        e-client (client/wrap-exceptions client)]
    (is (thrown-with-msg? Exception #"500"
	  (e-client {})))))

(deftest pass-on-non-exceptional
  (let [client (fn [req] {:status 200})
        e-client (client/wrap-exceptions client)
        resp (e-client {})]
    (is (= 200 (:status resp)))))

(deftest pass-on-exceptional-when-surpressed
  (let [client (fn [req] {:status 500})
        e-client (client/wrap-exceptions client)
        resp (e-client {:throw-exceptions false})]
    (is (= 500 (:status resp)))))


(deftest apply-on-compressed
  (let [client (fn [req] {:body (-> "foofoofoo"
				    .getBytes
				    util/gzip
				    java.io.ByteArrayInputStream.)
                          :headers {"Content-Encoding" "gzip"}})
        c-client (client/wrap-decompression client)
        resp (c-client {})]
    (is (= "foofoofoo" (-> resp :body  io/slurp*)))))

(deftest apply-on-deflated
  (let [client (fn [req] {:body (-> "barbarbar" .getBytes
				    util/deflate
				    java.io.ByteArrayInputStream.)
                          :headers {"Content-Encoding" "deflate"}})
        c-client (client/wrap-decompression client)
        resp (c-client {})]
    (is (= "barbarbar" (io/slurp* (:body resp))))))

(deftest pass-on-non-compressed
  (let [c-client (client/wrap-decompression (fn [req] {:body "foo"}))
        resp (c-client {:uri "/foo"})]
    (is (= "foo" (:body resp)))))


(deftest apply-on-accept
  (is-applied client/wrap-accept
	      {:accept :json}
	      {:headers {"Accept" "application/json"}}))

(deftest pass-on-no-accept
  (is-passed client/wrap-accept
	     {:uri "/foo"}))


(deftest apply-on-accept-encoding
  (is-applied client/wrap-accept-encoding
	      {:accept-encoding [:identity :gzip]}
	      {:headers {"Accept-Encoding" "identity, gzip"}}))

(deftest pass-on-no-accept-encoding
  (is-passed client/wrap-accept-encoding
	     {:uri "/foo"}))


(deftest apply-on-output-coercion
  (let [client (fn [req] {:body (io/input-stream (.getBytes "foo"))})
        o-client (client/wrap-output-coercion client)
        resp (o-client {:uri "/foo"})]
    (is (= "foo" (:body resp)))))

(deftest pass-on-no-output-coercion
  (let [client (fn [req] {:body nil})
        o-client (client/wrap-output-coercion client)
        resp (o-client {:uri "/foo"})]
    (is (nil? (:body resp))))
  (let [client (fn [req] {:body :thebytes})
        o-client (client/wrap-output-coercion client)
        resp (o-client {:uri "/foo" :as :byte-array})]
    (is (= :thebytes (:body resp)))))


(deftest apply-on-input-coercion
  (let [i-client (client/wrap-input-coercion identity)
        resp (i-client {:body "foo"})]
    (is (= "UTF-8" (:character-encoding resp)))
    (is (Arrays/equals (util/utf8-bytes "foo") (:body resp)))))

(deftest pass-on-no-input-coercion
  (is-passed client/wrap-input-coercion
	     {:body (util/utf8-bytes "foo")}))


(deftest apply-on-content-type
  (is-applied client/wrap-content-type
	      {:content-type :json}
	      {:content-type "application/json"}))

(deftest pass-on-no-content-type
  (is-passed client/wrap-content-type
	     {:uri "/foo"}))


(deftest apply-on-query-params
  (is-applied client/wrap-query-params
	      {:query-params {"foo" "bar" "dir" "<<"}}
	      {:query-string "foo=bar&dir=%3C%3C"}))

(deftest pass-on-no-query-params
  (is-passed client/wrap-query-params
	     {:uri "/foo"}))


(deftest apply-on-basic-auth
  (is-applied client/wrap-basic-auth
	      {:basic-auth ["Aladdin" "open sesame"]}
	      {:headers {"Authorization" "Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ=="}}))

(deftest pass-on-no-basic-auth
  (is-passed client/wrap-basic-auth
	     {:uri "/foo"}))

(deftest apply-on-method
  (let [m-client (client/wrap-method identity)
        echo (m-client {:key :val :method :post})]
    (is (= :val (:key echo)))
    (is (= :post (:request-method echo)))
    (is (not (:method echo)))))

(deftest pass-on-no-method
  (let [m-client (client/wrap-method identity)
        echo (m-client {:key :val})]
    (is (= :val (:key echo)))
    (is (not (:request-method echo)))))


(deftest apply-on-url
  (let [u-client (client/wrap-url identity)
        resp (u-client {:url "http://google.com:8080/foo?bar=bat"})]
    (is (= "http" (:scheme resp)))
    (is (= "google.com" (:server-name resp)))
    (is (= 8080 (:server-port resp)))
    (is (= "/foo" (:uri resp)))
    (is (= "bar=bat" (:query-string resp)))))

(deftest pass-on-no-url
  (let [u-client (client/wrap-url identity)
        resp (u-client {:uri "/foo"})]
    (is (= "/foo" (:uri resp)))))

(deftest chunked-request-test
  (let [client (fn [req]
		 {:body (-> "1\r\na\r\n3\r\nfoo\r\n0\r\n\r\n"
			    .getBytes
			    io/input-stream)
		  :headers {"transfer-encoding" "chunked"}})
	o-client (client/wrap-output-coercion client)
	resp (o-client {:chunked? true})]
    (is (= ["a" "foo"] (:body resp)))))

#_(deftest chunked-request-stress-test
  (let [client (fn [req]
		 {:body (test-stream (.getBytes "3\r\nfoo\r\n")
				     10
				     (.getBytes "0\r\n\r\n"))
		  :headers {"transfer-encoding" "chunked"}})
	o-client (client/wrap-output-coercion client)
	resp (o-client {:chunked? true})]
    (is (= 10 (count (:body resp))))))

(deftest strip-bad-punc-test
  (is (= "utf-8"
	 (client/strip-punc "utf-8;"))))

(deftest charset-in-body
  (let [body "<html>
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>
<meta name=\"author\" content=\"I Wayan Saryada\"/>
</head></html>"]
    (is (= "utf-8" (client/charset body)))))

(deftest charset-test
  (is (= "windows-1250"
         (client/charset "
<html><head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=windows-1250\"/>
<meta http-equiv=\"cache-control\" content=\"no-cache\"/>
<meta name=\"robots\" content=\"all\"/>
<title>Zemřel Pavel Vondruška, muzikant a jeden z 'Cimrmanů' - www.lidovky.cz</title>
</head><body></body></html>"))))

;;TODO: deal with this - the userlying sax parser for clojure breaks on the below html but the parser in webime.parser works fine.
#_(deftest charset-test
  (is (= "windows-1250"
         (charset (dom "<!DOCTYPE html PUBLIC \"-//Lidovky//DTD HTML 4//EN\" \"http://g.lidovky.cz/dtd/n3_uni.dtd\">
<html><head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=windows-1250\">
<meta http-equiv=\"cache-control\" content=\"no-cache\">
<meta name=\"robots\" content=\"all\">
<title>Zemřel Pavel Vondruška, muzikant a jeden z 'Cimrmanů' - www.lidovky.cz</title>
</head><body></body></html>")))))
