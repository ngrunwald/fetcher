(ns fetcher.client-test
  (:use clojure.test
        ring.adapter.jetty
        ring.middleware.reload)
  (:use [plumbing.streams :only [test-stream]])
  (:require [fetcher.client :as client]
            [fetcher.util :as util]
            [clojure.contrib.io  :as io]
            [fetcher.core :as fetcher])
  (:import (java.util Arrays)
           (java.util.concurrent.atomic AtomicInteger)))

(def base-req
  {:scheme "http"
   :server-name "localhost"
   :server-port 8080})

(deftest roundtrip
  (let [resp (client/request :get (merge base-req {:uri "/get"}))]
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
         (client/host-and-port {:server-name "host"
                                :server-port 8080})))
  (is (= "noporthost"
         (client/host-and-port {:server-name "noporthost"}))))

;; http://f.com:443/orig -- /target -> http://f.com:443/doh
;; http://g.com/old -- /new -> http://g.com/new
;; http://h.com:8080/old -- http://hh.com/new -> http://hh.com/new
(deftest redirect-req-test
  (let [client identity
        req (fetcher/parse-url "http://mud.com:8080/gnarl?boom=true")
        resp {:headers {"location" "/rad?arg=foo"}}
        red-req (client/redirect-req req resp)]
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
        resp (client/redirect client req first-resp)]
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
        resp (client/redirect client req first-resp)]
    (is (= 200 (:status resp)))
    (is (= :get (:request-method (:req resp))))
    (is (= "http" (:scheme (:req resp))))
    (is (= "/bat" (:uri (:req resp))))))

(deftest pass-on-non-redirect
  (let [client (fn [req] {:status 200 :body (:body req)})
        req {:body "ok"}
        first-resp (client req)
        resp (client/redirect client req first-resp)]
    (is (= 200 (:status resp)))
    (is (= "ok" (:body resp)))))

(deftest apply-on-compressed
  (let [resp (client/decompress {:body (-> "foofoofoo"
                                           .getBytes
                                           util/gzip
                                           java.io.ByteArrayInputStream.)
                                 :headers {"content-encoding" "gzip"}})]
    (is (= "foofoofoo" (-> resp :body  io/slurp*)))))

(deftest apply-on-deflated
  (let [resp (client/decompress {:body (-> "barbarbar" .getBytes
                                           util/deflate
                                           java.io.ByteArrayInputStream.)
                                 :headers {"content-encoding" "deflate"}})]
    (is (= "barbarbar" (io/slurp* (:body resp))))))

(deftest pass-on-non-compressed
  (let [resp (client/decompress {:body "foo"})]
    (is (= "foo" (:body resp)))))

(deftest apply-on-accept
  (is (=
       {:headers {"Accept" "application/json"}}
       (client/accept {:accept :json}))))

(deftest pass-on-no-accept
  (let [req {:uri "/foo"}]
    (is (= req (client/accept req)))))

(deftest apply-on-accept-encoding
  (is (=  {:headers {"Accept-Encoding" "identity, gzip"}}
          (client/accept-encoding
           {:accept-encoding [:identity :gzip]}))))

(deftest pass-on-no-accept-encoding
  (let [req {:uri "/foo"}]
    (is (= req (client/accept-encoding req)))))


(deftest apply-on-output-coercion
  (let [resp (client/output-coercion
              {:as :byte-array}
              {:body (io/input-stream (.getBytes "foo"))})]
    (is (Arrays/equals (.getBytes "foo") (:body resp)))))

(deftest pass-on-no-output-coercion
  (let [resp (client/output-coercion {} {:body nil})]
    (is (nil? (:body resp))))
  (let [resp (client/output-coercion
              {:as :byte-array}
              {:body :thebytes})]
    (is (= :thebytes (:body resp)))))

(deftest apply-on-input-coercion
  (let [resp (client/input-coercion {:body "foo"})]
    (is (= "UTF-8" (:character-encoding resp)))
    (is (Arrays/equals (util/utf8-bytes "foo") (:body resp)))))

(deftest pass-on-no-input-coercion
  (let [req {:body (util/utf8-bytes "foo")}]
    (is (= req
           (client/input-coercion req)))))

(deftest apply-on-content-type
  (is (= {:content-type "application/json"}
         (client/content-type
          {:content-type :json}))))

(deftest pass-on-no-content-type
  (let [req {:uri "/foo"}]
    (is (= req (client/content-type req)))))

(deftest apply-on-query-params
  (is (= {:query-string "foo=bar&dir=%3C%3C"}
         (client/query-params
          {:query-params {"foo" "bar" "dir" "<<"}}))))

(deftest pass-on-no-query-params
  (let [req {:uri "/foo"}]
    (is (= req
           (client/query-params req)))))

(deftest apply-on-basic-auth
  (is (= {:headers {"Authorization" "Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ=="}}
         (client/basic-auth
          {:basic-auth ["Aladdin" "open sesame"]}))))

(deftest pass-on-no-basic-auth
  (let [req {:uri "/foo"}]
    (is (= req
           (client/basic-auth req)))))

(deftest apply-on-url
  (let [resp (fetcher/parse-url "http://google.com:8080/foo?bar=bat")]
    (is (= "http" (:scheme resp)))
    (is (= "google.com" (:server-name resp)))
    (is (= 8080 (:server-port resp)))
    (is (= "/foo" (:uri resp)))
    (is (= "bar=bat" (:query-string resp)))))

(deftest chunked-request-test
  (let [resp (client/output-coercion
              {:chunked? true}
              {:body (-> "1\r\na\r\n3\r\nfoo\r\n0\r\n\r\n"
                         .getBytes
                         io/input-stream)
               :headers {"transfer-encoding" "chunked"}})]
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
         (client/strip-punc "utf-8;")))
  (is
   (= "iso-8859-2"
      (client/strip-punc "iso-8859-2"))))

(deftest charset-in-body
  (let [body "<html>
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>
<meta name=\"author\" content=\"I Wayan Saryada\"/></head></html>"]
    (is (= "utf-8" (client/charset {:body body})))))

(deftest charset-test
  (is (= "windows-1250"
         (client/charset {:body  "
<html><head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=windows-1250\"/>
<meta http-equiv=\"cache-control\" content=\"no-cache\"/>
<meta name=\"robots\" content=\"all\"/>
<title>Zemřel Pavel Vondruška, muzikant a jeden z 'Cimrmanů' - www.lidovky.cz</title>
</head><body></body></html>"}))))

;; ;;TODO: deal with this - the userlying sax parser for clojure breaks on the below html but the parser in webime.parser works fine.
;; #_(deftest charset-test
;;   (is (= "windows-1250"
;;          (charset (dom "<!DOCTYPE html PUBLIC \"-//Lidovky//DTD HTML 4//EN\" \"http://g.lidovky.cz/dtd/n3_uni.dtd\">
;; <html><head>
;; <meta http-equiv=\"Content-Type\" content=\"text/html; charset=windows-1250\">
;; <meta http-equiv=\"cache-control\" content=\"no-cache\">
;; <meta name=\"robots\" content=\"all\">
;; <title>Zemřel Pavel Vondruška, muzikant a jeden z 'Cimrmanů' - www.lidovky.cz</title>
;; </head><body></body></html>")))))

(deftest redirect-path-test
  (is (= [[:302 "http://localhost:8080/moved"]
          [:301 "http://localhost:8080/get"]]
           (:redirects (client/request :get "http://localhost:8080/bounce")))))