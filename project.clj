(defproject fetcher "1.0.3-SNAPSHOT"
  :description "work based fetcher service."
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojars.vgeshel/http.async.client "0.2.2-SNAPSHOT"]
                 [javax.mail/mail "1.4.3"]
                 [work "0.1.4-SNAPSHOT"]
                 [webmine "0.1.3-SNAPSHOT"]
                 [clj-sys/plumbing "0.1.3-SNAPSHOT"]
                 [crane "1.0-SNAPSHOT"]
                 [clj-time "0.2.0-SNAPSHOT"]]
  :dev-dependencies [[swank-clojure "1.2.1"]
                     [crane/lein-crane "0.0.1-SNAPSHOT"]
                     [robert/hooke "1.1.0"]]
  :repositories {"java.net" "http://download.java.net/maven/2"
                 "clojars" "http://clojars.org/repo"}
  :test-selectors {:default (fn [v] (not (or (:integration v)
                                             (:system v))))
                   :integration :integration
                   :system :system
                   :performance :performance
                   :all (fn [_] true)})