(defproject fetcher "1.0.3-SNAPSHOT"
  :description "work based fetcher service."
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [http.async.client "0.2.2-SNAPSHOT"]
                 [javax.mail/mail "1.4.3"]
                 [work "0.1.4-SNAPSHOT"]
                 [webmine "0.1.3-SNAPSHOT"]
                 [clj-sys/plumbing "0.1.3-SNAPSHOT"]
                 [crane "1.0-SNAPSHOT"]
                 [clj-time "0.2.0-SNAPSHOT"]]
  :dev-dependencies [[swank-clojure "1.2.1"]]
  :repositories {"java.net" "http://download.java.net/maven/2"
                 "clojars" "http://clojars.org/repo"
                 "snapshots" "http://mvn.getwoven.com/repos/woven-public-snapshots"
                 "releases" "http://mvn.getwoven.com/repos/woven-public-releases"})