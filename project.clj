(defproject fetcher "0.0.5-SNAPSHOT"
  :description "Clojure HTTP Fetching"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.apache.httpcomponents/httpclient "4.1"]
                 [org.apache.httpcomponents/httpcore "4.1"]
                 [clj-sys/plumbing "0.1.5-SNAPSHOT"]
                 [html-parse "0.0.1-SNAPSHOT"]
                 [clj-time "0.4.0-SNAPSHOT"]
                 [commons-codec "1.4"]
                 [commons-io "1.4"]]
  :dev-dependencies [[swank-clojure "1.3.0-SNAPSHOT"]
                     [ring/ring-jetty-adapter "0.3.5"]
                     [ring/ring-devel "0.3.5"]]
  :repositories {"java.net" "http://download.java.net/maven/2"
                 "clojars" "http://clojars.org/repo"})