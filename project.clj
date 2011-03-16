(defproject fetcher "0.0.2-SNAPSHOT"
  :description "Clojure HTTP Fetching"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.apache.httpcomponents/httpclient "4.1"]
                 [org.apache.httpcomponents/httpcore "4.1"]
                 [clj-sys/plumbing "0.1.3-SNAPSHOT"]
                 [work "0.2.7-SNAPSHOT"]
                 [html-parse "0.0.1-SNAPSHOT"]
                 [webmine "0.1.3-SNAPSHOT"]
                 [crane "1.0-SNAPSHOT"]
                 [clj-time "0.3.0-SNAPSHOT"]
                 [commons-codec "1.4"]
                 [commons-io "1.4"]]
  :dev-dependencies [[swank-clojure "1.3.0-SNAPSHOT"]
                     [ring/ring-jetty-adapter "0.3.5"]
                     [ring/ring-devel "0.3.5"]]
  :repositories {"java.net" "http://download.java.net/maven/2"
                 "clojars" "http://clojars.org/repo"
                 "snapshots" "https://mvn.getwoven.com:8443/repos/woven-internal-snapshots"
                 "releases" "https://mvn.getwoven.com:8443/repos/woven-internal-releases"
                 "woven" "http://mvn.getwoven.com/repos/woven-public"})