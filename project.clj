(defproject woven/clj-http "0.1.2-chunked-SNAPSHOT"
  :description "A Clojure HTTP library wrapping the Apache HttpComponents client."
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.apache.httpcomponents/httpclient "4.1"]
                 [org.apache.httpcomponents/httpcore "4.1"]
                 [clj-sys/plumbing "0.1.3-SNAPSHOT"]
                 [work "0.2.3-SNAPSHOT"]
                 [commons-codec "1.4"]
                 [commons-io "1.4"]]
  :dev-dependencies [[swank-clojure "1.2.0"]
                     [ring/ring-jetty-adapter "0.3.5"]
                     [ring/ring-devel "0.3.5"]]
  :repositories {"snapshots" "http://mvn.getwoven.com/repos/woven-public-snapshots"
                 "releases" "http://mvn.getwoven.com/repos/woven-public-releases"})

