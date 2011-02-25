(ns fetcher.run-server
  (:use ring.adapter.jetty)
  (:use ring.middleware.reload)
  (:use fetcher.core-test))

(defn -main [& args]
  (println "booting test server")
  (run-jetty
    (-> #'handler (wrap-reload '(fetcher.core-test)))
    {:port 8080}))
