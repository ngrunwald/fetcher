(ns fetcher.callback
  (:require (clj-time [core :as time]
                      [format :as time-fmt]
                      [coerce :as time-coerce])))

(defn mk-update-fetch
  [get-url set-url]
  (fn [{k :key}]
    (let [right-now (time-fmt/unparse (time-fmt/formatters :rfc822)
                                      (time/now))
          current-info (get-url k)
          updated-info (merge current-info
                              {:last-fetched right-now})]
      (set-url k updated-info))))

(defn mk-update-feed
  [get-feed set-feed]
  (fn [{k :key h :headers}]
    (let [last-modified (or (get h :last-modified nil)
                            (get h :date nil))
          etag (or (get h :etag nil)
                   last-modified)
          right-now (time-fmt/unparse (time-fmt/formatters :rfc822)
                                      (time/now))
          current-info (get-feed k)
          updated-info (merge current-info
                              {:last-modified (or last-modified
                                                  right-now)
                               :etag (or etag
                                         last-modified
                                         right-now)})]
      (set-feed k updated-info))))

(defn mk-save-body
  [get-item put-item]
  (fn [{k :key b :body}]
      (put-item k (assoc (get-item k) :body b))))

(defn mk-move-feed
  [get-feed set-feed]
  (fn [{k :key h :headers}]
    (let [feed (get-feed k)
          new-url (:location h)]
      (set-feed k (assoc feed :url new-url
                         :key new-url)))))