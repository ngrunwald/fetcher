(ns fetcher.core-test
  (:use clojure.test
        plumbing.core
        fetcher.core)
  (:require [work.core :as work])
  (:import java.util.concurrent.Executors))

(declare test-feed-urls)

(defn- promise-map [m]
  (reduce
   (fn [res [k v]]
     (let [p (promise)]
       (deliver p v)
       (assoc res k p)))
   {} m))

(deftest callback-test
  (let [m (atom nil)
	callback (fn [{body :body}]
		     (swap! m (constantly body)))
	dg (with-url "key" "http://dl.dropbox.com/u/1205228/test.txt"
	     callback)
	state (promise-map {
			    :status {:code 200}
			    :headers {:content-type "text/plain; charset=ascii"}
			    :body (let [bs (.getBytes "This is a test")
					baos (java.io.ByteArrayOutputStream.)]
				    (.write baos bs 0 (count bs))
				    baos) })]
    (dg state)
    (is (= @m "This is a test"))))

(deftest fetch-test
  (let [m (promise)
	callback (fn [{b :body}]
		     (deliver m b))]
    (fetch {:key "key" :url "http://dl.dropbox.com/u/1205228/test.txt"} callback)
    (is (= @m "This is a test.\n"))))

(defn queue-input
  [count]
  (let [xs (atom (take count (cycle test-feed-urls)))]
    (fn []
      (let [x (first @xs)]
        (swap! xs rest)
        (if x {:key x :url x}
            nil)))))

(deftest ^{:performance true}
  async-req-test
  (set-log-level! ["com.ning.http.client.providers.NettyAsyncHttpProvider"] :error)
  (let [num-fetches 1000
        min-responses (* 0.9 num-fetches)
        producer (queue-input num-fetches)
        finished (atom [])
        pool (work/queue-work {:f (with-log :error fetch)
                               :in producer
                               :out (fn [x] (swap! finished conj (:url x)))
                               :threads (work/available-processors)
                               :exec work/async})]
    (time (wait-until #(>= (count @finished) min-responses) 300))
    (.shutdown pool)
    (is (>= (count @finished) min-responses))))

(def test-feed-urls
     ["http://daringfireball.net/index.xml" "http://www.rollingstone.com/siteServices/rss/allNews" "http://feeds.feedburner.com/iamwhitePosterous" "http://www.9to5mac.com/feed" "http://abovethecrowd.com/feed/" "http://feeds.feedburner.com/pulsosocial" "http://www.bytearray.org/?feed=rss2" "http://usabilitytestinghowto.blogspot.com/feeds/posts/default" "http://mgalligan.com/rss" "http://feeds.searchengineland.com/searchcap" "http://feeds.feedburner.com/palletops" "http://googledevjp.blogspot.com/feeds/posts/default" "http://blog.boxee.tv/feed/" "http://twitgoo.com/1r6osl/?format=rss" "http://davidrcole.com/feed/" "http://milancermak.posterous.com/rss.xml" "http://typedia.com/blog/feed/" "http://diveintohtml5.org//hg.diveintohtml5.org/hgweb.cgi/atom-log" "http://www.curious-creature.org/feed/" "http://networkeffect.allthingsd.com/feed/" "http://feeds.feedburner.com/garry" "http://feeds.feedburner.com/devourfeed" "http://www.princeton.edu/main/feed.xml" "http://www.mdshakes.org/feed/" "http://blog.javorek.net/feed/" "http://www.dataists.com/feed/" "http://kinecthacks.net/feed/" "http://www.dailymile.com/opensearch.xml" "http://feeds.g4tv.com/g4tv/thefeed" "http://www.good.is/rss/main" "http://cityroom.blogs.nytimes.com/feed/" "http://store.androidandme.com/rss/rssnewitems.php" "http://www.whedonesque.com/rss.xml.php" "http://www.cs.umass.edu/frontpage/feed" "http://scripting.com/rss.xml" "http://cms.myspacecdn.com/cms/api/opensearch_people.xml" "http://frozencanuck.wordpress.com/feed/" "http://hbaseblog.com/feed/" "http://www.trueventures.com/xmlrpc.php" "http://opinionator.blogs.nytimes.com/feed/" "http://www.kurzweilai.net/xmlrpc.php?rsd" "http://www.railway-technology.com/news-rss.xml" "http://heartbreaknympho.com/feed/" "http://mockupstogo.net/rss.xml" "https://github.com/buildbox/contentcheck-maven-plugin/commits/master.atom" "http://feliciaday.com/feed" "http://blog.makezine.com/archive/make_store/index.xml" "http://www.zdnet.com/search?t=1,7&mode=rss" "http://www.techdirt.com/techdirt_rss.xml" "http://www.forkparty.com/feed/" "http://feeds.feedburner.com/GoogleOpenSourceBlog" "http://crazybob.org/roller/rss/crazybob" "http://blogs.strat-cons.com/?feed=rss2" "http://wp.appadvice.com/xmlrpc.php" "http://answers.onstartups.com/feeds/question/329" "http://sleepinghedgehog.com/feed/" "http://www.heureka.cz/direct/firefox/search.xml" "http://x264dev.multimedia.cx/feed" "http://allthestuffido.com/rss.xml" "http://lab.andre-michelle.com/feed/" "http://beervana.blogspot.com/feeds/posts/default" "http://battellemedia.com/index.xml" "http://www.letsredu.com/xmlrpc.php" "http://joeposnanski.blogspot.com/feeds/posts/default" "http://feeds.feedburner.com/5by5" "http://picasaweb.google.com/data/feed/base/user/ukazteiphone/albumid/5538997136458996465?alt=rss&kind=photo&hl=en_US" "http://www.investorplace.com/feed/" "http://design-daily.com/feed/" "http://www.podnikanivusa.com/feed/" "https://github.com/dochang/asia/commits/master.atom" "http://aria42.wordpress.com/feed/" "http://feeds.venturehacks.com/venturehacks" "http://insidescoopsf.sfgate.com/paololucchesi/feed/" "http://blog.coderanger.net/atom.xml" "http://www.blogger.com/feeds/20663591/posts/default" "http://www.aptima.com/news.xml" "http://politics.nytimes.com/congress/votes/house/atom" "http://feeds.feedburner.com/KevinKelly" "http://pixycz.posterous.com/rss.xml" "http://www.letemsvetemapplem.eu/wp-includes/wlwmanifest.xml" "http://www.asjava.com/feed/" "http://blog.summation.net/rss.xml" "http://corduroyclub.com/feed" "http://www.w3.org/News/atom.xml" "http://wiselivingblog.com/feed/" "http://arronla.com/feed/" "http://feeds.feedburner.com/seomoz" "http://www.tmcamp.com/feed/" "http://feeds.feedburner.com/scoutmob/nyc" "http://streeteasy.com/meta/nyc_opensearch_plugin.xml" "http://blog.getcloudapp.com/feed.xml" "http://www.cindyalvarez.com/feed/" "http://www.mactalk.com.au/feed/" "http://cdixon.org/feed/" "http://blog.backblaze.com/feed/" "http://blog.mozilla.com/feed/" "http://www.adafruit.com/blog/feed/" "atom" "http://alanvanroemburg.tumblr.com/rss" "http://www.funnyordie.com/videos.rss" "http://hellohealth.com/feed/" "http://www.tofugu.com/feed/"])