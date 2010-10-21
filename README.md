##Fetcher

A production scale HTTP fetcher.

A large scale fetcher should be limited by the speed of fetching results and streaming them to disk.  We don't want other communication or processing happening in the fetcher.  Rather, other services listen on the fetcher's results and pick up work by polling disk or listening on some channel.

We still have another phase of tuning ahead, but the first step is that, in addition to using Work for threading the fetcher, we also use an async http client.

##Async HTTP client with NIO

We did a bunch of research, and decided to start with Ning's NIO-based async http client.

Some folks suggested rolling our own solution using evening a la epoll, or directly using java.nio.channels.SelectorProvider.  We try to use other libs when we can, so we avoided this and will do it only if forced to for performance reasons.

If you use fetcher, you don't need to worry about the details of the async http client, as you just pass your put-done fn into Work as usual.

In case you case, you can see how we've wired up the fetch function to wrap the Ning library and use Work's put-done as Ning's response-callback.

    (defn dispatch-generator
      "Return a fn to handle a completed response."
      [feed-key feed-url response-callback]
      (fn [state]
        (let [code (-> (c/status state) :code)
              headers (c/headers state)
              body (-> (c/body state) .toString)]
          (response-callback feed-key
                             feed-url
                             code
                             headers
                             body)
          [true :continue])))

    (defn fetch
      "fetch a feed for updates.  Responses are handled asynchronously by the provided callback.

      The callback should accept five arguments: k, u, response code, headers, and body."
      [[k u & headers] put-done]
      (let [callbacks (merge async-req/*default-callbacks*
                             {:status status-check
                              :completed (dispatch-generator k u put-done)})
            req (async-req/prepare-request :get
                     u
                     :headers headers)
        resp (apply async-req/execute-request
                        req
                        (apply concat callbacks))]
        resp))

This is most of the core of fetcher's plumbing.  The other notable part is the handlers, which are just a way of putting your put-done function together with functions that handle redirects and other non-OK (not 200) status codes.

##Async worker pools with Work

The other change we made for async is in Work.  Work used to call the put-done function with the results of the work function, but the async case is more of a continuation passing style - injecting the put-function into the work function so that it can be supplied to the async http client as the handler function.  This is the only different in Work, so we won't dwell on it, as it is straightforward.

From the client code perspective, you create an ansyc work poll by passing an :async keyword as the last argument to Work's queue-work function.

    (defn fetch-pool
      [get-work put-done]
      (work/queue-work
       fetch
       get-work
       put-done
       (work/available-processors)
       :async))

##Notes and Next Steps

Some other notables if you want to start using fetcher.

Handlers take: [key, url, headers, body]

Items in the fetch queue are always of the form: [key url & header-map]

If you want to see examples of workflows that use fetcher as a step in a more complex pipeline, or how to deploy fetcher using crane, check out the crawler, which we will write about soon.

Once we get fetcher working together with the rest of the systems we are building, we'll go through a wave of fine tuning for things like caching DNS lookups, HTTP pipelining, and ensuring that our writes are not a bottleneck (which current multicast to both disk and to a queue).  We'll also focus on robustness to non-standard redirects, character set detection, etc.