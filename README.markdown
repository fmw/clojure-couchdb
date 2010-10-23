#clojure-couchdb

Simple clojure library to interface with [Apache CouchDB](http://couchdb.apache.org/).

Depends on clojure-contrib and [clj-http](http://github.com/clj-sys/clj-http).


##Database Functions:

###database-list
    user=> (database-list "http://localhost:5984/")
    ["foo" "bar" "some-db" "another-database"]
###database-info
    user=> (database-info "http://localhost:5984/" "foo")
    {:instance_start_time "1242496720047538", :disk_size 4096, :compact_running false, :purge_seq 0, :update_seq 0, :doc_del_count 0, :doc_count 0, :db_name "foo"}
###database-create
    user=> (database-create "http://localhost:5984/" "new-db")
    "new-db"
###database-delete
    user=> (database-delete "http://localhost:5984/" "new-db")
    true
###database-compact
    user=> (database-compact "http://localhost:5984/" "foo")
    true

##Document Functions:

###document-list
    user=> (document-list "http://localhost:5984/" "some-db")
    ("doc1" "doc2" "another-doc" "yet-another-doc")
###document-get
    user=> (document-get "http://localhost:5984/" "some-db" "doc1")
    {:bar [1 2 3], :_rev "1-2326402976", :_id "doc1"}
    user=> (document-get "http://localhost:5984/" "some-db" "doc1" "1-2326402976")
    {:bar [1 2 3], :_rev "1-2326402976", :_id "doc1"}
###document-create
    user=> (document-create "http://localhost:5984/" "some-db" {:foo 42})
    {:foo 42, :_id "5bc3673322f38a4998aca23976acd4c6", :_rev "1-1799037045"}
    user=> (document-create "http://localhost:5984/" "some-db"  "my-doc" {:foo 42})
    {:foo 42, :_id "my-doc", :_rev "1-2310851567"}
###document-update
    user=> (let [doc (document-get "http://localhost:5984/" "some-db" "my-doc")]
             (document-update "http://localhost:5984/" "some-db" "my-doc" (assoc doc :bam true)))
    {:foo 42, :bam true, :_id "my-doc", :_rev "2-1148768398"}
###document-revisions
    user=> (document-revisions "http://localhost:5984/" "some-db" "my-doc")
    {"2-1148768398" "available", "1-2310851567" "available"}
###document-delete
    user=> (document-delete "http://localhost:5984/" "some-db" "my-doc")
    true
###document-bulk-update
    user=> (document-bulk-update "http://localhost:5984/" "some-db" [{:_id "someornoid1" :foo 42 :name "test"} {:_id "someid2" :foo 0 :name "me"}])
    user=> ({:_rev "1-949ae2b415db193b3a273f5205be8182", :_id "someornoid1", :foo 42, :name "test"} {:_rev "1-77d4ba9888f0490bf9656a5a91a374ff", :_id "someid2", :foo 0, :name "me"})

##Attachment Functions

###attachment-list
    user=> (attachment-list "http://localhost:5984/" "some-db" "my-doc")
    {"my-attachment" {:length 28, :content_type "text/plain", :stub true}, "f" {:length 3, :content_type "cntnt/type", :stub true}
###attachment-create
    user=> (attachment-create "http://localhost:5984/" "some-db" "my-doc" "new-attachment" "PAYLOAD" "text/plain")
    "new-attachment"
###attachment-get
    user=> (attachment-get "http://localhost:5984/" "some-db" "my-doc" "new-attachment")
    {:body-seq ("PAYLOAD"), :content-type "text/plain"}
###attachment-get-bin
    user=> (attachment-get-bin "http://localhost:5984/" "some-db" "my-doc" "new-attachment")
    {:body-seq #<byte[] [B@379d2f6b>, :content-type "image/png"}
###attachment-get-stream
    user=> (attachment-get-stream "http://localhost:5984/" "some-db" "my-doc" "new-attachment")
    {:body-stream #<HttpInputStream sun.net.www.protocol.http.HttpURLConnection$HttpInputStream@247ef45b>, :content-type "image/png"}
###attachment-delete
    user=> (attachment-delete "http://localhost:5984/" "some-db" "my-doc" "new-attachment")
    true

##View Functions

For now, views should be created/edited outside of this API.

In the following examples consider a simple view:

    "foobars": {
        "map": "function(doc) {  emit(doc.foobar, doc); }"
    }

###view-get
    user=> ; create some docs
    user=> (document-create "http://localhost:5984/" "some-db" "doc1" {:foobar 42})
    {:_rev "1-4227851621", :_id "doc1", :foobar 42}
    user=> (document-create "http://localhost:5984/" "some-db" "doc2" {:foobar 23})
    {:_rev "1-1185099016", :_id "doc2", :foobar 23}
    user=> ; run views
	user=> (use clojure.contrib.pprint) ; for pprint
    user=> (pprint (view-get "http://localhost:5984/" "some-db" "my-design-doc" "foobars"))
    {:rows
     [{:value {:foobar 23, :_rev "1-1185099016", :_id "doc2"},
       :key 23,
       :id "doc2"}
      {:value {:foobar 42, :_rev "1-4227851621", :_id "doc1"},
       :key 42,
       :id "doc1"}],
     :offset 0,
     :total_rows 2}
    nil
    user=> (pprint (view-get "http://localhost:5984/" "some-db" "my-design-doc" "foobars" {:descending true}))
    {:rows
     [{:value {:foobar 42, :_rev "1-4227851621", :_id "doc1"},
       :key 42,
       :id "doc1"}
      {:value {:foobar 23, :_rev "1-1185099016", :_id "doc2"},
       :key 23,
       :id "doc2"}],
     :offset 0,
     :total_rows 2}
    nil
    user=> (pprint (view-get "http://localhost:5984/" "some-db" "my-design-doc" "foobars" {:startkey 40}))
    {:rows
     [{:value {:foobar 42, :_rev "1-4227851621", :_id "doc1"},
       :key 42,
       :id "doc1"}],
     :offset 1,
     :total_rows 2}
    nil

Nested clojure terms are also allowed in view options (eg. as keys) -- they will be converted to json and then to proper url-encoded GET args.

###view-temp-get 

    user=> ; The following will produce the same output as the example above
    user=> (pprint (view-temp-get "http://localhost:5984/" "some-db" {:map "function(doc) { emit(doc.foobar, doc); }" }))

##Show Functions

###show-get
Returns the string-representation of a show from CouchDB

Consider a show named forty-two in the design-document test:
    
    function(doc,req) { 
      return { 
               body: \"<div>42</div>\" 
             }; 
    }

    user=> ; The latest field is the id. It's left blank in this example
    user=> (show-get "http://localhost:5984/" "some-db" "test" "forty-two" "")
    user=> ("<div>42</div>")

##Contributors

In no particular order:

* [robinbrandt](http://github.com/robinbrandt/)
* [Wojciech Kaczmarek](http://github.com/kunley)
* [Moritz Ulrich](http://github.com/the-kenny/)
* [Ryan Senior](http://github.com/senior)
* [Ronen](http://github.com/narkisr)

If I forgot you here, please send me a message. I just can't track down all commiters of this project.
