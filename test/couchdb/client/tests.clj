(ns couchdb.client.tests
  (:import (java.io FileInputStream))
  (:require (couchdb [client :as couchdb])
            (clojure.contrib [error-kit :as kit]))
  (:use (clojure test)
        [clojure.contrib.duck-streams :only [reader]]
        [clojure.contrib.java-utils :only [file]]))


(def +test-server+ "http://localhost:5984/")
(def +test-db+ "clojure-couchdb-test-database")
(def +test-db2+  "clojure-couchdb-test-database2")
(def +test-db3+  "clojure-couchdb-test-database3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         Utilities           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- qualify-sym [sym]
  (let [v (resolve sym)]
    (assert v)
    (apply symbol (map #(str (% (meta v))) [:ns :name]))))

(defmethod assert-expr 'raised? [msg [_ error-type & body :as form]]
  (let [error-name (qualify-sym error-type)]
    `(kit/with-handler
      (do
        ~@body
        (report {:type :fail
                 :message ~msg
                 :expected '~form
                 :actual ~(str error-name " not raised.")}))
      (kit/handle ~error-type {:as err#}
              (report {:type :pass
                       :message ~msg
                       :expected '~form
                       :actual nil})))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           Tests             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest databases
  ;; get a list of existing DBs
  (let [db-list (couchdb/database-list +test-server+)
        has-test-db? (some #{+test-db+} db-list)]
    ;; if the db exists, delete it
    (when has-test-db?
      (is (= (couchdb/database-delete +test-server+ +test-db+) true))
      (is (= (- (count db-list) 1)
             (count (couchdb/database-list +test-server+)))))
    ;; now create the db
    (is (= (couchdb/database-create +test-server+ +test-db+) +test-db+))
    (if has-test-db?
      (is (= (count db-list)
             (count (couchdb/database-list +test-server+))))
      (is (= (+ (count db-list) 1)
             (count (couchdb/database-list +test-server+))))))
  ;; now get info about the db
  (let [info (couchdb/database-info +test-server+ +test-db+)]
    (is (= (:db_name info) +test-db+))
    (is (= (:doc_count info) 0))
    (is (= (:doc_del_count info) 0))
    (is (= (:update_seq info) 0)))
  ;; compact the db
  ;(is (= (couchdb/database-compact +test-db+) true)) ; this conflicts with database deletion in release 0.9
)


(deftest documents
  ;; first get list of documents
  (let [docs (couchdb/document-list +test-server+ +test-db+)]
    (is (zero? (count docs)))
    ;; now create a document with a server-generated ID
    (let [doc (couchdb/document-create +test-server+ +test-db+ {:foo 1})]
      (is (= 1 (:foo (couchdb/document-get +test-server+
                                         +test-db+
                                         (:_id doc))))))
    ;; and recheck the list of documents
    (let [new-docs (couchdb/document-list +test-server+ +test-db+)]
      (is (= 1 (count new-docs))))
    ;; now make a new document with an ID we choose
    (let [new-doc (couchdb/document-create +test-server+ +test-db+
                                           "foobar" {:foo 1})]
      ;; and recheck the list of documents
      (let [new-docs (couchdb/document-list +test-server+ +test-db+)]
        (is (= 2 (count new-docs)))
        (is (= 1 (count (filter #(= % "foobar") new-docs)))))
      ;; and try to get the document back from the server
      (is (= (:foo (couchdb/document-get +test-server+ +test-db+ :foobar)) 1))
      ;; now let's update our document
      (is (= (:foo (couchdb/document-update +test-server+ +test-db+
                                            "foobar"
                                            (assoc new-doc :foo 5)) 5)))
      ;; and grab it back from the server just to make sure
      (is (= (:foo (couchdb/document-get +test-server+ +test-db+
                                         :foobar) 5))))
    ;; create a document that we're going to delete
    (let [tbd (couchdb/document-create +test-server+ +test-db+ "tbd" {})]
      ;; now delete the document
      (is (= (couchdb/document-delete +test-server+ +test-db+ "tbd") true))
      ;; and check that it's gone
      (is (raised? couchdb/DocumentNotFound
                   (couchdb/document-get +test-server+ +test-db+ "tbd"))))))



(deftest attachments
  (let [binary-attachment (byte-array [(byte 0x12)
				       (byte 0x23)
				       (byte 0x44)])]
    ;; list
    (is (= (couchdb/attachment-list +test-server+ +test-db+ "foobar") {}))
    ;; create textual 
    (is (= (couchdb/attachment-create +test-server+ +test-db+
				      "foobar" "my-attachment #1"
				      "ATTACHMENT DATA" "text/plain")
	   "my-attachment #1"))
    ;; create binary
    (is (= (couchdb/attachment-create +test-server+ +test-db+
				      "foobar" "attachment-2"
				      binary-attachment "application/binary"))
	"attachment-2")
				    
    ;; get textual
    (is (= (couchdb/attachment-get +test-server+ +test-db+
				   "foobar" "my-attachment #1")
	   {:body "ATTACHMENT DATA"
	    :content-type "text/plain; charset=utf-8"}))

    ;; get binary attachment and ensure it is what we uploaded
    (is (let [attachment (:body (couchdb/attachment-get +test-server+ +test-db+
							"foobar" "attachment-2"))
	      max_index (dec (count attachment))]
	  (loop [index 0]
	    (if (> index max_index)
	      true
	      (let [attachment_byte (nth attachment index)
		    data_byte (nth binary-attachment index)]
		(if (not (= attachment_byte data_byte))
		  'false
		  (recur (inc index))))))))	  
	  
      
    ;; re-check the list
    (let [atts (couchdb/attachment-list +test-server+ +test-db+ "foobar")
	  att1 (get atts "my-attachment #1")]
      (is (= (count atts) 2))
      (is (not (nil? att1)))
      (is (= (select-keys att1 [:length :content_type :stub])
	     {:length 15
	      :content_type "text/plain; charset=UTF-8"
	      :stub true})))
    ;; delete textual
    (is (= (couchdb/attachment-delete +test-server+ +test-db+
				      "foobar" "my-attachment #1") true))
    ;; delete binary
    (is (= (couchdb/attachment-delete +test-server+ +test-db+
				      "foobar" "attachment-2") true))
    ;; re-check the list again
    (is (= (couchdb/attachment-list +test-server+ +test-db+ "foobar") {}))
  
    ;; create with InputStream
    (if-not (.exists (file *file*))
      (println "File " *file* "not found. Skipping InputStream-Test")
      (do
	(let [istream (FileInputStream. *file*)]
	  (is (= (couchdb/attachment-create +test-server+ +test-db+
					    "foobar" "my-attachment #2"
					    istream "text/clojure")
		 "my-attachment #2")))
	;; get back the attachment we just created
	(let [istream (FileInputStream. *file*)]
	  (is (= (couchdb/attachment-get +test-server+ +test-db+
					 "foobar" "my-attachment #2")
		 {:body (line-seq (reader istream))
		  :content-type "text/clojure"})))))))

(deftest views
  (let [design-doc "viewdocs"]
    (let [view-name "testview-all"
	  js "function(doc) { emit(null, doc) }"]
      ;; add view    
      (is (not (nil? (couchdb/view-add +test-server+ +test-db+ design-doc view-name :map js))))
      ;; list - one view
      (is (-> (couchdb/view-list +test-server+ +test-db+ design-doc)
	      count
	      (= 1)))
      ;; execute view
      (is (-> (couchdb/view-get +test-server+ +test-db+ design-doc view-name)
	      :rows
	      count
	      (= 2))))
    (let [view-name "testview-some"
	  js "function(doc) { if (doc._id == 'foobar') emit(1, doc) }"]
      (is (not (nil? (couchdb/view-add +test-server+ +test-db+ design-doc view-name :map js))))
      (is (-> (couchdb/view-get +test-server+ +test-db+ design-doc view-name)
	      :rows
	      count
	      (= 1))))))

  


(deftest documents-passing-map
  ;; test that all the document-related functions work the same whether they
  ;; get passed a string or a map as the document

  ;; create two new documents, one for testing string names, one for testing
  ;; passing the doc-map itself
  (let [regdoc (couchdb/document-create +test-server+ +test-db+
                                        "regdoc" {1 2 3 4 "baz" "quux"})
        mapdoc (couchdb/document-create +test-server+ +test-db+
                                        "mapdoc" {1 2 3 4 "baz" "quux"})]
    ;; update
    (let [regdoc-return (couchdb/document-update +test-server+ +test-db+
                                                 "regdoc"
                                                 (assoc regdoc :foo 42))
          mapdoc-return (couchdb/document-update +test-server+ +test-db+
                                                 mapdoc (assoc mapdoc :foo 42))]
      (is (= (dissoc regdoc-return :_id :_rev)
             (dissoc mapdoc-return :_id :_rev))))
    ;; get most recent revision
    (let [regdoc-return (couchdb/document-get +test-server+ +test-db+ "regdoc")
          mapdoc-return (couchdb/document-get +test-server+ +test-db+ mapdoc)]
      (is (= (dissoc regdoc-return :_id :_rev)
             (dissoc mapdoc-return :_id :_rev))))
    ;; get specific revision
    (let [regdoc-return (couchdb/document-get +test-server+ +test-db+
                                              "regdoc" (:_rev regdoc))
          mapdoc-return (couchdb/document-get +test-server+ +test-db+
                                              mapdoc (:_rev mapdoc))]
      (is (= (dissoc regdoc-return :_id :_rev)
             (dissoc mapdoc-return :_id :_rev))))
    ;; revisions
    (let [regdoc-return (couchdb/document-revisions +test-server+ +test-db+
                                                    "regdoc")
          mapdoc-return (couchdb/document-revisions +test-server+ +test-db+
                                                    mapdoc)]
      (is (= (count regdoc-return) (count mapdoc-return))))
    ;; deletion
    (let [regdoc-return (couchdb/document-delete +test-server+ +test-db+
                                                 "regdoc")
          mapdoc-return (couchdb/document-delete +test-server+ +test-db+
                                                 mapdoc)]
      ;; are both return values true?
      (is (= regdoc-return mapdoc-return true))
      ;; make sure fetching both documents gives a DocumentNotFound error
      (is (raised? couchdb/DocumentNotFound
                   (couchdb/document-get +test-server+ +test-db+ "regdoc")))
      (is (raised? couchdb/DocumentNotFound
                   (couchdb/document-get +test-server+ +test-db+ mapdoc))))))


(deftest attachments-passing-map
  ;; test that all the attachment-related functions work the same whether they
  ;; get passed a string or a map as the document
  
  ;; create two new documents, one for testing string names, one for testing
  ;; passing the doc-map itself
  (let [regdoc (couchdb/document-create +test-server+ +test-db+
                                        "regdoc" {1 2 3 4 "baz" "quux"})
        mapdoc (couchdb/document-create +test-server+ +test-db+
                                        "mapdoc" {1 2 3 4 "baz" "quux"})]
    ;; creating
    (let [regdoc-return (couchdb/attachment-create +test-server+ +test-db+
                                                   "regdoc" "att1"
                                                   "payload" "text/plain")
          mapdoc-return (couchdb/attachment-create +test-server+ +test-db+
                                                   mapdoc "att1" "payload"
                                                   "text/plain")]
      (is (= regdoc-return mapdoc-return)))
    ;; listing
    (let [regdoc-return (couchdb/attachment-list +test-server+ +test-db+
                                                 "regdoc")
          mapdoc-return (couchdb/attachment-list +test-server+ +test-db+
                                                 mapdoc)]
      (is (= regdoc-return mapdoc-return)))
    ;; fetching
    (let [regdoc-return (couchdb/attachment-get +test-server+ +test-db+
                                                "regdoc" "att1")
          mapdoc-return (couchdb/attachment-get +test-server+ +test-db+
                                                mapdoc "att1")]
      (is (= regdoc-return mapdoc-return)))
    ;; deleting
    (let [regdoc-return (couchdb/attachment-delete +test-server+ +test-db+
                                                   "regdoc" "att1")
          mapdoc-return (couchdb/attachment-delete +test-server+ +test-db+
                                                   mapdoc "att1")]
      ;; are both return values true?
      (is (= regdoc-return mapdoc-return true))
      ;; make sure fetching both attachments gives a DocumentNotFound error
      (is (raised? couchdb/DocumentNotFound
                   (couchdb/attachment-get +test-server+ +test-db+
                                           "regdoc" "att1")))
      (is (raised? couchdb/DocumentNotFound
                   (couchdb/attachment-get +test-server+ +test-db+
                                           mapdoc "att1"))))))


(deftest cleanup
  ;; be a good citizen and delete the database we use for testing
  (is (= (couchdb/database-delete +test-server+ +test-db+) true)))


(deftest error-checking
  ;; try to access an invalid database name
  (is (raised? couchdb/InvalidDatabaseName
               (couchdb/database-info +test-server+ "#one")))
  ;; try to get DB that doesn't exist
  (is (raised? couchdb/DatabaseNotFound
               (couchdb/database-info +test-server+ +test-db+)))
  ;; create our test-db
  (is (= (couchdb/database-create +test-server+ +test-db+) +test-db+))
  ;; try to create it again
  (is (raised? couchdb/PreconditionFailed
               (couchdb/database-create +test-server+ +test-db+)))
  ;; try to grab non-extant document
  (is (raised? couchdb/DocumentNotFound
               (couchdb/document-get +test-server+ +test-db+ "foo")))
  ;; create a document with invalid JSON
  (is (raised? couchdb/ServerError
               (couchdb/document-create +test-server+ +test-db+
                                        "not a JSON object")))
  ;; create a document for reals this time
  (let [doc (couchdb/document-create +test-server+ +test-db+
                                     "foo" {:foo 42})]
    (is (= (:foo doc) 42))
    (is (= (:_id doc) "foo"))
    ;; try to update the document without sending the version
    (is (raised? couchdb/ResourceConflict
                 (couchdb/document-update +test-server+ +test-db+
                                          "foo" {:foo 43})))
    ;; update the document for real
    (couchdb/document-update +test-server+ +test-db+
                             "foo" (assoc doc :foo 43))
    ;; check that it updated
    (let [new-doc (couchdb/document-get +test-server+ +test-db+ "foo")]
      (is (= (:foo new-doc) 43))))
  ;; create an initial version of a document
  (let [first-rev (couchdb/document-create
                   +test-server+ +test-db+
                   "bam" {:answer "one"})]
    ;; test that we can just update the document straight up
    (is (= (:answer (couchdb/document-update
                     +test-server+ +test-db+
                     "bam" (assoc first-rev :answer "two")) "two")))
    ;; now try to insert with the wrong revision
    (is (raised? couchdb/ResourceConflict
                 (couchdb/document-update
                  +test-server+ +test-db+
                  "bam" (assoc first-rev :answer "three")))))
  ;; try to delete an attachment that doesn't exist
  (is (= true (couchdb/attachment-delete +test-server+ +test-db+ "bam" "f"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Conflict/Replication Testing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn test-db-fixture [db f]
     (try
       (couchdb/database-create +test-server+ db)
       (f)
       (finally
	(couchdb/database-delete +test-server+ db))))

(def test-db2-fixture (partial test-db-fixture +test-db2+))
(def test-db3-fixture (partial test-db-fixture +test-db3+))
(def test-db2-db3-fixture (compose-fixtures test-db2-fixture test-db3-fixture))

(def document-list* (partial couchdb/document-list +test-server+))
(def document-create* (partial couchdb/document-create +test-server+))
(def document-get-conflicts* (partial couchdb/document-get-conflicts +test-server+))
(def database-replicate* #(couchdb/database-replicate +test-server+ %1 +test-server+ %2))

(deftest replication-test
  (database-replicate* +test-db+  +test-db2+)
  (is (= (document-list* +test-db+)
	 (document-list* +test-db2+))))

(deftest single-conflict-test
  (let [doc1 (document-create* +test-db+ "conflict" {:foo 1})
	doc2 (document-create* +test-db2+ "conflict" {:bar 1})] 

    (database-replicate* +test-db+ +test-db2+)
    (let [conflicts (document-get-conflicts* +test-db2+ "conflict")]
      (is (= 1 (count conflicts)))
      (is (= (:_rev doc2) (first conflicts))))))

(deftest multiple-conflict-test
  (let [doc1 (document-create* +test-db+ "conflict2" {:foo 1})
	doc2 (document-create* +test-db2+ "conflict2" {:bar 1})
	doc3 (couchdb/document-update +test-server+ +test-db3+ "conflict2" {:baz 1})]

    (database-replicate* +test-db+ +test-db3+)
    (database-replicate* +test-db2+ +test-db3+)

    (let [conflicts
	  (document-get-conflicts* +test-db3+ "conflict2")]
      (is (= 2 (count conflicts)))
      (is (= (:_rev doc3) (first conflicts)))
      (is (= (:_rev doc2) (second conflicts))))))

(deftest no-conflict-test
  (document-create* +test-db+ "no-conflict4" {:foo 1})
  (document-create* +test-db2+ "no-conflict5" {:fo 1})
  (database-replicate* +test-db+ +test-db2+)
  (let [conflicts1
	  (document-get-conflicts* +test-db2+ "no-conflict4")
	conflicts2
	  (document-get-conflicts* +test-db2+ "no-conflict5")]
      (is (zero? (count conflicts1)))
      (is (zero? (count conflicts2)))))

(deftest single-conflict-resolve-test
  (let [doc1 (document-create* +test-db+ "conflict6" {:foo 1})
	doc2 (document-create* +test-db2+ "conflict6" {:bar 1})] 

    (database-replicate* +test-db+ +test-db2+)
    (let [conflicts (document-get-conflicts* +test-db2+ "conflict6")]
      (is (= 1 (count conflicts)))
      (is (= (:_rev doc2) (first conflicts)))
      (couchdb/document-resolve-conflict +test-server+ +test-db2+ "conflict6"
					 (first conflicts) merge)
      (let [conflicts (document-get-conflicts* +test-db2+ "conflict6")
	    merged-doc (couchdb/document-get +test-server+ +test-db2+ "conflict6")]

	(is (zero? (count conflicts)))
	(is (= 1 (:foo merged-doc)))
	(is (= 1 (:bar merged-doc)))))))

(deftest multiple-conflict-resolve-test
  (let [doc1 (document-create* +test-db+ "conflict7" {:foo 1})
	doc2 (document-create* +test-db2+ "conflict7" {:bar 1})
	doc3 (couchdb/document-update +test-server+ +test-db3+ "conflict7" {:baz 1})]

    (database-replicate* +test-db+ +test-db3+)
    (database-replicate* +test-db2+ +test-db3+)

    (let [conflicts (document-get-conflicts* +test-db3+ "conflict7")]
      
      (is (= 2 (count conflicts)))
      (is (= (:_rev doc3) (first conflicts)))
      (is (= (:_rev doc2) (second conflicts)))

      (couchdb/document-resolve-conflict +test-server+ +test-db3+ "conflict7"
					 (first conflicts) merge)
      (let [merged-doc (couchdb/document-get +test-server+ +test-db3+ "conflict7")
	    conflicts-after-merge (document-get-conflicts* +test-db3+ "conflict7")]

	(is (= 1 (count conflicts-after-merge)))
	(is (= (second conflicts)
	       (first conflicts-after-merge)))
	(is (= 1 (:foo merged-doc)))
	(is (= 1 (:baz merged-doc)))
	(is (nil? (:bar merged-doc)))))))

;;; test-ns-hook is used to run tests in the specified order
(defn test-ns-hook []
  (databases)
  (documents)
  (attachments)
  (views)
  (documents-passing-map)
  (attachments-passing-map)
  (test-db2-fixture replication-test)
  (test-db2-fixture single-conflict-test)
  (test-db2-db3-fixture multiple-conflict-test)
  (test-db2-fixture single-conflict-resolve-test)
  (test-db2-db3-fixture multiple-conflict-resolve-test)
  (cleanup)
  (error-checking)
  (cleanup))
  

