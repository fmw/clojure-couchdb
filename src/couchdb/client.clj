(ns couchdb.client
  (:require [clojure.contrib [error-kit :as kit]]
	    [clj-http.core :as http-core]
	    [clj-http.client :as http-client])
  (:use [clojure.contrib.java-utils :only [as-str]]
        [clojure.contrib.json :only [read-json json-str]]
	[clojure.contrib.str-utils :only [str-join]])
  (:import (java.net URLEncoder)))

(kit/deferror InvalidDatabaseName [] [database]
  {:msg (str "Invalid Database Name: " database)
   :unhandled (kit/throw-msg Exception)})

(kit/deferror DatabaseNotFound [] [e]
  {:msg (str "Database Not Found: " e)
   :unhandled (kit/throw-msg java.io.FileNotFoundException)})

(kit/deferror DocumentNotFound [] [e]
  {:msg (str "Document Not Found: " e)
   :unhandled (kit/throw-msg java.io.FileNotFoundException)})

(kit/deferror AttachmentNotFound [] [e]
  {:msg (str "Attachment Not Found: " e)
   :unhandled (kit/throw-msg java.io.FileNotFoundException)})

(kit/deferror ResourceConflict [] [e]
  "Raised when a 409 code is returned from the server."
  {:msg (str "Resource Conflict: " e)
   :unhandled (kit/throw-msg Exception)})

(kit/deferror PreconditionFailed [] [e]
  "Raised when a 412 code is returned from the server."
  {:msg (str "Precondition Failed: " e)
   :unhandled (kit/throw-msg Exception)})

(kit/deferror ServerError [] [e]
  "Raised when any unexpected code >= 400 is returned from the server."
  {:msg (str "Unhandled Server Error: " e)
   :unhandled (kit/throw-msg Exception)})


(def #^{:doc
  "Executes the HTTP request corresponding to the given map and returns the
   response map for corresponding to the resulting HTTP response.

   In addition to the standard Ring request keys, the following keys are also
   recognized:
   * :url
   * :method
   * :query-params
   * :basic-auth
   * :content-type
   * :accept
   * :accept-encoding
   * :as

  The following additional behaviors over also automatically enabled:
   * No exceptions are thrown for status codes
   * Gzip and deflate responses are accepted and decompressed
   * Input and output bodies are coerced as required and indicated by the :as
     option."}
  http-request
  (-> #'http-core/request
    http-client/wrap-redirects
    http-client/wrap-decompression
    http-client/wrap-input-coercion
    http-client/wrap-output-coercion
    http-client/wrap-query-params
    http-client/wrap-basic-auth
    http-client/wrap-accept
    http-client/wrap-accept-encoding
    http-client/wrap-content-type
    http-client/wrap-method
    http-client/wrap-url))


(defn couch-request*
  [response]
  (let [result (try (assoc response :json
			   (read-json (apply str (:body response))))
                    ;; if there's an error reading the JSON, just don't make a :json key
                    (catch Exception e 
                      response))]
    (if (>= (:status result) 400)
      (kit/raise* ((condp = (:status result)
		       404 (condp = (:reason (:json result))
			       ;; before svn rev 775577 this should be "no_db_file"
			       "no_db_file" DatabaseNotFound  
			       "Document is missing attachment" AttachmentNotFound
			       DocumentNotFound)
		       409 ResourceConflict
		       412 PreconditionFailed
		       ServerError)
                   {:e (:json result)}))
      result)))




(defn couch-request [args]
  (couch-request* (http-request (merge {:method :get} args))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         Utilities           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn url-encode
  "Wrapper around java.net.URLEncoder returning a (UTF-8) URL encoded
representation of argument, either a string or map."
  [arg]
  (if (map? arg)
    (str-join \& (map #(str-join \= (map url-encode %)) arg))
    (URLEncoder/encode (as-str arg) "UTF-8")))

(defn valid-dbname?
  [database]
  (boolean (re-find #"^[a-z][a-z0-9_$()+-/]*$" database)))

(defn validate-dbname
  [database]
  (if (valid-dbname? database)
    (url-encode database)
    (kit/raise InvalidDatabaseName database)))

(defn stringify-top-level-keys
  [[k v]]
  (if (keyword? k)
    [(if-let [n (namespace k)]
       (str n (name k))
       (name k))
     v]
    [k v]))

(defn- normalize-url
  "If not present, appends a / to the url-string."
  [url]
  (if-not (= (last url) \/)
    (str url \/ )
    url))

(defn- vals-lift [f m]
  (reduce (fn [acc [k v]] (assoc acc k (f v))) {} (seq m)))

(def #^{:private true} vals2json (partial vals-lift json-str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       Utility Macros       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- vars-to-rebind []
  (letfn [(rebind? [var]
                   (let [m (meta var)]
                    (and (= (:ns m)
                            (find-ns 'couchdb.client))
                         (:rebind m))))]
   (filter rebind? (vals (ns-map *ns*)))))

(defmacro with-server
  "with-server rebinds all couchdb functions which take a server argument with
the first argument, so you can call the functions without the server argument.

Example:
;(with-server http://localhost:5984
;  (database-list))"

  [server & body]
  (let [ssharp (gensym "server-")]      ;necessary because nested-`
   `(let [~ssharp ~server]
      (with-bindings ~(apply hash-map
                             (mapcat #(vector % `(partial (var-get ~%) ~ssharp))
                             (vars-to-rebind)))
        (do
          ~@body)))))

;; (with-server (apply str (concat "http://" "localhost" ":5984")) (database-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Databases          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn #^{:rebind true} database-list 
  [server]
  (:json (couch-request {:url (str (normalize-url server) "_all_dbs"),
			 :method :get})))

(defn #^{:rebind true} database-create
  [server database]  
  (when-let [database (validate-dbname database)]
    (couch-request {:url (str (normalize-url server) database),
		    :method :put})
    database))
    
(defn #^{:rebind true} database-delete
  [server database]
  (when-let [database (validate-dbname database)]
    (couch-request {:url (str (normalize-url server) database),
		    :method :delete})
    true))

(defn #^{:rebind true} database-info
  [server database]
  (when-let [database (validate-dbname database)]
    (:json (couch-request {:url (str (normalize-url server) database),
			   :method :get}))))

(defn #^{:rebind true} database-compact
  [server database]
  (when-let [database (validate-dbname database)]
    (couch-request {:url (str (normalize-url server) database "/_compact"),
		    :method :post})
    true))


(defn #^{:rebind true} database-replicate
  [src-server src-database target-server target-database]
  (couch-request {:url (str (normalize-url target-server) "_replicate"),
		  :method :post,
		  :content-type :json,
		  :body (json-str {"source" (str (normalize-url src-server) src-database),
				   "target" target-database})}))
		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         Documents           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare document-get)

(defn- do-get-doc
  [database document]
  (if (map? document)
    (if-let [id (:_id document)]
      id
      (kit/raise ResourceConflict "missing :_id key"))
    document))

(defn- do-get-rev
  [server database document]
  (if (map? document)
    (if-let [rev (:_rev document)]
      rev
      (kit/raise ResourceConflict "missing :_rev key"))
    (:_rev (document-get (normalize-url server) database document))))

(defn- do-document-touch
  [server database payload id method]
  (when-let [database (validate-dbname database)]
    (let [response (:json (couch-request {:url (str (normalize-url server)
						    database
						    (when id (str "/" (url-encode (as-str id))))),
					  :method method
					  :content-type :json
					  :body (json-str payload)}))]					       
      (merge payload
             {:_id (:id response)
              :_rev (:rev response)}))))

(defn #^{:rebind true} document-list
  ([server database]
     (when-let [database (validate-dbname database)]
       (map :id (:rows 
		 (:json (couch-request {:url (str (normalize-url server)
                                                  database
                                                  "/_all_docs")}))))))
  ([server database & [options]]
     (when-let [database (validate-dbname database)] 
       (map (if (:include_docs options) :doc :id)
            (:rows (:json (couch-request {:url (str (normalize-url server) database "/_all_docs?"
						    (url-encode (vals2json options)))})))))))

(defn #^{:rebind true} document-create
  ([server database payload]
     (do-document-touch (normalize-url server) database payload nil :post))
  ([server database id payload]
     (do-document-touch (normalize-url server) database payload id :put)))

(defn #^{:rebind true} document-update
  [server database id payload]
  ;(assert (:_rev payload)) ;; payload needs to have a revision or you'll get a PreconditionFailed error
  (let [id (do-get-doc database id)]
    (do-document-touch (normalize-url server) database payload id :put)))

(defn #^{:rebind true} document-get
  ([server database id]
     (when-let [database (validate-dbname database)]
       (let [id (do-get-doc database id)]
         (:json (couch-request {:url (str (normalize-url server) database "/"
					  (url-encode (as-str id)))})))))

  ([server database id rev]
     (when-let [database (validate-dbname database)]
       (let [id (do-get-doc database id)]
         (:json (couch-request {:url (str (normalize-url server) database "/"
					  (url-encode (as-str id)) "?rev=" rev)}))))))


(defn #^{:rebind true} document-delete
  ([server database id rev]
     (if-not (empty? id)
	(when-let [database (validate-dbname database)]
	  (let [id (do-get-doc database id)]
	    (couch-request {:url (str (normalize-url server) database "/"
				      (url-encode (as-str id)) "?rev=" rev),
			    :method :delete})
	    true))
	false))
  ([server database id]
      (if-not (empty? id)
	(when-let [database (validate-dbname database)]
	  (let [id (do-get-doc database id)]
	    (document-delete server database id
			     (do-get-rev (normalize-url server)
					 database id))))
	false)))

(defn #^{:rebind true} document-bulk-update
  "Does a bulk-update to couchdb, accoding to
http://wiki.apache.org/couchdb/HTTP_Bulk_Document_API"
  [server database document-coll & [request-options]]
  (when-let [database (validate-dbname database)]
    (let [response (:json
		    (couch-request {:url (str (normalize-url server) database "/_bulk_docs"
					      (url-encode (vals2json request-options))),
				    :method :post,
				    :content-type :json,
				    :body (json-str {:docs document-coll})}))]
      ;; I don't know if this is correct... I assume that the server sends the
      ;; ids and revs in the same order as ib my request back.
      (map (fn [respdoc, orgdoc]
             (merge orgdoc
                    {:_id (:id respdoc)
                     :_rev (:rev respdoc)}))
           response document-coll))))

(defn- revision-comparator
  [x y]
  (> (Integer/decode (apply str (take-while #(not= % \-) x)))
     (Integer/decode (apply str (take-while #(not= % \-) y)))))

(defn #^{:rebind true} document-revisions
  [server database id]
  (when-let [database (validate-dbname database)]
    (let [id (do-get-doc database id)]
      (apply merge (map (fn [m]
                          (sorted-map-by revision-comparator (:rev m) (:status m)))
                        (:_revs_info (:json
				      (couch-request {:url (str (normalize-url server) database "/"
								(url-encode (as-str id)) "?revs_info=true")}))))))))

(defn- url-encode-str [s]
  (-> s
      as-str
      url-encode))

(defn #^{:rebind true} document-get-conflicts
  "Returns a list of document revisions that conflict with the doc id"
  [server database id]
  (when-let [database (validate-dbname database)]
    (:_conflicts (:json (couch-request {:url (str (normalize-url server) database "/"
						  (url-encode-str (do-get-doc database id)) "?conflicts=true"),
				:content-type :json})))))

    

(defn #^{:rebind true} document-resolve-conflict
  "Function for resolving a conflicted document takes a server,
   database, document id, conflict revision (see document-get-conflicts)
   and a function that takes two args, the first conflicted doc, the second
   the current doc"
  [server database id conflict-rev resolve-fn]
  (when-let [database (validate-dbname database)]
    (let [merged-doc (resolve-fn (document-get server database id conflict-rev)
				 (document-get server database id))]
      (document-update server database id merged-doc)
      (document-delete server database id conflict-rev))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Views            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-couchdb-params [options & to-jsonify]
  "Return a URL-encoded parameter string of 'options', JSON-transforming those parameters in 'to-jsonify'"
  (let [jsonified-options (vals2json (select-keys options to-jsonify))
	regular-options (apply dissoc options to-jsonify)]
    (merge regular-options jsonified-options)))


(defn #^{:rebind true} view-list [server db design-doc]
  "Get a list of views, including their definitions"
  (:views (:json (couch-request {:url (str (normalize-url server) db "/_design/" design-doc) }))))


(defn #^{:rebind true} view-add [server db design-doc view-name type js]
  "Add a JavaScript view to a design document in a database. Type is one of :map or :reduce.
   Overwrites any previous view of the same name and type."
  (with-server server
    (let [doc-path (str "_design/" design-doc)]
      (kit/with-handler
	(let [doc-content (document-get db doc-path)]
	  (document-update db doc-path (assoc-in doc-content [:views view-name type] js)))
	(kit/handle DocumentNotFound []
	  (document-create db doc-path
			   {:language "javascript"
			    :views {(keyword view-name)
				    {(keyword type) js}}}))))))
	  

(defn #^{:rebind true} view-get [server db design-doc view-name & [view-options]]
  (:json (couch-request {:url (str (normalize-url server) db "/_design/" design-doc "/_view/" view-name "?"
				   (url-encode (make-couchdb-params view-options :key :startkey :endkey)))})))

(defn #^{:rebind true} view-temp-get [server db view-map & [view-options]]
  (:json (couch-request {:url (str (normalize-url server) db "/_temp_view?"
				   (url-encode (make-couchdb-params view-options :key :startkey :endkey)))
			 :method :post,
			 :content-type :json,
			 :body (json-str view-map)})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Attachments          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn #^{:rebind true} attachment-list
  [server database document]
  (let [document (do-get-doc database document)]
    (into {} (map stringify-top-level-keys
                  (:_attachments (document-get (normalize-url server)
                                               database document))))))

(defn #^{:rebind true} attachment-create
  [server database document id payload content-type]
  (when-let [database (validate-dbname database)]
    (let [document (do-get-doc database document)
          rev (do-get-rev (normalize-url server) database document)]
      (couch-request {:url (str (normalize-url server) database "/"
				(url-encode (as-str document)) "/"
				(url-encode (as-str id)) "?rev=" rev),
		      :method :put,
		      :content-type content-type,
		      :body payload}))
    id))

(defn attachment-get
  "returns the attachment as a seq of strings in :body"
  [server database document id]
  (when-let [database (validate-dbname database)]
    (let [document (do-get-doc database document)
          response (couch-request {:url (str (normalize-url server) database "/"
					     (url-encode (as-str document)) "/"
					     (url-encode (as-str id)))
				   :as :byte-array})]
					     
      {:body (:body response)
       :content-type (get (:headers response) "content-type")})))


(defn attachment-delete
  [server database document id]
  (when-let [database (validate-dbname database)]
    (let [document (do-get-doc database document)
          rev (do-get-rev (normalize-url server) database document)]
      (couch-request {:url (str (normalize-url server) database "/"
				(url-encode (as-str document)) "/"
				(url-encode (as-str id)) "?rev=" rev),
		      :method :delete})
      true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Shows            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn #^{:rebind true} show-get
  "Returns the contents of a show as a list of strings according to
 http://wiki.apache.org/couchdb/Formatting_with_Show_and_List"
  [server database design-doc show-name id & [show-options]]
  (:body
   (couch-request {:url (str (normalize-url server) database "/_design/"
			     design-doc "/_show/" show-name "/"
			     id "?" (url-encode (vals2json show-options)))})))



