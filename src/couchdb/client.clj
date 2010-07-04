(ns couchdb.client
  (:require [clojure.contrib [error-kit :as kit]])
  (:use [clojure.contrib.java-utils :only [as-str]]
        [clojure.contrib.json :only [json-str read-json]]
        [clojure-http.client :only [request url-encode]]))

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


(defn couch-request
  [& args]
  (let [response (apply request args)
        result (try (assoc response :json
                           (read-json (apply str (:body-seq response)) true))
                 ;; if there's an error reading the JSON, just don't make a :json key
                 (catch Exception e 
                   response))]
    (if (>= (:code result) 400)
      (kit/raise* ((condp = (:code result)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         Utilities           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;          Databases          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn database-list 
  [server]
  (:json (couch-request (str (normalize-url server) "_all_dbs"))))

(defn database-create
  [server database]
  (when-let [database (validate-dbname database)]
    (couch-request (str (normalize-url server) database) :put)
    database))

(defn database-delete
  [server database]
  (when-let [database (validate-dbname database)]
    (couch-request (str (normalize-url server) database) :delete)
    true))

(defn database-info
  [server database]
  (when-let [database (validate-dbname database)]
    (:json (couch-request (str (normalize-url server) database)))))

(defn database-compact
  [server database]
  (when-let [database (validate-dbname database)]
    (couch-request (str (normalize-url server) database "/_compact") :post)
    true))


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
    (let [response (:json (couch-request
                           (str (normalize-url server)
                                database
                                (when id
                                  (str "/" (url-encode (as-str id)))))
                           method
                           {"Content-Type" "application/json"}
                           {}
                           (json-str payload)))]
      (merge payload
             {:_id (:id response)
              :_rev (:rev response)}))))

(defn document-list
  ([server database]
     (when-let [database (validate-dbname database)]
       (map :id (:rows (:json (couch-request (str (normalize-url server)
                                                  database
                                                  "/_all_docs")))))))
  ([server database & [options]]
     (when-let [database (validate-dbname database)] 
       (map (if (:include_docs options) :doc :id)
            (:rows (:json (couch-request
                           (str (normalize-url server) database "/_all_docs?"
                                (url-encode (vals2json options))))))))))
       

(defn document-create
  ([server database payload]
     (do-document-touch (normalize-url server) database payload nil :post))
  ([server database id payload]
     (do-document-touch (normalize-url server) database payload id :put)))

(defn document-update
  [server database id payload]
  ;(assert (:_rev payload)) ;; payload needs to have a revision or you'll get a PreconditionFailed error
  (let [id (do-get-doc database id)]
    (do-document-touch (normalize-url server) database payload id :put)))

(defn document-get
  ([server database id]
     (when-let [database (validate-dbname database)]
       (let [id (do-get-doc database id)]
         (:json (couch-request (str (normalize-url server) database "/"
                                    (url-encode (as-str id))))))))
  ([server database id rev]
     (when-let [database (validate-dbname database)]
       (let [id (do-get-doc database id)]
         (:json (couch-request (str (normalize-url server) database "/"
                                    (url-encode (as-str id)) "?rev=" rev)))))))

(defn document-delete
  [server database id]
  (if-not (empty? id)
    (when-let [database (validate-dbname database)]
      (let [id (do-get-doc database id)
            rev (do-get-rev (normalize-url (normalize-url server)) database id)]
        (couch-request (str (normalize-url server) database "/"
                            (url-encode (as-str id)) "?rev=" rev)
                       :delete)
        true))
    false))


(defn document-bulk-update
  "Does a bulk-update to couchdb, accoding to
http://wiki.apache.org/couchdb/HTTP_Bulk_Document_API"
  [server database document-coll & [request-options]]
  (when-let [database (validate-dbname database)]
    (let [response (:json
                    (couch-request
                     (str (normalize-url server) database "/_bulk_docs"
                          (url-encode (vals2json request-options)))
                     :post
                     {"Content-Type" "application/json"}
                     {}
                     (json-str {:docs document-coll})))]
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

(defn document-revisions
  [server database id]
  (when-let [database (validate-dbname database)]
    (let [id (do-get-doc database id)]
      (apply merge (map (fn [m]
                          (sorted-map-by revision-comparator (:rev m) (:status m)))
                        (:_revs_info (:json (couch-request (str (normalize-url server) database "/" (url-encode (as-str id)) "?revs_info=true")))))))))

;; Views

(defn view-get [server db design-doc view-name & [view-options]]
  (:json (couch-request 
   (str (normalize-url server) db "/_design/" design-doc "/_view/" view-name "?"
	(url-encode (vals2json view-options))))))

(defn view-temp-get [server db view-map & [view-options]]
  (:json (couch-request 
          (str (normalize-url server) db "/_temp_view?"
               (url-encode (vals2json view-options)))
          :post
          {"Content-Type" "application/json"}
          {}
          (json-str view-map))))

(defn view-create
  "Create a map/reduce view.  The new view is a clojure-map with keys of :map and
  (optionally) :reduce.  Values are the string representations of the functions
  in the language of the design-doc (usually javascript)."
  [server db design-name view-name view-map]
  (let [design-key (str "_design/" design-name)]
    (when-let [design-doc (document-get server db design-key)]
      (document-update server db design-key
                       (assoc design-doc :views (assoc (:views design-doc) (keyword view-name) view-map))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Attachments          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn attachment-list
  [server database document]
  (let [document (do-get-doc database document)]
    (into {} (map stringify-top-level-keys
                  (:_attachments (document-get (normalize-url server)
                                               database document))))))


(defn attachment-create
  [server database document id payload content-type]
  (when-let [database (validate-dbname database)]
    (let [document (do-get-doc database document)
          rev (do-get-rev (normalize-url server) database document)]
      (couch-request (str (normalize-url server) database "/"
                          (url-encode (as-str document)) "/"
                          (url-encode (as-str id)) "?rev=" rev)
                     :put
                     {"Content-Type" content-type}
                     {}
                     payload))
    id))

(defn attachment-get
  [server database document id]
  (when-let [database (validate-dbname database)]
    (let [document (do-get-doc database document)
          response (couch-request (str (normalize-url server) database "/"
                                       (url-encode (as-str document)) "/"
                                       (url-encode (as-str id))))]
      {:body-seq (:body-seq response)
       :content-type ((:get-header response) "content-type")})))

(defn attachment-delete
  [server database document id]
  (when-let [database (validate-dbname database)]
    (let [document (do-get-doc database document)
          rev (do-get-rev (normalize-url server) database document)]
      (couch-request (str (normalize-url server) database "/"
                          (url-encode (as-str document)) "/"
                          (url-encode (as-str id)) "?rev=" rev)
                     :delete)
      true)))

;; Shows

(defn show-get
  "Returns the contents of a show as a list of strings according to
 http://wiki.apache.org/couchdb/Formatting_with_Show_and_List"
  [server database design-doc show-name id & [show-options]]
  (:body-seq
   (couch-request 
    (str (normalize-url server) database "/_design/"
         design-doc "/_show/" show-name "/"
         id "?" (url-encode (vals2json show-options))))))
