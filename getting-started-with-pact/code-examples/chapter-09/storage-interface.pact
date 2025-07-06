;; storage-interface.pact
;; Generic storage interface with multiple implementations

(interface storage-v1
  @doc "Generic key-value storage interface"

  ;; Core storage operations
  (defun store:string (key:string value:object)
    @doc "Store value under key, returns success message")

  (defun retrieve:object (key:string)
    @doc "Retrieve value by key")

  (defun exists:bool (key:string)
    @doc "Check if key exists in storage")

  (defun delete:string (key:string)
    @doc "Delete key-value pair")

  (defun list-keys:[string] ()
    @doc "List all stored keys")

  ;; Batch operations
  (defun store-batch:string (entries:[object])
    @doc "Store multiple key-value pairs")

  (defun retrieve-batch:[object] (key-list:[string])
    @doc "Retrieve multiple values by keys")

  ;; Storage schema
  (defschema storage-entry
    @doc "Schema for storage entries"
    key:string
    value:object
    created:time
    updated:time
    metadata:object)

  ;; Administrative capabilities
  (defcap STORAGE_ADMIN:bool ()
    @doc "Administrative capability for storage operations")

  (defcap STORE_DATA:bool (key:string)
    @doc "Capability to store data")

  (defcap RETRIEVE_DATA:bool (key:string)
    @doc "Capability to retrieve data")

  ;; Constants
  (defconst MAX_KEY_LENGTH:integer 256
    @doc "Maximum length for storage keys")

  (defconst MAX_VALUE_SIZE:integer 1048576
    @doc "Maximum size for stored values (1MB)")
)

;; Simple table-based implementation
(module table-storage GOVERNANCE
  @doc "Simple table-based storage implementation"

  (implements storage-v1)

  (defcap GOVERNANCE ()
    (enforce-keyset "storage-admin"))

  ;; Implementation schema
  (defschema entry
    key:string
    value:object
    created:time
    updated:time
    metadata:object)

  (deftable storage-data:{entry})

  ;; Capabilities
  (defcap STORAGE_ADMIN:bool ()
    (enforce-keyset "storage-admin"))

  (defcap STORE_DATA:bool (key:string)
    (enforce (!= key "") "key cannot be empty")
    (enforce (<= (length key) MAX_KEY_LENGTH) "key too long"))

  (defcap RETRIEVE_DATA:bool (key:string)
    (enforce (!= key "") "key cannot be empty"))

  ;; Constants
  (defconst MAX_KEY_LENGTH 256)
  (defconst MAX_VALUE_SIZE 1048576)

  ;; Core operations
  (defun store:string (key:string value:object)
    (with-capability (STORE_DATA key)
      (let ((now (at 'block-time (chain-data))))
        (write storage-data key {
          "key": key,
          "value": value,
          "created": now,
          "updated": now,
          "metadata": {}
        }))
      "Data stored successfully"))

  (defun retrieve:object (key:string)
    (with-capability (RETRIEVE_DATA key)
      (at 'value (read storage-data key))))

  (defun exists:bool (key:string)
    (with-default-read storage-data key
      { "key": "" }
      { "key" := stored-key }
      (!= stored-key "")))

  (defun delete:string (key:string)
    (with-capability (STORAGE_ADMIN)
      (with-read storage-data key { "key" := _ }
        (update storage-data key { "value": {} }))
      "Data deleted successfully"))

  (defun list-keys:[string] ()
    (map (at 'key) (select storage-data (constantly true))))

  (defun store-batch:string (entries:[object])
    (with-capability (STORAGE_ADMIN)
      (map (lambda (entry)
             (store (at 'key entry) (at 'value entry)))
           entries)
      "Batch store completed"))

  (defun retrieve-batch:[object] (key-list:[string])
    (map retrieve key-list))

  ;; Metadata operations
  (defun get-metadata:object (key:string)
    (at 'metadata (read storage-data key)))

  (defun set-metadata:string (key:string metadata:object)
    (with-capability (STORAGE_ADMIN)
      (update storage-data key { "metadata": metadata })
      "Metadata updated"))
)

;; Memory-based implementation (for testing)
(module memory-storage GOVERNANCE
  @doc "In-memory storage implementation for testing"

  (implements storage-v1)

  (defcap GOVERNANCE ()
    (enforce-keyset "test-admin"))

  ;; Use a single row to store all data as JSON
  (defschema memory-store
    data:object)

  (deftable memory-data:{memory-store})

  ;; Capabilities
  (defcap STORAGE_ADMIN:bool ()
    (enforce-keyset "test-admin"))

  (defcap STORE_DATA:bool (key:string)
    (enforce (!= key "") "key cannot be empty"))

  (defcap RETRIEVE_DATA:bool (key:string)
    (enforce (!= key "") "key cannot be empty"))

  ;; Constants
  (defconst MAX_KEY_LENGTH 256)
  (defconst MAX_VALUE_SIZE 1048576)

  ;; Helper functions
  (defun get-all-data:object ()
    (with-default-read memory-data "store"
      { "data": {} }
      { "data" := data }
      data))

  (defun set-all-data:string (data:object)
    (write memory-data "store" { "data": data }))

  ;; Core operations
  (defun store:string (key:string value:object)
    (with-capability (STORE_DATA key)
      ;; For simplicity, store each key-value pair as a separate record
      (write memory-data key { "value": value })
      "Data stored in memory"))

  (defun retrieve:object (key:string)
    (with-capability (RETRIEVE_DATA key)
      (at "value" (read memory-data key))))

  (defun exists:bool (key:string)
    (with-default-read memory-data key
      { "value": {} }
      { "value" := val }
      (!= val {})))

  (defun delete:string (key:string)
    (with-capability (STORAGE_ADMIN)
      (with-read memory-data key { "value" := val }
        (update memory-data key { "value": {} })
        "Data deleted from memory")))

  (defun list-keys:[string] ()
    (keys memory-data))

  (defun store-batch:string (entries:[object])
    (with-capability (STORAGE_ADMIN)
      (map (lambda (entry)
             (store (at 'key entry) (at 'value entry)))
           entries)
      "Batch stored in memory"))

  (defun retrieve-batch:[object] (key-list:[string])
    (map retrieve key-list))

  ;; Clear all data (testing only)
  (defun clear-all:string ()
    (with-capability (STORAGE_ADMIN)
      (set-all-data {})
      "All data cleared"))
)

;; Initialize tables
(create-table storage-data)
(create-table memory-data)