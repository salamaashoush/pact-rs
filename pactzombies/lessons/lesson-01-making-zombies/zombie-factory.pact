(module zombies GOVERNANCE

  (defcap GOVERNANCE ()
    @doc "Module governance capability"
    (enforce-guard (read-keyset "zombies-admin")))

  (defschema zombie
    @doc "Schema for zombie data"
    name:string
    dna:integer
    level:integer
    ready-time:time
    win-count:integer
    loss-count:integer
    owner:string)

  (deftable zombies-table:{zombie})

  (defun generate-dna:integer (name:string)
    @doc "Generate DNA from a name"
    (let ((hash-result (hash (+ name (format-time "%F %T" (at "block-time" (chain-data)))))))
      (abs (str-to-int 64 (take 16 hash-result)))))

  (defun create-zombie:string (name:string)
    @doc "Create a new zombie"
    (let* ((owner (at "sender" (chain-data)))
           (dna (generate-dna name))
           (zombie-id (format "zombie_{}" [dna])))
      (insert zombies-table zombie-id
        { "name": name
        , "dna": dna
        , "level": 1
        , "ready-time": (at "block-time" (chain-data))
        , "win-count": 0
        , "loss-count": 0
        , "owner": owner
        })
      (format "Zombie {} created with DNA: {}" [name dna])))
)