(module zombies GOVERNANCE

  (defcap GOVERNANCE ()
    @doc "Module governance capability"
    (enforce-guard (read-keyset "zombies-admin")))

  (defschema zombie
    @doc "Schema for zombie data"
    name:string
    dna:integer
    type:string
    level:integer
    strength:integer
    speed:integer
    ready-time:time
    win-count:integer
    loss-count:integer
    owner:string)

  (deftable zombies-table:{zombie})

  (defconst ZOMBIE_TYPES ["walker" "runner" "tank" "hunter" "spitter"])
  (defconst COOLDOWN_PERIOD 60.0) ; 60 seconds between zombie creation

  (defun generate-dna:integer (name:string)
    @doc "Generate DNA from a name"
    (let ((hash-result (hash (+ name (format-time "%F %T" (at "block-time" (chain-data)))))))
      (abs (str-to-int 64 (take 16 hash-result)))))

  (defun get-zombie-type:string (dna:integer)
    @doc "Determine zombie type from DNA"
    (at (mod dna (length ZOMBIE_TYPES)) ZOMBIE_TYPES))

  (defun calculate-strength:integer (dna:integer type:string)
    @doc "Calculate strength based on DNA and type"
    (let ((base-strength (mod (/ dna 1000000000) 100)))
      (cond 
        ((= type "tank") (+ base-strength 20))
        ((= type "hunter") (+ base-strength 10))
        (+ base-strength 5)))

  (defun calculate-speed:integer (dna:integer type:string)
    @doc "Calculate speed based on DNA and type"
    (let ((base-speed (mod (/ dna 100000) 100)))
      (cond 
        ((= type "runner") (+ base-speed 25))
        ((= type "hunter") (+ base-speed 15))
        ((= type "tank") (- base-speed 10))
        base-speed))

  (defun check-cooldown:bool (owner:string)
    @doc "Check if owner can create a new zombie"
    (let ((owned-zombies (select zombies-table (where "owner" (= owner)))))
      (if (= 0 (length owned-zombies))
        true
        (let* ((latest-zombie (at 0 (sort ["ready-time"] owned-zombies)))
               (last-creation (at "ready-time" latest-zombie))
               (current-time (at "block-time" (chain-data)))
               (time-diff (diff-time current-time last-creation)))
          (>= time-diff COOLDOWN_PERIOD))))

  (defun create-zombie:string (name:string)
    @doc "Create a new zombie with type and stats"
    (let* ((owner (at "sender" (chain-data)))
           (dna (generate-dna name))
           (zombie-type (get-zombie-type dna))
           (strength (calculate-strength dna zombie-type))
           (speed (calculate-speed dna zombie-type))
           (zombie-id (format "zombie_{}" [dna])))
      
      (enforce (check-cooldown owner) 
        (format "Please wait {} seconds before creating another zombie" 
          [COOLDOWN_PERIOD]))
      
      (insert zombies-table zombie-id
        { "name": name
        , "dna": dna
        , "type": zombie-type
        , "level": 1
        , "strength": strength
        , "speed": speed
        , "ready-time": (at "block-time" (chain-data))
        , "win-count": 0
        , "loss-count": 0
        , "owner": owner
        })
      (format "Zombie {} created! Type: {}, DNA: {}, STR: {}, SPD: {}" 
        [name zombie-type dna strength speed]))

  (defun get-my-zombies:[object{zombie}] ()
    @doc "Get all zombies owned by the caller"
    (let ((owner (at "sender" (chain-data))))
      (select zombies-table (where "owner" (= owner))))

  (defun get-zombie-count:integer (owner:string)
    @doc "Get number of zombies owned by an address"
    (length (select zombies-table (where "owner" (= owner)))))
)