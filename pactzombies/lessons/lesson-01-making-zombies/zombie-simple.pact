(module zombies GOV

  (defcap GOV () true)

  (defschema zombie
    name:string
    dna:integer
    level:integer
    owner:string)

  (deftable zombies-table:{zombie})

  (defun create-zombie (name:string)
    (let* ((owner (at "sender" (chain-data)))
           (dna (abs (str-to-int 64 (take 16 (hash name)))))
           (zombie-id (format "zombie_{}" [dna])))
      (insert zombies-table zombie-id
        { "name": name
        , "dna": dna
        , "level": 1
        , "owner": owner
        })
      (format "Zombie {} created with DNA: {}" [name dna])))
)