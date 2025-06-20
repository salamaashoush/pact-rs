;; Database operations test
(create-table accounts {"balance": "decimal", "owner": "string"} "Account balances")

(write accounts "alice" {"balance": 100.0, "owner": "Alice"})
(write accounts "bob" {"balance": 50.0, "owner": "Bob"})

(read accounts "alice")
(read accounts "bob")

(keys accounts)