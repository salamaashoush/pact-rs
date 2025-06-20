;; Complex operations for advanced testing
(let ((users {"alice": {"age": 30, "city": "NYC"}, 
              "bob": {"age": 25, "city": "LA"}}))
  (let ((alice-age (at "age" (at "alice" users))))
    (+ alice-age 5)))

;; Nested calculations
(let ((calculate (lambda (x y) (+ (* x x) (* y y)))))
  (calculate 3 4))

;; List operations
(let ((numbers [1, 2, 3, 4, 5]))
  (fold (+) 0 numbers))

;; Object manipulation
(let ((obj {"a": 1, "b": 2, "c": 3}))
  (keys obj))