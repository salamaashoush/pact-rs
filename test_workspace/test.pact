(namespace 'test)

(module test-module GOVERNANCE
"Test module"
(defcap GOVERNANCE () true)
(defun test-function (x:integer) (+ x 1))
(defconst TEST_CONST 42))