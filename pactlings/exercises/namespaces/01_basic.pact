;; Exercise: Basic Namespaces
;; Learn namespace creation and organization

;; TODO: Define keysets for different namespaces
(define-keyset 'YOUR_NAMESPACE_ADMIN_KEYSET
  (read-keyset "YOUR_NAMESPACE_ADMIN_KEYSET"))

(define-keyset 'YOUR_NAMESPACE_USER_KEYSET  
  (read-keyset "YOUR_NAMESPACE_USER_KEYSET"))

;; TODO: Create a namespace for your application
(define-namespace 'YOUR_NAMESPACE_NAME 
  YOUR_ADMIN_GUARD_HERE
  YOUR_USER_GUARD_HERE)

;; TODO: Set the current namespace
(namespace 'YOUR_NAMESPACE_NAME)

;; TODO: Create a keyset within the namespace
(define-keyset "YOUR_NAMESPACE_NAME.module-admin")

;; TODO: Create a module within the namespace
(module YOUR_MODULE_NAME YOUR_MODULE_GOVERNANCE
  @doc "Module within namespace"
  
  ;; TODO: Define a capability
  (defcap YOUR_CAPABILITY_NAME ()
    @doc "Namespace capability"
    YOUR_CODE_HERE)
  
  ;; TODO: Define a function
  (defun YOUR_FUNCTION_NAME ()
    @doc "Namespace function"
    YOUR_CODE_HERE)
  
  ;; TODO: Function to demonstrate namespace isolation
  (defun get-namespace-info ()
    @doc "Get information about current namespace"
    YOUR_CODE_HERE))

;; TODO: Create another namespace for comparison
(namespace "")  ;; Reset to root namespace

(define-namespace 'YOUR_SECOND_NAMESPACE
  YOUR_ADMIN_GUARD_HERE
  YOUR_USER_GUARD_HERE)

(namespace 'YOUR_SECOND_NAMESPACE)

;; TODO: Create module in second namespace
(module YOUR_SECOND_MODULE_NAME YOUR_GOVERNANCE
  @doc "Module in second namespace"
  
  (defun cross-namespace-call ()
    @doc "Call function from another namespace"
    ;; TODO: Call function from first namespace using qualified name
    YOUR_CODE_HERE))