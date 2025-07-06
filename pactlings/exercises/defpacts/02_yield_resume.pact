;; Exercise: Yield and Resume in Defpacts
;; Learn data transfer between pact steps

(define-keyset 'cross-chain-keyset (read-keyset "cross-chain-keyset"))

(module yield-resume-exercise 'cross-chain-keyset
  @doc "Cross-chain data transfer with yield/resume"
  
  (defschema transfer-data
    sender:string
    amount:decimal
    token:string)
  
  (defschema cross-chain-state
    chain-id:string
    status:string
    data:object{transfer-data})
  
  (deftable transfers:{cross-chain-state})
  
  ;; TODO: Create a defpact that yields data to another chain
  (defpact cross-chain-transfer (transfer-id:string sender:string receiver:string amount:decimal)
    @doc "Transfer tokens across chains"
    
    ;; Step 1: Prepare transfer on source chain
    (step
      (let ((transfer-data:object{transfer-data} 
             { "sender": sender
             , "amount": amount  
             , "token": "KDA" }))
        
        ;; TODO: Store transfer data
        YOUR_CODE_HERE
        
        ;; TODO: Yield data to target chain
        YOUR_CODE_HERE
        
        ;; Return confirmation
        (format "Transfer {} initiated for {} KDA from {}" [transfer-id amount sender])))
    
    ;; Step 2: Complete transfer on target chain  
    (step
      ;; TODO: Resume the yielded data
      (resume { "sender" := s, "amount" := a, "token" := t }
        
        ;; TODO: Update transfer status
        YOUR_CODE_HERE
        
        ;; TODO: Complete the transfer
        YOUR_CODE_HERE)))
  
  ;; TODO: Function to initiate cross-chain transfer
  (defun start-cross-chain-transfer (transfer-id:string sender:string receiver:string amount:decimal target-chain:string)
    @doc "Start cross-chain transfer"
    YOUR_CODE_HERE)
  
  ;; TODO: Function to continue cross-chain transfer
  (defun continue-cross-chain-transfer (transfer-id:string step:integer)
    @doc "Continue cross-chain transfer to next step"
    YOUR_CODE_HERE)
  
  ;; TODO: Function to get transfer status
  (defun get-transfer-status (transfer-id:string)
    @doc "Get cross-chain transfer status"
    YOUR_CODE_HERE))

;; Create the table
(create-table transfers)