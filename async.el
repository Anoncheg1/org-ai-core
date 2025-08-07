; -*- lexical-binding: t -*-

;; Usage:
;; You define and run pipeline with `start-async-chain'.
;; You may call own function defined with (data callback) parameters.
;; You   may   redefine    `async-default-aggregator'   for   parallel
;; calls. There may be only one aggregator for now.
;; :parallel should be at the beginin of list
;; :aggregator may be anywhere in parallel list
;;
;; TODO: make :aggregator to be able to set many of them. (or it is not necessary?)
;; TODO: add :catch for error handling. (or it is not necessory?)
;;
;;; How this works:
;; Each async records-functions wrapped in lambda that call to next record with result.
;; All lambda functions created as a one lambda and we call it.

;;;###autoload
(defun async-default-template (data callback delay result-suffix)
  "Default async function template that appends RESULT-SUFFIX to DATA after DELAY seconds and calls CALLBACK."
  (run-at-time delay nil callback
               (concat (or (if data (concat data " -> "))
                           "") result-suffix)))

;;;###autoload
(defun async-default-aggregator (results)
  "Default aggregator for parallel results, concatenating them with commas."
  ;; (print "aggregator" results)
  (let ((r (mapconcat 'identity results ", ")))
    (if (> (length results) 1)
        (concat "{" r "}")
      r)))

(defun async-create-function (spec)
  "Create an async function from SPEC.
SPEC is either a function that accepts (data, callback), a plist with :result and :delay,
or a list representing a sequential sub-chain."
  (cond
   ((functionp spec) spec)
   ((and (listp spec) (not (eq (car spec) :parallel)) (listp (car spec)))
    ;; Treat as a sequential sub-chain
    (lambda (data callback)
      (start-async-chain data spec callback)))
   (t
    ;; Handle plist
    (let ((result (or (plist-get spec :result) "Result"))
          (delay (or (plist-get spec :delay) 1)))
      (mapc (lambda (x)
              (if (and (symbolp x) (not (member x '(:result :delay))))
                  (error "Unknown key %s in async function spec" x)))
            spec)
      (lambda (data callback)
        (async-default-template data callback delay result))))))

(defun async-plist-remove (plist key)
  "Remove KEY and its value from PLIST, returning a new plist.
Used for :aggregator."
  (if (memq key plist)
      (let ((new-plist (copy-sequence plist)))
        (delq (cadr (memq key new-plist)) new-plist)
        (delq key new-plist))
    plist))

(defun async-plist-get (plist key)
  "Get value by KEY from PLIST.
Used for :aggregator."
  (if (memq key plist)
      (let ((value (cadr (memq key plist))))
        (if (eql (car value) 'function)
            (cadr value)  ;; Extract symbol from function
          ;; else
          value))
    ;; else
    nil))


(defun async--handle-parallel-step (specs data chain-step current-index)
  "Execute parallel SPECS with DATA, aggregate results with AGGREGATOR, and call CHAIN-STEP with CURRENT-INDEX."
  (let* ((aggregator (async-plist-get specs :aggregator))
         (specs (async-plist-remove specs :aggregator))
         (results '())
         (pending-calls (length specs)))
    (if (zerop pending-calls)
        (funcall chain-step data (1+ current-index))
      (dolist (spec specs)
        (let ((func (async-create-function spec)))
          (funcall func data
                   (lambda (result)
                     (push result results)
                     (when (zerop (setq pending-calls (1- pending-calls)))
                       (let ((aggregated-result (funcall (or aggregator #'async-default-aggregator) results)))
                         (funcall chain-step aggregated-result (1+ current-index)))))))))))

(defun async--handle-sequential-step (step data chain-step current-index)
  "Execute sequential STEP with DATA and call CHAIN-STEP with CURRENT-INDEX."
  (let ((func (async-create-function step)))
    (funcall func data
             (lambda (result)
               (funcall chain-step result (1+ current-index))))))

;;;###autoload
(defun start-async-chain (initial-data sequence &optional final-callback)
  "Execute a sequence of async functions from SEQUENCE, starting with INITIAL-DATA.
Each spec is either:
1) a function (taking data and callback),
2) a plist with :result and :delay keys,
3) (:parallel spec1 spec2 ...) for parallel execution,
4) a list of specs for a sequential sub-chain.
For parallel steps, execute functions concurrently and combine results using AGGREGATOR or async-default-aggregator.
Each function in SEQUENCE takes DATA and a CALLBACK, passing results to the next function.
(chain-step(data 0) -> (funcall func data callback) -> lambda (result) -> (chain-step(data 1))
Returns result of the first function in the chain."
  (letrec ((chain-step
            (lambda (data current-index)
              (if (< current-index (length sequence))
                  (let ((step (nth current-index sequence)))
                    ;; (print (list step current-index))
                    (if (and (listp step) (eq (car step) :parallel))
                        (async--handle-parallel-step (cdr step) data chain-step current-index)
                      (async--handle-sequential-step step data chain-step current-index)))
                ;; finally
                (if final-callback
                    (funcall final-callback data)
                  ;; else
                  (print (format "Final result: %s" data))
                )))))
    (funcall chain-step initial-data 0))
  )

;;; Examples of usage:
;; 1. Sequential and parallel steps with default template
;; (start-async-chain nil
;;  '((:result "Step 1" :delay 1)
;;    (:parallel
;;     (:result "Parallel A" :delay 2)
;;     (
;;      (:result "Sub-seq a" :delay 1)
;;      (:result "Sub-seq b" :delay 1)
;;      )
;;     (:result "Parallel B" :delay 2))
;;    (:result "Step 3" :delay -1)))

;; 2. Mixing custom function and parallel steps
;; (defun custom-async-step (data callback)
;;   "Custom async function that modifies data differently.
;;   CALLBACK is optionall and may be ignored, see `async-create-function'
;;   for refence."
;;   (run-at-time 1.5 nil callback
;;                (concat data " -> Custom Step")))

;; (start-async-chain nil
;;  '((:result "Step 1" :delay 1)
;;    (:parallel
;;     custom-async-step
;;     (:result "Parallel B" :delay 1))
;;    (:result "Step 3" :delay 1)))

;; 3. With custom aggregator
;; (defun custom-aggregator (results)
;;   "Custom aggregator that joins results with ' & '."
;;   (mapconcat 'identity results " & "))

;; (start-async-chain nil
;;  '((:result "Step 1" :delay 1)
;;    (:parallel
;;     (:result "Parallel A" :delay 1)
;;     (:result "Parallel B" :delay 2)
;;     :aggregator #'custom-aggregator)))

;; Output: "Final result: Step 1 -> Parallel B & Step 1 -> Parallel A"

;; 4. Use external data in callback and callback with one argument
;; (let* ((var "myvar")
;;        (stepcallback)
;;        (callback1 (lambda (data)
;;                     (funcall stepcallback (concat data " -> " var))))
;;        (call (lambda (data callback)
;;                (setq stepcallback callback)
;;                (run-at-time 0 nil callback1
;;                                                   (concat data " -> " "Step1"))))
;;        )
;;   (start-async-chain nil
;;                      (list call
;;                            call
;;                            call
;;                            )))
;; Output:  "Final result:  -> Step1 -> myvar -> Step1 -> myvar -> Step1 -> myvar"


;; 5. Use mutable lambdas
;; (let* ((call (lambda (step)
;;                (lambda (data callback)
;;                  (run-at-time 0 nil callback
;;                               (concat data " -> " "Step" (number-to-string step)))))
;;              ))
;;   (start-async-chain nil
;;                      (list (funcall call 0)
;;                            (funcall call 1)
;;                            (funcall call 2)
;;                            (funcall call 3))))
;; Output:  "Final result:  -> Step0 -> Step1 -> Step2 -> Step3"
;;; provide
(provide 'async)
