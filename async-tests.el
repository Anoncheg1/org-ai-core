; -*- lexical-binding: t -*-
(require 'async)
(require 'ert)

;; (eval-buffer) or (load-file "path/to/async-tests.el")
;; Running Tests: Load the test file and run:
;; (eval-buffer)
;; (ert t)
;; to execute all tests. Individual tests can be run with (ert 'test-name).

;;; - Help functions ------------------------------------------------

(defvar test-results nil "Store async callback results for testing.")
(defvar test-print-output nil "Store print outputs for testing.")

(defun test-capture-print (orig-print &rest args)
  "In `test-print-output' variable store print output."
  (push args test-print-output)
  (apply orig-print args))

(defun wait-for-async (timeout)
  "Wait up to TIMEOUT seconds for test-results to be non-nil, return result."
  (let ((start-time (float-time)))
    ;; loop - Once `test-results` is non-nil or the `timeout` is reached, the loop exits.
    (while (and (null test-results)
                (< (- (float-time) start-time) timeout))
      (sit-for 0.1))
    (prog1 test-results
      ;; After returning the result, we set `test-results` to nil, preparing it for the next asynchronous test.
      (setq test-results nil))))

(defun reset-test-state ()
  "Reset test-results and test-print-output."
  (setq test-results nil
        test-print-output nil))

;;; - Tests --------------------------------------------------------

(ert-deftest test-async-plist-tests()

  (should
   (eql (async-plist-get '(:parallel
                                 (:result "Parallel A" :delay 1)
                                 (:result "Parallel B" :delay 2)
                                 :aggregator #'async-default-aggregator
                                 )
                               :aggregator)
              #'async-default-aggregator))
  (should
   (eql (async-plist-get '(:parallel
                           :aggregator #'async-default-aggregator
                                 (:result "Parallel A" :delay 1)
                                 (:result "Parallel B" :delay 2)

                                 )
                               :aggregator)
              #'async-default-aggregator))
  (should
   (eql (async-plist-get '(:parallel
                                 (:result "Parallel A" :delay 1)
                                 (:result "Parallel B" :delay 2)
                                 )
                               :aggregator)
              nil))
  (should
   (equal (async-plist-remove '(:parallel
                           :aggregator #'async-default-aggregator
                                 (:result "Parallel A" :delay 1)
                                 (:result "Parallel B" :delay 2)
                                 )
                               :aggregator)
              '(:parallel (:result "Parallel A" :delay 1) (:result "Parallel B" :delay 2))))
  (should
   (equal (async-plist-remove '(:parallel
                                 (:result "Parallel A" :delay 1)
                                 (:result "Parallel B" :delay 2)
                                 )
                               :aggregator)
              '(:parallel (:result "Parallel A" :delay 1) (:result "Parallel B" :delay 2))))
  )


(ert-deftest test-async-default-template-basic ()
  "Test async-default-template with basic input."
  (reset-test-state)
  (advice-add 'print :around #'test-capture-print)
  (async-default-template "test"
                         (lambda (result) (setq test-results result))
                         0.5
                         "suffix")
  (should (string= (wait-for-async 1) "test -> suffix"))
  (advice-remove 'print #'test-capture-print))

(ert-deftest test-async-default-template-nil-and-suffix ()
  "Test async-default-template with nil data."
  (reset-test-state)
  (advice-add 'print :around #'test-capture-print)
  (async-default-template nil
                         (lambda (result) (setq test-results result))
                         0.5
                         "suffix")
  (should (string= (wait-for-async 1) "suffix"))
  ;; (should (equal (car test-print-output) '(("DATA" nil))))
  (advice-remove 'print #'test-capture-print))

(ert-deftest test-async-default-template-zero-delay ()
  "Test async-default-template with zero delay."
  (reset-test-state)
  (async-default-template "test"
                         (lambda (result) (setq test-results result))
                         0
                         "suffix")
  (should (string= (wait-for-async 0.5) "test -> suffix")))

(ert-deftest test-async-default-aggregator-multiple ()
  "Test async-default-aggregator with multiple results."
  (reset-test-state)
  (advice-add 'print :around #'test-capture-print)
  (let ((result (async-default-aggregator '("a" "b" "c"))))
    (should (string= result "{a, b, c}")))
    ;; (should (equal (car test-print-output) '(("async-default-aggregator" ("a" "b" "c"))))))
  (advice-remove 'print #'test-capture-print))

(ert-deftest test-async-default-aggregator-single ()
  "Test async-default-aggregator with single result."
  (reset-test-state)
  (let ((result (async-default-aggregator '("a"))))
    (should (string= result "a"))))

(ert-deftest test-async-default-aggregator-empty ()
  "Test async-default-aggregator with empty results."
  (reset-test-state)
  (let ((result (async-default-aggregator '())))
    (should (string= result ""))))

(ert-deftest test-async-create-function-plist ()
  "Test async-create-function with plist spec."
  (reset-test-state)
  (let ((func (async-create-function '(:result "test-result" :delay 0.5))))
    (funcall func "input" (lambda (result) (setq test-results result)))
    (should (string= (wait-for-async 1) "input -> test-result"))))

(ert-deftest test-async-create-function-plist-defaults ()
  "Test async-create-function with empty plist."
  (reset-test-state)
  (let ((func (async-create-function '())))
    (funcall func "input" (lambda (result) (setq test-results result))))
    (should (string= (wait-for-async 1) "input")))

;; (ert-deftest test-async-create-function-function ()
;;   "Test async-create-function with function spec."
;;   (reset-test-state)
;;   (let ((func (async-create-function 'custom-async-step)))
;;     (funcall func "input" (lambda (result) (setq test-results result)))
;;     (should (string= (wait-for-async 2) "input -> Custom Step"))))



(ert-deftest test-async-handle-parallel-step-empty ()
  "Test async--handle-parallel-step with empty specs."
  (reset-test-state)
  (async--handle-parallel-step
   '()
   "input"
   ;; 'async-default-aggregator
   (lambda (result idx) (setq test-results (cons result idx)))
   0)
  (let ((result (wait-for-async 0.5)))
    (should (string= (car result) "input"))
    (should (= (cdr result) 1))))

;; (defun test-async-custom-aggregator (results)
;;   "Custom aggregator that joins results with ' & '."
;;   (mapconcat 'identity results " & "))

;; (ert-deftest test-async-handle-parallel-step-multiple ()
;;   "Test async--handle-parallel-step with multiple specs."
;;   (reset-test-state)
;;   ;; (advice-add 'print :around #'test-capture-print)
;;   (async--handle-parallel-step
;;    '((:result "A" :delay 0.5) (:result "B" :delay 0.5))
;;    "input"
;;    'async-default-aggregator
;;    (lambda (result idx) (setq test-results (cons result idx)))
;;    0)
;;   (let ((result (wait-for-async 1)))
;;     (should (or (string= (car result) "A, B") (string= (car result) "B, A")))
;;     (should (= (cdr result) 1)))
;;     ;; (should (equal (car test-print-output) '(("async--handle-parallel-step" "input")))))
;;   ;; (advice-remove 'print #'test-capture-print)
;;   )

(ert-deftest test-async-handle-parallel-step-custom-aggregator ()
  "Test async--handle-parallel-step with custom aggregator."
  (reset-test-state)
  ;; (async--handle-parallel-step
  ;;  '((:result "A" :delay 0.5) (:result "B" :delay 0.5))
  ;;  "input"
  ;;  'custom-aggregator
  ;;  (lambda (result idx) (setq test-results (cons result idx)))
  ;;  0)
  (async--handle-parallel-step
   '((:result "A" :delay 0.8) (:result "B" :delay 0.9))
   "input"
   ;; #'async-default-aggregator
   (lambda (result idx) (setq test-results (cons result idx)))
   0)
  (let ((result (wait-for-async 1))) ; ("{input -> B, input -> A}" . 1)
    (should (or (string= (car result) "{input -> B, input -> A}")
                (string= (car result) "{input -> A, input -> B}")))
    (should (= (cdr result) 1))))

(ert-deftest test-async-handle-sequential-step-plist ()
  "Test async--handle-sequential-step with plist step."
  (reset-test-state)
  (async--handle-sequential-step
   '(:result "Step1" :delay 0.5)
   "input"
   (lambda (result idx) (setq test-results (cons result idx)))
   0)
  (let ((result (wait-for-async 1)))
    (should (string= (car result) "input -> Step1"))
    (should (= (cdr result) 1))))

;; (ert-deftest test-async-handle-sequential-step-function ()
;;   "Test async--handle-sequential-step with function step."
;;   (reset-test-state)
;;   (async--handle-sequential-step
;;    'custom-async-step
;;    "input"
;;    (lambda (result idx) (setq test-results (cons result idx)))
;;    0)
;;   (let ((result (wait-for-async 2)))
;;     (should (string= (car result) "input -> Custom Step"))
;;     (should (= (cdr result) 1))))

(ert-deftest test-start-async-chain-sequential ()
  "Test start-async-chain with sequential steps."
  (reset-test-state)
  (advice-add 'print :around #'test-capture-print)
  (start-async-chain nil
                     '((:result "Step1" :delay 0.4)
                       (:result "Step2" :delay 0.4)))
  (wait-for-async 1)
  (should (string= (car (car test-print-output)) "Final result: Step1 -> Step2"))
  (advice-remove 'print #'test-capture-print))

(ert-deftest test-start-async-chain-parallel ()
  "Test start-async-chain with parallel steps."
  (reset-test-state)
  (advice-add 'print :around #'test-capture-print)
  (start-async-chain nil
                     '((:parallel (:result "A" :delay 0.5) (:result "B" :delay 0.5))))
  (wait-for-async 1)
  (let ((result (car (car test-print-output))))
    (should (or (string= result "Final result: {A, B}")
                (string= result "Final result: {B, A}"))))
  (advice-remove 'print #'test-capture-print))

(ert-deftest test-start-async-chain-mixed ()
  "Test start-async-chain with mixed sequential and parallel steps."
  (reset-test-state)
  (advice-add 'print :around #'test-capture-print)
  (start-async-chain nil
                     '((:result "Step1" :delay 0.5)
                       (:parallel (:result "A" :delay 0.5) (:result "B" :delay 0.5))
                       (:result "Step3" :delay 0.5)))
  (wait-for-async 2)
  (let ((result (car (car test-print-output))))
    (should (or (string= result "Final result: {Step1 -> B, Step1 -> A} -> Step3")
                (string= result "Final result: {Step1 -> A, Step1 -> B} -> Step3"))))
  (advice-remove 'print #'test-capture-print))

;; (ert-deftest test-start-async-chain-custom-function-aggregator ()
;;   "Test start-async-chain with custom function and aggregator."
;;   (reset-test-state)
;;   (start-async-chain nil
;;                      '((:result "Step1" :delay 0.5)
;;                        (:parallel custom-async-step (:result "B" :delay 0.5)))
;;                      'custom-aggregator)
;;   (let ((result (car (wait-for-async 2))))
;;     (should (or (string= result "Final result:  -> Step1 ->  -> Custom Step & B")
;;                 (string= result "Final result:  -> Step1 -> B &  -> Custom Step")))))

(ert-deftest test-start-async-chain-empty-sequence ()
  "Test start-async-chain with empty sequence."
  (reset-test-state)
  (advice-add 'print :around #'test-capture-print)
  (start-async-chain "test" '())
  (wait-for-async 0.5)
  (should (string= (car (car test-print-output)) "Final result: test"))
  (advice-remove 'print #'test-capture-print))

(ert-deftest test-start-async-chain-invalid-spec ()
  "Test start-async-chain with invalid plist spec."
  (reset-test-state)
  (let ((debug-on-error nil))
    (should-error
     (progn
       (start-async-chain nil '((:invalid-key "value") (:delay 0.5)))
       (wait-for-async 2)))))

(ert-deftest test-start-async-chain-zero-delay-parallel ()
  "Test start-async-chain with zero delay in parallel steps."
  (reset-test-state)
  (advice-add 'print :around #'test-capture-print)
  (start-async-chain nil
                     '((:parallel (:result "A" :delay 0) (:result "B" :delay 0))))
  (wait-for-async 0.5)
  (let ((result (car (car test-print-output))))
    (should (or (string= result "Final result: {A, B}")
                (string= result "Final result: {B, A}"))))
  (advice-remove 'print #'test-capture-print))

(ert-deftest test-start-async-chain-large-parallel ()
  "Test start-async-chain with large number of parallel steps."
  (reset-test-state)
  (advice-add 'print :around #'test-capture-print)
  (let ((specs (mapcar (lambda (i) `(:result ,(format "A%d" i) :delay 0.5))
                       (number-sequence 1 5)))) ; ((:result "A1" :delay 0.5) (:result "A2" :delay 0.5) (:result "A3" :delay 0.5) (:result "A4" :delay 0.5) (:result "A5" :delay 0.5))
    (start-async-chain nil `((:parallel ,@specs)))
    (wait-for-async 1)
    (let ((result (car (car test-print-output))))
      (should (string-match-p "Final result: {A[1-5], A[1-5], A[1-5], A[1-5], A[1-5]}" result))))
  (advice-remove 'print #'test-capture-print))

(ert-deftest test-start-async-chain-nil-aggregator ()
  "Test start-async-chain with nil aggregator."
  (reset-test-state)
  (advice-add 'print :around #'test-capture-print)
  (start-async-chain nil
                     '((:parallel (:result "A" :delay 0.5) (:result "B" :delay 0.5)))
                     nil)
  (wait-for-async 1)
  (let ((result (car (car test-print-output))))
    (should (or (string= result "Final result: {A, B}")
                (string= result "Final result: {B, A}"))))
  (advice-remove 'print #'test-capture-print))

(ert-deftest test-start-async-chain-error1 ()
  "Test start-async-chain with nil aggregator."
  (reset-test-state)
  (should-error
   (progn
     (start-async-chain nil
                        '((:result nil :delay 0.5)))
     (wait-for-async 1) )))
