; -*- lexical-binding: t -*-
(require 'ert)             ; Testing framework
(require 'org-ai-timers)

;; (eval-buffer) or (load-file "path/to/async-tests.el")
;; Running Tests: Load the test file and run:
;; (eval-buffer)
;; (ert t)
;;
;; (setq ert-debug-on-error t)
(ert-deftest test-org-ai-timers--get-keys-for-variable ()
  (let* ((marker (make-marker))
         (cb (current-buffer))
         (org-ai-timers--element-marker-variable-dict `((,cb . ,marker)
                                                        (,cb . ,(make-marker))
                                                        (,cb . ,marker)))
         (tres (org-ai-timers--get-keys-for-variable marker)))
    (should (eq (car tres) cb))
    (should (eq (length tres) 1))
    ;; (print org-ai-timers--element-marker-variable-dict)
    ))

(ert-deftest test-org-ai-timers--get-keys-for-variable2 ()
  (let* ((marker (make-marker))
         (cb (current-buffer))
         (org-ai-timers--element-marker-variable-dict `((,cb . ,(make-marker))
                                                        (,cb . ,(make-marker))))
         (tres (org-ai-timers--get-keys-for-variable marker)))
    (should (eq tres nil))
    ;; (print org-ai-timers--element-marker-variable-dict)
    ))


(ert-deftest test-org-ai-timers--org-ai-timers--set ()
  (let* ((marker (make-marker))
         (cb (current-buffer))
         (org-ai-timers--element-marker-variable-dict `((,cb . ,marker)
                                                        (,cb . ,(make-marker))
                                                        (,cb . ,marker)))
         (tres (org-ai-timers--get-keys-for-variable marker)))
    (should (eq (car tres) cb))
    (should (eq (length tres) 1))
    ;; (print org-ai-timers--element-marker-variable-dict)
    ))

;; To run these tests:
;; 1. Save the code to an .el file (e.g., `org-ai-params-test.el`).
;; 2. Open Emacs and load the file: `M-x load-file RET org-ai-tests2.el RET`.
;; 3. Run all tests: `M-x ert RET t RET`.
;;    Or run specific tests: `M-x ert RET org-ai-block--let-params-all-from-info RET`.
;; OR
;; to run: emacs -batch -l ert -l pinyin-isearch.el -l pinyin-isearch-pinyin-tests.el -f ert-run-tests-batch-and-exit 2> out.log
;;  OR
;; eval-buffer
;; M-x ert RET t RET
