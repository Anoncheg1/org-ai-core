(require 'ert)             ; For testing framework
(require 'org-ai-block)

;; Helper function to set up a temporary Org buffer for testing.
;; It inserts content and optional Org properties, then returns the
;; parsed Org-AI block element and its parameters alist.
(defun org-ai-test-setup-buffer (block-content &optional properties-alist)
  "Create a temporary Org buffer with BLOCK-CONTENT and optional PROPERTIES-ALIST.
PROPERTIES-ALIST should be an alist like '((property-name . \"value\")).
Returns a list (ELEMENT INFO-ALIST), where ELEMENT is the parsed Org-AI block
and INFO-ALIST is the parameters from its header."
  (let ((buf (generate-new-buffer "*org-ai-test-temp*")))
    (with-current-buffer buf
      (org-mode)
      (setq-local org-export-with-properties t) ; Ensure properties are considered
      (when properties-alist
        (dolist (prop properties-alist)
          (insert (format "#+PROPERTY: %s %s\n" (car prop) (cdr prop)))))
      (insert block-content)
      (goto-char (point-min))
      ;; Move point to the start of the AI block to ensure `org-element-at-point` works
      ;; and `org-entry-get-with-inheritance` can find properties.
      (search-forward "#+begin_ai")
      (let* ((element (org-element-at-point))
             ;; org-element-property :parameters returns a plist, which alist-get works on.
             (info-alist (org-element-property :parameters element)))
        element))))


;; --- Test Cases ---

(ert-deftest org-ai-block--let-params-all-from-info ()
  "Test when all parameters are provided in the block header (info alist)."
  (let* ((test-block "#+begin_ai :stream t :sys \"A helpful LLM.\" :max-tokens 50 :model \"gpt-3.5-turbo\" :temperature 0.7\n#+end_ai\n")
       (element (org-ai-test-setup-buffer test-block))
       (info)
       (marker (copy-marker (org-element-property :contents-end element)))
       (buffer (org-element-property :buffer element))
       evaluated-result)
  (unwind-protect
      (with-current-buffer buffer

          ;; Position point inside the block for correct context, though not strictly needed for info directly.
          (goto-char (org-element-property :begin element))
          (setq info (org-ai-get-block-info))
          (org-ai-block--let-params info ((stream) (sys) (max-tokens :type integer) (model) (temperature :type float) (unknown "s"))
                                         ;; (print (list max-tokens (type-of max-tokens)))
                                         ;; (print (list temperature (type-of temperature)))
                                         ;; (print (list unknown (type-of unknown)))
                                         (should (string-equal stream "t"))
                                         (should (= max-tokens 50))
                                         (should (string-equal sys "A helpful LLM."))
                                         (should (string-equal model "gpt-3.5-turbo"))
                                         (should (= temperature 0.7))
                                         (should (string-equal unknown "s"))
                                         ))
    (kill-buffer buffer)
    )))

;; (ert-deftest org-ai-block--let-params-inherited-properties ()
;;   "Test when parameters are sourced from inherited Org properties."
;;   (let* ((test-block "#+begin_ai\n#+end_ai\n") ; No parameters in block header
;;          (setup-result (org-ai-test-setup-buffer test-block
;;                                                  '((model . "text-davinci-003")
;;                                                    (max-tokens . "100")
;;                                                    (temperature . "0.5")
;;                                                    (sys . "Inherited system prompt"))))
;;          (element (car setup-result))
;;          (info (cadr setup-result)) ; Empty info from block header
;;          evaluated-result)
;;     (unwind-protect
;;         (with-current-buffer (marker-buffer (org-element-property :begin element))
;;           ;; Position point inside the block for `org-entry-get-with-inheritance`
;;           (goto-char (org-element-property :begin element))
;;           (setq evaluated-result
;;                 (org-ai-test-eval-macro
;;                  '(org-ai-block--let-params info
;;                                             ((stream nil) ; default `nil`
;;                                              (sys nil)    ; no default, inherited from property
;;                                              (max-tokens nil :type number)
;;                                              (model nil)
;;                                              (temperature nil :type number))
;;                                             (list stream sys max-tokens model temperature))
;;                  element info)))
;;       (kill-buffer (marker-buffer (org-element-property :begin element))))
;;     (should (equal (car evaluated-result) nil)) ; No stream property or default
;;     (should (equal (cadr evaluated-result) "Inherited system prompt")) ; From inherited property
;;     (should (equal (caddr evaluated-result) 100)) ; From inherited property, converted to number
;;     (should (equal (nth 3 evaluated-result) "text-davinci-003")) ; From inherited property, no conversion (model is special)
;;     (should (equal (nth 4 evaluated-result) 0.5))) ; From inherited property, converted to number
;;   )

;; (ert-deftest org-ai-block--let-params-default-form ()
;;   "Test when parameters fall back to default forms."
;;   (let* ((test-block "#+begin_ai\n#+end_ai\n") ; No block params, no inherited props
;;          (setup-result (org-ai-test-setup-buffer test-block))
;;          (element (car setup-result))
;;          (info (cadr setup-result)) ; Empty info
;;          evaluated-result)

;;     (unwind-protect
;;         (with-current-buffer (marker-buffer (org-element-property :begin element))
;;           (goto-char (org-element-property :begin element))
;;           (setq evaluated-result
;;                 (org-ai-test-eval-macro
;;                  '(org-ai-block--let-params info
;;                     ((stream t) ; Default true
;;                      (sys "Default system prompt")
;;                      (max-tokens 200 :type number)
;;                      (model "default-model-name")
;; (temperature 0.8 :type number)
;;                      (non-existent-param "fallback-value")) ; Test a parameter not in definitions
;;                     (list stream sys max-tokens model temperature non-existent-param))
;;                  element info)))
;;       (kill-buffer (marker-buffer (org-element-property :begin element))))
;;     (should (equal (car evaluated-result) t))
;;     (should (equal (cadr evaluated-result) "Default system prompt"))
;;     (should (equal (caddr evaluated-result) 200))
;;     (should (equal (nth 3 evaluated-result) "default-model-name"))
;;     (should (equal (nth 4 evaluated-result) 0.8))
;;     ;; Note: `non-existent-param` is not in definitions, so it won't be bound by `let-params`.
;;     ;; This `list` will cause an error because `non-existent-param` is not defined.
;;     ;; The macro itself only binds variables listed in `definitions`.
;;     ;; Removing `non-existent-param` from the test list.
;;     ))


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
