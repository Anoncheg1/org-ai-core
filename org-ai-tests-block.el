; -*- lexical-binding: t -*-
(require 'ert)             ; Testing framework
(require 'org-ai-block)

;; (eval-buffer) or (load-file "path/to/async-tests.el")
;; Running Tests: Load the test file and run:
;; (eval-buffer)
;; (ert t)
;;
;; (setq ert-debug-on-error t)


;;; Helper function to set up a temporary Org buffer for testing.
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
      ;; Check if #+begin_ai exists to avoid search failure
      (unless (string-match-p "#\\+begin_ai" block-content)
        (error "Test setup failed: block-content does not contain '#+begin_ai'"))
      ;; Move point to the start of the AI block
      (unless (search-forward "#+begin_ai" nil t)
        (error "Failed to find '#+begin_ai' in buffer"))
      (beginning-of-line) ; Ensure point is at the start of the block
      (let* ((element (org-element-at-point)))
        (unless (eq (org-element-type element) 'special-block)
          (error "No valid Org-AI block found at point"))
        element)) ; return
        ))


(org-ai-test-setup-buffer "#+begin_ai\nTest content\n#+end_ai")

;;; --- Test Cases ---

;; - test for test

(ert-deftest test-setup-buffer-basic ()
  "Test that org-ai-test-setup-buffer sets up a buffer correctly."
  (let* ((block-content "#+begin_ai\nTest content\n#+end_ai")
         (element (org-ai-test-setup-buffer block-content)))
    (should (eq (org-element-type element) 'special-block))
    (should (equal (org-element-property :type element) "ai"))))

;; - org-ai-block--let-params

(ert-deftest org-ai-block--let-params-all-from-info-test ()
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
          (setq info (org-ai-block-get-info))
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
    (kill-buffer buffer))))

;; - org-ai-interface-step1

;; (defun org-ai-block--org-ai-api-request-prepare (req-type content element sys-prompt sys-prompt-for-all-messages model max-tokens top-p temperature frequency-penalty presence-penalty service stream)
;;   )

(ert-deftest org-ai-block--org-ai-agent-call-test ()

  (let* ((test-block "#+begin_ai :stream t :sys \"A helpful LLM.\" :max-tokens 50 :model \"gpt-3.5-turbo\" :temperature 0.7\n\n#+end_ai\n")
         (org-ai-agent-call #'org-ai-block--org-ai-api-request-prepare)
         ;; - setup test buffer
         (element (org-ai-test-setup-buffer test-block))
         (info)
         (marker (copy-marker (org-element-property :contents-end element)))
         (buffer (org-element-property :buffer element))
         evaluated-result)
    ;; (unwind-protect
        (with-current-buffer buffer
          ;; - set cursor
          (goto-char (org-element-property :begin element))
          ;; (print (list "element" (org-element-property :contents-begin element)))

          (let ((org-ai-agent-call (lambda (req-type element sys-prompt sys-prompt-for-all-messages model max-tokens top-p temperature frequency-penalty presence-penalty service stream)
                                     (print (list 'req-type (type-of req-type) req-type))
                                     (print (list 'element (type-of element) element))
                                     (print (list 'sys-prompt (type-of sys-prompt) sys-prompt))
                                     (print (list 'sys-prompt-for-all-messages (type-of sys-prompt-for-all-messages) sys-prompt-for-all-messages))
                                     (print (list 'model (type-of model) model))
                                     (print (list 'max-tokens (type-of max-tokens) max-tokens))
                                     (print (list 'top-p (type-of top-p) top-p))
                                     (print (list 'temperature (type-of temperature) temperature))
                                     (print (list 'frequency-penalty (type-of frequency-penalty) frequency-penalty))
                                     (print (list 'presence-penalty (type-of presence-penalty) presence-penalty))
                                     (print (list 'service (type-of service) service))
                                     (print (list 'stream (type-of stream) stream))
                                     (should (and (eql req-type 'chat) (eql (type-of req-type) 'symbol) ))
                                     (should (org-element-type element 'special-block))
                                     (should (eql (type-of element) 'cons))
                                     (should (eql (type-of sys-prompt) 'string))
                                     (should (string= sys-prompt "A helpful LLM."))
                                     (should (and (eql (type-of sys-prompt-for-all-messages) 'symbol) (null sys-prompt-for-all-messages)))
                                     (should (and (eql (type-of model) 'string) (string= model "gpt-3.5-turbo")))
                                     (should (and (eql (type-of max-tokens) 'integer) (= max-tokens 50)))
                                     (should (and (eql (type-of top-p) 'symbol) (null top-p)))
                                     (should (and (eql (type-of temperature) 'float) (= temperature 0.7)))
                                     (should (and (eql (type-of frequency-penalty) 'symbol) (null frequency-penalty)))
                                     (should (and (eql (type-of presence-penalty) 'symbol) (null presence-penalty)))
                                     (should (and (eql (type-of service) 'symbol) (string= service "openai")))
                                     (should (and (eql (type-of stream) 'symbol) (eql stream t)))
                                                  ;; (string-equal stream "t"))
                                     ;; (print (list req-type content element sys-prompt sys-prompt-for-all-messages model max-tokens top-p temperature frequency-penalty presence-penalty service stream))
                                     )))
            (org-ai-ctrl-c-ctrl-c)
            )
          ;; (org-ai-interface-step1)

      (kill-buffer buffer)
      )
      ;; )
    )
  (should t)
  )


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
