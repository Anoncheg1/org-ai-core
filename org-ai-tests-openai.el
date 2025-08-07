(require 'ert)
(require 'org-ai-openai)

;; (eval-buffer)
;; (ert t)
;;

(defun org-ai-tests--progress-reporter-stop-one ()
  "Start one request
Stop it with `org-ai-openai-stop-url-request'.
"

  (let ((buf (generate-new-buffer "*org-ai-test-temp*")))
    (with-current-buffer buf
      (org-mode)
      (insert "#+begin_ai\n#+end_ai")
      (goto-char (point-min))
      (org-ai-block-p)
      ))
  )

;;; - For `org-ai--openai-get-token'

;; (require 'org-ai) ;; Assuming the function is defined in org-ai.el

(ert-deftest org-ai--openai-get-token-string-test ()
  "Test when org-ai-api-creds-token is a non-empty string."
  (let ((org-ai-api-creds-token "test-token-123"))
    (should (equal (org-ai--openai-get-token 'openai) "test-token-123"))))

(ert-deftest org-ai--openai-get-token-plist-valid-test ()
  "Test when org-ai-api-creds-token is a plist with valid service token."
  (let ((org-ai-api-creds-token '(:openai "test-token-openai" :anthropic "test-token-anthropic")))
    (should (equal (org-ai--openai-get-token 'openai) "test-token-openai"))))

(ert-deftest org-ai--openai-get-token-plist-invalid-test ()
  "Test when org-ai-api-creds-token is a plist without the service token."
  (let ((org-ai-api-creds-token '(:anthropic "test-token-anthropic")))
    (should-error (org-ai--openai-get-token 'openai)
                  :type 'error
                  :regexp "Token not found in defined plist `org-ai-api-creds-token'")))

(ert-deftest org-ai--openai-get-token-auth-source-test ()
  "Test when token is retrieved from auth-source."
  (let ((org-ai-api-creds-token "")
        (auth-sources '((:host "api.openai.com" :user "user" :secret "auth-token-123"))))
    (fset 'org-ai--openai-get-token-auth-source (lambda (service) "auth-token-123"))
    (should (equal (org-ai--openai-get-token 'openai) "auth-token-123"))
    (fmakunbound 'org-ai--openai-get-token-auth-source)))

(ert-deftest org-ai--openai-get-token-no-valid-token-test ()
  "Test when no valid token is provided."
  (let ((org-ai-api-creds-token "")
        (auth-sources nil))
    (fset 'org-ai--openai-get-token-auth-source (lambda (service) nil))
    (should-error (org-ai--openai-get-token 'openai)
                  :type 'error
                  :regexp "Please set `org-ai-api-creds-token' to your OpenAI API token or setup auth-source"))
    (fmakunbound 'org-ai--openai-get-token-auth-source))

;;; - org-ai--get-value-or-string
(ert-deftest org-ai--get-value-or-string-test ()
  ;; Example variables
  (defvar my-plist '(:foo "bar" :baz "qux"))
  (defvar my-string "hello")
  (defvar my-number 42)

  ;; Using org-ai--get-value-or-string
  (should (equal (org-ai--get-value-or-string my-plist "foo") "bar"))
  (should (equal (org-ai--get-value-or-string my-string "foo") "hello"))
  (should (equal (org-ai--get-value-or-string my-number "foo")  nil))
)
;;         (org-ai-block--set-variable

;; ;;     (with-current-buffer buf
;; ;;       (org-mode)
;;   (let ((buf (generate-new-buffer "*org-ai-test-temp*")))
;;     ))

;; (defun org-ai-tests--progress-reporter-start-two-and-stop-one ()
;;   "."
;;   (let ((buf (generate-new-buffer "*org-ai-test-temp*")))
;;     (with-current-buffer buf
;;       (org-mode)
;;       (setq-local org-export-with-properties t) ; Ensure properties are considered
;;       (when properties-alist
;;         (dolist (prop properties-alist)
;;           (insert (format "#+PROPERTY: %s %s\n" (car prop) (cdr prop)))))
;;       (insert block-content)
;;       (goto-char (point-min))
;;       ;; Move point to the start of the AI block to ensure `org-element-at-point` works
;;       ;; and `org-entry-get-with-inheritance` can find properties.
;;       (search-forward "#+begin_ai")
;;       (let* ((element (org-element-at-point))
;;              ;; org-element-property :parameters returns a plist, which alist-get works on.
;;              (info-alist (org-element-property :parameters element)))
;;         element))))
