;; (require 'ert)             ; For testing framework
(require 'org-ai-openai)


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
