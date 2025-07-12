;; - for org-element-at-point to work and org-ai-where-is-src-block-result
;; - breaks 'fill-paragraph
(when (boundp 'org-element-greater-elements)
  (setq org-element-greater-elements (remove 'special-block org-element-greater-elements)))

;; (defun org-ai-block--org-fill-element-advice (func-call &rest args)
(defun org-ai-block-fill-paragraph (&optional justify region)
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (when current-prefix-arg 'full) t)))
  (org-ai-block--org-fill-element-advice justify)
  )
(setq-local fill-paragraph-function 'org-ai-block-fill-paragraph)

(defun org-ai-block--org-fill-element-advice (func-call &rest args)
  (let ((justify (car args)))
  (with-syntax-table org-mode-transpose-word-syntax-table
    (let ((element (save-excursion (end-of-line) (org-element-at-point))))
      ;; (if (org-element-type element)
      (if (string-equal "ai" (org-element-property :type element))
          ;; - fixed part of part of `org-fill-element' for paragraph
          ;; Paragraphs may contain `line-break' type objects.
	  (let ((beg (max (point-min) (org-element-contents-begin element)))
                (end (min (point-max) (org-element-contents-end element))))
            ;; Do nothing if point is at an affiliated keyword.
            (unless (< (line-end-position) beg)
              (save-excursion
                (goto-char beg)
                (let ((regions (list (make-marker))))
                  ;; Set marker for the start of the first region.
                  (set-marker (car regions) beg)
                  ;; Collect region boundaries by finding empty lines, using markers.
                  (while (< (point) end)
                    (if (looking-at-p "^[ \t]*$")
                        (progn
                          (push (make-marker) regions)
                          (set-marker (car regions) (point))
                          (forward-line 1))
                      (forward-line 1)))
                  ;; Set marker for the end of the last region.
                  (push (make-marker) regions)
                  (set-marker (car regions) end)
                  (setq regions (nreverse regions))
                  ;; Apply fill-paragraph to each region.
                  (while (cdr regions)
                    (let ((region-beg (marker-position (car regions)))
                          (region-end (marker-position (cadr regions))))
                      (when (> region-end region-beg)
                        (fill-region-as-paragraph region-beg region-end justify))
                      (pop regions)))
                  ;; Clean up markers to avoid memory leaks.
                  (dolist (m regions)
                    (set-marker m nil))))
              t))
)
      )
  )))

(advice-add 'org-fill-element :after #'org-ai-block--org-fill-element-advice)

(defun org-ai-block--org-element-context-advice (func-call &rest args)
  "For `org-babel-where-is-src-block-result'.
Allow to simplify code by using many org-babel functions."
  (if (not (assq 'org-ai-mode minor-mode-alist))
      (apply func-call args)
    ;; else
    (let ((element (apply func-call args))) ;          (type (org-element-property :type element)))
      (if (string-equal "ai" (org-element-property :type element))
          (cons 'src-block (cdr element)) ; fake "ai" special-block as src-block
        ;; else
        element))))
;; - required for org-babel-where-is-src-block-result
(advice-add 'org-element-context :around #'org-ai-block--org-element-context-advice)
;; - required?????
;; (advice-add 'org-element-at-point :around #'org-ai-block--org-element-context-advice)
