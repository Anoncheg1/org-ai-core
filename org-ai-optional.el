;;; org-ai-optional.el --- Useful functions that not enabled by default.  -*- lexical-binding: t; -*-

;;; License

;; Copyright (C) 2025 github.com/Anoncheg1

;;; Code:

;; (cl-defun org-ai-optional-remove-distant-empty-lines (start end)
;;   "Remove empty lines in current buffer between START and END.
;; Removes an empty line only if another empty line is two lines above it.
;; An 'empty line' is blank or whitespace-only.

;; Example:
;;   Original:      Result:
;;   line 1         line 1
;;                  (empty A) (kept)  (empty A)
;;   line 2         line 2
;;                  (empty B) (removed)line 3
;;   line 3         line 4
;;                  (empty C) (removed)line 5
;;   line 4         (empty D)
;;   line 5         line 6
;;                  (empty D) (kept)
;;   line 6

;; Case ME:
;; line 1         line 1
;;                  (empty A) (kept)  (empty A)
;; line 2         line 2
;;                  (empty B) (not removed)
;; line 3 with [ME]:   line 4
;; "
;;   (interactive "r") ;; Usable interactively on a selected region.

;;   (let ((lines-to-delete-pos '()) ; Stores positions of lines to remove.
;;         (line-info-list '())      ; Stores (line-pos . is-blank-p) for all lines in region.
;;         (original-start start)    ; Store original region start for robustness.
;;         (original-end end))       ; Store original region end for robustness.

;;     (save-excursion ;; Preserve point and mark positions after execution.

;;       ;; Phase 1: Collect info about lines in the region.
;;       ;; Iterate through the region, recording each line's starting position
;;       ;; and whether it's blank (contains only whitespace).
;;       (goto-char start)
;;       (while (< (point) end)
;;         (push (cons (point) (string-blank-p (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
;;               line-info-list)
;;         (forward-line 1))
;;       ;; Reverse the list to get lines in document order (top to bottom).
;;       (setq line-info-list (nreverse line-info-list))

;;       ;; Phase 2: Identify lines for deletion.
;;       ;; Iterate through the collected line information, keeping track of the
;;       ;; blank status of the previous two lines to apply the condition.
;;       (let ((line-minus-1-info nil) ; Stores (pos . is-blank) for the line one step back.
;;             (line-minus-2-info nil)) ; Stores (pos . is-blank) for the line two steps back.
;;         (dolist (current-line-info line-info-list)
;;           (let* ((current-line-pos (car current-line-info))
;;                  (current-line-is-blank (cdr current-line-info)))

;;             ;; Condition for deletion: current line is blank AND the line two steps back existed
;;             ;; and was also blank.
;;             (when (and current-line-is-blank
;;                        line-minus-2-info             ; Check if line two steps back actually exists.
;;                        (cdr line-minus-2-info))      ; Check if that line was blank.
;;               ;; If condition met, mark current line for deletion.
;;               ;; `push` adds positions to the front, so the list will be in reverse order
;;               ;; of appearance (later lines appear first in the list), which is ideal for deletion.
;;               (push current-line-pos lines-to-delete-pos)))

;;           ;; Update history for the next iteration:
;;           ;; Old 'minus-1' becomes new 'minus-2'.
;;           ;; Current line becomes new 'minus-1'.
;;           (setq line-minus-2-info line-minus-1-info)
;;           (setq line-minus-1-info current-line-info))))

;;     ;; Phase 3: Delete the identified lines.
;;     ;; Iterate through `lines-to-delete-pos`. Since it's already ordered from
;;     ;; the largest position to the smallest, deletions occur safely from the
;;     ;; end of the region towards the beginning, preventing position invalidation.
;;     (save-excursion
;;       (dolist (pos lines-to-delete-pos)
;;         ;; Robustness check: Ensure the position is still within the original bounds.
;;         (when (and (>= pos original-start) (< pos original-end))
;;           (goto-char pos) ; Move point to the start of the line to be deleted.
;;           ;; Delete the current line, including its trailing newline.
;;           (delete-region (point) (progn (forward-line 1) (point))))))

;;     (message "Removed empty lines based on condition.")))

(require 'cl-lib) ; Ensure cl-lib is loaded for cl-defun and cl-destructuring-bind

(cl-defun org-ai-optional-remove-distant-empty-lines (start end)
  "Remove empty lines in current buffer between START and END.
Removes an empty line only if another empty line is two lines above it.
An 'empty line' is blank or whitespace-only.
Does not remove an empty line if the line immediately following it contains '[ME]:'.
"
  (interactive "r")

  (let ((lines-to-delete-pos '())
        (original-start start)
        (original-end end))

    (save-excursion
      ;; Phase 1: Collect info about lines in the region.
      ;; Each element: (line-start-position is-blank-p line-text)
      (let ((line-data-list '()))
        (goto-char start)
        (while (and (< (point) end) (not (eobp))) ; Iterate as long as point is within region and not end of buffer
          (let* ((line-start-pos (line-beginning-position))
                 (line-end-pos (line-end-position))
                 (line-text (buffer-substring-no-properties line-start-pos line-end-pos))
                 (is-blank (string-blank-p line-text)))
            ;; Store as a simple list: (position is-blank-p line-text)
            (push (list line-start-pos is-blank line-text) line-data-list))
          (forward-line 1))
        (setq line-data-list (nreverse line-data-list)) ; Reverse to get in document order

        ;; Phase 2: Identify lines for deletion.
        ;; Iterate through the list with indices to check previous and next lines efficiently.
        (dotimes (i (length line-data-list))
          ;; Use cl-destructuring-bind for clear access to current line's data
          (cl-destructuring-bind (current-line-pos current-is-blank current-text)
              (nth i line-data-list)
            (let* (
                   ;; Safely get blank status for previous two lines
                   (prev-line-is-blank
                    (and (> i 0)
                         (let ((prev-data (nth (1- i) line-data-list)))
                           ;; (nth 1 prev-data) gets the 'is-blank-p' value (second element of the list)
                           (if prev-data (nth 1 prev-data) nil))))

                   (prev-prev-line-is-blank
                    (and (> i 1)
                         (let ((prev-prev-data (nth (- i 2) line-data-list)))
                           ;; (nth 1 prev-prev-data) gets the 'is-blank-p' value
                           (if prev-prev-data (nth 1 prev-prev-data) nil))))

                   ;; Safely get text for the next line
                   (next-line-text
                    (and (< (1+ i) (length line-data-list))
                         (let ((next-data (nth (1+ i) line-data-list)))
                           ;; (nth 2 next-data) gets the 'line-text' value (third element of the list)
                           (if next-data (nth 2 next-data) nil))))

                   (next-line-contains-me-p (and next-line-text (string-match-p "\\[ME\\]:" next-line-text))))

              ;; Condition for deletion:
              ;; 1. Current line is blank.
              ;; 2. Line two steps back was blank.
              ;; 3. The next line does NOT contain "[ME]:".
              (when (and current-is-blank
                         prev-prev-line-is-blank
                         (not next-line-contains-me-p))
                (push current-line-pos lines-to-delete-pos))))))

    ;; Phase 3: Delete the identified lines.
    ;; Delete from largest position to smallest to avoid invalidating positions.
    (save-excursion
      (dolist (pos lines-to-delete-pos)
        ;; Ensure the position is still within the original bounds.
        (when (and (>= pos original-start) (< pos original-end))
          (goto-char pos)
          ;; Delete the current line, including its trailing newline.
          (delete-region (point) (progn (forward-line 1) (point))))))

    ;; (message "Removed empty lines based on condition.")
    )))


;; ;; - Test for `org-ai-optional-remove-distant-empty-lines' function
;; (with-temp-buffer
;;     ;; Set up initial buffer content
;;     (insert "line 1\n\nline 2\n\nline 3\n\nline 4\nline 5\n\nline 6\n")
;;     ;; Define the region to operate on (entire buffer in this case)
;;     (let ((start (point-min))
;;           (end (point-max)))
;;       ;; Call the function
;;       (org-ai-optional-remove-distant-empty-lines start end))
;;       (buffer-substring-no-properties                           (point-min)
;;                                                                 (point-max))
;;     )
;; ;; [ME]: case
;; (with-temp-buffer
;;     ;; Set up initial buffer content
;;     (insert "line 1\n\nline 2\n\nline 3\n\nline 4\n\nline 5\n\n[ME]:line 6\n")
;;     ;; Define the region to operate on (entire buffer in this case)
;;     (let ((start (point-min))
;;           (end (point-max)))
;;       ;; Call the function
;;       (org-ai-optional-remove-distant-empty-lines start end))
;;       (buffer-substring-no-properties                           (point-min)
;;                                                                 (point-max))
;;     )


(defun org-ai-optional-remove-headers (beg-pos end-pos)
  "Remove Org mode header prefixes between BEG-POS and END-POS in the current buffer.
Uses `org-outline-regexp-bol' to match headers, respecting user-configured prefixes."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg-pos end-pos)
      (goto-char (point-min))
      (replace-regexp-in-region org-outline-regexp-bol "" (point-min) (point-max)))))


(provide 'org-ai-optional)

(defcustom org-ai-optional-fill-paragraph-functions
  (list
   'org-fill-paragraph)
   "List of steps to perform in the `my/org-fill-paragraph' function.
Replace single `fill-paragraph-function' variable with list of
functions."
  :type '(repeat function)
  :group 'my/org)

(defun org-ai-optional-fill-paragraph (&optional justify region)
  "Call functions until success.
Replace single `fill-paragraph-function' with list of functions.
Usage:
(setq org-ai-optional-fill-paragraph-functions
          (append (list #'my/org-ai-fill-paragraph)
                  org-ai-optional-fill-paragraph-functions))

(keymap-set org-mode-map \"M-q\" #'org-ai-optional-fill-paragraph)
"
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (when current-prefix-arg 'full) t)))
  ;; call in loop functions, untill one return true
  (seq-find (lambda(step)
                ;; (message step) ; debug
                (funcall step justify region))
            my/org-fill-paragraph-functions))


(defmacro org-ai-optional--apply-to-region-lines (func start end &rest args)
  "Apply FUNC to each line in region from START to END with ARGS.
START and END is a pointer. FUNC is called with
(line-start line-end . ARGS) for each line.
Executed inside `save-excursion'."
  `(let ((end-marker (copy-marker ,end)))

     (save-excursion
       (goto-char ,start)
       (while (< (point) (marker-position end-marker))
         (let ((line-start (line-beginning-position)) ; may be replace to just (point)
               (line-end (line-end-position)))
           ;; (print (list "my/apply-to-region-lines aa" line-start line-end))
           (if (< line-start line-end)
             (apply ,func line-start line-end ,@args)
             ;; else - skip emtpy line
           (forward-line 1)))))))


(defun org-ai-optional-block-fill-paragraph (&optional justify region)
  "Fill every line as paragraph in the current Org AI block.
Ignoring code blocks that start with '```sometext' and end with '```'."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list (when current-prefix-arg 'full) t)))
  ;; inspired by `org-fill-element'
  (with-syntax-table org-mode-transpose-word-syntax-table
    (let ((element (org-ai-block-p)))
      (when (and element (string-equal "ai" (org-element-property :type element)))
        ;; Determine the boundaries of the content
        (let ((beg (max (point-min) (org-element-contents-begin element))) ; first line of content
              (end (min (point-max) (org-element-contents-end element)))
              block-start block-end)
          ;; Ignore code blocks that start with "```sometext" and end with "```"
          (save-excursion
            (while (< beg end)
              (goto-char beg)
              (if (re-search-forward "^[ \t\f]*```\\w" end t) ; ex. "    ```elisp"
                  (progn
                    (setq block-start (copy-marker (line-beginning-position)))
                    (if (re-search-forward "^[ \t\f]*```[ \t\f]*$" end t) ; ex. "    ```      "
                        (progn
                          (setq block-end (copy-marker (line-beginning-position)))
                          (my/apply-to-region-lines #'fill-region-as-paragraph beg (marker-position block-start) justify)
                          (goto-char (marker-position block-end))
                          (forward-line 1)
                          (setq beg (point)))
                      ;; else - not found end of block
                      (set-marker block-start nil)))
                ;; else - no block - apply to every line
                (org-ai-optional--apply-to-region-lines #'fill-region-as-paragraph beg end justify)
                (setq beg end)))
            ;; (print "my/org-ai-fill-paragraph return t")
            t))))))
