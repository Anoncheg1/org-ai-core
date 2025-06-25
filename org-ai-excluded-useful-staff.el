;;; org-ai-openai.el --- OpenAI API related functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 github.com/Anoncheg1

(cl-defun org-ai-remove-distant-empty-lines (start end)
  "Remove empty lines in current buffer between START and END.
Removes an empty line only if another empty line is two lines above it.
An 'empty line' is blank or whitespace-only.

Example:
  Original:      Result:
  line 1         line 1
                 (empty A) (kept)  (empty A)
  line 2         line 2
                 (empty B) (removed)line 3
  line 3         line 4
                 (empty C) (removed)line 5
  line 4         (empty D)
  line 5         line 6
                 (empty D) (kept)
  line 6
"
  (interactive "r") ;; Usable interactively on a selected region.

  (let ((lines-to-delete-pos '()) ; Stores positions of lines to remove.
        (line-info-list '())      ; Stores (line-pos . is-blank-p) for all lines in region.
        (original-start start)    ; Store original region start for robustness.
        (original-end end))       ; Store original region end for robustness.

    (save-excursion ;; Preserve point and mark positions after execution.

      ;; Phase 1: Collect info about lines in the region.
      ;; Iterate through the region, recording each line's starting position
      ;; and whether it's blank (contains only whitespace).
      (goto-char start)
      (while (< (point) end)
        (push (cons (point) (string-blank-p (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
              line-info-list)
        (forward-line 1))
      ;; Reverse the list to get lines in document order (top to bottom).
      (setq line-info-list (nreverse line-info-list))

      ;; Phase 2: Identify lines for deletion.
      ;; Iterate through the collected line information, keeping track of the
      ;; blank status of the previous two lines to apply the condition.
      (let ((line-minus-1-info nil) ; Stores (pos . is-blank) for the line one step back.
            (line-minus-2-info nil)) ; Stores (pos . is-blank) for the line two steps back.
        (dolist (current-line-info line-info-list)
          (let* ((current-line-pos (car current-line-info))
                 (current-line-is-blank (cdr current-line-info)))

            ;; Condition for deletion: current line is blank AND the line two steps back existed
            ;; and was also blank.
            (when (and current-line-is-blank
                       line-minus-2-info             ; Check if line two steps back actually exists.
                       (cdr line-minus-2-info))      ; Check if that line was blank.
              ;; If condition met, mark current line for deletion.
              ;; `push` adds positions to the front, so the list will be in reverse order
              ;; of appearance (later lines appear first in the list), which is ideal for deletion.
              (push current-line-pos lines-to-delete-pos)))

          ;; Update history for the next iteration:
          ;; Old 'minus-1' becomes new 'minus-2'.
          ;; Current line becomes new 'minus-1'.
          (setq line-minus-2-info line-minus-1-info)
          (setq line-minus-1-info current-line-info))))

    ;; Phase 3: Delete the identified lines.
    ;; Iterate through `lines-to-delete-pos`. Since it's already ordered from
    ;; the largest position to the smallest, deletions occur safely from the
    ;; end of the region towards the beginning, preventing position invalidation.
    (save-excursion
      (dolist (pos lines-to-delete-pos)
        ;; Robustness check: Ensure the position is still within the original bounds.
        (when (and (>= pos original-start) (< pos original-end))
          (goto-char pos) ; Move point to the start of the line to be deleted.
          ;; Delete the current line, including its trailing newline.
          (delete-region (point) (progn (forward-line 1) (point))))))

    (message "Removed empty lines based on condition.")))


;; ;; - Test for `org-ai-remove-distant-empty-lines' function
;; (with-current-buffer (get-buffer-create "*test*")
;;     ;; Set up initial buffer content
;;     (insert "line 1\n\nline 2\n\nline 3\n\nline 4\nline 5\n\nline 6\n")
;;     ;; Define the region to operate on (entire buffer in this case)
;;     (let ((start (point-min))
;;           (end (point-max)))
;;       ;; Call the function
;;       (org-ai-remove-distant-empty-lines start end)))


(provide 'org-ai-excluded-useful-staff)
