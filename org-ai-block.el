;;; org-ai-block.el --- org-ai special block helpers -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;; org-ai.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; org-ai.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with org-ai.el.
;; If not, see <https://www.gnu.org/licenses/>.

;;; Changelog
;; - TODO: rename org-ai-special-block to org-ai-block-p
;; - DONE: complete is short for completion
;; - DONE: org-ai-block-get-info fail if there is nothing in block.
;; - DONE: rename all CONTEXT to ELEMENT because context cause confusing (Org terms).

;;; Commentary:

;; Defines functions for dealing with #+begin_ai..#+end_ai special blocks

;; Note Org terms:
;; - element - "room" you are in (e.g., a paragraph) (TYPE PROPS) (org-element-at-point)
;; - context - "furniture" you are touching within that room (e.g., a bold word, a link). (TYPE PROPS) (org-element-context)

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-macs)

(when (and (boundp 'org-protecting-blocks) (listp org-protecting-blocks))
  (add-to-list 'org-protecting-blocks "ai"))

(when (boundp 'org-structure-template-alist)
  (add-to-list 'org-structure-template-alist '("A" . "ai")))

;; `org-element-with-disabled-cache' is not available pre org-mode 9.6.6, i.e.
;; emacs 28 does not ship with it
(defmacro org-ai-block--org-element-with-disabled-cache (&rest body)
  "Run BODY without active org-element-cache."
  (declare (debug (form body)) (indent 0))
  `(cl-letf (((symbol-function #'org-element--cache-active-p) (lambda (&rest _) nil)))
     ,@body))

(defun org-ai-block-p ()
  "Are we inside a #+begin_ai...#+end_ai block?
Like `org-in-src-block-p'."
  (org-ai-block--org-element-with-disabled-cache ;; with cache enabled we get weird Cached element is incorrect warnings
    (cl-loop with context = (org-element-context)
             while (and context
                        (not (equal 'special-block (org-element-type context)))
                        (not (string-equal "ai" (org-element-property :type context))))
             do (setq context (org-element-property :parent context))
             finally return context)))

(defun org-ai-block-get-info (&optional element)
  "Parse the header of #+begin_ai...#+end_ai block.
`ELEMENT' is the element of the special block. Return an alist of
key-value pairs.
Like org-babel-get-src-block-info."
  (let* ((element (or element (org-ai-block-p)))
         (header-start (org-element-property :post-affiliated element))
         (header-end (or (org-element-property :contents-begin element)
                         (point-at-eol))) ; fix for empty block
                     )
    (if (or (not header-start) (not header-end))
        (error "Error: org-ai was not able to extract the beginning/end of the org-ai block")
      (save-match-data
        (let* ((string (string-trim (buffer-substring-no-properties header-start header-end)))
               (string (string-trim-left (replace-regexp-in-string "^#\\+begin_ai" "" string))))
          (org-babel-parse-header-arguments string))))))

(defun org-ai-block--string-equal-ignore-case (string1 string2)
  "Helper for backwards compat.
STRING1 and STRING2 are strings. Return t if they are equal
ignoring case."
  (eq 't (compare-strings string1 0 nil string2 0 nil t)))

(defun org-ai-block-get-content (&optional element)
  "Extracts the text content of the #+begin_ai...#+end_ai block.
`ELEMENT' is the element of the special block.

Will expand noweb templates if an 'org-ai-noweb' property or
'noweb' header arg is \"yes\""

  (let* ((element (or element (org-ai-block-p)))
         (content-start (org-element-property :contents-begin element))
         (content-end (org-element-property :contents-end element))
         (unexpanded-content (string-trim (buffer-substring-no-properties content-start content-end)))
         (info (org-ai-block-get-info element))
         (noweb-control (or (alist-get :noweb info nil)
                            (org-entry-get (point) "org-ai-noweb" 1)
                            "no"))
         (content (if (org-ai-block--string-equal-ignore-case "yes" noweb-control)
                      (org-babel-expand-noweb-references (list "markdown" unexpanded-content))
                      unexpanded-content)))
    content))

(defun org-ai-block--get-request-type (info)
  "Look at the header of the #+begin_ai...#+end_ai block.
returns the type of request. `INFO' is the alist of key-value
pairs from `org-ai-block-get-info'."
  (cond
   ((not (eql 'x (alist-get :chat info 'x))) 'chat)
   ((not (eql 'x (alist-get :completion info 'x))) 'completion)
   ((not (eql 'x (alist-get :complete info 'x))) 'completion)
   ((not (eql 'x (alist-get :image info 'x))) 'image)
   ((not (eql 'x (alist-get :sd-image info 'x))) 'sd-image)
   ((not (eql 'x (alist-get :local info 'x))) 'local-chat)
   (t 'chat)))

(cl-defun org-ai-block--get-sys (&key info default)
  "Check if :sys exist in #+begin_ai parameters.
If exist return nil or string, if not exist  return `default'."
  (let ((sys-raw  (alist-get :sys info 'x)))
    ;; if 'x - not resent
    (if (eql 'x sys-raw)
        default
      ;; else - nil or string
      sys-raw)))

(defmacro org-ai-block--let-params (info definitions &rest body)
  "A specialized `let*' macro for Org-AI parameters.
DEFINITIONS is a list of (VARIABLE &optional DEFAULT-FORM &key TYPE).
TYPE can be 'number or 'identity.
Parameters are sourced from:
1. From Org-AI block header `info' alist. (e.g., :model \"gpt-4\")
2. Org inherited property. (e.g., #+PROPERTY: model gpt-4)
3. DEFAULT-FORM."
  `(let* ,(cl-loop for def-item in definitions
                   collect
                   (let* ((sym (car def-item))
                          (default-form (cadr def-item))
                          ;; Look for the :type keyword in the rest of the list
                          (type (cadr (member :type def-item))))
                     `(,sym (or (alist-get ,(intern (format ":%s" (symbol-name sym))) info)
                                ,(cond
                                  ((string= (symbol-name sym) "model") ; Special: no conversion for model
                                   `(org-entry-get-with-inheritance ,(symbol-name sym)))
                                  ((eq type 'number)
                                   `(when-let ((prop (org-entry-get-with-inheritance ,(symbol-name sym))))
                                      (if (stringp prop) (string-to-number prop) prop)))
                                  (t ; Default: identity conversion
                                   `(org-entry-get-with-inheritance ,(symbol-name sym))))
                                ,@(when default-form `(,default-form))))))
     ,@body))


(defun org-ai-block--get-contents-end-marker (element)
  "Return a marker for the :contents-end property of ELEMENT."
  (with-current-buffer (org-element-property :buffer element)
    (let ((contents-end-pos (org-element-property :contents-end element)))
      (when contents-end-pos
        (copy-marker contents-end-pos)))))


(defun org-ai-block--chat-role-regions ()
  "Splits the special block by role prompts."
  (if-let* ((element (org-ai-block-p))
            (content-start (org-element-property :contents-begin element))
            (content-end (org-element-property :contents-end element)))
      (let ((result (save-match-data
                      (save-excursion
                        (goto-char content-start)
                        (cl-loop with result
                                 while (search-forward-regexp "\\[SYS\\]:\\|\\[ME\\]:\\|\\[AI\\]:\\|\\[AI_REASON\\]:" content-end t)
                                 do (push (match-beginning 0) result)
                                 finally return result)))))
        (if result
            (cl-concatenate 'list (list content-start) (reverse result) (list content-end))
          (list content-start content-end)))))


(defun org-ai-mark-last-region ()
  "Marks the last prompt in an org-ai block."
  (interactive)
  (when-let* ((regions (reverse (org-ai-block--chat-role-regions)))
              (last-region-end (pop regions))
              (last-region-start (pop regions)))
        (goto-char last-region-end)
        (push-mark last-region-start t t)))

(defun org-ai-mark-region-at-point ()
  "Marks the prompt at point."
  (interactive)
  (when-let* ((regions (org-ai-block--chat-role-regions))
              (start (cl-find-if (lambda (x) (>= (point) x)) (reverse regions)))
              (end (cl-find-if (lambda (x) (<= (point) x)) regions)))
    (when (= start end)
      (setq end (cl-find-if (lambda (x) (< start x)) regions)))
    (when (not end)
      (setq end start)
      (setq start (cl-find-if (lambda (x) (> end x)) (reverse regions))))
    (when (and start end)
      (goto-char start)
      (push-mark end t t)
      (cons start end))))

(defun org-ai-kill-region-at-point (&optional arg)
  "Kills the prompt at point.
The numeric `ARG' can be used for killing the last n."
  (interactive "P")
  (cl-loop repeat (or arg 1)
           do (when-let ((region (org-ai-mark-region-at-point)))
                (cl-destructuring-bind (start . end) region
                  (kill-region end start)))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(provide 'org-ai-block)

;;; org-ai-block.el ends here
