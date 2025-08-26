;;; oai-block.el --- oai special block helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Robert Krahn and contributers
;; Copyright (C) 2025 github.com/Anoncheg1

;; This file is NOT part of GNU Emacs.

;; oai-block.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; oai-block.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with oai.el.
;; If not, see <https://www.gnu.org/licenses/>.

;;; Changelog
;; - DONE: complete is short for completion
;; - DONE: oai-block-get-info fail if there is nothing in block.
;; - DONE: rename all CONTEXT to ELEMENT because context cause confusing (Org terms).
;; - DONE: rename all except interface functions to "oai-block-" prefix.

;;; Commentary:

;; Defines functions for dealing with #+begin_ai..#+end_ai special blocks

;; None Org babel: We choose not to fake as babel source block and use
;; functionality because it require too much advices.

;; Note Org terms:
;; - element - "room" you are in (e.g., a paragraph) (TYPE PROPS) (org-element-at-point)
;; - context - "furniture" you are touching within that room (e.g., a bold word, a link). (TYPE PROPS) (org-element-context)
;; - org-dblock-start-re
;; (org-element-property :begin  (oai-block-p)) return content begin, while for other Org elements it return header begining.
;; may be fixed with org-element-put-property, but it is not KISS.

;;; Code:
;;; -=-= all
(require 'org)
(require 'org-element)
(require 'org-macs)
(require 'oai-block-tags)

(when (and (boundp 'org-protecting-blocks) (listp org-protecting-blocks))
  (add-to-list 'org-protecting-blocks "ai"))

(when (boundp 'org-structure-template-alist)
  (add-to-list 'org-structure-template-alist '("A" . "ai")))


;; `org-element-with-disabled-cache' is not available pre org-mode 9.6.6, i.e.
;; emacs 28 does not ship with it
(defmacro oai-block--org-element-with-disabled-cache (&rest body)
  "Run BODY without active org-element-cache."
  (declare (debug (form body)) (indent 0))
  `(cl-letf (((symbol-function #'org-element--cache-active-p) (lambda (&rest _) nil)))
     ,@body))

(defun oai-block-p ()
  "Are we inside a #+begin_ai...#+end_ai block?
Like `org-in-src-block-p'. Return element."
  (oai-block--org-element-with-disabled-cache ;; with cache enabled we get weird Cached element is incorrect warnings
    (cl-loop with context = (org-element-context)
             while (and context
                        (not (equal 'special-block (org-element-type context)))
                        (not (string-equal "ai" (org-element-property :type context))))
             do (setq context (org-element-property :parent context))
             finally return context)))

(defun oai-block-element-by-marker (marker)
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (oai-block-p))))

(defun oai-block-get-info (&optional element no-eval)
  "Parse the header of #+begin_ai...#+end_ai block.
`ELEMENT' is the element of the special block. Return an alist of
key-value pairs.
Like `org-babel-get-src-block-info' but instead of list return only
arguments.
To get value use: (alist-get :value (oai-block-get-info))
Use ELEMENT only in current moment."
  (org-babel-parse-header-arguments
   (org-element-property
    :parameters
    (or element (oai-block-p))) no-eval))

(defun oai-block--string-equal-ignore-case (string1 string2)
  "Helper for backwards compat.
STRING1 and STRING2 are strings. Return t if they are equal
ignoring case."
  (eq 't (compare-strings string1 0 nil string2 0 nil t)))

(defun oai-block-get-content (&optional element)
  "Extracts the text content of the #+begin_ai...#+end_ai block.
`ELEMENT' is the element of the special block.

Will expand noweb templates if an 'oai-noweb' property or
'noweb' header arg is \"yes\".
Use ELEMENT only in current moment, if buffer modified you will need new
ELEMENT."
  (let* ((element (or element (oai-block-p)))
         (content-start (org-element-property :contents-begin element))
         (content-end (org-element-property :contents-end element))
         (unexpanded-content (if (or (not content-start) (not content-end))
                                 (error "Empty block")
                               ;; else
                               (string-trim (buffer-substring-no-properties content-start content-end))))
         (info (oai-block-get-info element))
         (noweb-control (or (alist-get :noweb info nil)
                            (org-entry-get (point) "oai-noweb" 1)
                            "no"))
         (content (if (oai-block--string-equal-ignore-case "yes" noweb-control)
                      (org-babel-expand-noweb-references (list "markdown" unexpanded-content))
                    unexpanded-content)))
    content))

(defun oai-block--get-request-type (info)
  "Look at the header of the #+begin_ai...#+end_ai block.
returns the type of request. `INFO' is the alist of key-value
pairs from `oai-block-get-info'."
  (cond
   ((not (eql 'x (alist-get :chat info 'x))) 'chat)
   ((not (eql 'x (alist-get :completion info 'x))) 'completion)
   ((not (eql 'x (alist-get :complete info 'x))) 'completion)
   ((not (eql 'x (alist-get :image info 'x))) 'image)
   ((not (eql 'x (alist-get :sd-image info 'x))) 'sd-image)
   ((not (eql 'x (alist-get :local info 'x))) 'local-chat)
   (t 'chat)))

(cl-defun oai-block--get-sys (&key info default)
  "Check if :sys exist in #+begin_ai parameters.
If exist return nil or string, if not exist  return `default'."
  (let ((sys-raw  (alist-get :sys info 'x)))
    ;; if 'x - not resent
    (if (eql 'x sys-raw)
        default
      ;; else - nil or string
      sys-raw)))

(defmacro oai-block--let-params (info definitions &rest body)
  "A specialized `let*' macro for Oai parameters.
DEFINITIONS is a list of (VARIABLE &optional DEFAULT-FORM &key TYPE).
TYPE can be 'number or 'identity.
Parameters are sourced from:
1. From Oai block header `info' alist. (e.g., :model \"gpt-4\")
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


(defvar oai-block--roles-regex "\\[SYS\\]:\\|\\[ME\\]:\\|\\[ME:\\]\\|\\[AI\\]:\\|\\[AI_REASON\\]:")

(defun oai-block--chat-role-regions ()
  "Splits the special block by role prompts.
Return line begining positions of first line of content, roles, #+end_ai
line."
  (if-let* ((element (oai-block-p))
            (content-start (org-element-property :contents-begin element))
            (content-end (org-element-property :contents-end element)))
      (let ((result (save-match-data
                      (save-excursion
                        (goto-char content-start)
                        (cl-loop with result
                                 while (search-forward-regexp oai-block--roles-regex content-end t) ; todo, make as global variable
                                 do (push (match-beginning 0) result)
                                 finally return result)))))
        (if result
            (cl-concatenate 'list (list content-start) (reverse result) (list content-end))
          (list content-start content-end)))))



;;; -=-= Interactive

(defcustom oai-block-fontify-markdown t
  "fontinfy ```lang blocks."
  :type 'boolean
  :group 'oai)

(defun oai-mark-last-region ()
  "Marks the last prompt in an oai block."
  (interactive)
  (when-let* ((regions (reverse (oai-block--chat-role-regions)))
              (last-region-end (pop regions))
              (last-region-start (pop regions)))
        (goto-char last-region-end)
        (push-mark last-region-start t t)))

(defun oai-mark-region-at-point ()
  "Marks the prompt at point."
  (interactive)
  (when-let* ((regions (oai-block--chat-role-regions))
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

(defun oai-forward-section (&optional arg)
  "Move forward to end of section.
A negative argument ARG = -N means move backward."
  (interactive "^p")
  ;;   TODO:
  ;; With argument ARG, do it ARG times;
  ;; a negative argument ARG = -N means move backward N paragraphs.
  (when-let* ((regions (oai-block--chat-role-regions))
              (start (cl-find-if (lambda (x) (>= (point) x)) (reverse regions)))
              (end (cl-find-if (lambda (x) (< (point) x)) regions)))
    (oai--debug "oai-forward-section1 %s " (list start end))
    (or arg (setq arg 1))
    (if (> arg 0)
        (goto-char end)
      ;; else - backward
      (let ((prev (cl-find-if (lambda (x) (>= (1- start) x)) (reverse regions))))
        (oai--debug "oai-forward-section2 %s %s " (> (point) start) prev)
        (if (and (> (point) start) ; if at the middle of first section
                 (not prev))
            (goto-char start)
          ;; else
          (goto-char (cl-find-if (lambda (x) (>= (1- start) x)) (reverse regions))))))))

(defun oai-kill-region-at-point (&optional arg)
  "Kills the prompt at point.
The numeric `ARG' can be used for killing the last n."
  (interactive "P")
  (cl-loop repeat (or arg 1)
           do (when-let ((region (oai-mark-region-at-point)))
                (cl-destructuring-bind (start . end) region
                  (kill-region end start)))))

;;; -=-= Markers

(defun oai-block--get-content-end-marker (&optional element)
  "Return a marker for the :contents-end property of ELEMENT.
Used in `oai-call-block'"
  (let ((el (or element (oai-block-p))))
    (let ((contents-end-pos (org-element-property :contents-end el)))
      (when contents-end-pos
        (copy-marker contents-end-pos)))))

(defun oai-block-get-header-marker (&optional element)
  "Return marker for ai block at current buffer at current positon.
Use ELEMENT only in current moment."
  (let ((el (or element (oai-block-p))))
    ;; (with-current-buffer (org-element-property :buffer el)
    (if el
        (save-excursion
          (goto-char (org-element-property :contents-begin el))
          (forward-line -1)
          (copy-marker (point))))))

;;; -=-= Result

(defun oai-block-insert-result (result &optional result-params hash exec-time)
  "Modified `org-babel-insert-result' function.
Insert RESULT into the current buffer.
TODO: EXEC-TIME."
  (when (stringp result)
    (setq result (substring-no-properties result)))
  (save-excursion
    (let* ((visible-beg (point-min-marker))
           (visible-end (copy-marker (point-max) t))
           (existing-result (oai-block-where-is-result t nil hash))
           ;; When results exist outside of the current visible
           ;; region of the buffer, be sure to widen buffer to
           ;; update them.
           (outside-scope (and existing-result
                               (buffer-narrowed-p)
                               (or (> visible-beg existing-result)
                                   (<= visible-end existing-result))))
           beg end indent)
      (unwind-protect
          (progn
            (when outside-scope (widen)) ;; ---- WIDDEN
            (goto-char existing-result) ;; must be true
            (setq indent (current-indentation))
            (forward-line 1)
            (setq beg (point))
            (cond
             ((member "replace" result-params)
              (delete-region (point) (org-babel-result-end)))
             ((member "append" result-params)
              (goto-char (org-babel-result-end)) (setq beg (point-marker))))
            (goto-char beg) (insert (concat result "\n"))
            (setq end (copy-marker (point) t))
            (org-babel-examplify-region beg end "")
            ;; finally
            (when outside-scope (narrow-to-region visible-beg visible-end)) ;; ---- NARROW
            ))))
  t)

(defun oai-block-where-is-result (&optional insert _info hash)
  "Modified `org-babel-where-is-src-block-result' function."
  (oai--debug "oai-block-where-is-result")
  (let ((context (oai-block-p)))
    (catch :found
      (org-with-wide-buffer
       (let* ((name (org-element-property :name context))
              (named-results (and name (org-babel-find-named-result name))))
         (goto-char (or named-results (org-element-end context)))
         (print (list "name" name named-results))
         (cond
          ;; Existing results named after the current source.
          (named-results
           (when (org-babel--clear-results-maybe hash)
             (org-babel--insert-results-keyword name hash))
           (throw :found (point)))
          ;; Named results expect but none to be found.
          (name)
          ;; No possible anonymous results at the very end of
          ;; buffer or outside CONTEXT parent.
          ((eq (point)
               (or (pcase (org-element-type (org-element-parent context))
                     ((or `section `org-data)
                      (org-element-end (org-element-parent context)))
                     (_ (org-element-contents-end
                         (org-element-parent context))))
                   (point-max))))
          ;; Check if next element is an anonymous result below
          ;; the current block.
          ((let* ((next (org-element-at-point))
                  (end (save-excursion
                         (goto-char
                          (org-element-post-affiliated next))
                         (line-end-position)))
                  (empty-result-re (concat org-babel-result-regexp "$"))
                  (case-fold-search t))
             (re-search-forward empty-result-re end t))
           (forward-line 0)
           (when (org-babel--clear-results-maybe hash)
             (org-babel--insert-results-keyword nil hash))
           (throw :found (point)))))
     ;; ;; Ignore other elements.
     ;; (_ (throw :found nil))
       )
      ;; No result found.  Insert a RESULTS keyword below element, if
      ;; appropriate.  In this case, ensure there is an empty line
      ;; after the previous element.
      (when insert
        (save-excursion
          (goto-char (min (org-element-end context) (point-max)))
          (skip-chars-backward " \t\n")
          (forward-line)
          (unless (bolp) (insert "\n"))
          (insert "\n")
          (org-babel--insert-results-keyword
           (org-element-property :name context) hash)
          (point))))))

(defun oai-block-remove-result (&optional info keep-keyword)
  "Remove the result of the current source block.
INFO argument is currently ignored.
When KEEP-KEYWORD is non-nil, keep the #+RESULT keyword and just remove
the rest of the result."
  (interactive)
  (let ((location (oai-block-where-is-result nil info))
	(case-fold-search t))
    (when location
      (save-excursion
        (goto-char location)
	(when (looking-at org-babel-result-regexp)
	  (delete-region
	   (if keep-keyword (line-beginning-position 2)
	     (save-excursion
	       (skip-chars-backward " \r\t\n")
	       (line-beginning-position 2)))
	   (progn (forward-line) (org-babel-result-end))))))))


;;; -=-= Markdown block, fontify mostly
(defvar oai-block--markdown-begin-re "^```\\([^ \t\n[{]+\\)[\s-]?\n")
(defvar oai-block--markdown-end-re "^```[\s-]?$")

(defun oai-block--fontify-markdown-subblocks (start end)
  "Fontify ```language ... ``` fenced mardown code blocks.
 We search for begining of block, then for end of block, then fontify
 with `org-src-font-lock-fontify-block'."
  (goto-char start)
  (let ((case-fold-search t))
    (while (and (< (point) end)
                (re-search-forward oai-block--markdown-begin-re end t))
      (let* ((lang (match-string 1))
             (block-begin (match-end 0)))
        ;; (print (list "re-search-forward4" (point) end))
        (when (re-search-forward oai-block--markdown-end-re end t)
          (let ((block-end (match-beginning 0)))
            (when (fboundp (org-src-get-lang-mode lang)) ; for org-src-font-lock-fontify-block
              (org-src-font-lock-fontify-block lang block-begin block-end)
              t
              )))))))

(defun oai-block--font-lock-fontify-ai-subblocks (limit)
  "Fontify markdown subblocks in ai blocks, up to LIMIT.
This is special fontify function, that return t when match found.
We insert advice right after `org-fontify-meta-lines-and-blocks-1' witch
called as a part of Org Font Lock mode configuration of keywords (in
`org-set-font-lock-defaults' and corresponding font-lock highlighting
rules in `font-lock-defaults' variable."
  (if oai-block-fontify-markdown
      (let ((case-fold-search t)
            (ret))
        (while (and (re-search-forward "^#\\+begin_ai[^\n]*\n" limit t)
                    (< (point) limit))
          (let ((beg (match-end 0)))
            (when (re-search-forward "^#\\+end_ai.*$" nil t)
              (let ((end (match-beginning 0)))
                (save-match-data
                  (setq ret (oai-block--fontify-markdown-subblocks beg end)))
                ))))
        ;; required by font lock mode:
        (goto-char limit)
        ret
        )))

(defun oai-block--insert-after (list pos element)
  "Insert ELEMENT at after position POS in LIST."
  (nconc (take (1+ pos) list) (list element) (nthcdr (1+ pos) list)))

;;; -=-= Fontify Markdown blocks and Tags

(defun oai-block--set-ai-keywords()
  "Hook, that Insert our fontify functions in Org font lock keywords."
  (setq org-font-lock-extra-keywords (oai-block--insert-after
                                      org-font-lock-extra-keywords
                                      (seq-position org-font-lock-extra-keywords '(org-fontify-meta-lines-and-blocks))
                                      '(oai-block--font-lock-fontify-ai-subblocks)))
  (setq org-font-lock-extra-keywords (oai-block--insert-after
                                      org-font-lock-extra-keywords
                                      (seq-position org-font-lock-extra-keywords '(org-fontify-meta-lines-and-blocks))
                                      '(oai-block-tags--font-lock-fontify-links)))
  )

;;; -=-= Select markdown block

;; not used now TODO: make interactive for region mark
(defun oai-block-markdown-mark-fenced-code-body (&optional limit-begin limit-end)
  "Mark content inside Markdown fenced code block (```), excluding header/footer.
LIMIT-BEGIN and LIMIT-END restrict the search region around point.
Returns t if region was marked, nil otherwise."
    (let ((point-pos (point))
          (start nil)
          (end nil))
      (save-excursion
        ;; Find start fence
        (when (re-search-backward oai-block--markdown-begin-re (or limit-begin (point-min)) t)
          (setq start (match-end 0))
          (goto-char point-pos)
          ;; do we inside owr block?
          (when (and (re-search-backward oai-block--markdown-end-re  (or limit-begin (point-min)) t)
                     (> (match-beginning 0) start)
                     (setq start nil))))
        ;; Find end fence
        (goto-char point-pos)
        (when (and start
                   (re-search-forward oai-block--markdown-end-re (or limit-end (point-max)) t))
          (setq end (match-beginning 0))
          (goto-char point-pos)
          ;; do we inside owr block?
          (when (and (re-search-forward oai-block--markdown-begin-re (or limit-end (point-max)) t)
                     (< (match-end 0) end)
                     (setq end nil)))))
      ;; If point is inside fences, mark region
      (when (and start end (> point-pos start) (< point-pos end))
        (set-mark start)
        (goto-char end)
        (forward-line -1)
        (end-of-line)
        (activate-mark)
        t)))


(defun oai-block-mark-src-block-body ()
  "Mark Org blocks content around cursor.
Excluding header and footer."
  (interactive)
  (let ((elem (org-element-at-point)))
    (goto-char (org-element-property :begin elem))
    (forward-line 1)
    (set-mark (point))
    (let ((case-fold-search t))
          (re-search-forward "#\\+end_" nil t))
    (beginning-of-line)
    ;; (goto-char (org-element-property :end elem))
    ;; (forward-line -2)
    ;; (end-of-line)
    (activate-mark)
    t))

;; like M-h but for C-c h

(defun oai-block-mark-md-block-body ()
  "Mark content of Markdown code block, or fallback to org-mark-element.
Mark or select block content around cursor."
  (interactive)
  (cond
   ;; 1) mardown in ai block & 2) ai block only
   ((and (featurep 'oai-block)
         (bound-and-true-p oai-mode)
         (if-let*((element (oai-block-p))
                  (content-start (org-element-property :contents-begin element))
                  (content-end (org-element-property :contents-end element)))
             (if
                 ;; 1 mardown in ai block
                 (oai-block-markdown-mark-fenced-code-body content-start content-end)
                 t
               ;; else - 2 ai block only
               (set-mark content-start)
               (goto-char content-end)
               (activate-mark)
               t))))
   ;; 1) mardown in Org blocks & 2) Org block only
   ((let ((elem (org-element-at-point)))
           (and (member (org-element-type elem) '(src-block example-block quote-block verse-block special-block comment-block))
                (if (and (featurep 'oai-block)
                         (oai-block-markdown-mark-fenced-code-body (org-element-property :begin elem) (org-element-property :end elem)))
                    t
                  ;; else
                  (oai-block-mark-src-block-body)))))
   ;; Markdown code block (``` fenced block)
   ;; ((markdown-mark-fenced-code-body))
   ;; Otherwise
   (t
    (org-mark-element))))

;;; provide
(provide 'oai-block)
;;; oai-block.el ends here
