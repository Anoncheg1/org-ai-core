;;; oai-block-tags.el --- Logging for oai in separate buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 github.com/Anoncheg1

;;; License

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Licensed under the GNU Affero General Public License, version 3 (AGPLv3)
;; <https://www.gnu.org/licenses/agpl-3.0.en.html>
;;; Commentary:
;; How this works?
;; We appply `oai-block-tags-replace' to text of last user request. in oai.el: (oai-restapi--modify-last-user-content expanded #'oai-block-tags-replace))
;;
;;; Code
;;; -=-= variables
(require 'org)

(defvar oai-block-tags--regexes '(
                               ;; :backtrace "@Backtrace`?\\([^a-zA-Z]\\|$\\)"
                               :backtrace "\\(`?@Backtrace`?\\)\\([^a-zA-Z\"']\\|$\\)"
                               :path "`?@\\(\\.\\.?/\\|\\.\\.?\\\\\\|\\.\\.?\\|/\\|\\\\\\|[A-Za-z]:\\\\\\)[a-zA-Z0-9_./\\\\-]*`?"
                                          ))
(defvar oai-block-tags--regexes-path "`?@\\(\\.\\.?/\\|\\.\\.?\\\\\\|\\.\\.?\\|/\\|\\\\\\|[A-Za-z]:\\\\\\)[a-zA-Z0-9_./\\\\-]*`?")

(defvar oai-block-tags--markdown-prefixes '(:backtrace "```elisp-backtrace"
                                         :path-directory "```ls-output"
                                         :path-file  "```"))

(defvar oai-block-tags--backtrace-max-lines 12
  "Max lines to get from Backtrace buffer from begining.
All lines are rarely required, first 4-8 are most imortant.")

(defvar oai-block-tags-use-simple-directory-content nil
  "If non-nil use `directory-files' with simple list of item.
Otherwise ls command used.  Also `directory-files-and-attributes' may be
used.")


(cl-assert
  (equal (mapcar (lambda (s)
                   (when (string-match oai-block-tags--regexes-path s)
                     (substring s (match-beginning 0) (match-end 0))))
                 '("@/file-s_s"
                   "@/file.t_xt"
                   "@./file.txt"
                   "@/some/path/file.txt"
                   "@C:\\some\\file.txt"
                   "@L:\\folder\\file.txt"
                   "@\\network\\share"
                   "@.\\windowsfile"
                   "@/file/"
                   "@/file.txt/"
                   "@./file.txt/"
                   "@/some/path/file.txt/"
                   "@C:\\some\\file.txt\\"
                   "@L:\\folder\\file.txt\\"
                   "@\\network\\share\\"
                   "@.\\windowsfile\\"
                   "@Backtrace"
                   "@not/a/path"
                   "@Backtrace"
                   "@not/a/path"
                   "@not/a/path/"
                   "@../right"
                   "@../right/"
                   "@.."
                   "@."
                   "@/"))
         '("@/file-s_s" "@/file.t_xt" "@./file.txt" "@/some/path/file.txt" "@C:\\some\\file.txt" "@L:\\folder\\file.txt" "@\\network\\share" "@.\\windowsfile" "@/file/" "@/file.txt/" "@./file.txt/" "@/some/path/file.txt/" "@C:\\some\\file.txt\\" "@L:\\folder\\file.txt\\" "@\\network\\share\\" "@.\\windowsfile\\" nil nil nil nil nil "@../right" "@../right/" "@.." "@." "@/")))

;;; -=-= Backtrace

(defun oai-block-tags--take-n-lines (string n)
  "Return a string with the first N lines from STRING.
If N exceeds the number of lines, return all lines. If N <= 0, return an empty string."
  (let* ((lines (split-string string "\n"))
         (lines-to-keep (cl-subseq lines 0 (min (max 0 n) (length lines)))))
    (mapconcat #'identity lines-to-keep "\n")))

;; (oai-block-tags--take-n-lines "a\nb\nc\nd" 2)
;; (oai-block-tags--take-n-lines "a\nb\nc\nd" 4)
;; (oai-block-tags--take-n-lines "a\nb\nc" 10)
;; (oai-block-tags--take-n-lines "a\nb\nc" 0) ;; ""
;; (oai-block-tags--take-n-lines "a\nb\nc" -3) ;; ""
;; (oai-block-tags--take-n-lines "" 4) ;; ""
;; (oai-block-tags--take-n-lines "x\ny\nz\n" 2)
;; (oai-block-tags--take-n-lines nil 2) ;; error


(defun oai-block-tags--get-backtrace-buffer-string ()
  "Return the contents of the *Backtrace* buffer as a string, or nil if it does not exist."
  (let ((buf (get-buffer "*Backtrace*")))
    (when buf
      (with-current-buffer buf
        (string-trim (substring-no-properties (buffer-string)))))))

;;; -=-= Links: Files & Directories

(defun oai-block-tags--get-directory-content (path-string)
  (if oai-block-tags-use-simple-directory-content
      (concat (apply #'mapconcat #'identity (directory-files path-string)  '("\n")))
    ;; else
    (let ((buf (dired-noselect path-string)))
      (unwind-protect
          (with-current-buffer buf
            (buffer-substring-no-properties (point-min) (point-max)))
        (kill-buffer buf)))))

(defun oai-block-tags--read-file-to-string-safe (path-string &optional coding)
  ;; check
  (when (not (and (file-exists-p path-string)
                  (file-regular-p path-string)
                  (file-readable-p path-string)))
           (user-error "File does not exist or not readable: %s" path-string))
  ;; read
  (condition-case err
      (with-temp-buffer
        (when coding
          (set-buffer-file-coding-system coding))
        (insert-file-contents path-string)
        (buffer-string))
    (error (message "Error reading file %s: %s" path-string err)
           nil)))

;; (oai-block-tags--read-file-to-string-safe

(defun oai-block-tags--filepath-to-language (path-string)
  "For path-string return Org babel source block language name."
  (let* ((mode-symbol (assoc-default path-string auto-mode-alist 'string-match))
        (mode-string (apply #'mapconcat #'identity (butlast (string-split (symbol-name mode-symbol) "-")) '("-"))))
    (car (rassq (intern mode-string) org-src-lang-modes))))

(cl-assert
 (string-equal (oai-block-tags--filepath-to-language "/tmp/a.el") "elisp"))

(defun oai-block-tags--compose-block-for-path (path-string content)
  "Return file/directory content in mardown block without last ```."
  (concat
   "\nHere " (file-name-nondirectory (directory-file-name path-string)) (if (file-directory-p path-string) " folder" "") ":\n"
   ;; prefix
   (if (file-directory-p path-string)
       (plist-get oai-block-tags--markdown-prefixes :path-directory)
     ;; else - not derectory
     (let* ((mode-symbol (assoc-default path-string auto-mode-alist 'string-match))
            (mode (if mode-symbol
                      (progn
                        (car (rassq mode-symbol org-src-lang-modes)) ; string to symbol, get string
                        (apply #'mapconcat #'identity (butlast (string-split (symbol-name mode-symbol) "-")) '("-"))) ;; emacs-elisp from emacs-elisp
                    ;; else
                    "")))
     (concat (plist-get oai-block-tags--markdown-prefixes :path-file) mode)))
   "\n"
   content
   "\n```\n"))

(cl-assert
 (string-equal (oai-block-tags--compose-block-for-path "a.el" "ss")
"\nHere a.el:
```emacs-lisp
ss
```\n"))

(defun oai-block-tags--compose-block-for-path-full (path-string)
  "Return file or directory in prepared mardown block."
  (oai-block-tags--compose-block-for-path path-string
                                          (if (file-directory-p path-string)
                                              (oai-block-tags--get-directory-content path-string)
                                            ;; else
                                            (oai-block-tags--read-file-to-string-safe path-string)
                                            )))

;;; -=-= Org links find
(defun path-references-current-buffer-p (path)
  "Return non-nil if PATH references the file currently visited by this buffer.
Handles symlinks, remote files (TRAMP), and buffers without files."
  (when buffer-file-name
    (ignore-errors
      (let ((buffer-file (file-truename buffer-file-name))
            (input-file  (file-truename (expand-file-name path))))
        (string= buffer-file input-file)))))

(defun get-org-content-block (element)
  "Return markdown block for LLM for current element at current position.
Move pointer to the end of block."
  (let ((beg (or (org-element-property :contents-begin element)
                       (org-element-begin element)))
              (end (or (org-element-property :contents-end element)
                       (org-element-end element))))
          (when (and beg end)
            ;; - skip headers if begin at header
            (save-excursion (goto-char beg)
                            (when (looking-at "#\\+begin_")
                              (forward-line)
                              (setq beg (point)))
                            (goto-char end)
                            (when (or (looking-at "#\\+end_")
                                      (search-backward "#+end_" nil beg))
                              (forward-line -1)
                              (setq end (line-end-position))))

            ;; Compose result block
            (goto-char end) ; for return
            (concat
             ;; - Header ```
             (if (eq (org-element-type element) 'src-block)
                 (concat "```"  (org-element-property :language element) "\n")
               ;; else
               "```text\n")
             ;; - Body
             (string-trim (buffer-substring-no-properties beg
                                                          end))
             ;; - Footer ```
             "\n```\n")
            )))

(defun get-org-content ()
  "Return markdown block for LLM for current element at current position.
For Org buffer only.
Supported: blocks and headers.
Move pointer to the end of block."
  (let* ((element (org-element-context))
         (type (org-element-type element))
         (blocks-types '(comment-block center-block dynamic-block example-block
                                       export-block quote-block special-block
                                       src-block verse-block)))
      (cond
       ;; - blocks type
       ((member type  blocks-types)
        (get-org-content-block element))
       ;; - headline type
       ((eq type 'headline)
        (let ((org-element-end (org-element-at-point))
              (res "")
              el
              type
              )
          (while (< (point) (org-element-end element))
            ;; (print "vv")
            (setq el (org-element-context))
            (setq type (org-element-type el))
            (setq res (concat res
                              (cond ((eq type 'headline)
                                     ;; (print "hh")
                                     (prog1 (concat (make-string (org-element-property :level el) ?#) " " (org-element-property :raw-value el) "\n")
                                       (forward-line)))
                                    ((member type  blocks-types)
                                     ;; (print "bb")
                                     (prog1 (get-org-content-block el)
                                       ;; (condition-case nil
                                           (org-forward-element)
                                           ;; (org-next-item)
                                         ;; (error nil))
                                       ))
                                    (t
                                     ;; (print "t")
                                     (prog1
                                          (concat (buffer-substring-no-properties (line-beginning-position) (org-element-end el)) "\n")
                                       ;; (condition-case nil
                                           (org-forward-element)
                                           ;; (org-next-item)
                                         ;; (error nil))
                                       )))))
                  )
          res
          )))))

(defun get-replacement-for-org-link (link-string)
  "Return replacement for org-link  string.
Supported:
- file with name of block,
- directory
- local link."
  ;; `org-link-open' for type and opening,  `org-link-search' for search in current buffer.

  ;; from `org-link-open-from-string'
  ;; 1) convert string to Org element
  (let ((link (with-temp-buffer
                (let ((org-inhibit-startup nil))
                  (insert link-string)
                  (org-mode)
                  (goto-char (point-min))
                  (org-element-link-parser)))))
    (if (not link)
      (user-error "No valid link in %S" link-string))
    ;; from `org-link-open'
    ;; 2) extract path and type
    (let ((type (org-element-property :type link))
          (path (org-element-property :path link)))
      (print (list "type?" type path))
      (pcase type
        ("file" ; org-link-search
         (let* ((option (org-element-property :search-option link))) ;; nil if no ::, may be "" if after :: there is empty last part
           (print (list "option" option))
           (print (list "check" (path-references-current-buffer-p path)))

           (if (and option
                    (not (string-empty-p option)))
               ;; case 1) path to current file with option
               (if (path-references-current-buffer-p path)
                   (get-replacement-for-org-link (concat "[[" option "]]")) ; recursive call
                 ;; - else  case 2) path to other file with option
                 "") ;; TODO
             ;; else - no ::, only path
             (oai-block-tags--compose-block-for-path-full path))
           ))
        ;;         (path-extended (if option (concat path "::" option) path)))
        ;;    ;; check file
        ;;    (if (and (not (file-directory-p path-string))
        ;;             (not (and (file-exists-p path) (file-readable-p path))))
        ;;        (user-error "File not readable or not exist %S" link-string))
        ;;    ;; get
        ;;    (if option
        ;;        ;; subitem in file
        ;;        (progn

        ;;          )

        ;;        ;; - else - no "::" - just file or directory
        ;;        (if (file-directory-p path-string)
        ;;                           (oai-block-tags--get-directory-content path-string)
        ;;                         ;; else
        ;;                         (oai-block-tags--compose-block-for-path-full path-string))
        ;;        )))
        ;; ((or "coderef" "custom-id" "fuzzy" "radio")
        (or "radio" "fuzzy"
            (save-excursion
              (org-with-wide-buffer
               (if (equal type "radio")
	           (org-link--search-radio-target path)
	         (org-link-search
	          (pcase type
	            ("custom-id" (concat "#" path))
	            ("coderef" (format "(%s)" path))
	            (_ path))
	          ;; Prevent fuzzy links from matching themselves.
	          (and (equal type "fuzzy")
	               (+ 2 (org-element-begin link)))))
               (get-org-content)))
         ;; (save-excursion
         ;;   (org-with-wide-buffer
	 ;;    (if (equal type "radio")
	 ;;        (org-link--search-radio-target path)
	 ;;      (org-link-search
	 ;;       (pcase type
	 ;;         ("custom-id" (concat "#" path))
	 ;;         ("coderef" (format "(%s)" path))
	 ;;         (_ path))
	 ;;       ;; Prevent fuzzy links from matching themselves.
	 ;;       (and (equal type "fuzzy")
	 ;;            (+ 2 (org-element-begin link)))))
	 ;;    ;; (point)
         ;;    (get-org-block-content-in-current-buffer) ; return block
         ;;    ))
         )))))

;; (get-replacement-for-org-link  "[[xx]]")

;;; -=-= Replace
;; Supported:
;; - @Backtrace
;; - @/path/file.txt
;; - @./name - file
;; - @name - <<target>> or #+NAME: name - in current file

(defun oai-block-tags--replace-last-regex-smart (string regexp &optional replacement subexp)
  "Replace the last match of REGEXP in STRING with REPLACEMENT,
preserving any extra captured groups.
If REPLACEMENT not provided return found string for regexp."
  (let ((pos 0)
        (last-pos nil)
        (last-end nil)
        (last-group ""))
    (while (and pos
                (string-match regexp string pos))
      (setq pos (match-beginning 0))
      (setq last-pos pos) ; beg

      (setq last-end (match-end 0)) ; end
      ;; (print (list "last-group"   (match-string 0 string) (match-string 1 string) (match-string 2 string)))
      (setq pos last-end) ; move forward
      )
    (if last-pos
        (if replacement
            ;; (replace-match replacement 'fixedcase 'literal string)
            ;; (if (eq (aref string (1- last-end)) ?\s) ;; if space after match
                ;; (replace-match replacement 'fixedcase 'literal string 1)
              ;; else
              (concat (substring string 0 last-pos)
                      replacement
                      ;; last-group
                      (substring string last-end))
          ;; else - just return 0 group
          ;; remove leading and ending (` and space) characters
           (replace-regexp-in-string "^[` ]*" ""
                                     (replace-regexp-in-string "[` ]*\$" ""
                                                               (match-string 0 string))) ;; (substring string last-pos last-end)
          ) ; what was found
      string)))

(cl-assert
 (equal (oai-block-tags--replace-last-regex-smart "asdasd@Backtraceasdasdasd" "\\(@Backtrace\\)" "111")
        "asdasd111asdasdasd"))

(cl-assert
 (equal (oai-block-tags--replace-last-regex-smart "asdasd@Backtraceasdasdasd" "@Backtrace")
        "@Backtrace"))

;; search without replace
(cl-assert
 (and
  (equal (let ((regex (plist-get oai-block-tags--regexes :backtrace)))
           (oai-block-tags--replace-last-regex-smart
            "foo `@Backtrace` bar `@Backtrace `@BacktraceX"
            regex)) "@Backtrace")

  (equal (let ((regex (plist-get oai-block-tags--regexes :backtrace)))
           (oai-block-tags--replace-last-regex-smart
            "foo `@Backtrace` bar `@Backtrace `@Backtrace`X"
            regex)) "@Backtrace")))

(cl-assert
 (equal (let ((regex (plist-get oai-block-tags--regexes :backtrace)))
          (oai-block-tags--replace-last-regex-smart
           "foo `@Backtrace` bar `@Backtrace `@Backtrace`X"
           regex
           "REPLACED"))
        "foo `@Backtrace` bar `@Backtrace REPLACEDX"))
;; with space
(cl-assert
 (equal (let ((regex (plist-get oai-block-tags--regexes :backtrace)))
          (oai-block-tags--replace-last-regex-smart
           "foo `@Backtrace` bar `@Backtrace `@BacktraceX"
           regex
           "REPLACED"))
        "foo `@Backtrace` bar REPLACED`@BacktraceX"))

(cl-assert
 (equal (oai-block-tags--replace-last-regex-smart
         "foo `@/asd.txt` X"
         oai-block-tags--regexes-path
         "REPLACED")
        "foo REPLACED X"))

(cl-assert
 (equal (let ((regex (plist-get oai-block-tags--regexes :backtrace)))
          (oai-block-tags--replace-last-regex-smart "foo `@.` bar " (plist-get oai-block-tags--regexes :path) "REPLACED"))
        "foo REPLACED bar "))

(cl-assert
 (string-equal
  (oai-block-tags--replace-last-regex-smart "asd `@/tmp/t.txt` assd" (plist-get oai-block-tags--regexes :path) "path")
  "asd path assd"))
 ;; (oai-block-tags--replace-last-regex-smart "asd `[[/tmp][sd]]` assd" (plist-get oai-block-tags--regexes :path) "path")





(defun oai-block-tags-replace (string)
  "Replace links in STRING with their targets.
And return modified string or the same string."
  (let ((backtrace-re (plist-get oai-block-tags--regexes :backtrace))
        ;; (oai-block-tags--regexes-path (plist-get oai-block-tags--regexes :path))
        )
    ;;return
    (cond
     ;; - "@Backtrace" substring exist - replace the last one only
     ((string-match backtrace-re string)
           (if-let* ((bt (oai-block-tags--get-backtrace-buffer-string)) ; *Backtrace* buffer exist
                     (bt (oai-block-tags--take-n-lines bt oai-block-tags--backtrace-max-lines))
                     (bt (concat "\n" (plist-get oai-block-tags--markdown-prefixes :backtrace)
                                 bt
                                 markdown-postfix)) ; prepare string
                     (new-string (oai-block-tags--replace-last-regex-smart string backtrace-re bt))) ; insert backtrace
               new-string
             ;; else
             string
             ;; (if (and (equal (length string) (length new-string))
             ;;          (string-equal string new-string))
             ;;     (error "@Backtrace not found")
             ;;   ;; else
             ;;   new-string)
             ))
          ;; - Path @/path/file.txt - replace the last one only
          ((string-match oai-block-tags--regexes-path string)
           (if-let* ((path-string (oai-block-tags--replace-last-regex-smart string oai-block-tags--regexes-path))
                     ;; remove first @ character from link
                     (path-string (if (> (length path-string) 0)
                                      (substring path-string 1)
                                    ""))
                     (replacement (oai-block-tags--compose-block-for-path-full path-string))
                     (new-string (oai-block-tags--replace-last-regex-smart string
                                                                           oai-block-tags--regexes-path
                                                                           replacement)))
               new-string
             ;; else
             string))
          ;; - Org links [[link]]

          ;; We search  for link regex,  when found we check  if there
          ;; are double of  found substring after founded  one, if one
          ;; more exist we skip the first  one that found. if no other
          ;; exist we replace it.
          ((string-match org-link-any-re string)
           (let ((new-string string)
                 (replaced nil)
                 (pos-end 0)
                 (pos-beg 0)
                 (match)
                 (replacement))
             (while (string-match org-link-any-re new-string pos-end)
               (setq pos-beg (match-beginning 0))
               (setq pos-end (match-end 0))
               (setq match (match-string 0 new-string))
               (print (list "aa" (regexp-quote match) new-string))
               (when (not (string-match (regexp-quote match) new-string pos-end))
                 (setq replacement (get-replacement-for-org-link match))


                       ;; (substring new-string pos-beg pos-en)
                 (setq new-string (concat (substring new-string 0 pos-beg)
                                          replacement
                                          ;; last-group
                                          (substring new-string pos-end)))
                 (setq pos-end (+ pos-beg (length replacement)))
                 (print (list pos-end new-string))
                 (setq replaced t)
                 ;; (oai-block-tags--replace-last-regex-smart)
                 ))
             (if replaced
                 new-string)
             )
           )


           ;; (if-let* ((path-string (oai-block-tags--replace-last-regex-smart string org-link-any-re)))
           ;;     (progn
           ;;       (print (list "link" path-string))
           ;;       string))
          ;; - default
          (t string))))

;; (oai-block-tags-replace  "11[[sas]]222[[bbbaa]]3333[[sas]]4444")

;; (let* ((link (with-temp-buffer
;;                (let ((org-inhibit-startup nil))
;;                  (insert "[[file:~/docsmy_short/modified/emacsh::*graphiz - graphs][graphiz - graphs]]")
;;                  (org-mode)
;;                  (goto-char (point-min))
;;                  (org-element-link-parser))))
;;            (type (org-element-property :type link))
;;            (path (org-element-property :path link))
;;            (follow (org-link-get-parameter type :follow))
;;            (option (org-element-property :search-option link))) ;; after ::
;;       (print (list type path option follow)))

;; - just output test, too hard to compare with something.
(let* ((temp-dir (make-temp-file "my-tmp-dir-" t))     ;; Create temp directory
       (file1 (expand-file-name "file1.txt" temp-dir)) ;; Known file name
       (file2 (expand-file-name "file2.el" temp-dir))
       (file3 (expand-file-name "file3.py" temp-dir))
       )

      (with-temp-file file1
        (insert "Contents for file1"))
      (with-temp-file file2
        (insert "(defun aa() )"))
      (with-temp-file file3
        (insert "import os"))
      (print (oai-block-tags-replace (format "ssvv `@%s` bbb" temp-dir)))
      (if (not (string-match (regexp-quote "```text") (print (oai-block-tags-replace (format "ssvv `@%s` bbb" file1)))))
          (error "ss"))
      ;; (print (oai-block-tags-replace (format "ssvv `@%s` bbb" file2))))
      (if (not (string-match (regexp-quote "```emacs-lisp") (print (oai-block-tags-replace (format "ssvv `@%s` bbb" file2)))))
          (error "ss2"))
      (oai-block-tags-replace (format "ssvv [[%s]] bbb" file3))
      )

  ;; Return list of paths for later use
  ;; (list temp-dir file1 file2))

;; get folder content
(let ((path "/tmp/tttt1"))
                (if (not (file-exists-p path))
                    (make-directory path)
                  )
                ;; (temporary-file-directory
                (oai-block-tags-replace (format "ssvv `@%s` bbb" path)))
;; "ssvv
;; Here tttt1 folder:
;; ```ls-output
;;   /tmp/tttt1:

;; ```
;;  bbb")


;;; -=-= Fontify Backtrace & links

(defun oai-block-tags--font-lock-fontify-links (limit)
  "Fontify Org links in #+begin_ai ... #+end_ai blocks, up to LIMIT.
This is special fontify function, that return t when match found.
1) search for ai block begin and then end, 2) call fontify on range that goto to the begining firstly
`org-activate-links'."
  (if oai-block-fontify-markdown
      (let ((case-fold-search t)
            (ret))
        (while (and (re-search-forward "^#\\+begin_ai[^\n]*\n" limit t)
                    (< (point) limit))
          (let ((beg (match-end 0)))
            (when (re-search-forward "^#\\+end_ai.*$" nil t)
              (let ((end (match-beginning 0)))
                (save-match-data
                  ;; fontify Org links [[..]]
                  ;; (message beg)
                  ;; - [[link][]]
                  (progn
                    (goto-char beg)
                    (while (re-search-forward org-link-any-re end t)
                      (goto-char (match-beginning 0))
                      (setq ret (org-activate-links end))
                      ))
                  ;; - @Backtrace
                  (progn
                    (goto-char beg)
                    (while (re-search-forward (plist-get oai-block-tags--regexes :backtrace) limit t)
                      (add-face-text-property (match-beginning 0) (match-end 0) 'org-link)
                      (setq ret t)))
                  ;; - @/tmp/
                  (progn
                    (goto-char beg)
                    (while (re-search-forward (plist-get oai-block-tags--regexes :path) limit t)
                      (add-face-text-property (match-beginning 0) (match-end 0) 'org-link)
                      (setq ret t)))
                  ;; fontify markdown sub-blocks
                  ;; (oai-block--fontify-markdown-subblocks beg end)
                  )
                ))))
        ;; required by font lock mode:
        (goto-char limit)
        ret)))


(provide 'oai-block-tags)
;;; oai-block-tags.el ends here
