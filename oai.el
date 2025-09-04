;;; oai.el --- AI blocks for org-mode. -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Robert Krahn
;; Copyright (C) 2025 github.com/Anoncheg1
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Author: github.com/Anoncheg1,codeberg.org/Anoncheg
;; Keywords: org, ai, llm, url, http
;; URL: https://github.com/Anoncheg1/oai
;; Version: 0.1,  Fork from orig. version: 0.5.6 (commit cc4a4eb778e4689573ebd2d472b8164f4477e8b8)
;; Created: 20 Aug 2025
;; Package-Requires: ((emacs "27.1") (compat "30.1"))

;;; License

;; This file is NOT part of GNU Emacs.

;; oai.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; oai.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with oai.el.  If not, see <https://www.gnu.org/licenses/>.

;; Licensed under the GNU Affero General Public License, version 3 (AGPLv3)
;; <https://www.gnu.org/licenses/agpl-3.0.en.html>

;;; Commentary:
;;
;; OAI extend Org mode with "ai block" that allows you to interact
;; with the OpenAI-compatible REST APIs. Fork of "org-ai".
;;
;; It allows you to:
;; - Use #+begin_ai..#+end_ai blocks for org-mode
;; - Chat with a language model from within an org mode buffer.
;; - Call requests from multiple block and buffers.
;;
;; For the Internet connection used built-in libs: url.el and url-http.el.
;;
;; See see https://github.com/Anoncheg1/oai-core for the full set
;; of features and setup instructions.
;;
;; Configuration:
;; (add-to-list 'load-path "path/to/oai")
;; (require 'oai)
;; (add-hook 'org-mode-hook #'oai-mode) ; oai.el
;; (setq oai-restapi-con-token "xxx") ; oai-restapi.el
;;
;; You will need an OpenAI API key.  It can be stored in the format
;;   machine api.openai.com login oai password <your-api-key>
;; in your ~/.authinfo.gpg file (or other auth-source) and will be picked up
;; when the package is loaded.
;;
;; Available commands (TODO to refine):
;;
;; - Inside org-mode / #+begin_ai..#+end_ai blocks:
;;     - C-c C-c to send the text to the OpenAI API and insert a response (org-ai.el)
;;     - Press C-c <backspace> (oai-kill-region-at-point) to remove the chat part under point.  (oai-block.el)
;;     - oai-mark-region-at-point will mark the region at point.  (oai-block.el)
;;     - oai-mark-last-region will mark the last chat part.  (oai-block.el)

;; Architecture:
;;   (raw info) Interface -> (structured Org info + raw Org body) Agent -> API to LLM + Callback (may be part of Agent or API)

;; Callback write result to ORG

;; Execution chain:
;;
;;

;;; Changes:

;; - DONE: remove websocket
;; - DONE: additional should be additional
;; - DONE: :stream nil/t parameter
;; - DONE: guide for connecting to custom LLM provider.
;; - DONE: Guide to add custom functions for text post-processing.
;; - DONE: make oai-block dependent on org and oai-restapi dependent on url only. separate 'oai-block and 'oai-restapi, for now oai-restapi dependes on oai-block and org.
;; - TODO: rename all to "file-shit" naming convention
;; - TODO: make org-ai-variable.el and pass them to -api.el functions as parameters.
;; - TODO: provide ability to replace url-http with plz or oai-restapi with llm(plz)
;; - TODO: implement "#+PROPERTY: var  foo=1" and  "#+begin_ai :var foo=1" and to past to text in [foo]
;; - TODO: implement expanders for variables like links and references
;; - TODO: implement contant-tags "Fix @problems then document the changes in @/CHANGELOG.md" @url, @file, @folder, @header? (Org)

;;; Code
;;; -=-= Includes
(require 'oai-block-tags) ; `oai-block-tags-replace' for `oai-expand-block'
(require 'oai-block)
(require 'oai-restapi)
;; (require 'org-ai-openai-image)
;; (require 'org-ai-useful)
;; (require 'org-ai-on-project)
;; (require 'org-ai-talk)
;; (require 'org-ai-sd)
;; (require 'org-ai-oobabooga)

;;; -=-= C-c C-c main interface

(defcustom oai-agent-call #'oai-restapi-request-prepare ; oai-restapi.el
  "Pass processed ai block info to AI assistent or some Emacs agent.
See `oai-call-block' and `oai-restapi-request-prepare' for parameters.
TODO: pass callback for writing."
  :type 'function
  :group 'oai)

(defun oai-ctrl-c-ctrl-c ()
  "Main command for #+begin_ai."
  (when-let ((element (oai-block-p))) ; oai-block.el
    (oai-call-block) ; here
    t))


(defun oai-call-block ()
  "Read Org parameters and send the text content to next step."
  (interactive)
  ;; -- remove result block
  (oai-block-remove-result)

  ;; -- Org "Pre-parsing"
  (let* ((element (oai-block-p)) ; oai-block.el
         (info (oai-block-get-info element)) ; ((:max-tokens . 150) (:service . "together") (:model . "xxx")) ; oai-block.el
         ;; (end-marker (oai-block--get-content-end-marker element))
         (req-type (oai-block--get-request-type info)) ; oai-block.el
         (sys-prompt-for-all-messages (or (not (eql 'x (alist-get :sys-everywhere info 'x)))
                                          (org-entry-get-with-inheritance "SYS-EVERYWHERE") ; org
                                          oai-restapi-default-inject-sys-prompt-for-all-messages)) ; oai-restapi.el
         (sys-prompt (or (org-entry-get-with-inheritance "SYS") ; org
                         (oai-block--get-sys :info info ; oai-block.el
                                          :default oai-restapi-default-chat-system-prompt)))) ; oai-restapi.el variable
    ;; - Process Org params and call agent
    (oai-block--let-params info
                              ;; format: (variable optional-default type)
                              ((service oai-restapi-con-service string) ; oai-restapi.el
                               (model (if (oai-restapi--get-value-or-string oai-restapi-con-model service)
                                        ;; (oai-restapi--get-value-or-string org-ai-creds-completion-model service)
                                      :type string)) ; oai-restapi.el
                               (max-tokens oai-restapi-default-max-tokens :type number)
                               (top-p nil :type number)
                               (temperature nil :type number)
                               (frequency-penalty nil :type number)
                               (presence-penalty nil :type number)
                               (stream "t" :type string)
                               )
                              (print (list "service" service (type-of service)))
                              ;; - body with some Org "Post-parsing":
                              ;; (print (list "SERVICE" service (stringp service) (org-ai--read-service-name service)))
                              (let (
                                    (service (or service
                                                 oai-restapi-con-service)) ; default in oai-restapi.el
                                    (stream (if (and stream (string-equal-ignore-case stream "nil"))
                                                nil
                                              ;; else
                                              (oai-restapi--stream-supported service model))))
                                ;; - main call
                                (funcall oai-agent-call req-type element sys-prompt sys-prompt-for-all-messages ; message
                                         model max-tokens top-p temperature frequency-penalty presence-penalty service stream ; model params
                                         )))))

;;; -=-= key M-x: oai-expand-block
;;;###autoload
(defun oai-expand-block (&optional element)
  "Show a temp buffer with what the ai block expands to.
This is what will be sent to the api.  ELEMENT is the ai block.
Like `org-babel-expand-src-block'."
  (interactive)
  (let* ((element (or element (oai-block-p))) ; oai-block.el
         (expanded (string-trim
                    (oai-block-get-content element) ; oai-block.el
                    ))
         (expanded (oai-restapi--collect-chat-messages expanded))
         (expanded (oai-restapi--modify-last-user-content expanded #'oai-block-tags-replace))
         (expanded (oai-restapi--stringify-chat-messages expanded)))
    (if (called-interactively-p 'any)
        (let ((buf (get-buffer-create "*OAi Preview*")))
          (with-help-window buf (with-current-buffer buf
                                  (insert expanded))))
      expanded)))

;;; -=-= key C-g: keyboard quit

;; (defvar org-ai-talk--reading-process)
(defun oai-keyboard-quit ()
  "Keyboard quit advice.
It's designed to \"do the right thing\":
- If there is an active region, do nothing (normal \\<mapvar> & \\[keyboard-quit] will deactivate it).
- If there is speech recorded or played, stop it.
- If there is currently a running openai request, stop it."
  (interactive)
  ;; - if ai mode active in current buffer
  (if (and (bound-and-true-p oai-mode)
           (not (minibufferp (window-buffer (selected-window))))) ; not in minubuffer
      ;; - stop current request
      (if (bound-and-true-p oai-debug-buffer)
          ;; - show all errors in debug mode
          (cond
           ((region-active-p) nil)
           (t (call-interactively #'oai-restapi-stop-url-request))) ; oai-restapi.el
        ;; else - suppress error in normal mode
        (condition-case _
            (cond
             ((region-active-p) nil)
             ;; ((and (boundp 'org-ai-talk--reading-process) ; org-ai-talk.el
             ;;       (fboundp 'org-ai-talk-stop) ; org-ai-talk.el
             ;;       org-ai-talk--reading-process ; org-ai-talk.el
             ;;       (process-live-p org-ai-talk--reading-process)) ; org-ai-talk
             ;;  (org-ai-talk-stop)) ; org-ai-talk
             ;; (org-ai-oobabooga--current-request ; org-ai-oobabooga
             ;;  (org-ai-oobabooga-stop)) ; org-ai-oobabooga
             ;; (org-ai--current-request-buffer-for-stream ; oai-restapi.el
             ;;  (org-ai-interrupt-current-request)) ; oai-restapi.el

             (t (oai-restapi-stop-url-request)) ; oai-restapi.el
             ;; (org-ai--current-request-buffer-for-image ; org-ai-openai-image.el
             ;;  (org-ai-image-interrupt-current-request)) ; org-ai-openai-image.el
             )
          (error nil)))))

;; (defun org-ai--install-keyboard-quit-advice () ; TODO: make Org only
;;   "Cancel current request when `keyboard-quit' is called."
;;   (advice-add 'keyboard-quit :before #'oai-keyboard-quit))

;; (defun org-ai--uninstall-keyboard-quit-advice ()
;;   "Remove the advice that cancels current request when `keyboard-quit' is called."
;;   (advice-remove 'keyboard-quit #'oai-keyboard-quit)) ; here

;;; -=-= M-x oai-toggle-debug
;;;###autoload
(defun oai-toggle-debug (&optional element)
  "Enable/disable debug."
  (interactive)
  (if oai-debug-buffer
      (progn
        (setq oai-debug-buffer nil)
        (message "Disable oai debugging"))

    ;; else
    (setq oai-debug-buffer   "*debug-oai*")
    (message "Enable oai debugging")))

;;; -=-= Minor mode

(defvar oai-mode-map (make-sparse-keymap)
  "Keymap for `oai-mode'.")

(let ((map oai-mode-map))
  ;; (define-key map (kbd "C-c M-a v") 'org-ai-image-variation) ; org-ai-openai-image.el
  ;; (define-key map (kbd "C-c M-a $") 'org-ai-open-account-usage-page) ; org-ai-openai-image.el
  (define-key map (kbd "C-c h") #'oai-mark-region-at-point) ; oai-block.el
  ;; (define-key map (kbd "C-c DEL") 'org-ai-kill-region-at-point) ; oai-block.el
  (define-key map (kbd "C-c <backspace>") #'oai-kill-region-at-point) ; oai-block.el
  ;; (define-key map (kbd (string-join (list "C-c" " r"))) 'org-ai-talk-capture-in-org) ; org-ai-talk.el
  (define-key map (kbd "C-c ?") #'oai-open-request-buffer) ; oai-restapi.el
  (define-key map (kbd "M-h") #'oai-block-mark-md-block-body) ; oai-block.el
  )

(define-minor-mode oai-mode
  "Minor mode for `org-mode' integration with the OpenAI API."
  :init-value nil
  :lighter oai-mode-line-string ; " org-ai"
  :keymap oai-mode-map
  :group 'oai
  (if oai-mode
      (progn
        (add-hook 'org-ctrl-c-ctrl-c-hook #'oai-ctrl-c-ctrl-c nil 'local)
        (advice-add 'keyboard-quit :before #'oai-keyboard-quit)
        (add-hook 'org-font-lock-set-keywords-hook #'oai-block--set-ai-keywords)
        )
    ;; else - off
    (remove-hook 'org-ctrl-c-ctrl-c-hook #'oai-ctrl-c-ctrl-c 'local)
    (advice-remove 'keyboard-quit #'oai-keyboard-quit)
    (remove-hook 'org-font-lock-set-keywords-hook #'oai-block--set-ai-keywords)
   ))

;;;###autoload
(defun oai-open-request-buffer ()
  "Opens the url request buffer for ai block at current position.
Call original \"C-c ?\" key if not at ai block."
  (interactive)
  (if-let ((element (oai-block-p)))
      (if-let* ((url-buffer (car (oai-restapi-get-buffers-for-element element)))
                (display-buffer-base-action
                 (list '(
                         ;; display-buffer--maybe-same-window  ;FIXME: why isn't this redundant?
                         display-buffer-reuse-window ; pop up bottom window
                         display-buffer-in-previous-window ;; IF RIGHT WINDOW EXIST
                         display-buffer-in-side-window ;; right side window - MAINLY USED
                         display-buffer--maybe-pop-up-frame-or-window ;; create window
                         ;; ;; If all else fails, pop up a new frame.
                         display-buffer-pop-up-frame )
                       '(window-width . 0.6) ; 80 percent
                       '(side . right))))
          (progn
            (pop-to-buffer url-buffer)
            (with-current-buffer url-buffer
              (local-set-key (kbd "C-c ?") 'delete-window)))
        ;; else
        (message "No url buffer found."))
  ;; - else - no element - call original Org key
  (call-interactively (lookup-key org-mode-map (kbd "C-c ?")))))

;;; -=-= Minor mode - string line
(defvar oai-mode-line-string "")

(defun oai-update-mode-line (count)
  "Used in ora-timers.el to show count of active requests."
  (if (and count (> count 0))
      (setq oai-mode-line-string (format " org-ai[%d]" count))
    ;; else
    (setq oai-mode-line-string " org-ai"))
  (force-mode-line-update)
  ;; (propertize (format " org-ai[%d]" count)
  ;;                   'face (if (> count 0) 'error 'default))
  )

;;; - Global mode (old)
;; (defvar org-ai-global-prefix-map (make-sparse-keymap)
;;   "Keymap for `org-ai-global-mode'.")

;; (let ((map org-ai-global-prefix-map)) ; here
;;   ;; (define-key map (kbd "p") 'org-ai-on-project) ; org-ai-on-project.el
;;   ;; (define-key map (kbd "P") 'org-ai-prompt-in-new-buffer) ; org-ai-useful.el
;;   ;; (define-key map (kbd "r") 'org-ai-on-region) ; org-ai-useful.el
;;   ;; (define-key map (kbd "c") 'org-ai-refactor-code) ; org-ai-useful.el
;;   ;; (define-key map (kbd "s") 'org-ai-summarize) ; org-ai-useful.el
;;   (define-key map (kbd "m") 'oai-restapi-switch-chat-model) ; oai-restapi.el
;;   (define-key map (kbd "!") 'oai-open-request-buffer) ; oai-restapi.el
;;   ;; (define-key map (kbd "$") 'org-ai-open-account-usage-page) ; org-ai-openai-image.el
;;   ;; (define-key map (kbd "t") 'org-ai-talk-input-toggle) ; org-ai-talk.el
;;   ;; (define-key map (kbd "T") 'org-ai-talk-output-toggle) ; org-ai-talk.el
;;   ;; (define-key map (kbd "R") 'org-ai-talk-read-region) ; org-ai-talk.el
;;   (define-key map (kbd "SPC") 'org-ai-mark-region-at-point)) ; oai-block.el

;; (defvar org-ai-global-mode-map (make-sparse-keymap)
;;   "Keymap for `org-ai-global-mode'.")

;; (define-key org-ai-global-mode-map (kbd "C-c M-a") org-ai-global-prefix-map) ; here

;; ;;;###autoload
;; (define-minor-mode org-ai-global-mode
;;   "Non `org-mode' specific minor mode for the OpenAI API."
;;   :init-value nil
;;   :lighter ""
;;   :global t
;;   :keymap org-ai-global-mode-map
;;   :group 'oai)



;;; provide
(provide 'oai)
;;; oai.el ends here
