;;; org-ai.el --- Refactored Use ChatGPT and other LLMs in org-mode and beyond -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Robert Krahn and contributers
;; Copyright (C) 2025 github.com/Anoncheg1

;; URL: https://github.com/Anoncheg1/org-aire
;; Version: 0.1,  Orig. Version: 0.5.6
;;
;; Package-Requires: ((emacs "27.1") (websocket "1.15"))

;;; Changes:
;; - DONE: remove websocket
;; - DONE: additional should be additional
;; - DONE: :stream nil/t parameter
;; - DONE: guide for connecting to custom LLM provider.
;; - DONE: Guide to add custom functions for text post-processing.
;; - DONE: make org-ai-block dependent on org and org-ai-openai dependent on url only. separate 'org-ai-block and 'org-ai-openai, for now org-ai-openai dependes on org-ai-block and org.
;; - TODO: rename all to "file-shit" naming convention
;; - TODO: make org-ai-variable.el and pass them to -api.el functions as parameters.
;; - TODO: provide ability to replace url-http with plz or org-ai-openai with llm(plz)
;; - TODO: implement "#+PROPERTY: var  foo=1" and  "#+begin_ai :var foo=1" and to past to text in [foo]
;; - TODO: implement expanders for variables like links and references

;;; License

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

;;; Commentary:

;; Refactored version of "org-ai".
;; Provides a minor-mode for org-mode and a global minor-mode that allows you to
;; interact with the OpenAI API (and, with Stable Diffusion, as well as various local LLMs.)
;;
;; It allows you to:
;; - #+begin_ai..#+end_ai blocks for org-mode
;; - "chat" with a language model from within an org mode buffer
;;
;; For the Internet connection used built-in libs: url.el and url-http.el.
;;
;; See see https://github.com/Anoncheg1/org-ai-core for the full set
;; of features and setup instructions.
;;
;; Configuration:
;; (add-to-list 'load-path "path/to/org-ai")
;; (require 'org-ai)
;; (add-hook 'org-mode-hook #'org-ai-mode) ; org-ai.el
;; (setq org-ai-openai-api-token "xxx") ; org-ai-openai.el
;;
;; You will need an OpenAI API key.  It can be stored in the format
;;   machine api.openai.com login org-ai password <your-api-key>
;; in your ~/.authinfo.gpg file (or other auth-source) and will be picked up
;; when the package is loaded.
;;
;; Available commands (TODO to refine):
;;
;; - Inside org-mode / #+begin_ai..#+end_ai blocks:
;;     - C-c C-c to send the text to the OpenAI API and insert a response (org-ai.el)
;;     - Press C-c <backspace> (org-ai-kill-region-at-point) to remove the chat part under point.  (org-ai-block.el)
;;     - org-ai-mark-region-at-point will mark the region at point.  (org-ai-block.el)
;;     - org-ai-mark-last-region will mark the last chat part.  (org-ai-block.el)

;; Architecture:
;;   (raw info) Interface -> (structured Org info + raw Org body) Agent -> API to LLM + Callback (may be part of Agent or API)

;; Callback write result to ORG

;; Agent - two parts:
;; 1) receive structured info and pass to "API to LLM"
;; 2) receive callback and pass back to "API to LLM" to display to user

;; Why callback is not separate? Because of "stream" mode - just for speed


;; Interface: (Why chain of calls? To not mess with return structures.)
;;  - org-ai-interface-step1 (parse Org and messages)
;;  - org-ai-interface-step2 (parse Org, choose callback)

;;; Code:

(require 'org-ai-block)
(require 'org-ai-openai)
;; (require 'org-ai-openai-image)
;; (require 'org-ai-useful)
;; (require 'org-ai-on-project)
;; (require 'org-ai-talk)
;; (require 'org-ai-sd)
;; (require 'org-ai-oobabooga)

;;; - Agent function


;; (defun org-ai-agent-mycall (service model prompt)

(defcustom org-ai-agent-call #'org-ai-api-request-prepare ; org-ai-openai.el
  "Pass LLM adjusted information for further agent or LLM's API call.
TODO: pass callback for writing "
  :type 'function
  :group 'org-ai)


;;; - C-c C-c
(defun org-ai-ctrl-c-ctrl-c ()
  "Main command for #+begin_ai."
  (when-let ((element (org-ai-block-p))) ; org-ai-block.el
    (print "here")
    (org-ai-interface-step1) ; here
    t))


(defun org-ai-interface-step1 ()
  "First intefrace step 2) for #+begin_ai.
Read Org parameters and send the text content to next step."
  (interactive)
  ;; -- 1) Org "Pre-parsing"
  (let* ((element (org-ai-block-p)) ; org-ai-block.el
         (info (org-ai-block-get-info element)) ; ((:max-tokens . 150) (:service . "together") (:model . "xxx")) ; org-ai-block.el
         ;; (end-marker (org-ai-block--get-content-end-marker element))
         (req-type (org-ai-block--get-request-type info)) ; org-ai-block.el
         (sys-prompt-for-all-messages (or (not (eql 'x (alist-get :sys-everywhere info 'x)))
                                          (org-entry-get-with-inheritance "SYS-EVERYWHERE") ; org
                                          org-ai-default-inject-sys-prompt-for-all-messages)) ; org-ai-openai.el
         (sys-prompt (or (org-entry-get-with-inheritance "SYS") ; org
                         (org-ai-block--get-sys :info info ; org-ai-block.el
                                          :default org-ai-default-chat-system-prompt)))) ; org-ai-openai.el variable
    (org-ai--debug info)
    ;; - Process Org params and call agent
    (org-ai-block--let-params info
                              ;; format: (variable optional-default type)
                              ((service :type identity)
                               (model (if messages org-ai-default-chat-model org-ai-default-completion-model) :type string) ; org-ai-openai.el
                               (max-tokens org-ai-default-max-tokens :type number)
                               (top-p nil :type number)
                               (temperature nil :type number)
                               (frequency-penalty nil :type number)
                               (presence-penalty nil :type number)
                               (stream "t" :type string))
                              ;; - body with some Org "Post-parsing":
                              (let ((content (org-ai-block-get-content element))
                                    (service (or (if (stringp service) (org-ai--read-service-name service) service) ; org-ai-openai.el
                                                 ;; (org-ai--service-of-model model)
                                                 org-ai-service)) ; org-ai-openai.el
                                    (stream (if (and stream (string-equal-ignore-case stream "nil"))
                                                nil
                                              ;; else
                                              (org-ai--stream-supported service model))))
                                ;; - main call
                                (funcall org-ai-agent-call req-type content element sys-prompt sys-prompt-for-all-messages ; message
                                         model max-tokens top-p temperature frequency-penalty presence-penalty service stream ; model params
                                         )))))

;;; - M-x org-ai-expand-block
;;;###autoload
(defun org-ai-expand-block (&optional element)
  "Show a temp buffer with what the org-ai block expands to.
This is what will be sent to the api.  ELEMENT is the org-ai block.
Like `org-babel-expand-src-block'."
  (interactive)
  (let* ((element (or element (org-ai-block-p))) ; org-ai-block.el
         (expanded (org-ai-block-get-content element))) ; org-ai-block.el
    (if (called-interactively-p 'any)
        (let ((buf (get-buffer-create "*Org-Ai Preview*")))
          (with-help-window buf (with-current-buffer buf
                                  (insert expanded))))
      expanded)))

;;; - keyboard quit C-g

;; (defvar org-ai-talk--reading-process)

(defun org-ai-keyboard-quit ()
  "Keyboard quit advice.
It's designed to \"do the right thing\":
- If there is an active region, do nothing (normal \\<mapvar> & \\[keyboard-quit] will deactivate it).
- If there is speech recorded or played, stop it.
- If there is currently a running openai request, stop it."
  (interactive)
  (if org-ai-debug-buffer
      ;; - all errors
    (cond
       ((region-active-p) nil)
       (t (org-ai-openai-stop-url-request))) ; org-ai-openai.el
    ;; - suppress error
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
       ;; (org-ai--current-request-buffer-for-stream ; org-ai-openai.el
       ;;  (org-ai-interrupt-current-request)) ; org-ai-openai.el
       (t (org-ai-openai-stop-url-request)) ; org-ai-openai.el
       ;; (org-ai--current-request-buffer-for-image ; org-ai-openai-image.el
       ;;  (org-ai-image-interrupt-current-request)) ; org-ai-openai-image.el
       )
    (error nil))))

(defun org-ai--install-keyboard-quit-advice () ; TODO: make Org only
  "Cancel current request when `keyboard-quit' is called."
  (unless (advice-member-p #'org-ai-keyboard-quit 'keyboard-quit) ; here
    (advice-add 'keyboard-quit :before #'org-ai-keyboard-quit)))

(defun org-ai--uninstall-keyboard-quit-advice ()
  "Remove the advice that cancels current request when `keyboard-quit' is called."
  (advice-remove 'keyboard-quit #'org-ai-keyboard-quit)) ; here

(org-ai--install-keyboard-quit-advice) ; here

;;; - Minor mode

(defvar org-ai-mode-map (make-sparse-keymap)
  "Keymap for `org-ai-mode'.")

(let ((map org-ai-mode-map))
  ;; (define-key map (kbd "C-c M-a v") 'org-ai-image-variation) ; org-ai-openai-image.el
  ;; (define-key map (kbd "C-c M-a $") 'org-ai-open-account-usage-page) ; org-ai-openai-image.el
  (define-key map (kbd "C-c M-a SPC") 'org-ai-mark-region-at-point) ; org-ai-block.el
  ;; (define-key map (kbd "C-c DEL") 'org-ai-kill-region-at-point) ; org-ai-block.el
  (define-key map (kbd "C-c <backspace>") 'org-ai-kill-region-at-point) ; org-ai-block.el
  ;; (define-key map (kbd (string-join (list "C-c" " r"))) 'org-ai-talk-capture-in-org) ; org-ai-talk.el
  (define-key map (kbd "C-c ?") 'org-ai-open-request-buffer) ; org-ai-openai.el
  )

(define-minor-mode org-ai-mode
  "Minor mode for `org-mode' integration with the OpenAI API."
  :init-value nil
  :lighter org-ai-mode-line-string ; " org-ai"
  :keymap org-ai-mode-map
  :group 'org-ai
  (add-hook 'org-ctrl-c-ctrl-c-hook #'org-ai-ctrl-c-ctrl-c nil t))

(defun org-ai-open-request-buffer ()
  "Opens the url request buffer for ai block at current position.
Call original \"C-c ?\" key if not at ai block."
  (interactive)
  ;; TODO GET BUFEER from list for current block

  (let* ((element (org-ai-block-p))
         (url-buffer (if element
                         (org-ai-timers--get-variable (org-ai-block-get-header-marker element))
                       ;; else
                       (car (org-ai-timers--get-all-variables)))))
    (if (or (not element) (not url-buffer))
        ;; call original Org key
        (call-interactively (lookup-key org-mode-map (kbd "C-c ?")))
      ;; else
      (if url-buffer
          (let
              ((display-buffer-base-action
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
            (pop-to-buffer url-buffer)
            (with-current-buffer url-buffer
              (local-set-key (kbd "C-c ?") 'delete-window)))
        ;; else
        (message "No url buffer found.")))))

;;; - Global mode
;; (defvar org-ai-global-prefix-map (make-sparse-keymap)
;;   "Keymap for `org-ai-global-mode'.")

;; (let ((map org-ai-global-prefix-map)) ; here
;;   ;; (define-key map (kbd "p") 'org-ai-on-project) ; org-ai-on-project.el
;;   ;; (define-key map (kbd "P") 'org-ai-prompt-in-new-buffer) ; org-ai-useful.el
;;   ;; (define-key map (kbd "r") 'org-ai-on-region) ; org-ai-useful.el
;;   ;; (define-key map (kbd "c") 'org-ai-refactor-code) ; org-ai-useful.el
;;   ;; (define-key map (kbd "s") 'org-ai-summarize) ; org-ai-useful.el
;;   (define-key map (kbd "m") 'org-ai-switch-chat-model) ; org-ai-openai.el
;;   (define-key map (kbd "!") 'org-ai-open-request-buffer) ; org-ai-openai.el
;;   ;; (define-key map (kbd "$") 'org-ai-open-account-usage-page) ; org-ai-openai-image.el
;;   ;; (define-key map (kbd "t") 'org-ai-talk-input-toggle) ; org-ai-talk.el
;;   ;; (define-key map (kbd "T") 'org-ai-talk-output-toggle) ; org-ai-talk.el
;;   ;; (define-key map (kbd "R") 'org-ai-talk-read-region) ; org-ai-talk.el
;;   (define-key map (kbd "SPC") 'org-ai-mark-region-at-point)) ; org-ai-block.el

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
;;   :group 'org-ai)



;;; - Minor mode - string line
(defvar org-ai-mode-line-string "")

(defun org-ai-update-mode-line (count)
  (if (and count (> count 0))
      (setq org-ai-mode-line-string (format " org-ai[%d]" count))
    ;; else
    (setq org-ai-mode-line-string " org-ai"))
  (force-mode-line-update)
  ;; (propertize (format " org-ai[%d]" count)
  ;;                   'face (if (> count 0) 'error 'default))
  )

;;; - provide
(provide 'org-ai)
;;; org-ai.el ends here
