;;; org-ai-openai.el --- OpenAI API related functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Robert Krahn and contributers
;; Copyright (C) 2025 github.com/Anoncheg1

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

;;; Changelog:
;; - DONE: comment redundent "org-ai-use-auth-source" variable
;; - DONE: add debug-switch "org-ai-debug-buffer"
;; - DONE: add :stream nil
;; - DOME: allow to disable "system" role completely
;; - DONE: BUG: :completion req-type don't output anything
;; - DONE: `org-ai-after-chat-insertion-hook' not called for :stream nil and :completion
;; - DONE: hook errors handling at user side.
;; - DONE: stop timer when "When no connection"
;; - DONE: shut network process when timer expire - kill buffer, remove callback.
;; - DONE: coding is broken for received text for not English languages
;; - DONE: make URLs as variables without hardcoding in functions and bunch of variables
;; - DONE: write guide to add new LLM provider
;; - DONE: remove org-ai-block, org, org-element
;; - TODO: rename all functions to convention
;; - TODO: rename file to -api.el
;; - TODO: escame #+end_ai after insert of text in block


;;; Commentary:

;; Get info block from #begin_ai and call url-retrieve.  Asynchronous
;; but only one call per buffer.
;;
;; Interface function: "org-ai-stream-completion".
;;
;; (org-ai-stream-completion service|model messages|prompt context)
;; -> (org-ai-stream-request service messages|prompt callback)
;; -> org-ai--get-headers, org-ai--get-endpoint, org-ai--payload, url-retrieve
;; - org-ai--get-headers = org-ai-api-creds-token = "api-key" or "x-api-key" or "Authorization"
;; - org-ai--get-endpoint = hardcoded URL or org-ai-openai-chat-endpoint, org-ai-openai-completion-endpoint, org-ai-google-chat-endpoint
;; -> callback: (org-ai--insert-stream-response) or (org-ai--insert-single-response)
;;
;; Main variables:
;; URL = org-ai--get-endpoint()  or org-ai-openai-chat-endpoint, org-ai-openai-completion-endpoint, org-ai-google-chat-endpoint
;; Headers = org-ai--get-headers
;; Token = org-ai-api-creds-token
;;
;; When we create request we count requests, create two timers global one and local inside url buffer.
;; When we receive error or final answer we stop local, recount requests and update global.
;;
;; Chat mode
;; - :message (org-ai-openai--collect-chat-messages ...)
;; - (org-ai--normalize-response response) -> (cl-loop for response in normalized
;;   - (setq role (org-ai--response-type response))
;;   - (setq text (decode-coding-string (org-ai--response-payload response)) 'utf-8)
;; Completion mode
;; - :prompt content-string
;; - (setq text  (decode-coding-string (org-ai--get-single-response-text result) 'utf-8))
;;
;; How requests forced to stop with C-g?

;; We save url-buffer with header marker with
;; `org-ai-timers--progress-reporter-run' function.  that call:
;; (org-ai-timers--set-variable url-buffer header-marker) in
;; `org-ai-timers--interrupt-current-request' we remove buffer from
;; saved and call (org-ai-openai--interrupt-url-request url-buffer).

;;; Code:

(require 'org)
(require 'org-element)
(require 'url)
(require 'url-http)
(require 'cl-lib)
(require 'gv)
(require 'json)
(require 'org-ai-block)
(require 'org-ai-timers)

;;; - Constants, variables
(defcustom org-ai-jump-to-end-of-block t
  "If non-nil, jump to the end of the block after inserting the completion."
  :type 'boolean
  :group 'org-ai)

(defcustom org-ai-auto-fill nil
  "If non-nil, will fill paragraphs when inserting completions."
  :type 'boolean
  :group 'org-ai)

(defcustom org-ai-service 'openai
  "Service to use if not specified."
  :type '(choice (const :tag "OpenAI" openai)
                 (const :tag "Azure-OpenAI" azure-openai)
                 (const :tag "perplexity.ai" perplexity.ai)
                 (const :tag "anthropic" anthropic)
                 (const :tag "DeepSeek" deepseek)
                 (const :tag "google" google)
                 (const :tag "Together" together)
                 (const :tag "Github" github))
  :group 'org-ai)

(defcustom org-ai-api-creds-token ""
  "This is your OpenAI API token.
You need to specify if you do not store the token in
`auth-sources'.  You can retrieve it at
https://platform.openai.com/account/api-keys."
  :type '(choice (string :tag "String value")
                  (plist :key-type symbol :value-type string :tag "Plist with symbol key and string value"))
  :type 'string
  :group 'org-ai)

;; (defcustom org-ai-use-auth-source t
;;   "If non-nil, use auth-source to retrieve the OpenAI API token.
;; The secret should be stored in the format
;;   machine api.openai.com login org-ai password <your token>
;; in the `auth-sources' file."
;;   :type 'boolean
;;   :group 'org-ai)

(defcustom org-ai-creds-completion-model "text-davinci-003"
  "The default model to use for completion requests.  See https://platform.openai.com/docs/models for other options."
  :type '(choice (string :tag "String value")
                  (plist :key-type symbol :value-type string :tag "Plist with symbol key and string value"))
  :group 'org-ai)

(defcustom org-ai-creds-chat-model "gpt-4o-mini"
  "The default model to use for chat-gpt requests.  See https://platform.openai.com/docs/models for other options."
  :type '(choice (string :tag "String value")
                  (plist :key-type symbol :value-type string :tag "Plist with symbol key and string value"))
  :group 'org-ai)

(defcustom org-ai-chat-models '("gpt-4o-mini"
                                "gpt-4"
                                "gpt-4-32k"
                                "gpt-4-turbo"
                                "gpt-4o"
                                "gpt-4o-mini"
                                "gpt-4o-realtime-preview"
                                "gpt-4o-search-preview"
                                "gpt-4o-mini-search-preview"
                                "gpt-4.1"
                                "gpt-4.1-nano"
                                "gpt-4.1-mini"
                                "gpt-3.5-turbo"
                                "o1"
                                "o1-pro"
                                "o1-preview"
                                "o1-mini"
                                "o3"
                                "o3-mini"
                                "o4-mini"
                                "chatgpt-4o-latest")
  "Alist of available chat models.  See https://platform.openai.com/docs/models."
  :type '(alist :key-type string :value-type string)
  :group 'org-ai)

(defcustom org-ai-default-max-tokens nil
  "The default maximum number of tokens to generate.  This is what costs money."
  :type 'string
  :group 'org-ai)

(defcustom org-ai-default-max-tokens-add-recomendation t
  "Additionally add system recomendation for chat mode about limit.
Function `org-ai-openai--get-lenght-recomendation' is used to create
prompt."
  :type 'boolean
  :group 'org-ai)

(defcustom org-ai-default-chat-system-prompt "Be helpful."
  "The system message helps set the behavior of the assistant:
https://platform.openai.com/docs/guides/chat/introduction.  This
default prompt is send as the first message before any user (ME)
or assistant (AI) messages.  Inside a +#begin_ai...#+end_ai block
you can override it with: '[SYS]: <your prompt>'."
  :type 'string
  :group 'org-ai)

(defcustom org-ai-default-inject-sys-prompt-for-all-messages nil
  "Wether to add the `org-ai-default-chat-system-prompt' before all user messages.

By default the system prompt is only added before the first
message.

You can set this to true for a single block using the
:sys-everywhere option on the #+begin_ai block.

This can be useful to enforce the behavior specified by this
messages."
  :type '(choice (const :tag "Before every message" all)
                 (const :tag "Before first" first)
                 (const :tag "Before last" last)
                 (const :tag "Don't add" nil)
                 )
  :group 'org-ai)

(make-obsolete-variable 'org-ai-default-inject-sys-prompt-for-all-messages
                        "With newer ChatGPT versions this is no longer necessary."
                        "2023-12-26")

(defvar org-ai-openai-chat-endpoint "https://api.openai.com/v1/chat/completions")

(defvar org-ai-openai-completion-endpoint "https://api.openai.com/v1/completions")

;; Azure-Openai specific variables

(defcustom org-ai-azure-openai-api-base "https://your-instance.openai.azure.com"
  "Base API URL for Azure-OpenAI."
  :type 'string
  :group 'org-ai)

;; Additional Azure-Openai specific variables
(defcustom org-ai-azure-openai-deployment "azure-openai-deployment-name"
  "Deployment name for Azure-OpenAI API."
  :type 'string
  :group 'org-ai)

(defcustom org-ai-azure-openai-api-version "2023-07-01-preview"
  "API version for Azure-OpenAI."
  :type 'string
  :group 'org-ai)

(defcustom org-ai-anthropic-api-version "2023-06-01"
  "API version for api.anthropic.com."
  :type 'string
  :group 'org-ai)

;; (defvar org-ai--last-url-request-buffer nil
;;   "Internal var that stores the current request buffer.
;; For stream responses.
;; May be shown for debugging.")
;; (make-variable-buffer-local 'org-ai--last-url-request-buffer)

;; (defvar org-ai--current-request-buffer nil
;;   "Internal var that stores the current request buffer.
;; For chat completion responses.")

(defvar org-ai--current-url-request-callback nil
  "Internal var that stores the current request callback.
Called within url request buffer, should know about target position,
that is why defined as lambda with marker.")
(make-variable-buffer-local 'org-ai--current-url-request-callback)

(defvar org-ai--current-request-is-streamed nil
  "Whether we expect a streamed response or a single completion payload.")
(make-variable-buffer-local 'org-ai--current-request-is-streamed)


(defvar org-ai-after-chat-insertion-hook nil
  "Hook that is called when a chat response is inserted.
Note this is called for every stream response so it will typically only
contain fragments.  First argument is
`TYPE' - simbol 'role, 'text or'end,
second - text or role name,
third - position before text insertion
fourth - target buffer with ai block for third position.")

(defvar org-ai--current-insert-position-marker nil
  "Where to insert the result.")
(make-variable-buffer-local 'org-ai--current-insert-position-marker)

(defvar org-ai--current-chat-role nil
  "During chat response streaming, this holds the role of the \"current speaker\".")
(make-variable-buffer-local 'org-ai--current-chat-role)

(defvar org-ai--currently-chat-got-first-response nil)
(make-variable-buffer-local 'org-ai--currently-chat-got-first-response)

(defvar org-ai--currently-inside-code-markers nil
  "For If code block received apply `fill-paragraph'.")
(make-variable-buffer-local 'org-ai--currently-inside-code-markers)

(defvar org-ai--currently-reasoning nil)
(make-variable-buffer-local 'org-ai--currently-reasoning)

(defvar org-ai--url-buffer-last-position-marker nil
  "Local buffer var to store last read position.")
(make-variable-buffer-local 'org-ai--url-buffer-last-position-marker)
;; (makunbound 'org-ai--url-buffer-last-position-marker)

(cl-deftype org-ai--response-type ()
  '(member role text stop error))

(cl-defstruct org-ai--response
  (type (:type org-ai--response-type))
  payload)

;; (defvar org-ai--debug-data nil)
;; (defvar org-ai--debug-data-raw nil)

;; (with-current-buffer "*scratch*"
;;   (erase-buffer)
;;   (pop-to-buffer "*scratch*" t)
;;   (let ((n 16))
;;    (insert (car (nth n org-ai--debug-data-raw)))
;;    (goto-char (cadr (nth n org-ai--debug-data-raw)))
;;    (beginning-of-line)))

(defcustom org-ai-debug-buffer nil
  "If non-nil, enable debuging to a debug buffer.
  Set to something like *debug-org-ai*. to enable debugging."
  :type 'string
  :group 'org-ai)

;;; - debugging
(defun org-ai--prettify-json-string (json-string)
  "Convert a compact JSON string to a prettified JSON string.
This function uses a temporary buffer to perform the prettification.
Returns the prettified JSON string.
Argument JSON-STRING string with json."
  (condition-case err
      (let* ((parsed-json (json-read-from-string json-string))
             ;; 1. First, encode the JSON object. This will be compact with your json-encode.
             (compact-json (json-encode parsed-json)))
        (with-temp-buffer
           (insert compact-json)
           (json-pretty-print-buffer)
           (buffer-string)))
    (error
        (message "Error formatting JSON: %S" err)
        (message "Input JSON: %S" json-string))))

(defun org-ai--debug-get-caller()
  "Return string with name of function of caller function.
Heavy to execute."
  (let* ((backtrace-line-length 20)
         (print-level 3)
         (print-length 10)
         (bt
          ;; (with-output-to-string (backtrace))
          (backtrace-to-string (backtrace-get-frames 'backtrace))
          )
         (caller))
         ;; (print bt)
         (seq-find
          ; - predicate
          (lambda (line)
            (let* ( (mpos (string-match "(" line))
                   (sline (substring line 0 mpos))
                   (tline (string-trim-right (string-trim-left sline))))
                   (if (and (not (string-empty-p tline))
                            (not (member tline '("org-ai--debug-get-caller" "org-ai--debug" ) )))
                       (setq caller tline)
                     nil ; else
                     )))
          ;; - lines
          (cdr (split-string bt "\n" t)))
         caller))

(defun org-ai--debug (&rest args)
  "If firt argument of ARGS is a stringwith %s than behave like format.
Otherwise format every to string and concatenate."
  (when (and org-ai-debug-buffer args)

    (save-excursion
      (let* ((buf-exist (get-buffer org-ai-debug-buffer))
             (bu (or buf-exist (get-buffer-create org-ai-debug-buffer)))
             (current-window (selected-window))
             (bu-window (or (get-buffer-window bu)
                            (if (>= (count-windows) 2)
                                (display-buffer-in-direction ; exist but hidden
                                 bu
                                 '((direction . left)
                                   (window . new)
                                   (window-width . 0.2)))
                              ;; else
                              (display-buffer-in-direction ; exist but hidden
                               bu
                               '((direction . left)
                                 (window . new)
                                 )))
                            (select-window current-window))))

        (with-current-buffer bu
          ;; - move point to  to bottom
          (if buf-exist ; was not created
              (goto-char (point-max)))
          ;; ;; - scroll debug buffer down
          (if bu-window
              (with-selected-window (get-buffer-window bu)
                (with-no-warnings
                  (end-of-buffer nil))
                ;; (recenter '(t))
                ))
          ;; ;; - output caller function ( working, but too heavy)
          ;; (let ((caller
          ;;        (org-ai--debug-get-caller)))
          ;;   (when caller
          ;;     (insert "Din ")
          ;;     (insert caller)
          ;;     (insert " :")))
          ;; - output args
          (save-match-data
            (if (and (equal (type-of (car args)) 'string)
                     (string-match "%s" (car args)))
                (progn
                  (insert (apply 'format (car args) (cdr args)))
                  (newline))

              ;; else
              (insert (apply #'concat (mapcar (lambda (arg)
                                                (if (equal (type-of arg) 'string)
                                                    (format "%s\n" arg)
                                                  (concat (prin1-to-string arg) "\n"))
                                                ) args)))
              ;; (newline))
              )))))))

;; (org-ai--debug "test %s" 2)
;; (org-ai--debug "test" 2 3 "sd")

(defun org-ai--debug-urllib (source-buf)
  "Copy `url-http' buffer with response to our debugging buffer.
Argument SOURCE-BUF url-http response buffer."
  (when (and source-buf org-ai-debug-buffer)
    (save-excursion
      (let* ((buf-exist (get-buffer org-ai-debug-buffer))
             (bu (or buf-exist (get-buffer-create org-ai-debug-buffer))))
        (with-current-buffer bu
          (let ((stri (with-current-buffer source-buf
                        ;; (save-excursion
                          (buffer-substring-no-properties (or org-ai--url-buffer-last-position-marker
                                                              (point-min))
                                                          (point-max)))))
            (goto-char (point-max))
            (insert "url-buf response:\n")
            (insert stri)
            (newline))))
      )))

;;; - Get constant functions
(defun org-ai--check-model (model endpoint)
  "Check if the model name is somehow mistyped.
`MODEL' is the model name.  `ENDPOINT' is the API endpoint."
  (unless model
    (error "No org-ai model specified."))

  (when (or (string-match-p "api.openai.com" endpoint)
            (string-match-p "openai.azure.com" endpoint))

    (let ((lowercased (downcase model)))
      (when (and (string-prefix-p "gpt-" model) (not (string-equal lowercased model)))
        (warn "Model name '%s' should be lowercase. Use '%s' instead." model lowercased)))

    (unless (member model org-ai-chat-models)
      (message "Model '%s' is not in the list of available models. Maybe this is because of a typo or maybe we haven't yet added it to the list. To disable this message add (add-to-list 'org-ai-chat-models \"%s\") to your init file." model model))))

;; (defmacro org-ai--read-service-name (name)
;;   "Map a service NAME such as 'openai' to a valid `org-ai-service' symbol."
;;   ,(intern-soft ,name))

;; (defun org-ai--service-of-model (model)
;;   "Return the service of the model.
;; `MODEL' is the model name."
;;   (cond
;;    ((string-prefix-p "gpt-" model) 'openai)
;;    ((string-prefix-p "chatgpt-" model) 'openai)
;;    ((string-prefix-p "o1" model) 'openai)
;;    ((string-prefix-p "o3" model) 'openai)
;;    ((string-prefix-p "o4" model) 'openai)
;;    ((string-prefix-p "claude" model) 'anthropic)
;;    ((string-prefix-p "gemini" model) 'google)
;;    ((string-prefix-p "deepseek" model) 'deepseek)
;;    (t nil)))

(defun org-ai--openai-get-token (service)
  "Try to get the openai token.
Either from `org-ai-api-creds-token' or from auth-source.
Optional argument SERVICE of token."
  (cond ((and (stringp org-ai-api-creds-token)
              (not (string-empty-p org-ai-api-creds-token)))
         org-ai-api-creds-token)
        ((plistp org-ai-api-creds-token)
         (or (plist-get org-ai-api-creds-token
                        ;; convert symbol to keyword
                        (intern (concat ":"
                                        (if (symbolp service)
                                            (symbol-name service)
                                          ;; else
                                          service))))
             (error "Token not found in defined plist `org-ai-api-creds-token'.")))
        ((and
          ;; org-ai-use-auth-source
          (org-ai--openai-get-token-auth-source service)))
        (t
         (error "Please set `org-ai-api-creds-token' to your OpenAI API token or setup auth-source (see org-ai readme)"))))

(defmacro org-ai--get-value-or-string (var key)
  "Retrieve value from VAR using KEY if VAR is a plist, or return VAR if it's a string.
VAR is the variable name to evaluate.
KEY is a string used as a key (converted to key symbol with : character)
if VAR is a plist."
  `(cond ((plistp ,var)
          (plist-get ,var (intern (concat ":" ,key))))
         ((stringp ,var)
          ,var)
         (t nil)))

;; (defun org-ai--openai-get-chat-model (service)
;;   "Allow to set default model as one string or per service."

;; )
;; (defun org-ai--openai-get-completion-model (service)
;;   "Allow to set default model as one string or per service."

;; )

(defun org-ai--strip-api-url (url)
  "Strip the leading https:// and trailing / from an URL."
  (let* ((stripped-url
          ;; Remove "https://" or "http://" if present
          (cond
           ((string-prefix-p "https://" url) (substring url 8))
           ((string-prefix-p "http://" url) (substring url 7))
           (t url)))
         (parts (split-string stripped-url "/" t))) ; Split by '/', t means remove empty strings
    ;; Return the first part, which should be the hostname
    (car parts)))

(defun org-ai--openai-get-token-auth-source (&optional service)
  "Retrieves the authentication token for the OpenAI SERVICE using auth-source."
  (require 'auth-source)
  (let* ((service (or service org-ai-service))
         ;; azure-openai - special case
         (endpoint (if (eql endpoint 'azure-openai)
                       (org-ai--strip-api-url org-ai-azure-openai-api-base)
                     ;; else - parse URL
                     (car (alist-get service org-ai-endpoints)))))
    (or (auth-source-pick-first-password :host endpoint :user "org-ai")
        (auth-source-pick-first-password :host endpoint :login "org-ai"))))

(defcustom org-ai-endpoints
  '((perplexity.ai	"https://api.perplexity.ai/chat/completions")
    (deepseek		"https://api.deepseek.com/v1/chat/completions")
    (anthropic		"https://api.anthropic.com/v1/messages")
    (google		"https://generativelanguage.googleapis.com/v1beta/openai/chat/completions")
    (together		"https://api.together.xyz/v1/chat/completions")
    (github		"https://models.github.ai/inference/chat/completions")
    )
  "Endpoints for services.
  This is a not ordered list of key-value pairs in format of List of
  lists: (SYMBOL VALUE-STRING). Used for POST HTTP request to service."
  :type '(alist :key-type (symbol :tag "Service")
                :value-type (string :tag "Endpoint URL")
  :group 'org-ai))


(defun org-ai--get-endpoint (messages &optional service)
  "Determine the correct endpoint based on the service and
whether messages are provided."
  (let* ((service (or service org-ai-service))
        (endpoint (car (alist-get service org-ai-endpoints))))
    (cond
     (endpoint endpoint)
     ((eq service 'azure-openai)
      (format "%s/openai/deployments/%s%s/completions?api-version=%s"
              org-ai-azure-openai-api-base org-ai-azure-openai-deployment
              (if messages "/chat" "") org-ai-azure-openai-api-version))
     (t
      (if messages org-ai-openai-chat-endpoint org-ai-openai-completion-endpoint)))))

(defun org-ai--get-headers (service)
  "Determine the correct headers based on the service."
  (let ((service (or service org-ai-service)))
    `(("Content-Type" . "application/json")
      ,@(cond
        ((eq service 'azure-openai)
         `(("api-key" . ,(org-ai--openai-get-token service))))
        ((eq service 'anthropic)
         `(("x-api-key" . ,(org-ai--openai-get-token service))
           ("anthropic-version" . ,org-ai-anthropic-api-version)))
        ((eq service 'google)
         `(("Accept-Encoding" . "identity")
           ("Authorization" . ,(encode-coding-string (string-join `("Bearer" ,(org-ai--openai-get-token service)) " ") 'utf-8))))
        (t
         `(("Authorization" . ,(encode-coding-string (string-join `("Bearer" ,(org-ai--openai-get-token service)) " ") 'utf-8))))))))


(defun org-ai-openai--get-lenght-recomendation (max-tokens)
  "Recomendation to limit yourself.
- words = tokens * 0.75
- tokens = words * 1.33333
- token = 4 characters
- word - 5 characters
- sentence - 15-25 words = 20 words = 26 tokens (tech/academ larger)
- paragraph - 6 sentences, 500-750 characters, 150-300 words = 150 words = 200 tokens
- page - around 3-4 paragraphs, 500 words = 600 tokens
"
  (when max-tokens
    (cond ((< max-tokens 75)
           (format "Do final answer with %d words limit." (* max-tokens 0.75)))
          ((and (>= max-tokens 75)
                (< max-tokens 500))
           (format "Do final answer with %d sentences limit." (/ max-tokens 29)))
          ((and (>= max-tokens 500)
                (<= max-tokens 1000))
           (format "Do final answer with %d paragraph or %d pages limit length, but not strict." (/ max-tokens 200) (ceiling (/ max-tokens 600.0)))))))

;;; - Main
;; org-ai-stream-completion - old

;; &optional &key prompt messages context model max-tokens temperature top-p frequency-penalty presence-penalty service stream
(defun org-ai-api-request-prepare (req-type element sys-prompt sys-prompt-for-all-messages model max-tokens top-p temperature frequency-penalty presence-penalty service stream)
  "Compose API request from data and start a server-sent event stream.
Call `org-ai-api-request' function as a next step.
Called from `org-ai-interface-step1' in main file.
`REQ-TYPE' symbol - is completion or chat mostly.
`ELEMENT' org-element - is ai block, should be converted to market at once.
`SYS-PROMPT' string - first system instruction as a string.
`SYS-PROMPT-FOR-ALL-MESSAGES' from `org-ai-default-inject-sys-prompt-for-all-messages' variable.
`MODEL' string - is the model to use.
`MAX-TOKENS' integer - is the maximum number of tokens to generate.
`TEMPERATURE' integer - 0-2 lower - more deterministic.
`TOP-P' integer - 0-1 lower - more deterministic.
`FREQUENCY-PENALTY' integer - -2-2, lower less repeat words.
`PRESENCE-PENALTY' integer - -2-2, lower less repeat concepts.
`SERVICE' symbol or string - is the AI cloud service such as 'openai or 'azure-openai'.
`STREAM' string - as bool, indicates whether to stream the response."
  (org-ai--debug "org-ai-api-request-prepare")
  (let* (
         (content (string-trim (org-ai-block-get-content element))) ; string - is block content
         (messages (unless (eql req-type 'completion)
                     ;; - split content to messages
                     (org-ai-openai--collect-chat-messages content
                                                           sys-prompt
                                                           sys-prompt-for-all-messages
                                                           (if org-ai-default-max-tokens-add-recomendation
                                                               (org-ai-openai--get-lenght-recomendation max-tokens)
                                                             )))) ; org-ai-block.el
         (end-marker (org-ai-block--get-content-end-marker element))
         ;; TODO: replace with result of `org-ai-agent-callback' call
         (callback (cond ; set to org-ai--current-url-request-callback
                    (messages
                     (lambda (result) (org-ai--insert-stream-response end-marker result t)))
                    ;; - completion
                    (t (lambda (result) (org-ai--insert-single-response end-marker
                                                                        (org-ai--get-single-response-text result)
                                                                        nil))))))
    ;; - Call and save buffer.
    (org-ai-timers--set
     (org-ai-api-request service model callback
                         :prompt content ; if completion
                         :messages messages
                         :max-tokens max-tokens
                         :temperature temperature
                         :top-p top-p
                         :frequency-penalty frequency-penalty
                         :presence-penalty presence-penalty
                         :stream stream)
     (org-ai-block-get-header-marker element))

    ;; - run timer that show /-\ looping, notification of status
    (org-ai-timers--progress-reporter-run
     #'org-ai-openai--interrupt-url-request
     )))


;; Together.xyz 2025
;; '(id "nz7KyaB-3NKUce-9539d1912ce8b148" object "chat.completion" created 1750575101 model "meta-llama/Llama-3.3-70B-Instruct-Turbo-Free" prompt []
;;   choices [(finish_reason "length" seed 3309196889559996400 logprobs nil index 0
;;             message (role "assistant" content " The answer is simple: live a long time. But how do you do that? Well, itâs not as simple as it sounds." tool_calls []))] usage (prompt_tokens 5 completion_tokens 150 total_tokens 155 cached_tokens 0))

(defun org-ai--get-single-response-text (&optional response)
  "Return text from response or nil and signal error if it have \"error\" field.
For Completion LLM mode. Used as callback for `org-ai-api-request'."
  ;; (org-ai--debug "org-ai--get-single-response-text response:" response)
  (when response
    (if-let ((error (plist-get response 'error)))
        (if-let ((message (plist-get error 'message))) (error message) (error error))
      ;; else - no "error" field
      (if-let* ((choice (aref (plist-get response 'choices) 0))
                (text (or (plist-get choice 'text)
                          ;; Together.xyz way
                          (plist-get (plist-get choice 'message) 'content))))
          ;; - Decode text
          (decode-coding-string text 'utf-8)))))

(cl-assert
 (equal
  (let ((test-val
         '(id "nz7KyaB-3NKUce-9539d1912ce8b148" object "chat.completion" created 1750575101 model "meta-llama/Llama-3.3-70B-Instruct-Turbo-Free" prompt []
              choices [(finish_reason "length" seed 3309196889559996400 logprobs nil index 0
                                      message (role "assistant" content " The answer is simple: live a long time. But how do you do that? Well, itâs not as simple as it sounds." tool_calls []))] usage (prompt_tokens 5 completion_tokens 150 total_tokens 155 cached_tokens 0))
         )) (org-ai--get-single-response-text test-val))
  " The answer is simple: live a long time. But how do you do that? Well, itâs not as simple as it sounds."))



(defun org-ai--insert-single-response (end-marker &optional text insert-role final)
  "Insert result to ai block.
Should be used in two steps: 1) for insertion of text 2) with TEXT equal
to nil, for finalizing by setting pointer to the end and insertion of me
role.
Here used for completion mode in `org-ai-api-request'.
END-MARKER is where to put result,
TEXT is string from the response of OpenAI API extracted with `org-ai--get-single-response-text'.
END-MARKER is a buffer and position at the end of block.
FINAL wherer to finalize, also applied if no text provided."
  (org-ai--debug "org-ai--insert-single-response end-marker, text:" end-marker
                                                 text "")

    (let ((buffer (marker-buffer end-marker))
          (pos (marker-position end-marker)))
      (org-ai--debug "org-ai--insert-single-response buffer,pos:" buffer pos "")

      ;; - write in target buffer
      (if text
          (with-current-buffer buffer ; Where target ai block located.
            ;; set mark (point) to allow user "C-u C-SPC" command to easily select the generated text
            (push-mark end-marker)
            (save-excursion
              ;; - go  to the end of previous line and open new one
              (goto-char pos)
              (if (bolp)
                  (goto-char (1- pos)))
              (newline)
              ;; - Make sure we have enough space at end of block, don't write on same line
              (when (string-suffix-p "#+end_ai" (buffer-substring-no-properties (point) (line-end-position)))
                (insert "\n")
                (backward-char))
              (insert text)
              (set-marker end-marker (point))
              (condition-case hook-error
                  (run-hook-with-args 'org-ai-after-chat-insertion-hook 'end text pos buffer)
                (error
                 (message "Error during \"after-chat-insertion-hook\": %s" hook-error)))
              (when final
                (org-element-cache-reset)
                (undo-boundary))
              (setq pos (point))))

            ;; ;; - save in url buffer. for what?
            ;; (setq org-ai--current-insert-position-marker pos)

        ;; - else - DONE
        ;; (org-ai-reset-stream-state)
        ;; - special cases for DONE
        (with-current-buffer buffer
          (when insert-role
            (save-excursion
              ;; - go  to the end of previous line and open new one
              (goto-char (1- pos))
              (newline)
              (insert "\n\n[ME]: ")
              (setq pos (point)))
            (set-marker end-marker (point)))
          (when org-ai-jump-to-end-of-block
            (goto-char pos))
          ;; final
          (org-element-cache-reset)
          (undo-boundary)))))


;; Here is an example for how a full sequence of OpenAI responses looks like:
;; '((id "chatcmpl-9hM1UJgWe4cWKcJKvoMBzzebOOzli" object "chat.completion.chunk" created 1720119788 model "gpt-4o-2024-05-13" system_fingerprint "fp_d576307f90" choices [(index 0 delta (role "assistant" content "") logprobs nil finish_reason nil)])
;;   (id "chatcmpl-9hM1UJgWe4cWKcJKvoMBzzebOOzli" object "chat.completion.chunk" created 1720119788 model "gpt-4o-2024-05-13" system_fingerprint "fp_d576307f90" choices [(index 0 delta (content "Hello") logprobs nil finish_reason nil)])
;;   (id "chatcmpl-9hM1UJgWe4cWKcJKvoMBzzebOOzli" object "chat.completion.chunk" created 1720119788 model "gpt-4o-2024-05-13" system_fingerprint "fp_d576307f90" choices [(index 0 delta (content ",") logprobs nil finish_reason nil)])
;;   (id "chatcmpl-9hM1UJgWe4cWKcJKvoMBzzebOOzli" object "chat.completion.chunk" created 1720119788 model "gpt-4o-2024-05-13" system_fingerprint "fp_d576307f90" choices [(index 0 delta (content " Robert") logprobs nil finish_reason nil)])
;;   (id "chatcmpl-9hM1UJgWe4cWKcJKvoMBzzebOOzli" object "chat.completion.chunk" created 1720119788 model "gpt-4o-2024-05-13" system_fingerprint "fp_d576307f90" choices [(index 0 delta nil logprobs nil finish_reason "stop")])
;;   nil)
;;
;; and Anthropic:
;; '((type "message_start" message (id "msg_01HoMq4LgkUpHpkXqXoZ7R1W" type "message" role "assistant" model "claude-3-5-sonnet-20240620" content [] stop_reason nil stop_sequence nil usage (input_tokens 278 output_tokens 2)))
;;   (type "content_block_start" index 0 content_block (type "text" text ""))
;;   (type "ping")
;;   (type "content_block_delta" index 0 delta (type "text_delta" text "Hello Robert"))
;;   (type "content_block_delta" index 0 delta (type "text_delta" text "."))
;;   (type "content_block_stop" index 0)
;;   (type "message_delta" delta (stop_reason "end_turn" stop_sequence nil) usage (output_tokens 22))
;;   (type "message_stop"))
;;
;; and Google
;; '((created 1745008491
;;    model "gemini-2.5-pro-preview-03-25"
;;    object "chat.completion.chunk"
;;    choices [(delta (content "Hello Robert! How can I help you today?"
;;                     role "assistant")
;;              finish_reason "stop"
;;              index 0)]))
(defun org-ai--normalize-response (response)
  "This function normalizes JSON data received from OpenAI-style, Anthropic, and Perplexity endpoints.
`RESPONSE' is one JSON message of the stream response."
  ;; (org-ai--debug "response:" response)

  (if-let ((error-message (plist-get response 'error)))
      (list (make-org-ai--response :type 'error :payload (or (plist-get error 'message) error-message)))

    (let ((response-type (plist-get response 'type)))

      ;; first try anthropic
      (cond
       ((string= response-type "ping") nil)
       ((string= response-type "message_start")
        (when-let ((role (plist-get (plist-get response 'message) 'role)))
          (list (make-org-ai--response :type 'role :payload role))))
       ((string= response-type "content_block_start")
        (when-let ((text (plist-get (plist-get response 'content_block) 'text)))
          (list (make-org-ai--response :type 'text :payload text))))
       ((string= response-type "content_block_delta")
        (when-let ((text (plist-get (plist-get response 'delta) 'text)))
          (list (make-org-ai--response :type 'text :payload text))))
       ((string= response-type "content_block_stop") nil)
       ((string= response-type "message_delta")
        (when-let ((stop-reason (plist-get (plist-get response 'delta) 'stop_reason)))
          (list (make-org-ai--response :type 'stop :payload stop-reason))))
       ((string= response-type "message_stop") nil)


       ;; try perplexity.ai
       ((and (plist-get response 'model) (string-prefix-p "llama-" (plist-get response 'model)))
        (let ((choices (plist-get response 'choices)))
          (when (and choices (> (length choices) 0))
            (let* ((choice (aref choices 0))
                   (message (plist-get choice 'message))
                   (delta (plist-get choice 'delta))
                   (role (or (plist-get delta 'role) (plist-get message 'role)))
                   (content (or (plist-get delta 'content) (plist-get message 'content)))
                   (finish-reason (plist-get choice 'finish_reason)))
              (append
               (when role
                 (list (make-org-ai--response :type 'role :payload role)))
               (when content
                 (list (make-org-ai--response :type 'text :payload content)))
               (when finish-reason
                 (list (make-org-ai--response :type 'stop :payload finish-reason))))))))

       ;; single message e.g. from non-streamed completion
       ((let ((choices (plist-get response 'choices)))
          (and (= 1 (length choices))
               (plist-get (aref choices 0) 'message)))
        (let* ((choices (plist-get response 'choices))
               (choice (aref choices 0))
               (text (plist-get (plist-get choice 'message) 'content))
               (role (plist-get (plist-get choice 'message) 'role))
               (finish-reason (or (plist-get choice 'finish_reason) 'stop)))
          (list (make-org-ai--response :type 'role :payload role)
                (make-org-ai--response :type 'text :payload text)
                (make-org-ai--response :type 'stop :payload finish-reason))))

       ;; try openai, deepseek, gemini streamed
       (t (let ((choices (plist-get response 'choices)))
            (cl-loop for choice across choices
                     append (let* ((delta (plist-get choice 'delta))
                                   (role (when-let ((role (plist-get delta 'role)))
                                           (if (and (string= "assistant" role)
                                                    (plist-get delta 'reasoning_content))
                                               "assistant_reason"
                                             role)))
                                   (content (plist-get (plist-get choice 'delta) 'content))
                                   (reasoning-content (plist-get delta 'reasoning_content))
                                   (finish-reason (plist-get choice 'finish_reason))
                                   (result nil))
                              (when finish-reason
                                (push (make-org-ai--response :type 'stop :payload finish-reason) result))
                              (when reasoning-content
                                (setq org-ai--currently-reasoning t)
                                (push (make-org-ai--response :type 'text :payload reasoning-content) result))
                              (when (and content (> (length content) 0))
                                (push (make-org-ai--response :type 'text :payload content) result)
                                (when org-ai--currently-reasoning
                                  (setq org-ai--currently-reasoning nil)
                                  (push (make-org-ai--response :type 'role :payload "assistant") result)))
                              (when role
                                (push (make-org-ai--response :type 'role :payload role) result))
                              result))))))))

(defun org-ai--insert-stream-response (end-marker &optional response insert-role)
  "Insert result to ai block for chat mode.
When first chunk received we stop waiting timer for request.
END-MARKER'is where to put result,
RESPONSE is one JSON message of the stream response.
Used as callback for `org-ai-api-request', called in url buffer.
When RESPONSE is nil, it means we are done.
Save variables:
`org-ai--current-insert-position-marker',
`org-ai--currently-inside-code-markers'
`org-ai--currently-chat-got-first-response'
`org-ai--current-chat-role' in current buffer.
Called within url-buffer."

(org-ai--debug "org-ai--insert-stream-response org-ai-reset-stream-state")
  ;; (if (not response)
  ;;     (progn
  ;;       (org-ai--debug "org-ai--insert-stream-response org-ai-reset-stream-state")
  ;;       ;; (org-ai-reset-stream-state))
    ; - else
    (let ((normalized (org-ai--normalize-response response)) ; list of messages
          (buffer (marker-buffer end-marker))
          (first-resp org-ai--currently-chat-got-first-response)
          (pos (or org-ai--current-insert-position-marker
                   (marker-position end-marker)))
          (c-inside-code-m org-ai--currently-inside-code-markers)
          (c-chat-role org-ai--current-chat-role)
          (url-buffer (current-buffer))
          stop-flag)
      ;; (org-ai--debug "org-ai--insert-stream-response" normalized)
      (unwind-protect ; we need to save variables to url buffer
          (with-current-buffer buffer ; target buffer with block
            (save-excursion
              ;; (when (not normalized)
              ;;   (org-ai--debug "org-ai--insert-stream-response NO normalized!" response))
              ;; - LOOP Per message
              (cl-loop for response in normalized
                       do (let ((type (org-ai--response-type response)))
                            ;; (org-ai--debug "org-ai--insert-stream-response: %s %s %s" type end-marker org-ai--current-insert-position-marker)
                            ;; - Type of message: error
                            (when (eq type 'error)
                              (error (org-ai--response-payload response)))

                            ;; - Make sure we have enough space at end of block, don't write on same line
                            (goto-char pos)
                            (when (string-suffix-p "#+end_ai" (buffer-substring-no-properties (point) (line-end-position)))
                              (insert "\n")
                              (backward-char))

                            ;; - Type of message
                            (cl-case type
                              (role (let ((role (org-ai--response-payload response)))
                                      (when (not (string= role c-chat-role))
                                        (goto-char pos)

                                        (setq c-chat-role role)
                                        (let ((role (and insert-role (org-ai--response-payload response))))
                                          (cond
                                           ((string= role "assistant_reason")
                                            (insert "\n[AI_REASON]: "))
                                           ((string= role "assistant")
                                            (insert "\n[AI]: "))
                                           ((string= role "user")
                                            (insert "\n[ME]: "))
                                           ((string= role "system")
                                            (insert "\n[SYS]: ")))

                                          (condition-case hook-error
                                              (run-hook-with-args 'org-ai-after-chat-insertion-hook 'role role pos buffer)
                                            (error
                                             (message "Error during \"after-chat-insertion-hook\" for role: %s" hook-error)))

                                          (setq pos (point))
                                          ))))

                              (text (let ((text (decode-coding-string (org-ai--response-payload response) 'utf-8)))
                                      (goto-char pos)
                                      (when (or first-resp (not (string= (string-trim text) "")))
                                        (when (and (not first-resp) (string-prefix-p "```" text))
                                          ;; start markdown codeblock responses on their own line
                                          (insert "\n"))
                                        (when first-resp
                                          ;; call stop waiting with url-buffer and progress reporter.
                                          (org-ai--debug "org-ai--insert-stream-response first-resp")
                                          (org-ai-timers--interrupt-current-request url-buffer)
                                          )
                                        ;; track if we are inside code markers
                                        (setq c-inside-code-m (and (not c-inside-code-m) ; org-ai--currently-inside-code-markers
                                                        (string-match-p "```" text)))
                                        ;; (org-ai--debug response)
                                        ;; (org-ai--debug text)
                                        (insert text)
                                        ;; - "auto-fill" if not in code block
                                        (when (and org-ai-auto-fill (not c-inside-code-m))
                                          (fill-paragraph))

                                        (condition-case hook-error
                                            (run-hook-with-args 'org-ai-after-chat-insertion-hook 'text text pos buffer)
                                          (error
                                           (message "Error during \"after-chat-insertion-hook\" for text: %s" hook-error)))
                                        )
                                      (setq first-resp t)
                                      (setq pos (point))
                                      ))

                              (stop (progn
                                      ;; (when pos
                                      (goto-char pos)

                                      ;; (message "inserting user prompt: %" (string= c-chat-role "user"))
                                      (let ((text "\n\n[ME]: "))
                                        (if insert-role
                                            (insert text)
                                          ;; else
                                          (setq text ""))

                                        (condition-case hook-error
                                            (run-hook-with-args 'org-ai-after-chat-insertion-hook 'end text pos buffer)
                                          (error
                                           (message "Error during \"after-chat-insertion-hook\": %s" hook-error)))
                                        (setq pos (point)))

                                      (org-element-cache-reset)
                                      (setq stop-flag t)
                                      ))))))
            ;; - without save-excursion - go to the end.
            (when (and org-ai-jump-to-end-of-block
                       stop-flag)
              (goto-char pos)))
        ;; - after buffer - UNWINDFORMS - save variables to url-buffer
        (setq org-ai--current-insert-position-marker pos)
        (setq org-ai--currently-inside-code-markers c-inside-code-m)
        (setq org-ai--current-chat-role c-chat-role)
        )
      ;; - in let
      normalized))
;; org-ai-stream-request - old
(cl-defun org-ai-api-request (service model callback &optional &key prompt messages max-tokens temperature top-p frequency-penalty presence-penalty stream)
  "Use API to LLM to request and get response.
Executed by `org-ai-api-request-prepare'
`PROMPT' is the query for completions `MESSAGES' is the query for
chatgpt. `CALLBACK' is the callback function. `MODEL' is the
model to use. `MAX-TOKENS' is the maximum number of tokens to
generate. `TEMPERATURE' is the temperature of the distribution.
`TOP-P' is the top-p value. `FREQUENCY-PENALTY' is the frequency
penalty. `PRESENCE-PENALTY' is the presence penalty.
Variables used to save state:
not buffer local:

buffer local and nil by default:
`org-ai--current-insert-position-marker' - in url callback to track where we insert.
`org-ai--currently-chat-got-first-response' - for Stream, bool.
`org-ai--currently-inside-code-markers' - code block received, bool.
`org-ai--current-request-is-streamed'
`org-ai--current-url-request-callback'.

Parallel requests require to keep `url-request-buffer'
to be able  to kill it. We  solve this by creating timer  in buffer with
name of url-request-buffer +1.
We count running ones in global integer only.

For not stream url return event and hook after-change-functions
 triggered only after url buffer already kill, that is why we don't use
 this hook.
"


  ;; (setq org-ai--debug-data nil)
  ;; (setq org-ai--debug-data-raw nil)

  ;; (org-ai--debug service (type-of service))
  ;; (org-ai--debug stream (type-of stream))

  ;; - HTTP body preparation as a string
  (let ((url-request-extra-headers (org-ai--get-headers service))
        (url-request-method "POST")
        (endpoint (org-ai--get-endpoint messages service))
        (url-request-data (org-ai--payload :prompt prompt
					   :messages messages
					   :model model
					   :max-tokens max-tokens
					   :temperature temperature
					   :top-p top-p
					   :frequency-penalty frequency-penalty
					   :presence-penalty presence-penalty
					   :service service
					   :stream stream)))
    ;; - regex check
    (org-ai--check-model model endpoint) ; not empty and if "api.openai.com" or "openai.azure.com"

    (org-ai--debug "org-ai-api-request endpoint:" endpoint (type-of endpoint)
                   "request-data:" (org-ai--prettify-json-string url-request-data))

    (org-ai--debug "Main request before, that return a \"urllib buffer\".")
    (let ((url-request-buffer
           (url-retrieve ; <- - - - - - - - -  MAIN
            endpoint
            (lambda (_events) ; called with url-request-buffer as current buffer
              "url-request-buffer event"
              ;; called at error or at the end after `after-change-functions' hooks
              (org-ai--debug "url-retrieve callback:" _events)

              (org-ai--debug-urllib (current-buffer))

              ;; Called for not stream, call `org-ai--current-url-request-callback'
              (org-ai--url-request-on-change-function nil nil nil)

              (org-ai--maybe-show-openai-request-error) ; TODO: change to RESULT by global customizable option
              ;; finally stop track buffer, error or not
              ;; (org-ai--debug "Main request lambda" _events)
              (org-ai-timers--interrupt-current-request (current-buffer) #'org-ai-openai--stop-tracking-url-request)
              ))))

      (org-ai--debug "Main request after." url-request-buffer)

      (with-current-buffer url-request-buffer ; old org-ai--last-url-request-buffer
        ; just in case, also reset in `org-ai-reset-stream-state'
        (setq org-ai--currently-inside-code-markers nil)
        (setq org-ai--current-insert-position-marker nil)
        (setq org-ai--currently-chat-got-first-response nil)
        (setq org-ai--current-chat-role nil)
        ;; - it is `org-ai--insert-stream-response' or `org-ai--insert-single-response'
        (setq org-ai--current-url-request-callback callback)
        ;; - `org-ai--url-request-on-change-function', `org-ai-reset-stream-state', `org-ai--current-request-is-streamed'
        (setq org-ai--current-request-is-streamed stream)

        ;; - for stream add hook, otherwise remove - do word by word output (optional actually)
        (if stream
            (unless (member 'org-ai--url-request-on-change-function after-change-functions)
              (add-hook 'after-change-functions #'org-ai--url-request-on-change-function nil t))
          ;; else - not stream
          (remove-hook 'after-change-functions #'org-ai--url-request-on-change-function t))
        )
      url-request-buffer)))



(cl-defun org-ai-api-request-llm (service model callback &optional &key prompt messages max-tokens temperature top-p frequency-penalty presence-penalty)
  "Simplified version of `org-ai-api-request' without stream support.
Used for building agents or chain of requests.
Call callback with nil or result of `org-ai--normalize-response' of response."
  (let ((url-request-extra-headers (org-ai--get-headers service))
        (url-request-method "POST")
        (endpoint (org-ai--get-endpoint messages service))
        (url-request-data (org-ai--payload :prompt prompt
					   :messages messages
					   :model model
					   :max-tokens max-tokens
					   :temperature temperature
					   :top-p top-p
					   :frequency-penalty frequency-penalty
					   :presence-penalty presence-penalty
					   :service service
					   :stream nil)))
    (org-ai--debug "org-ai-api-request-llm prompt: %s" prompt)
    (org-ai--debug "org-ai-api-request-llm messages: %s" messages)
    (org-ai--debug "org-ai-api-request-llm endpoint: %s %s" endpoint (type-of endpoint))
    (org-ai--debug "org-ai-api-request-llm request-data:" (org-ai--prettify-json-string url-request-data))



    (let* ((url-request-buffer
           (url-retrieve ; <- - - - - - - - -  MAIN
            endpoint
            (lambda (_events)
              (org-ai--debug-urllib (current-buffer))
              (unless (org-ai--maybe-show-openai-request-error) ; TODO: change to RESULT by global customizable option
                ;; - read from url-buffer
                (when (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
                  (goto-char url-http-end-of-headers)
                  (let ((json-object-type 'plist)
                        (json-key-type 'symbol)
                        (json-array-type 'vector))
                    (condition-case _err
                        ;;   (print (list "HERE5" (current-buffer) (point) (point-max)))
                        ;;   ;; ;; (#s(org-ai--response role "assistant") #s(org-ai--response text "It seems ") #s(org-ai--response stop "length"))

                        (let* ((res1 (buffer-substring-no-properties (point) (point-max)))
                              (res2 (json-read-from-string res1))
                              (res3 (org-ai--normalize-response res2))
                              (res4 (nth 1 res3))
                              (res5 (org-ai--response-payload res4))
                              (res (decode-coding-string res5 'utf-8))
                              )
                          (print (list "HERE6" res3 ))
                          (print (list "HERE6h7" callback))
                          ;; to prevent catching error to here
                          (run-at-time 0 nil callback res)
                          )
                        ;; (funcall callback (decode-coding-string (org-ai--response-payload (nth 1
                        ;;                                                                        (org-ai--normalize-response
                        ;;                                                                         (json-read-from-string
                        ;;                                                                          (buffer-substring-no-properties (point) (point-max))))))
                        ;;                                         'utf-8))
                      (error
                       (message "HERE7 error"))
                       ;; (funcall callback nil)
                       )))))))
           ;; (timeout (or timeout org-ai-timers-duration))
           ;; (waiter (run-with-timer 3 0 (lambda(b) (print b)) b))
          )
      url-request-buffer)))

;; - Test!
;; (let ((service 'together)
;;       (model "meta-llama/Llama-3.3-70B-Instruct-Turbo-Free")
;;       (max-tokens 10)
;;       (temperature nil)
;;       (top-p nil)
;;       (frequency-penalty nil)
;;       (presence-penalty nil))
;;   (org-ai-timers--progress-reporter-run
;;    1
;;    (lambda (buf) (org-ai-timers--interrupt-current-request buf #'org-ai-openai--interrupt-url-request))
;;    (org-ai-api-request-llm service model (lambda (result)
;;                                            (org-ai-timers--interrupt-current-request (current-buffer) #'org-ai-openai--stop-tracking-url-request)
;;                                            (print (list "hay" result)))
;;                            :timeout 20
;;                            :messages  (vector (list :role 'system :content "You a helpful.")
;;                                               (list :role 'user :content "How to do staff?"))
;;                            :max-tokens max-tokens
;;                            :temperature temperature
;;                            :top-p top-p
;;                            :frequency-penalty frequency-penalty
;;                            :presence-penalty presence-penalty)))

(defvar org-ai-api-request-llm-retries-local-url-buffer  nil)
(make-variable-buffer-local 'org-ai-api-request-llm-retries-local-url-buffer)

(cl-defun org-ai-api-request-llm-retries (service model timeout callback &optional &key retries prompt messages header-marker max-tokens temperature top-p frequency-penalty presence-penalty)
  "Add TIMEOUT and RETRIES parameters to `org-ai-api-request-llm' functiion.
Timer function accept left attempts count.
Timer function restart requst and restart timer with attempts-1.
In callback we add cancel timer function."
  (org-ai--debug "org-ai-api-request-llm-retries1 timeout %s" timeout)
  (let ((cb (current-buffer)))
    (with-temp-buffer ; to separate variable `org-ai-api-request-llm-retries-local-url-buffer'
      (org-ai--debug "org-ai-api-request-llm-retries12")
      (let* ((left-retries (if retries (1- retries)
                             ;; else
                             3))
             (timer (run-with-timer timeout
                                    0
                                    (lambda ()
                                      (org-ai--debug "in org-ai-api-request-llm-retries timer %s" left-retries)
                                      ;; - kill old
                                      (when (buffer-live-p org-ai-api-request-llm-retries-local-url-buffer)
                                        (org-ai-timers--interrupt-current-request org-ai-api-request-llm-retries-local-url-buffer #'org-ai-openai--interrupt-url-request))
                                      ;; - restart
                                      (if (> left-retries 0)
                                          (org-ai-api-request-llm-retries service model timeout callback
                                                                          left-retries prompt messages header-marker max-tokens temperature top-p frequency-penalty presence-penalty))
                                      ))))
        (org-ai--debug "org-ai-api-request-llm-retries2")
        (setq org-ai-api-request-llm-retries-local-url-buffer
              (org-ai-api-request-llm service model
                                      (lambda (result-llm-retries)
                                        (org-ai--debug "org-ai-api-request-llm callback1, timer, result:" timer result-llm-retries)
                                        (if timer
                                            (cancel-timer timer))
                                        (org-ai--debug "org-ai-api-request-llm  callback2")
                                        ;; (with-current-buffer cb
                                        (org-ai--debug "org-ai-api-request-llm  callback3 %s %s" callback result-llm-retries)
                                        (run-at-time 0 nil callback result-llm-retries)
                                        ;; (funcall callback result-llm-retries)
                                        (org-ai--debug "org-ai-api-request-llm  callback4")
                                        ;; )
                                      )
                                      :prompt prompt
                                      :messages  messages
                                      :max-tokens max-tokens
                                      :temperature temperature
                                      :top-p top-p
                                      :frequency-penalty frequency-penalty
                                      :presence-penalty presence-penalty))
        ;; save url-buffer
        (org-ai-timers--set org-ai-api-request-llm-retries-local-url-buffer
                            header-marker)
        (org-ai--debug "org-ai-api-request-llm-retries3" org-ai-timers--element-marker-variable-dict org-ai-api-request-llm-retries-local-url-buffer)
        ))))

(defun org-ai--maybe-show-openai-request-error ()
  "If the API request returned an error, show it.
`REQUEST-BUFFER' is the buffer containing the request.
Return t if error happen, otherwise nil"
  (when (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
    (goto-char url-http-end-of-headers))
  (condition-case nil
      (when-let* ((body (json-read))
                  (err (or (alist-get 'error body)
                           (plist-get body 'error)))
                  (message (or (alist-get 'message err)
                               (plist-get err 'message)))
                  (message (if (and message (not (string-blank-p message)))
                               message
                             (json-encode err))))
        (org-ai--show-error message))
    (error nil)))

(defun org-ai--show-error (error-message)
  "Show an error message in a buffer.
`ERROR-MESSAGE' is the error message to show."
  (condition-case nil
      (let ((buf (get-buffer-create "*org-ai error*")))
        (with-current-buffer buf
          (read-only-mode -1)
          (erase-buffer)
          (insert "Error from the service API:\n\n")
          (insert error-message)
          (display-buffer buf)
          (goto-char (point-min))
          (toggle-truncate-lines -1)
          (read-only-mode 1)
          ;; close buffer when q is pressed
          (local-set-key (kbd "q") (lambda () (interactive) (kill-buffer)))
          t))
    (error nil)))

(cl-defun org-ai--payload (&optional &key service model prompt messages max-tokens temperature top-p frequency-penalty presence-penalty stream)
  "Create the payload for the OpenAI API.
`PROMPT' is the query for completions `MESSAGES' is the query for
chatgpt. `MODEL' is the model to use. `MAX-TOKENS' is the
maximum number of tokens to generate. `TEMPERATURE' is the
temperature of the distribution. `TOP-P' is the top-p value.
`FREQUENCY-PENALTY' is the frequency penalty. `PRESENCE-PENALTY'
is the presence penalty.
`STREAM' is a boolean indicating whether to stream the response."
  (let ((extra-system-prompt)
        (max-completion-tokens))

    (when (eq service 'anthropic)
      (when (string-equal (plist-get (aref messages 0) :role) "system")
        (setq extra-system-prompt (plist-get (aref messages 0) :content))
        (cl-shiftf messages (cl-subseq messages 1)))
      (setq max-tokens (or max-tokens 4096)))

    ;; o1 models currently does not support system prompt
    (when (and (or (eq service 'openai) (eq service 'azure-openai))
               (or (string-prefix-p "o1" model) (string-prefix-p "o3" model)))
      (setq messages (cl-remove-if (lambda (msg) (string-equal (plist-get msg :role) "system")) messages))
      ;; o1 does not support max-tokens
      (when max-tokens
        (setq max-tokens nil)
        (setq max-completion-tokens (or max-tokens 128000))))

   (let* ((input (if messages `(messages . ,messages) `(prompt . ,prompt)))
          ;; TODO yet unsupported properties: n, stop, logit_bias, user
          (data (map-filter (lambda (x _) x)
                            `(,input
                              (model . ,model)
                              ,@(when stream                `((stream . ,stream)))
                              ,@(when max-tokens            `((max_tokens . ,max-tokens)))
                              ,@(when max-completion-tokens `((max-completion-tokens . ,max-completion-tokens)))
                              ,@(when temperature           `((temperature . ,temperature)))
                              ,@(when top-p                 `((top_p . ,top-p)))
                              ,@(when frequency-penalty     `((frequency_penalty . ,frequency-penalty)))
                              ,@(when presence-penalty      `((presence_penalty . ,presence-penalty)))))))

     (when extra-system-prompt
       (setq data (append data `((system . ,extra-system-prompt)))))

     (encode-coding-string (json-encode data) 'utf-8))))

(defun org-ai--url-request-on-change-function (_beg _end _len)
  "First function that read url-request buffer and extracts JSON stream responses.
Three arguments are passed to each function: the positions of
the beginning and end of the range of changed text,
and the length in chars of the pre-change text replaced by that range.
Call `org-ai--current-url-request-callback' with data.
After processing call `org-ai--current-url-request-callback' with nil.
This  callback  here  is `org-ai--insert-stream-response'  for  chat  or
`org-ai--insert-single-response' for completion.
Called within url-retrieve buffer."
  ;; (org-ai--debug "org-ai--url-request-on-change-function: %s %s %s %s" _beg _end _len (current-buffer))
  ;; (with-current-buffer org-ai--last-url-request-buffer
  (when (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
    (save-match-data
      (save-excursion
        (if org-ai--url-buffer-last-position-marker
            (goto-char org-ai--url-buffer-last-position-marker)
          ;; else
          (goto-char url-http-end-of-headers)
          (setq org-ai--url-buffer-last-position-marker (point-marker)))

          ;; Avoid a bug where we skip responses because url has modified the http
          ;; buffer and we are not where we think we are.
          ;; TODO this might break
          (unless (eolp)
            (beginning-of-line))

          ;; - Non-streamed - response of a single json object
          (if (not org-ai--current-request-is-streamed)
              (let ((json-object-type 'plist)
                    (json-key-type 'symbol)
                    (json-array-type 'vector))
                (condition-case _err
                    (let ( ; error
                          (data (json-read-from-string
                                 (buffer-substring-no-properties (point) (point-max))))
                          ;; (data (json-read))  ; problem: with codepage, becaseu url buffer not utf-8
                          )
                      (when data
                        (org-ai--debug "on change 1)")
                        (funcall org-ai--current-url-request-callback data) ; INSERT CALLBACK!
                        ;; We call this in lambda "url-request-buffer event" anyway
                        ;; (org-ai-timers--interrupt-current-request (current-buffer) #'org-ai-openai--stop-tracking-url-request)
                        ))
                  (error
                   nil
                   )
                  )
                ;; - Done or Error
                (org-ai--debug "on change 2)")
                (funcall org-ai--current-url-request-callback nil) ; INSERT CALLBACK!
                (message "org-ai request done"))

            ;; - else - streamed, multiple json objects prefixed with "data: "
            ;; (org-ai--debug "org-ai--url-request-on-change-function 2.1) %s %s" (point) (eolp))
            (let ((errored nil))
              (while (and (not errored)
                          (search-forward "data: " nil t))
                (let ((line (buffer-substring-no-properties (point) (line-end-position)))
                      (tmp-buf "*org-ai--temp*"))
                  ;; (org-ai--debug "on change 2.2) line: %s" line)
                  ;; (message "...found data: %s" line)
                  (if (not (string= line "[DONE]"))
                      (let ((json-object-type 'plist)
                            (json-key-type 'symbol)
                            (json-array-type 'vector)
                            data)
                        ;; (data (json-read-from-string line)) ; slow
                        (setq data (with-current-buffer (get-buffer-create tmp-buf t) ; faster
                                     (condition-case _err
                                         (progn
                                           (erase-buffer)
                                           (insert line)
                                           (goto-char (point-min))
                                           (json-read))
                                       (error
                                        (org-ai--debug "org-ai--url-request-on-change-function 2.3) errored")
                                        (setq errored t)
                                        (kill-buffer)
                                        nil))))
                        ;; (setq org-ai--debug-data (append org-ai--debug-data (list data)))
                        (when data
                          (end-of-line)
                          (set-marker org-ai--url-buffer-last-position-marker (point))
                          ;; (org-ai--debug (format "on change 3) %s" org-ai--url-buffer-last-position-marker))
                          (funcall org-ai--current-url-request-callback data) ; INSERT CALLBACK!
                          ))

                    ;; - else "[DONE]" string found
                    (progn
                      ;; (end-of-line)
                      (set-marker org-ai--url-buffer-last-position-marker (point))
                      ;; (setq org-ai--url-buffer-last-position-marker nil)
                      (org-ai-timers--interrupt-current-request (current-buffer) #'org-ai-openai--stop-tracking-url-request) ; stop timer
                      ;; (org-ai-timers--interrupt-current-request (current-buffer) #'org-ai-openai--interrupt-url-request) ; stop timer
                      (if (get-buffer tmp-buf)
                          (kill-buffer tmp-buf))
                      ;; (org-ai--debug "on change 4)")
                      (funcall org-ai--current-url-request-callback nil) ; INSERT CALLBACK!
                      ;; (org-ai-reset-stream-state)
                      (message "org-ai request done"))
                    )))))
          ;; (goto-char p) ; additional protection
          ;; (org-ai--debug "org-ai--url-request-on-change-function end")
          ))))

(defun org-ai--stream-supported (service model)
  "Check if the stream is supported by the service and model.
`SERVICE' is the service to use. `MODEL' is the model to use."
  ;; stream not supported by openai o1 models
  (not (and (or (eq service 'openai) (eq service 'azure-openai))
            (or
             (string-prefix-p "o1-pro" model)))))

;; (defun org-ai--kill-query-process ()
;;   (let ((proc (get-buffer-process org-ai--last-url-request-buffer)))
;;     (set-process-sentinel proc 'ignore)
;;     (delete-process proc)))


;; (defun org-ai-reset-url-state ()

;; (defun org-ai-reset-stream-state ()
;;   "Reset the stream state.
;; Should be called within url-retrieve buffer. Danger function."
;;   (interactive)
;;   (setq org-ai--current-url-request-callback nil)
;;   (setq org-ai--url-buffer-last-position-marker nil)
;;   (setq org-ai--current-chat-role nil)
;;   (setq org-ai--current-request-is-streamed nil)
;;   (org-ai--progress-reporter-global-cancel (current-buffer))
;;   )

;;; - Reporter & requests interrupt functions
;; 1) `org-ai-timers--progress-reporter-run' - start global timer
;; 2) `org-ai-timers--interrupt-current-request' - interrupt, called to stop tracking on-changes or kill buffer
;; When  we kill  one buffer  and if  no others  we report  failure or
;; success  if there  are  other  we just  continue  and don't  change
;; reporter.
;; Functions:
;; Failure by time:
;; `org-ai-openai-stop-all-url-requests' 'failed
;; `org-ai-timers--interrupt-current-request'
;; Success:
;; `org-ai-timers--interrupt-current-request'
;; Interactive:
;; `org-ai-openai-interrupt-url-request'
;; Variables:
;; - `org-ai--global-progress-reporter' - lambda that return a string,
;; - `org-ai--global-progress-timer' - timer that output /-\ to echo area.
;; - `org-ai--global-progress-timer-remaining-ticks'.
;; - `org-ai--current-progress-timer' - count life of url buffer,
;; - `org-ai--current-progress-timer-remaining-ticks'."

(defun org-ai-openai--interrupt-url-request (url-buffer)
  "Remove on-update hook and kill buffer.
Called from `org-ai-openai-stop-url-request',
`org-ai-openai-stop-all-url-requests'."
  (org-ai--debug "org-ai-openai--interrupt-url-request"
                 (eq (current-buffer) url-buffer)
                 (buffer-live-p url-buffer))
  (if (eq (current-buffer) url-buffer)
      (progn
        (remove-hook 'after-change-functions #'org-ai--url-request-on-change-function t)
        (when (buffer-live-p url-buffer)
          (let (kill-buffer-query-functions)
            (kill-buffer url-buffer))))
    ;; else
    (when (and url-buffer (buffer-live-p url-buffer))
      (with-current-buffer url-buffer
        (remove-hook 'after-change-functions #'org-ai--url-request-on-change-function t)
        (let (kill-buffer-query-functions) ; set to nil
          (kill-buffer url-buffer))))))

(defun org-ai-openai--stop-tracking-url-request (url-buffer)
  "Remove on-update hook and not kill buffer.
  Called from `org-ai-openai-stop-url-request',
`org-ai-openai-stop-all-url-requests'."
  (org-ai--debug "org-ai-openai--stop-tracking-url-request"
                 (eq (current-buffer) url-buffer)
                 (buffer-live-p url-buffer))
  (if (eq (current-buffer) url-buffer)
      (remove-hook 'after-change-functions #'org-ai--url-request-on-change-function t)
    ;; else
    (if (and url-buffer (buffer-live-p url-buffer))
      (with-current-buffer url-buffer
        (remove-hook 'after-change-functions #'org-ai--url-request-on-change-function t)))))

(cl-defun org-ai-openai-stop-url-request (&optional &key element url-buffer failed)
  "Interrupt the request for ELEMENT or URL-BUFFER.
If  no ELEMENT  or URL-BUFFER  provided we  use in  current ai  block at
current position at current buffer.
Return t if buffer was found.
Called from `org-ai-timers--progress-reporter-run'."
  (interactive)
  (org-ai--debug "org-ai-openai-stop-url-request, current buffer %s, url-buf %s, element %s"
                 (current-buffer) url-buffer
                 (org-ai-block-p))
  (if-let* ((element (or element (org-ai-block-p)))
            (url-buffers (if url-buffer
                            (list url-buffer)
                          ;; else
                          (org-ai-timers--get-keys-for-variable (org-ai-block-get-header-marker element)))))
        (progn
          (org-ai--debug "org-ai-openai-stop-url-request, element %s, url-buffers %s"
                         element
                         url-buffers)
          (org-ai-timers--interrupt-current-request url-buffers #'org-ai-openai--interrupt-url-request failed)
          t)
        ;; ;; else - called not at from some block, but from elsewhere
        (org-ai--debug "org-ai-openai-stop-url-request all %s" (org-ai-timers--get-keys-for-variable (org-ai-block-get-header-marker element)))
        (org-ai-openai-stop-all-url-requests) ; kill all
      ;; else
      ))

;;;###autoload
(cl-defun org-ai-openai-stop-all-url-requests (&optional &key failed)
  "Return t if buffer was found."
  (interactive)
  (let ((buffers (org-ai-timers--get-all-keys)))
    (org-ai--debug "org-ai-openai-stop-all-url-request" buffers)
    (when buffers
      (setq org-ai-timers--element-marker-variable-dict nil)
      (org-ai-timers--interrupt-current-request buffers #'org-ai-openai--interrupt-url-request failed)
      t)))



;;; - Others

(defun org-ai-openai--collect-chat-messages (content-string &optional default-system-prompt persistant-sys-prompts max-token-recommendation)
  "Takes `CONTENT-STRING' and splits it by [SYS]:, [ME]:, [AI]: and [AI_REASON]: markers.
If `PERSISTANT-SYS-PROMPTS' is non-nil, [SYS] prompts are
intercalated. The [SYS] prompt used is either
`DEFAULT-SYSTEM-PROMPT', may be nil to disable, or the first [SYS]
prompt found in `CONTENT-STRING'."
  (if max-token-recommendation
      (setq default-system-prompt (concat default-system-prompt
                                          (if default-system-prompt " ")
                                          max-token-recommendation)))
  (with-temp-buffer
    (erase-buffer)
    (insert content-string)
    (goto-char (point-min))

    (let* (;; collect all positions before [ME]: and [AI]:
           (sections (cl-loop while (search-forward-regexp "\\[SYS\\]:\\|\\[ME\\]:\\|\\[ME:\\]\\|\\[AI\\]:\\|\\[AI_REASON\\]:" nil t)
                              collect (save-excursion
                                        (goto-char (match-beginning 0))
                                        (point))))

           ;; make sure we have from the beginning if there is no first marker
           (sections (if (not sections)
                         (list (point-min))
                       (if (not (= (car sections) (point-min)))
                           (cons (point-min) sections)
                         sections)))

           (parts (cl-loop for (start end) on sections by #'cdr
                           collect (string-trim (buffer-substring-no-properties start (or end (point-max))))))

           ;; if no role is specified, assume [ME]
           (parts (if (and
                       (not (string-prefix-p "[SYS]:" (car parts)))
                       (not (string-prefix-p "[ME]:" (car parts)))
                       (not (string-prefix-p "[ME:" (car parts)))
                       (not (string-prefix-p "[AI]:" (car parts)))
                       (not (string-prefix-p "[AI_REASON]:" (car parts))))
                      (progn (setf (car parts) (concat "[ME]: " (car parts)))
                             parts)
                    parts))

           ;; split part and create (list :role 'role :content string)
           (messages (cl-loop for part in parts
                              ;; Filter out reasoning parts as including them will cause 404
                              ;; https://api-docs.deepseek.com/guides/reasoning_model#multi-round-conversation
                              if (not (string= "[AI_REASON]" (car (split-string part ":"))))
                              collect (cl-destructuring-bind (type &rest content) (split-string part ":")
                                        (let ((type (string-trim type))
                                              (content (string-trim (string-join content ":"))))
                                          (if (string= type "[ME") ; typo by human
                                            (list :role 'user :content (string-trim (substring content 1)))
                                            ;; else
                                            (list :role (cond ((string= type "[SYS]") 'system)
                                                              ((string= type "[ME]") 'user)
                                                              ((string= type "[ME") 'user)
                                                              ((string= type "[AI]") 'assistant)
                                                              (t 'assistant))
                                                  :content content))))))

           (messages (cl-remove-if-not (lambda (x) (not (string-empty-p (plist-get x :content)))) messages))

           ;; merge messages with same role
           (messages (cl-loop with last-role = nil
                              with result = nil
                              for (_ role _ content) in messages
                              if (eql role last-role)
                              do (let ((last (pop result)))
                                   (push (list :role role :content (string-join (list (plist-get last :content) content) "\n")) result))
                              else
                              do (push (list :role role :content content) result)
                              do (setq last-role role)
                              finally return (reverse result)))

           ;; [SYS] in messages?
           (starts-with-sys-prompt-p (and messages (eql (plist-get (car messages) :role) 'system)))

           (sys-prompt (if starts-with-sys-prompt-p
                           (plist-get (car messages) :content)
                         ;; else
                         default-system-prompt))

           (messages (if persistant-sys-prompts
                         (cl-loop with result = nil
                                  for (_ role _ content) in messages
                                  if (eql role 'assistant)
                                  do (push (list :role 'assistant :content content) result)
                                  else if (eql role 'user)
                                  do (progn
                                       (push (list :role 'system :content sys-prompt) result)
                                       (push (list :role 'user :content content) result))
                                  finally return (reverse result))
                       (if (or starts-with-sys-prompt-p (not sys-prompt))
                           messages
                         (cons (list :role 'system :content sys-prompt) messages)))))

      (apply #'vector messages))))

;; deal with unspecified prefix
(cl-assert
 (equal
  (let ((test-string "\ntesting\n  [ME]: foo bar baz zorrk\nfoo\n[AI]: hello hello[ME]: "))
    (org-ai-openai--collect-chat-messages test-string))
  '[(:role user :content "testing\nfoo bar baz zorrk\nfoo")
    (:role assistant :content "hello hello")]))

;; sys prompt
(cl-assert
 (equal
  (let ((test-string "[SYS]: system\n[ME]: user\n[AI]: assistant"))
    (org-ai-openai--collect-chat-messages test-string))
  '[(:role system :content "system")
    (:role user :content "user")
    (:role assistant :content "assistant")]))

;; sys prompt intercalated
(cl-assert
 (equal
  (let ((test-string "[SYS]: system\n[ME]: user\n[AI]: assistant\n[ME]: user"))
    (org-ai-openai--collect-chat-messages test-string nil t))
  '[(:role system :content "system")
    (:role user :content "user")
    (:role assistant :content "assistant")
    (:role system :content "system")
    (:role user :content "user")]))

;; merge messages with same role
(cl-assert
 (equal
  (let ((test-string "[ME]: hello [ME]: world")) (org-ai-openai--collect-chat-messages test-string))
  '[(:role user :content "hello\nworld")]))

(cl-assert
 (equal
  (let ((test-string "[ME:] hello world")) (org-ai-openai--collect-chat-messages test-string))
  '[(:role user :content "hello world")]))

(cl-assert
 (equal
  (let ((test-string "[ME]: hello [ME:] world")) (org-ai-openai--collect-chat-messages test-string))
  '[(:role user :content "hello\nworld")]))

;; (org-ai--normalize-response '(id "o3fA4D4-62bZhn-9617f44f6d399d91" object "chat.completion" created 1752904364 model "meta-llama/Llama-3.3-70B-Instruct-Turbo-Free" prompt [] choices [(finish_reason "stop" seed 819567834314233700 logprobs nil index 0 message (role "assistant" content "It works: `(2 3 1)` is returned." tool_calls []))] usage (prompt_tokens 131 completion_tokens 14 total_tokens 145 cached_tokens 0)))
;; (#s(org-ai--response role "assistant") #s(org-ai--response text "It works: `(2 3 1)` is returned.") #s(org-ai--response stop "stop"))
;; (org-ai--normalize-response '(id "o3f9cZv-4Yz4kd-9617f234fd6f9d91" object "chat.completion" created 1752904277 model "meta-llama/Llama-3.3-70B-Instruct-Turbo-Free" prompt [] choices [(finish_reason "stop" seed 589067420664432000 logprobs nil index 0 message (role "assistant" content "`(mapcar 'cdr '((a . 2) (x . 3) (2 . 1)))` returns `(2 3 1)`." tool_calls []))] usage (prompt_tokens 84 completion_tokens 34 total_tokens 118 cached_tokens 0)))
;; (#s(org-ai--response role "assistant") #s(org-ai--response text "`(mapcar 'cdr '((a . 2) (x . 3) (2 . 1)))` returns `(2 3 1)`.") #s(org-ai--response stop "stop"))
;; (org-ai--response-type response)
;; (comment
;;   (with-current-buffer "org-ai-mode-test.org"
;;    (org-ai-openai--collect-chat-messages (org-ai-block-get-content))))

;; - Not used now
(cl-defun org-ai-openai--stringify-chat-messages (messages &optional &key
                                                    default-system-prompt
                                                    (system-prefix "[SYS]: ")
                                                    (user-prefix "[ME]: ")
                                                    (assistant-prefix "[AI]: "))
  "Converts a chat message to a string.
`MESSAGES' is a vector of (:role :content) pairs. :role can be
'system, 'user or 'assistant. If `DEFAULT-SYSTEM-PROMPT' is
non-nil, a [SYS] prompt is prepended if the first message is not
a system message. `SYSTEM-PREFIX', `USER-PREFIX' and
`ASSISTANT-PREFIX' are the prefixes for the respective roles
inside the assembled prompt string."
  (let ((messages (if (and default-system-prompt
                           (not (eql (plist-get (aref messages 0) :role) 'system)))
                      (cl-concatenate 'vector (vector (list :role 'system :content default-system-prompt)) messages)
                    messages)))
    (cl-loop for (_ role _ content) across messages
             collect (cond ((eql role 'system) (concat system-prefix content))
                           ((eql role 'user) (concat user-prefix content))
                           ((eql role 'assistant) (concat assistant-prefix content)))
             into result
             finally return (string-join result "\n\n"))))

(cl-assert
 (equal
  (org-ai-openai--stringify-chat-messages '[(:role system :content "system")
                                     (:role user :content "user")
                                     (:role assistant :content "assistant")])
  "[SYS]: system\n\n[ME]: user\n\n[AI]: assistant"))

(cl-assert
 (equal
  (org-ai-openai--stringify-chat-messages '[(:role user :content "user")
                                     (:role assistant :content "assistant")]
                                   :default-system-prompt "system")
  "[SYS]: system\n\n[ME]: user\n\n[AI]: assistant"))

(cl-assert
 (equal
  (org-ai-openai--stringify-chat-messages '[(:role user :content "user")
                                     (:role assistant :content "assistant")]
                                   :user-prefix "You: "
                                   :assistant-prefix "Assistant: ")
  "You: user\n\nAssistant: assistant"))


(cl-assert
 (equal
  (let ((test-val '(id "o3fA4D4-62bZhn-9617f44f6d399d91" object "chat.completion" created 1752904364 model "meta-llama/Llama-3.3-70B-Instruct-Turbo-Free" prompt [] choices [(finish_reason "stop" seed 819567834314233700 logprobs nil index 0 message (role "assistant" content "It works: `(2 3 1)` is returned." tool_calls []))] usage (prompt_tokens 131 completion_tokens 14 total_tokens 145 cached_tokens 0))
                  ))
    (org-ai--normalize-response test-val))
  '(#s(org-ai--response role "assistant") #s(org-ai--response text "It works: `(2 3 1)` is returned.") #s(org-ai--response stop "stop"))))


(let* ((test-val '(#s(org-ai--response role "assistant") #s(org-ai--response text "It seems ") #s(org-ai--response stop "length")))
       (test-val0 (nth 0 test-val))
       (test-val1 (nth 1 test-val))
       )
  (and
   (equal (length test-val) 3)
   (equal (org-ai--response-type test-val0) 'role)
   (string-equal (decode-coding-string (org-ai--response-payload test-val0) 'utf-8) "assistant")
   (equal (org-ai--response-type test-val1) 'text)
   (string-equal (decode-coding-string (org-ai--response-payload test-val1) 'utf-8) "It seems ")
  ))



(defun org-ai-switch-chat-model ()
  "Change `org-ai-creds-chat-model'."
  (interactive)
  (let ((model (completing-read "Model: "
                                (append org-ai-chat-models
                                        '("claude-3-opus-latest" "claude-3-5-sonnet-latest" "claude-3-7-sonnet-latest"
                                          "gemini-2.5-pro-preview-03-25" "gemini-2.5-flash-preview-04-17" "gemini-2.0-flash" "gemini-2.0-pro-exp"
                                          "deepseek-chat" "deepseek-reasoner"))
                                nil t)))
    (setq org-ai-creds-chat-model model)))


(provide 'org-ai-openai)

;;; org-ai-openai.el ends here
