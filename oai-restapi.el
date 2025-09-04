;;; oai-restapi.el --- OpenAI REST API related functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Robert Krahn
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

;;; Changelog:
;; - DONE: comment redundent "org-ai-use-auth-source" variable
;; - DONE: add debug-switch "oai-debug-buffer"
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
;; - DONE: remove oai-block, org, org-element
;; - TODO: rename all functions to convention
;; - TODO: rename file to -api.el
;; - TODO: escame #+end_ai after insert of text in block
;; - TODO: add support for several backends, curl, request.el


;;; Commentary:

;; Get info block from #begin_ai and call url-retrieve.  Asynchronous
;; but only one call per buffer.
;;
;; Interface function: "org-ai-stream-completion".
;;
;; (org-ai-stream-completion service|model messages|prompt context)
;; -> (org-ai-stream-request service messages|prompt callback)
;; -> org-ai--get-headers, org-ai--get-endpoint, oai-restapi--payload, url-retrieve
;; - org-ai--get-headers = org-ai-api-creds-token = "api-key" or "x-api-key" or "Authorization"
;; - org-ai--get-endpoint = hardcoded URL or oai-restapi-con-chat-endpoint, oai-restapi-con-completion-endpoint, org-ai-google-chat-endpoint
;; -> callback: (oai-restapi--insert-stream-response) or (oai-restapi--insert-single-response)
;;
;; Main variables:
;; URL = org-ai--get-endpoint()  or oai-restapi-con-chat-endpoint, oai-restapi-con-completion-endpoint, org-ai-google-chat-endpoint
;; Headers = org-ai--get-headers
;; Token = org-ai-api-creds-token
;;
;; When we create request we count requests, create two timers global one and local inside url buffer.
;; When we receive error or final answer we stop local, recount requests and update global.
;;
;; Chat mode
;; - :message (oai-restapi--collect-chat-messages ...)
;; - (oai-restapi--normalize-response response) -> (cl-loop for response in normalized
;;   - (setq role (org-ai--response-type response))
;;   - (setq text (decode-coding-string (org-ai--response-payload response)) 'utf-8)
;; Completion mode
;; - :prompt content-string
;; - (setq text  (decode-coding-string (oai-restapi--get-single-response-text result) 'utf-8))
;;
;; How requests forced to stop with C-g?

;; We save url-buffer with header marker with
;; `oai-timers--progress-reporter-run' function.  that call:
;; (oai-timers--set-variable url-buffer header-marker) in
;; `oai-timers--interrupt-current-request' we remove buffer from
;; saved and call (oai-restapi--interrupt-url-request url-buffer).

;;; Code:
;;; -=-= Constants, variables
(require 'org)
(require 'org-element)
(require 'url)
(require 'url-http)
(require 'cl-lib)
(require 'gv)
(require 'json)
(require 'oai-block-tags)
(require 'oai-block)
(require 'oai-timers)
(require 'oai-debug)

(defcustom oai-restapi-jump-to-end-of-block t
  "If non-nil, jump to the end of the block after inserting the completion."
  :type 'boolean
  :group 'oai)

(defcustom oai-restapi-auto-fill nil
  "If non-nil, will fill paragraphs when inserting completions."
  :type 'boolean
  :group 'oai)

(defcustom oai-restapi-con-service 'openai
  "Service to use if not specified."
  :type '(choice (const :tag "OpenAI" openai)
                 (const :tag "Azure-OpenAI" azure-openai)
                 (const :tag "perplexity.ai" perplexity.ai)
                 (const :tag "anthropic" anthropic)
                 (const :tag "DeepSeek" deepseek)
                 (const :tag "google" google)
                 (const :tag "Together" together)
                 (const :tag "Github" github))
  :group 'oai)

(defcustom oai-restapi-con-endpoints
  '(:openai		"https://api.openai.com/v1/chat/completions"
    :openai-completion	"https://api.openai.com/v1/completions"
    :perplexity.ai	"https://api.perplexity.ai/chat/completions"
    :deepseek		"https://api.deepseek.com/v1/chat/completions"
    :anthropic		"https://api.anthropic.com/v1/messages"
    :google		"https://generativelanguage.googleapis.com/v1beta/openai/chat/completions"
    :together		"https://api.together.xyz/v1/chat/completions"
    :github		"https://models.github.ai/inference/chat/completions"
    )
  "Endpoints for services.
  This is a not ordered list of key-value pairs in format of List of
  lists: (SYMBOL VALUE-STRING). Used for POST HTTP request to service.
To add service use: (plist-put oai-restapi-con-endpoints :myservice \"http\")."
  :type '(plist :key-type symbol :value-type string
                :tag "Plist with Service as a symbol key and Endpoint URL as a string value")
         :group 'oai)


(defcustom oai-restapi-con-token nil
  "This is your OpenAI API token.
If not-nil, store  token as a string  or may be as a list of key-value:
'(:openai \"token\").
You can retrieve it at
https://platform.openai.com/account/api-keys.
If  nil, `auth-sources'  file  (with encryption  support)  used to  read
token.  In such  case the secret should be stored  in the format:
machine openai password <your token>
or
machine openai--0 password <your token>
machine openai--1 password <your token>"
  :type '(choice (string :tag "String value")
                 (plist :key-type symbol :value-type (choice (string :tag "Plist with symbol key and string value")
                                                             (list :tag "Plist with symbol key and list of string values")))
                 (const :tag "Use auth-source." nil))
  :type 'string
  :group 'oai)

(defcustom oai-restapi-con-model "gpt-4o-mini"
  "The default model to use.
See https://platform.openai.com/docs/models for other options.
If mode is not chat but completion, appropriate model should be set."
  :type '(choice (string :tag "String value")
                  (plist :key-type symbol :value-type string :tag "Plist with symbol key and string value"))
  :group 'oai)

;; (defcustom org-ai-use-auth-source t
;;   "If non-nil, use auth-source to retrieve the OpenAI API token.
;; The secret should be stored in the format
;;   machine api.openai.com login org-ai password <your token>
;; in the `auth-sources' file."
;;   :type 'boolean
;;   :group 'oai)

;; (defcustom org-ai-creds-completion-model "text-davinci-003"
;;   "The default model to use for completion requests.  See https://platform.openai.com/docs/models for other options."
;;   :type '(choice (string :tag "String value")
;;                   (plist :key-type symbol :value-type string :tag "Plist with symbol key and string value"))
;;   :group 'oai)


(defcustom oai-restapi-openai-known-chat-models '("gpt-4o-mini"
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
  "Alist of OpenAI chat models from https://platform.openai.com/docs/models."
  :type '(alist :key-type string :value-type string)
  :group 'oai)

(defcustom oai-restapi-default-max-tokens nil
  "The default maximum number of tokens to generate.  This is what costs money."
  :type 'string
  :group 'oai)

(defcustom oai-restapi-add-max-tokens-recommendation t
  "Additionally add system recomendation for chat mode about limit.
Function `oai-restapi--get-lenght-recommendation' is used to create
prompt."
  :type 'boolean
  :group 'oai)

(defcustom oai-restapi-default-chat-system-prompt "Be helpful."
  "The system message helps set the behavior of the assistant:
https://platform.openai.com/docs/guides/chat/introduction.  This
default prompt is send as the first message before any user (ME)
or assistant (AI) messages.  Inside a +#begin_ai...#+end_ai block
you can override it with: '[SYS]: <your prompt>'."
  :type 'string
  :group 'oai)

(defcustom oai-restapi-default-inject-sys-prompt-for-all-messages nil
  "Wether to add the `oai-restapi-default-chat-system-prompt' before all user messages.

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
  :group 'oai)

(make-obsolete-variable 'oai-restapi-default-inject-sys-prompt-for-all-messages
                        "With newer ChatGPT versions this is no longer necessary."
                        "2023-12-26")


;; Azure-Openai specific variables

(defcustom oai-restapi-azure-openai-api-base "https://your-instance.openai.azure.com"
  "Base API URL for Azure-OpenAI."
  :type 'string
  :group 'oai)

;; Additional Azure-Openai specific variables
(defcustom oai-restapi-azure-openai-deployment "azure-openai-deployment-name"
  "Deployment name for Azure-OpenAI API."
  :type 'string
  :group 'oai)

(defcustom oai-restapi-azure-openai-api-version "2023-07-01-preview"
  "API version for Azure-OpenAI."
  :type 'string
  :group 'oai)

(defcustom oai-restapi-anthropic-api-version "2023-06-01"
  "API version for api.anthropic.com."
  :type 'string
  :group 'oai)

(defcustom oai-restapi-show-error-in-result t
  "If non-nil, show error in #+RESULTS of block, otherwise in a buffer."
  :type 'boolean
  :group 'oai)

;; (defvar org-ai--last-url-request-buffer nil
;;   "Internal var that stores the current request buffer.
;; For stream responses.
;; May be shown for debugging.")
;; (make-variable-buffer-local 'org-ai--last-url-request-buffer)

;; (defvar org-ai--current-request-buffer nil
;;   "Internal var that stores the current request buffer.
;; For chat completion responses.")

(defvar oai-restapi--current-url-request-callback nil
  "Internal var that stores the current request callback.
Called within url request buffer, should know about target position,
that is why defined as lambda with marker.")
(make-variable-buffer-local 'oai-restapi--current-url-request-callback)

(defvar oai-restapi--current-request-is-streamed nil
  "Whether we expect a streamed response or a single completion payload.")
(make-variable-buffer-local 'oai-restapi--current-request-is-streamed)


(defvar oai-restapi-after-chat-insertion-hook nil
  "Hook that is called when a chat response is inserted.
Note this is called for every stream response so it will typically only
contain fragments.  First argument is
`TYPE' - simbol 'role, 'text or'end,
second - text or role name,
third - position before text insertion
fourth - target buffer with ai block for third position.")

(defvar oai-restapi--current-insert-position-marker nil
  "Where to insert the result.")
(make-variable-buffer-local 'oai-restapi--current-insert-position-marker)

(defvar oai-restapi--current-chat-role nil
  "During chat response streaming, this holds the role of the \"current speaker\".
Used for hook only.")
(make-variable-buffer-local 'oai-restapi--current-chat-role)

(defvar oai-restapi--currently-chat-got-first-response nil)
(make-variable-buffer-local 'oai-restapi--currently-chat-got-first-response)

(defvar oai-restapi--currently-inside-code-markers nil
  "For If code block received apply `fill-paragraph'.")
(make-variable-buffer-local 'oai-restapi--currently-inside-code-markers)

(defvar oai-restapi--currently-reasoning nil)
(make-variable-buffer-local 'oai-restapi--currently-reasoning)

(defvar oai-restapi--url-buffer-last-position-marker nil
  "Local buffer var to store last read position.")
(make-variable-buffer-local 'oai-restapi--url-buffer-last-position-marker)
;; (makunbound 'oai-restapi--url-buffer-last-position-marker)

(cl-deftype oai-restapi--response-type ()
  '(member role text stop error))

(cl-defstruct oai-restapi--response
  (type (:type oai-restapi--response-type))
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

;;; -=-= debugging
(defun oai-restapi--prettify-json-string (json-string)
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



(defun oai-restapi--debug-urllib (source-buf)
  "Copy `url-http' buffer with response to our debugging buffer.
Argument SOURCE-BUF url-http response buffer."
  (when (and source-buf (bound-and-true-p oai-debug-buffer))
    (save-excursion
      (let* ((buf-exist (get-buffer oai-debug-buffer))
             (bu (or buf-exist (get-buffer-create oai-debug-buffer))))
        (with-current-buffer bu
          (let ((stri (with-current-buffer source-buf
                        ;; (save-excursion
                          (buffer-substring-no-properties (or oai-restapi--url-buffer-last-position-marker
                                                              (point-min))
                                                          (point-max)))))
            (goto-char (point-max))
            (insert "url-buf response:\n")
            (insert stri)
            (newline))))
      )))

;;; -=-= Get constant functions
(defun oai-restapi--check-model (model endpoint)
  "Check if the model name is somehow mistyped.
`MODEL' is the model name.  `ENDPOINT' is the API endpoint."
  (unless model
    (error "No oai model specified."))

  (when (or (string-match-p "api.openai.com" endpoint)
            (string-match-p "openai.azure.com" endpoint))

    (let ((lowercased (downcase model)))
      (when (and (string-prefix-p "gpt-" model) (not (string-equal lowercased model)))
        (warn "Model name '%s' should be lowercase. Use '%s' instead." model lowercased)))

    (unless (member model oai-restapi-openai-known-chat-models)
      (message "Model '%s' is not in the list of available models. Maybe this is because of a typo or maybe we haven't yet added it to the list. To disable this message add (add-to-list 'oai-restapi-openai-known-chat-models \"%s\") to your init file." model model))))

(defun oai-restapi--split-dash-number (str)
  (pcase-let ((`(,a ,b) (split-string str "--")))
    (and b (string-match-p "\\`[0-9]+\\'" b)
         (cons a (string-to-number b)))))

;; (oai-restapi--split-dash-number nil)         ;; error
;; (oai-restapi--split-dash-number "foo")       ;; nil
;; (oai-restapi--split-dash-number "foo--")     ;; nil
;; (oai-restapi--split-dash-number "foo--23")   ;; ("foo" . 23)
;; (oai-restapi--split-dash-number "--1")       ;; ("" . 1)
;; (oai-restapi--split-dash-number "a--b")      ;; nil
;; (oai-restapi--split-dash-number "foo--2.4")  ;; nil

(defun oai-restapi--openai-service-clear-dashes (service)
  "Remove --N part from SERVICE if it have such.
SERVICE is a string."
  (let ((spl (oai-restapi--split-dash-number service)))
    (if spl (car spl)
      service)))

(defmacro oai-restapi--get-value-or-string (var key)
  "Retrieve value from VAR using KEY if VAR is a plist, or return VAR if it's a string.
VAR is the variable name to evaluate.
KEY is a string (without :) or keyword (leading with :) used as a key if
VAR is a plist."
  `(cond ((plistp ,var)
          (plist-get ,var (if (stringp ,key)
                              (intern (concat ":" ,key))
                            ,key)))
          ((stringp ,var)
           ,var)
          (t nil)))

;; (oai-restapi--get-value-or-string oai-restapi-con-endpoints "openai")
;; (oai-restapi--get-value-or-string oai-restapi-con-endpoints :openai)

(defun oai-restapi--get-token (service)
  "Try to get the openai token.
Either from `oai-restapi-con-token' or from auth-source.
Optional argument SERVICE of token.
Never return nil, signal error if token not found"
  (cond
   ;; one token
   ((and (stringp oai-restapi-con-token)
         (not (string-empty-p oai-restapi-con-token)))
    oai-restapi-con-token)
   ;; several tokens
   ((plistp oai-restapi-con-token)
    ;; if service is "openai--N"
    (let* ((spl (if (stringp service) (oai-restapi--split-dash-number service)))
           (service-number (if spl (cdr spl)))
           (service (if spl (car spl) service))
           tokens)
      ;; find service in plist
      (setq tokens (oai-restapi--get-value-or-string oai-restapi-con-token service))
      (cond ((stringp tokens) tokens)
            ;; servie “foo--0” we get first from tokens
            ((and spl (listp tokens) service-number)
             (nth service-number tokens))
            ;; service have “foo”, we just get first from tokens.
            ((and tokens (listp tokens) (not spl) (stringp service))
             (car tokens))
            (t
             (error "Token not found in defined plist `oai-restapi-con-token'.")))))
   ((and
     (or (not oai-restapi-con-token) (string-empty-p oai-restapi-con-token))
     (oai-restapi--get-token-auth-source service)))
   (t
    (error "Please set `oai-restapi-con-token' to your OpenAI API token or setup auth-source (see oai readme)"))))


;; (defun org-ai--openai-get-chat-model (service)
;;   "Allow to set default model as one string or per service."

;; )
;; (defun org-ai--openai-get-completion-model (service)
;;   "Allow to set default model as one string or per service."

;; )

(defun oai-restapi--strip-api-url (url)
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

(defun oai-restapi--get-token-auth-source (&optional service)
  "Retrieves the authentication token for the OpenAI SERVICE using auth-source."
  (require 'auth-source)
  (let ((service (or service oai-restapi-con-service)))
    (or (and (stringp service) (auth-source-pick-first-password :host (oai-restapi--openai-service-clear-dashes service)))
        (auth-source-pick-first-password :host service)
        (and (stringp service) (auth-source-pick-first-password :host (concat service "--0" ))))))


(defun oai-restapi--get-endpoint (messages &optional service)
  "Determine the correct endpoint based on the service and
whether messages are provided."
  (let* ((service (or (if service
                          (oai-restapi--openai-service-clear-dashes service))
                      (oai-restapi-con-service)))
         (endpoint (oai-restapi--get-value-or-string oai-restapi-con-endpoints service)))
    (cond
     (endpoint endpoint)
     ((eq service 'azure-openai)
      (format "%s/openai/deployments/%s%s/completions?api-version=%s"
              oai-restapi-azure-openai-api-base oai-restapi-azure-openai-deployment
              (if messages "/chat" "") oai-restapi-azure-openai-api-version))
     (t
      (if messages (oai-restapi--get-value-or-string oai-restapi-con-endpoints :openai)
        (oai-restapi--get-value-or-string oai-restapi-con-endpoints :openai-completion))))))

(defun oai-restapi--get-headers (service)
  "Determine the correct headers based on the service."
  (let ((serv (if service
                  (oai-restapi--openai-service-clear-dashes service)
              oai-restapi-con-service))
        (token (oai-restapi--get-token service)))
    `(("Content-Type" . "application/json")
      ,@(cond
        ((eq serv 'azure-openai)
         `(("api-key" . ,token)))
        ((eq serv 'anthropic)
         `(("x-api-key" . ,token)
           ("anthropic-version" . ,oai-restapi-anthropic-api-version)))
        ((eq serv 'google)
         `(("Accept-Encoding" . "identity")
           ("Authorization" . ,(encode-coding-string (string-join `("Bearer" ,token) " ") 'utf-8))))
        (t
         `(("Authorization" . ,(encode-coding-string (string-join `("Bearer" ,token) " ") 'utf-8))))))))


(defun oai-restapi--get-lenght-recommendation (max-tokens)
  "Recomendation to limit yourself.
- words = tokens * 0.75
- tokens = words * 1.33333
- token = 4 characters
- word - 5 characters
- sentence - 15-25 words = 20 words = 26 tokens (tech/academ larger)
- paragraph - 6 sentences, 500-750 characters, 150-300 words = 150 words = 200 tokens
- page - around 3-4 paragraphs, 500 words = 600 tokens."
  (when max-tokens
    (cond ((< max-tokens 75)
           (format "Your asnwer limit is about %d words." (* max-tokens 0.75)))
          ((and (>= max-tokens 75)
                (< max-tokens 500))
           (format "Your asnwer limit is about %d sentences." (/ max-tokens 29)))
          ((and (>= max-tokens 500)
                (<= max-tokens 1000))
           (format "Your asnwer limit is about %d paragraphs or %d pages." (/ max-tokens 200) (ceiling (/ max-tokens 600.0)))))))

;;; -=-= Main

;; org-ai-stream-completion - old
(defun oai-restapi-request-prepare (req-type element sys-prompt sys-prompt-for-all-messages model max-tokens top-p temperature frequency-penalty presence-penalty service stream)
  "Compose API request from data and start a server-sent event stream.
Call `oai-restapi-request' function as a next step.
Called from `oai-call-block' in main file.
`REQ-TYPE' symbol - is completion or chat mostly. Set `oai-block--get-request-type'.
`ELEMENT' org-element - is ai block, should be converted to market at once.
`SYS-PROMPT' string - first system instruction as a string.
`SYS-PROMPT-FOR-ALL-MESSAGES' from `oai-restapi-default-inject-sys-prompt-for-all-messages' variable.
`MODEL' string - is the model to use.
`MAX-TOKENS' integer - is the maximum number of tokens to generate.
`TEMPERATURE' integer - 0-2 lower - low 0.3 high-probability tokens
producing predictable outputs. 1.5 diversity by flattening the
probability distribution.
`TOP-P' integer - 0-1 lower - chooses tokens whose cumulative probability exceeds this threshold, adapting to context.
`FREQUENCY-PENALTY' integer - -2-2, lower less repeat words.
`PRESENCE-PENALTY' integer - -2-2, lower less repeat concepts.
`SERVICE' symbol or string - is the AI cloud service such as openai or azure-openai.
`STREAM' string - as bool, indicates whether to stream the response."
  (oai--debug "oai-restapi-request-prepare")
  (let* (
         (content (string-trim (oai-block-get-content element))) ; string - is block content
         (messages (unless (eql req-type 'completion)
                     ;; - split content to messages
                     (oai-restapi--collect-chat-messages content
                                                           sys-prompt
                                                           sys-prompt-for-all-messages
                                                           (if oai-restapi-add-max-tokens-recommendation
                                                               (oai-restapi--get-lenght-recommendation max-tokens)
                                                             )))) ; oai-block.el
         (end-marker (oai-block--get-content-end-marker element))
         (callback (cond ; set to oai-restapi--current-url-request-callback
                    (messages
                     (lambda (result) (oai-restapi--insert-stream-response end-marker result t)))
                    ;; - completion
                    (t (lambda (result) (oai-restapi--insert-single-response end-marker
                                                                        (oai-restapi--get-single-response-text result)
                                                                        nil))))))
    (oai--debug "oai-restapi-request-prepare messages1" messages)
    (setq messages (oai-restapi--modify-last-user-content messages #'oai-block-tags-replace))
    (oai--debug "oai-restapi-request-prepare messages2" messages)
    ;; - Call and save buffer.
    (oai-timers--set
     (oai-restapi-request service model callback
                         :prompt content ; if completion
                         :messages messages
                         :max-tokens max-tokens
                         :temperature temperature
                         :top-p top-p
                         :frequency-penalty frequency-penalty
                         :presence-penalty presence-penalty
                         :stream stream)
     (oai-block-get-header-marker element))

    ;; - run timer that show /-\ looping, notification of status
    (oai-timers--progress-reporter-run
     #'oai-restapi--interrupt-url-request
     )))

;;; -=-= Main parts

;; Together.xyz 2025
;; '(id "nz7KyaB-3NKUce-9539d1912ce8b148" object "chat.completion" created 1750575101 model "meta-llama/Llama-3.3-70B-Instruct-Turbo-Free" prompt []
;;   choices [(finish_reason "length" seed 3309196889559996400 logprobs nil index 0
;;             message (role "assistant" content " The answer is simple: live a long time. But how do you do that? Well, itâs not as simple as it sounds." tool_calls []))] usage (prompt_tokens 5 completion_tokens 150 total_tokens 155 cached_tokens 0))

(defun oai-restapi--get-single-response-text (&optional response)
  "Return text from response or nil and signal error if it have \"error\" field.
For Completion LLM mode. Used as callback for `oai-restapi-request'."
  ;; (oai--debug "oai-restapi--get-single-response-text response:" response)
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
         )) (oai-restapi--get-single-response-text test-val))
  " The answer is simple: live a long time. But how do you do that? Well, itâs not as simple as it sounds."))



(defun oai-restapi--insert-single-response (end-marker &optional text insert-role final)
  "Insert result to ai block.
Should be used in two steps: 1) for insertion of text 2) with TEXT equal
to nil, for finalizing by setting pointer to the end and insertion of me
role.
Here used for completion mode in `oai-restapi-request'.
END-MARKER is where to put result,
TEXT is string from the response of OpenAI API extracted with `oai-restapi--get-single-response-text'.
END-MARKER is a buffer and position at the end of block.
FINAL wherer to finalize, also applied if no text provided."
  (oai--debug "oai-restapi--insert-single-response end-marker, text:" end-marker
                                                 text "")

    (let ((buffer (marker-buffer end-marker))
          (pos (marker-position end-marker)))
      (oai--debug "oai-restapi--insert-single-response buffer,pos:" buffer pos "")

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
                  (run-hook-with-args 'oai-restapi-after-chat-insertion-hook 'end text pos buffer)
                (error
                 (message "Error during \"after-chat-insertion-hook\": %s" hook-error)))
              (when final
                (org-element-cache-reset)
                (undo-boundary))
              (setq pos (point))))

            ;; ;; - save in url buffer. for what?
            ;; (setq oai-restapi--current-insert-position-marker pos)

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
          (when oai-restapi-jump-to-end-of-block
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
(defun oai-restapi--normalize-response (response)
  "This function normalizes JSON data received from OpenAI-style, Anthropic, and Perplexity endpoints.
`RESPONSE' is one JSON message of the stream response."
  ;; (oai--debug "response:" response)

  (if-let ((error-message (plist-get response 'error)))
      (list (make-oai-restapi--response :type 'error :payload (or (plist-get error 'message) error-message)))

    (let ((response-type (plist-get response 'type)))

      ;; first try anthropic
      (cond
       ((string= response-type "ping") nil)
       ((string= response-type "message_start")
        (when-let ((role (plist-get (plist-get response 'message) 'role)))
          (list (make-oai-restapi--response :type 'role :payload role))))
       ((string= response-type "content_block_start")
        (when-let ((text (plist-get (plist-get response 'content_block) 'text)))
          (list (make-oai-restapi--response :type 'text :payload text))))
       ((string= response-type "content_block_delta")
        (when-let ((text (plist-get (plist-get response 'delta) 'text)))
          (list (make-oai-restapi--response :type 'text :payload text))))
       ((string= response-type "content_block_stop") nil)
       ((string= response-type "message_delta")
        (when-let ((stop-reason (plist-get (plist-get response 'delta) 'stop_reason)))
          (list (make-oai-restapi--response :type 'stop :payload stop-reason))))
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
                 (list (make-oai-restapi--response :type 'role :payload role)))
               (when content
                 (list (make-oai-restapi--response :type 'text :payload content)))
               (when finish-reason
                 (list (make-oai-restapi--response :type 'stop :payload finish-reason))))))))

       ;; single message e.g. from non-streamed completion
       ((let ((choices (plist-get response 'choices)))
          (and (= 1 (length choices))
               (plist-get (aref choices 0) 'message)))
        (let* ((choices (plist-get response 'choices))
               (choice (aref choices 0))
               (text (plist-get (plist-get choice 'message) 'content))
               (role (plist-get (plist-get choice 'message) 'role))
               (finish-reason (or (plist-get choice 'finish_reason) 'stop)))
          (list (make-oai-restapi--response :type 'role :payload role)
                (make-oai-restapi--response :type 'text :payload text)
                (make-oai-restapi--response :type 'stop :payload finish-reason))))

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
                                (push (make-oai-restapi--response :type 'stop :payload finish-reason) result))
                              (when reasoning-content
                                (setq oai-restapi--currently-reasoning t)
                                (push (make-oai-restapi--response :type 'text :payload reasoning-content) result))
                              (when (and content (> (length content) 0))
                                (push (make-oai-restapi--response :type 'text :payload content) result)
                                (when oai-restapi--currently-reasoning
                                  (setq oai-restapi--currently-reasoning nil)
                                  (push (make-oai-restapi--response :type 'role :payload "assistant") result)))
                              (when role
                                (push (make-oai-restapi--response :type 'role :payload role) result))
                              result))))))))

(defun oai-restapi--insert-stream-response (end-marker &optional response insert-role)
  "Insert result to ai block for chat mode.
When first chunk received we stop waiting timer for request.
END-MARKER'is where to put result,
RESPONSE is one JSON message of the stream response.
Used as callback for `oai-restapi-request', called in url buffer.
When RESPONSE is nil, it means we are done.
Save variables:
`oai-restapi--current-insert-position-marker',
`oai-restapi--currently-inside-code-markers'
`oai-restapi--currently-chat-got-first-response'
`oai-restapi--current-chat-role' in current buffer.
Called within url-buffer."

(oai--debug "oai-restapi--insert-stream-response")
  ;; (if (not response)
  ;;     (progn
  ;;       (oai--debug "oai-restapi--insert-stream-response org-ai-reset-stream-state")
  ;;       ;; (org-ai-reset-stream-state))
    ; - else
    (let ((normalized (oai-restapi--normalize-response response)) ; list of messages
          (buffer (marker-buffer end-marker))
          (first-resp oai-restapi--currently-chat-got-first-response)
          (pos (or oai-restapi--current-insert-position-marker
                   (marker-position end-marker)))
          (c-inside-code-m oai-restapi--currently-inside-code-markers)
          (c-chat-role oai-restapi--current-chat-role)
          (url-buffer (current-buffer))
          stop-flag)
      ;; (oai--debug "oai-restapi--insert-stream-response" normalized)
      (unwind-protect ; we need to save variables to url buffer
          (with-current-buffer buffer ; target buffer with block
            (save-excursion
              ;; (when (not normalized)
              ;;   (oai--debug "oai-restapi--insert-stream-response NO normalized!" response))
              ;; - LOOP Per message
              (cl-loop for response in normalized
                       do (let ((type (oai-restapi--response-type response)))
                            ;; (oai--debug "oai-restapi--insert-stream-response: %s %s %s" type end-marker oai-restapi--current-insert-position-marker)
                            ;; - Type of message: error
                            (when (eq type 'error)
                              (error (oai-restapi--response-payload response)))

                            ;; - Make sure we have enough space at end of block, don't write on same line
                            (goto-char pos)
                            (when (string-suffix-p "#+end_ai" (buffer-substring-no-properties (point) (line-end-position)))
                              (insert "\n")
                              (backward-char))

                            ;; - Type of message
                            (cl-case type
                              (role (let ((role (oai-restapi--response-payload response)))
                                      (when (not (string= role c-chat-role))
                                        (goto-char pos)

                                        (setq c-chat-role role)
                                        (let ((role (and insert-role (oai-restapi--response-payload response))))
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
                                              (run-hook-with-args 'oai-restapi-after-chat-insertion-hook 'role role pos buffer)
                                            (error
                                             (message "Error during \"after-chat-insertion-hook\" for role: %s" hook-error)))

                                          (setq pos (point))
                                          ))))

                              (text (let ((text (decode-coding-string (oai-restapi--response-payload response) 'utf-8)))
                                      (goto-char pos)
                                      (when (or first-resp (not (string= (string-trim text) "")))
                                        (when (and (not first-resp) (string-prefix-p "```" text))
                                          ;; start markdown codeblock responses on their own line
                                          (insert "\n"))
                                        (when first-resp
                                          ;; call stop waiting with url-buffer and progress reporter.
                                          (oai--debug "oai-restapi--insert-stream-response first-resp")
                                          (oai-timers--interrupt-current-request url-buffer)
                                          )
                                        ;; track if we are inside code markers
                                        (setq c-inside-code-m (and (not c-inside-code-m) ; oai-restapi--currently-inside-code-markers
                                                        (string-match-p "```" text)))
                                        ;; (oai--debug response)
                                        ;; (oai--debug text)
                                        (insert text)
                                        ;; - "auto-fill" if not in code block
                                        (when (and oai-restapi-auto-fill (not c-inside-code-m))
                                          (fill-paragraph))

                                        (condition-case hook-error
                                            (run-hook-with-args 'oai-restapi-after-chat-insertion-hook 'text text pos buffer)
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
                                            (run-hook-with-args 'oai-restapi-after-chat-insertion-hook 'end text pos buffer)
                                          (error
                                           (message "Error during \"after-chat-insertion-hook\": %s" hook-error)))
                                        (setq pos (point)))

                                      (org-element-cache-reset)
                                      (setq stop-flag t)
                                      ))))))
            ;; - without save-excursion - go to the end.
            (when (and oai-restapi-jump-to-end-of-block
                       stop-flag)
              (goto-char pos)))
        ;; - after buffer - UNWINDFORMS - save variables to url-buffer
        (setq oai-restapi--current-insert-position-marker pos)
        (setq oai-restapi--currently-inside-code-markers c-inside-code-m)
        (setq oai-restapi--current-chat-role c-chat-role)
        )
      ;; - in let
      normalized))

;; org-ai-stream-request - old
(cl-defun oai-restapi-request (service model callback &optional &key prompt messages max-tokens temperature top-p frequency-penalty presence-penalty stream)
  "Use API to LLM to request and get response.
Executed by `oai-restapi-request-prepare'
`PROMPT' is the query for completions `MESSAGES' is the query for
chatgpt. `CALLBACK' is the callback function. `MODEL' is the
model to use. `MAX-TOKENS' is the maximum number of tokens to
generate. `TEMPERATURE' is the temperature of the distribution.
`TOP-P' is the top-p value. `FREQUENCY-PENALTY' is the frequency
penalty. `PRESENCE-PENALTY' is the presence penalty.
Variables used to save state:
not buffer local:

buffer local and nil by default:
`oai-restapi--current-insert-position-marker' - in url callback to track where we insert.
`oai-restapi--currently-chat-got-first-response' - for Stream, bool.
`oai-restapi--currently-inside-code-markers' - code block received, bool.
`oai-restapi--current-request-is-streamed'
`oai-restapi--current-url-request-callback'.

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

  ;; (oai--debug service (type-of service))
  ;; (oai--debug stream (type-of stream))

  ;; - HTTP body preparation as a string
  (let ((url-request-extra-headers (oai-restapi--get-headers service))
        (url-request-method "POST")
        (endpoint (oai-restapi--get-endpoint messages service))
        (url-request-data (oai-restapi--payload :prompt prompt
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
    (oai-restapi--check-model model endpoint) ; not empty and if "api.openai.com" or "openai.azure.com"
    (oai--debug "oai-restapi-request endpoint:" service (type-of service))
    (oai--debug "oai-restapi-request endpoint:" endpoint (type-of endpoint))
    (oai--debug "oai-restapi-request headers:" url-request-extra-headers)
    (oai--debug "oai-restapi-request request-data:" (oai-restapi--prettify-json-string url-request-data))


    (oai--debug "Main request before, that return a \"urllib buffer\".")
    (let ((url-request-buffer
           (url-retrieve ; <- - - - - - - - -  MAIN
            endpoint
            (lambda (_events) ; called with url-request-buffer as current buffer
              "url-request-buffer event"
              ;; called at error or at the end after `after-change-functions' hooks
              (oai--debug "url-retrieve callback:" _events)

              (oai-restapi--debug-urllib (current-buffer))

              ;; Called for not stream, call `oai-restapi--current-url-request-callback'
              (oai-restapi--url-request-on-change-function nil nil nil)

              (oai-restapi--maybe-show-openai-request-error) ; TODO: change to RESULT by global customizable option
              ;; finally stop track buffer, error or not
              ;; (oai--debug "Main request lambda" _events)
              (oai-timers--interrupt-current-request (current-buffer) #'oai-restapi--stop-tracking-url-request)
              ))))

      (oai--debug "Main request after." url-request-buffer)

      (with-current-buffer url-request-buffer ; old org-ai--last-url-request-buffer
        ; just in case, also reset in `org-ai-reset-stream-state'
        (setq oai-restapi--currently-inside-code-markers nil)
        (setq oai-restapi--current-insert-position-marker nil)
        (setq oai-restapi--currently-chat-got-first-response nil)
        (setq oai-restapi--current-chat-role nil)
        ;; - it is `oai-restapi--insert-stream-response' or `oai-restapi--insert-single-response'
        (setq oai-restapi--current-url-request-callback callback)
        ;; - `oai-restapi--url-request-on-change-function', `oai-restapi--current-request-is-streamed'
        (setq oai-restapi--current-request-is-streamed stream)

        ;; - for stream add hook, otherwise remove - do word by word output (optional actually)
        (if stream
            (unless (member 'oai-restapi--url-request-on-change-function after-change-functions)
              (add-hook 'after-change-functions #'oai-restapi--url-request-on-change-function nil t))
          ;; else - not stream
          (remove-hook 'after-change-functions #'oai-restapi--url-request-on-change-function t))
        )
      url-request-buffer)))



(cl-defun oai-restapi-request-llm (service model callback &optional &key prompt messages max-tokens temperature top-p frequency-penalty presence-penalty)
  "Simplified version of `oai-restapi-request' without stream support.
Used for building agents or chain of requests.
Call callback with nil or result of `oai-restapi--normalize-response' of response."
  (let ((url-request-extra-headers (oai-restapi--get-headers service))
        (url-request-method "POST")
        (endpoint (oai-restapi--get-endpoint messages service))
        (url-request-data (oai-restapi--payload :prompt prompt
					   :messages messages
					   :model model
					   :max-tokens max-tokens
					   :temperature temperature
					   :top-p top-p
					   :frequency-penalty frequency-penalty
					   :presence-penalty presence-penalty
					   :service service
					   :stream nil)))
    (oai--debug "oai-restapi-request-llm prompt: %s" prompt)
    (oai--debug "oai-restapi-request-llm messages: %s" messages)
    (oai--debug "oai-restapi-request-llm endpoint: %s %s" endpoint (type-of endpoint))
    (oai--debug "oai-restapi-request-llm request-data:" (oai-restapi--prettify-json-string url-request-data))



    (let* ((url-request-buffer
           (url-retrieve ; <- - - - - - - - -  MAIN
            endpoint
            (lambda (_events)
              (oai-restapi--debug-urllib (current-buffer))
              (if (oai-restapi--maybe-show-openai-request-error) ; TODO: change to RESULT by global customizable option
                  (funcall callback nil) ; signal error to callback
                ;; else - read from url-buffer
                (when (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
                  (goto-char url-http-end-of-headers)
                  (let ((json-object-type 'plist)
                        (json-key-type 'symbol)
                        (json-array-type 'vector))
                    (condition-case _err
                        ;; (let* ((res1 (buffer-substring-no-properties (point) (point-max)))
                        ;;       (res2 (json-read-from-string res1))
                        ;;       (res3 (oai-restapi--normalize-response res2))
                        ;;       (res4 (nth 1 res3))
                        ;;       (res5 (oai-restapi--response-payload res4))
                        ;;       (res (decode-coding-string res5 'utf-8))
                        ;;       )
                        ;;   (print (list "HERE6" res3 ))
                        ;;   (print (list "HERE6h7" callback))
                        ;;   ;; to prevent catching error to here
                        ;;   (run-at-time 0 nil callback res)
                        ;;   )
                        ;; (message "no HERE7 error")
                        (funcall callback (decode-coding-string (oai-restapi--response-payload (nth 1
                                                                                               (oai-restapi--normalize-response
                                                                                                (json-read-from-string
                                                                                                 (buffer-substring-no-properties (point) (point-max))))))
                                                                'utf-8))
                      (error
                       nil
                       ;; (message "HERE7 error")
                       ;; (funcall callback nil) ; signal error to callback
                       ))))))))
           ;; (timeout (or timeout oai-timers-duration))
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
;;   (oai-timers--progress-reporter-run
;;    1
;;    (lambda (buf) (oai-timers--interrupt-current-request buf #'oai-restapi--interrupt-url-request))
;;    (oai-restapi-request-llm service model (lambda (result)
;;                                            (oai-timers--interrupt-current-request (current-buffer) #'oai-restapi--stop-tracking-url-request)
;;                                            (print (list "hay" result)))
;;                            :timeout 20
;;                            :messages  (vector (list :role 'system :content "You a helpful.")
;;                                               (list :role 'user :content "How to do staff?"))
;;                            :max-tokens max-tokens
;;                            :temperature temperature
;;                            :top-p top-p
;;                            :frequency-penalty frequency-penalty
;;                            :presence-penalty presence-penalty)))

(defvar oai-restapi-request-llm-retries-local-url-buffer  nil
  "Save url-buffer in temp buffer in which timer have been started.")
(make-variable-buffer-local 'oai-restapi-request-llm-retries-local-url-buffer)

(cl-defun oai-restapi-request-llm-retries (service model timeout callback &optional &key retries prompt messages header-marker max-tokens temperature top-p frequency-penalty presence-penalty)
  "Add TIMEOUT and RETRIES parameters to `oai-restapi-request-llm' function.
Timer function restart requst and restart timer with attempts-1.
In callback we add cancel timer function.
We save and cancel time only in callback.
TIMER is time to wait for one request."
  (oai--debug "oai-restapi-request-llm-retries1 timeout %s" timeout)
  (let ((cb (current-buffer)))
    (if (or (and retries (> retries 0)) ; just in case
            (not retries))
        (with-temp-buffer ; to separate variable `oai-restapi-request-llm-retries-local-url-buffer'
          (oai--debug "oai-restapi-request-llm-retries12")
          (let* ((left-retries (if retries (1- retries) 3))
                 ;; run timer in temp buffer
                 (timer (run-with-timer timeout
                                        0
                                        (lambda ()
                                          (oai--debug "in oai-restapi-request-llm-retries timer: left-retries: %s buffer-life: %s buffer:" left-retries (buffer-live-p oai-restapi-request-llm-retries-local-url-buffer) oai-restapi-request-llm-retries-local-url-buffer)
                                          ;; - kill old and restart only if request was hanging
                                          (when (buffer-live-p oai-restapi-request-llm-retries-local-url-buffer)
                                            (oai-timers--interrupt-current-request oai-restapi-request-llm-retries-local-url-buffer #'oai-restapi--interrupt-url-request)
                                            ;; - restart
                                            (if (> left-retries 0)
                                                (oai-restapi-request-llm-retries service model timeout callback
                                                                                :retries left-retries
                                                                                :messages messages
                                                                                :max-tokens max-tokens
                                                                                :header-marker header-marker
                                                                                :temperature temperature
                                                                                :top-p top-p
                                                                                :frequency-penalty frequency-penalty
                                                                                :presence-penalty presence-penalty)
                                              ;; else - failed
                                              ;; (oai-timers--remove-variable header-marker) ;?
                                              (run-at-time 0 nil callback nil)
                                              (with-current-buffer (marker-buffer header-marker)
                                                  (save-excursion
                                                    (goto-char header-marker)
                                                    (oai-block-insert-result "Failed")))
                                              (oai-timers--update-global-progress-reporter)
                                              ))))))
            (oai--debug "oai-restapi-request-llm-retries2")
            ;; inside temp buffer we set variable
            (setq oai-restapi-request-llm-retries-local-url-buffer
                  (oai-restapi-request-llm service model
                                          (lambda (result-llm)
                                            (oai--debug "oai-restapi-request-llm callback1, timer, result:" timer result-llm)
                                            (if timer
                                                (cancel-timer timer))
                                            (oai--debug "oai-restapi-request-llm  callback2")
                                            ;; (with-current-buffer cb
                                            (oai--debug "oai-restapi-request-llm  callback3 %s %s" callback result-llm)
                                            (if result-llm
                                                (progn
                                                  (oai--debug "oai-restapi-request-llm here")
                                                  (run-at-time 0 nil callback result-llm))
                                              ;; else - error - retry
                                              (if (> left-retries 0)
                                                  (progn
                                                    (oai--debug "oai-restapi-request-llm here2")
                                                    ;; retrie after 3 sec
                                                  (run-at-time 3 nil (lambda () (oai-restapi-request-llm-retries service model timeout callback
                                                                                  :retries left-retries
                                                                                  :messages messages
                                                                                  :max-tokens max-tokens
                                                                                  :header-marker header-marker
                                                                                  :temperature temperature
                                                                                  :top-p top-p
                                                                                  :frequency-penalty frequency-penalty
                                                                                  :presence-penalty presence-penalty))))
                                                ;; else - failed
                                                (oai--debug "oai-restapi-request-llm failed")
                                                (run-at-time 0 nil callback nil)
                                                (with-current-buffer (marker-buffer header-marker)
                                                  (save-excursion
                                                    (goto-char header-marker)
                                                    (oai-block-insert-result "Failed")))
                                                (oai-timers--update-global-progress-reporter)
                                                ))

                                            ;; (funcall callback result-llm)
                                            (oai--debug "oai-restapi-request-llm  callback4")
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
            (oai-timers--set oai-restapi-request-llm-retries-local-url-buffer
                             header-marker)
            (oai--debug "oai-restapi-request-llm-retries3" oai-timers--element-marker-variable-dict oai-restapi-request-llm-retries-local-url-buffer)
            )))))

(defun oai-restapi--maybe-show-openai-request-error ()
  "If the API request returned an error, show it.
`REQUEST-BUFFER' is the buffer containing the request.
Return t if error happen, otherwise nil"
  (oai--debug "oai-restapi--maybe-show-openai-request-error1")


  (let ((http-code (url-http-symbol-value-in-buffer 'url-http-response-status (current-buffer))) ;; integer
        (http-data (if (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
                       ;; get data after HTTP headers from current url buffer
                            (progn
                              (string-trim (buffer-substring-no-properties url-http-end-of-headers
                                                                           (point-max))))
                       ;; else
                       ""))
        (http-header-first-line (buffer-substring-no-properties (point-min)
                                                                (save-excursion
                                                                  (goto-char (point-min))
                                                                  (line-end-position)))))
    (oai--debug "oai-restapi--maybe-show-openai-request-error2 %s %s" http-code http-data)
    (or
     (when (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
           (goto-char url-http-end-of-headers)
           (condition-case nil

               (when-let* ((body (json-read))
                           (err (or (alist-get 'error body)
                                    (plist-get body 'error)))
                           (message (or (alist-get 'message err)
                                        (plist-get err 'message)))
                           (message (if (and message (not (string-blank-p message)))
                                        message
                                      (json-encode err))))
                 (if oai-restapi-show-error-in-result
                     (oai-restapi-insert-result-error (concat (format "%s\n" http-header-first-line)
                                                              "Error from the service API:\n\t" message) (current-buffer))
                   ;; else
                   (oai-restapi--show-error message))
                 )
             (error nil)))
     (when (and http-code (/= http-code 200))
       (if oai-restapi-show-error-in-result
           (oai-restapi-insert-result-error (format "HTTP Error from the service: %s %s \n %s" http-code http-data http-header-first-line) (current-buffer))
           ;; (oai-block-insert-result (format "HTTP Error code from the service API: %s" http-code))
           (oai-restapi--show-error (number-to-string http-code))
         )))))

(defun oai-restapi-insert-result-error (message url-buffer)
  (let ((header-marker (oai-timers--get-variable url-buffer)))
    (with-current-buffer (marker-buffer header-marker)
      (save-excursion
        (goto-char header-marker)
        (oai-block-insert-result message)))))

(defun oai-restapi--show-error (error-message)
  "Show an error message in a buffer.
`ERROR-MESSAGE' is the error message to show."
  (condition-case nil
      (let ((buf (get-buffer-create "*oai error*")))
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

(cl-defun oai-restapi--payload (&optional &key service model prompt messages max-tokens temperature top-p frequency-penalty presence-penalty stream)
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

(defun oai-restapi--url-request-on-change-function (_beg _end _len)
  "First function that read url-request buffer and extracts JSON stream responses.
Three arguments are passed to each function: the positions of
the beginning and end of the range of changed text,
and the length in chars of the pre-change text replaced by that range.
Call `oai-restapi--current-url-request-callback' with data.
After processing call `oai-restapi--current-url-request-callback' with nil.
This  callback  here  is `oai-restapi--insert-stream-response'  for  chat  or
`oai-restapi--insert-single-response' for completion.
Called within url-retrieve buffer."
  ;; (oai--debug "oai-restapi--url-request-on-change-function: %s %s %s %s" _beg _end _len (current-buffer))
  ;; (with-current-buffer org-ai--last-url-request-buffer
  (when (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
    (save-match-data
      (save-excursion
        (if oai-restapi--url-buffer-last-position-marker
            (goto-char oai-restapi--url-buffer-last-position-marker)
          ;; else
          (goto-char url-http-end-of-headers)
          (setq oai-restapi--url-buffer-last-position-marker (point-marker)))

          ;; Avoid a bug where we skip responses because url has modified the http
          ;; buffer and we are not where we think we are.
          ;; TODO this might break
          (unless (eolp)
            (beginning-of-line))

          ;; - Non-streamed - response of a single json object
          (if (not oai-restapi--current-request-is-streamed)
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
                        (oai--debug "on change 1)")
                        (funcall oai-restapi--current-url-request-callback data) ; INSERT CALLBACK!
                        ;; We call this in lambda "url-request-buffer event" anyway
                        ;; (oai-timers--interrupt-current-request (current-buffer) #'oai-restapi--stop-tracking-url-request)
                        ))
                  (error
                   nil
                   )
                  )
                ;; - Done or Error
                (oai--debug "on change 2)")
                (funcall oai-restapi--current-url-request-callback nil) ; INSERT CALLBACK!
                (message "oai request done")
                )

            ;; - else - streamed, multiple json objects prefixed with "data: "
            ;; (oai--debug "oai-restapi--url-request-on-change-function 2.1) %s %s" (point) (eolp))
            (let ((errored nil))
              (while (and (not errored)
                          (search-forward "data: " nil t))
                (let ((line (buffer-substring-no-properties (point) (line-end-position)))
                      (tmp-buf "*oai--temp*"))
                  ;; (oai--debug "on change 2.2) line: %s" line)
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
                                        (oai--debug "oai-restapi--url-request-on-change-function 2.3) errored")
                                        (setq errored t)
                                        (kill-buffer)
                                        nil))))
                        ;; (setq org-ai--debug-data (append org-ai--debug-data (list data)))
                        (when data
                          (end-of-line)
                          (set-marker oai-restapi--url-buffer-last-position-marker (point))
                          ;; (oai--debug (format "on change 3) %s" oai-restapi--url-buffer-last-position-marker))
                          (funcall oai-restapi--current-url-request-callback data) ; INSERT CALLBACK!
                          ))

                    ;; - else "[DONE]" string found
                    (progn
                      ;; (end-of-line)
                      (set-marker oai-restapi--url-buffer-last-position-marker (point))
                      ;; (setq oai-restapi--url-buffer-last-position-marker nil)
                      (oai-timers--interrupt-current-request (current-buffer) #'oai-restapi--stop-tracking-url-request) ; stop timer
                      ;; (oai-timers--interrupt-current-request (current-buffer) #'oai-restapi--interrupt-url-request) ; stop timer
                      (if (get-buffer tmp-buf)
                          (kill-buffer tmp-buf))
                      ;; (oai--debug "on change 4)")
                      (funcall oai-restapi--current-url-request-callback nil) ; INSERT CALLBACK!
                      ;; (org-ai-reset-stream-state)
                      (message "oai request done"))
                    )))))
          ;; (goto-char p) ; additional protection
          ;; (oai--debug "oai-restapi--url-request-on-change-function end")
          ))))

;; (defun org-ai--kill-query-process ()
;;   (let ((proc (get-buffer-process org-ai--last-url-request-buffer)))
;;     (set-process-sentinel proc 'ignore)
;;     (delete-process proc)))


;; (defun org-ai-reset-url-state ()

;; (defun org-ai-reset-stream-state ()
;;   "Reset the stream state.
;; Should be called within url-retrieve buffer. Danger function."
;;   (interactive)
;;   (setq oai-restapi--current-url-request-callback nil)
;;   (setq oai-restapi--url-buffer-last-position-marker nil)
;;   (setq oai-restapi--current-chat-role nil)
;;   (setq oai-restapi--current-request-is-streamed nil)
;;   (org-ai--progress-reporter-global-cancel (current-buffer))
;;   )

;;; -=-= Reporter & Requests interrupt functions
;; 1) `oai-timers--progress-reporter-run' - start global timer
;; 2) `oai-timers--interrupt-current-request' - interrupt, called to stop tracking on-changes or kill buffer
;; When  we kill  one buffer  and if  no others  we report  failure or
;; success  if there  are  other  we just  continue  and don't  change
;; reporter.
;; Functions:
;; Failure by time:
;; `oai-restapi-stop-all-url-requests' 'failed
;; `oai-timers--interrupt-current-request'
;; Success:
;; `oai-timers--interrupt-current-request'
;; Interactive:
;; `oai-restapi-interrupt-url-request'
;; Variables:
;; - `oai-timers--global-progress-reporter' - lambda that return a string,
;; - `oai-timers--global-progress-timer' - timer that output /-\ to echo area.
;; - `oai-timers--global-progress-timer-remaining-ticks'.
;; - `oai-timers--current-progress-timer' - count life of url buffer,
;; - `oai-timers--current-progress-timer-remaining-ticks'."

(defun oai-restapi--interrupt-url-request (url-buffer)
  "Remove on-update hook and kill buffer.
Called from `oai-restapi-stop-url-request',
`oai-restapi-stop-all-url-requests'."
  ;; (oai--debug "oai-restapi--interrupt-url-request"
  ;;                (eq (current-buffer) url-buffer)
  ;;                (buffer-live-p url-buffer))
  (if (eq (current-buffer) url-buffer)
      (progn
        (remove-hook 'after-change-functions #'oai-restapi--url-request-on-change-function t)
        (when (buffer-live-p url-buffer)
          (let (kill-buffer-query-functions)
            (kill-buffer url-buffer))))
    ;; else
    (when (and url-buffer (buffer-live-p url-buffer))
      (with-current-buffer url-buffer
        (remove-hook 'after-change-functions #'oai-restapi--url-request-on-change-function t)
        (let (kill-buffer-query-functions) ; set to nil
          (kill-buffer url-buffer))))))

(defun oai-restapi--stop-tracking-url-request (url-buffer)
  "Remove on-update hook and not kill buffer.
  Called from `oai-restapi-stop-url-request',
`oai-restapi-stop-all-url-requests'."
  (oai--debug "oai-restapi--stop-tracking-url-request"
                 (eq (current-buffer) url-buffer)
                 (buffer-live-p url-buffer))
  (if (eq (current-buffer) url-buffer)
      (remove-hook 'after-change-functions #'oai-restapi--url-request-on-change-function t)
    ;; else
    (if (and url-buffer (buffer-live-p url-buffer))
      (with-current-buffer url-buffer
        (remove-hook 'after-change-functions #'oai-restapi--url-request-on-change-function t)))))

(cl-defun oai-restapi-stop-url-request (&optional &key element url-buffer failed)
  "Interrupt the request for ELEMENT or URL-BUFFER.
If  no ELEMENT  or URL-BUFFER  provided we  use in  current ai  block at
current position at current buffer.
Return t if buffer was found.
Called from `oai-timers--progress-reporter-run'."
  (interactive)
  ;; (oai--debug "oai-restapi-stop-url-request, current buffer %s, url-buf %s, element %s"
  ;;                (current-buffer) url-buffer
  ;;                (oai-block-p))
  (if-let* ((element (or element (oai-block-p)))
            (url-buffers (if url-buffer
                             (list url-buffer)
                           ;; else
                           (oai-timers--get-keys-for-variable (oai-block-get-header-marker element)))))
      (progn
        (oai--debug "oai-restapi-stop-url-request, element %s, url-buffers %s"
                       element
                       url-buffers)
        (oai-timers--interrupt-current-request url-buffers #'oai-restapi--interrupt-url-request failed)
        t)
    ;; ;; else - called not at from some block, but from elsewhere
    ;; (oai--debug "oai-restapi-stop-url-request all %s" (oai-timers--get-keys-for-variable (oai-block-get-header-marker element)))
    (oai-restapi-stop-all-url-requests) ; kill all

    ;; else
    )

  )

;;;###autoload
(cl-defun oai-restapi-stop-all-url-requests (&optional &key failed)
  "Called from `oai-restapi-stop-url-request' when not at some block,
  Return t if buffer was found."
  (interactive)
  (oai-timers--interrupt-all-requests #'oai-restapi--interrupt-url-request failed))



;;; -=-= chat-messages collect/stringify

(defun oai-restapi--collect-chat-messages (content-string &optional default-system-prompt persistant-sys-prompts max-token-recommendation)
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
    (oai-restapi--collect-chat-messages test-string))
  '[(:role user :content "testing\nfoo bar baz zorrk\nfoo")
    (:role assistant :content "hello hello")]))

;; sys prompt
(cl-assert
 (equal
  (let ((test-string "[SYS]: system\n[ME]: user\n[AI]: assistant"))
    (oai-restapi--collect-chat-messages test-string))
  '[(:role system :content "system")
    (:role user :content "user")
    (:role assistant :content "assistant")]))

;; sys prompt intercalated
(cl-assert
 (equal
  (let ((test-string "[SYS]: system\n[ME]: user\n[AI]: assistant\n[ME]: user"))
    (oai-restapi--collect-chat-messages test-string nil t))
  '[(:role system :content "system")
    (:role user :content "user")
    (:role assistant :content "assistant")
    (:role system :content "system")
    (:role user :content "user")]))

;; merge messages with same role
(cl-assert
 (equal
  (let ((test-string "[ME]: hello [ME]: world")) (oai-restapi--collect-chat-messages test-string))
  '[(:role user :content "hello\nworld")]))

(cl-assert
 (equal
  (let ((test-string "[ME:] hello world")) (oai-restapi--collect-chat-messages test-string))
  '[(:role user :content "hello world")]))

(cl-assert
 (equal
  (let ((test-string "[ME]: hello [ME:] world")) (oai-restapi--collect-chat-messages test-string))
  '[(:role user :content "hello\nworld")]))

;; (oai-restapi--normalize-response '(id "o3fA4D4-62bZhn-9617f44f6d399d91" object "chat.completion" created 1752904364 model "meta-llama/Llama-3.3-70B-Instruct-Turbo-Free" prompt [] choices [(finish_reason "stop" seed 819567834314233700 logprobs nil index 0 message (role "assistant" content "It works: `(2 3 1)` is returned." tool_calls []))] usage (prompt_tokens 131 completion_tokens 14 total_tokens 145 cached_tokens 0)))
;; (#s(oai-restapi--response role "assistant") #s(oai-restapi--response text "It works: `(2 3 1)` is returned.") #s(oai-restapi--response stop "stop"))
;; (oai-restapi--normalize-response '(id "o3f9cZv-4Yz4kd-9617f234fd6f9d91" object "chat.completion" created 1752904277 model "meta-llama/Llama-3.3-70B-Instruct-Turbo-Free" prompt [] choices [(finish_reason "stop" seed 589067420664432000 logprobs nil index 0 message (role "assistant" content "`(mapcar 'cdr '((a . 2) (x . 3) (2 . 1)))` returns `(2 3 1)`." tool_calls []))] usage (prompt_tokens 84 completion_tokens 34 total_tokens 118 cached_tokens 0)))
;; (#s(oai-restapi--response role "assistant") #s(oai-restapi--response text "`(mapcar 'cdr '((a . 2) (x . 3) (2 . 1)))` returns `(2 3 1)`.") #s(oai-restapi--response stop "stop"))
;; (oai-restapi--response-type response)
;; (comment
;;   (with-current-buffer "oai-mode-test.org"
;;    (oai-restapi--collect-chat-messages (oai-block-get-content))))

;; - Not used now
(cl-defun oai-restapi--stringify-chat-messages (messages &optional &key
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
  (oai-restapi--stringify-chat-messages '[(:role system :content "system")
                                     (:role user :content "user")
                                     (:role assistant :content "assistant")])
  "[SYS]: system\n\n[ME]: user\n\n[AI]: assistant"))

(cl-assert
 (equal
  (oai-restapi--stringify-chat-messages '[(:role user :content "user")
                                     (:role assistant :content "assistant")]
                                   :default-system-prompt "system")
  "[SYS]: system\n\n[ME]: user\n\n[AI]: assistant"))

(cl-assert
 (equal
  (oai-restapi--stringify-chat-messages '[(:role user :content "user")
                                     (:role assistant :content "assistant")]
                                   :user-prefix "You: "
                                   :assistant-prefix "Assistant: ")
  "You: user\n\nAssistant: assistant"))


(cl-assert
 (equal
  (let ((test-val '(id "o3fA4D4-62bZhn-9617f44f6d399d91" object "chat.completion" created 1752904364 model "meta-llama/Llama-3.3-70B-Instruct-Turbo-Free" prompt [] choices [(finish_reason "stop" seed 819567834314233700 logprobs nil index 0 message (role "assistant" content "It works: `(2 3 1)` is returned." tool_calls []))] usage (prompt_tokens 131 completion_tokens 14 total_tokens 145 cached_tokens 0))
                  ))
    (oai-restapi--normalize-response test-val))
  '(#s(oai-restapi--response role "assistant") #s(oai-restapi--response text "It works: `(2 3 1)` is returned.") #s(oai-restapi--response stop "stop"))))


(let* ((test-val '(#s(oai-restapi--response role "assistant") #s(oai-restapi--response text "It seems ") #s(oai-restapi--response stop "length")))
       (test-val0 (nth 0 test-val))
       (test-val1 (nth 1 test-val))
       )
  (and
   (equal (length test-val) 3)
   (equal (oai-restapi--response-type test-val0) 'role)
   (string-equal (decode-coding-string (oai-restapi--response-payload test-val0) 'utf-8) "assistant")
   (equal (oai-restapi--response-type test-val1) 'text)
   (string-equal (decode-coding-string (oai-restapi--response-payload test-val1) 'utf-8) "It seems ")
  ))



;;; -=-= Last user message

(defun oai-restapi--find-last-user-index (vec)
  "Return the index of the last element in VEC whose :role is 'user, or nil."
  (let ((i (1- (length vec)))
        idx)
    (while (and (>= i 0) (not idx))
      (let ((elt (aref vec i)))
        (when (and (listp elt)
                   (eq (plist-get elt :role) 'user))
          (setq idx i)))
      (setq i (1- i)))
    idx))


(defun oai-restapi--modify-last-user-content (vec new-content)
  "Return new vector based on VEC, replacing last 'user :content with NEW-CONTENT
(string or function of old content). Uses `oai-restapi--find-last-user-index`."
  (let ((idx (oai-restapi--find-last-user-index vec))
        (newvec (copy-sequence vec)))
    (when idx
      (let* ((elt (aref newvec idx))
             (old-content (plist-get elt :content))
             (rep-content (if (functionp new-content)
                              (funcall new-content old-content)
                            new-content))
             (new-elt (plist-put (copy-sequence elt) :content rep-content)))
        (aset newvec idx new-elt)))
    newvec))

(cl-assert
 (equal (oai-restapi--modify-last-user-content
         (vector (list :role 'system :content "foo")
                 (list :role 'user :content "How to make coffe1?")
                 (list :role 'assistant :content "IDK.")
                 (list :role 'user :content "How to make coffe2?")
                 (list :role 'system :content "other"))
         (lambda (x) (concat x " wtf")))
        '[(:role system :content "foo")
          (:role user :content "How to make coffe1?")
          (:role assistant :content "IDK.")
          (:role user :content "How to make coffe2? wtf")
          (:role system :content "other")]))

;;; -=-= Others

(defun oai-restapi--stream-supported (service model)
  "Check if the stream is supported by the service and model.
`SERVICE' is the service to use. `MODEL' is the model to use.
Used in oai.el"
  ;; stream not supported by openai o1 models
  (not (and (or (eq service 'openai) (eq service 'azure-openai))
            (or
             (string-prefix-p "o1-pro" model)))))

;; not used
(defun oai-restapi-switch-chat-model ()
  "Change `oai-restapi-con-model'."
  (interactive)
  (let ((model (completing-read "Model: "
                                (append oai-restapi-openai-known-chat-models
                                        '("claude-3-opus-latest" "claude-3-5-sonnet-latest" "claude-3-7-sonnet-latest"
                                          "gemini-2.5-pro-preview-03-25" "gemini-2.5-flash-preview-04-17" "gemini-2.0-flash" "gemini-2.0-pro-exp"
                                          "deepseek-chat" "deepseek-reasoner"))
                                nil t)))
    (setq oai-restapi-con-model model)))


(defun oai-restapi-get-buffers-for-element (&optional element)
  (if-let ((element (or element (oai-block-p))))
      (oai-timers--get-keys-for-variable (oai-block-get-header-marker element))
    nil))

(provide 'oai-restapi)

;;; oai-restapi.el ends here
