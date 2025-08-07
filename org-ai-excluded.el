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


(defun org-ai-openai--get-greatest-variable (alist)
    (if (null alist)
        nil
      (car (sort alist (lambda (x y) (> (cdr x) (cdr y)))))))

;; (equal (org-ai-openai--get-greatest-variable '((bb . 3) (aa . 2))) '(bb . 3))
;; (equal (org-ai-openai--get-greatest-variable '((aa . 2) (bb . 3))) '(bb . 3))
;; (equal (org-ai-openai--get-greatest-variable '((cc . 1)))          '(cc . 1))
;; (equal (org-ai-openai--get-greatest-variable '())                  nil)

(defun apply-to-old-keys (seconds timed-alist func)
  "Remove keys from `timed-alist' whose timestamps are older than SECONDS seconds."
  (let ((current (time-to-seconds (current-time))))
    (mapc func (seq-filter
                (lambda (entry)
                  (<= (- current (time-to-seconds (cdr entry))) seconds))
                timed-alist)))

(defun org-ai--progress-reporter-update ()
  "2) interrupt
3) Remove keys"
  (apply-to-old-keys org-ai-progress-duration
                     org-ai-block--element-marker-variable-dict
                     (lambda ()
                       )))


(defun org-ai--progress-reporter-global-cancel (block-marker &optional failed)
  "Stop progress notification for element.
BLOCK-MARKER is marker for ai block header from
`org-ai-block-get-header-marker'."
  (org-ai-block--set-variable :value nil :block-header-marker block-marker)

  (when-let ((apply-to-old-keys org-ai-progress-duration
                                org-ai-block--element-marker-variable-dict

               (time-longest  (org-ai-openai--get-greatest-variable org-ai-block--element-marker-variable-dict))
  org-ai-block--element-marker-variable-dict

  (when org-ai--current-progress-reporter

    (if failed ; timeout
        (progn ; from `url-queue-kill-job'
          ;; (progress-reporter-done org-ai--current-progress-reporter)
          (progress-reporter-update org-ai--current-progress-reporter nil "- Connection failed")
          (message (concat org-ai--progress-reporter-waiting-string "- Connection failed"))
          (setq org-ai--current-progress-reporter nil)
          (org-ai-interrupt-current-request)
          ;; (when (buffer-live-p org-ai--last-url-buffer)
          ;;   (org-ai--kill-query-process))
          )
      ;; else success
      (progress-reporter-done org-ai--current-progress-reporter)
      (setq org-ai--current-progress-reporter nil)))
  ;; clear time
  (when org-ai--current-progress-timer
    (cancel-timer org-ai--current-progress-timer)
    (setq org-ai--current-progress-timer-remaining-ticks 0)))


(cl-defun org-ai-api-request-sync (service model timeout &optional &key prompt messages max-tokens temperature top-p frequency-penalty presence-penalty)
  "Return nil or result of `org-ai--normalize-response'.
To a"
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
    (org-ai--debug "org-ai-api-request-sync endpoint:" endpoint (type-of endpoint)
                   "request-data:" (org-ai--prettify-json-string url-request-data)
                   )
    (let ((url-request-buffer
           (url-retrieve-synchronously ; <- - - - - - - - -  MAIN
            endpoint
            t
            t
            timeout)))
      (if url-request-buffer
          (with-current-buffer url-request-buffer
            (org-ai--debug-urllib url-request-buffer)
            (org-ai--maybe-show-openai-request-error) ; TODO: change to RESULT by global customizable option

            ;; - read from url-buffer
            (when (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
              (goto-char url-http-end-of-headers)
              (let ((json-object-type 'plist)
                    (json-key-type 'symbol)
                    (json-array-type 'vector))
                (condition-case _err
                    ;; ;; (#s(org-ai--response role "assistant") #s(org-ai--response text "It seems ") #s(org-ai--response stop "length"))
                    ;; (let* ((res1 (buffer-substring-no-properties (point) (point-max)))
                    ;;        (res2 (json-read-from-string res1))
                    ;;        (res3 (org-ai--normalize-response res2))
                    ;;        (res4 (nth 1 res3))
                    ;;        (res5 (org-ai--response-payload res4))
                    ;;        (res (decode-coding-string res5 'utf-8)))
                    ;;   res
                    ;;   )
                    (decode-coding-string (org-ai--response-payload (nth 1
                                                                         (org-ai--normalize-response
                                                                          (json-read-from-string
                                                                           (buffer-substring-no-properties (point) (point-max))))))
                                          'utf-8)
                  (error nil)))))
        ;; else
        (print "org-ai-api-request-sync: timeout for request")
        (org-ai--debug "org-ai-api-request-sync: timeout for request:" service model messages)))))

(let ((service 'together)
      (model "meta-llama/Llama-3.3-70B-Instruct-Turbo-Free")
      (max-tokens 99)
      (temperature nil)
      (top-p nil)
      (frequency-penalty nil)
      (presence-penalty nil))
  (org-ai-api-request-sync service model
                           99
                           :messages  (vector (list :role 'system :content "Be good.")
                                              (list :role 'user :content "How to do staff?"))
                           :max-tokens max-tokens
                           :temperature temperature
                           :top-p top-p
                           :frequency-penalty frequency-penalty
                           :presence-penalty presence-penalty))

(defun my/sync-request (messages service model timeout max-tokens top-p temperature frequency-penalty presence-penalty)
  "Do synchronous request.
Return string of LLM answer as assistent."
  (let ((lst '(0 1 2 3)) ; retries
        ret)
    (while (and (setq lst (cdr lst)) ; make list shorter
                (not ret)) ; ret is not nil?
      ;; second request
      (setq ret (org-ai-api-request-sync service model timeout
                                          :messages messages
                                          :max-tokens max-tokens
                                          :temperature temperature
                                          :top-p top-p
                                          :frequency-penalty frequency-penalty
                                          :presence-penalty presence-penalty))
      (setq timeout (* timeout 2)))
    ret))

(my/sync-request '([(:role system :content "Be helpful. Now, plan research of 3 parts and do only first part to answer this user request:") (:role user :content "How to live?") (:role system :content "Research 2-th part and what was missed before.")] together "meta-llama/Llama-3.3-70B-Instruct-Turbo-Free" 1.0e+INF "How to live?" 20 nil nil nil nil))
