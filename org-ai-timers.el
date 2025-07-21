;;; org-ai-timers.el --- org-ai-core Timers and notifications. -*- lexical-binding: t; -*-

;; `interrupt-request-func' is for implementation of interrupt that `org-ai-openai--interrupt-url-request'
;;; - Variables
(defcustom org-ai-timers-echo-gap 0.2
  "Echo update interval."
  :type 'float
  :group 'org-ai)

(defcustom org-ai-timers-duration 25
  "The total duration in seconds for which the timer should run.
Delay after which it will be killed."
  :type 'integer
  :group 'org-ai)

(defvar org-ai-timers--global-progress-reporter nil
  "Progress-reporter for request response to indical waiting.")

(defvar org-ai-timers--global-progress-timer nil
  "Timer for updating the progress reporter.")

(defvar org-ai-timers--global-progress-timer-remaining-ticks 0
  "The time when the timer started.")

(defvar org-ai-timers--current-timer nil
  "Timer for waiting for url buffer.")
(make-variable-buffer-local 'org-ai-timers--current-timer)

(defvar org-ai-timers--current-timer-remaining-ticks 0
  "The time when the timer started.")
(make-variable-buffer-local 'org-ai-timers--current-timer-remaining-ticks)

(defvar org-ai-timers--global-progress-reporter-waiting-string "Waiting for a response")

(defvar org-ai-timers--element-marker-variable-dict nil
  "Allow to store url buffer per block.
Intented for usage with `org-ai-block--copy-header-marker' and keep pairs of
( block marker-> url-retrieve buffer).
Should be used for interactive interrup of request only.
We use pairs of (block-header-marker url-buffer)")

;;; - variable-dict
(defun org-ai-timers--get-variable (block-header-marker)
  "Get variable for key.
Key is Indented for usage with `org-ai-block-get-header-marker'.
Use ELEMENT only in current moment."
    ;; (print "org-ai-timers--get-variable bm, varibles, get:")
    ;; (print  bm)
    ;; (print org-ai-timers--element-marker-variable-dict)
    ;; (print (alist-get bm org-ai-timers--element-marker-variable-dict))
    (alist-get block-header-marker org-ai-timers--element-marker-variable-dict nil nil #'equal))

(defun org-ai-timers--set-variable (value block-header-marker)
  "Assign value to key.
Indented for usage with `org-ai-block-get-header-marker'."
    (if (eq value nil)
        (setf (alist-get block-header-marker org-ai-timers--element-marker-variable-dict nil 'remove #'equal) nil)
      ;; else
      (setf (alist-get block-header-marker org-ai-timers--element-marker-variable-dict nil nil #'equal) value)))

(defun org-ai-timers--remove-variable (value)
  (setq org-ai-timers--element-marker-variable-dict
        (rassq-delete-all value org-ai-timers--element-marker-variable-dict))) ; for buffer eq is ok

;; (org-ai-timers--set-variable 1 :block-header-marker 'aa)
;; (org-ai-timers--set-variable 2 :block-header-marker 'cc)
;; (org-ai-timers--set-variable 3 :block-header-marker 'bb)
;; (org-ai-timers--get-all-variables)
;; (print org-ai-timers--element-marker-variable-dict)
;; (org-ai-timers--remove-variable 1)

(defun org-ai-timers--get-all-variables ()
  "Get all url-buffers."
  (mapcar #'cdr org-ai-timers--element-marker-variable-dict))

;; (defun org-ai-timers--clear-variables () ; too simple
;;   (setq org-ai-timers--element-marker-variable-dict nil))

;;; - Timers Global
(defun org-ai-timers--stop-global-progress-reporter (&optional failed)
  "Stop global timer of progress reporter for restart or at success.
Don't clear list of url-buffers.
Called in
`org-ai-timers--progress-reporter-run' for restart,
`org-ai-timers--interrupt-all-requests' for full stop."
  (org-ai--debug "org-ai-timers--stop-global-progress-reporter"
                 org-ai-timers--global-progress-reporter
                 org-ai-timers--global-progress-timer)
  ;; finish notifications
  (when org-ai-timers--global-progress-reporter
    (if failed ; timeout
        (progn ; from `url-queue-kill-job'
          ;; (progress-reporter-done org-ai-timers--global-progress-reporter)
          (progress-reporter-update org-ai-timers--global-progress-reporter nil "- Connection failed")
          (message (concat org-ai-timers--global-progress-reporter-waiting-string "- Connection failed")))
      ;; else - echo success
      (progress-reporter-done org-ai-timers--global-progress-reporter))
    ;; when
    (setq org-ai-timers--global-progress-reporter nil))

  ;; clear time
  (when org-ai-timers--global-progress-timer
    (org-ai--debug "org-ai-timers--stop-global-progress-reporter wtf1")
    (cancel-timer org-ai-timers--global-progress-timer)
    (org-ai--debug "org-ai-timers--stop-global-progress-reporter wtf2")
    (setq org-ai-timers--global-progress-timer nil)
    (org-ai--debug "org-ai-timers--stop-global-progress-reporter wtf3")
    (setq org-ai-timers--global-progress-timer-remaining-ticks 0)))

(defun org-ai-timers--update-global-progress-reporter (&optional failed)
  "Count url-buffers and stop reporter if it is empty.
Called from
`org-ai-openai-stop-url-request',
`org-ai-timers--interrupt-current-request'
`org-ai-timers--progress-reporter-run'."
  (org-ai--debug "org-ai-timers--update-global-progress-reporter len:"
                 (length (org-ai-timers--get-all-variables))
                 (org-ai-timers--get-all-variables))
  (let ((count (length (org-ai-timers--get-all-variables))))
    (org-ai-update-mode-line count)
    (when (eql count 0)
      (org-ai-timers--stop-global-progress-reporter failed))))

(defun org-ai-timers--interrupt-all-requests (interrupt-request-func &optional failed)
  "Interrup all url requests and stop global timer.
Called from
`org-ai-openai-stop-url-request' when not at some block,
`org-ai-timers--progress-reporter-run' by global timer."
  (org-ai--debug "org-ai-timers--interrupt-all-requests" interrupt-request-func failed)
  ;; stop requests
  (mapc (lambda (url-buffer)
          (funcall interrupt-request-func url-buffer))
        (org-ai-timers--get-all-variables))
  ;; clear list
  (setq org-ai-timers--element-marker-variable-dict nil)
  ;; (org-ai-timers--clear-variables)
  ;; stop global timer
  (org-ai-timers--stop-global-progress-reporter failed))

;; (defun org-ai-timers--stop-current-timer (url-buffer &optional failed)
;;   (org-ai--debug "org-ai-timers--stop-current-timer")
;;   ;; - Remove variable
;;   (org-ai-timers--remove-variable url-buffer)

;;   (if (eq (current-buffer) url-buffer)
;;       (progn
;;         (org-ai--debug "org-ai-timers--stop-current-timer if1")
;;         (when org-ai-timers--current-timer
;;           (cancel-timer org-ai-timers--current-timer)
;;           (setq org-ai-timers--current-timer nil)
;;           (setq org-ai-timers--current-timer-remaining-ticks 0)))
;;     ;; else
;;     (org-ai--debug "org-ai-timers--stop-current-timer if2")
;;     ;; - Clear time and kill buffer
;;     (when (and url-buffer (buffer-live-p url-buffer))
;;       (with-current-buffer url-buffer
;;         ;; - Stop url-buffer timer
;;         (when org-ai-timers--current-timer
;;           (cancel-timer org-ai-timers--current-timer)
;;           (setq org-ai-timers--current-timer nil)
;;           (setq org-ai-timers--current-timer-remaining-ticks 0)))))
;;   (org-ai-timers--update-global-progress-reporter failed))

;;; - Timers Local
(defun org-ai-timers--interrupt-current-request (url-buffer &optional interrupt-request-func failed)
  "Stop waiting for request, remove buffer from list, update global timer.
Called from
`org-ai--insert-stream-response' after receiving first chunk,
`org-ai--url-request-on-change-function' for  not stream after  reply or
\"DONE\" string found for stream.
`org-ai-openai-stop-url-request'."
  (org-ai--debug "org-ai-timers--interrupt-current-request"
                 url-buffer
                 (buffer-live-p url-buffer)
                 failed)
  ;;                (print org-ai-block--element-marker-variable-dict))
  ;; - Remove variable
  (org-ai-timers--remove-variable url-buffer)
  ;; ;; else
  ;; (org-ai-timers--set-variable nil :element element))
  ;; - Clear time and kill buffer
  (funcall interrupt-request-func url-buffer)
  ;; (if (or (eq (current-buffer) url-buffer)
  ;;         (not (buffer-live-p url-buffer)))
  ;;     (progn
  ;;       (org-ai--debug "org-ai-timers--interrupt-current-request if1" org-ai-timers--current-timer)
  ;;       ;; (when org-ai-timers--current-timer
  ;;       ;;   (cancel-timer org-ai-timers--current-timer)
  ;;       ;;   (setq org-ai-timers--current-timer nil)
  ;;       ;;   (setq org-ai-timers--current-timer-remaining-ticks 0))
  ;;       (when interrupt-request-func
  ;;         (funcall interrupt-request-func url-buffer)))
  ;;   ;; - else

  ;;   (org-ai--debug "org-ai-timers--interrupt-current-request if2")
  ;;   (when (and url-buffer (buffer-live-p url-buffer))
  ;;     (with-current-buffer url-buffer
  ;;       ;; - Stop url-buffer timer
  ;;       ;; (when org-ai-timers--current-timer
  ;;       ;;   (cancel-timer org-ai-timers--current-timer)
  ;;       ;;   (setq org-ai-timers--current-timer nil)
  ;;       ;;   (setq org-ai-timers--current-timer-remaining-ticks 0))
  ;;       )
  ;;       ;; - Kill buffer
  ;;       (when interrupt-request-func
  ;;         (funcall interrupt-request-func url-buffer))
  ;;       ))
    ;; - Update global timer
    (org-ai-timers--update-global-progress-reporter failed))

;;; - Main - constructor
(defun org-ai-timers--progress-reporter-run (url-buffer header-marker interrupt-request-func)
  "Start progress notification.
Run two timers in URL-BUFFER and global one.
Global one do echo notification, local kill url buffer.
Called from `org-ai-api-request'.

Set:
- `org-ai-timers--global-progress-reporter' - lambda that return a string,
- `org-ai-timers--global-progress-timer' - timer that output /-\ to echo area.
- `org-ai-timers--global-progress-timer-remaining-ticks'.
- `org-ai-timers--current-timer' - count life of url buffer,
- `org-ai-timers--current-timer-remaining-ticks'."
  (org-ai--debug "org-ai-timers--progress-reporter-run")
  ;; - save buffer
  (org-ai-timers--set-variable url-buffer header-marker)
  ;; - update mode-line
  ;; (org-ai-timers--update-global-progress-reporter)
  (org-ai-update-mode-line (length (org-ai-timers--get-all-variables))) ; count

  ;; - precalculate ticks based on duration, 25/ 0.2 = 125 ticks
  (setq org-ai-timers--global-progress-timer-remaining-ticks
        (round (/ org-ai-timers-duration org-ai-timers-echo-gap)))

  ;; - if exist, add remaining ticks
  (when (not org-ai-timers--global-progress-reporter)
    ;; else - create new - reporter
    (setq org-ai-timers--global-progress-reporter (make-progress-reporter org-ai-timers--global-progress-reporter-waiting-string)))

  (when (not org-ai-timers--global-progress-timer)
    ;; timer1
    (setq org-ai-timers--global-progress-timer
          (run-with-timer ; do not respect `with-current-buffer'
           1.0 org-ai-timers-echo-gap ; start after 1 sec
           (lambda ()
             "timer1 in current buffer"
             ;; expired?
             (if (or (<= org-ai-timers--global-progress-timer-remaining-ticks 0)
                     (not org-ai-timers--global-progress-reporter))
                 (progn
                   (org-ai--debug "org-ai-timers--progress-reporter-run expired")
                   (org-ai-timers--interrupt-all-requests interrupt-request-func 'failed))
               ;; else -  ticks -= 1
               (setq org-ai-timers--global-progress-timer-remaining-ticks
                     (1- org-ai-timers--global-progress-timer-remaining-ticks))
               (progress-reporter-update org-ai-timers--global-progress-reporter))))))

  ;; timer2 - request killer
  ;; (with-current-buffer url-buffer
  ;;   (setq-local org-ai-timers--current-timer-remaining-ticks
  ;;               org-ai-timers--global-progress-timer-remaining-ticks)
  ;;   (org-ai--debug "org-ai-timers--progress-reporter-run timer2 org-ai-timers--current-timer-remaining-ticks" (current-buffer) org-ai-timers--current-timer-remaining-ticks)
  ;;   (setq-local org-ai-timers--current-timer
  ;;         (run-with-timer
  ;;          1.0 org-ai-timers-echo-gap ; start after 1 sec
  ;;          (lambda ()
  ;;            "timer2 in current buffer"
  ;;            ;; expired?
  ;;            (if (<= org-ai-timers--current-timer-remaining-ticks 0)
  ;;                (progn
  ;;                  (org-ai--debug "org-ai-timers--progress-reporter-run timer2 is expired" (current-buffer) org-ai-timers--current-timer-remaining-ticks)
  ;;                  (org-ai-timers--interrupt-current-request url-buffer interrupt-request-func 'failed))

  ;;              ;; else -  ticks -= 1
  ;;              (setq org-ai-timers--current-timer-remaining-ticks
  ;;                    (1- org-ai-timers--current-timer-remaining-ticks))))
  ;;          )))
  )

;;; provide
(provide 'org-ai-timers)
;;; org-ai-timers.el ends here
