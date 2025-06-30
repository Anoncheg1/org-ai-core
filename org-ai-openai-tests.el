(require 'org-ai-openai)

(defun test-org-ai--strip-api-url ()
  "Runs tests for `org-ai--strip-api-url` explicitly for each case,
   without using a loop or an explicit assert function."

  (unless (string= (org-ai--strip-api-url "https://api.perplexity.ai/chat/completions") "api.perplexity.ai")
    (error "Test 1 Failed: https://api.perplexity.ai/chat/completions"))

  (unless (string= (org-ai--strip-api-url "http://www.example.com/path/to/file") "www.example.com")
    (error "Test 2 Failed: http://www.example.com/path/to/file"))

  ;; (unless (string= (org-ai--strip-api-url "ftp://some.server.org") "some.server.org")
  ;;   (error "Test 3 Failed: ftp://some.server.org"))

  (unless (string= (org-ai--strip-api-url "no-protocol.com/stuff") "no-protocol.com")
    (error "Test 4 Failed: no-protocol.com/stuff"))

  (unless (string= (org-ai--strip-api-url "http://www.google.com/search?q=elisp") "www.google.com")
    (error "Test 5 Failed: http://www.google.com/search?q=elisp"))

  (unless (string= (org-ai--strip-api-url "localhost:8080/app") "localhost:8080")
    (error "Test 6 Failed: localhost:8080/app"))

  (unless (string= (org-ai--strip-api-url "example.com") "example.com")
    (error "Test 7 Failed: example.com"))

  (unless (string= (org-ai--strip-api-url "https://sub.domain.co.uk") "sub.domain.co.uk")
    (error "Test 8 Failed: https://sub.domain.co.uk"))

  (unless (string= (org-ai--strip-api-url "domain.com/") "domain.com")
    (error "Test 9 Failed: domain.com/"))

  (unless (string= (org-ai--strip-api-url "localhost") "localhost")
    (error "Test 10 Failed: localhost"))

  ;; (unless (string= (org-ai--strip-api-url "") "")
  ;;   (error "Test 11 Failed: empty string"))

  (message "All individual tests passed for org-ai--strip-api-url!")
  t) ; Return t for success

(test-org-ai--strip-api-url)
