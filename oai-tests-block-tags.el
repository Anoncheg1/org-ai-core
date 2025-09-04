;;; oai-tests-block-tags.el ---  -*- lexical-binding: t -*-
;; Copyright (c) 2025 github.com/Anoncheg1,codeberg.org/Anoncheg
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Author: <github.com/Anoncheg1,codeberg.org/Anoncheg>
;; Keywords: tools, async, callback
;; URL: https://github.com/Anoncheg1/async1

;;; License

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

;;; Code:

;;; - Help functions ------------------------------------------------

(require 'oai-block-tags)
(require 'ert)

;; (eval-buffer) or (load-file "path/to/async-tests.el")
;; Running Tests: Load the test file and run:
;; (eval-buffer)
;; (ert t)
;; to execute all tests. Individual tests can be run with (ert 'test-name).

;;; - Tests --------------------------------------------------------

(ert-deftest oai-block-tags--read-file-to-string-safe--read-ok ()
  "Should read a regular readable file and return its contents."
  (let ((tmpfile (make-temp-file "oai-test")))
    (unwind-protect
        (progn
          (write-region "Hello, test!" nil tmpfile)
          (should (equal (oai-block-tags--read-file-to-string-safe tmpfile)
                         "Hello, test!")))
      (delete-file tmpfile))))

(ert-deftest oai-block-tags--read-file-to-string-safe--file-missing ()
  "Should signal user-error if the file does not exist."
  (should-error (oai-block-tags--read-file-to-string-safe "/no/such/file")
                :type 'user-error))

(ert-deftest oai-block-tags--read-file-to-string-safe--nonregular ()
  "Should signal user-error if path is not a regular file."
  (let ((tmpdir (make-temp-file "oai-test-dir" t)))
    (unwind-protect
        (should-error (oai-block-tags--read-file-to-string-safe tmpdir)
                      :type 'user-error)
      (delete-directory tmpdir))))

(ert-deftest oai-block-tags--read-file-to-string-safe--unreadable ()
  "Should signal user-error if the file is not readable."
  (let ((tmpfile (make-temp-file "oai-test-unreadable")))
    (unwind-protect
        (progn
          (write-region "not readable" nil tmpfile)
          (set-file-modes tmpfile 0)
          (should-error (oai-block-tags--read-file-to-string-safe tmpfile)
                        :type 'user-error))
      ;; Restore permissions so we can delete it
      (set-file-modes tmpfile #o600)
      (delete-file tmpfile))))

(ert-deftest oai-block-tags--read-file-to-string-safe--with-coding ()
  "Should honor the coding argument; reading ASCII content as UTF-8 should work."
  (let ((tmpfile (make-temp-file "oai-test-coding")))
    (unwind-protect
        (progn
          (write-region "abc" nil tmpfile)
          (should (equal
                   (oai-block-tags--read-file-to-string-safe tmpfile 'utf-8)
                   "abc")))
      (delete-file tmpfile))))
;;; provide
(provide 'oai-tests-block-tags)

;;; oai-tests-block-tags.el ends here
