;;; advent.el --- Advent of Code helpers

;; Author: Keegan Carruthers-Smith

;;; Commentary:

;; Simple adventofcode.com helper which downloads todays input as well as open
;; todays question.  Also a helper to submit an answer.
;;
;; Ensure you have logged in with advent-login.  Once logged in, just call the
;; function advent.
;;
;; Demo at https://asciinema.org/a/ypGwNO8JyPpIEXz7CC7ZkaOFp
;;
;; Modified by Kiran Shila to use this project structure

;;; Code:

(require 'url)

(defvar advent-dir
  default-directory
  "The directory you are doing advent of code in.")

(defun advent-login (session)
  "Login to adventofcode.com.
Argument SESSION session cookie value."
  (interactive "sValue of session cookie from logged in browser: ")
  (url-cookie-store "session" session "Thu, 25 Dec 2027 20:17:36 -0000" ".adventofcode.com" "/" nil))

(defun advent (day year)
  "Load todays adventofcode.com problem and input.
Optional argument DAY Load this day instead.  Defaults to today.
Optional argument YEAR Load this year instead. Defaults to this year"
  (interactive
   (if current-prefix-arg
       (rest (calendar-read-date))
     (list (advent--day) (advent--year))))
  (delete-other-windows)
  (split-window-right)
  (advent-input day year))

(defun advent-submit (answer level &optional day year)
  "Submits ANSWER for LEVEL to todays adventofcode.com problem.
Argument LEVEL is either 1 or 2.
Optional argument DAY is the day to submit for.  Defaults to today.
Optional argument YEAR is the year to submit for. Defaults to this year."
  (interactive
   (list
    ;; answer
    (let ((answer-default (advent--tag-default)))
      (read-string
       (cond
        ((and answer-default (> (length answer-default) 0))
         (format "Submit (default %s): " answer-default))
        (t "Submit: "))
       nil nil answer-default))
    ;; level
    (read-string "Level (1 or 2): ")))
  (let* ((day (or day (advent--day)))
         (year (or year (advent--year)))
         (url (format "http://adventofcode.com/%d/day/%d/answer" year day))
         (url-request-method "POST")
         (url-request-data (format "level=%s&answer=%s" level answer))
         (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded"))))
    (eww-browse-url url)))

(defun advent-input (day year)
  "Load todays adventofcode.com input in other window.
Optional argument DAY Load this day instead.  Defaults to today.
Optional argument YEAR Load this year instead. Deafults to this year."
  (interactive
   (if current-prefix-arg
       (rest (calendar-read-date))
     (list (advent--day) (advent--year))))
  (let* ((url (format "http://adventofcode.com/%d/day/%d/input" year day))
         (dir (format "%sresources/%d/%d" advent-dir year day))
         (file (format "%s/input" dir)))
    (if (not (file-exists-p file))
        (url-retrieve url 'advent--download-callback (list file))
      (find-file-other-window file))))

(defun advent--download-callback (status file)
  (if (plist-get status :error)
      (message "Failed to download todays advent %s" (plist-get status :error))
    (mkdir (file-name-directory file) t)
    (goto-char (point-min))
    (re-search-forward "\r?\n\r?\n")
    (write-region (point) (point-max) file)
    (find-file-other-window file)))

(defun advent--day ()
  (elt (decode-time (current-time) "America/New_York") 3))

(defun advent--year ()
  (elt (decode-time (current-time) "America/New_York") 5))

(defun advent--tag-default ()
  "Copied version of grep-tag-default."
  (or (and transient-mark-mode mark-active
           (/= (point) (mark))
           (buffer-substring-no-properties (point) (mark)))
      (funcall (or find-tag-default-function
                   (get major-mode 'find-tag-default-function)
                   'find-tag-default))
      ""))

;;; advent.el ends here
