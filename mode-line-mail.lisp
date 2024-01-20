;;;; mode-line-mail.lisp --- Helper functions for mail

;;  This file is NOT part of StumpWM.
;;
;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Use mu to check for new email messages in the inbox folder of configurable
;; maildirs.
;;
;; Dependencies:
;; * mu (see URL `https://www.djcbsoftware.nl/code/mu/')
;; * wc from the GNU core utilities (see URL `https://www.gnu.org/software/coreutils/')
;;
;;; Code:

(defpackage :al/mail
  (:use #:cl #:stumpwm #:al/util)
  (:export :mail-mode-line))

(in-package :al/mail)

;;; Code:
(defvar mu-mail-timer nil)

(defvar mu-mail-update-time 180
  "Time between executing the queries (in seconds).")

(defvar maildirs '("fastmail" "redpencil")
  "The folder name(s) of the maildir(s) from which to query the inbox.")

(defvar mu-mail-unread '(0 0)
  "Counter(s) for the number of unread messages in `maildirs'.")

(defun construct-mu-find-unread-command (maildir)
  "Construct a mu command to find the number of unread messages in MAILDIR's inbox."
  (concat "mu find \"flag:unread AND maildir:/" maildir "/inbox\" | wc -l"))

(defun run-mu-command (maildir)
  "Get the number of unread messages in MAILDIR's inbox."
  (parse-integer
   (trim-newline
    (run-shell-command (construct-mu-find-unread-command maildir) t))))

(defun update-mu-mail-unread ()
  "Retrieve the number of unread messages for each inbox in `maildirs'."
  (setf mu-mail-unread
        (mapcar #'(lambda (maildir) (run-mu-command maildir)) maildirs)))

(defun mail-mode-line ()
  "Construct the mode line indicating the number of unread messages. If there are
no unread messages an icon in the default mode line foreground colour is printed.
If there is at least one unread message, the icon is coloured and followed by
integers indicating how many unread messages there are following the order as
specified by `maildirs'."
  (unless mu-mail-timer
    (setf mu-mail-timer
          (run-with-timer 0 mu-mail-update-time #'update-mu-mail-unread)))
  (if (> (count-if #'(lambda (n) (> n 0)) mu-mail-unread) 0)
      (format nil "~a ~{~d ~}"
              (colour-icon stumpwm::+al/icon-mail+ :fg stumpwm::+al/colour-mode-line-info+)
              mu-mail-unread)
      (format nil "~a " (icon-string stumpwm::+al/icon-mail+))))

;; Local Variables:
;; jinx-local-words: "MAILDIR's fastmail maildir maildirs"
;; End:
