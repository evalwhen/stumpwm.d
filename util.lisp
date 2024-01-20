;;;; util.lisp --- Some (handy) utility functions

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
;; Some helper functions used throughout the configuration.
;;
;; Dependencies:
;; * dunst (see URL `https://dunst-project.org/')
;;
;;; Code:

(defpackage :al/util
  (:use #:cl #:stumpwm)
  (:export #:trim-newline
           #:colour-string
           #:colour-icon
           #:bold-string
           #:icon-string
           #:dunst-notification))

(in-package :al/util)

;;; Font selection
(defun bold-string (str)
  "Surround STR with font tags such that it is bold when printed.
See `set-font' in font.lisp"
  (concat stumpwm::+al/font-bold+ str stumpwm::+al/font-main+))

(defun icon-string (str)
  "Surround STR with font tags such that it is interpreted as an element of the
icon font. See `set-font' in font.lisp"
  (concat stumpwm::+al/font-icon+ str stumpwm::+al/font-main+))

;;; String manipulation
(defun trim-newline (string)
  "Trim any newlines from the given string STRING."
  (string-trim (string #\newline) string))

(defun colour-string (str &key (fg stumpwm::+al/colour-accent+) bg)
  "Make STR a string with FG as foreground colour and BG as background colour.
The colours can be NIL, or a string containing either a single digit indicating a
number from `*colors*' list or \"#XXXXXX.\" representing a hex colour code."
  (let ((fc (and (stringp fg)
                 (concat "^(:fg "
                         (if (= 1 (length fg))
                             fg
                             (concat "\"" fg "\""))
                         ")")))
        (bc (and (stringp bg)
                 (concat "^(:bg "
                         (if (= 1 (length bg))
                             bg
                             (concat "\"" bg "\""))
                         ")"))))
    (concat "^[" fc bc str "^]")))

(defun colour-icon (icon &key (fg stumpwm::+al/colour-accent+) bg)
  "Make ICON a string with FG as foreground colour and BG as background colour.
The colours can be NIL or a string containing either a single digit indicating a
number from `*colors*' list or \"#XXXXXX.\" representing a hex colour code."
  (icon-string (colour-string icon :fg fg :bg bg)))

;;; Notifications
(defun dunst-notification (icon summary message &key id priority timeout)
  "Use dunstify to display a notification with given ICON, SUMMARY, and MESSAGE.
If not provided the key arguments are set the defaults as configured for Dunst."
  (let ((command
    (concat "dunstify "
            (when priority (concat " -u " priority))
            (when timeout (concat " -t " timeout))
            (when id (concat " -r " id)))))
    (run-shell-command (concat command " -i " icon " " summary " " message))))

;; Local Variables:
;; jinx-local-words: "dunstify"
;; End:
