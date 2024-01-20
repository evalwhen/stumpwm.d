;;;; backlight.lisp --- Functions to set and query backlight

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
;; Module to set and query the screen backlight using the brightnessctl tool.
;;
;; Dependencies:
;; * brightnessctl (see URL `https://github.com/Hummer12007/brightnessctl')
;;
;;; Code:

(defpackage :al/backlight
  (:use #:cl #:stumpwm)
  (:export #:get-backlight-rel
           #:decrease-backlight
           #:increase-backlight))

(in-package :al/backlight)

(defvar *default-delta-pc* 10
  "The default delta, as percentage, for changing the backlight brightness.")

(defun run-backlight-operation (op &optional value)
  "Execute the brightnessctl command specified by OP."
  (run-shell-command (concat "brightnessctl " op value) t))

(defun get-max-backlight ()
  "Retrieve maximum backlight brightness."
  (parse-integer (run-backlight-operation "m")))

(defun get-backlight ()
  "Retrieve current backlight brightness."
  (parse-integer (run-backlight-operation "g")))

(defun get-backlight-rel ()
  "Retrieve current backlight brightness relative to the maximum, in other words
the current backlight brightness as a percentage."
  (round (* 100
            (float (/ (get-backlight)
                      (get-max-backlight))))))

(defun increase-backlight (&optional (delta *default-delta-pc*))
  "Increase the backlight brightness with DELTA percent."
  (run-backlight-operation "set " (format nil "+~d%" delta)))

(defun decrease-backlight (&optional (delta *default-delta-pc*))
  "Decrease the backlight brightness with DELTA percent."
  (run-backlight-operation "set " (format nil "~d%-" delta)))

;; Local Variables:
;; jinx-local-words: "brightnessctl"
;; End:
