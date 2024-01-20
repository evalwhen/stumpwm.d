;;;; mode-line-battery.lisp --- Enrich contrib's battery-portable module mode line

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
;; A small wrapper around the battery-portable module from contrib to vary the
;; string printed in the mode line depending the battery state. This wrapper
;; uses sysfs to determine the state of the battery.
;;
;;; Code:

(defpackage :al/battery
  (:use #:cl #:stumpwm)
  (:export #:battery-mode-line)
  (:import-from :al/util :colour-icon))

(in-package :al/battery)

;;;; Code:
;; Configure battery-portable module
(setf battery-portable::*refresh-time* 180)

;; Wrapper for a prettier mode line string
(defvar sysfs "/sys/class/power_supply/BAT0/"
  "The sysfs folder to check for the battery status.")

(defvar status nil
  "The status of the battery when last checked..")

(defvar timer nil)

(defvar update-time 180
  "Time between two battery status checks (in seconds).")

(defun get-battery-status ()
  "Read the battery status via sysfs."
  (setf status
        (let ((status-file (concat sysfs "status")))
          (if (probe-file status-file)
              (with-open-file (in status-file)
                (read-line in))))))

(defun battery-mode-line ()
  "Generate the contents of the mode line battery depending on the current state
of the battery."
  (unless timer (setf timer (run-with-timer 0 update-time #'get-battery-status)))
  (cond
    ((or (equal status "Full") (equal status "Not charging"))
     (format nil "~a "    (colour-icon stumpwm::+al/icon-battery-charged+)))
    ((equal status "Discharging")
     (format nil "~a %B " (colour-icon stumpwm::+al/icon-battery-discharging+)))
    ((equal status "Charging")
     (format nil "~a %B " (colour-icon stumpwm::+al/icon-battery-charging+)))
    (t
     (format nil "~a %B " (colour-icon stumpwm::+al/icon-battery-discharging+)))))

;; Local Variables:
;; jinx-local-words: "contrib contrib's sysfs"
;; End:
