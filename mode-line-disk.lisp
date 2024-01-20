;;;; mode-line-disk.lisp --- Simple disk usage reporter

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
;; A simple disk usage reporter inspired by the disk module in contrib. This
;; module shows the percentage of disk space currently in use in the mode line
;; only when the percentage is higher than a configurable threshold. Otherwise,
;; nothing is printed to the mode line.
;;
;;; Code:

(defpackage :al/disk
  (:use #:cl #:stumpwm)
  (:export :disk-mode-line)
  (:import-from :al/util :trim-newline)
  (:import-from :al/util :colour-icon))

(in-package :al/disk)

(ql:quickload 'cl-diskspace)

(defvar timer nil)

(defvar update-time 3600
  "Time between updates for checking disk usage (in seconds).")

(defvar path "/"
  "The disk partition for which to query the percentage used.")

(defvar usage 0
  "The percentage of disk space used at the most recent query.")

(defvar threshold 75
  "The usage percentage above which an indicator should be shown in the mode line.")

(defun disk-used (path)
  "Calculate how much disk space is currently used in PATH."
  (- (diskspace:disk-total-space path)
     (diskspace:disk-free-space path)))

(defun disk-used-percent (path)
  "Calculate how much percent of disk space is currently used in PATH."
  (let ((value
          (truncate (* 100
                       (/ (disk-used path)
                          (diskspace:disk-total-space path))))))
    (format nil "~d" value)))

(defun update-disk-usage ()
  "Update the value `usage' to the percentage of disk space currently used in `path'."
  (setf usage (parse-integer (disk-used-percent path))))

(defun disk-mode-line ()
  "Conditionally construct the mode line indicator if any of the partitions is
used for more than `threshold' percent."
  (unless timer
    (setf timer (run-with-timer 0 update-time #'update-disk-usage)))
    (when (> usage threshold)
      (format nil "~a ~{~d% ~}" (colour-icon stumpwm::+al/icon-hdd+) usage)))

;; Local Variables:
;; jinx-local-words: "contrib"
;; End:
