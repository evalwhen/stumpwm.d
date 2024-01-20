;;;; mode-line-updates.lisp --- Mode line indicators for package updates

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
;; A module to show the number of package updates that are available. This
;; module is written for Arch Linux and relies on its Pacman package manager.
;;
;; Dependencies:
;; * checkupdates from pacman-contrib (See URL `https://gitlab.archlinux.org/pacman/pacman-contrib')
;; * paru (See URL `https://github.com/morganamilo/paru')
;; * wc from the GNU core utilities (see URL `https://www.gnu.org/software/coreutils/')
;;
;;; Code:

(defpackage :al/package-updates
  (:use #:cl #:stumpwm #:al/util)
  (:export :updates-mode-line))

(in-package :al/package-updates)

;;; Pacman
(defvar pacman-timer nil)
(defvar pacman-update-time 1800
  "Time between two checks whether system package updates are available (in seconds).")
(defvar number-of-updates 0
  "The number of available system package updates on last check.")

(defun get-pacman-updates ()
  "Get the number of system packages that have an update available."
  (sb-thread:make-thread
   (lambda ()
     (setf number-of-updates
           (parse-integer (trim-newline (run-shell-command "checkupdates | wc -l" t)))))))

;;; AUR
(defvar aur-timer nil)
(defvar aur-update-time 2600
  "Time between two checks whether aur package updates are available (in seconds).")
(defvar number-of-aur-updates 0
  "The number of available aur package updates on last check.")

(defun get-aur-updates ()
  "Get the number of aur packages that have an update available."
  (sb-thread:make-thread
   (lambda ()
     (setf number-of-aur-updates
           (parse-integer (trim-newline (run-shell-command "paru -Qua | wc -l" t)))))))

;;; Mode line
(defun updates-mode-line ()
  "Print the number of available package updates. If no updates are available,
only the icon is printed, using the default mode line foreground colour. If
updates are available, the icon is printed, coloured to attract attention,
followed by two integers consecutively indicating how many system and aur
package updates are available."
  (unless pacman-timer
    (setf pacman-timer
          (run-with-timer 0 pacman-update-time #'get-pacman-updates)))
  (unless aur-timer
    (setf aur-timer
          (run-with-timer 0 aur-update-time #'get-aur-updates)))
  (if (or (> number-of-updates 0)
          (> number-of-aur-updates 0))
      (format nil "~a ~d ~d"
              (colour-icon stumpwm::+al/icon-pacman+ :fg stumpwm::+al/colour-mode-line-info+)
              number-of-updates
              number-of-aur-updates)
      (format nil "~a"
              (icon-string stumpwm::+al/icon-pacman+))))

;; Local Variables:
;; jinx-local-words: "Pacman aur checkupdates contrib pacman paru"
;; End:
