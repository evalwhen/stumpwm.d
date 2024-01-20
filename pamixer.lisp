;;;; pamixer.lisp --- Functions to query state of default sink and source via pamixer

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
;; A module for pamixer that allows to control the volume of the default sink
;; and source. Furthermore, functions to display the appropriate information
;; in the mode line are provided.
;;
;; Dependencies:
;; * pamixer (see URL `https://github.com/cdemoulins/pamixer')
;; * pactl provided my pulseaudio
;; * ripgrep (See URL `https://github.com/BurntSushi/ripgrep')
;;
;;; Code:

(defpackage :al/pamixer
  (:use #:cl #:stumpwm)
  (:export #:is-muted-p
           #:increase-volume
           #:decrease-volume
           #:get-volume
           #:toggle-mute
           #:source-toggle-mute
           #:source-is-muted-p
           #:source-increase-volume
           #:source-decrease-volume
           #:source-get-volume
           #:headphones-plugged-in-p
           #:pamixer-mode-line)
  (:import-from :al/util :trim-newline)
  (:import-from :al/util :colour-icon))

(in-package :al/pamixer)

;;;; Code:
(defun pamixer-run (args &optional (wait-output nil))
  "Execute the pamixer command specified by the ARGS"
  (run-shell-command (concat "pamixer " args) wait-output))

;;; Default sink
(defun pamixer-run-sink (args &optional (wait-output nil))
  "Execute a command on the default sink"
  (pamixer-run args wait-output))

(defun is-muted-p ()
  "Return T if the default sink is currently muted, NIL otherwise"
  (string-equal "true" (trim-newline (pamixer-run-sink "--get-mute" t))))

(defun get-volume ()
  "Returns the current volume of the default sink as a percentage"
  (parse-integer (pamixer-run-sink "--get-volume" t)))

(defun toggle-mute ()
  "Toggle whether the default sink is muted"
  (pamixer-run-sink "--toggle-mute"))

(defun increase-volume (&optional (step 5))
  "Increase the volume of the default sink with STEP percent"
  (when (is-muted-p)
    (toggle-mute))
  (pamixer-run-sink (format nil "--increase ~d" step)))

(defun decrease-volume (&optional (step 5))
  "Decrease the volume of the default sink with STEP percent"
  (when (is-muted-p)
    (toggle-mute))
  (pamixer-run-sink (format nil "--decrease ~d" step)))

;;; Default source
(defun pamixer-run-source (args &optional (wait-output nil))
  "Execute a command on the default source"
  (pamixer-run (concat "--default-source " args) wait-output))

(defun source-is-muted-p ()
  "Return T if default source is currently muted, NIL otherwise"
  (string-equal "true" (trim-newline (pamixer-run-source "--get-mute" t))))

(defun source-is-active-p ()
  "Return T if default source is not muted, NIL otherwise"
  (not (source-is-muted-p)))

(defun source-toggle-mute ()
  "Toggle whether the default source is muted"
  (pamixer-run-source "--toggle-mute")
  (update-sourcep))

(defun source-increase-volume (&optional (step 5))
  "Increase the volume of the default sink with STEP percent"
  (when (source-is-muted-p)
    (source-toggle-mute))
  (pamixer-run-source (format nil "--increase ~d" step)))

(defun source-decrease-volume (&optional (step 5))
  "Decrease the volume of the default sink with STEP percent"
  (when (source-is-muted-p)
    (source-toggle-mute))
  (pamixer-run-source (format nil "--decrease ~d" step)))

(defun source-get-volume ()
  "Returns the current volume of the default source as a percentage or the
string \"muted\" if it is currently muted"
  (parse-integer (pamixer-run-source "--get-volume" t)))

;;; Headphones
(defun headphones-plugged-in-p ()
  "Return T if headphones are plugged in, NIL otherwise."
  (when (search "headphones" (run-shell-command "pactl list sinks | rg 'active port:'" t))
    t))

;;; Mode line
(defvar headphonesp nil "Whether headphones where plugged on last check.")
(defvar headphones-timer nil)
(defvar headphones-refresh-time 600
  "The time between checks whether the headphones are plugged in (in seconds)")

(defun update-headphonesp ()
  "Check whether headphones are plugged in and set `headphonesp' appropriately."
  (setf headphonesp (headphones-plugged-in-p)))

(defvar sourcep nil "Whether the default source is active, that is not muted.")

(defun update-sourcep ()
  "Check whether default source is active and set `sourcep' appropriately."
  (setf sourcep (source-is-active-p)))

(defun pamixer-mode-line ()
  "Generate the mode line status information for the default sink and, if unmuted,
the default source. In case headphones are connected a corresponding indicator
is generated as well."
  (unless headphones-timer
    (setf headphones-timer (run-with-timer 0 headphones-refresh-time #'update-headphonesp)))
  (concat
   (when headphonesp
     (format nil "~a " (colour-icon stumpwm::+al/icon-headphones+)))
   (when sourcep
     (format nil "~a ~3d% "
             (colour-icon stumpwm::+al/icon-microphone+)
             (source-get-volume)))
   (if (is-muted-p)
       (format nil "~a " (colour-icon stumpwm::+al/icon-volume-muted+))
       (format nil "~a ~3d% " (colour-icon stumpwm::+al/icon-volume-on+) (get-volume)))))

;; Local Variables:
;; jinx-local-words: "pactl pamixer pulseaudio ripgrep"
;; End:
