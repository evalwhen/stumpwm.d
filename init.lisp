;; -*-lisp-*-
;;;; init.lisp --- Main configuration file for StumpWM

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
;; This file contains the primary entry-point for the StumpWM configuration. It
;; configures some overall settings and loads the additional configuration files.
;;
;;; Code:

;;; Quicklisp configuration
;; #-quicklisp
;;  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
;;                                         (user-homedir-pathname))))
;;    (when (probe-file quicklisp-init)
;;      (load quicklisp-init)))

;;; StumpWM
(in-package :stumpwm)
(setf *default-package* :stumpwm)

;; Determine on which machine StumpWM is running
(setf *is-work-machine-p* (equal 1 1)
      ;; (equal (machine-instance)
      ;;        "anathema")
      )

(setf *is-personal-machine-p* (equal 1 1)
      ;; (equal (machine-instance)
      ;;        "tardigrade")
      )

;; Set module directory
(set-module-dir "~/.stumpwm.d/modules/")

;; Change prefix from default C-t
;; (set-prefix-key (kbd "C-t"))

;; Some basics
(setf *startup-message* nil
      *suppress-abort-messages* t
      *shell-program* (getenv "SHELL")
      *mouse-focus-policy* :click)

(when *initializing*
  (which-key-mode))

;;; Load modules
(load-module "end-session")
;; (load-module "mpd")
(load-module "battery-portable")
(load-module "cpu")
(load-module "mem")
(load-module "wifi")
(load-module "winner-mode")
;; control, replace with pamixer
(load-module "stump-volume-control")
;; (load-module "stump-nm")


(ql:quickload "cl-ppcre")
(load-module "swm-ssh")

(ql:quickload "clx-truetype")
(load-module "ttf-fonts")
(xft:cache-fonts)

;; Connect to mpd server
;; (mpd:mpd-connect)


;; TODO: for voice ;;; Load additional configuration files
(defvar *config-path* "~/.stumpwm.d/")

(defun al/load (filename)
  "Load the file called FILENAME from the configuration path."
  (load (concat *config-path* filename ".lisp")))

(al/load "font")
(al/load "icons")
(al/load "colours")
(al/load "applications")

(al/load "util")

;; (al/load "theme")
;;(al/load "placement")
(al/load "backlight")
(al/load "commands")
(al/load "desktop")
(al/load "keybindings")
(al/load "mode-line")

(al/load "autostart")

;; (al/load "pamixer")

;; Useful for debugging
(setq *debug-level* 5)
(redirect-all-output (data-dir-file "debug-output" "txt"))

(defun load-swank ()
  "Load a swank server"
  (ql:quickload :slynk)
  (require 'slynk)
  (slynk:create-server :dont-close t)
  )
;; (load-wank)



;; Local Variables:
;; jinx-local-words: "Quicklisp autostart backlight clx cpu init mem mpd pamixer ppcre quicklisp stumpwm swm tardigrade truetype ttf util wifi"
;; End:
