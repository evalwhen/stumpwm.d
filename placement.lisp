;;;; placement.lisp --- Configuration concerning groups, frames, and windows

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
;; Configures which groups should be created and the placement of some commonly
;; used applications.
;;
;;; Code:

;;; Groups
(defvar *group-emacs*  (al/util:icon-string +al/icon-code+))
(defvar *group-other*  (al/util:icon-string +al/icon-otter+))
(defvar *group-social* (al/util:icon-string +al/icon-communication+))
(defvar *group-web*    (al/util:icon-string +al/icon-browser+))

;; Create groups
(when *initializing*
  (grename *group-emacs*)
  (gnewbg *group-web*)
  (gnewbg *group-social*)
  (gnewbg *group-other*))

;;; Automatically move applications to preferred groups
(define-frame-preference *group-emacs*
    (0 nil t :class "Emacs"))

(define-frame-preference *group-web*
    (0 nil t :class "firefox")
  (0 nil t :class "LibreWolf")
  (0 nil t :class "Chromium"))

(define-frame-preference *group-other*
    (0 nil t :class "FreeTube")
  (0 nil t :class "Zotero"))

(define-frame-preference *group-social*
    (0 nil t :class "Jitsi Meet")
  (0 nil t :class "Rocket.Chat")
  (0 nil t :class "Signal"))

;; Winner-mode
(defvar *al/winner-commands*
  '(delete-window-and-frame
    hsplit-and-focus
    vsplit-and-focus
    stumpwm:balance-frames
    stumpwm:iresize
    stumpwm:move-window
    stumpwm:move-windows-to-group
    stumpwm:move-window-to-group
    stumpwm:next
    stumpwm:only
    stumpwm:pull-from-windowlist))

(add-hook *post-command-hook*
          (lambda (command)
            (when (member command winner-mode:*default-commands*)
              (winner-mode:dump-group-to-file))))

;; Local Variables:
;; jinx-local-words: "FreeTube LibreWolf Zotero firefox"
;; End:
