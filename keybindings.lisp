;;;; keybindings.lisp --- Custom keybindings

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
;; This files configures the keybindings and undefines standard keybindings
;; that are not used.
;;
;;; Code:

;;; Undefine unused default keys
(undefine-key *root-map* (kbd "c"))
(undefine-key *root-map* (kbd "t"))
(undefine-key *root-map* (kbd "v"))
(undefine-key *root-map* (kbd "C-a"))
(undefine-key *root-map* (kbd "C-b"))
(undefine-key *root-map* (kbd "C-c"))
(undefine-key *root-map* (kbd "C-e"))
(undefine-key *root-map* (kbd "F1"))
(undefine-key *root-map* (kbd "F2"))
(undefine-key *root-map* (kbd "F3"))
(undefine-key *root-map* (kbd "F4"))
(undefine-key *root-map* (kbd "F5"))
(undefine-key *root-map* (kbd "F6"))
(undefine-key *root-map* (kbd "F7"))
(undefine-key *root-map* (kbd "F8"))
(undefine-key *root-map* (kbd "F9"))
(undefine-key *root-map* (kbd "F10"))

(undefine-key *group-root-map* (kbd "A"))
(undefine-key *group-root-map* (kbd "C-k"))
(undefine-key *group-root-map* (kbd "C-N"))
(undefine-key *group-root-map* (kbd "C-w"))
(undefine-key *group-root-map* (kbd "C-RET"))
(undefine-key *group-root-map* (kbd "RET"))
(undefine-key *group-root-map* (kbd "DEL"))
;; (undefine-key *group-root-map* (kbd "0"))
;; (undefine-key *group-root-map* (kbd "1"))
;; (undefine-key *group-root-map* (kbd "2"))
;; (undefine-key *group-root-map* (kbd "3"))
;; (undefine-key *group-root-map* (kbd "4"))
;; (undefine-key *group-root-map* (kbd "5"))
;; (undefine-key *group-root-map* (kbd "6"))
;; (undefine-key *group-root-map* (kbd "7"))
;; (undefine-key *group-root-map* (kbd "8"))
;; (undefine-key *group-root-map* (kbd "9"))
(undefine-key *group-root-map* (kbd "\""))

(undefine-key *tile-group-root-map* (kbd "A"))
(undefine-key *tile-group-root-map* (kbd "f"))
(undefine-key *tile-group-root-map* (kbd "F"))
(undefine-key *tile-group-root-map* (kbd "k"))
(undefine-key *tile-group-root-map* (kbd "o"))
(undefine-key *tile-group-root-map* (kbd "P"))
(undefine-key *tile-group-root-map* (kbd "q"))
(undefine-key *tile-group-root-map* (kbd "s"))
(undefine-key *tile-group-root-map* (kbd "S"))
(undefine-key *tile-group-root-map* (kbd "W"))
(undefine-key *tile-group-root-map* (kbd "X"))
(undefine-key *tile-group-root-map* (kbd "C-l"))
(undefine-key *tile-group-root-map* (kbd "C-n"))
(undefine-key *tile-group-root-map* (kbd "C-p"))
(undefine-key *tile-group-root-map* (kbd "C-SPC"))
(undefine-key *tile-group-root-map* (kbd "C-0"))

(undefine-key *tile-group-root-map* (kbd "C-1"))
(undefine-key *tile-group-root-map* (kbd "C-2"))
(undefine-key *tile-group-root-map* (kbd "C-3"))
(undefine-key *tile-group-root-map* (kbd "C-4"))
(undefine-key *tile-group-root-map* (kbd "C-5"))
(undefine-key *tile-group-root-map* (kbd "C-6"))
(undefine-key *tile-group-root-map* (kbd "C-7"))
(undefine-key *tile-group-root-map* (kbd "C-8"))
(undefine-key *tile-group-root-map* (kbd "C-9"))
(undefine-key *tile-group-root-map* (kbd "Up"))
(undefine-key *tile-group-root-map* (kbd "Down"))
(undefine-key *tile-group-root-map* (kbd "Left"))
(undefine-key *tile-group-root-map* (kbd "Right"))
(undefine-key *tile-group-root-map* (kbd "M-Up"))
(undefine-key *tile-group-root-map* (kbd "M-Down"))
(undefine-key *tile-group-root-map* (kbd "M-Left"))
(undefine-key *tile-group-root-map* (kbd "M-Right"))
(undefine-key *tile-group-root-map* (kbd "TAB"))
(undefine-key *tile-group-root-map* (kbd "M-TAB"))

;;; Groups
;; (define-key *root-map* (kbd "g") '*groups-map*)

;; ;; Some top-map keybindings for most used groups
;; (define-key *top-map* (kbd "s-RET") "gother")

;; (define-key *top-map* (kbd "s-e")   (concat "gselect " *group-emacs*))
;; (define-key *top-map* (kbd "s-E")   (concat "gmove-and-follow " *group-emacs*))
;; (define-key *top-map* (kbd "C-s-e") (concat "gmove " *group-emacs*))

;; (define-key *top-map* (kbd "s-b")   (concat "gselect " *group-web*))
;; (define-key *top-map* (kbd "s-B")   (concat "gmove-and-follow " *group-web*))
;; (define-key *top-map* (kbd "C-s-b") (concat "gmove " *group-web*))

;; (define-key *top-map* (kbd "s-o")   (concat "gselect " *group-other*))
;; (define-key *top-map* (kbd "s-O")   (concat "gmove-and-follow " *group-other*))
;; (define-key *top-map* (kbd "C-s-o") (concat "gmove " *group-other*))

;; (define-key *top-map* (kbd "s-c")   (concat "gselect " *group-social*))
;; (define-key *top-map* (kbd "s-C")   (concat "gmove-and-follow " *group-social*))
;; (define-key *top-map* (kbd "C-s-c") (concat "gmove " *group-social*))

;; (defvar *al/groups-map*
;;   (let ((m (make-sparse-keymap)))
;;     (define-key m (kbd "g") "grouplist")
;;     (define-key m (kbd "c") "gnew")
;;     (define-key m (kbd "n") "gnext")
;;     (define-key m (kbd "N") "gnext-with-window")
;;     (define-key m (kbd "p") "gprev")
;;     (define-key m (kbd "P") "gprev-with-window")
;;     (define-key m (kbd "o") "gother")
;;     (define-key m (kbd "s") "gselect")
;;     (define-key m (kbd "m") "gmove")
;;     (define-key m (kbd "M") "gmove-marked")
;;     (define-key m (kbd "k") "gkill")
;;     (define-key m (kbd "r") "grename")
;;     m)
;;   "Simplified version of the default `*groups-map*'")

;; (define-key *root-map* (kbd "g") '*al/groups-map*)

;;; Frames
(define-key *top-map* (kbd "C-s-h") "move-focus left")
(define-key *top-map* (kbd "C-s-j") "move-focus down")
(define-key *top-map* (kbd "C-s-k") "move-focus up")
(define-key *top-map* (kbd "C-s-l") "move-focus right")

(define-key *root-map* (kbd "o") "fother")
(define-key *root-map* (kbd "f") "fselect")
;; Replace frame numbers by home row letters
;; TODO: is it possible to change the visualisation of these characters, they are kinda small?
(setf *frame-number-map* "asdfghjkl")

;; (define-key *root-map* (kbd "2") "vsplit-and-focus")
;; (define-key *root-map* (kbd "s") "hsplit-and-focus")
;; (define-key *root-map* (kbd "0") "remove-split")

;;; Windows
(define-key *top-map* (kbd "C-s-H")   "move-window left")
(define-key *top-map* (kbd "C-s-J")   "move-window down")
(define-key *top-map* (kbd "C-s-K")   "move-window up")
(define-key *top-map* (kbd "C-s-L")   "move-window right")

(define-key *root-map* (kbd "RET") "expose")
(define-key *root-map* (kbd "b")   "pull-from-windowlist") ; Similar to Emacs' switch-to-buffer

(defvar *al/windows-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "w") "windowlist")
    (define-key m (kbd "W") "frame-windowlist")
    (define-key m (kbd "d") "delete-window")
    (define-key m (kbd "D") "delete-window-and-frame")
    (define-key m (kbd "k") "kill-window")
    (define-key m (kbd "n") "next")
    (define-key m (kbd "o") "other-window")
    (define-key m (kbd "p") "prev")
    m))

(define-key *root-map* (kbd "w") '*al/windows-map*)

;;; Applications
(defvar *al/applications-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "e") (concat "exec " +al/app-editor+))   ; Launch a new Emacs instance
    (define-key m (kbd "t") (concat "exec " +al/app-terminal+)) ; Launch a new terminal
    (define-key m (kbd "b") (concat "exec " +al/app-web-browser+))
    (define-key m (kbd "B") (concat "exec " +al/app-web-browser-alt+))
    ;; (define-key m (kbd "p") (concat "exec " +al/app-music-player+))
    ;; (define-key m (kbd "P") (concat "exec " +al/app-music-player-pi+))
    (define-key m (kbd "m") (concat "exec " +al/app-messenger+))
    (define-key m (kbd "M") (concat "exec " +al/app-email+))
    (define-key m (kbd "v") (concat "exec " +al/app-document-viewer+))
    (define-key m (kbd "o") (concat "exec " +al/app-office+))
    (define-key m (kbd "z") (concat "exec " +al/app-library+))
    (define-key m (kbd "x") (concat "exec " +al/app-launcher+))
    (define-key m (kbd "y") (concat "exec " +al/app-youtube+))
    (define-key m (kbd "f") (concat "exec " +al/app-file-manager+))
    (when *is-work-machine-p*
      (define-key m (kbd "c") (concat "exec " +al/app-chat+))
      (define-key m (kbd "C") (concat "exec " +al/app-conference-call+))
      (define-key m (kbd "a") (concat "exec " +al/app-api-tester+)))
    m))

(define-key *root-map* (kbd "a") '*al/applications-map*)

;; Additional keybindings for often used applications
(define-key *root-map* (kbd "e") +al/app-editor+) ; Launch or switch to Emacs
(define-key *root-map* (kbd "t") "al/terminal")   ; Launch or switch to terminal
(define-key *top-map* (kbd "s-x") (concat "exec " +al/app-launcher+))

;; Lock screen
(define-key *top-map* (kbd "s-S") "exec mpc pause; slock") ; Note: mpd module does not have a
                                                           ; pause command

;; Remap keys for browsers
(define-remapped-keys
    `((,(lambda (win)
          (let ((class (window-class win)))
          (or (string-equal "Librewolf" class)
              (string-equal "Firefox" class))))
        ("C-n"   . "Down")
        ("C-p"   . "Up")
        ("C-f"   . "Right")
        ("C-b"   . "Left")
        ("C-v"   . "Next")
        ("C-s"   . "C-f")
        ("M-v"   . "Prior")
        ("M-w"   . "C-c")
        ("C-w"   . "C-x")
        ("C-y"   . "C-v")
        ("M-<"   . "Home")
        ("M->"   . "End")
        ("C-M-b" . "M-Left")
        ("C-M-f" . "M-Right")
        ("C-k"   . "C-w"))))

(define-key *root-map* (kbd "C-q") "send-raw-key")

;;; Screen
(define-key *top-map* (kbd "XF86MonBrightnessDown") "al/backlight-decrease")
(define-key *top-map* (kbd "XF86MonBrightnessUp")   "al/backlight-increase")

(defvar *al/display-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "a") "exec autorandr --change")
    (define-key m (kbd "l") "exec autorandr --load laptop")
    (define-key m (kbd "h") "exec autorandr horizontal")
    (define-key m (kbd "v") "exec autorandr vertical")
    (define-key m (kbd "r") "exec pkill -USR1 redshift")
    (define-key m (kbd "m") "al/toggle-mode-line")
    m))

(define-key *top-map* (kbd "XF86Display") '*al/display-map*)
; TODO: Temporary fix until I can get XF86Display to work
(define-key *top-map* (kbd "F4")          '*al/display-map*)


;;; Media
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume-up")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume-down")
(define-key *top-map* (kbd "XF86AudioMute") "volume-toggle-mute")


;;; Media
;; (define-key *top-map* (kbd "XF86AudioPlay") "mpd-toggle-pause")
;; (define-key *top-map* (kbd "XF86AudioNext") "mpd-next")
;; (define-key *top-map* (kbd "XF86AudioPrev") "mpd-prev")

;; (define-key *top-map* (kbd "XF86AudioRaiseVolume") "al/increase-volume")
;; (define-key *top-map* (kbd "XF86AudioLowerVolume") "al/decrease-volume")
;; (define-key *top-map* (kbd "XF86AudioMute")        "al/toggle-mute")
;; (define-key *top-map* (kbd "XF86AudioMicMute")     "al/toggle-source-mute")

;; (defvar *al/media-map*
;;   (let ((m (make-sparse-keymap)))
;;     (define-key m (kbd "m") "al/toggle-source-mute")
;;     (define-key m (kbd "i") "al/increase-source-volume")
;;     (define-key m (kbd "d") "al/decrease-source-volume")
;;     (define-key m (kbd "s") "al/toggle-mute")
;;     (define-key m (kbd "p") "exec pavucontrol")
;;     m))

;; (define-key *root-map* (kbd "m") '*al/media-map*)

;;; Print screen
(define-key *top-map* (kbd "Print")
  "exec scrot '%Y%m%d-%H%M%S.png' -q 100 -e 'mv $f ~/pictures/screenshots/'") ; Whole screen
(define-key *top-map* (kbd "C-Print")
  "exec scrot '%Y%m%d-%H%M%S.png' -q 100 -e 'mv $f ~/pictures/screenshots/' -s") ; Select window

;;; End session
(defvar *al/end-session-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "q")   "end-session")
    (define-key m (kbd "s")   "suspend-computer")
    (define-key m (kbd "l")   "logout")
    (define-key m (kbd "S")   "shutdown-computer")
    (define-key m (kbd "r")   "loadrc")
    (define-key m (kbd "R")   "restart-hard")
    (define-key m (kbd "C-r") "restart-computer")
    m))

(define-key *top-map* (kbd "XF86PowerOff") '*al/end-session-keymap*)
(when *is-work-machine-p*
  (define-key *root-map* (kbd "End") '*al/end-session-keymap*))

;;; Desktop management
(defvar *al/desktop-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "s") "al/save-desktop")
    (define-key m (kbd "l") "al/load-desktop")
    m)
  "Keys to dump and restore desktop to/from a file")

(define-key *root-map* (kbd "d") '*al/desktop-keymap*)

;;; Winner-mode
(defvar *al/winner-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "Left") "winner-undo")
    (define-key m (kbd "Right") "winner-redo")
    m)
  "Keys to cycle through frame configurations")

(define-key *root-map* (kbd "c") '*al/winner-map*)

;;; Utilities
(define-key *root-map* (kbd "C-s") "swm-ssh-menu")

;; Local Variables:
;; jinx-local-words: "Undefine asdfghjkl autorandr backlight fother fselect gkill gmove gnew gnext gother gprev grename grouplist gselect hsplit loadrc mpc mpd mv pavucontrol pkill png redshift scrot slock swm vsplit windowlist"
;; End:
