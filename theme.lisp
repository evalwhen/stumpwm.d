;;;; theme.lisp --- Theme configuration

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
;; This file configures the visual elements for StumpWM.
;;
;;; Code:

;;; Border colours
(set-focus-color         +al/colour-border-focus+)
(set-unfocus-color       +al/colour-border-unfocus+)

(set-float-focus-color   +al/colour-border-float-focus+)
(set-float-unfocus-color +al/colour-border-float-unfocus+)

(set-win-bg-color        +al/colour-bg+)

(setf *normal-border-width*    2
      *maxsize-border-width*   2
      *transient-border-width* 2
      *window-border-style*    :thin)

;;; Message and input bar
(set-fg-color         +al/colour-fg+)
(set-bg-color         +al/colour-bg+)
(set-border-color     +al/colour-bg-active+)
(set-msg-border-width 2)

(setf *input-window-gravity*         :center
      *message-window-input-gravity* :center
      *message-window-gravity*       :center
      *message-window-padding*       10
      *message-window-y-padding*     10)

;;; Gravity of windows in frames
(set-normal-gravity    :center)
(set-maxsize-gravity   :center)
(set-transient-gravity :center)
(gravity               :center)

;;; which-key
(setf *key-seq-color* "^8") ; must use *colors* index due to hardcoded '*' in `print-key-seq'
(setf *which-key-format*
      (concat +al/font-bold+  ; Use bold font for keys
              *key-seq-color* ; Use colour set above
              "*~6a"
              "^n"            ; Reset colours to normal
              +al/font-main+  ; Set font back to regular
              "~30a"))

;;; Bar colours
(setf *bar-med-color* "^B^8*"
      *bar-hi-color* "^9*"
      *bar-crit-color* "^B^9*")

;;; Etc.
;; (setf *ignore-wm-inc-hints* t) ; Ensure Emacs uses full height of a frame
