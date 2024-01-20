;;;; commands.lisp --- Custom commands

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
;; This file defines a set of additional commands.
;;
;;; Code:

(defcommand al/terminal () ()
  "Start a terminal unless one is already running, in which it is pulled into focus it."
  (run-or-pull +al/app-terminal+ '(:class "kitty"))
  )

;;; Frames
(defcommand delete-window-and-frame () ()
  "Delete the current frame with its window"
  (delete-window)
  (remove-split))

(defcommand hsplit-and-focus () ()
  "Create a new frame on the right and focus it"
  (hsplit)
  (move-focus :right))

(defcommand vsplit-and-focus () ()
  "Create a new frame below and move focus to it"
  (vsplit)
  (move-focus :down))

;;; Audio
;; (defcommand al/increase-volume () ()
;;   "Increase the volume and show a notification containing the new volume"
;;   (al/pamixer:increase-volume)
;;   (al/util:dunst-notification "volume_up"
;;                               "Volume"
;;                               (format nil "~d%" (al/pamixer:get-volume))
;;                               :id "865"
;;                               :timeout "2000"))

;; (defcommand al/decrease-volume () ()
;;   "Decrease the volume and show a notification containing the new volume."
;;   (al/pamixer:decrease-volume)
;;   (al/util:dunst-notification "volume_down"
;;                               "Volume"
;;                               (format nil "~d%" (al/pamixer:get-volume))
;;                               :id "865"
;;                               :timeout "2000"))

;; (defcommand al/toggle-mute () ()
;;   "Toggle whether the volume is muted"
;;   (al/pamixer:toggle-mute)
;;   (if (al/pamixer:is-muted-p)
;;       (al/util:dunst-notification "volume_mute"
;;                                   "Volume"
;;                                   "Muted"
;;                                   :id "865"
;;                                   :timeout "2000")
;;       (al/util:dunst-notification "volume_on"
;;                                   "Volume"
;;                                   (format nil "\"Unmuted at ~d%\"" (al/pamixer:get-volume))
;;                                   :id "865"
;;                                   :timeout "2000")))

;; (defcommand al/toggle-source-mute () ()
;;   (al/pamixer:source-toggle-mute)
;;   (if (al/pamixer:source-is-muted-p)
;;       (al/util:dunst-notification "mic_mute"
;;                                   "Microphone"
;;                                   "Muted"
;;                                   :id "642"
;;                                   :timeout "2000")
;;       (al/util:dunst-notification "mic_on"
;;                                   "Microphone"
;;                                   (format nil "\"Unmuted at ~d%\"" (al/pamixer:source-get-volume))
;;                                   :id "642"
;;                                   :timeout "2000")))

;; (defcommand al/increase-source-volume () ()
;;   "Increase the volume and show a notification containing the new volume"
;;   (al/pamixer:source-increase-volume)
;;   (al/util:dunst-notification "mic_on"
;;                               "Microphone"
;;                               (format nil "~d%" (al/pamixer:source-get-volume))
;;                               :id "642"
;;                               :timeout "2000"))

;; (defcommand al/decrease-source-volume () ()
;;   "Increase the volume and show a notification containing the new volume"
;;   (al/pamixer:source-decrease-volume)
;;   (al/util:dunst-notification "mic_on"
;;                               "Microphone"
;;                               (format nil "~d%" (al/pamixer:source-get-volume))
;;                               :id "642"
;;                               :timeout "2000"))

;;; Backlight
(defcommand al/backlight-increase () ()
  "Increase the brightness of the backlight and show notification with new value"
  (al/backlight:increase-backlight)
  (al/util:dunst-notification "backlight_up"
                              "Backlight"
                              (format nil "~3d%" (al/backlight:get-backlight-rel))
                              :id "274"
                              :timeout "2000"))

(defcommand al/backlight-decrease () ()
  "Decrease the brightness of the backlight and show notification with new value"
  (al/backlight:decrease-backlight)
  (al/util:dunst-notification "backlight_down"
                              "Backlight"
                              (format nil "~3d%" (al/backlight:get-backlight-rel))
                              :id "274"
                              :timeout "2000"))

;;; Mode line
(defcommand al/toggle-mode-line () ()
  "Toggle the mode line on the current screen and head"
  (toggle-mode-line (current-screen) (current-head)))
