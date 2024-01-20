;;;; applications.lisp --- Define the default applications

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
;; Define variables for often used applications. This file centralises their
;; management and configuration to a single location.
;;
;;; Code:

(defvar +al/app-chat+            "rocketchat-desktop")
(defvar +al/app-conference-call+ "jitsi-meet-desktop")
(defvar +al/app-document-viewer+ "evince")
(defvar +al/app-editor+          "emacs")
(defvar +al/app-email+           "emacs -f \"mu4e\"")
(defvar +al/app-file-manager+    "pcmanfm")
(defvar +al/app-launcher+        "rofi -show run")
(defvar +al/app-library+         "zotero")
(defvar +al/app-messenger+       "signal-desktop")
(defvar +al/app-office+          "libreoffice")
(defvar +al/app-api-tester+      "postman")
;; (defvar +al/app-terminal+        "kitty")
(defvar +al/app-terminal+        "xfce4-terminal")
(defvar +al/app-web-browser+     "GDK_SCALE=2 GDK_DPI_SCALE=0 WEBKIT_DISABLE_COMPOSITING_MODE=1 start-nyxt")
(defvar +al/app-web-browser-alt+ "firefox")
(defvar +al/app-youtube+         "freetube")

;; (defvar +al/app-music-player+    (concat +al/app-terminal+ " ncmpcpp"))
;; (defvar +al/app-music-player-pi+
;;   (concat +al/app-terminal+
;;           " ncmpcpp -h "
;;           (with-open-file (in (concat *config-path* "auth-ncmpcpp-pi")) (read-line in))))

;; Utilities
(setq swm-ssh:*swm-ssh-default-term* +al/app-terminal+)

;; Local Variables:
;; jinx-local-words: "auth emacs firefox freetube libreoffice librewolf ncmpcpp pcmanfm rocketchat rofi zotero"
;; End:
