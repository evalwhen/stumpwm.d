;;;; mode-line-network.lisp --- Wrapper to enrich contrib's wifi module

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
;; A wrapper around the wifi module from the contrib modules to enrich the
;; printed mode line printed with some additional information.
;;
;; Dependencies:
;; * ip from iproute2
;; * ripgrep (See URL `https://github.com/BurntSushi/ripgrep')
;;
;;; Code:

(defpackage :al/network
  (:use #:cl #:stumpwm)
  (:export #:network-mode-line)
  (:import-from :al/util :trim-newline)
  (:import-from :al/util :colour-icon))

(in-package :al/network)

;;; Configure wifi module
(setf wifi::*wifi-modeline-fmt*          (concat (colour-icon stumpwm::+al/icon-wifi+) "%p")
      wifi::*wifi-signal-quality-fmt-pc* "^[~a~3d%^]")

;;; Ethernet connection
(defun ethernet-connected-p ()
  "Return T if an ethernet device is currently up, NIL otherwise."
  (loop
    for path in (list-directory "/sys/class/net/")
      thereis
      (let ((device-name (car (last (pathname-directory path)))))
        (if (equal "en" (subseq device-name 0 2)) ; systemd starts ethernet device names with "en"
            (with-open-file (file (merge-pathnames #P"operstate" path) :if-does-not-exist nil)
              (when file
                (equal "up" (read-line file))))))))

;;; VPN/Wireguard connection
(defvar vpnp nil
  "Whether a vpn connection was active on last check.")

(defvar vpn-timer nil)

(defvar vpn-update-time 300
  "The time between checks of whether a vpn connection is active (in seconds).")

(defun update-vpnp ()
  "Return T if a vpn connection is currently active, NIL otherwise"
  (setf vpnp (string/= "" (trim-newline (run-shell-command "ip link | rg 'azirevpn|tun0'" t)))))

;;; Mode line
(defun network-mode-line ()
  "Print the appropriate elements for the mode line. This contains at least the
wifi information, if a vpn connection is currently active the appropriate
indicator is also shown."
  (unless vpn-timer
    (setf vpn-timer (run-with-timer 0 vpn-update-time #'update-vpnp)))
  (concat
   (when vpnp (format nil "~a " (colour-icon stumpwm::+al/icon-vpn+)))
   "%I "))

;; Local Variables:
;; jinx-local-words: "contrib contrib's ip ripgrep systemd wifi"
;; End:
