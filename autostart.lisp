;;;; autostart.lisp --- Automatically start applications

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
;; Automatically start several applications when initialising StumpWM.
;;
;;; Code:

(defun al/run-in-thread (cmd)
  "Execute the provided command CMD in a separate thread."
  (sb-thread:make-thread (lambda () (run-shell-command cmd))))

(when *initializing*

  (al/run-in-thread "exec ~/.nutstore/dist/bin/nutstore-pydaemon.py")
  (al/run-in-thread "mihomo -d ~/.config/clash")
  (al/run-in-thread +al/app-web-browser+)
  (al/run-in-thread +al/app-editor+)
  (al/run-in-thread +al/app-terminal+)
  ;; (al/run-in-thread +al/app-messenger+)
  )

;; (when (and *is-work-machine-p*
;;            *initializing*)
;;   (al/run-in-thread +al/app-chat+)
;;   (al/run-in-thread +al/app-conference-call+))

;; Local Variables:
;; jinx-local-words: "autostart"
;; End:
