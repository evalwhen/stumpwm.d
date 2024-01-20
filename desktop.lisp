;;;; desktop.lisp --- Utility commands to deal with desktop layout

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
;; This file provides a convenience wrapper around StumpWM's command to dump and
;; restore the current desktop configuration of the current group.
;;
;;; Code:

(defparameter *al/desktop-dump-file* "~/.cache/stumpwm-desktop"
  "Where my desktop dump should go and can be loaded from.")

(defcommand al/save-desktop () ()
  "Save desktop layout to *al/desktop-dump-file*."
  (dump-desktop-to-file *al/desktop-dump-file*))

(defcommand al/load-desktop () ()
  "Restore desktop layout from *al/desktop-dump-file*."
  (restore-from-file *al/desktop-dump-file*))
