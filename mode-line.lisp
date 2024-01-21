;;;; mode-line.lisp --- Configuration for the mode line

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
;; This file contains the main configuration for the mode line and loads
;; additional, mode line-related configuration files.
;;
;;; Code:

;;; Load additional modules
;; (al/load "mode-line-windows")
(al/load "mode-line-battery")
;; (al/load "mode-line-mail")
;; (al/load "mode-line-updates")
;; (al/load "mode-line-disk")
;; (al/load "mode-line-network")

;;; General mode line settings
;; (setf *mode-line-position*           :top
;;       *mode-line-timeout*            5
;;       *mode-line-background-color*   +al/colour-mode-line-bg+
;;       *mode-line-foreground-color*   +al/colour-mode-line-fg+
;;       *mode-line-border-color*       +al/colour-mode-line-bg+
;;       *mode-line-border-width*       0
;;       *mode-line-pad-x*              5
;;       *mode-line-highlight-template* (concat "^["
;;                                              "^(:fg \"" +al/colour-group-fg-active+ "\")"
;;                                              "^(:bg \"" +al/colour-group-bg-active+  "\")"
;;                                              "~a^]"))

;;; Element-specific configuration
;; (setf *group-format* "%t")

;; (setf *window-format*
;;       (concat "%m%s "
;;               (al/util:colour-string " %c "
;;                                      :fg +al/colour-fg-inactive+
;;                                      :bg +al/colour-bg-inactive+)
;;               " %70t"))

;; (setf *time-modeline-string*
;;       (concat
;;        (al/util:colour-icon +al/icon-time+) " %H:%M "
;;        (al/util:colour-icon +al/icon-date+) " %a %e %b"))

;; Contrib module configuration
;; (setf cpu::*cpu-modeline-fmt*       (concat (al/util:colour-icon +al/icon-cpu+) "%c")
;;       cpu::*cpu-usage-modeline-fmt* " ^[~a~2d%^]")

;; (setf mem::*mem-modeline-fmt* (concat (al/util:colour-icon +al/icon-memory+) "%p"))

;; (setf mpd:*mpd-modeline-fmt*
;;       (concat (al/util:colour-string "%L") ; status
;;               " %a "                       ; artist
;;               (al/util:colour-string "-")  ; separator
;;               " %t"))                      ; track

;; (defun al/mode-line-separator (&optional (icon +al/icon-separator+))
;;   "Print a coloured separator ICON."
;;   (format nil " ~a " (al/util:colour-icon icon)))

;; ;;; Add all elements to the mode line
;; (setf *screen-mode-line-format*
;;       (list "%d"                                                          ; date and time
;;             (al/mode-line-separator)
;;             ;; '(:eval (al/mail:mail-mode-line))                             ; email
;;             '(:eval (al/package-updates:updates-mode-line))               ; package updates
;;             (al/mode-line-separator +al/icon-separator-double+)
;;             "%g"                                                          ; groups
;;             (al/mode-line-separator)
;;             '(:eval (al/generate-mode-line-urgent-windows))               ; urgent windows
;;             '(:eval (al/generate-mode-line-windows))                      ; windows
;;             "^>"                                                          ; right align
;;             ;; "%m "                                                         ; mpd
;;             ;; '(:eval (al/pamixer:pamixer-mode-line))                       ; audio
;;             "%C "                                                         ; cpu
;;             "%M"                                                          ; memory
;;             '(:eval (al/disk:disk-mode-line))                             ; disk
;;             '(:eval (al/network:network-mode-line))                       ; network
;;             '(:eval (al/battery:battery-mode-line))
;;             ))                     ; battery

(setf *screen-mode-line-format*
      (list "[%n]"                      ; Groups
            "%v"                        ; Windows
            "^>"                        ; Push right
            " | %B"
            " | %M"
            " | %d"                     ;; Clock
            ;; '(:eval (al/battery:battery-mode-line))
            ))                   



;;; Actually start the mode line
(when *initializing*
  (mode-line))

;; Local Variables:
;; jinx-local-words: "Contrib cpu mpd"
;; End:
