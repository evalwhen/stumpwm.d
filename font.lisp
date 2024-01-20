;;;; font.lisp --- Font configuration

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
;; Load the desired fonts and assign aliases.
;;
;;; Code:
;; (set-font (list (make-instance 'xft:font
;;                                :family "Hack"
;;                                :subfamily "Regular"
;;                                :size 11)
;;                 (make-instance 'xft:font
;;                                :family "Hack"
;;                                :subfamily "Bold"
;;                                :size 11)
;;                 (make-instance 'xft:font
;;                                ;; :family "Font Awesome 6 Free Solid"
;;                                :family "Hack"
;;                                :subfamily "Solid"
;;                                :size 12)))

(set-font (make-instance 'xft:font :family "Fira Code Retina" :subfamily "Regular" :size 16))

;;; Convenience parameters
(defparameter +al/font-main+ "^f0")
(defparameter +al/font-bold+ "^f1")
(defparameter +al/font-icon+ "^f2")
