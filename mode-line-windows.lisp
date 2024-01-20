;;;; mode-line-windows.lisp --- Group windows by class in the mode line
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
;; Customise how windows are displayed in the mode line by grouping them per
;; class and adding a counter with the number of windows per class.
;;
;; This is adapted from alezost's StumpWM configuration (see URL `https://github.com/alezost/stumpwm-config')
;;
;;; Code:

;;; Window list
(defvar al/window-alist nil
  "Alist of (CLASS . NUM) pairs of the available windows.
CLASS is a window class; NUM is the number of windows of this class.")

(defvar al/current-window nil
  "Class of the current window.")

(defvar al/urgent-window-list nil
  "A list of window classes for urgent windows")

(defun al/update-window-alist (&rest _)
  "Refresh the value of `al/window-alist'."
  (declare (ignore _))
  (setf al/window-alist nil)
  (mapc (lambda (w)
          (let* ((wc (window-class w))
                 (entry (assoc wc al/window-alist :test #'string=)))
            (if entry
                (setf (cdr entry) (1+ (or (cdr entry) 1)))
                (push (list wc) al/window-alist))))
        (group-windows (current-group))))

(defun al/update-current-window (&rest _)
  "Refresh the value of `al/current-window'."
  (declare (ignore _))
  (when (current-window)
    (setf al/current-window (window-class (current-window)))))

(defun al/update-urgent-window-list (&rest _)
  "Refresh the value of `al/urgent-window-list'"
  (declare (ignore _))
  (setf al/urgent-window-list nil)
  (mapc (lambda (w)
          (push (string-capitalize (window-class w)) al/urgent-window-list))
        (screen-urgent-windows (current-screen))))

;;; Hooks
(add-hook *focus-window-hook*   'al/update-current-window)
(add-hook *focus-group-hook*    'al/update-current-window)
(add-hook *new-window-hook*     'al/update-window-alist)
(add-hook *destroy-window-hook* 'al/update-window-alist)
(add-hook *focus-group-hook*    'al/update-window-alist)

(add-hook *urgent-window-hook*  'al/update-urgent-window-list)
(add-hook *focus-window-hook*   'al/update-urgent-window-list)

;;; Mode line
(defun al/mode-line-window-class (&key (str "%c") active urgent)
  "Window class colour construct for mode line and window list"
  (let ((fg (cond (active +al/colour-window-fg-active+)
                  (urgent +al/colour-window-fg-urgent+)
                  (t      +al/colour-window-fg-inactive+)))
        (bg (cond (active +al/colour-window-bg-active+)
                  (urgent +al/colour-window-bg-urgent+)
                  (t      +al/colour-window-bg-inactive+))))
    (al/util:colour-string (format nil " ~a " str) :fg fg :bg bg)))

(defun al/mode-line-class-counter (num &key active)
  "Window class counter colour construct for mode line"
  (let ((fg (cond (active +al/colour-class-counter-fg-active+)
                  (t      +al/colour-class-counter-fg-inactive+)))
        (bg (cond (active +al/colour-class-counter-bg-active+)
                  (t      +al/colour-class-counter-bg-inactive+))))
  (al/util:colour-string (format nil " ~d " num) :fg fg :bg bg)))

(defun al/generate-mode-line-windows ()
  (when (and al/window-alist al/current-window)
    (format
     nil
     "~{~a~^ ~}"
     (mapcar (lambda (assoc)
               (destructuring-bind (class . num)
                   assoc
                 (let ((active (string-equal class al/current-window)))
                   (concat
                    (al/mode-line-window-class :str (string-capitalize class) :active active)
                    (and num (al/mode-line-class-counter num :active active))))))
             al/window-alist))))

(defun al/generate-mode-line-urgent-windows ()
  (when al/urgent-window-list
    (format nil "~{~a ~}"
            (map 'list
                 (lambda (class) (al/mode-line-window-class :str (string-capitalize class)
                                                            :urgent t))
                 al/urgent-window-list))))

;; Local Variables:
;; jinx-local-words: "Alist alezost's alist"
;; End:
