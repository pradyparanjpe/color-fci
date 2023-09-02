﻿;;; color-fci.el --- Paint fill-column indicator -*- lexical-binding: t; -*-

;; Copyright © 2023  Pradyumna Paranjape.

;; Author: Pradyumna Paranjape <pradyparanjpe@rediffmail.com>
;; URL: https://www.gitlab.com/pradyparanjpe/color-fci
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.
;; This file is a part of color-fci

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A quick dirty hack to change color of fill-column-indicator (fci)
;; to indicate the fraction of `fill-column' occupied by current line.
;; Minor mode: Schedules `run-with-idle-timer' to run recolor fci.
;; Customize using customization group `color-fci'.
;; Interactive callable `color-fci' paints fci.

;;; Code:

(defgroup color-fci nil
  "Paint fill-column by changing face background"
  :group 'convenience
  :group 'display
  :prefix "color-fci")

(defvar color-fci-timer nil
  "Handle for `color-fci-mode'")

(defvar color-fci--remap-cookie
  nil
  "Cookie to remap relative for face `fill-column-indicator' BUFFER-LOCAL")

(make-variable-buffer-local 'color-fci--remap-cookie)

(defcustom color-fci-bright-frac (/ 1.0 3.0)
  "Brightness fraction of `fill-column'"
  :type 'number
  :group 'color-fci)

(defcustom color-fci-invert nil
  "When non-nil, Invert colors: red for 0, cyan for 1"
  :type 'boolean
  :group 'color-fci)

(defcustom color-fci-tracks-point nil
  "When non-nil, color is calculated based on `current-column'
instead of line fill fraction"
  :type 'boolean
  :group 'color-fci)

(defcustom color-fci-overflow-color "#ff00ff"
  "Color of indicator when line overflows `fill-column'"
  :type 'color
  :group 'color-fci)

(defcustom color-fci-call-freq-sec 2
  "Idle seconds after which, fill-column-indicator is recolored"
  :type 'number
  :group 'color-fci)

(defun color-fci--tracker ()
  "Returns tracker position.

If `color-fci-tracks-point' is non-nil, use column of point
Otherwise, use column of end of current line"
  (/ (if color-fci-tracks-point (* 1.0 (current-column))
       (let ((eol-point (save-excursion (end-of-line) (current-column))))
         (* 1.0 eol-point)))
     fill-column))

(defun color-fci--calc-color (frac)
  "Calculate \\'(Red Green Blue) values list 0 < val < 1 for FRAC fraction."
  (let* ((frac (if color-fci-invert (- 1.0 frac) frac))
         (red (min 1.0 (* 2.0 frac)))
         (green (min 1.0 (* 2.0 (- 1.0 frac))))
         (blue (max 0 (- 1.0 (* 10 frac)))))
    `(,red ,green ,blue)))

(defun color-fci--fill-cap-color (frac &optional bright)
  "Color based on filled capacity fraction FRAC.

\=0\= is empty (good = cyan), \=1\= is filled (bad = red)
Fraction of brightness is provided through BRIGHT, else assumed as 1.0
FRAC > 1.0 is interpreted as *overfilled* and returns
BRIGHT-scaled `color-fci-overflow-color')"
  (let* ((bright (max 0 (min (or bright 1) 1)))
         (rgb (if (> frac 1.0)
                  (color-name-to-rgb color-fci-overflow-color)
                (color-fci--calc-color frac)))
         (col-vals (mapcar (lambda (x) (* bright x)) rgb)))
    (apply #'color-rgb-to-hex `(,@col-vals 2))))

(defun color-fci--which-attr ()
  "Which attribute to paint?

graphical: background
non-graphical: foreground"
  (if (display-graphic-p) :background :foreground))

;;;###autoload
(defun color-fci ()
  "Color fill-column according to position of cursor"
  (interactive)
  (when display-fill-column-indicator-mode
    ;; Drop previous cookie
    (if color-fci--remap-cookie
        (face-remap-remove-relative color-fci--remap-cookie))
    ;; Create new
    (setq-local color-fci--remap-cookie
                (face-remap-add-relative
                 'fill-column-indicator
                 `(,(color-fci--which-attr)
                   ,(color-fci--fill-cap-color
                     (color-fci--tracker) color-fci-bright-frac))))))

;;;###autoload
(define-minor-mode color-fci-mode
  "Toggle color-fci-mode

When color-fci-mode is ON, color of `display-fill-column-indicator-character'
changes according to fraction of `fill-column' occupied by current line"
  :lighter nil
  (if color-fci-mode
      (unless color-fci-timer
        ;; Cron
        (setq color-fci-timer (run-with-idle-timer
                               color-fci-call-freq-sec t #'color-fci)))
    (when color-fci-timer
      ;; Drop cron
      (cancel-timer color-fci-timer)
      (setq color-fci-timer nil)
      ;; reset original color
      (when color-fci--remap-cookie
        (face-remap-remove-relative color-fci--remap-cookie)
        (setq-local color-fci--remap-cookie nil)))))

(provide 'color-fci)
;;; color-fci.el ends here
