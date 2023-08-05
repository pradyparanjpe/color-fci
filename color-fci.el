;;; color-fci.el --- Paint fill-column indicator -*- lexical-binding: t; -*-
;;; Commentary:
;; Copyright (C) 2019-2023  Free Software Foundation, Inc.

;; Author: Pradyumna Paranjape <pradyparanjpe@rediffmail.com>
;; URL: https://www.gitlab.com/pradyparanjpe/color-fci
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces

;;; License:

;; This file is part of color-fci.
;;
;; color-fci is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; color-fci is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with color-fci.  If not, see <https://www.gnu.org/licenses/>.
;;
;; color fill-column-indicator according to fraction of fill-column
;; occupied by current line
;;
;;; Code:

(defgroup color-fci nil
  "paint fill-column by changing face background")

(defvar color-fci-timer nil
  "Handle for `color-fci-mode'")

(defvar color-fci-orig-bg-color
  (face-attribute 'fill-column-indicator :background)
  "Original value of background for face `fill-column-indicator'")

(defcustom color-fci-bright-frac 0.33
  "Brightness fraction of fill-column"
  :type 'number
  :group 'color-fci)

(defcustom color-fci-tracks-point nil
  "When non-nil, color is calculated based on column of point
instead of line fill fraction"
  :type 'boolean
  :group 'color-fci)

(defcustom color-fci-overflow-color
  "#ff00ff"
  "Color of indicator when line overflows fill-column"
  :group 'color-fci)

(defcustom color-fci-call-freq-sec 2
  "Idle seconds after which, fill-column-indicator is recolored"
  :type 'number
  :group 'color-fci)

(defun color-fci--tracker ()
  "Returns tracker position.

If `color-fci-tracks-point' is non-nil, use column of point
Otherwise, use column of end of current line"
  (if color-fci-tracks-point
      (* 1.0 (current-column))
    (let ((eol-point (save-excursion (end-of-line) (current-column))))
      (* 1.0 eol-point))))

(defun fill-cap-color (frac &optional bright invert)
  "Color based on filled capacity fraction FRAC.

=0= is empty (bad = red), =1= is filled (good = blue-green)
Fraction of brightness is provided through BRIGHT, else assumed as 1.0
If INVERT is non-nil, goodness/badness is reversed.
FRAC > 1.0 is interpreted as *overfilled* and returns
BRIGHT-scaled `color-fci-overflow-color')"
  (if (> frac 1.0)
      (let* ((bright (max 0 (min (or bright 1) 1)))
             (col-vals
              (mapcar (lambda (x)
                        (* bright x))
                      (color-name-to-rgb
                       color-fci-overflow-color))))
        (apply #'color-rgb-to-hex `(,@col-vals 2)))
    (let* ((frac (if invert (- 1.0 frac) frac))
           (bright (or bright 1))
           (red (* bright (* 2 (- 0.5 (max 0 (- frac 0.5))))))
           (green (* bright (* 2 (- 0.5 (max 0 (- 0.5 frac))))))
           (blue (* bright (* 10 (max 0 (- frac 0.9))))))
      (color-rgb-to-hex red green blue 2))))

(defun color-fci ()
  "Color fill-column according to position of cursor"
  (interactive)
  (when display-fill-column-indicator-mode
    (set-face-background
     'fill-column-indicator
     (fill-cap-color (/ (color-fci--tracker) fill-column)
                     color-fci-bright-frac t))))

(define-minor-mode color-fci-mode
  "Toggle color-fci-mode

When color-fci-mode is ON, color of `display-fill-column-indicator-character'
changes according to fraction of `fill-column' occupied by current line"
  :lighter nil
  (if color-fci-mode
      (unless color-fci-timer
        (setq color-fci-orig-bg-color
              (face-attribute 'fill-column-indicator :background))
        (setq color-fci-timer (run-with-idle-timer
                               color-fci-call-freq-sec t #'color-fci)))
    (when color-fci-timer
      (cancel-timer color-fci-timer)
      (setq color-fci-timer nil)
      (set-face-background 'fill-column-indicator color-fci-orig-bg-color))))

(provide 'color-fci)
;;; color-fci.el ends here
