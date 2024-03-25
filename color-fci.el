;;; color-fci.el --- Paint fill-column indicator -*- lexical-binding: t; -*-

;; Copyright © 2023-2024 Pradyumna Paranjape.

;; Author: Pradyumna Paranjape <pradyparanjpe@rediffmail.com>
;; URL: https://www.gitlab.com/pradyparanjpe/color-fci
;; Version: 0.0.2
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


(require 'cl-lib)
(require 'color)
(require 'display-fill-column-indicator)
(require 'face-remap)


(defgroup color-fci nil
  "Paint `fill-column' by changing face background."
  :group 'convenience
  :group 'display
  :prefix "color-fci")

(defvar color-fci--timer nil
  "Timers for `color-fci-mode'.")

(put 'color-fci--timer 'risky-local-variable t)


(defvar-local color-fci--cookie nil
  "Buffer cookie to remap relative face `fill-column-indicator'.")

(put 'color-fci--cookie 'risky-local-variable t)


(defface color-fci-overflow
  '((default (:background "#ff00ff")))
  "Face of indicator when line overflows `fill-column'.

Interpret fill-fraction > 1.0 as *overfilled* and return this value."
  :group 'color-fci)


(defcustom color-fci-bright-frac (/ 1.0 3.0)
  "Brightness fraction of `fill-column'."
  :type 'number
  :group 'color-fci)


(defcustom color-fci-invert nil
  "When non-nil, Invert colors: red for 0, cyan for 1.

Otherwise [default], \\='0\\=' is empty (good = cyan),
\\='1\\=' is filled (bad = red)."
  :type 'boolean
  :group 'color-fci)


(defcustom color-fci-tracks-point nil
  "When non-nil, calculate color from `current-column'.

When nil, calculate from instead of line fill fraction."
  :type 'boolean
  :group 'color-fci)


(defcustom color-fci-call-freq-sec 2
  "Idle seconds after which, fill-column-indicator is recolored."
  :type 'number
  :group 'color-fci)

(defun color-fci--tracker ()
  "Return tracker position.

See `color-fci-tracks-point' to decide what to track."
  (/ (if color-fci-tracks-point (current-column)
       (save-excursion (end-of-line) (current-column)))
     (float fill-column)))


(defun color-fci--calc-spec (frac)
  "Calculate #RRGGBB values list 0 < val < 1 for FRAC fraction."
  (let ((frac (if color-fci-invert (- 1.0 frac) frac)))
    `(:background ,(color-rgb-to-hex (min 1.0 (* 2.0 frac))  ; red
                                     (min 1.0 (* 2.0 (- 1.0 frac)))  ; green
                                     (max 0 (- 1.0 (* 10 frac))) 2))))  ; blue


(defun color-fci--scale-face-spec (spec &optional bright)
  "Scale attributes of SPEC by BRIGHT.

All possible color values of SPEC scaled by BRIGHT fraction.
BRIGHT may be in the interval [0, 1].  If nil, return SPEC unmodified."
  (if (not bright) spec
    (apply
     #'append
     (cl-loop for (prop val) on spec by #'cddr
              collect
              (pcase val
                ((and (pred stringp) (pred color-values))  ; color string
                 (list prop (apply #'color-rgb-to-hex
                                   `(,@(mapcar (lambda (x) (* bright x))
                                               (color-name-to-rgb val))
                                     2))))
                ((pred listp)
                 (list prop (color-fci--scale-face-spec val bright)))
                (_ `(,prop ,val)))))))


(defun color-fci--fill-cap-spec (frac &optional bright)
  "Color based on filled capacity fraction FRAC.

FRAC is fraction of color in the interval [0, 1].  Fraction of
brightness is provided through BRIGHT, [default: 1.0]."
  (color-fci--scale-face-spec
   (if (> frac 1.0) (face-all-attributes 'color-fci-overflow (selected-frame))
     (color-fci--calc-spec frac))
   (when bright (max 0 (min bright 1)))))

;;;###autoload
(defun color-fci ()
  "Color `fill-column' according to position of cursor."
  (interactive)
  (when display-fill-column-indicator-mode
    ;; Drop previous cookie
    (when color-fci--cookie (face-remap-remove-relative color-fci--cookie))
    ;; Create new
    (setq-local color-fci--cookie
                (apply #'face-remap-add-relative
                       `(fill-column-indicator
                         ,@(color-fci--fill-cap-spec
                            (color-fci--tracker) color-fci-bright-frac))))))

;;;###autoload
(define-minor-mode color-fci-mode
  "Toggle `color-fci-mode'.

When `color-fci-mode' is ON, color of `display-fill-column-indicator-character'
changes according to fraction of `fill-column' occupied by current line."
  :lighter nil
  (if color-fci-mode
      (unless color-fci--timer
        ;; Cron
        (setq color-fci--timer
              (run-with-idle-timer color-fci-call-freq-sec t #'color-fci)))
    (when color-fci--timer
      ;; Drop cron
      (cancel-timer color-fci--timer)
      (setq color-fci--timer nil)
      ;; reset original color
      (when color-fci--cookie
        (face-remap-remove-relative color-fci--cookie)
        (setq-local color-fci--cookie nil)))))

(provide 'color-fci)
;;; color-fci.el ends here
