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

(defvar color-fci-timer nil
  "Timers for `color-fci-mode'.")


(defvar color-fci--remap-cookie nil
  "Buffer cookie to remap relative face `fill-column-indicator'.")

(make-variable-buffer-local 'color-fci--remap-cookie)


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


(defface color-fci-overflow
  '((default (:background "#ff00ff")))
  "Face of indicator when line overflows `fill-column'.

Interpret fill-fraction > 1.0 as *overfilled* and return this value."
  :group 'color-fci)


(defcustom color-fci-call-freq-sec 2
  "Idle seconds after which, fill-column-indicator is recolored."
  :type 'number
  :group 'color-fci)

(defun color-fci--tracker ()
  "Return tracker position.

See `color-fci-tracks-point' to decide what to track."
  (/ (if color-fci-tracks-point (* 1.0 (current-column))
       (let ((eol-point (save-excursion (end-of-line) (current-column))))
         (* 1.0 eol-point)))
     fill-column))


(defun color-fci--calc-rgb (frac)
  "Calculate \\'(Red Green Blue) values list 0 < val < 1 for FRAC fraction."
  (let* ((frac (if color-fci-invert (- 1.0 frac) frac))
         (red (min 1.0 (* 2.0 frac)))
         (green (min 1.0 (* 2.0 (- 1.0 frac))))
         (blue (max 0 (- 1.0 (* 10 frac)))))
    `(:background (,red ,green ,blue))))


(defun color-fci--scale-face-spec (spec &optional bright)
  "Scale attributes of SPEC by BRIGHT.

SPEC is face spec to be scaled.  Attributes which are scaled: :foreground,
:distant-foreground, :background, :underline :overline :strike-through :box.
BRIGHT may be in the interval [0, 1].  If nil, return SPEC unmodified."
  (or
   (when bright
     (apply
      #'append
      (cl-loop for (prop val) on spec by #'cddr
               collect
               (if (not (member prop '(:foreground
                                       :distant-foreground
                                       :background
                                       :underline
                                       :overline
                                       :strike-through
                                       :box)))
                   `(,prop ,val)
                 (pcase val
                   ((pred stringp)
                    (list prop
                          (apply #'color-rgb-to-hex
                                 `(,@(mapcar (lambda (x) (* bright x))
                                             (color-name-to-rgb val))
                                   2))))
                   ((pred listp)
                    (list prop
                          (apply
                           #'color-rgb-to-hex
                           `(,@(mapcar (lambda (x) (* bright x)) val)
                             2))))
                   (_ `(,prop ,val)))))))
   spec))


(defun color-fci--fill-cap-spec (frac &optional bright)
  "Color based on filled capacity fraction FRAC.

FRAC is fraction of color in the interval [0, 1].  Fraction of
brightness is provided through BRIGHT, [default: 1.0]."
  (color-fci--scale-face-spec
   (if (> frac 1.0)
       (face-all-attributes 'color-fci-overflow (selected-frame))
     (color-fci--calc-rgb frac))
   (when bright (max 0 (min bright 1)))))

;;;###autoload
(defun color-fci ()
  "Color `fill-column' according to position of cursor."
  (interactive)
  (when display-fill-column-indicator-mode
    ;; Drop previous cookie
    (if color-fci--remap-cookie
        (face-remap-remove-relative color-fci--remap-cookie))
    ;; Create new
    (setq-local color-fci--remap-cookie
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
