;;; highlight-symbol.el --- automatic and manual symbol highlighting
;;
;; Copyright (C) 2007-2009 Nikolaj Schumacher
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 1.1
;; Keywords: faces, matching
;; URL: http://nschum.de/src/emacs/highlight-symbol/
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Add the following to your .emacs file:
;; (require 'highlight-symbol)
;; (global-set-key [(control f3)] 'highlight-symbol-at-point)
;; (global-set-key [f3] 'highlight-symbol-next)
;; (global-set-key [(shift f3)] 'highlight-symbol-prev)
;; (global-set-key [(meta f3)] 'highlight-symbol-prev)))
;; (global-set-key [(control meta f3)] 'highlight-symbol-query-replace)
;;
;; Use `highlight-symbol-at-point' to toggle highlighting of the symbol at
;; point throughout the current buffer.  Use `highlight-symbol-mode' to keep the
;; symbol at point highlighted.
;;
;; The functions `highlight-symbol-next', `highlight-symbol-prev',
;; `highlight-symbol-next-in-defun' and `highlight-symbol-prev-in-defun' allow
;; for cycling through the locations of any symbol at point.
;; When `highlight-symbol-on-navigation-p' is set, highlighting is triggered
;; regardless of `highlight-symbol-idle-delay'.
;;
;; `highlight-symbol-query-replace' can be used to replace the symbol.
;;
;;; Change Log:
;;
;; 2009-04-13 (1.1)
;;    Added `highlight-symbol-query-replace'.
;;
;; 2009-03-19 (1.0.5)
;;    Fixed `highlight-symbol-idle-delay' void variable message.
;;    Fixed color repetition bug.  (thanks to Hugo Schmitt)
;;
;; 2008-05-02 (1.0.4)
;;    Added `highlight-symbol-on-navigation-p' option.
;;
;; 2008-02-26 (1.0.3)
;;    Added `highlight-symbol-remove-all'.
;;
;; 2007-09-06 (1.0.2)
;;    Fixed highlighting with delay set to 0.  (thanks to Stefan Persson)
;;
;; 2007-09-05 (1.0.1)
;;    Fixed completely broken temporary highlighting.
;;
;; 2007-07-30 (1.0)
;;    Keep temp highlight while jumping.
;;    Replaced `highlight-symbol-faces' with `highlight-symbol-colors'.
;;    Fixed dependency and Emacs 21 bug.  (thanks to Gregor Gorjanc)
;;    Prevent calling `highlight-symbol-at-point' on nil.
;;
;; 2007-04-20 (0.9.1)
;;    Fixed bug in `highlight-symbol-jump'.  (thanks to Per Nordlöw)
;;
;; 2007-04-06 (0.9)
;;    Initial release.
;;
;;; Code:

(require 'thingatpt)
(require 'hi-lock)
(require 'hexrgb)
(require 'cl)

(push "^No symbol at point$" debug-ignored-errors)

(defgroup highlight-symbol nil
  "Automatic and manual symbols highlighting"
  :group 'faces
  :group 'matching)

(defface highlight-symbol-face
  '((((class color) (background dark))
     (:background "gray30"))
    (((class color) (background light))
     (:background "gray90")))
  "*Face used by `highlight-symbol-mode'."
  :group 'highlight-symbol)

(defvar highlight-symbol-timer nil)

(defun highlight-symbol-update-timer (value)
  (when highlight-symbol-timer
    (cancel-timer highlight-symbol-timer))
  (setq highlight-symbol-timer
        (and value (/= value 0)
             (run-with-idle-timer value t 'highlight-symbol-temp-highlight))))

(defvar highlight-symbol-mode nil)

(defun highlight-symbol-set (symbol value)
  (when symbol (set symbol value))
  (when highlight-symbol-mode
    (highlight-symbol-update-timer value)))

(defcustom highlight-symbol-idle-delay 1.5
  "*Number of seconds of idle time before highlighting the current symbol.
If this variable is set to 0, no idle time is required.
Changing this does not take effect until `highlight-symbol-mode' has been
disabled for all buffers."
  :type 'number
  :set 'highlight-symbol-set
  :group 'highlight-symbol)

(defcustom highlight-symbol-on-navigation-p nil
  "*Wether or not to temporary highlight the symbol when using
`highlight-symbol-jump' family of functions."
  :type 'boolean
  :group 'highlight-symbol)

(defvar highlight-symbol nil)
(make-variable-buffer-local 'highlight-symbol)

(defvar highlight-symbol-list nil)
;(make-variable-buffer-local 'highlight-symbol-list)

(defvar highlight-symbol-last-symbol nil)
(defvar highlight-symbol-last-bounds nil)
(defvar highlight-symbol-last-point nil)

(defconst highlight-symbol-border-pattern
  (if (>= emacs-major-version 22) '("\\_<" . "\\_>") '("\\<" . "\\>")))

;;;###autoload
(define-minor-mode highlight-symbol-mode
  "Minor mode that highlights the symbol under point throughout the buffer.
Highlighting takes place after `highlight-symbol-idle-delay'."
  nil " hl-s" nil
  (if highlight-symbol-mode
      ;; on
      (let ((hi-lock-archaic-interface-message-used t))
        (unless hi-lock-mode (hi-lock-mode 1))
        (highlight-symbol-update-timer highlight-symbol-idle-delay)
        (add-hook 'post-command-hook 'highlight-symbol-mode-post-command nil t))
    ;; off
    (remove-hook 'post-command-hook 'highlight-symbol-mode-post-command t)
    (highlight-symbol-mode-remove-temp)
    (kill-local-variable 'highlight-symbol)))

(defconst highlight-symbol-saturation-alist
  (mapcar (lambda (pair) (list (/ (car pair) 360.0) (cadr pair)))
          '(
            (12 0.3) (13 0.8) (20 0.8) (25 0.75) (30 0.8) (35 0.9)
            (60 0.8) (120 0.75) (125 0.75) (130 0.9) (140 1.0) (150 0.6)
            (160 1.0) (170 0.8) (180 1.0) (210 0.4) (220 0.5) (230 0.45)
            (240 0.35) (250 0.4) (260 0.55) (270 0.6) (290 0.7) (300 0.6)
            (320 0.6) (330 0.5) (340 0.4)
            ))
  ;((0.0 0.6) (0.15 0.75) (0.3333 0.8) (0.4 0.8) (0.6667 0.5) (0.7 0.3))
  )

;;;###autoload
(defun highlight-symbol-at-point ()
  "Toggle highlighting of the symbol at point.
This highlights or unhighlights the symbol at point using the first
element in of `highlight-symbol-faces'."
  (interactive)
  (let ((symbol (highlight-symbol-get-symbol)))
    (unless symbol (error "No symbol at point"))
    (unless hi-lock-mode (hi-lock-mode 1))
    (if (member symbol highlight-symbol-list)
        ;; remove
        (progn
          (setq highlight-symbol-list (delete symbol highlight-symbol-list))
          (mapc (lambda (buffer)
                  (set-buffer buffer)
                  (hi-lock-unface-buffer symbol)
                  )
                (buffer-list))
          )
      ;; add
      (when (equal symbol highlight-symbol)
        (highlight-symbol-mode-remove-temp))
      (let* ((complete-saturation-alist
              (let ((begin (car highlight-symbol-saturation-alist))
                    (end (car (last highlight-symbol-saturation-alist))))
                (append `((,(- (car end) 1) ,(cadr end)))
                        highlight-symbol-saturation-alist
                        `((,(+ (car begin) 1) ,(cadr begin))))))
             (hue (/ (mod (sxhash symbol) 360) 360.0))
             (bottom (find-if (lambda (item) (< hue (car item)))
                              complete-saturation-alist :from-end))
             (top    (find-if (lambda (item) (> hue (car item)))
                              complete-saturation-alist))
             (saturation (/ (+ (* (- hue (car bottom)) (cadr top))
                               (* (- (car top) hue) (cadr bottom)))
                            (- (car top) (car bottom))))
             (color (hexrgb-hsv-to-hex
                     hue saturation 1.0))
             )
        (print bottom)
        (setq color `((background-color . ,color)
                      (foreground-color . "black")))
        ;; highlight
        (with-no-warnings
          (mapc (lambda (buffer)
                  (set-buffer buffer)
                  (if (< emacs-major-version 22)
                      (hi-lock-set-pattern `(,symbol (0 (quote ,color) t)))
                    (hi-lock-set-pattern symbol color))
                  )
                (buffer-list)))
        (push symbol highlight-symbol-list)))))

;;;###autoload
(defun highlight-symbol-remove-all ()
  "Remove symbol highlighting in buffer."
  (interactive)
  (mapc (lambda (buffer)
          (set-buffer buffer)
          (mapc 'hi-lock-unface-buffer highlight-symbol-list)
          (setq highlight-symbol-list nil)
          )
        (buffer-list)
        )
  )

;;;###autoload
(defun highlight-symbol-next ()
  "Jump to the next location of the symbol at point within the function."
  (interactive)
  (highlight-symbol-jump 1))

;;;###autoload
(defun highlight-symbol-prev ()
  "Jump to the previous location of the symbol at point within the function."
  (interactive)
  (highlight-symbol-jump -1))

;;;###autoload
(defun highlight-symbol-next-in-defun ()
  "Jump to the next location of the symbol at point within the defun."
  (interactive)
  (save-restriction
    (narrow-to-defun)
    (highlight-symbol-jump 1)))

;;;###autoload
(defun highlight-symbol-prev-in-defun ()
  "Jump to the previous location of the symbol at point within the defun."
  (interactive)
  (save-restriction
    (narrow-to-defun)
    (highlight-symbol-jump -1)))

;;;###autoload
(defun highlight-symbol-query-replace (replacement)
  "*Replace the symbol at point."
  (interactive (let ((symbol (or (thing-at-point 'symbol)
                                 (error "No symbol at point"))))
                 (highlight-symbol-temp-highlight)
                 (set query-replace-to-history-variable
                      (cons (substring-no-properties symbol)
                            (eval query-replace-to-history-variable)))
                 (list
                  (read-from-minibuffer "Replacement: " nil nil nil
                                        query-replace-to-history-variable))))
  (goto-char (beginning-of-thing 'symbol))
  (query-replace-regexp (highlight-symbol-get-symbol) replacement))

(defun highlight-symbol-get-symbol ()
  "Return a regular expressio dandifying the symbol at point."
  (let ((symbol (thing-at-point 'symbol)))
    (if symbol
        (progn
          (setq highlight-symbol-last-symbol symbol)
          (setq highlight-symbol-last-bounds (bounds-of-thing-at-point 'symbol))
          (setq highlight-symbol-last-point (point))
          )
      (setq symbol highlight-symbol-last-symbol))
    (when symbol (concat (car highlight-symbol-border-pattern)
                         (regexp-quote symbol)
                         (cdr highlight-symbol-border-pattern)))))

(defun highlight-symbol-temp-highlight ()
  "Highlight the current symbol until a command is executed."
  (when highlight-symbol-mode
    (let ((symbol (highlight-symbol-get-symbol)))
      (unless (or (equal symbol highlight-symbol)
                  (member symbol highlight-symbol-list))
        (highlight-symbol-mode-remove-temp)
        (when symbol
          (setq highlight-symbol symbol)
          (hi-lock-set-pattern symbol 'highlight-symbol-face))))))

(defun highlight-symbol-mode-remove-temp ()
  "Remove the temporary symbol highlighting."
  (when highlight-symbol
    (hi-lock-unface-buffer highlight-symbol)
    (setq highlight-symbol nil)))

(defun highlight-symbol-mode-post-command ()
  "After a command, change the temporary highlighting.
Remove the temporary symbol highlighting and, unless a timeout is specified,
create the new one."
  (if (eq this-command 'highlight-symbol-jump)
      (when highlight-symbol-on-navigation-p
        (highlight-symbol-temp-highlight))
    (if (eql highlight-symbol-idle-delay 0)
        (highlight-symbol-temp-highlight)
      (highlight-symbol-mode-remove-temp))))

(defun highlight-symbol-jump (dir)
  "Jump to the next or previous occurence of the symbol at point.
DIR has to be 1 or -1."
  (let ((symbol (highlight-symbol-get-symbol)))
    (if symbol
        (let* ((case-fold-search nil)
               (b (bounds-of-thing-at-point 'symbol))
               (bounds (if b b highlight-symbol-last-bounds))
               (point (if b (point) highlight-symbol-last-point))
               (offset (- point (if (< 0 dir) (cdr bounds) (car bounds)))))
          (unless (eq last-command 'highlight-symbol-jump)
            (push-mark))
          ;; move a little, so we don't find the same instance again
          (goto-char (- (point) offset))
          (let ((target (re-search-forward symbol nil t dir)))
            (unless target
              (goto-char (if (< 0 dir) (point-min) (point-max)))
              (setq target (re-search-forward symbol nil nil dir)))
            (goto-char (+ target offset)))
          (setq this-command 'highlight-symbol-jump))
      (error "No symbol at point"))))

(provide 'highlight-symbol)

;;; highlight-symbol.el ends here
