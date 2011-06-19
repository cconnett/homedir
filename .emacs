;(require 'python-mode)
;(require 'rst-mode)

;(load "/usr/share/emacs/site-lisp/site-gentoo")

;(load-library "lilypond-mode.el")
;(require 'lilypond-mode)
;(setq auto-mode-alist
;      (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))
;(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))

;(require 'pyrex-mode)
;(autoload 'rst-mode "rst-mode" "mode for editing reStructuredText documents" t)
;; Context hilighting, configure font-lock-mode

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(require 'font-lock)

(when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings)
      ;(global-set-key [S-up] 'windmove-up)
      ;(global-set-key [S-down]  'windmove-down)
      ;(global-set-key [S-left] 'windmove-left)
      ;(global-set-key [S-right]  'windmove-right)
      )

(column-number-mode t)
;(if (>= emacs-major-version 23)
;    (add-to-list 'default-frame-alist '(font . "-unknown-DejaVu Sans Mono-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1"))
;    )
(unless (window-system)
      (menu-bar-mode -1))
;(tool-bar-mode -1)
(when (window-system)
  (set-scroll-bar-mode 'right))  ; show the scroll bar on the right side

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-ignore-files
      '("\\.hi$")
      )

(require 'guess-offset)
(require 'sourcepawn-mode)

(global-font-lock-mode t)
(global-auto-revert-mode t)
(transient-mark-mode nil)
(show-paren-mode t)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key [C-insert] 'overwrite-mode)
(global-unset-key (kbd "<insert>"))
(global-unset-key [C-z])

(setq shift-select-mode nil)
(setq org-support-shift-select nil)
;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;;(add-hook 'before-save-hook 'delete-trailing-whitespace) ;; This moves the point if it's indented to enter new code and drives me crazy;
;;(add-hook 'before-save-hook (lambda () (untabify (point-min) (point-max))))
;;(menu-bar-enable-clipboard)

;;disable splash screen startup message and scratch buffer message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; n.b.: scroll-up makes the *text* scroll up, so conventionally it's
;; scrolling down, and vice versa for scroll-down
(global-set-key [mouse-4] 'scroll-down)
(global-set-key [mouse-5] 'scroll-up)
(global-set-key [vertical-scroll-bar down-mouse-1] 'scroll-bar-drag)
(global-set-key [vertical-scroll-bar drag-mouse-1] 'scroll-bar-drag)
(global-set-key (kbd "C-c SPC") 'just-one-space)

(set-face-background 'show-paren-match-face "cornflower blue")
(set-face-foreground 'font-lock-string-face "forest green")

(make-face 'py-type-face)
(make-face 'py-comment-face)
(make-face 'py-keyword-face)
(set-face-foreground 'py-type-face "steel blue")
(set-face-foreground 'py-comment-face "firebrick")
(set-face-foreground 'py-keyword-face "medium blue")

(add-hook 'python-mode-hook
          (lambda ()
            (set (make-local-variable 'font-lock-type-face) 'py-type-face)
            (set (make-local-variable 'font-lock-comment-face) 'py-comment-face)
            (set (make-local-variable 'font-lock-keyword-face) 'py-keyword-face)
            (set-face-foreground 'py-pseudo-keyword-face "dodger blue")
            ))

;;set intentation preferences
(setq tab-width 8)
(setq c-basic-offset 2)
(setq indent-tabs-mode nil)
;;enable highlighting of trailing whitespace
(setq-default show-trailing-whitespace t)

(add-hook 'text-mode-hook
          (lambda ()
            (setq truncate-lines nil)))

(defun compiz-go ()
  (interactive)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 4)
  (setq c-default-style "k&r"))

(add-hook 'java-mode-hook
          (lambda ()
            (setq tab-width 8)
            (setq indent-tabs-mode t)))

(add-hook 'prolog-mode-hook
          (lambda ()
            (setq tab-width 2)))

(add-hook 'sh-mode-hook
          (lambda ()
            (set-face-foreground 'sh-heredoc-face "dark magenta")))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(remove-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'bibtex-mode-hook '(lambda () (menu-bar-mode 1)))

(setq-default truncate-lines t)
(setq frame-title-format '("%b - " "emacs@" system-name))

;(setq-default font-lock-support-mode fast-lock-mode)
(setq-default font-lock-use-fonts t)
(setq-default font-lock-use-colors t)
(setq-default font-lock-use-maximal-decoration t)
(setq-default scroll-preserve-screen-position t)
(setq-default indent-tabs-mode nil)
(setq-default default-major-mode 'text-mode)
;(setq default-major-mode 'org-mode)
(setq interpreter-mode-alist
      (cons '("python" . python-mode)
            interpreter-mode-alist))

(setq auto-mode-alist
      (append '(("\\.C$"  . c++-mode)
                ("\\.cc$" . c++-mode)
                ("\\.hh$" . c++-mode)
                ("\\.c$"  . c-mode)
                ("\\.h$"  . c++-mode)
                ("\\.py$" . python-mode)
                ("\\.PY$" . python-mode)
                ;("\\.pyx$". pyrex-mode)
                ("\\.pyx$". python-mode)
                ("\\.m$"  . matlab-mode)
                ("\\.tex$". latex-mode)
                ("\\.pl$" . perl-mode)
                ("\\.pro$" . prolog-mode)
                ("\\.[hg]s$" . haskell-mode)
                ("\\.hi$" . haskell-mode)
                ("\\.l[hg]s$" . literate-haskell-mode)
                ("\\.ebuild$" . shell-script-mode)
                ("\\.sp$" . sourcepawn-mode)
                ("\\.txt$" . text-mode)
                ("\\.org$" . org-mode)
                ("todo$" . org-mode)
                ("TODO$" . org-mode)
                ("Tupfile$" . makefile-mode)
                ("generated_Tupdeps$" . makefile-mode)
                ;("\\.ly$" . lilypond-mode)
                )
              auto-mode-alist))

;; lisp: indent IF normally

;(put 'if 'lisp-indent-function nil)
;(put 'if 'fi:lisp-indent-hook nil)

;; Add F1 to do a hyperspec lookup.

;; (add-hook 'fi:lisp-listener-mode-hook
;;   (function
;;    (lambda ()
;;      (local-set-key [f1]     'hyperspec-lookup))))

;; (add-hook 'fi:common-lisp-mode-hook
;;   (function (lambda ()
;;            (local-set-key [f1] 'hyperspec-lookup))))

(setq ilisp-*use-fsf-compliant-keybindings* t)
(setq inferior-lisp-program "/usr/bin/sbcl --noinform")
(setq lisp-indent-fuction 'common-lisp-indent-function)

(defun increase-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (ceiling (* 1.25
                                  (face-attribute 'default :height)))))

(defun decrease-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (floor (* 0.75
                                (face-attribute 'default :height)))))

(defun reset-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      90))

;(load "auctex.el" nil t t)
;(load "preview-latex.el" nil t t)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(flyspell-issue-welcome-flag nil)
 '(haskell-font-lock-symbols t)
 '(haskell-program-name "ghci -fglasgow-exts")
 '(safe-local-variable-values (quote ((Encoding . utf-8)))))

;; Insertion of Dates.
(defun insert-date-string ()
  "Insert a nicely formated date string."
  (interactive)
  (insert (format-time-string "[%a %b %d %Y / %H:%M %Z]")))

;; C-c i calls insert-date-string
(global-set-key (kbd "C-c i") 'insert-date-string)
;; C-c s calls sort-lines
(global-set-key (kbd "C-c s") 'sort-lines)
;; C-c TAB calls tab-to-tab-stop
(global-set-key (kbd "C-c TAB") 'tab-to-tab-stop)

;; Set it so if my finger slips off control while saving it still
;; saves instead of query-saving.
(global-set-key (kbd "C-x s") 'save-buffer)
;; Ditto for open file
(global-set-key (kbd "C-x f") 'ido-find-file)

(defun intelligent-close ()
  "quit a frame the same way no matter what kind of frame you are on"
  (interactive)
  (if (eq (car (visible-frame-list)) (selected-frame))
      ;;for parent/master frame...
      (if (> (length (visible-frame-list)) 1)
          ;;close a parent with children present
          (delete-frame (selected-frame))
        ;;close a parent with no children present
        (save-buffers-kill-emacs))
    ;;close a child frame
    (delete-frame (selected-frame))))

;; Intelligently close the selected frame or all emacs if only one
;; frame left.
(global-set-key (kbd "C-x C-c") 'intelligent-close)
(global-set-key (kbd "<f5>") 'make-frame)
(global-set-key (kbd "<f6>") 'make-frame-on-display)
(global-set-key (kbd "C-c C-d") 'delete-trailing-whitespace)

(defun insert-scc-at-point ()
  "Insert an SCC annotation at point."
  (interactive)
  (if (or (looking-at "\b\|[ t]\|$") (and (not (bolp))
					  (save-excursion
					    (forward-char -1)
					    (looking-at "\b\|[ t]"))))
      (let ((space-at-point (looking-at "[ t]")))
	(unless (and (not (bolp)) (save-excursion
				    (forward-char -1)
				    (looking-at "[ t]")))
	  (insert " "))
	(insert "{-# SCC \"\" #-}")
	(unless space-at-point
	  (insert " "))
	(forward-char (if space-at-point -5 -6)))
    (error "Not over an area of whitespace")))

(defun kill-scc-at-point ()
  "Kill the SCC annotation at point."
  (interactive)
  (save-excursion
    (let ((old-point (point))
	  (scc "\({-#[ t]*SCC \"[^\"]*\"[ t]*#-}\)[ t]*"))
      (while (not (or (looking-at scc) (bolp)))
	(forward-char -1))
      (if (and (looking-at scc)
	       (<= (match-beginning 1) old-point)
	       (> (match-end 1) old-point))
	  (kill-region (match-beginning 0) (match-end 0))
	(error "No SCC at point")))))
(global-set-key (kbd "C-c y") 'insert-scc-at-point)
(global-set-key (kbd "C-c k") 'kill-scc-at-point)

;; (require 'flymake)
;; (defun flymake-Haskell-init ()
;;   (flymake-simple-make-init-impl
;;    'flymake-create-temp-with-folder-structure nil nil
;;    (file-name-nondirectory buffer-file-name)
;;    'flymake-get-Haskell-cmdline))
;; (defun flymake-get-Haskell-cmdline (source base-dir)
;;   (list "flycheck_haskell.pl"
;;         (list source base-dir)))

;; (push '(".+\\.hs$" flymake-Haskell-init flymake-simple-java-cleanup)
;;       flymake-allowed-file-name-masks)
;; (push '(".+\\.lhs$" flymake-Haskell-init flymake-simple-java-cleanup)
;;       flymake-allowed-file-name-masks)
;; (push
;;  '("^\\(\.+\.hs\\|\.lhs\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)"
;;    1 2 3 4) flymake-err-line-patterns)
;; ;; optional setting
;; ;; if you want to use flymake always, then add the following hook.
;;  (add-hook
;;   'haskell-mode-hook
;;   '(lambda ()
;;      (if (not (null buffer-file-name)) (flymake-mode))))
;; (when (fboundp 'resize-minibuffer-mode) ; for old emacs
;;   (resize-minibuffer-mode)
;;   (setq resize-minibuffer-window-exactly nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(put 'downcase-region 'disabled nil)
;; (defun set-frame-size-according-to-resolution () ; so much fail
;;   (interactive)
;;   (if window-system
;;   (progn
;;     ;; use 120 char wide window for largeish displays
;;     ;; and smaller 80 column windows for smaller displays
;;     ;; pick whatever numbers make sense for you
;;     (if (> (x-display-pixel-width) 1280)
;;         (add-to-list 'default-frame-alist (cons 'width 120))
;;       (add-to-list 'default-frame-alist (cons 'width 80)))
;;     ;; for the height, subtract a couple hundred pixels
;;     ;; from the screen height (for panels, menubars and
;;     ;; whatnot), then divide by the height of a char to
;;     ;; get the height we want
;;     (add-to-list 'default-frame-alist
;;                  (cons 'height (max 24 (/ (- (x-display-pixel-height) 600) (frame-char-height))))))))
;; (set-frame-size-according-to-resolution)
