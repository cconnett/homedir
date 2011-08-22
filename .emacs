(add-to-list 'load-path "~/.emacs.d/site-lisp/")
;(load-file "/home/build/public/eng/elisp/google.el")

;; XWindows preferences
(unless (window-system)
      (menu-bar-mode -1))
(when (window-system)
  (set-scroll-bar-mode 'right))  ; show the scroll bar on the right side

;; Requires
(require 'font-lock)
(require 'guess-offset)
(require 'sourcepawn-mode)
(require 'ido)
(require 'highlight-symbol)
(require 'column-marker)

(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-ignore-files '("\\.hi$"))

(column-number-mode t)
(global-font-lock-mode t)
(global-auto-revert-mode t)
(transient-mark-mode nil)
(show-paren-mode t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq shift-select-mode nil)
(setq org-support-shift-select nil)
(setq-default show-trailing-whitespace t)
(setq-default truncate-lines t)
(setq-default fill-column 80)
(set-face-background 'column-marker-1 "magenta")
(add-hook 'find-file-hook (lambda () (column-marker-1 80)))
(add-hook 'first-change-hook (lambda () (column-marker-1 80)))

(global-set-key (kbd "C-c SPC") 'just-one-space)
(global-set-key (kbd "C-c TAB") 'tab-to-tab-stop)
(global-set-key (kbd "C-c i") 'insert-date-string)
(global-set-key (kbd "C-c s") 'sort-lines)
(global-set-key (kbd "C-x f") 'ido-find-file)
(global-set-key (kbd "C-x s") 'save-buffer)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [(meta f3)] 'highlight-symbol-prev)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [C-insert] 'overwrite-mode)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [mouse-4] 'scroll-down)
(global-set-key [mouse-5] 'scroll-up)
(global-set-key [vertical-scroll-bar down-mouse-1] 'scroll-bar-drag)
(global-set-key [vertical-scroll-bar drag-mouse-1] 'scroll-bar-drag)
(global-unset-key (kbd "<insert>"))
(global-unset-key [C-z])
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; Windmove
(when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings)
      )
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; Python font faces
(make-face 'py-comment-face)
(make-face 'py-keyword-face)
(make-face 'py-pseudo-keyword-face)
(make-face 'py-type-face)
(set-face-background 'show-paren-match-face "cornflower blue")
(set-face-foreground 'font-lock-string-face "forest green")
(set-face-foreground 'py-comment-face "firebrick")
(set-face-foreground 'py-keyword-face "medium blue")
(set-face-foreground 'py-type-face "steel blue")
(add-hook 'python-mode-hook
          (lambda ()
            (set (make-local-variable 'font-lock-type-face) 'py-type-face)
            (set (make-local-variable 'font-lock-comment-face) 'py-comment-face)
            (set (make-local-variable 'font-lock-keyword-face) 'py-keyword-face)
            (set-face-foreground 'py-pseudo-keyword-face "dodger blue")
            ))

;;set intentation preferences
;;(setq tab-width 8)
;;(setq c-basic-offset 2)
;;(setq indent-tabs-mode nil)

(add-hook 'text-mode-hook
          (lambda ()
            (setq truncate-lines nil)))

(add-hook 'sh-mode-hook
          (lambda ()
            (set-face-foreground 'sh-heredoc-face "dark magenta")))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(remove-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

(add-hook 'latex-mode-hook 'll-mode)
(add-hook 'bibtex-mode-hook '(lambda () (menu-bar-mode 1)))

(setq frame-title-format '("%b - " "emacs@" system-name))

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
                ("\\.gcl$"  . c++-mode)
                ("\\.model$"  . c++-mode)
                ("\\.py$" . python-mode)
                ("\\.PY$" . python-mode)
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
                )
              auto-mode-alist))

(setq ilisp-*use-fsf-compliant-keybindings* t)
(setq inferior-lisp-program "/usr/bin/sbcl --noinform")
(setq lisp-indent-fuction 'common-lisp-indent-function)

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
