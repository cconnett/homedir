;;; .emacs --- A .emacs file for cjc.
;;
;;; Commentary:
;;
;;; Code:

(package-initialize)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))


(defun string-suffix-p (str1 str2 &optional ignore-case)
  "Python: STR1.endswith(STR2).  IGNORE-CASE passed through."
  (let ((begin1 (- (length str1)
                   (length str2)))
        (end1 (length str1)))
    (when (< begin1 0)
      (setq begin1 0))
    (eq t (compare-strings str2 nil nil str1 begin1
                           end1 ignore-case))))

(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(add-to-list 'load-path "~/.emacs.d/elpa/")

(defvar at-google (string-suffix-p system-name "corp.google.com")
  "Whether the current machine is a Google corp workstation.")

;; Requires
(require 'pp)
(require 'font-lock)
(require 'ido)
(require 'flymake)
(require 'flymake-cursor)
(require 'flymake-easy)
(require 'highlight-symbol)
(require 'column-marker)
(require 'srefactor)
(require 'srefactor-lisp)
(require 'clang-format)

;; Home only
(unless at-google
  (autoload 'python-mode "python-mode" "Python Mode."
    t))
;; Google only
(when at-google
  (load-file "/usr/share/emacs24/site-lisp/emacs-google-config/devtools/editors/emacs/google.el")
  (require 'google)
  (require 'google-coding-style)
  (require 'google-cc-extras)
  (require 'google-pyformat)
  (google-cc-extras/bind-default-keys)
  (setq create-lockfiles nil))

(autoload 'js2-mode "js2-mode" "Major mode for editing JavaScript code."
  :interactive)

;; Machine formatting
(defun clang-format-file ()
  "Format the whole buffer with clang."
  (interactive)
  (clang-format-region (point-min)
                       (point-max)))

(defun google-mdformat ()
  "Run http://go/mdformat on the current file."
  (interactive)
  (reformat-file "/google/data/ro/teams/g3doc/mdformat --in_place"
                 "mdformat" ".md"))
(defun google-gclfmt ()
  "Run http://go/gclfmt on the current file."
  (interactive)
  (reformat-file "/usr/bin/gclfmt -w" "gcl"
                 ".gcl"))
(defun google-nclfmt ()
  "Run http://go/nclfmt on the current file."
  (interactive)
  (reformat-file "/usr/bin/nclfmt --in_place"
                 "ncl" ".ncl"))
(defun lispfmt ()
  "Run lispfmt.el on the current file."
  (interactive)
  (reformat-file (expand-file-name "~/bin/lispfmt.el")
                 "lispfmt"
                 "el"))

;; (unless (boundp 'format-mode/machine-format)
;;   (defvar format-mode/machine-format nil)
;;   (make-variable-buffer-local 'format-mode/machine-format))

(define-minor-mode format-mode
  "Machine format the buffer before saving."
  :lighter " Format"
  (unless (member 'format-mode-format-file before-save-hook)
    (add-hook 'before-save-hook #'format-mode-format-file)))

(global-set-key [f12]
                #'format-mode)

(defun format-mode-format-file ()
  "Format the current buffer with a machine formatter for the major mode."
  (interactive)
  (when (symbol-value 'format-mode)
    (message "Machine formatting for %s" major-mode)
    (cond
     ((memq major-mode
            '(c++-mode js-mode js2-mode protobuf-mode))
      (if at-google
          (google-clang-format-file)
        (clang-format-file)))
     ((memq major-mode '(json-mode))
      (json-mode-beautify))
     ((memq major-mode
            '(python-mode))
      (google-pyformat))
     ((memq major-mode
            '(markdown-mode))
      (google-mdformat))
     ((memq major-mode
            '(gcl-mode borg-mode))
      (google-gclfmt))
     ((memq major-mode
            '(ncl-mode))
      (google-nclfmt))
     ((memq major-mode
            '(emacs-lisp-mode lisp-mode))
      (lispfmt))
     (t (message "No formatter found for %s" major-mode)))
    (when (symbol-value 'flymake-mode)
      (flymake-restart-syntax-check))))

;; Auto-save files go in system temp.
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; XWindows preferences
(unless (window-system)
  (menu-bar-mode -1))
(when (window-system)
  (setq default-frame-alist '((left-fringe . 0)
                              (right-fringe . 0)))
  (set-scroll-bar-mode 'right)
  (setq confirm-kill-emacs 'y-or-n-p))

;; ido mode settings
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-ignore-files '("\\.hi$"))

;; General preferences
(global-auto-revert-mode t)
(column-number-mode t)
(global-font-lock-mode t)
(setq transient-mark-mode nil)
(show-paren-mode t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq shift-select-mode nil)
(setq-default show-trailing-whitespace t)
(setq-default truncate-lines t)
(setq-default fill-column (if at-google 80 72))
(setq-default resize-mini-windows nil)
(set-face-background 'column-marker-1 "magenta")
(add-hook 'find-file-hook
          (lambda ()
            (column-marker-1 80)))
(add-hook 'first-change-hook
          (lambda ()
            (column-marker-1 80)))
(setq mouse-wheel-scroll-amount '(1
                                  ((shift) . 4)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 4)

(global-set-key (kbd "C-<f12>")
                (lambda ()
                  (interactive)
                  (load-file user-init-file)))
(global-set-key (kbd "C-M-s-<f12>")
                (lambda ()
                  (save-buffers-kill-emacs)))
(global-set-key (kbd "C-c SPC")
                'just-one-space)
(global-set-key (kbd "C-c TAB")
                'tab-to-tab-stop)
(global-set-key (kbd "C-c i")
                'insert-date-string)
(global-set-key (kbd "C-c d")
                'insert-pydebug-string)
(global-set-key (kbd "C-c e")
                'insert-pydebug-dep-string)
(global-set-key (kbd "C-c t")
                'insert-pdb-string)
(global-set-key (kbd "C-c s")
                'sort-lines)
(global-set-key (kbd "C-c #")
                'comment-region)
(global-set-key (kbd "C-c $")
                'uncomment-region)
(global-set-key (kbd "C-c <")
                'decrease-left-margin)
(global-set-key (kbd "C-c >")
                'increase-left-margin)
(global-set-key (kbd "C-c l")
                'font-lock-fontify-buffer)
(global-set-key (kbd "C-x f")
                'ido-find-file)
(global-set-key (kbd "C-x s")
                'save-buffer)
(global-set-key (kbd "RET")
                'newline-and-indent)
;; These don't work in -nw mode.
(global-set-key (kbd "s-j")
                'next-line)
(global-set-key (kbd "s-k")
                'previous-line)
(global-set-key (kbd "s-h")
                'backward-char)
(global-set-key (kbd "s-l")
                'forward-char)
;; N.B.: Emacs cannot distinguish between these two in -nw mode
(global-set-key (kbd "C-S-o")
                'vi-open-line-above)
(global-set-key (kbd "C-o")
                'vi-open-line-below)

(global-set-key [f4]
                'highlight-symbol)
(global-set-key [(meta f3)]
                'highlight-symbol-prev)
(global-set-key [(shift f3)]
                'highlight-symbol-prev)
(global-set-key [(meta f2)]
                'highlight-symbol-prev-force)
(global-set-key [(shift f2)]
                'highlight-symbol-prev-force)
(global-set-key [C-insert]
                'overwrite-mode)
(global-set-key [f3]
                'highlight-symbol-next)
(global-set-key [f2]
                'highlight-symbol-next-force)
(global-set-key [mouse-4]
                'scroll-down)
(global-set-key [mouse-5]
                'scroll-up)
(global-set-key [vertical-scroll-bar down-mouse-1]
                'scroll-bar-drag)
(global-set-key [vertical-scroll-bar drag-mouse-1]
                'scroll-bar-drag)

(global-set-key (kbd "M-p")
                (lambda ()
                  (interactive)
                  (flymake-goto-prev-error)
                  (let ((err-info (flymake-find-err-info flymake-err-info
                                                         (line-number-at-pos))))
                    (if (car err-info)
                        (message "%s"
                                 (flymake-ler-text (caar err-info)))
                      (message "No lint errors.")))))
(global-set-key (kbd "M-n")
                (lambda ()
                  (interactive)
                  (flymake-goto-next-error)
                  (let ((err-info (flymake-find-err-info flymake-err-info
                                                         (line-number-at-pos))))
                    (if (car err-info)
                        (message "%s"
                                 (flymake-ler-text (caar err-info)))
                      (message "No lint errors.")))))

(global-unset-key (kbd "<insert>"))
                                        ;(global-unset-key [f2])
(global-unset-key [C-z])
(global-unset-key [(control z)])
(global-unset-key [(control x)
                   (control z)])

;; Windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; Python font faces
(make-face 'py-comment-face)
(make-face 'py-keyword-face)
(make-face 'py-pseudo-keyword-face)
(make-face 'py-type-face)
(set-face-background 'show-paren-match-face
                     "cornflower blue")
(set-face-foreground 'font-lock-string-face
                     "forest green")
(set-face-foreground 'py-comment-face "firebrick")
(set-face-foreground 'py-keyword-face "medium blue")
(set-face-foreground 'py-type-face "steel blue")
(add-hook 'python-mode-hook
          (lambda ()
            (unless (equal major-mode 'google3-build-mode)
              (flymake-python-load))
            (set (make-local-variable 'font-lock-type-face)
                 'py-type-face)
            (set (make-local-variable 'font-lock-comment-face)
                 'py-comment-face)
            (set (make-local-variable 'font-lock-keyword-face)
                 'py-keyword-face)
            (set-face-foreground 'py-pseudo-keyword-face
                                 "dodger blue")))

(add-hook 'sh-mode-hook
          (lambda ()
            (set-face-foreground 'sh-heredoc-face "dark magenta")))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(remove-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

(add-hook 'latex-mode-hook 'll-mode)
(add-hook 'bibtex-mode-hook
          '(lambda ()
             (menu-bar-mode 1)))

(setq frame-title-format '("%b - " "emacs@" system-name))

(set-face-attribute 'region nil :background "#ddd")
(setq-default font-lock-use-fonts t)
(setq-default font-lock-use-colors t)
(setq-default font-lock-maximum-decoration
              t)
(setq-default scroll-preserve-screen-position
              t)
(setq-default indent-tabs-mode nil)
(setq-default format-mode t)
(setq-default default-major-mode 'text-mode)
(setq-default js-indent-level 2)
;;(setq default-major-mode 'org-mode)

(setq interpreter-mode-alist (cons '("python" . python-mode) interpreter-mode-alist))

(setq auto-mode-alist (append '(("TODO$" . org-mode)
                                ("Tupfile$" . makefile-mode)
                                ("\\.C$" . c++-mode)
                                ("\\.PY$" . python-mode)
                                ("\\.[hg]s$" . haskell-mode)
                                ("\\.c$" . c-mode)
                                ("\\.cc$" . c++-mode)
                                ("\\.css$" . css-mode)
                                ("\\.gss$" . css-mode)
                                ("\\.ebuild$" . shell-script-mode)
                                ("\\.gcl$" . borg-mode)
                                ("\\.h$" . c++-mode)
                                ("\\.hh$" . c++-mode)
                                ("\\.hi$" . haskell-mode)
                                ("\\.js$" . js2-mode)
                                ("\\.json$" . json-mode)
                                ("\\.l[hg]s$" . literate-haskell-mode)
                                ("\\.m$" . matlab-mode)
                                ("\\.md$" . markdown-mode)
                                ("\\.model$" . borg-mode)
                                ("\\.ng$" . html-mode)
                                ("\\.org$" . org-mode)
                                ("\\.pl$" . perl-mode)
                                ("\\.pp$" . c++-mode)
                                ("\\.pro$" . prolog-mode)
                                ("\\.py$" . python-mode)
                                ("\\.pyx$" . python-mode)
                                ("\\.sp$" . sourcepawn-mode)
                                ("\\.tex$" . latex-mode)
                                ("\\.tpl$" . html-mode)
                                ("\\.txt$" . text-mode)
                                ("generated_Tupdeps$" . makefile-mode)
                                ("todo$" . org-mode))
                              auto-mode-alist))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(clang-format-style "google")
 '(css-indent-offset 2)
 '(desktop-save-mode t)
 '(flymake-info-line-regexp ":[RC]:")
 '(flymake-warn-line-regexp ":W:")
 '(flyspell-issue-welcome-flag nil)
 '(haskell-font-lock-symbols t)
 '(haskell-program-name "ghci -fglasgow-exts")
 '(highlight-symbol-on-navigation-p t)
 '(ido-default-file-method (quote selected-window))
 '(ilisp-*use-fsf-compliant-keybindings* t)
 '(inferior-lisp-program "/usr/bin/sbcl --noinform")
 '(js-indent-level 2 t)
 '(js2-auto-indent-flag nil)
 '(js2-basic-offset 2)
 '(js2-global-externs (quote ("chrome" "angular")))
 '(js2-mirror-mode t)
 '(js2-mode-escape-quotes nil)
 '(lisp-indent-fuction (quote common-lisp-indent-function))
 '(markdown-enable-math t)
 '(org-support-shift-select nil)
 '(py-continuation-offset 2)
 '(py-indent-offset 2 t)
 '(py-smart-indentation nil)
 '(pyformat-args "-i -y --force_quote_type single --binpack_named_arguments"
                 t)
 '(safe-local-variable-values (quote ((encoding . utf-8)
                                      (Encoding . utf-8))))
 '(sgml-basic-offset 2)
 '(sh-basic-offset 2)
 '(sh-indentation 2)
 '(standard-indent 2)
 '(vc-follow-symlinks t))

(defun flymake-python-load ()
  (interactive)
  (flymake-easy-load (lambda (filename)
                       `("~/bin/mypylint" ,filename))
                     '(("^1:0:F: \\(.*line \\([0-9]+\\))\\)" nil
                        2 nil 1)
                       ("^\\([0-9]+\\):\\([0-9]+\\):\\([FCREW]: .*\\)"
                        nil 1 2 3))
                     'temp-with-folder
                     "py"
                     "^W:"
                     "^[RC]:"))

;; Insertion of Dates.
(defun insert-date-string ()
  "Insert a nicely formated date string."
  (interactive)
  (insert (format-time-string "[%a %b %d %Y / %H:%M %Z]")))

(defun insert-pydebug-string ()
  "Insert a python debugger statement."
  (interactive)
  (insert "import IPython; IPython.embed()"))

(defun insert-pydebug-dep-string ()
  "Insert a python debugger build target."
  (interactive)
  (insert "\"//third_party/py/IPython:ipython-libs\","))

(defun insert-pdb-string ()
  "Insert a python debugger build target."
  (interactive)
  (insert "import pdb; pdb.set_trace()"))

(defun intelligent-close ()
  "Quit a frame the same way no matter what kind of frame you are on."
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
(global-set-key (kbd "C-x C-c")
                'intelligent-close)
(global-set-key (kbd "<f5>")
                'make-frame)
(global-set-key (kbd "<f6>")
                'make-frame-on-display)
(global-set-key (kbd "C-c C-d")
                'delete-trailing-whitespace)

(defun insert-scc-at-point ()
  "Insert an SCC annotation at point."
  (interactive)
  (if (or (looking-at "\b\|[ t]\|$")
          (and (not (bolp))
               (save-excursion
                 (forward-char -1)
                 (looking-at "\b\|[ t]"))))
      (let ((space-at-point (looking-at "[ t]")))
        (unless (and (not (bolp))
                     (save-excursion
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
      (while (not (or (looking-at scc)
                      (bolp)))
        (forward-char -1))
      (if (and (looking-at scc)
               (<= (match-beginning 1) old-point)
               (> (match-end 1) old-point))
          (kill-region (match-beginning 0) (match-end 0))
        (error "No SCC at point")))))
(global-set-key (kbd "C-c y")
                'insert-scc-at-point)
(global-set-key (kbd "C-c k")
                'kill-scc-at-point)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil
                         :background "white"
                         :foreground "black"
                         :inverse-video nil
                         :box nil
                         :strike-through nil
                         :overline nil
                         :underline nil
                         :slant normal
                         :weight normal
                         :height 90
                         :width normal
                         :foundry "unknown"
                         :family "DejaVu Sans Mono"))))
 '(flymake-errline ((((class color))
                     (:underline "red"))))
 '(flymake-infoline ((((class color))
                      (:underline "gray"))))
 '(flymake-warnline ((((class color))
                      (:underline "orange")))))

(defun vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(add-hook 'after-save-hook
          (lambda ()
            (when (equal user-init-file (buffer-file-name))
              (condition-case e
                  (load-file user-init-file)
                ((debug error)
                 (message "Error loading .emacs! %s"
                          (error-message-string e)))))))

(add-hook 'markdown-mode-hook 'turn-off-auto-fill)

(setq js2-additional-externs '("goog" "angular" "describe" "it" "xit" "inject"
                               "module" "expect" "beforeEach" "exports" "guitar"
                               "sandman"))


(add-hook 'js2-post-parse-callbacks
          (lambda ()
            (let ((buf (buffer-string))
                  (index 0))
              (while (string-match "\\(goog\\.require\\|goog\\.provide|\\goog\\.module\\)('\\([^'.]*\\)"
                                   buf index)
                (setq index (+ 1
                               (match-end 0)))
                (add-to-list 'js2-additional-externs
                             (match-string 2 buf))))))

(provide '.emacs)
;;; .emacs ends here
