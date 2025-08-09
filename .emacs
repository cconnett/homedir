;;; .emacs --- A .emacs file for cjc.

(package-initialize)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(setq text-quoting-style 'straight)
(setq ring-bell-function 'ignore)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/highlight-symbol.el/")
(add-to-list 'load-path "~/.emacs.d/elpa/")

(require 'font-lock)
(require 'ivy)
(require 'highlight-symbol)
(require 'column-marker)
(require 'flymake)
(require 'apheleia)
(require 'python-mode)
(require 'cc-mode)

;; Machine formatting

;; (defun save-buffer-without-format ()
;;   (interactive)
;;   (let ((b (current-buffer)))
;;     (with-temp-file (format "/tmp/%s"
;;                             (buffer-name b))
;;       (let ((before-save-hook (remove #'format-mode-format-file before-save-hook)))
;;         (with-current-buffer b
;;           (let ((before-save-hook (remove #'format-mode-format-file before-save-hook)))
;;             (save-buffer)))))))

;; (define-minor-mode format-mode
;;   "Machine format the buffer before saving."
;;   :lighter " Format"
;;   (unless (member 'format-mode-format-file before-save-hook)
;;     (add-hook 'before-save-hook #'format-mode-format-file)))

;; (global-set-key [f12]
;;                 #'format-mode)

;; (defun format-mode-format-file ()
;;   "Format the current buffer with a machine formatter for the major mode."
;;   (interactive)
;;   (when (symbol-value 'format-mode)
;;     (message "Machine formatting for %s" major-mode)
;;     (cond
;;      ((memq major-mode
;;             '(c-mode c++-mode js-mode js2-mode protobuf-mode
;;                      typescript-mode))
;;       (clang-format-file))
;;      ((memq major-mode
;;             '(json-mode))
;;       (json-mode-beautify))
;;      ;; ((memq major-mode
;;      ;;        '(typescript-mode))
;;      ;;  (tide-format-before-save))
;;      ;; ((memq major-mode
;;      ;;        '(protobuffer-mode))
;;      ;;  (google-fpbfmt))
;;      ((memq major-mode
;;             '(python-mode))
;;       (python-black-all))
;;      ((memq major-mode
;;             '(markdown-mode))
;;       (google-mdformat))
;;      ((memq major-mode
;;             '(haskell-mode))
;;       (hsfmt))
;;      ((memq major-mode
;;             '(emacs-lisp-mode lisp-mode z3-mode z3-smt2-mode))
;;       (lispfmt))
;;      ;; ((memq major-mode
;;      ;;        '(html-mode))
;;      ;;  (htmlfmt))
;;      (t (message "No formatter found for %s" major-mode)))))

;; Auto-save files go in system temp.
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; XWindows preferences
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (window-system)
  (setq default-frame-alist '((left-fringe . 0)
                              (right-fringe . 0)))
  (set-scroll-bar-mode 'right)
  (setq confirm-kill-emacs 'y-or-n-p))

;; ido mode settings
(ivy-mode t)
(setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

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
(setq-default fill-column 80)
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

(global-set-key (kbd "C-c C-d")
                'delete-trailing-whitespace)
(global-set-key (kbd "C-<f12>")
                (lambda ()
                  (interactive)
                  (load-file user-init-file)))
(global-set-key (kbd "C-s-<f12>")
                (lambda ()
                  (interactive)
                  (save-buffers-kill-emacs)))
(global-set-key (kbd "C-c SPC")
                'just-one-space)
(global-set-key (kbd "M-SPC")
                'just-one-space)
(global-set-key (kbd "C-c TAB")
                'tab-to-tab-stop)
(global-set-key (kbd "C-c i")
                'insert-date-string)
(global-set-key (kbd "C-c s")
                'sort-lines)
(bind-key* "C-c C-s" 'sort-lines)
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

(global-set-key (kbd "s-t")
                'my/transpose-args)

(global-set-key (kbd "C-x f")
                'ido-find-file)
(global-set-key (kbd "C-x s")
                'save-buffer)
(global-set-key (kbd "RET")
                'newline-and-indent)
(global-set-key (kbd "C-x C-i")
                'insert-char)
;; These don't work in -nw mode.
(global-set-key (kbd "M-j")
                'next-line)
(global-set-key (kbd "M-k")
                'previous-line)
(global-set-key (kbd "M-h")
                'backward-char)
(global-set-key (kbd "M-l")
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
(global-set-key [mouse-8]
                'pop-to-mark-command)
(global-set-key [mouse-9]
                'unpop-to-mark-command)
(global-set-key (kbd "M-<left>")
                'pop-to-mark-command)
(global-set-key (kbd "M-<right>")
                'unpop-to-mark-command)
(global-set-key [vertical-scroll-bar down-mouse-1]
                'scroll-bar-drag)
(global-set-key [vertical-scroll-bar drag-mouse-1]
                'scroll-bar-drag)
(global-set-key [f9]
                'format-mode-format-file)
(global-set-key [(control f9)]
                'python-black-all)

;; (global-set-key (kbd "M-p")
;;                 (lambda ()
;;                   (interactive)
;;                   (flymake-goto-prev-error)
;;                   (let ((err-info (flymake-find-err-info flymake-err-info
;;                                                          (line-number-at-pos))))
;;                     (if (car err-info)
;;                         (message "%s"
;;                                  (flymake-ler-text (caar err-info)))
;;                       (message "No lint errors.")))))
;; (global-set-key (kbd "M-n")
;;                 (lambda ()
;;                   (interactive)
;;                   (flymake-goto-next-error)
;;                   (let ((err-info (flymake-find-err-info flymake-err-info
;;                                                          (line-number-at-pos))))
;;                     (if (car err-info)
;;                         (message "%s"
;;                                  (flymake-ler-text (caar err-info)))
;;                       (message "No lint errors.")))))

(global-unset-key (kbd "<insert>"))
(global-set-key (kbd "C-z")
                'undo)
(global-unset-key [(control x)
                   (control z)])

;; Windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(global-set-key (kbd "C-<next>") 'next-window-any-frame)
(global-set-key (kbd "C-<prior>") 'previous-window-any-frame)

;; Python font faces
(make-face 'py-comment-face)
(make-face 'py-keyword-face)
(make-face 'py-pseudo-keyword-face)
(make-face 'py-type-face)
(set-face-background 'show-paren-match "cornflower blue")
(set-face-foreground 'font-lock-string-face
                     "forest green")
(set-face-foreground 'py-comment-face "firebrick")
(set-face-foreground 'py-keyword-face "medium blue")
(set-face-foreground 'py-type-face "steel blue")
(add-hook 'python-mode-hook
          (lambda ()
            (set (make-local-variable 'font-lock-type-face)
                 'py-type-face)
            (set (make-local-variable 'font-lock-comment-face)
                 'py-comment-face)
            (set (make-local-variable 'font-lock-keyword-face)
                 'py-keyword-face)
            (set-face-foreground 'py-pseudo-keyword-face
                                 "dodger blue")
            (bind-key* "C-<backspace>" 'backward-kill-word)
            (bind-key* "<del>" 'delete-forward-char)

            )
          )

(add-hook 'sh-mode-hook
          (lambda ()
            (set-face-foreground 'sh-heredoc-face "dark magenta")))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(remove-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

(add-hook 'latex-mode-hook 'll-mode)
(add-hook 'bibtex-mode-hook
          (lambda ()
            (menu-bar-mode 1)))

(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

(setq frame-title-format '("%b - " "emacs@" system-name))
(setq-default font-lock-use-fonts t)
(setq-default font-lock-use-colors t)
(setq-default font-lock-maximum-decoration
              t)
(setq-default scroll-preserve-screen-position
              t)
(setq-default indent-tabs-mode nil)
                                        ;(setq-default format-mode t)
(apheleia-global-mode +1)
(setq-default default-major-mode 'text-mode)

(setq interpreter-mode-alist (cons '("python" . python-mode) interpreter-mode-alist))

(setq auto-mode-alist (append '(("\\.C$" . c++-mode)
                                ("\\.ino$" . c++-mode)
                                ("\\.PY$" . python-mode)
                                ("\\.[hg]s$" . haskell-mode)
                                ("\\.c$" . c-mode)
                                ("\\.cc$" . c++-mode)
                                ("\\.css$" . css-mode)
                                ("BUILD$" . bazel-mode)
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
                                ("\\.pl$" . perl-mode)
                                ("\\.pp$" . c++-mode)
                                ("\\.pro$" . prolog-mode)
                                ("\\.py$" . python-mode)
                                ("\\.smt$" . z3-mode)
                                ("\\.tex$" . latex-mode)
                                ("\\.txt$" . text-mode)
                                )
                              auto-mode-alist))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(apheleia-mode-alist
   '((asm-mode . asmfmt)
     (awk-mode . gawk)
     (bash-ts-mode . shfmt)
     (bazel-mode . buildifier)
     (beancount-mode . bean-format)
     (c++-ts-mode . clang-format)
     (caddyfile-mode . caddyfmt)
     (cc-mode . clang-format)
     (c-mode . clang-format)
     (c-ts-mode . clang-format)
     (c++-mode . clang-format)
     (caml-mode . ocamlformat)
     (clojure-mode . cljfmt)
     (clojure-ts-mode . cljfmt)
     (cmake-mode . cmake-format)
     (cmake-ts-mode . cmake-format)
     (common-lisp-mode . lisp-indent)
     (conf-toml-mode . dprint)
     (cperl-mode . perltidy)
     (crystal-mode . crystal-tool-format)
     (csharp-mode . csharpier)
     (css-mode . prettier-css)
     (css-ts-mode . prettier-css)
     (dart-mode . dart-format)
     (dart-ts-mode . dart-format)
     (dockerfile-mode . dprint)
     (elixir-mode . mix-format)
     (elixir-ts-mode . mix-format)
     (elm-mode . elm-format)
     (emacs-lisp-mode . lisp-indent)
     (fish-mode . fish-indent)
     (gdscript-mode . gdformat)
     (gdscript-ts-mode . gdformat)
     (gleam-ts-mode . gleam)
     (go-mode . gofmt)
     (go-ts-mode . gofmt)
     (graphql-mode . prettier-graphql)
     (haskell-mode . fourmolu)
     (haskell-ts-mode . fourmolu)
     (hcl-mode . hclfmt)
     (html-mode . prettier-html)
     (html-ts-mode . prettier-html)
     (hurl-mode . hurlfmt)
     (java-mode . google-java-format)
     (java-ts-mode . google-java-format)
     (jinja2-mode)
     (js3-mode . prettier-javascript)
     (js-json-mode . prettier-json)
     (js-mode . prettier-javascript)
     (js-ts-mode . prettier-javascript)
     (json-mode . prettier-json)
     (json-ts-mode . prettier-json)
     (kotlin-mode . ktlint)
     (kotlin-ts-mode . ktlint)
     (latex-mode . latexindent)
     (LaTeX-mode . latexindent)
     (lua-mode . stylua)
     (lua-ts-mode . stylua)
     (lisp-mode . lisp-indent)
     (nasm-mode . asmfmt)
     (nix-mode . nixfmt)
     (nix-ts-mode . nixfmt)
     (nomad-mode . nomad)
     (perl-mode . perltidy)
     (php-mode . phpcs)
     (purescript-mode . purs-tidy)
     (python-mode isort black)
     (python-ts-mode . black)
     (robot-mode . robotidy)
     (ruby-mode . prettier-ruby)
     (ruby-ts-mode . prettier-ruby)
     (rustic-mode . rustfmt)
     (rust-mode . rustfmt)
     (rust-ts-mode . rustfmt)
     (snakemake-mode . snakefmt)
     (scss-mode . prettier-scss)
     (sql-mode . pgformatter)
     (svelte-mode . prettier-svelte)
     (terraform-mode . terraform)
     (TeX-latex-mode . latexindent)
     (TeX-mode . latexindent)
     (toml-ts-mode . taplo)
     (tsx-ts-mode . prettier-typescript)
     (tuareg-mode . ocamlformat)
     (typescript-mode . prettier-typescript)
     (typescript-ts-mode . prettier-typescript)
     (typst-mode . typstyle)
     (typst-ts-mode . typstyle)
     (v-mode . vfmt)
     (web-mode . prettier)
     (yaml-mode . prettier-yaml)
     (yaml-ts-mode . prettier-yaml)
     (yang-mode . pyang)
     (zig-mode . zig-fmt)
     (zig-ts-mode . zig-fmt)))
 '(auto-hscroll-mode t)
 '(c-basic-offset 4)
 '(css-indent-offset 4)
 '(desktop-save-mode t)
 '(flymake-info-line-regexp ":[RC]:")
 '(flymake-jslint-detect-trailing-comma nil)
 '(flymake-warn-line-regexp ":W:")
 '(flyspell-issue-welcome-flag nil)
 '(haskell-font-lock-symbols t)
 '(haskell-program-name "ghci -fglasgow-exts")
 '(highlight-symbol-on-navigation-p t)
 '(ido-default-file-method 'selected-window)
 '(ilisp-*use-fsf-compliant-keybindings* t)
 '(inferior-lisp-program "/usr/bin/sbcl --noinform")
 '(lisp-indent-fuction 'common-lisp-indent-function)
 '(markdown-enable-math t)
 '(mouse-yank-at-point t)
 '(package-selected-packages
   '(python-mode eldoc flycheck ivy flycheck-pycheckers flymake apheleia haskell-mode z3-mode json-mode))
 '(py-continuation-offset 4)
 '(py-indent-offset 4)
 '(py-smart-indentation nil)
 '(safe-local-variable-values '((encoding . utf-8) (Encoding . utf-8)))
 '(sgml-basic-offset 4)
 '(sh-basic-offset 4)
 '(standard-indent 4)
 '(typescript-indent-level 4)
 '(vc-follow-symlinks t))

;; Insertion of Dates.
(defun insert-date-string ()
  "Insert a nicely formated date string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M %Z")))

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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 110 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(flymake-error ((((class color)) (:underline "red"))))
 '(flymake-infoline ((((class color)) (:underline "gray"))))
 '(flymake-warning ((((class color)) (:underline "orange"))))
 '(markdown-code-face ((t (:inherit fixed-pitch :background "azure3")))))

(set-face-attribute 'region nil :background "black")
(set-face-attribute 'region nil :foreground "white")

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

(make-variable-buffer-local 'auto-hscroll-mode)
(add-hook 'markdown-mode-hook
          (lambda ()
            (setq auto-hscroll-mode nil)
            (auto-fill-mode t)))

(provide '.emacs)
