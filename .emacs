;;; .emacs --- A .emacs file for cjc.

(package-initialize)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(setq text-quoting-style 'straight)

(defun string-suffix-p (str1 str2 &optional ignore-case)
  "Python: STR1.endswith(STR2).  IGNORE-CASE passed through."
  (let ((begin1 (- (length str1)
                   (length str2)))
        (end1 (length str1)))
    (when (< begin1 0)
      (setq begin1 0))
    (eq t (compare-strings str2 nil nil str1 begin1
                           end1 ignore-case))))

(defun stopwatch-region ()
  "Enclose the region in stopwatch timing commands."
  (interactive)
  (let ((name (read-string "description: ")))
    (save-excursion
      (move-beginning-of-line nil)
      (indent-for-tab-command)
      (insert (format "sw.stop('%s')" name))
      (newline-and-indent)
      (exchange-point-and-mark)
      (move-beginning-of-line nil)
      (indent-for-tab-command)
      (insert (format "sw.start('%s')\n" name))
      (indent-for-tab-command))))
(global-set-key (kbd "C-c n")
                'stopwatch-region)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(add-to-list 'load-path "~/.emacs.d/elpa/")

(defvar at-google (string-suffix-p system-name "corp.google.com")
  "Whether the current machine is a Google corp workstation.")

;; Requires
(require 'pp)
(require 'font-lock)
                                        ;(require 'ido)
(require 'ivy)
(require 'flymake)
(require 'flymake-cursor)
(require 'flymake-easy)
(require 'highlight-symbol)
(require 'column-marker)
(require 'srefactor)
                                        ; (require 'srefactor-lisp)
(require 'clang-format)
                                        ; (require 'paren)

;; Home only
(unless at-google
  (autoload 'python-mode "python-mode" "Python Mode."
    t))
;; Google only
(when at-google
  (load-file "/usr/share/emacs/site-lisp/emacs-google-config/devtools/editors/emacs/google.el")
  (require 'google)
  (require 'google-coding-style)
  (require 'google-cc-extras)
  (require 'google-pyformat)
                                        ;(require 'flymake-jslint)
  (google-cc-extras/bind-default-keys)
  (setq create-lockfiles nil))

;; Machine formatting
(defun clang-format-file ()
  "Format the whole buffer with clang."
  (interactive)
  (clang-format-region (point-min)
                       (point-max)
                       "file"))

(defun local-google-pyformat ()
  (interactive)
  (let* ((new-file-name (make-temp-file "emacs"))
         (get-changed-lines-command (concat "diff -U0 " buffer-file-name " " new-file-name
                                            " | grep @@ | " " cut -d' ' -f3 | " " perl -n -e '/[+]+(\\d+)(?:,(\\d+))?/; "
                                            "print \"-l \" . $1 . \"-\" . ($1+$2) . \" \"'"))
         (lines (progn
                  (append-to-file nil nil new-file-name)
                  (if (file-exists-p buffer-file-name)
                      (shell-command-to-string get-changed-lines-command)
                    "")))
         (formatter-command (concat "python3 -m yapf -i  " pyformat-args
                                    " " lines)))
    (message "%s" formatter-command)
    (if (and (zerop (length lines))
             (file-exists-p buffer-file-name))
        (message "No changes. Skipping formatting.")
      (progn
        (if (zerop (length lines))
            (message "Formatting entire file.")
          (message "Formatting lines %s" lines))
        (reformat-file formatter-command "yapf" ".py")
        (delete-file new-file-name)))))

(defun google-pyformat-all ()
  (interactive)
  (reformat-file (concat "/usr/local/bin/yapf " pyformat-args)
                 "pyformat"
                 ".py"))

(defun google-buildifer ()
  "Run buildifier on the current file."
  (interactive)
  (reformat-file "/usr/bin/buildifier" "buildifier"
                 ".bzl"))
(defun google-mdformat ()
  "Run http://go/mdformat on the current file."
  (interactive)
  (reformat-file "/google/data/ro/teams/g3doc/mdformat --in_place"
                 "mdformat" ".md"))
(defun google-sqlformat ()
  "Run http://go/googlesql_format on the current file."
  (interactive)
  (reformat-file "~/bin/sqlfmtwrapper" "googlesql"
                 ".sql"))
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
(defun google-fpbfmt ()
  "Run protoprint on the current file."
  (interactive)
  (reformat-file "/usr/bin/fpb" "fpb" "textproto"))
(defun hsfmt ()
  "Run hindent on the current file."
  (interactive)
  (reformat-file (expand-file-name "~/bin/hindent --sort-imports --line-length 80 --indent-size 2")
                 "hs"
                 ".hs"))
(defun htmlfmt ()
  "Run tidy on the current file."
  (interactive)
  (reformat-file (expand-file-name "/usr/bin/tidy -q --doctype omit")
                 "html"
                 ".html"))

(defun save-buffer-without-format ()
  (interactive)
  (let ((b (current-buffer)))
    (with-temp-file (format "/tmp/%s"
                            (buffer-name b))
      (let ((before-save-hook (remove #'format-mode-format-file before-save-hook)))
        (with-current-buffer b
          (let ((before-save-hook (remove #'format-mode-format-file before-save-hook)))
            (save-buffer)))))))

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
            '(c-mode c++-mode js-mode js2-mode protobuf-mode
                     typescript-mode))
      (if at-google
          (clang-format-file)
        (clang-format-file)))
     ((memq major-mode
            '(json-mode))
      (json-mode-beautify))
     ;; ((memq major-mode
     ;;        '(typescript-mode))
     ;;  (tide-format-before-save))
     ;; ((memq major-mode
     ;;        '(protobuffer-mode))
     ;;  (google-fpbfmt))
     ((memq major-mode
            '(python-mode))
      (google-pyformat-all))
     ((memq major-mode
            '(skylark-mode))
      (google-buildifier))
     ((memq major-mode
            '(markdown-mode))
      (google-mdformat))
     ((memq major-mode
            '(sql-mode))
      (google-sqlformat))
     ((memq major-mode
            '(gcl-mode borg-mode))
      (google-gclfmt))
     ((memq major-mode
            '(ncl-mode))
      (google-nclfmt))
     ((memq major-mode
            '(haskell-mode))
      (hsfmt))
     ((memq major-mode
            '(emacs-lisp-mode lisp-mode z3-mode z3-smt2-mode))
      (lispfmt))
     ;; ((memq major-mode
     ;;        '(html-mode))
     ;;  (htmlfmt))
     (t (message "No formatter found for %s" major-mode)))
    (when (symbol-value 'flymake-mode)
      (flymake-restart-syntax-check))))

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
                                        ;(setq ido-enable-flex-matching t)
(setq ido-ignore-files '("\\~$"))

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
(global-set-key (kbd "C-c TAB")
                'tab-to-tab-stop)
(global-set-key (kbd "C-c i")
                'insert-date-string)
(global-set-key (kbd "C-c d")
                'insert-pydebug-string)
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
(global-set-key [f15]
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
(global-set-key [f9]
                'format-mode-format-file)
(global-set-key [(control f9)]
                'google-pyformat-all)

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
;; (global-unset-key [f2])
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

(defun flymake-jslint-load ()
  (interactive)
  ;; (REGEXP FILE-IDX LINE-IDX COL-IDX ERR-TEXT-IDX)
  (flymake-easy-load (lambda (filename)
                       `("~/bin/myjslint" ,filename))
                     '(("^\\([^:]+\\):\\([^:]+\\):\\([^:]+\\):\\([^:]+\\)$"
                        1 2 4 3))
                     'temp-with-folder
                     "js"
                     ""))

(add-hook 'js2-mode-hook 'flymake-jslint-load)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(remove-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

(add-hook 'latex-mode-hook 'll-mode)
(add-hook 'bibtex-mode-hook
          '(lambda ()
             (menu-bar-mode 1)))

(setq frame-title-format '("%b - " "emacs@" system-name))
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
                                ("\\.ino$" . c++-mode)
                                ("\\.PY$" . python-mode)
                                ("\\.[hg]s$" . haskell-mode)
                                ("\\.c$" . c-mode)
                                ("\\.cc$" . c++-mode)
                                ("\\.clif$" . c++-mode)
                                ("\\.css$" . css-mode)
                                ("\\.gss$" . css-mode)
                                ("BUILD$" . bazel-mode)
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
                                ("\\.smt$" . z3-mode)
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
 '(auto-hscroll-mode t)
 '(c-basic-offset 2)
 '(clang-format-executable "~/bin/clang-format")
 '(clang-format-style "google")
 '(css-indent-offset 2)
 '(desktop-save-mode t)
 '(flymake-info-line-regexp ":[RC]:")
 '(flymake-jslint-command "~/bin/myjslint")
 '(flymake-jslint-detect-trailing-comma nil)
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
 '(js2-global-externs (quote ("chrome" "angular" "require" "setTimeout")))
 '(js2-highlight-level 3)
 '(js2-include-node-externs t)
 '(js2-mirror-mode t)
 '(js2-mode-escape-quotes nil)
 '(js2-strict-trailing-comma-warning nil)
 '(lisp-indent-fuction (quote common-lisp-indent-function))
 '(markdown-enable-math t)
 '(mouse-yank-at-point t)
 '(org-support-shift-select nil)
 '(package-selected-packages (quote (boogie-friends z3-mode flx php-mode swiper
                                                    srefactor flymake-easy flymake-cursor json-mode
                                                    js2-mode)))
 '(py-continuation-offset 2)
 '(py-indent-offset 2 t)
 '(py-smart-indentation nil)
 ;; '(pyformat-args (concat "-i --style "
 ;;                         (expand-file-name "~/homedir/.style.yapf"))
 ;;                 t)

 '(pyformat-args "-i " t)
 '(safe-local-variable-values (quote ((encoding . utf-8)
                                      (Encoding . utf-8))))
 '(sgml-basic-offset 2)
 '(sh-basic-offset 2)
 '(sh-indentation 2)
 '(standard-indent 2)
 '(typescript-indent-level 2)
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
  (insert (format-time-string "%Y-%m-%d %H:%M %Z")))

(defun insert-pydebug-string ()
  "Insert a python debugger statement."
  (interactive)
  (insert "import IPython; IPython.embed()"))

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
                      (:underline "orange"))))
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

(setq js2-additional-externs '("goog" "angular" "describe" "fdescribe" "xdescribe"
                               "it" "fit" "xit" "inject" "module" "expect"
                               "beforeEach" "exports" "guitar" "sandman"
                               "chrome" "Mousetrap" "$" "jQuery" "require"
                               "spyOn" "beforeEach" "jasmine" "setInterval"
                               "setTimeout" "clearInterval" "Intl"))


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
