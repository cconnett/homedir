#!/opt/emacs/bin/emacs --script

(package-initialize)
(add-to-list 'load-path "~/.emacs.d/elpa/")
(require 'srefactor-lisp)

(message "%s"
         (nth 3 command-line-args))
(find-file (nth 3 command-line-args))
(srefactor-lisp-format-buffer)
(save-buffer)
