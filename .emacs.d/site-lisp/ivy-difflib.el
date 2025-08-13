;; Ivy configuration with difflib.el for similarity-based sorting

(require 'ivy)
(require 'heap)
(use-package difflib
  :ensure t)
(setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

(cl-defun difflib-get-top-matches (word possibilities &key (n 3))
  "Use SequenceMatcher to return list of the best \"good enough\" matches.

WORD is a sequence for which close matches are desired (typically a string).

POSSIBILITIES is a list of sequences against which to match word
;; (typically a list of strings).

Optional arg N (default 3) is the maximum number of close matches to return. N
must be > 0.

This implementation keeps a heap of the best N results to obviate an explicit CUTOFF.

The best (no more than N) matches among the POSSIBILITES are returned in a
list, sorted by similarity score, most similar first."
  (when (not (> n 0))
    (error "N must be > 0: %S" n))
  (let ((s (difflib--make-matcher))
        (cutoff 0.0)
        (heap (make-heap (lambda (a b) (< (car a) (car b))) n)))
    (difflib-set-seq2 s word)
    (cl-loop for x being the elements of possibilities
             do (difflib-set-seq1 s x)
             if (and (>= (difflib-real-quick-ratio s) cutoff)
                     (>= (difflib-quick-ratio s) cutoff)
                     (>= (setq ratio (difflib-ratio s)) cutoff))
             do (progn
                  (when (>= (heap-size heap) n)
                    (heap-delete-root heap)
                    (setq cutoff (car (heap-root heap))))
                  (heap-add heap (cons ratio x))))
    ;;(print heap)
    (let ((ret nil))
      (cl-loop while (not (heap-empty heap))
               do (setq ret (cons (cdr (heap-delete-root heap)) ret)))
      ret)))


;; Custom sorting function using difflib
(defun ivy-difflib-sort-function (name candidates)
  "Sort CANDIDATES by similarity to NAME using difflib.
Returns candidates sorted by their similarity ratio to the input."
  (if (< (length name) 1)  ; Don't sort if input is empty
      candidates
    (difflib-get-top-matches name candidates :n ivy-height)))

;; Configure Ivy to use difflib sorting for specific commands
(defun ivy-configure-difflib-sorting ()
  "Configure Ivy to use difflib-based sorting."
  (interactive)

  ;; Set as default sorting for all Ivy completions
  (setq ivy-sort-matches-functions-alist
        '((t . ivy-difflib-sort-function)))
  )

;; Main configuration function
(defun ivy-difflib-setup ()
  "Set up Ivy with difflib sorting."
  (interactive)

  ;; Basic setup
  (ivy-mode 1)
  (counsel-mode 1)

  ;; Configure sorting
  (ivy-configure-difflib-sorting)

  ;; Set up some additional Ivy settings that work well with fuzzy matching
  (setq ivy-initial-inputs-alist nil)  ; Don't start with ^
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-height 15)

  (message "Ivy configured with difflib sorting!"))

;; Run the setup
(ivy-difflib-setup)

;; Optional: Bind a key to toggle between difflib and default sorting
(defvar ivy-difflib-enabled t
  "Whether difflib sorting is currently enabled.")

(defun ivy-toggle-difflib-sorting ()
  "Toggle between difflib and default Ivy sorting."
  (interactive)
  (setq ivy-difflib-enabled (not ivy-difflib-enabled))
  (if ivy-difflib-enabled
      (progn
        (setq ivy-sort-matches-functions-alist
              '((t . ivy-difflib-sort-function)))
        (message "Difflib sorting enabled"))
    (setq ivy-sort-matches-functions-alist
          '((t . nil)))
    (message "Difflib sorting disabled")))

;; Bind to a convenient key
(global-set-key [f6] 'ivy-toggle-difflib-sorting)
