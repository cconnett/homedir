;;; reformat-file.el --- Support for external utilities that reformat an entire file -*- lexical-binding: t; -*-

;;; Commentary:

;; This package provides a function to export a buffer to text, run some utility
;; on a copy, diff the copy, and apply the changes to the current buffer.  This
;; lets you reformat files without losing the point, mark, overlays, and so on.

;; This is not an official Google product.

;; Copyright 2017 Google Inc.
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     https://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Code:

(require 'apply-ed-script)
(require 'cl-lib)

;; TODO(jrockway): Implement per-command error buffers.  (So that different
;; file's formatters put output in different buffers.)
(defconst reformat-file-errors "*reformat-file-errors*"
  "Name of the buffer to show error messages in.")

(defun reformat-file--dump-buffer (&optional name extension)
  "Dump the current buffer to a pair of temporary files, returning their names.

NAME will be a prefix of the temporary file names, and EXTENSION
will be their extension.

If you don't want to dump the whole file, narrow the buffer
before calling this function."
  (let* ((prefix (concat "/tmp/" (or name "")))
         (original (make-temp-file prefix nil extension))
         (reformatted (make-temp-file prefix nil extension)))
    (write-region (point-min) (point-max) original nil 'quiet)
    (write-region (point-min) (point-max) reformatted nil 'quiet)
    (cons original reformatted)))

;; TODO(jrockway): Don't use shell-command-on-region, it's too noisy.
(defun reformat-file--shell-command (command-spec &rest args)
  "Run (shell-command COMMAND-SPEC) with format arguments filled in with ARGS.

Return a list containing (command exit-code stdout)."
  (with-temp-buffer
    (let* ((command (apply #'format (cons command-spec args)))
           (exit-code
            (shell-command-on-region (point) (point) command t nil
                                     reformat-file-errors)))
      (list command exit-code
            (buffer-substring-no-properties (point-min) (point-max))))))

(defun reformat-file--diff (original reformatted)
  "Diff ORIGINAL and REFORMATTED.

Return NIL if there are no differences, or an ed script to
translate ORIGINAL to REFORMATTED otherwise."
  (cl-destructuring-bind (_command exit-code diff)
      (reformat-file--shell-command "/usr/bin/diff -e -- %s %s"
                                    original reformatted)
    (cl-case exit-code
      (0 nil)
      (1 diff)
      (2 (error "Diff encountered a problem!"))
      (t (error "Unknown exit code %d from diff!" exit-code)))))

(defun reformat-file--get-ed-script (command reformatted original)
  "Run COMMAND to reformat REFORMATTED and diff against ORIGINAL.

Return the resulting ed script."
  (when (buffer-live-p (get-buffer reformat-file-errors))
    (kill-buffer reformat-file-errors))
  (reformat-file--shell-command (concat command " %s") reformatted)
  (reformat-file--diff original reformatted))

(defun reformat-file (command &optional name extension)
  "Reformat the current buffer by running COMMAND on the current buffer.

COMMAND is a string representing a shell command to which the
filename to edit's name is appended.  The command is expected to
edit the file in place.

NAME is an optional string to prepend to the generated temporary
file names.  It makes it easier for users to see why they have
files in /tmp if something goes wrong with the automatic
deletion.

EXTENSION is a suffix on the temporary file names, used in case
your tool checks the extension of the file it's editing."
  (interactive "sCommand: ")
  (cl-destructuring-bind (original . reformatted)
      (reformat-file--dump-buffer name extension)
    (let ((script (reformat-file--get-ed-script command reformatted original)))
      (delete-file original)
      (delete-file reformatted)
      (if (not script)
          (message "No changes to apply!")
        (apply-ed-script script)))))

(defun reformat-file-in-place (command &optional name)
  "Reformat the file for the current buffer by running COMMAND.

COMMAND is a string representing a shell command to which the
filename to edit's name is appended.  The command is expected to
edit the file in place.

This function is similar to `reformat-file' but is intended for
use when the reformatter does not work on a temporary file for
some reason (e.g. the directory the file is in matters).

NAME is an optional string to prepend to the generated temporary
file names.  It makes it easier for users to see why they have
files in /tmp if something goes wrong with the automatic
deletion."
  (interactive "sCommand: ")
  (barf-if-buffer-read-only)
  (unless (buffer-file-name)
    (error "No file associated with current buffer"))
  (let* ((prefix (concat "/tmp/" (or name "")))
         (copy-of-original (make-temp-file prefix))
         backup-made-to)
    (unwind-protect
        (progn
          ;; If the buffer is modified, make a backup.
          (when (buffer-modified-p)
            (let ((backup-name (make-temp-file prefix)))
              (copy-file (buffer-file-name) backup-name 'exists-ok 'keep-time)
              (setq backup-made-to backup-name))
            ;; Write whole file.  Note that unlike reformat-file, narrowing is
            ;; ignored.
            (write-region nil nil (buffer-file-name)))
          ;; Save the state then run the script on the original file.
          (copy-file (buffer-file-name) copy-of-original 'exists-ok 'keep-time)
          (let ((ed-script
                 (reformat-file--get-ed-script command (buffer-file-name)
                                               copy-of-original)))
            (clear-visited-file-modtime)
            (if (not ed-script)
                (message "No changes to apply!")
              (apply-ed-script ed-script)
              (copy-file copy-of-original (buffer-file-name)
                         'exists-ok 'keep-time))))

      ;; Clean up: restore the original file if necessary and delete
      ;; temporaries.
      (when backup-made-to
        (copy-file backup-made-to (buffer-file-name) 'exists-ok 'keep-time)
        (delete-file backup-made-to))
      (delete-file copy-of-original))))

;;; Example formatter.

(defun reformat-file-with-perl (script)
  "Apply the Perl expression SCRIPT to each line of the buffer.

The line goes into $_ and whatever $_ is at the end of your
script is replaced in the buffer.

It's an example of how one might use `reformat-file' defined above."
  (interactive "sPerl script: ")
  (reformat-file (format "perl -pi -ne '%s'" script) "perl"))

(provide 'reformat-file)
;;; reformat-file.el ends here
