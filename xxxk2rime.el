#!/bin/sh
":"; exec emacs --quick --script "$0" "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(setq xklb-dictionary (car argv))
(unless (file-exists-p xklb-dictionary)
  (kill-emacs 0))

(require 'seq)

(defun xklb-read-xxxk-dict (file)
  "Read xxxk dictionary file."
  (with-temp-buffer
    (insert-file-contents file)
    ;; decode from gbk
    (let ((buf-str (buffer-string)))
      (erase-buffer)
      (decode-coding-string buf-str 'gbk t (current-buffer))
      (set-text-properties (point-min) (point-max) nil))
    ;; read
    (seq-map
     (lambda (x)
       (let ((line (split-string x)))
         (seq-reverse line)))
     (let (flag)
       (seq-filter
        (lambda (x)
          (unless flag (setq flag (string-equal x "[DATA]")))
          (and flag
               (let ((line (split-string x)))
                 (and (length= line 2)
                      (length< (car line) 7)
                      (length= (cadr line) 1)))))
        (split-string (buffer-string) "\n"))))))

(princ "# Rime dictionary\n")
(princ "# encoding: utf-8\n")
(princ "\n")
(princ "---\n")
(princ "name: xklbdz\n")
(princ "version: \"1\"\n")
(princ "sort: original\n")
(princ "...\n")
(princ "\n")

(let ((dict (xklb-read-xxxk-dict xklb-dictionary)))
  (mapc
   (lambda (x)
     (princ (format "%s\t%s\n" (nth 0 x) (nth 1 x))))
   dict))

(kill-emacs 0)
