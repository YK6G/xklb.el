;; -*- lexical-binding: t; -*-

(require 'quail)
(require 'subr-x)
(require 'seq)

(defvar xklb-dictionary
  (expand-file-name "xklbdz.dict.yaml" user-emacs-directory))
(defvar xklb-cache-file
  (expand-file-name "xklb-dict.el" user-emacs-directory))

(defvar xklb-punctuation
  '(("," "，")
    ("." "。")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data struct
(defsubst xklb--make-codetree (code trans)
  (list code trans))

(defsubst xklb--get-code (codetree)
  (car codetree))

(defsubst xklb--get-trans (codetree)
  (cadr codetree))

(defsubst xklb--set-trans (codetree trans)
  (setcdr codetree (cons trans (cddr codetree))))

(defsubst xklb--get-subtree (codetree code)
  (assq code (cddr codetree)))

(defsubst xklb--insert-tree (codetree subtree)
  (setcdr (cdr codetree) (cons subtree (cddr codetree))))

(defun xklb--insert-trans (codetree trans)
  (xklb--set-trans codetree
		   (vconcat (xklb--get-trans codetree) trans)))

(defun xklb--query-tree (codetree codelist)
  (when-let* ((code (car codelist))
	      (entry (xklb--get-subtree codetree code)))
    (let ((rest (cdr codelist)))
      (if (null rest)
	  entry
	(xklb--query-tree entry rest)))))

(defun xklb--mapc-tree (codetree function)
  (funcall function codetree)
  (mapc (lambda (tree) (xklb--mapc-tree tree function))
	(cddr codetree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xklb-defrule (codetree codelist trans)
  (let ((code (car codelist))
	(entry (xklb--get-subtree codetree (car codelist)))
	(rest (cdr codelist)))
    (cond ((and entry rest)       ;; continue
	   (xklb-defrule entry rest trans))
	  ((and entry (null rest))  ;; insert
	   (xklb--insert-trans entry trans))
	  ((and (null entry) rest)  ;; make a new tree and continue
	   (let ((new-tree (xklb--make-codetree code '())))
	     (xklb--insert-tree codetree new-tree)
	     (xklb-defrule new-tree rest trans)))
	  ((null (and entry rest))  ;; make a new tree
	   (xklb--insert-tree
	    codetree
	    (xklb--make-codetree code trans))))))

(defun xklb--gen-guidance (codetree)
  (mapc (lambda (code)
	  (when-let* ((tree (xklb--get-subtree codetree code))
		      (trans (xklb--get-trans tree))
		      (ch (aref trans 0)))
	    (xklb--insert-trans codetree (vector ch))))
	"aeiou"))

(defun xklb--read-dict (file)
  "Read dictionary file."
  (with-temp-buffer
    (insert-file-contents file)
    (mapcar
     (lambda (line)
       (let ((c (split-string line)))
	 (cons (string-to-list (cadr c))
	       (vector (car c)))))
     (seq-filter (lambda (x)
		   (and (length> x 2)
			(eq (elt x 1) ?\t)))
		 (split-string (buffer-string) "\n")))))

(defun xklb--build-tree (file)
  (let ((rules (xklb--read-dict file))
	(tree (xklb--make-codetree nil nil)))
    (mapc (lambda (x) (xklb-defrule tree (car x) (cdr x)))
	  rules)
    ;; build guiance
    (xklb--mapc-tree tree #'xklb--gen-guidance)
    tree))

(defun xklb-setup (&optional file)
  (quail-define-package
   "xklb" "chinese" "星" t
   "星空两笔输入法。"
   '((" " . quail-select-current)) t)
  (if (file-exists-p xklb-cache-file)
      (quail-install-map
       (with-temp-buffer
	 (insert-file-contents xklb-cache-file)
	 (read (current-buffer))))
    (quail-install-map
     (cons nil (cddr (xklb--build-tree (or file xklb-dictionary)))) "xklb")))

(defun xklb-make-cache ()
  (with-temp-file xklb-cache-file
    (prin1
     (cons nil (cddr (xklb--build-tree xklb-dictionary)))
     (current-buffer))))

(provide 'xklb)
