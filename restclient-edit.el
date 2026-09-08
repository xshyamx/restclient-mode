;;; restclient-edit.el --- Edit elisp hooks  -*- lexical-binding: t; -*-

;; Author: shyam

;;; Commentary:

;; Edit elisp expressions hook definitions in a separate buffer

;;; Code:


(defconst restclient-elisp-var-or-hook-regexp
  (concat "^\\(?:"
	  ;; hook
	  "->" restclient--space+ (regexp-opt '("pre-request" "on-response") t)
	  restclient--space+ "\\(.*\\)"
	  "\\|"
	  ;; elisp variable declaration
	  restclient-var-prefix "\\(" restclient-var-name-regexp "\\)"
	  restclient--space* restclient-elisp-var-assigment restclient--space*
	  "\\(" restclient-multi-line-begin "\\|.*\\)"
	  "\\)$")
  "Regexp to match elisp variable or hook")

(defun restclient-src-commit ()
  "Update the s-expression associated with a hook in the originating
restclient buffer"
  (interactive)
  (let ((buffer (buffer-local-value 'src-buffer (current-buffer)))
	(begin (buffer-local-value 'src-begin (current-buffer)))
	(end (buffer-local-value 'src-end (current-buffer)))
	(sexp (buffer-local-value 'src-sexp (current-buffer)))
	(s (buffer-string)))
    (when (and begin end buffer)
      (kill-buffer-and-window)
      ;; update only if there is a change
      (unless (string= s sexp)
	(with-current-buffer buffer
	  (save-excursion
	    (goto-char begin)
	    (atomic-change-group
	      (delete-region begin end)
	      (insert s))))))))

(defun restclient-src-abort ()
  "Close the edit buffer without saving changes to the originating
restclient buffer"
  (interactive)
  (kill-buffer-and-window))

(defun restclient-edit-indirect ()
  "Open new emacs-lisp buffer to edit s-expression associated with a hook
and save back to the restclient buffer"
  (interactive)
  (let ((begin) (end) (exp) (sexp) (buffer) (mbegin)
	(p (point))
	(src-buf (current-buffer)))
    (save-excursion
      (beginning-of-line)
      (when (or (looking-at restclient-elisp-var-or-hook-regexp)
		(re-search-backward restclient-elisp-var-or-hook-regexp nil nil))
	(setq mbegin (match-beginning 0)
	      begin (or (match-beginning 2)
			(match-beginning 4))
	      exp (or (match-string-no-properties 1)
		      (match-string-no-properties 3)))
	(goto-char begin)
	(when (string= "<<" (match-string-no-properties 4))
	  (forward-line)
	  (setq begin (point)))
	(forward-sexp)
	(setq end (point))
	(when (and begin end
		   (> end begin)
		   (>= p mbegin)
		   (<= p end))
	  (setq buffer (concat "*" exp "*")
		sexp (buffer-substring-no-properties begin end))
	  (with-current-buffer (get-buffer-create buffer)
	    (erase-buffer)
	    (insert sexp)
	    (emacs-lisp-mode)
	    (keymap-local-set "C-c '" #'restclient-src-commit)
	    (keymap-local-set "C-c C-k" #'restclient-src-abort)
	    (setq-local src-buffer src-buf
			src-begin begin
			src-end end
			src-sexp sexp))
	  (switch-to-buffer-other-window buffer)
	  (message "Edit, %s to save and %s to abort"
		   (propertize "C-c '" 'face 'highlight)
		   (propertize "C-c C-k" 'face 'highlight)))))))

(provide 'restclient-edit)
;;; restclient-edit.el
