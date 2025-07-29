;;; restclient-edit.el --- Edit elisp hooks  -*- lexical-binding: t; -*-

;; Author: shyam

;;; Commentary:

;; Edit elisp expressions hook definitions in a separate buffer

;;; Code:

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
      (kill-buffer (current-buffer))
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
  (kill-buffer (current-buffer)))

(defun restclient-edit-indirect ()
  "Open new emacs-lisp buffer to edit s-expression associated with a hook
and save back to the restclient buffer"
  (interactive)
  (let ((begin) (end) (sexp) (buffer) (mbegin)
	(p (point))
	(src-buf (current-buffer))
	(regexp (rx bol "->" (+ space)
		    (group (+ (not space))) (+ space)
		    (group "(" (* any)) eol)))
    (save-excursion
      (goto-char (line-beginning-position))
      (when (or (looking-at regexp)
		(re-search-backward regexp nil nil))
	(setq mbegin (match-beginning 0)
	      begin (match-beginning 2))
	(goto-char begin)
	(forward-sexp)
	(setq end (point))
	(when (and begin end
		   (> end begin)
		   (>= p mbegin)
		   (<= p end))
	  (setq buffer (concat "*" (match-string-no-properties 1) "*")
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
