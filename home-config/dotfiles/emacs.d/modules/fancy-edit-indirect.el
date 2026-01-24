;;; fancy-edit-indirect.el --- edit-indirect but fancy   -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(declare-function pm-innermost-span "polymode")

(defun fancy-edit-indirect ()
  "Edit the current block in an indirect buffer."
  (interactive)
  (switch-to-buffer-other-window
   (cond
    ((bound-and-true-p pm/current)
     (fancy-edit-indirect--polymode)))))

(defun fancy-edit-indirect--polymode-determine-indent (span)
  "Determine the given polymode span's indentation level.

SPAN should be a polymode span as returned by `pm-innermost-span'."
  (let ((start (cadr span))
        (end (caddr span))
        (body-indent-offset (eieio-oref (cadddr span) :body-indent-offset))
        indent)

    (save-excursion
      (goto-char start)

      ;; We're at the head; either skip it if it's empty, or calculate
      ;; the post-head indent if not.
      (unless (looking-at "[[:space:]]*$")
        (re-search-forward "[[:space:]]*" (line-end-position) t 1)
        (setq indent (- (match-end 0) (match-beginning 0))))

      (forward-line)
      (while (< (point) end)
        (back-to-indentation)
        (cond
         ;; If we're at the tail, skip the line, *unless* we don't
         ;; have an indent set yet, at which point take the indent +
         ;; the body offset.
         ((eq 'tail (car (pm-innermost-span)))
          (unless (eq indent nil)
            (setq indent (+ (current-indentation) body-indent-offset))))

         ;; We ignore empty lines
         ((and
           (looking-at "[[:space:]]*$")
           (looking-back "^[[:space:]]*")))

         ;; Finally, take the indentation of the line with the
         ;; least indentation.
         ((or
           (eq indent nil)
           (< (current-indentation) indent))
          (setq indent (current-indentation))))
        (forward-line)))
    indent))

(defun fancy-edit-indirect--polymode ()
  "Edit the current polymode block in an indirect buffer."
  (let* ((span (pm-innermost-span))
         (filename (buffer-file-name))
         (head-empty (save-excursion
                       (goto-char (cadr span))
                       (when (looking-at "[[:space:]]*$")
                         (match-string 0))))
         (tail-empty (save-excursion
                       (goto-char (caddr span))
                       (when (looking-back "^[[:space:]]*")
                         (match-string 0))))
         (mode (eieio-oref (cadddr span) :mode))
         (indent (fancy-edit-indirect--polymode-determine-indent span)))

    (setq-local
     edit-indirect-after-creation-hook
     (lambda ()
       (save-excursion
         (let ((inhibit-read-only t))
           ;; If the tail is completely empty, we delete it, and
           ;; add it back later
           (when tail-empty
             (goto-char (buffer-end 1))
             (delete-line))

           (goto-char (buffer-end -1))
           ;; Ditto for the head
           (when head-empty
             (delete-line))

           (while (< (point) (buffer-end 1))
             (unless (looking-at "[[:space:]]*$")
               (delete-region (line-beginning-position) (+ (line-beginning-position) indent)))
             (forward-line))))
       (setq-local buffer-file-name (format "%s.-ei-" filename))
       (funcall mode))

     edit-indirect-after-commit-functions
     (lambda (start end)
       (save-excursion
         ;; We work from the bottom up, since editing the buffer will
         ;; move the end
         (goto-char end)

         (when tail-empty
           (insert tail-empty)
           (forward-line -1))

         (while (> (point) start)
           (unless (looking-at "[[:space:]]*$")
             (indent-to indent))
           (forward-line -1))

         (goto-char start)

         (when head-empty
           (insert head-empty)
           (insert "\n")
           (indent-to indent)))))

    (edit-indirect-region (cadr span) (caddr span))))

(provide 'fancy-edit-indirect)

;;; fancy-edit-indirect.el ends here
