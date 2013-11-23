(load-library "word-list")

(defvar type-timers nil)

(defun init-game ()

  ;; Shuffle the vector of words and turn it in to a list.
  (setq words (mapcar 'eval (shuffle-vector words)))
  (type-loop))

(defun type-goto-char (pos)
  (if (< pos (point-max))
      (goto-char pos)
    (goto-char (point-max))
    (insert (make-string (- pos (point-max)) ? ))))

(defun spawn-word ()
  (when (string= "Type" (buffer-name (current-buffer)))
   (let ((word (pop words)))
     (type-goto-char (random (- fill-column (length word))))
     (delete-char (length word))
     (insert word))))

(defun type-loop ()
  (push (run-at-time "0 sec" 2 'spawn-word) type-timers))

(defun type ()
  (interactive)
  (switch-to-buffer "Type")
  (type-mode)
  (init-game))

(defun type-cleanup ()
  (mapc 'cancel-timer type-timers))

(define-derived-mode type-mode nil "Type"
  "A mode for playing Type."
  (make-local-variable 'words)
  (make-local-variable 'type-timers)
  (overwrite-mode)
  (add-hook 'kill-buffer-hook 'type-cleanup))
