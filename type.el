(load-library "word-list")

(defvar type-timers nil)

(defun init-game ()
  ;; Shuffle the vector of words and turn it in to a list.
  (setq words (mapcar 'eval (shuffle-vector words)))
  (type-loop))

(defun type-goto-char (point)
  (if (< point (point-max))
      (goto-char point)
    (goto-char (point-max))
    (insert (make-string (- point (point-max)) ? ))))

(defun type-insert-overwrite (point word)
  (type-goto-char point)
  (let* ((len (length word))
         (bytes-to-delete
          (if (< (point-max) (+ point len))
              (- len (- (+ point len) (point-max)))
            len)))
    (delete-char bytes-to-delete)
    (insert word)))

(defun type-spawn-word ()
  (when (string= "Type" (buffer-name (current-buffer)))
    (let* ((word (pop words))
           (point (random (- fill-column (length word)))))
      (message "word: %s, point: %d" word point)
      (type-insert-overwrite point word))))

(defun type-loop ()
  (push (run-at-time "0 sec" 2 'type-spawn-word) type-timers))

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
