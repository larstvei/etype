(load-library "word-list")

(defvar type-timers nil)

(defvar banned-initials nil)

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
    (when (type-fit-word word)
      (delete-char bytes-to-delete)
      (insert word))))

(defun type-fit-word (word)
  (let ((space (make-string (length word) ? )))
    (or (looking-at (concat space "|[^ ]+$"))
        (search-backward space nil nil)
        (search-forward space nil nil))))

(defun type-spawn-word ()
  (save-excursion
    (when (string= "Type" (buffer-name (current-buffer)))
      (let* ((word (pop words))
             (point (random (- fill-column (length word)))))
        (type-insert-overwrite point (concat " " word " "))))))

(defun type-open-line ()
  (save-excursion
    (goto-char 1)
    (open-line 1)
    (insert (make-string fill-column ? ))))

(defun type-loop ()
  (push (run-at-time "5 sec" 3 'type-open-line) type-timers)
  (push (run-at-time "0 sec" 1.5 'type-spawn-word) type-timers))

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
