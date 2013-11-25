(require 'cl)

(defvar etype-score 0)

(defvar etype-in-game nil)

(defvar etype-completing-word nil)

(defvar etype-words nil)

(defvar etype-timers nil)

(defvar etype-banned-initials nil)

(defconst etype-lines-file "etype.lines")

(defun etype-read-file ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name etype-lines-file default-directory))
    (apply
     'vector
     (split-string
      (buffer-substring-no-properties (point-min) (point-max)) "\n"))))

(defun init-game ()
  (let ((space (make-string fill-column ? )))
    (dotimes (i 30)
      (insert space)
      (newline))
    (goto-char (point-min))
    (setq etype-in-game t)
    ;; Shuffle the vector of etype-words and turn it in to a list.
    (setq etype-words (mapcar 'eval (shuffle-vector (etype-read-file))))))

(defun etype-fit-word (word)
  (let ((space (make-string (+ 1 (length word)) ? )))
    (or (and (looking-at space) (point))
        (search-backward space nil nil)
        (search-forward space nil nil))))

(defun etype-playing-p ()
  (string= "Etype" (buffer-name (current-buffer))))

(defun etype-search-timers (point)
  (first
   (remove-if-not
    (lambda (timer)
      (let ((arg (timer--args timer)))
        (and (numberp (first arg))
             (= point (first arg))))) etype-timers)))

(defun etype-move-word (point)
  (message "score: %f" etype-score)
  (save-excursion
    (when (etype-playing-p)
      (goto-char point)
      (let* ((word (thing-at-point 'word))
             (len (length word))
             (timer (etype-search-timers point)))
        (delete-char len)
        (insert (make-string len ? ))
        (forward-char (- (+ 1 fill-column) len))
        (let ((point (etype-fit-word word)))
          (goto-char point)
          (delete-char len)
          (insert word)
          (setf (timer--args timer) (list (- (point) len))))))))

(defun etype-spawn-word ()
  (save-excursion
    (when (etype-playing-p)
      (let* ((word (pop etype-words))
             (point (random (- fill-column (length word)))))
        (goto-char point)
        (setq point (etype-fit-word word))
        (when point
          (delete-char (length word))
          (insert word)
          (push (run-at-time "5 sec" (+ 1 (random 4))
                             'etype-move-word point) etype-timers))))))

(defun etype-loop ()
  ;; (push (run-at-time "5 sec" 3 'etype-drop) etype-timers)
  (push (run-at-time "0 sec" 2 'etype-spawn-word) etype-timers))

(defun etype-search-word (key-etyped)
  (setq etype-completing-word
        (search-forward
         (concat " " (single-key-description last-input-event)) nil t)))

(defun etype-clear-word ()
  (let* ((len (length (thing-at-point 'word)))
         (space (make-string len ? )))
    (backward-word)
    (delete-char len)
    (insert space)
    (goto-char (point-min))
    (incf etype-score (* len 1.5))))

(defun etype-continue-word (key-etyped)
  (when (looking-at key-etyped) (forward-char))
  (when (looking-at " ")
    (etype-clear-word)
    (setq visible-cursor)
    (setq etype-completing-word nil)))

(defun etype-catch-input ()
  (interactive)
  (let ((key-etyped (single-key-description last-input-event)))
    (if etype-completing-word
        (etype-continue-word key-etyped)
      (etype-search-word key-etyped))))

(defun etype ()
  (interactive)
  (switch-to-buffer "Etype")
  (etype-mode)
  (init-game)
  (etype-loop))

(defun etype-cleanup ()
  (mapc 'cancel-timer etype-timers))

(define-derived-mode etype-mode nil "Etype"
  "A mode for playing Etype."
  (make-local-variable 'etype-words)
  (make-local-variable 'etype-timers)
  (make-local-variable 'etype-in-game)
  (make-local-variable 'etype-completing-word)
  (define-key (current-local-map) [remap self-insert-command] 'etype-catch-input)
  (add-hook 'kill-buffer-hook 'etype-cleanup))
