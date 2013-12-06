(require 'cl)

(defvar etype-words-in-play nil)

(defvar etype-unused-words nil)

(defvar etype-score 0)

(defvar etype-in-game nil)

(defvar etype-timers nil)

(defvar etype-overlay nil)

(defvar etype-completing-word nil)

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
    (insert (make-string fill-column ?-))
    (insert "\nScore: 0"))
  (goto-char (point-min))
  (setq etype-score 0)
  (setq etype-in-game t)
  ;; Shuffle the vector of etype-unused-words and turn it in to a list.

  (setq etype-unused-words (mapcar 'eval (shuffle-vector (etype-read-file)))))

(defun etype-fit-word (word)
  (let* ((space (make-string (+ 2 (length word)) ? )))
    (cond ((and (or (looking-back " ") (bolp)) (looking-at space))
           (point))
          ((search-backward space (point-at-bol) t)
           (message "%s: went backwards" word)
           (unless (or (looking-back " ") (bolp))
             (forward-char))
           (point))
          ((search-forward space (point-at-eol) t)
           (message "%s: went forwards" word)
           ;; (unless (looking-at space)
           ;;   (backward-char))
           (- (point) (- (length space) 1))))))

(defun etype-search-timers (point)
  (first
   (remove-if-not
    (lambda (timer)
      (let ((arg (timer--args timer)))
        (and (numberp (first arg))
             (= point (first arg))))) etype-timers)))

(defun etype-at-word-p (point)
  (save-excursion
    (let ((word-at-point (thing-at-point 'word)))
      (goto-char point)
      (equal word-at-point (thing-at-point 'word)))))

(defun etype-move-word (point)
  (when etype-in-game
    (let ((destination nil)
          (moving-word-at-point (etype-at-word-p point))
          (search-string (buffer-substring-no-properties point (point))))
      (save-excursion
        (goto-char point)
        (let* ((word (thing-at-point 'word))
               (len (length word))
               (timer (etype-search-timers point)))
          (next-line)
          (setq destination (etype-fit-word word))
          (when destination
            ;;              (message "word: %s moved to %d" word destination)
            (goto-char point)
            (delete-char len)
            (insert (make-string len ? ))
            (next-line)
            (backward-char len)
            (goto-char destination)
            (delete-char len)
            (insert word)
            (setf (timer--args timer) (list (- (point) len))))))
      (when (and destination moving-word-at-point)
        (search-forward search-string)
        (save-excursion
          (let ((point (point)))
            (beginning-of-thing 'word)
            (move-overlay etype-overlay (point) point)))))))

(defun etype-random ()
  (let ((random (abs (random))))
    (/ random (expt 10.0 (floor (log random 10))))))

(defun etype-get-word (&optional count)
  (let ((word (pop etype-unused-words)))
    (if (null (member
               (string-to-char word)
               (mapcar 'string-to-char etype-words-in-play)))
        word
      (add-to-list 'etype-unused-words word t)
      (unless (and count (> count 5))
        (etype-get-word (if count (+ count 1) 1))))))

(defun etype-spawn-word ()
  (save-excursion
    (when etype-in-game
      (let* ((word (etype-get-word))
             (point (random (- fill-column (length word)))))
        (when word
          (goto-char point)
          (setq point (etype-fit-word word))
          (when point
            (delete-char (length word))
            (insert word)
            (push word etype-words-in-play)
            (push (run-at-time
                   (concat (number-to-string (floor (etype-random))) " sec")
                   (etype-random) 'etype-move-word point) etype-timers)
            (message "Spawned word %s moves every %f second" word random)))))))

(defun etype-loop ()
  (push (run-at-time "0 sec" 2 'etype-spawn-word) etype-timers))

(defun etype-search-word (key-etyped)
  (setq etype-completing-word
        (search-forward-regexp
         (concat
          "\\<" (single-key-description last-input-event)) nil t))
  (when etype-completing-word
    (setq etype-overlay
          (make-overlay (- etype-completing-word 1) etype-completing-word))
    (overlay-put etype-overlay 'face '(:inherit isearch))))

(defun etype-continue-word (key-etyped)
  (when (looking-at key-etyped) (forward-char))
  (move-overlay etype-overlay (overlay-start etype-overlay) (point))
  (when (looking-at " ")
    (etype-clear-word)
    (setq etype-completing-word nil)))

(defun etype-clear-word ()
  (delete-overlay etype-overlay)
  (let* ((word (thing-at-point 'word))
         (len (length word))
         (space (make-string len ? )))
    (setq etype-words-in-play
          (remove word etype-words-in-play))
    (add-to-list 'etype-unused-words word t)
    (backward-word)
    (delete-char len)
    (insert space)
    (incf etype-score (* len 1.5)))
  (search-forward "Score: ")
  (delete-char (- (point-max) (point)))
  (insert (number-to-string etype-score))
  (goto-char (point-min)))

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
  (make-local-variable 'etype-score)
  (make-local-variable 'etype-timers)
  (make-local-variable 'etype-overlay)
  (make-local-variable 'etype-in-game)
  (make-local-variable 'etype-unused-words)
  (make-local-variable 'etype-words-in-play)
  (make-local-variable 'etype-completing-word)
  (define-key (current-local-map)
    [remap self-insert-command] 'etype-catch-input)
  (add-hook 'kill-buffer-hook 'etype-cleanup))
