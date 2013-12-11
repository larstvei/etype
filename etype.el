(require 'cl)

(defvar etype-words-in-play nil)

(defvar etype-unused-words nil)

(defvar etype-score 0)

(defvar etype-in-game nil)

(defvar etype-timers nil)

(defvar etype-overlay nil)

(defvar etype-point-max nil)

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
    (setq etype-point-max (point))
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
           (unless (or (looking-back " ") (bolp))
             (forward-char))
           (point))
          ((search-forward space (point-at-eol) t)
           (- (point) (- (length space) 1))))))

(defun etype-search-timers (word)
  (first
   (remove-if-not
    (lambda (timer)
      (member word (timer--args timer))) etype-timers)))

(defun etype-move-word (point word)
  (when etype-in-game
    (let ((moving-word-at-point (string= word (current-word t)))
          (search-string (buffer-substring-no-properties point (point))))
      (save-excursion
        (goto-char point)
        (unless (looking-at word)
          (beginning-of-buffer)
          (search-forward word etype-point-max)
          (backward-word))
        (let ((point (point))
              (timer (etype-search-timers word)))
          (next-line)
          (let* ((len (length word))
                 (space (make-string len ? ))
                 (destination (etype-fit-word word)))
            (when destination
              (goto-char point)
              (delete-char len)
              (insert space)
              (goto-char destination)
              (delete-char len)
              (insert word)
              (setf (timer--args timer) (list destination word))))))
      (when moving-word-at-point
        (search-forward-regexp (concat "\\<" search-string))
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
            (let ((random (etype-random)))
              (push (run-at-time
                     (concat (number-to-string random) " sec")
                     random 'etype-move-word point word) etype-timers))))))))

(defun etype-move-shooter (column)
  (save-excursion
    (end-of-buffer)
    (previous-line)
    (delete-region (point-at-bol) (point-at-eol))
    (insert (make-string (- fill-column 5) ?-))
    (beginning-of-line)
    (let* ((shooter " /.\\ ")
           (len (length shooter)))
      (cond ((and (> column 1) (< column (- fill-column (+ len 1))))
             (forward-char (- column 2)))
            ((> column (- fill-column (+ len 1)))
             (forward-char (- fill-column len))))
      (insert shooter))))

(defun etype-shoot (&optional steps)
  (let* ((bullet-dest (+ (- etype-point-max
                            (* (or steps 0) (+ fill-column 1)))
                         (current-column)))
         (overlay (make-overlay bullet-dest (+ bullet-dest 1))))
    (etype-move-shooter (current-column))
    (overlay-put overlay 'face (cons 'background-color "white"))
    (sit-for (* 0.000005 (or steps 0)))
    (delete-overlay overlay)
    (when (< (point) bullet-dest)
      (etype-shoot (+ (or steps 0) 1)))))

(defun etype-loop ()
  (push (run-at-time "0 sec" 2 'etype-spawn-word) etype-timers))

(defun etype-search-word (key-etyped)
  (setq etype-completing-word
        (search-forward-regexp
         (concat
          "\\<" (single-key-description last-input-event))
         etype-point-max t))
  (when etype-completing-word
    (etype-shoot)
    (setq etype-overlay
          (make-overlay (- etype-completing-word 1) etype-completing-word))
    (overlay-put etype-overlay 'face '(:inherit isearch))))

(defun etype-continue-word (key-typed)
  (when (looking-at key-typed) (forward-char)
        (move-overlay etype-overlay (overlay-start etype-overlay) (point))
        (etype-shoot)
        (when (looking-at " ")
          (etype-clear-word)
          (setq etype-completing-word nil))))

(defun etype-clear-word ()
  (delete-overlay etype-overlay)
  (etype-move-shooter (/ fill-column 2))
  (let* ((word (current-word t))
         (len (length word))
         (space (make-string len ? )))
    (backward-word)
    (let ((timer (etype-search-timers word)))
      (cancel-timer timer)
      (setq etype-timers (remove timer etype-timers)))
    (setq etype-words-in-play
          (remove word etype-words-in-play))
    (add-to-list 'etype-unused-words word t)
    (delete-char len)
    (insert space)
    (incf etype-score (* len 1.5)))
  (search-forward "Score: ")
  (delete-char (- (point-max) (point)))
  (insert (number-to-string etype-score))
  (goto-char (point-min)))

(defun etype-catch-input ()
  (interactive)
  (let ((key-typed (single-key-description last-input-event)))
    (if etype-completing-word
        (etype-continue-word key-typed)
      (etype-search-word key-typed))))

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
  (make-local-variable 'etype-point-max)
  (make-local-variable 'etype-unused-words)
  (make-local-variable 'etype-words-in-play)
  (make-local-variable 'etype-completing-word)
  (define-key (current-local-map)
    [remap self-insert-command] 'etype-catch-input)
  (add-hook 'kill-buffer-hook 'etype-cleanup))
