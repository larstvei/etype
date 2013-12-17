(require 'cl)

(defvar etype-words-in-play nil)

(defvar etype-unused-words nil)

(defvar etype-score 0)

(defvar etype-in-game nil)

(defvar etype-timers nil)

(defvar etype-overlay nil)

(defvar etype-point-max nil)

(defvar etype-completing-word nil)

(defvar etype-level 1)

(defconst etype-lines-file "etype.lines")

(defun etype-read-file ()
  "Returns a vector of lines from the 'etype-lines-file'."
  (with-temp-buffer
    (insert-file-contents (expand-file-name etype-lines-file default-directory))
    (apply
     'vector
     (split-string
      (buffer-substring-no-properties (point-min) (point-max)) "\n"))))

(defun init-game ()
  "Sets up the game grid containing 'fill-column' number of spaces and 30
lines. Also some variables are set."
  (let ((space (make-string fill-column ? )))
    (dotimes (i 30)
      (insert space)
      (newline))
    (setq etype-point-max (point))
    (let* ((score "\nScore: 0")
           (level "Level: 1"))
      (insert (make-string fill-column ?-))
      (insert score)
      (insert (make-string
               (- fill-column
                  (+ (length score) (length level))) ? ))
      (insert level)))
  (goto-char (point-min))
  (setq cursor-type nil)
  (setq etype-score 0)
  (setq etype-in-game t)
  ;; Shuffle the vector returned from etype-read-file, and turns it in to a
  ;; list.
  (setq etype-unused-words (mapcar 'eval (shuffle-vector (etype-read-file)))))

(defun etype-increase-level ()
  "Increases the level."
  (interactive)
  (when (< etype-level 10)
    (incf etype-level)
    (etype-update-level)))

(defun etype-decrease-level ()
  "Decreases the level."
  (interactive)
  (when (> etype-level 1)
    (decf etype-level)
    (etype-update-level)))

(defun etype-fit-word (word)
  "Returns a point that a word can be inserted on the next
line. If there is no room (a word is directly beneath it) it
tries to find the nearest point it could fit. If there is no room
NIL is returned, and the word is not moved."
  (let* ((point (point))
         (space (make-string (+ 2 (length word)) ? )))
    (if (and (or (looking-back " ") (bolp)) (looking-at space))
        point
      (let* ((backward (search-backward space (point-at-bol) t))
             (backward (and backward (+ backward 1))))
        (goto-char point)
        (let* ((forward (search-forward space (point-at-eol) t))
               (forward (and forward (- forward (- (length space) 1)))))
          (cond ((not backward) forward)
                ((not forward) backward)
                ((< (- point backward) (- forward point)) backward)
                (t forward)))))))

(defun etype-search-timers (word)
  "Returns the timer that is associated with WORD."
  (first
   (remove-if-not
    (lambda (timer)
      (member word (timer--args timer))) etype-timers)))

(defun etype-remove-word (point word)
  "Removes a word and replacing it with whitespace."
  (let* ((len (length word))
         (space (make-string len ? )))
    (goto-char point)
    (when (looking-at word)
      (delete-char len)
      (insert space))))

(defun etype-insert-word (point word)
  "Inserts word 'overwrite-mode' style, but only if the word fits on the
line."
  (goto-char point)
  (let* ((destination (etype-fit-word word)))
    (when destination
      (goto-char destination)
      (delete-char (length word))
      (insert word)
      (goto-char destination))))

(defun etype-move-word (point word)
  "Move WORD at POINT to the next line. If there is not enough space on the
next line the word will not move."
  (when etype-in-game
    (let ((moving-word-at-point (string= word (current-word t)))
          (search-string (buffer-substring-no-properties point (point))))
      (save-excursion
        (goto-char point)
        (unless (looking-at word)
          (beginning-of-buffer)
          (search-forward word etype-point-max)
          (backward-word))
        ;; The point is now in front of the word that is to be moved.
        (let ((point (point))
              (timer (etype-search-timers word)))
          (next-line)
          (let ((destination (etype-insert-word (point) word)))
            (when destination
              (etype-remove-word point word)
              (setf (timer--args timer) (list destination word))))))
      ;; If we are moving the word at point the overlay must be moved and
      ;; the point needs to be updated.
      (when moving-word-at-point
        (search-forward-regexp (concat "\\<" search-string))
        (save-excursion
          (let ((point (point)))
            (backward-word)
            (move-overlay etype-overlay (point) point)))))))

(defun etype-random ()
  "Returns a random float, depending on the level."
  (let ((random (abs (random))))
    (/ random (expt 10.0 (floor (log random 10))))))

(defun etype-get-word (&optional count)
  "Tries to find a word in ETYPE-UNUSED-WORDS that has a
different capital letter from all words in
ETYPE-WORDS-IN-PLAY. It does not try very hard, and gives up
after checking 5 words - this is done to give a natural slow down
when there are a lot of words in play."
  (let ((word (pop etype-unused-words)))
    (if (null (member
               (string-to-char word)
               (mapcar 'string-to-char etype-words-in-play)))
        word
      (add-to-list 'etype-unused-words word t)
      (unless (and count (> count 5))
        (etype-get-word (if count (+ count 1) 1))))))

(defun etype-spawn-word ()
  "This function spawns a word in the game. It does this by
finding a word and inserting it where it fits. It also updates
the timer which is associated with this function, setting it to a
new random time."
  (save-excursion
    (when etype-in-game
      (let* ((word (etype-get-word))
             (point (random (- fill-column (length word))))
             (random (/ (etype-random) etype-level)))
        (when (and word (etype-insert-word point word))
          (push word etype-words-in-play)
          (push (run-at-time random random 'etype-move-word (point) word)
                etype-timers)))))
  (setf (timer--repeat-delay (last etype-timers)) (/ (etype-random) etype-level)))

(defun etype-move-shooter (column)
  "Moves the shooter to COLUMN."
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
            ((> column (- fill-column (+ len 2)))
             (forward-char (- fill-column len))))
      (insert shooter))))

(defun etype-shoot (&optional steps)
  "Triggers the shooter to fire at a word. It calls itself
recursively until the bullet hits the word."
  (unless (= 0 (current-column))
    (let* ((bullet-dest (+ (- etype-point-max
                              (* (or steps 0) (+ fill-column 1)))
                           (current-column)))
           (overlay (make-overlay bullet-dest (+ bullet-dest 1)))
           (time (* 0.00005 (or steps 0))))
      (etype-move-shooter (current-column))
      (overlay-put overlay 'display "|")
      (run-at-time (+ time 0.05) nil 'delete-overlay overlay)
      (when (< (point) (- bullet-dest (+ fill-column 1)))
        (run-at-time time nil 'etype-shoot (+ (or steps 0) 1))))))

(defun etype-search-word (key-etyped)
  "Searches the buffer for a word that begins with the typed
key. If a word is found a shot is fired at it, and a overlay is
created."
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
  "Moves the point forward if the typed key is the char in front of the
point. If the word is complete the word is cleared."
  (when (looking-at key-typed) (forward-char)
        (move-overlay etype-overlay (overlay-start etype-overlay) (point))
        (etype-shoot)
        (when (looking-at " ")
          (etype-clear-word)
          (setq etype-completing-word nil))))

(defun etype-update-score (word)
  "Updates the score."
  (save-excursion
    (incf etype-score (* (length word) etype-level))
    (re-search-forward "Score: [0-9]+")
    (replace-match (concat "Score: " (number-to-string etype-score)))))

(defun etype-update-level ()
  "Updates the level."
  (save-excursion
    (re-search-forward "Level: [0-9]+")
    (replace-match (concat "Level: " (number-to-string etype-level)))))

(defun etype-clear-word ()
  "Removes a word from the game, and updating score."
  (let* ((word (current-word t))
         (timer (etype-search-timers (current-word t))))
    (cancel-timer timer)
    (setq etype-timers (remove timer etype-timers))
    (delete-overlay etype-overlay)
    (etype-move-shooter (/ fill-column 2))
    (backward-word)
    (etype-remove-word (point) word)
    (setq etype-words-in-play
          (remove word etype-words-in-play))
    (add-to-list 'etype-unused-words word t)
    (etype-update-score word)
    (goto-char (point-min))))

(defun etype-catch-input ()
  "'self-insert-command' is remapped to this function. Instead of
inserting the typed key, it triggers a shot."
  (interactive)
  (let ((key-typed (single-key-description last-input-event)))
    (if etype-completing-word
        (etype-continue-word key-typed)
      (etype-search-word key-typed))))

(defun etype ()
  "Starts a game of Etype."
  (interactive)
  (switch-to-buffer "Etype")
  (etype-mode)
  (init-game)
  (push (run-at-time 0 (etype-random) 'etype-spawn-word) etype-timers))

(defun etype-cleanup ()
  "Cancels all etype-timers."
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
  (make-local-variable 'etype-level)

  (define-key (current-local-map)
    [remap self-insert-command] 'etype-catch-input)

  (local-set-key (kbd "<up>")   'etype-increase-level)
  (local-set-key (kbd "<down>") 'etype-decrease-level)

  (add-hook 'kill-buffer-hook 'etype-cleanup))
