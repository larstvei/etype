;;; etype.el --- A typing game for Emacs

;; Copyright (C) 2014 Lars Tveito.

;; Author: Lars Tveito <larstvei@ifi.uio.no>

;; Contains code from GNU Emacs <https://www.gnu.org/software/emacs/>,
;; released under the GNU General Public License version 3 or later.

;; Etype is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; Etype is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along
;; with Etype.  If not, see <http://www.gnu.org/licenses/>.

(eval-when-compile
  (require 'cl))

(defcustom etype-lines-file
  (concat
   (if load-file-name
       (file-name-directory load-file-name)
     default-directory) "etype.lines")
  "A path to a file named 'etype.lines' containing one word per
line that will be used in the game."
  :group 'etype)

(defvar etype-words-in-play nil)

(defvar etype-unused-words nil)

(defvar etype-score 0)

(defvar etype-in-game nil)

(defvar etype-timers nil)

(defvar etype-overlay nil)

(defvar etype-point-max nil)

(defvar etype-completing-word nil)

(defvar etype-level 1)

(defvar etype-game-is-over nil)

(defvar etype-game-over-string
  '("_____ ____  _      _____   ____  _     _____ ____"
    "/  __//  _ \\/ \\__/|/  __/  /  _ \\/ \\ |\\/  __//  __\\"
    "| |  _| / \\|| |\\/|||  \\    | / \\|| | //|  \\  |  \\/|"
    "| |_//| |-||| |  |||  /_   | \\_/|| \\// |  /_ |    /"
    "\\____\\\\_/ \\|\\_/  \\|\\____\\  \\____/\\__/  \\____\\\\_/\\_\\"))

(defvar etype-new-game-string
  '("| '_ \\ / _ \\ \\ /\\ / /"
    "| | | |  __/\\ V  V / "
    "|_| |_|\\___| \\_/\\_/  "))

(defun etype-read-file ()
  "Returns a vector of lines from the 'etype-lines-file'."
  (with-temp-buffer
    (insert-file-contents etype-lines-file)
    (apply
     'vector
     (split-string
      (buffer-substring-no-properties (point-min) (point-max)) "\n"))))

(defun etype-menu (&optional game-over-p)
  ;; Menu stack
  (let* ((margin 2)
         (game-over-line (+ margin margin))
         (new-game-line (+ game-over-line
                           (length etype-game-over-string)
                           margin)))
    (when game-over-p
      (etype-draw etype-game-over-string game-over-line))
    (etype-draw etype-new-game-string new-game-line " new ")))

(defun etype-init-game ()
  "Sets up the game grid containing 'fill-column' number of spaces and 30
lines. Also some variables are set."
  (let ((inhibit-read-only t)
        (space (make-string fill-column ? )))
    (delete-region (point-min) (point-max))
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
  (setq cursor-type nil
        etype-score 0
        etype-level 1
        etype-in-game t
        etype-game-is-over nil
        etype-words-in-play nil)
  (unless etype-unused-words
    (setq etype-unused-words (etype-read-file)))
  (etype-spawn-wave 0 (etype-wave-limit)))

(defun etype-insert-centered (str)
  (let* ((len (length str))
         (beg (+ (point) (- (/ fill-column 2) (/ len 2)))))
    (goto-char beg)
    (delete-char len)
    (insert str)))

;; (defun etype-draw-game-over ()
;;   (save-excursion
;;     (let ((inhibit-read-only t))
;;       (loop for str in etype-game-over-string
;;             for i from 4 do
;;             (goto-line i)
;;             (etype-insert-centered str)
;;             finally
;;             (forward-line)
;;             (etype-insert-centered "Type > game over < to start new game."))
;;       (push "game" etype-words-in-play)
;;       (push "over" etype-words-in-play))))

(defun etype-draw (strings line &optional word)
  (save-excursion
    (let ((inhibit-read-only t))
      (loop for str in strings
            for i from line do
            (goto-line i)
            (etype-insert-centered str)
            finally
            (forward-line)
            (when word
              (etype-insert-centered word)
              (push (replace-regexp-in-string " "  "" word)
                    etype-words-in-play))))))

(defun etype-game-over ()
  (etype-cleanup)
  (setq etype-game-is-over t
        etype-completing-word nil)
  (etype-menu t))

(defun etype-increase-level ()
  "Increases the level."
  (interactive)
  (when (< etype-level 10)
    (let ((inhibit-read-only t))
      (incf etype-level)
      (etype-update-level))))

(defun etype-decrease-level ()
  "Decreases the level."
  (interactive)
  (when (> etype-level 1)
    (let ((inhibit-read-only t))
      (decf etype-level)
      (etype-update-level))))

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

(defun etype-next-line ()
  (let ((column (current-column)))
    (forward-line)
    (forward-char column)))

(defun etype-previous-line ()
  (let ((column (current-column)))
    (forward-line -1)
    (forward-char column)))

(defun etype-move-word (point word)
  "Move WORD at POINT to the next line. If there is not enough space on the
next line the word will not move."
  (when (and etype-in-game (not etype-game-is-over))
    (let ((inhibit-read-only t))
      (when etype-completing-word
        (goto-char etype-completing-word))
      (let ((moving-word-at-point (string= word (current-word t)))
            (search-string (buffer-substring-no-properties point (point))))
        (save-excursion
          (goto-char point)
          (unless (looking-at word)
            (goto-char (point-min))
            (search-forward word etype-point-max)
            (backward-word))
          ;; The point is now in front of the word that is to be moved.
          (let ((point (point))
                (timer (etype-search-timers word)))
            (etype-next-line)
            (let ((destination (etype-insert-word (point) word)))
              (cond (destination
                     (etype-remove-word point word)
                     (setf (timer--args timer) (list destination word)))
                    ((< (- etype-point-max fill-column 2) point)
                     (etype-game-over))))))
        ;; If we are moving the word at point the overlay must be moved and
        ;; the point needs to be updated.
        (when moving-word-at-point
          (search-forward-regexp (concat "\\<" search-string))
          (setq etype-completing-word (point))
          (save-excursion
            (let ((point (point)))
              (backward-word)
              (move-overlay etype-overlay (point) point))))))))

(defun etype-random ()
  "Returns a random float between 1 and 10, depending on the
level."
  (let* ((random (abs (random))))
    (/ random (expt 10.0 (floor (log random 10))))))

(defun etype-get-word (&optional count)
  "Tries to find a word in ETYPE-UNUSED-WORDS that has a
different capital letter from all words in
ETYPE-WORDS-IN-PLAY. It does not try very hard, and gives up
after checking 5 words - this is done to give a natural slow down
when there are a lot of words in play."
  (let ((word (elt etype-unused-words
                   (random (length etype-unused-words)))))
    (if (null (member
               (string-to-char word)
               (mapcar 'string-to-char etype-words-in-play)))
        word
      (unless (and count (> count 5))
        (etype-get-word (if count (+ count 1) 1))))))

(defun etype-spawn-word ()
  "This function spawns a word. It does this by finding a word
and inserting it at a random point in the game buffer. A timer
object is added to ETYPE-TIMERS, invoking ETYPE-MOVE-WORD at a
random time between 1 and 10 seconds."
  (save-excursion
    (let* ((word (etype-get-word))
           (point (1+ (random (- fill-column (1+ (length word))))))
           (random (- (etype-random) (/ etype-level 15.0))))
      (when (and word (etype-insert-word point word))
        (push word etype-words-in-play)
        (push (run-at-time random random 'etype-move-word (point) word)
              etype-timers)))))

(defun etype-spawn-wave (recur limit)
  "Spawn multiple words according to ETYPE-LEVEL."
  (save-excursion
    (let ((inhibit-read-only t))
      (when (< recur limit)
        (etype-spawn-word)
        (run-at-time (/ (etype-random) (ceiling (/ (* etype-level etype-level) 2.0))) nil
                     'etype-spawn-wave (1+ recur) limit)))))

(defun etype-wave-limit ()
  "Generates the number of words that should be spawn according to the level
and current score."
  (+ (* etype-level etype-level) 5
     (if (plusp etype-score) (ceiling (log etype-score)) 0)))

(defun etype-move-shooter (column)
  "Moves the shooter to COLUMN."
  (save-excursion
    (goto-char (point-max))
    (forward-line -1)
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
    (let* ((inhibit-read-only t)
           (bullet-dest (+ (- etype-point-max
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
  (search-forward-regexp
   (concat
    "\\<" (single-key-description last-input-event))
   etype-point-max t)
  (let ((word (word-at-point)))
    (when (and word
               (member (substring-no-properties word)
                       etype-words-in-play))
      (setq etype-completing-word (point))))
  (when etype-completing-word
    (let ((inhibit-read-only t))
      (etype-shoot)
      (setq etype-overlay
            (make-overlay (- etype-completing-word 1) etype-completing-word))
      (overlay-put etype-overlay 'face '(:inherit isearch)))))

(defun etype-continue-word (key-typed)
  "Moves the point forward if the typed key is the char in front of the
point. If the word is complete the word is cleared."
  (goto-char etype-completing-word)
  (let ((inhibit-read-only t))
    (cond ((looking-at key-typed)
           (forward-char)
           (setq etype-completing-word (point))
           (move-overlay etype-overlay (overlay-start etype-overlay) (point))
           (etype-shoot)
           (when (looking-at " ")
             (etype-clear-word)
             (setq etype-completing-word nil)))
          ((not etype-game-is-over) (etype-spawn-wave 0 etype-level)))))

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
  "Removes a word from the game, then updates the score."
  (let* ((inhibit-read-only t)
         (word (current-word t))
         (timer (etype-search-timers (current-word t))))
    (when timer
      (cancel-timer timer)
      (setq etype-timers (remove timer etype-timers)))
    (delete-overlay etype-overlay)
    (etype-move-shooter (/ fill-column 2))
    (backward-word)
    (etype-remove-word (point) word)
    (setq etype-words-in-play
          (remove word etype-words-in-play))
    (unless etype-game-is-over
      (etype-update-score word))    
    (cond ((and etype-game-is-over (string= word "new"))
           (etype-init-game))
          ((and (not etype-game-is-over)
                (remove-if (lambda (x) (string= x "")) etype-words-in-play))
           (etype-spawn-wave 0 (etype-wave-limit))))
    (goto-char (point-min))
    ;; (unless etype-game-is-over
    ;;   (etype-update-score word))
    ;; (unless (remove-if (lambda (x) (string= x ""))
    ;;                    etype-words-in-play)
    ;;   (etype-spawn-wave 0 (etype-wave-limit)))
    ;; (when (and etype-game-is-over (string= word "over"))
    ;;   (etype))
    ))

(defun etype-catch-input ()
  "'self-insert-command' is remapped to this function. Instead of
inserting the typed key, it triggers a shot."
  (interactive)
  (let ((key-typed (single-key-description last-input-event)))
    (if etype-completing-word
        (etype-continue-word key-typed)
      (etype-search-word key-typed))))

(defun etype-cleanup ()
  "Cancels all etype-timers."
  (mapc 'cancel-timer etype-timers))

(defun etype ()
  "Starts a game of Etype."
  (interactive)
  (switch-to-buffer "Etype")
  (etype-mode)
  (etype-init-game))

(define-derived-mode etype-mode nil "Etype"
  "A mode for playing Etype."
  (dolist (var '(etype-score
                 etype-level
                 etype-timers
                 etype-overlay
                 etype-in-game
                 etype-point-max
                 etype-unused-words
                 etype-words-in-play
                 etype-game-is-over
                 etype-completing-word
                 etype-game-is-over))
    (make-local-variable var))

  (read-only-mode 1)
  (define-key (current-local-map)
    [remap self-insert-command] 'etype-catch-input)

  (local-set-key (kbd "<up>")   'etype-increase-level)

  (add-hook 'kill-buffer-hook 'etype-cleanup))

(provide 'etype)
