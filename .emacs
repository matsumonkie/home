(package-initialize)

(defvar load-my-conf t)

;; * Reset Keymaps

(message "\n -- resetting keymaps --\n")

;; ** CTRL

;; do not unset "\C-m" otherwise enter key does not work in console mode
(dolist (key '(
"\C-a" "\C-b" "\C-d" "\C-e" "\C-f" "\C-g"
"\C-j" "\C-k" "\C-l" "\C-n" "\C-o" "\C-p"
"\C-q" "\C-r" "\C-s" "\C-t" "\C-u" "\C-v"
"\C-w" "\C-x" "\C-y" "\C-z" "\C-SPC"))
(global-unset-key key))

;; ** META

(dolist (key '(
  "\M-a" "\M-b" "\M-c" "\M-d" "\M-e" "\M-f"
  "\M-g" "\M-h" "\M-j" "\M-k" "\M-l" "\M-m"
  "\M-n" "\M-o" "\M-p" "\M-q" "\M-r" "\M-s"
  "\M-t" "\M-u" "\M-v" "\M-w" "\M-x" "\M-y"
  "\M-z" "\M-SPC"))
(global-unset-key key))

(global-unset-key "\C-e\C-n")

;; * Alias

(message "\n -- setting alias --\n")

;; ** Outline

(defalias 'onh 'outline-next-visible-heading)
(defalias 'oph 'outline-previous-visible-heading)
(defalias 'ouh 'outline-up-heading)
(defalias 'ose 'outline-show-entry)
(defalias 'ohe 'outline-hide-entry)
(defalias 'ohs 'outline-hide-subtree)
(defalias 'oss 'outline-show-subtree)
(defalias 'ohl 'outline-hide-leaves)
(defalias 'osb 'outline-show-branches)
(defalias 'osc 'outline-show-children)
(defalias 'ohb 'outline-hide-body)
(defalias 'osa 'outline-show-all)
(defalias 'oho 'outline-hide-other)

;; Replace
(defalias 'r 'query-replace)

;; Line mode
(defalias 'hl 'global-hl-line-mode)

;; Macro
(defalias 'sm 'start-kbd-macro)
(defalias 'em 'end-kbd-macro)
(defalias 'lm 'call-last-kbd-macro)

;; Rectangle
(defalias 'rm 'rectangle-mark-mode)

;; Upcase / downcase
(defalias 'ur 'upcase-region)
(defalias 'uw 'upcase-word)
(defalias 'dr 'downcase-region)
(defalias 'dw 'downcase-word)

(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'eb 'eval-buffer)

(defalias 'ar 'align-regexp)
;; Align with the symbol '='
(defalias 'ceq 'my-c-equal-align)

;; Information sur une commande
(defalias 'ap 'apropos)

;; buffer
(defalias 'mwb 'mark-whole-bufferi)
(defalias 'kb 'ido-kill-buffer)
(defalias 'reload 'revert-buffer)
(defalias 'ib 'indent-buffer)

;; Narrow
(defalias 'nr  'narrow-to-region)
(defalias 'wd  'widen)

;; Package
(defalias 'pl 'package-list-packages)

;; Multiple cursor
(defalias 'cml 'mc/edit-lines)
(defalias 'cmn 'mc/mark-next-like-this)
(defalias 'cmp 'mc/mark-previous-like-this)
(defalias 'cma 'mc/mark-all-like-this)

;; Magit
(defalias 'mm 'magit-mode)
(defalias 'gs 'magit-status)
(defalias 'gbm 'magit-blame-mode)

;; window
(defalias 'o 'other-window)
(defalias 'q 'delete-window)
(defalias 'f 'delete-other-windows)
(defalias 'eh 'enlarge-window-horizontally)
(defalias 'ev 'enlarge-window)
(defalias 'sh 'shrink-window-horizontally)
(defalias 'sv 'shrink-window)

;; copy/paste
(defalias 'cc 'copy-to-clipboard)
(defalias 'pc 'paste-from-clipboard)

;; visual line mode
(defalias 'vlm 'visual-line-mode)

;; artist mode
(defalias 'draw 'artist-select-operation)

;; vimish fold
(defalias 'fold 'vimish-fold)
(defalias 'unfold 'vimish-fold-unfold)

;; * Function

(message "\n -- setting functions --\n")

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun my-kill-buffer ()
  "kill current buffer without asking if it's the good one"
  (interactive)
  (kill-buffer (current-buffer)))

(defun my-backward-kill-line ()
  "Kill backward from point to beginning of line"
  (interactive) (kill-line 0))

(defun my-kill-line ()
  "Kill whole line if point is at the beginning of the line else only kill line"
  (interactive)
  (if (equal (point) (line-beginning-position))
      (kill-whole-line)
    (kill-line)))

(defun window-half-height ()
  (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun scroll-up-half ()
  (interactive)
  (scroll-up (window-half-height)))

(defun scroll-down-half ()
  (interactive)
  (scroll-down (window-half-height)))

(defun insert-comment ()
  "comment with /* */"
  (interactive)
  (if (region-active-p)
      (beginning-of-line)
    (insert "/*")
    (end-of-line)
    (insert "*/")))

(defun comment-line ()
  "Comments the current line"
  (interactive)
  (condition-case nil
      (comment-region (point-at-bol) (point-at-eol)) (error nil)))

(defun uncomment-line ()
  "Comments the current line and goes to the next one"
  (interactive)
  (condition-case nil
      (uncomment-region (point-at-bol) (point-at-eol)) (error nil)))

(defun my-copy-line ()
  "Copy current line in the kill ring"
  (interactive)
  (kill-ring-save (line-beginning-position)
		  (line-beginning-position 2))
  (message "Line copied"))

(defun scroll-up-lot ()
  (interactive)
  (forward-line -2))

(defun scroll-down-lot ()
  (interactive)
  (forward-line 2))

(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

(defun move-line-region-up (start end n)
  (interactive "r\np")
  (if (use-region-p)
      (move-region-up start end n)
    (move-line-up n)
    ))

(defun move-line-region-down (start end n)
  (interactive "r\np")
  (if (use-region-p)
      (move-region-down start end n)
    (move-line-down n)))

(defun my-horizontal-recenter ()
  "make the cursor horizontally centered in the window"
  (interactive)
  (move-to-column  (/ (- (line-end-position) (line-beginning-position)) 2)))

(defun my-c-equal-align ()
  "align region with the symbol '='"
  (interactive)
  (setq sym '=)
  (align-regexp (region-beginning) (region-end) sym))

(defun intelligent-close ()
  "quit a frame the same way no matter what kind of frame you are on"
  (interactive)
  (if (eq (car (visible-frame-list)) (selected-frame))
      ;;for parent/master frame...
      (if (> (length (visible-frame-list)) 1)
          ;;close a parent with children present
          (delete-frame (selected-frame))
        ;;close a parent with no children present
	(save-buffers-kill-emacs))
    ;;close a child frame
    (delete-frame (selected-frame))))

(defun my-jump-bracket ()
  "Will bounce between matching parens just like % in vi"
  (interactive)
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
	  ((string-match "[\]})>]" prev-char) (backward-sexp 1))
	  (t (error "%s" "Not on a paren, brace, or bracket")))))

(defun my-align-vars(beg end)
  "Aligns c/c++ variable declaration names on the same column, with beginning and end taken from selected region."
  (interactive "r")
  (save-excursion
    (let (bol eol expr-end
	      (max-col 0) col
	      poslist curpos)
      (goto-char end)
      (if (not (bolp))
          (setq end (line-end-position)))
      (goto-char beg)
      (while (and (> end (point)) (not (eobp)))
        (setq bol (line-beginning-position))
        (setq eol (line-end-position))
        (beginning-of-line)
        (setq expr-end (point))
        (if (search-forward-regexp "^[^/][^/]\\([a-zA-Z][a-zA-Z]*\\)[ \t]+[^;]" eol t)
            (let ()
              (setq expr-end (match-end 1))
              (while (search-forward-regexp "\\([a-zA-Z][a-zA-Z]*\\)[ \t]+[^;]" eol t)
                (setq expr-end (match-end 1)))
              (goto-char expr-end)
              (setq col (current-column))
              (if (search-forward-regexp (concat "\\(\\*\\|&[ \t]*\\)?"
                                                 "\\([a-zA-Z\\_][a-zA-Z0-9\\_]*\\)\\([\[][0-9]+[\]]\\)?"
                                                 "\\([ \t]*,[ \t]*\\([a-zA-Z\\_][a-zA-Z0-9\\_]*\\)\\([\[][0-9]+[\]]\\)?\\)*"
                                                 "[ \t]*;$") eol t)
                  (let ((name-col-end 0))
                    (if (eq (match-beginning 2) (match-beginning 0))
                        (setq name-col-end 1))
                    (setq poslist (cons (list expr-end col (match-beginning 0) name-col-end) poslist))
                    (if (> col max-col)
                        (setq max-col col))
                    (beginning-of-next-line))
                (beginning-of-next-line)))
          (beginning-of-next-line)))
      (setq curpos poslist)
      (while curpos
        (let* ((pos (car curpos))
               (col (car (cdr pos)))
               (col-end (car (cdr (cdr pos))))
               (col-end-name (car (cdr (cdr (cdr pos)))))
               (abs-pos (car pos)))
          (goto-char abs-pos)
          (delete-region abs-pos col-end)
          (insert (make-string (+ (+ (- max-col col) 1) col-end-name) 32)))
        (setq curpos (cdr curpos))))))

(defun my-align-all-vars()
  "Aligns c/c++ variable declaration names on the same column in this buffer."
  (interactive)
  (save-excursion
    (let (beg end)
      (beginning-of-buffer)
      (setq beg (point))
      (end-of-buffer)
      (setq end (point))
      (my-align-vars beg end))))

(defun my-comment()
  "Comment or uncomment line or region"
  (interactive)
  ;; check if we need to comment or uncomment
  (setq comment t)
  (save-excursion
    (beginning-of-line)
    (save-excursion
      (if (region-active-p)
	  (goto-char (region-beginning)))
      (if (search-forward-regexp "\\(//\\)\\|\\(/\\*\\)" (line-end-position) t)
	  (setq comment nil)))
    (if comment
	(if (region-active-p)
	    (comment-region (region-beginning) (region-end))
	  (comment-line))
      (progn
	(if (region-active-p)
	    (uncomment-region (region-beginning) (region-end))
	  (uncomment-line))))))

(defun beginning-of-next-line()
  "Moves cursor to the beginning of the next line, or nowhere if at end of the buffer"
  (interactive)
  (end-of-line)
  (if (not (eobp))
      (forward-char 1)))

(defun my-format-buffer ()
  "indent whole buffer and delete trailing whitespace"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))

(defun insert-brackets () "insert brackets and go between them" (interactive)
       (insert "[]")
       (backward-char 1))

(defun insert-parentheses ()
  "insert parentheses and go between them"
  (interactive)
  (insert "()")
  (backward-char 1))

(defun my-indent-line()
  "indent current-line"
  (interactive)
  (indent-according-to-mode))

(defun my-indent-buffer ()
  "Indent the current buffer"
  (interactive)
  (save-excursion (indent-region (point-min) (point-max) nil)))

(defun my-forward-block (&optional φn)
  "Move cursor forward to the beginning of next text block.
  A text block is separated by blank lines."
  (interactive "p")
  (let ((φn (if (null φn) 1 φn)))
    (search-forward-regexp "\n[\t\n ]*\n+" nil "NOERROR" φn)))

(defun my-backward-block (&optional φn)
  "Move cursor backward to previous text block."
  (interactive "p")
  (let ((φn (if (null φn) 1 φn))
        (ξi 1))
    (while (<= ξi φn)
      (if (search-backward-regexp "\n[\t\n ]*\n+" nil "NOERROR")
          (progn (skip-chars-backward "\n\t "))
        (progn (goto-char (point-min))
               (setq ξi φn)))
      (setq ξi (1+ ξi)))))


;; install xsel
(defun copy-to-clipboard ()
  (interactive)
  (if (display-graphic-p)
      (progn
        (message "Yanked region to x-clipboard!")
        (call-interactively 'clipboard-kill-ring-save)
        )
    (if (region-active-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
          (message "Yanked region to clipboard!")
          (deactivate-mark))
      (message "No region active; can't yank to clipboard!")))
  )

(defun paste-from-clipboard ()
  (interactive)
  (if (display-graphic-p)
      (progn
        (clipboard-yank)
        (message "graphics active")
        )
    (insert (shell-command-to-string "xsel -o -b"))
    )
  )

(global-set-key [f8] 'copy-to-clipboard)
(global-set-key [f9] 'paste-from-clipboard)

(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)

(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

;; Folding

(defun my-fold-toggle ()
  "toggle fold/unfold of paragraph under the mark"
  (interactive)
  (let
      ((overlays (overlays-at (point))))
    (if overlays
        (if (not (-map 'my-fold (overlays-at (point))))
            (progn
              (message "")
              (my-fold-paragraph)))
      (if (use-region-p)
          (progn
            (message "folding manual region")
            (vimish-fold beginning end))
        (my-fold-paragraph)))))

(defun my-fold (overlay)
  ""
  (if (folded-region-p overlay)
      (vimish-fold-unfold)
    (if (unfolded-region-p overlay)
        (vimish-fold-toggle))))

(defun folded-region-p (overlay)
  ""
  (eq (overlay-get overlay 'type) 'vimish-fold--folded))

(defun unfolded-region-p (overlay)
  ""
  (eq (overlay-get overlay 'type) 'vimish-fold--unfolded))

(defun my-fold-paragraph ()
  ""
  (mark-paragraph)
  (forward-line)
  (vimish-fold (region-beginning) (region-end)))

;; * Shortcut

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BASIC COMMAND
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-SPC")   'Control-X-prefix)
(global-set-key (kbd "C-e")     'set-mark-command)
(global-set-key (kbd "M-SPC")   'execute-extended-command)
(global-set-key (kbd "C-SPC u") 'universal-argument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OPEN FILE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [f1] 'find-file)
(global-set-key (kbd "C-SPC f") 'find-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SAVE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [f2] 'save-buffer)
(global-set-key [f3] 'write-file)
(global-set-key (kbd "C-w") 'save-buffer)
(global-set-key (kbd "C-SPC w") 'write-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KILL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [f4] 'kill-emacs)
(global-set-key (kbd "C-k") 'my-kill-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BUFFER MENU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [f7] 'buffer-menu)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GOTO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\M-g" 'goto-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WINDOWS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\M-b" 'next-buffer)
(global-set-key [f12] 'repeat-complex-command)
(global-set-key (kbd "C-SPC p") 'previous-multiframe-window)
(global-set-key (kbd "C-SPC n") 'next-multiframe-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MINIBUFFER HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key minibuffer-local-map (kbd "<up>")   'previous-history-element)
(define-key minibuffer-local-map (kbd "<down>") 'next-history-element)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;; EDITION SHORTCUT ;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNDO/REDO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-z") 'redo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KILL WORD/LINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Hack to solve problem for tab and C-i
;(global-set-key "\t" 'self-insert-command)

(global-set-key (kbd "C-i") 'backward-kill-word)
(global-set-key (kbd "C-o") 'repeat)

;(keyboard-translate ?\C-i ?\M-|)
;(global-set-key [?\M-|] 'backward-kill-word)
;(global-set-key "¿" 'tab-to-tab-stop)

(global-set-key "\C-u" 	  'kill-word)
(global-set-key "\M-i" 	  'my-backward-kill-line)
(global-set-key "\M-u" 	  'my-kill-line)
(global-set-key [delete] 'delete-char) ;; delete standard behaviour

;;;;;;;;;;;;;;;;;;;;;;;;
;; COPY / CUT / PASTE ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-SPC c") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-SPC d") 'kill-region)
(global-set-key (kbd "C-v") 	'yank)
(global-set-key (kbd "M-v") 	'yank-pop)

;;;;;;;;;;;;;;;;
;; RECTANGLES ;;
;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-e") 'rectangle-mark-mode)
(global-set-key (kbd "C-SPC r c") 'copy-rectangle-to-register)    ;; supprime un rectangle en l'enregistrant
(global-set-key (kbd "C-SPC r v") 'yank-rectangle)   	;; insère le dernier rectangle enregistré
(global-set-key (kbd "C-SPC r o") 'open-rectangle)   	;; insère un rectangle de blancs
(global-set-key (kbd "C-SPC r d") 'kill-rectangle) 	;; supprime un rectangle sans l'enregistrer
(global-set-key (kbd "C-SPC r t") 'string-rectangle)   	;; insérer un string dans un rectangle


;;;;;;;;;;;;;;;;;;
;; DISPLACEMENT ;;
;;;;;;;;;;;;;;;;;;


;;;;;;;;;;
;; WORD ;;
;;;;;;;;;;

;; PAGE
(global-set-key "\C-n" 'forward-word)
(global-set-key "\C-t" 'backward-word)

;; PARAGRAPH
(global-set-key "\C-d" 'my-backward-block)
(global-set-key "\C-l" 'my-forward-block)

;; BUFFER
(global-set-key "\M-d" 'beginning-of-buffer)
(global-set-key "\M-l" 'end-of-buffer)

;; PAGE
(global-set-key "\M-t" 'scroll-down-command)
(global-set-key "\M-n" 'scroll-up-command)



;;;;;;;;;
;; DEV ;;
;;;;;;;;;


(define-key global-map (kbd "C-SPC .") 'comment-line)
(define-key global-map (kbd "C-SPC :") 'uncomment-line)

(keyboard-translate ?\C-y ?\C-é)
;(global-set-key (kbd "é") "y")
(global-set-key (kbd "C-y") 'my-copy-line)

(global-set-key (kbd "C-SPC i") 'indent-region)

(global-set-key (kbd "C-SPC C-c")  'comment-region)
(global-set-key (kbd "C-SPC C-u")  'uncomment-region)

;; Behaviour for emacs in terminal
(if (display-graphic-p)
   (progn
      ;; shortcut terminator like :)
      (global-set-key (kbd "C-S-h") 'split-window-vertically)
      (global-set-key (kbd "C-S-v") 'split-window-horizontally)
      (global-set-key (kbd "C-S-r") 'windmove-right)
      (global-set-key (kbd "C-S-s") 'windmove-left)
      (global-set-key (kbd "C-S-n") 'windmove-down)
      (global-set-key (kbd "C-S-t") 'windmove-up)
      (global-set-key (kbd "C-S-d") 'delete-window))
  ;; else console mode
  (global-unset-key (kbd "C-@"))
  (global-set-key (kbd "C-@") 'Control-X-prefix))


;; Same with return and C-m
;(keyboard-translate ?\C-m ?\C-&)
;(global-set-key (kbd "C-&") 'newline-and-indent)
;(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "RET") 'newline-and-indent)
;; Same with C-c which is a prefix key
;(keyboard-translate ?\C-j ?\C-.)
(global-unset-key (kbd "C-@ C-@"))
(global-set-key (kbd "C-SPC C-SPC") 'execute-extended-command)

;; Go 2 lines up or down
(global-set-key (kbd "\C-s") 'scroll-up-lot)
(global-set-key (kbd "\C-r") 'scroll-down-lot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion automatique ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [(f1)] 'dabbrev-completion)
(global-set-key (kbd "C-q") 'dabbrev-expand)
(global-set-key (kbd "M-q") 'dabbrev-completion)

(global-set-key (kbd "M-<down>") 'move-line-region-down)
(global-set-key (kbd "M-<up>") 'move-line-region-up)

(global-set-key (kbd "M-S-t") 'tabbar-backward-group)
(global-set-key (kbd "M-S-n") 'tabbar-forward-group)
(global-set-key "\M-t" 'tabbar-backward-tab)
(global-set-key "\M-n" 'tabbar-forward-tab)

;;;;;;;;;;;;;;;
;; Recherche ;;
;;;;;;;;;;;;;;;
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "M-f") 'isearch-backward)
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)
(define-key isearch-mode-map "\M-f" 'isearch-repeat-backward)

;;;;;;;;;;;;;;
;; RECENTER ;;
;;;;;;;;;;;;;;
(global-set-key (kbd "C-S-l") 'my-horizontal-recenter)

;; ne detruit pas le serveur si le fichier dans lequel on se trouve est un client
(global-set-key (kbd "C-SPC q") 'intelligent-close)

;;;;;;;;;;;;;;;;;;;
;; FRAME SCALING ;;
;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-M-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "C-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-<down>")  'shrink-window)
(global-set-key (kbd "C-M-<up>")    'enlarge-window)

;;;;;;;;;;;;;
;; Compile ;;
;;;;;;;;;;;;;
(global-set-key (kbd "C-p")  'recenter-top-bottom)

(global-set-key (kbd "C-SPC m")  'rename-this-
(message "\n -- setting shortcuts --\n")
;; * System

;; Fast boot
(modify-frame-parameters nil '((wait-for-wm . nil)))
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)

;; Always show line number
(global-display-line-numbers-mode)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Always follow symlink
(setq vc-follow-symlinks t)

;; Opening file side by side rather than onTop/below
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; BACKUP
(defvar my-backup-directory (concat user-emacs-directory "backups"))
(unless (file-exists-p my-backup-directory)
  (make-directory my-backup-directory))
(setq backup-directory-alist
  `((".*" . ,my-backup-directory)))
(setq auto-save-file-name-transforms
  `((".*" ,my-backup-directory t)))
(setq delete-old-versions t
      backup-by-copying t          ; copy rather than rename, slower but simpler
      kept-new-versions 6
      kept-old-versions 2
      version-control t            ; version numbers for backup file
      delete-old-versions t
      delete-by-moving-to-trash t
      auto-save-default nil        ; no #file# backups
      )

;; UTF-8
(set-language-environment   'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)
(prefer-coding-system       'utf-8)


;; Scroll behaviour
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; No carriage return for long line
(if (boundp 'truncate-lines)
    (setq-default truncate-lines t) ; always truncate
  (progn
    (setq hscroll-margin 1)
    (setq auto-hscroll-mode 1)
    (setq automatic-hscrolling t)))

;; le contenu se déplace d'une seule ligne en fin de fenetre
;(setq scroll-step 1)

;; No visual alert
(setq visible-bell 'nil)

;; Cursor position fixed when page is scrolled
;(setq scroll-preserve-screen-position t)

; Save cursor position and load it automatically when opening file
(setq save-place-file (concat user-emacs-directory "saveplace"))
(setq-default save-place t)
(require 'saveplace)

;; Find case sensitive
(setq case-fold-search t)

;; Selection can be overwrite
(delete-selection-mode 1)

;; Mouse support
(if (load "mwheel" t)
	(mwheel-install))

;; Corresponding parentheses shown
(require 'paren)
(show-paren-mode t)
(setq blink-matching-paren t
 blink-matching-paren-on-screen t
 blink-matching-paren-dont-ignore-comments t)
;(set-face-background 'show-paren-match-face "green")

;; Automatic completion
(require 'dabbrev)
(set 'dabbrev-case-fold-search nil)
(set 'dabbrev-case-replace nil)
(global-set-key [(f1)] 'dabbrev-completion)
(global-set-key (kbd "\C-q") (quote dabbrev-expand))

;; Mode associated to file extension
(setq auto-mode-alist
  (append
    '(("\\.C$"    . c++-mode)
      ("\\.H$"    . c++-mode)
      ("\\.cc$"   . c++-mode)
      ("\\.hh$"   . c++-mode)
      ("\\.c$"    . c-mode)
      ("\\.h$"    . c++-mode)
      ("\\.m$"    . objc-mode)
      ("\\.java$" . java-mode)
      ("\\.tex$"  . latex-mode)
      ("\\.markdown$" . markdown-mode)
      ("\\.md$" . markdown-mode)
   ) auto-mode-alist))

;; Use same buffer for compilation
(setq-default display-buffer-reuse-frames t)

;; Allow narrowing region
(put 'narrow-to-region 'disabled nil)

;; Allow downcase-region
(put 'downcase-region 'disabled nil)

;; Indent with space only
(setq-default indent-tabs-mode nil)

;; No scratch buffer
;;(kill-buffer "*scratch*")

(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

(message "\n -- setting system --\n")
;; * Plugin

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defun install-package (name)
  (unless (package-installed-p name)
    (package-refresh-contents) (package-install name)))

;;; Redo +
;(install-package 'redo+)
;(require 'redo+)

;;; Nav
(install-package 'nav)
(require 'nav)
(global-set-key [f8] 'nav-toggle)

;;; Tabbar
(install-package 'tabbar)

;;; Ido
(require 'ido)
(ido-mode t)

;;; Flx-ido
(install-package 'flx-ido)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;;; Projectile
(install-package 'projectile)
(projectile-mode)
(global-set-key (kbd "C-SPC a") 'projectile-find-file)
                                        ;(global-set-key (kbd "C-SPC d") 'projectile-find-file-in-directory)
(global-set-key (kbd "C-SPC p") 'projectile-switch-project)

;;; Ace jump mode
(install-package 'ace-jump-mode)
(require 'ace-jump-mode)
(setq ace-jump-mode-case-fold nil)
(global-set-key (kbd "C-j") 'ace-jump-mode)
(global-set-key (kbd "M-j") 'ace-jump-char-mode)

;;; Expand region
(install-package 'expand-region)
(require 'expand-region)
(global-set-key (kbd "M-o") 'er/expand-region)

;;; Multiple cursor
(install-package 'multiple-cursors)
(require 'multiple-cursors)

;;; Magit
(install-package 'magit)
(require 'magit)
;; prevent instructions from being shown at startup
(setq magit-last-seen-setup-instructions "1.4.0")

;;; Org mode
(install-package 'org)
(require 'org-install)
(load "~/.emacs.d/conf/org.el")

;;; Web mode (.erb)
(install-package 'web-mode)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(setq web-mode-extra-auto-pairs
      '(("erb"  . (("open" "close")))
        ("php"  . (("open" "close")
                   ("open" "close")))
        ))
(setq web-mode-enable-auto-pairing t)

;;; Coffee mode
(install-package 'coffee-mode)
(custom-set-variables '(coffee-tab-width 2))

;;; Slim-mode
(install-package 'slim-mode)
(require 'slim-mode)

;;; Github markdown
(install-package 'markdown-mode)
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(custom-set-variables
 '(markdown-command "rdiscount"))

;;; ERC
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(require 'erc-match)
(setq erc-keywords '("iori" "itsu"))
(setq erc-autojoin-channels-alists
      '(("freenode.net" "#emacs" "#haskell")))
(add-to-list 'erc-nick "itsu")

;;; Jade
(install-package 'jade-mode)
(require 'jade-mode)

;;; Elm
(install-package 'elm-mode)
(require 'elm-mode)

;;; Csv
(install-package 'csv-mode)
(require 'csv-mode)

;;; Vimish Fold
(install-package 'vimish-fold)
(require 'vimish-fold)
(global-set-key (kbd "C-SPC t") 'my-fold-toggle)

;;; Outline & Outshine

(install-package 'outshine)
(require 'outshine)

(add-hook 'outline-minor-mode-hook 'outshine-mode)
(add-hook 'prog-mode-hook 'outline-minor-mode)
;; for haskell, headers like `--*` will not compile
;; this var makes it possible to write headers like: `-- *`
(setq outshine-preserve-delimiter-whitespace t)

(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

(custom-set-faces
 '(markdown-header-face-1 ((t (:inherit outline-1))))
 '(markdown-header-face-2 ((t (:inherit outline-2))))
 '(outline-1 ((t (:foreground "red" :bold t :underline t))))
 '(outline-2 ((t (:foreground "yellow" :bold nil :underline t))))
 )

(set-display-table-slot standard-display-table
                        'selective-display
                        (string-to-vector "[…]"))

(defun -add-font-lock-kwds (FONT-LOCK-ALIST)
  (font-lock-add-keywords
   nil (--map (-let (((rgx uni-point) it))
                `(,rgx (0 (progn
                            (compose-region (match-beginning 1) (match-end 1)
                                            ,(concat "\t" (list uni-point)))
                            nil))))
              FONT-LOCK-ALIST)))

(defmacro add-font-locks (FONT-LOCK-HOOKS-ALIST)
  `(--each ,FONT-LOCK-HOOKS-ALIST
     (-let (((font-locks . mode-hooks) it))
       (--each mode-hooks
         (add-hook it (-partial '-add-font-lock-kwds
                                (symbol-value font-locks)))))))

(defconst lisp-outlines-font-lock-alist
  ;; Outlines
  '(("\\(-- \\*\\) "          ?■)
    ("\\(-- \\*\\*\\) "       ?○)
    ("\\(-- \\*\\*\\*\\) "    ?✸)
    ("\\(-- \\*\\*\\*\\*\\) " ?✿)))

(defconst lisp-outlines-font-lock-alist
  ;; Outlines
  '(("\\(^;; \\*\\) "          ?■)
    ("\\(^;; \\*\\*\\) "       ?✸)
    ("\\(^;; \\*\\*\\*\\) "    ?✿)
    ("\\(^;; \\*\\*\\*\\*\\) " ?○)))

(defconst haskell-outlines-font-lock-alist
  ;; Outlines
  '(("\\(^-- \\*\\) "          ?■)
    ("\\(^-- \\*\\*\\) "       ?✸)
    ("\\(^-- \\*\\*\\*\\) "    ?✿)
    ("\\(^-- \\*\\*\\*\\*\\) " ?○)))

(add-font-locks
 '((haskell-outlines-font-lock-alist haskell-mode-hook)))

(add-font-locks
 '((lisp-outlines-font-lock-alist emacs-lisp-mode-hook)))

(message "\n -- setting plugin --\n")

;; * Dev

(message "\n -- setting dev --\n")

;; ** Haskell

(install-package 'haskell-mode)
(require 'haskell-mode)
(require 'haskell-interactive-mode)
(require 'haskell-process)

;; This will auto insert "module XXX where" template when creating a new XXX.hs file
(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; With ghc 8.X, errors are no longer shown in the repl. This fix it !
(setq haskell-process-args-stack-ghci
      '("--ghci-options=-ferror-spans -fshow-loaded-modules"
        "--no-build" "--no-load"))

(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-interactive-popup-errors nil)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-log t)
 '(haskell-process-type 'stack-ghci))

(setq haskell-compile-cabal-build-command "stack build")

;; Create tags on save
(custom-set-variables '(haskell-tags-on-save t))
(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)

;; somehow this settings remove the pragma: {-# LANGUAGE ViewPatterns #-} on every save...
;;(custom-set-variables '(haskell-stylish-on-save t))


;;; Shortcuts

(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
     (define-key haskell-mode-map (kbd "C-c C-l") 'my-haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-c C-m") 'my-load-and-execute)
     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
     ))

(define-key haskell-mode-map (kbd "M-s") 'haskell-interactive-mode-history-previous)
(define-key haskell-mode-map (kbd "M-r") 'haskell-interactive-mode-history-next)
(define-key haskell-cabal-mode-map (kbd "M-m") 'tabbar-forward-tab)

(defun my-haskell-process-load-file ()
  (interactive)
  "clear console & load code"
                                        ;  (when (fboundp 'haskell-interactive-mode-clear)
  (haskell-interactive-mode-clear)
  (haskell-process-load-file))

(defun my-load-and-execute ()
  (interactive)
  "load or reload code and execute the m function if present"
  (save-excursion
    (my-haskell-process-load-file)
    (haskell-interactive-switch)
    (insert "main")
    (haskell-interactive-mode-return)
    (sit-for 0.500)
    (haskell-interactive-switch-back)
    ))

;; ** Ruby

;; Enhanced Ruby Mode
(install-package 'ruby-mode)
(require 'ruby-mode)

(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent)
  (local-unset-key (kbd "C-j"))
)
(add-hook 'ruby-mode-hook 'set-newline-and-indent)

;; HAML mode
(install-package 'haml-mode)

;; RSense
(install-package 'rsense)
(require 'rsense)

;; Electric-ruby
(add-to-list 'load-path "~/.emacs.d/plugins")
(require 'ruby-electric)
(add-hook 'ruby-mode-hook
          (lambda () (ruby-electric-mode t)))

;; Rinari
(install-package 'rinari)
(require 'rinari)
(global-rinari-mode)

(add-hook 'rinari-minor-mode-hook (lambda()
  (local-set-key (kbd "C-c c") 'rinari-find-controller)
  (local-set-key (kbd "C-c m") 'rinari-find-model)
  (local-set-key (kbd "C-c v") 'rinari-find-view)
  (local-set-key (kbd "C-c f") 'rinari-find-file-in-project)
  (local-set-key (kbd "C-j")   'ace-jump-mode)
))

;; do not add header => -*- coding: utf-8 -*-
(setq ruby-insert-encoding-magic-comment nil)

;; ** HTML

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;; ** CSS

(setq css-indent-offset 2)
(setq scss-indent-offset 2)

;; ** Javascript

(setq js-indent-level 2)

;; * Appearance

(message "\n -- setting appeareance --\n")

;; No menu nor bar
(tool-bar-mode 0)
(menu-bar-mode 0)

;; syntaxical colorisation enabled
(require 'font-lock)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; 24h hour format
(display-time)
(setq display-time-24hr-format t)

;; Line and column number enabled
(column-number-mode t)
(line-number-mode t)

;; No blinking cursor
(blink-cursor-mode nil)

;; Frame name = edited file name
(setq frame-title-format '(buffer-file-name "%f"))

;; Underline current line
;(global-hl-line-mode 1)
;(set-face-background 'hl-line "#111")
;(set-face-foreground 'highlight nil)

;; Tabbar
(require 'tabbar)
(tabbar-mode)

(setq
 tabbar-scroll-left-help-function  nil   ; do not show help information
 tabbar-scroll-right-help-function nil
 tabbar-help-on-tab-function       nil
 tabbar-home-help-function         nil
 tabbar-buffer-home-button  (quote (("") "")) ; do not show tabbar button
 tabbar-scroll-left-button  (quote (("") ""))
 tabbar-scroll-right-button (quote (("") "")))

(set-face-attribute 'tabbar-default nil :weight 'normal :width 'normal :background "blue" :underline nil)
(set-face-attribute 'tabbar-unselected	nil :background "blue"   :foreground "white" :box nil)
(set-face-attribute 'tabbar-selected	nil :background "yellow" :foreground "black" :box nil)
(setq tabbar-separator '(1))

;; Add a buffer modification state indicator in the tab label, and place a
;; space around the label to make it looks less crowd.
(defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
  (setq ad-return-value
    (if (and (buffer-modified-p (tabbar-tab-value tab))
             (buffer-file-name (tabbar-tab-value tab)))
        (concat "*" (concat ad-return-value ""))
        (concat "" (concat ad-return-value "")))))

(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
This function is a custom function for tabbar-mode's tabbar-buffer-groups.
This function group all buffers into 3 groups:
Those Dired, those user buffer, and those emacs buffer.
Emacs buffer are those starting with “*”."
  (list
   (cond
    ((string-equal "*" (substring (buffer-name) 0 1))
     "Emacs Buffer"
     )
    ((eq major-mode 'dired-mode)
     "Dired"
     )
    (t
     "User Buffer"
     )
    )))

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)
