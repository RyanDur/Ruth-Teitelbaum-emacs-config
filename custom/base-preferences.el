(fset 'yes-or-no-p 'y-or-n-p)

(use-package diminish :ensure t)

(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.d/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(put 'narrow-to-region 'disabled nil)

(setq custom-file (concat user-emacs-directory "local/custom.el"))
(load-if-exists custom-file)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config (color-theme-sanityinc-tomorrow-bright))

(use-package powerline
  :ensure t
  :config (powerline-default-theme))

(blink-cursor-mode t)

(delete-selection-mode t)

(show-paren-mode t)

(define-key global-map (kbd "RET") 'newline-and-indent)

;; no toolbar
(tool-bar-mode -1)

;; no menu in a terminal
(unless window-system
  (menu-bar-mode -1))

;; no scroll bar
;;(scroll-bar-mode -1)

;; no horizontal scroll bar
(when (boundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(global-linum-mode 1)
(line-number-mode 1)
(column-number-mode 1)

(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; scratch buffer created -- happy hacking\n")

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x t") 'toggle-window-split)

(ido-mode t)
(setq ido-decorations
      (quote
       ("\n-> "
	""
	"\n   "
	"\n   ..."
	"["
	"]"
	" [No match]"
	" [Matched]"
	" [Not readable]"
	" [Too big]"
	" [Confirm]")))
(defun ido-disable-line-trucation ()
  (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

(setq-default indicate-empty-lines t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(global-set-key (kbd "C-c i") 'indent-buffer)

(use-package flyspell
  :ensure t
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (setq flyspell-issue-message-flg nil))

(defun flyspell-emacs-popup-textual (event poss word)
  "A textual flyspell popup menu."
  (require 'popup)
  (let* ((corrects (if flyspell-sort-corrections
		       (sort (car (cdr (cdr poss))) 'string<)
		     (car (cdr (cdr poss)))))
	 (cor-menu (if (consp corrects)
		       (mapcar (lambda (correct)
				 (list correct correct))
			       corrects)
		     '()))
	 (affix (car (cdr (cdr (cdr poss)))))
	 show-affix-info
	 (base-menu
	  (let ((save
		 (if (and (consp affix) show-affix-info)
		     (list
		      (list (concat "Save affix: " (car affix))
			    'save)
		      '("Accept (session)" session)
		      '("Accept (buffer)" buffer))
		   '(("Save word" save)
		     ("Accept (session)" session)
		     ("Accept (buffer)" buffer)))))
	    (if (consp cor-menu)
		(append cor-menu (cons "" save))
	      save)))
	 (menu (mapcar
		(lambda (arg) (if (consp arg) (car arg) arg))
		base-menu)))
    (cadr (assoc (popup-menu* menu :scroll-bar t) base-menu))))

(eval-after-load "flyspell"
  '(progn
     (fset 'flyspell-emacs-popup 'flyspell-emacs-popup-textual)))

(defun flyspell-emacs-popup-choose (org-fun event poss word)
  (if (window-system)
      (funcall org-fun event poss word)
    (flyspell-emacs-popup-textual event poss word)))

(eval-after-load "flyspell"
  '(progn
     (advice-add 'flyspell-emacs-popup
		 :around #'flyspell-emacs-popup-choose)))

(global-set-key (kbd "C-c s") 'flyspell-correct-word-before-point)

(org-babel-load-if-exists "custom/tree.org")

(use-package popup :ensure t)

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
  If there's no region, the current line will be duplicated. However, if
  there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
	(exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
	(exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
	(goto-char end)
	(newline)
	(insert region)
	(setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)
