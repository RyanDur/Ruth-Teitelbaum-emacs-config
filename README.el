(defun load-if-exists (file)
  (if (file-exists-p file)
      (progn
	(load file)
	(message (format "Loading file: %s" file)))
    (message (format "No %s file. So not loading one." file))))

(defun org-babel-load-if-exists (file)
  (if (file-exists-p (concat user-emacs-directory file))
      (progn
	(org-babel-load-file (concat user-emacs-directory file))
	(message (format "Loading file: %s" file)))
    (message (format "No %s file. So not loading one." file))))

(use-package diminish :ensure t)

(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(put 'narrow-to-region 'disabled nil)

(setq custom-file (concat user-emacs-directory "local/custom.el"))
(load-if-exists custom-file)

(org-babel-load-if-exists "custom/org-mode-settings.org")
