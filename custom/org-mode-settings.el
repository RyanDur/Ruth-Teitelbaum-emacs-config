(use-package org
  :pin org
  :ensure org-plus-contrib
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))

  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)

  (setq org-log-done t)

  (setq org-agenda-files (list "~/org"))
  (setq org-return-follows-link t)

  ;; M-S-RET not working in the terminal
  (org-defkey org-mode-map (kbd "C-c b") 'org-insert-todo-heading)

  ;; Better source code window editing
  (setq org-src-window-setup 'other-window)

  ;; Highlight and indent source code blocks
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0))

(add-hook 'org-mode-hook 'git-auto-commit-mode)

(use-package magit-org-todos
  :ensure t
  :config
  (magit-org-todos-autoinsert))

(use-package helm-bibtex
  :ensure t
  :config
  (setq bibtex-completion-bibliography
	'("~/Documents/bib/references.bib")))

(use-package org-ref
  :ensure t
  :config
  (setq reftex-default-bibliography '("~/Documents/bib/references.bib"))
  (setq org-ref-default-bibliography '("~/Documents/bib/references.bib")
	org-ref-pdf-directory "~/Documents/bib/pdfs/"))
