(global-set-key (kbd "C-c l") 'org-store-link)   
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-log-done t)

(setq org-agenda-files (list "~/org"))

(setq org-src-window-setup 'other-window)

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-edit-src-content-indentation 0)
