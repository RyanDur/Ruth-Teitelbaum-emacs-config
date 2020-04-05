(use-package add-node-modules-path
  :ensure t
  :config
  (eval-after-load 'typescript-mode
    '(add-hook 'typescript-mode-hook #'add-node-modules-path))
  (eval-after-load 'js-mode
    '(add-hook 'js-mode-hook #'add-node-modules-path)))

(add-to-list 'load-path "/Users/ryandurling/.emacs.d/submodules/tern/emacs")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(setq js-indent-level 2)

(use-package auto-complete
  :ensure t
  :config
  (add-hook 'js-mode-hook (lambda () (auto-complete-mode t)))
  (eval-after-load 'tern
    '(progn
       (require 'tern-auto-complete)
       (tern-ac-setup))))

(use-package exec-path-from-shell
  :ensure t
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (push "HISTFILE" exec-path-from-shell-variables)
  (exec-path-from-shell-initialize))

(add-hook 'after-init-hook 'global-company-mode)

(use-package indium
  :ensure t
  :config
  (add-hook 'js-mode-hook #'indium-interaction-mode))

(use-package flycheck
  :defer 1
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(javascript-jshint)))

  (flycheck-add-mode 'javascript-eslint 'js-mode)

  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(json-jsonlist)))

  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
		  (or (buffer-file-name) default-directory)
		  "node_modules"))
	   (eslint (and root
			(expand-file-name "node_modules/eslint/bin/eslint.js"
					  root))))
      (when (and eslint (file-executable-p eslint))
	(setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'js-mode-hook #'my/use-eslint-from-node-modules))

(defun eslint-fix-file ()
  (interactive)
  (message "eslint --fixing the file" (buffer-file-name))
  (shell-command (concat "node_modules/eslint/bin/eslint.js --fix " (buffer-file-name))))

(defun eslint-fix-file-and-revert ()
  (interactive)
  (eslint-fix-file)
  (revert-buffer t t))

(add-hook 'js-mode-hook
	  (lambda ()
	    (add-hook 'after-save-hook #'eslint-fix-file-and-revert)))

(use-package typescript-mode
  :ensure t)

(use-package ansi-color
  :ensure t
  :config
  (defun colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point-max)))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(add-to-list 'load-path "/Users/ryandurling/.emacs.d/submodules/auto-fix.el")
(autoload 'auto-fix-mode "auto-fix.el" nil t)
(add-hook 'typescript-mode-hook (lambda () (auto-fix-mode t)))
(add-hook 'auto-fix-mode-hook
	  (lambda () (add-hook 'before-save-hook #'auto-fix-before-save)))

(defun setup-ts-auto-fix ()
  (setq-local auto-fix-command "tslint")
  (auto-fix-mode +1))

(add-hook 'typescript-mode-hook #'setup-ts-auto-fix)
