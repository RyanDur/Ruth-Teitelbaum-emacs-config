(add-to-list 'load-path "/Users/ryandurling/.emacs.d/custom-downloads/tern/emacs")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))