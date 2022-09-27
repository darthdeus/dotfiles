;; mklink /j c:\users\jakub\.emacs.d c:\users\jakub\dotfiles\emacs.d

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(global-set-key [f7] (lambda ()
		       (interactive)
		       (find-file user-init-file)))

(global-set-key [f8] (lambda ()
		       (interactive)
		       (magit)))

(straight-use-package 'use-package)

(use-package magit :straight t)
(use-package evil :straight t)
