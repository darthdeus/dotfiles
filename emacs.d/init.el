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

;; Force saving of backup files from the following:
;;
;; - https://stackoverflow.com/a/20824625/72583
;; - https://www.emacswiki.org/emacs/ForceBackups
;;
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.

(setq vc-make-backup-files t)

;; Default and per-save backups go here:
(setq backup-directory-alist '(("" . "~/.emacs.d/backup/per-save")))

(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; PACKAGES ;;;;;;;;;;;;;;;;;

(straight-use-package 'use-package)

(use-package magit :straight t)
(use-package company :straight t :config (global-company-mode 1))
(use-package evil :straight t :config (evil-mode 1))
; (use-package sly :straight t)

(defun paredit-hooks ()
  (add-hook 'emacs-lisp-mode-hook 'evil-smartparens-mode)
  (add-hook 'lisp-mode-hook 'evil-smartparens-mode))

(use-package evil-smartparens
  :straight t
  :config
  (paredit-hooks))

;; (use-package helm :straight t)
(use-package vertico :straight t :config (vertico-mode 1))


