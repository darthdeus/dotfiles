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

(global-set-key (kbd "<f5>") (lambda () (interactive)
			       (find-file "~/.emacs.d/init.el")))

(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "C-c") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "C-c") 'evil-exit-visual-state)

(define-key evil-normal-state-map (kbd ",,") 'evil-buffer)

; (global-evil-surround-mode 1)
;
; (define-key evil-normal-state-map (kbd ",f") 'projectile-find-file)
; (define-key evil-normal-state-map (kbd "q") nil)

;; Make C-e, C-d, C-k behave same as in Emacs when in insert mode.
(define-key evil-insert-state-map (kbd "C-e") nil)
(define-key evil-insert-state-map (kbd "C-d") nil)
(define-key evil-insert-state-map (kbd "C-k") nil)

;; Makes C-e behave same as in Emacs. C-a works out of the box
(define-key evil-motion-state-map (kbd "C-e") nil)

;; Switching between windows with C-hjkl
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)

(define-key evil-normal-state-map (kbd "C-x h k") 'describe-key)
(define-key evil-normal-state-map (kbd "C-x h f") 'describe-function)
(define-key evil-normal-state-map (kbd "C-x h v") 'describe-variable)

(define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-down)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

;; Insert mode as well
(define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)

(define-key evil-insert-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-insert-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-insert-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-insert-state-map (kbd "C-k") 'evil-window-up)

(define-key evil-insert-state-map (kbd "C-x h k") 'describe-key)
(define-key evil-insert-state-map (kbd "C-x C-k C-k") 'kill-line)
