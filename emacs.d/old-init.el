;; PACKAGE CONFIG
(require 'package)
; TODO: does this make sense only together with use-package?
; https://www.reddit.com/r/emacs/comments/1rdstn/set_packageenableatstartup_to_nil_for_slightly/
(setq package-enable-at-startup nil)

(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "https://orgmode.org/elpa/")
                    ("melpa" . "https://melpa.org/packages/")
                    ("melpa-stable" . "https://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

; We use https://orgmode.org/worg/org-contrib/babel/ to load the code
; from org-mode.
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

;; Shows options when only part of a key binding (chord?) is pressed.
(use-package which-key
  :ensure t
  :init (which-key-mode 1))

;; TODO: try https://github.com/owainlewis/emacs-color-themes

;; Is there a higher-contrast variant of base16-ember?
;; Other themes are at https://github.com/chriskempson/base16.
(use-package base16-theme
  :ensure t
  :config (load-theme 'base16-default-dark t))

; (use-package spacemacs-theme
;   :ensure t
;   :config (load-theme 'spacemacs-dark t))

; (when (not package-archive-contents)
;   (package-refresh-contents)
;   (package-install 'use-package))


;; Saner emacs defaults
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

; TODO: is it better to use `defalias` or `fset`?
(defalias 'yes-or-no-p 'y-or-n-p)
; (fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "<s-C-return>") 'ansi-term)

(setq-default indent-tabs-mode nil)

; Disable the visual bell
(setq ring-bell-function 'ignore)
; Scroll line by line instead of moving half a screen when
; the cursor moves off.
(setq scroll-conservatively 100)
; Highlight the current line, but only in GUI mode.
(when window-system (global-hl-line-mode t))
; Render some symbols (like lambda) with a unicode symbol.
(when window-system (global-prettify-symbols-mode t))

; Disable backup? (setq make-backup-file nil)
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))
(setq auto-save-default nil)

(use-package beacon
  :ensure t
  :init (beacon-mode 1))



;; TODO - check how to enable ido for M-x
(ido-mode 1)
(ido-everywhere 1) ; TODO: ido-completing-read+
; TODOODODO
; (ido-ubiquitous 1)
; (flx-ido-mode 1)

;; EVIL MODE
(use-package evil
  :ensure t
  :init (evil-mode 1))

; (require 'evil-surround)

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

(define-key evil-normal-state-map (kbd "C-d") 'scroll-up-command)
(define-key evil-normal-state-map (kbd "C-u") 'scroll-down-command)

;; Insert mode as well
(define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)

(define-key evil-insert-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-insert-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-insert-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-insert-state-map (kbd "C-k") 'evil-window-up)

(define-key evil-insert-state-map (kbd "C-x h k") 'describe-key)
(define-key evil-insert-state-map (kbd "C-x C-k C-k") 'kill-line)

; (add-to-list 'evil-insert-state-modes 'inferior-haskell-mode)

; (require 'use-package)
;
; (defvar my-packages
;   '(clojure-mode
;     cider
;     rainbow-delimiters
;     paredit
;     clj-refactor
;     evil
;     evil-surround
;     evil-paredit
;     company
;     grizzl
;     projectile
;     undo-tree
;     sequential-command
;     flx
;     flx-ido
;     ido-ubiquitous
;     smex
;     color-theme-sanityinc-tomorrow
;     dirtree
;     haskell-mode ghc hindent hi2
;     gist
;     magit
;     yasnippet)
;   "A list of packages installed at launch")
;
; (use-package ensime
;              :ensure t
;              :pin melpa-stable)
;
; ;; Automatically install a pre-defined list of packages
; (dolist (p my-packages)
;   (when (not (package-installed-p p))
;     (package-install p)))

;; OLD CONFIG
;; ***********************************************************************************************************
;
; ;; path settings
; (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Users/darth/.cabal/bin:/Applications/ghc-7.8.3.app/Contents/bin/ghc"))
; (setq exec-path (append exec-path '("/usr/local/bin")))
; (setq exec-path (append exec-path '("/Users/darth/.cabal/bin")))
; (setq exec-path (append exec-path '("/Applications/ghc-7.8.3.app/Contents/bin/ghc")))
; (setq eshell-path-env "/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin")
;
;
; (require 'haskell-mode)
; (load "haskell-mode-autoloads.el")
;
; ;; blinking is annoying
; (blink-cursor-mode 0)
;
; (global-undo-tree-mode 0)
; (global-company-mode 1)
; (setq company-idle-delay 0)
;
; ;; Lisp settings
; (setq inferior-lisp-program "/usr/local/bin/clisp")
; (setq slime-contribs '(slime-fancy))
;
; (add-hook 'racket-mode-hook #'enable-paredit-mode)
;
; (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
; (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
; (add-hook 'emacs-lisp-mode-hook #'evil-paredit-mode)
;
; (add-hook 'clojure-mode-hook #'enable-paredit-mode)
; (add-hook 'clojure-mode-hook #'evil-paredit-mode)
; (add-hook 'clojure-mode-hook #'eldoc-mode)
;
; (require 'rainbow-delimiters)
; (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;
; (add-hook 'cider-repl-mode-hook #'subword-mode)
; (add-hook 'cider-repl-mode-hook #'paredit-mode)
; (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
; (add-hook 'cider-mode-hook #'eldoc-mode)
;
; (require 'clj-refactor)
; (add-hook 'clojure-mode-hook #'clj-refactor-mode)
; (cljr-add-keybindings-with-prefix "C-c C-s")
;
; ;; TODO - try to use this instead of paredit
; ; (require 'evil-smartparens)
; ; (add-hook 'clojure-mode-hook #'evil-smartparens-mode)
; ; (require 'smartparens-config)
; ; (smartparens-global-mode 1)
;
; ;; Coq - temporarily disabled to improve startup time
; ; (load-file "/usr/local/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")
; ; (load-file "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")
; ; (setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
; ; (autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)
;
; (when window-system (scroll-bar-mode 0))
; (when tool-bar-mode (tool-bar-mode 0))
; (when window-system (menu-bar-mode 0))
; (when (not (window-system)) (menu-bar-mode 0))
;
; ;; Smarter completion for M-x (ido style, but also msart)
; (smex-initialize)
; (global-set-key (kbd "M-x") 'smex)
; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
; (global-set-key (kbd "C-c m s") 'magit-status)
; (global-set-key (kbd "s-e") 'eval-region)
; (global-set-key (kbd "s-/") 'comment-region)
; (global-set-key (kbd "s-?") 'uncomment-region)
; (global-set-key (kbd "C-`") 'haskell-interactive-bring)
; (global-set-key (kbd "<f1>") 'makey-key-mode-popup-isearch)
;
; ;; Projectile config
; (projectile-global-mode)
; (setq projectile-enable-caching t)
;
; (add-hook 'before-save-hook 'delete-trailing-whitespace)
;
; (add-hook 'markdown-mode-hook '(lambda ()
;                                  (flyspell-mode 1)
;                                  (auto-fill-mode 1)))
;
; (add-hook 'speedbar-mode-hook '(lambda ()
;                                  (local-set-key (kbd "C-l") 'evil-window-right)
;                                  (local-set-key (kbd "C-h") 'evil-window-left)
;                                  (local-set-key (kbd "C-j") 'evil-window-down)
;                                  (local-set-key (kbd "C-k") 'evil-window-up)))
;
; (require 'yasnippet)
; (yas-load-directory "~/.emacs.d/snippets" t)
; (yas-global-mode 1)
;
;
; (require 'hi2)
; (autoload 'ghc-init "ghc" nil t)
; (add-hook 'haskell-mode-hook
; 	  (lambda ()
; 	    (ghc-init)
;             (turn-on-hi2)
;             ;; This is only needed when ghc-mod is loaded
;             (global-set-key (kbd "s-s") 'ghc-save-buffer)
;
;             (define-key evil-normal-state-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
;             (define-key evil-insert-state-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)))
;
; (eval-after-load "haskell-mode"
;   '(progn
;      (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
;      (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)))
;
; (setq haskell-tags-on-save t)
;
; (global-set-key (kbd "C-x a r") 'align-regexp)
; (global-set-key (kbd "<f5>") 'neotree-toggle)
; (global-set-key (kbd "<f12>") 'haskell-process-reload-devel-main)
;
; (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
;
; '(haskell-process-suggest-remove-import-lines f)
; '(haskell-process-auto-import-loaded-modules t)
;
; (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
; (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
; (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
; (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
; (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
; (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
; (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
; (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def)
; (define-key haskell-mode-map (kbd "C-c i") 'hindent/reformat-decl)
;
; (setq ghc-display-error 'minibuffer)
; (setq ghc-hlint-options '("-fno-warn-unused-do-bind"))
;
; ;; Packages and other emacs things that I want to check out
; ; http://www.emacswiki.org/emacs/SequentialCommand
; ; http://www.emacswiki.org/emacs/sequential-command.el
; ; http://www.emacswiki.org/emacs/sequential-command-config.el
; ; SICP video course - http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video-lectures/
; ; http://ergoemacs.org/emacs/emacs_modernization.html
; ; http://ergoemacs.org/emacs/elisp.html
; ; https://www.gnu.org/software/emacs/manual/html_node/eintr/
; ; http://bzg.fr/learn-emacs-lisp-in-15-minutes.html
; ; http://www.haskell.org/haskellwiki/Emacs/Keybindings_and_simple_usage
; ; http://haskell.github.io/haskell-mode/manual/latest/
; ; http://www.haskell.org/haskellwiki/Emacs/Inferior_Haskell_processes
; ; http://www.emacswiki.org/emacs/MacOSTweaks
; ; http://paste.lisp.org/display/132476
; ; http://www.gnu.org/software/emacs/manual/html_node/eintr/index.html
; ; What is smex and how it helps me? http://www.emacswiki.org/emacs/Smex
; ; Setting proper frame size when emacs starts http://www.emacswiki.org/emacs/FrameSize#toc1
; ;   Maybe also check better settings for default font size (and possibly keybinding zoom in/out ?)
; ; !!! Yes or No p http://www.emacswiki.org/emacs/YesOrNoP
; ; IDO http://www.emacswiki.org/emacs/InteractivelyDoThings
; ; more ido http://www.emacswiki.org/emacs/InteractivelyDoThings#toc8
; ; ido hacks https://github.com/scottjad/ido-hacks/blob/master/ido-hacks.el
; ; watch emacs screencasts http://emacsrocks.com/
; ; http://stackoverflow.com/questions/14836958/updating-packages-in-emacs
; ; http://ergoemacs.org/emacs/emacs_package_system.html
; ; xmpfilter in VIM http://justincampbell.me/til/annotate-ruby-code-in-vim-with-xmpfilter
; ; melpa http://melpa.milkbox.net/#/getting-started
; ; check out el-get vs elpa vs melpa vs marmelade
; ; http://www.gnu.org/software/emacs/tour/
; ; gnu hurd - check it out, see what it does, can it be used? how?
; ;   http://www.gnu.org/software/hurd/hurd.html
; ;   http://www.gnu.org/software/hurd/hurd/what_is_the_gnu_hurd.html
; ;   http://thread.gmane.org/gmane.os.hurd.bugs/18777
; ;   http://darnassus.sceen.net/gitweb/hurd-web.git/commitdiff/ae16fea583613b94785c262b764a1b68cf722ded
; ; PRO emacs config http://www.cyber.com.au/~twb/.emacs
; ; recursive editing levels http://www.gnu.org/software/emacs/manual/html_node/emacs/Recursive-Edit.html
; ; C-h i
; ; check out gnu homepage for some more cool stuff http://www.gnu.org/home.en.html
; ; emacs as word processor - read the whole thread https://lists.gnu.org/archive/html/emacs-devel/2013-11/msg00594.html
;
; ; My old config https://gist.github.com/darthdeus/d645f9e87db4cb8a9721
;
; (custom-set-faces
;  ;; custom-set-faces was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
;  '(company-preview ((t (:foreground "wheat"))))
;  '(company-preview-common ((t (:inherit company-preview :foreground "gray40"))))
;  '(company-scrollbar-bg ((t (:inherit company-tooltip :background "gray20"))))
;  '(company-scrollbar-fg ((t (:background "LightSkyBlue2"))))
;  '(company-tooltip ((t (:background "gray16"))))
;  '(company-tooltip-common ((t (:inherit company-tooltip :foreground "SlateGray3"))))
;  '(company-tooltip-common-selection ((t (:inherit company-tooltip-common :background "gray28" :foreground "SlateGray2"))))
;  '(company-tooltip-selection ((t (:inherit company-tooltip :background "gray28"))))
;  '(fringe ((t (:background "#282a2e"))))
;  '(ghc-face-error ((t (:underline "gray36"))))
;  '(ghc-face-warn ((t (:underline "DarkGoldenrod4"))))
;  '(idris-loaded-region-face ((t (:background "gray8"))) t)
;  '(mode-line ((t (:background "#282a2e" :foreground "gray39" :box (:line-width 2 :color "#282a2e") :weight normal))))
;  '(mode-line-buffer-id ((t (:foreground "#655969"))))
;  '(mode-line-highlight ((t (:foreground "#655969" :box nil :weight bold))))
;  '(mode-line-inactive ((t (:inherit mode-line :background "#282a2e" :foreground "gray39" :box (:line-width 2 :color "#282a2e") :weight normal))))
;  '(neo-button-face ((t (:foreground "SlateGray3" :underline nil))) t)
;  '(neo-dir-link-face ((t (:foreground "cornflower blue"))) t)
;  '(neo-file-link-face ((t (:foreground "gray83"))) t)
;  '(neo-root-dir-face ((t (:foreground "SlateGray3" :weight bold))) t)
;  '(shm-current-face ((t (:background "#efefef"))) t)
;  '(shm-quarantine-face ((t (:inherit font-lock-error))) t))
;
; (custom-set-variables
;  ;; custom-set-variables was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
;  '(ansi-term-color-vector
;    [unspecified "#181818" "#ab4642" "#a1b56c" "#f7ca88" "#7cafc2" "#ba8baf" "#7cafc2" "#d8d8d8"] t)
;  '(coq-load-path (quote ("src")))
;  '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
;  '(custom-safe-themes
;    (quote
;     ("cc495c40747ae22dd2de6e250cbd9a408e3588b59989368af565deeeff334126" "75c0b1d2528f1bce72f53344939da57e290aa34bea79f3a1ee19d6808cb55149" "9f3a4edb56d094366afed2a9ba3311bbced0f32ca44a47a765d8ef4ce5b8e4ea" "7bde52fdac7ac54d00f3d4c559f2f7aa899311655e7eb20ec5491f3b5c533fe8" "c56d90af9285708041991bbc780a073182cbe91721de17c6e7a8aac1380336b3" "8022cea21aa4daca569aee5c1b875fbb3f3248a5debc6fc8cf5833f2936fbb22" "a0fdc9976885513b03b000b57ddde04621d94c3a08f3042d1f6e2dbc336d25c7" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "e80932ca56b0f109f8545576531d3fc79487ca35a9a9693b62bf30d6d08c9aaf" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
;  '(evil-shift-width 4)
;  '(face-font-family-alternatives
;    (quote
;     (("Monospace" "courier" "fixed")
;      ("courier" "CMU Typewriter Text" "fixed")
;      ("Sans Serif" "helv" "helvetica" "arial" "fixed")
;      ("helv" "helvetica" "arial" "fixed"))))
;  '(haskell-complete-module-preferred
;    (quote
;     ("Data.ByteString" "Data.ByteString.Lazy" "Data.Conduit" "Data.Function" "Data.List" "Data.Map" "Data.Maybe" "Data.Monoid" "Data.Ord")))
;  '(haskell-interactive-mode-eval-mode (quote haskell-mode))
;  '(haskell-interactive-mode-eval-pretty nil)
;  '(haskell-interactive-mode-include-file-name nil)
;  '(haskell-interactive-popup-errors nil)
;  '(haskell-mode-contextual-import-completion nil)
;  '(haskell-notify-p t)
;  '(haskell-process-auto-import-loaded-modules t)
;  '(haskell-process-generate-tags nil)
;  '(haskell-process-log t)
;  '(haskell-process-reload-with-fbytecode nil)
;  '(haskell-process-suggest-haskell-docs-imports t)
;  '(haskell-process-suggest-remove-import-lines nil)
;  '(haskell-process-type (quote cabal-repl))
;  '(haskell-process-use-presentation-mode t)
;  '(haskell-stylish-on-save nil)
;  '(haskell-tags-on-save nil)
;  '(hindent-style "gibiansky")
;  '(linum-format " %5i ")
;  '(neo-theme (quote arrow))
;  '(package-selected-packages
;    (quote
;     (magit gist hi2 hindent ghc haskell-mode dirtree color-theme-sanityinc-tomorrow smex ido-ubiquitous flx-ido flx sequential-command projectile grizzl company evil-paredit evil-surround evil clj-refactor paredit rainbow-delimiters cider clojure-mode)))
;  '(safe-local-variable-values
;    (quote
;     ((haskell-indent-spaces . 4)
;      (hamlet/basic-offset . 4)
;      (haskell-process-use-ghci . t)
;      (haskell-process-type . cabal-repl))))
;  '(shell-file-name "/bin/bash")
;  '(shm-auto-insert-bangs t)
;  '(shm-auto-insert-skeletons t)
;  '(shm-use-presentation-mode t)
;  '(truncate-lines t)
;  '(when (not (facep (aref ansi-term-color-vector 0)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit rainbow-mode ido-completing-read+ amx avy smex ido-vertical-mode org-bullets spacemacs-theme beacon base16-theme which-key use-package evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
