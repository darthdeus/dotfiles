;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Jakub Arnold"
      user-mail-address "darthdeus@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;

(if (eq system-type 'darwin)
    (setq doom-font (font-spec :family "Fantasque Sans Mono" :size 18 :weight 'semi-light)
          doom-variable-pitch-font (font-spec :family "Fantasque Sans Mono" :size 16))
  (setq doom-font (font-spec :family "Fantasque Sans Mono" :size 22 :weight 'semi-light)
        doom-variable-pitch-font (font-spec :family "Fantasque Sans Mono" :size 20)))


;; (setq doom-font (font-spec :family "Iosevka" :size 20 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "Iosevka" :size 18))

;; (setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))

(setq-default line-spacing 0)

;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; Sets ',' as the <localleader>
;; https://github.com/hlissner/doom-emacs/issues/4242#issuecomment-724378708
(setq evil-snipe-override-evil-repeat-keys nil)
(setq doom-localleader-key ",")
(setq doom-localleader-alt-key "M-,")

;; Auto-guess fails for cargo workspace projects so we don't want it. This is
;; the default, but I'll still keep it here in case the default ever changes.
(setq! lsp-auto-guess-root nil)

;; (def-package! slime
;;   :defer t ; don't load the package immediately
;;   :init ; runs this immediately
;;   (setq inferior-lisp-program "sbcl")
;;   :config ; runs this when slime loads
;;   (set-repl-handler! 'lisp-mode #'sly-mrepl)
;;   (set-eval-handler! 'lisp-mode #'sly-eval-region)
;;   (set-lookup-handlers! 'lisp-mode
;;     :definition #'sly-edit-definition
;;     :documentation #'sly-describe-symbol)
;;
;;
;;   )

;; My custom paredit keybindings
(defun my-sexp-register-paredit-keybindings ()
  (progn
    (local-set-key (kbd "M-w") 'sp-forward-sexp)
    (local-set-key (kbd "M-b") 'sp-backward-sexp)
    (local-set-key (kbd "M-r") 'sp-raise-sexp)
    (local-set-key (kbd "M-\"") 'sp-splice-sexp)
    (local-set-key (kbd "M-L") 'sp-forward-slurp-sexp)
    (local-set-key (kbd "M-H") 'sp-backward-slurp-sexp)
    (local-set-key (kbd "M-J") 'sp-backward-barf-sexp)
    (local-set-key (kbd "M-K") 'sp-forward-barf-sexp)
    (local-set-key (kbd "M-(") (lambda () (interactive) (sp-wrap-with-pair "(")))
    ;; (local-set-key (kbd "M-[") (lambda () (interactive) (sp-wrap-with-pair "[")))
    ;; (local-set-key (kbd "M-{") (lambda () (interactive) (sp-wrap-with-pair "{")))

    ;; NOTE: Needed because doom registers this as well.
    (define-key evil-normal-state-map (kbd "C-<tab>") nil)
    (local-set-key (kbd "C-<tab>") 'sp-indent-defun)
    (rainbow-delimiters-mode 1)
    (evil-smartparens-mode 1)
    (aggressive-indent-mode 1)))

(add-hook 'lisp-mode-hook 'my-sexp-register-paredit-keybindings)
(add-hook 'emacs-lisp-mode-hook 'my-sexp-register-paredit-keybindings)
(add-hook 'clojure-mode-hook 'my-sexp-register-paredit-keybindings)
(add-hook 'clojurescript-mode-hook 'my-sexp-register-paredit-keybindings)
(add-hook 'fennel-mode-hook 'my-sexp-register-paredit-keybindings)
(add-hook 'racket-mode-hook 'my-sexp-register-paredit-keybindings)

(setq cider-ns-refresh-before-fn "user/stop-system!"
      cider-ns-refresh-after-fn "user/start-system!")

(setq inferior-lisp-program "ros -Q run")

;; (use-package! sly (evil-lisp))
;; (use-package! slime)

(defun clerk-show ()
  (interactive)
  (when-let
      ((filename
        (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))

; (define-key clojure-mode-map (kbd "<M-RET>") 'clerk-show)

(map! :localleader
      :map clojure-mode-map
      :desc "Run clerk-show"
      "<M-RET>" 'clerk-show)

(map! :localleader
      :map clojurescript-mode-map
      "p e" 'sp-unwrap-sexp)

(map! :localleader
      :map clojurescript-mode-map
      "p r" 'sp-rewrap-sexp)

(map! :localleader
      :map lisp-mode-map
      :desc "Start sly and load ASDF system"
      "q" (lambda () (interactive)
            (call-interactively 'sly-restart-inferior-lisp)
            (call-interactively 'sly-asdf-load-system my-asdf-system-name)))


(map! :localleader
      :map lisp-mode-map
      :desc "Eval buffer"
      "w" (lambda () (interactive) (call-interactively 'sly-eval-buffer)))

(map! :localleader
      :map clojure-mode-map
      :desc "Eval defun at mark"
      "e f" (lambda () (interactive)
              (call-interactively 'my-cider-eval-at-mark))

      :desc "Set eval mark"
      "e m" (lambda () (interactive)
              (call-interactively 'my-cider-mark-eval-point)))

;; A custom shortcut to mark and eval a region of code
;; to eval from anywhere on the same file.
(defvar my-cider-mark-char 248)

(defun my-cider-mark-eval-point ()
  (interactive)
  (evil-set-marker my-cider-mark-char))

(defun my-cider-eval-at-mark ()
  (interactive)
  (cider-eval-defun-at-point)
  (save-excursion
    (evil-goto-mark-line my-cider-mark-char)
    ;;(cider-eval-last-sexp)
    (cider-eval-defun-at-point)))

;; -- Keybindings --
(map! :localleader
      :map rust-mode-map

      ;; :desc "Go to references"
      ;; "g r" (lambda () (interactive)
      ;;         (call-interactively 'lsp-find-references))

      ;; :desc "Go to definition"
      ;; "g d" (lambda () (interactive)
      ;;         (call-interactively 'lsp-find-definition))

      ;; :desc "Go to type definition"
      ;; "g t" (lambda () (interactive)
      ;;         (call-interactively 'lsp-goto-type-definition))

      ;; :desc "Open error list"
      ;; "e l" (lambda () (interactive) (lsp-treemacs-errors-list))

      ;; :desc "Next error"
      ;; "e n" (lambda () (interactive) (next-error))

      ;; :desc "Previous error"
      ;; "e N" (lambda () (interactive) (previous-error))

      :desc "Code actions"
      "c a" (lambda () (interactive)
              (call-interactively 'lsp-execute-code-action))

      ;; :desc "Go to type definition"
      ;; "g t" (lambda () (interactive)
      ;;         (call-interactively 'lsp-goto-type-definition))

      :desc "Go to implementation"
      "g i" (lambda () (interactive)
              (call-interactively 'lsp-goto-implementation))

      :desc "Rename symbol"
      "r r" (lambda () (interactive)
              (call-interactively 'lsp-rename)))

;; :desc "Format buffer"
;; "b f" (lambda () (interactive)
;;         (call-interactively 'lsp-format-buffer))


(after! company
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0))

(setq display-line-numbers-type nil)

(setq scroll-margin 5)

(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

;; (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
;; (define-key evil-visual-state-map (kbd "C-c") 'evil-normal-state)
;; (define-key evil-visual-state-map (kbd "C-c") 'evil-exit-visual-state)

(map! :nvi "C-c" #'evil-normal-state)
(map! :nv "C-_ C-_" #'comment-line)
(map! :nvi "C-x C-s" #'evil-write)

(map! :n ",," #'evil-buffer)
(map! :n ",f" #'projectile-find-file)

(map! :i "C-d" #'paredit-forward-delete)
(map! :i "M-d" #'paredit-forward-kill-word)
(map! :i "C-<backspace>" #'paredit-backward-kill-word)
(map! :i "M-<backspace>" #'paredit-backward-kill-word)
;; (map! :n :mode 'org-mode ",f" #'projectile-find-file)

(after! org (map! :map org-mode-map :n ",f" #'projectile-find-file))
(after! json (map! :map json-mode-map :n ",f" #'projectile-find-file))
(after! sly-mrepl (map! :map sly-mrepl-mode-map :n "C-k" #'evil-window-up))
(after! sly-mrepl (map! :map sly-mrepl-mode-map :n "C-j" #'evil-window-down))

;; (map! :ni "C-j" 'evil-window-down)
;; (map! :ni "C-k" 'evil-window-up)

;; (map! :n ",b" #'+vertico/switch-workspace-buffer)
(map! :n ",b" #'switch-to-buffer)
(map! :n ",gt" #'consult-lsp-symbols)
(map! :n ",ga" #'+default/search-project)
(map! :n ",gd" #'+default/search-project-for-symbol-at-point)

;; (map! :n "gd" #'+default/search-project-for-symbol-at-point)
;;       :desc "Go to references"
;;       "g r" (lambda () (interactive)
;;               (call-interactively 'lsp-find-references))

;;       :desc "Go to definition"
;;       "g d" (lambda () (interactive)
;;               (call-interactively 'lsp-find-definition))

(map! :n "[d" #'flycheck-previous-error)
(map! :n "]d" #'flycheck-next-error)

(map! :vn "-" #'+format/region-or-buffer)

;; (map! :n "L" (lambda () (interactive)
;;                (call-interactively 'lsp-execute-code-action)))

(map! :n "L" #'lsp-execute-code-action)

(map! :n ",e" (lambda () (interactive)
                (call-interactively 'projectile-run-project "cargo run")))


;; (define-key evil-visual-state-map (kbd "C-x C-s") 'evil-write)

;; (define-key evil-normal-state-map (kbd ",,") 'evil-buffer)
;; (define-key evil-normal-state-map (kbd ",f") 'projectile-find-file)

;; Make C-e, C-d, C-k behave same as in Emacs when in insert mode.
;; (define-key evil-insert-state-map (kbd "C-e") nil)
;; (define-key evil-insert-state-map (kbd "C-d") nil)
;; (define-key evil-insert-state-map (kbd "C-k") nil)

(map! :ni "C-e" 'doom/forward-to-last-non-comment-or-eol)
(map! :ni "C-d" nil)
(map! :ni "C-k" nil)
(map! :ni "C-n" nil)
(map! :ni "C-p" nil)

(after! smartparens
  (sp-pair "(" nil :unless '(:rem sp-point-before-word-p)))

;; Switching between windows with C-hjkl
(map! :ni "C-l" 'evil-window-right)
(map! :ni "C-h" 'evil-window-left)
(map! :ni "C-j" 'evil-window-down)
(map! :ni "C-k" 'evil-window-up)

;; (define-key evil-normal-state-map (kbd "C-x h k") 'describe-key)
;; (define-key evil-normal-state-map (kbd "C-x h f") 'describe-function)
;; (define-key evil-normal-state-map (kbd "C-x h v") 'describe-variable)

                                        ; (define-key evil-normal-state-map (kbd "C-d") 'scroll-up-command)
                                        ; (define-key evil-normal-state-map (kbd "C-u") 'scroll-down-command)

;; Insert mode as well
;; Makes C-e behave same as in Emacs. C-a works out of the box
;; (define-key evil-motion-state-map (kbd "C-e") nil)

(map! :n "<left>"
      (lambda ()
        (interactive)
        (call-interactively 'evil-window-decrease-width)
        (call-interactively 'evil-window-decrease-width)
        (call-interactively 'evil-window-decrease-width)
        (call-interactively 'evil-window-decrease-width)
        (call-interactively 'evil-window-decrease-width)))

(map! :n "<right>"
      (lambda ()
        (interactive)
        (call-interactively 'evil-window-increase-width)
        (call-interactively 'evil-window-increase-width)
        (call-interactively 'evil-window-increase-width)
        (call-interactively 'evil-window-increase-width)
        (call-interactively 'evil-window-increase-width)))

(map! :n "<up>"
      (lambda ()
        (interactive)
        (call-interactively 'evil-window-decrease-height)
        (call-interactively 'evil-window-decrease-height)
        (call-interactively 'evil-window-decrease-height)
        (call-interactively 'evil-window-decrease-height)
        (call-interactively 'evil-window-decrease-height)))

(map! :n "<down>"
      (lambda ()
        (interactive)
        (call-interactively 'evil-window-increase-height)
        (call-interactively 'evil-window-increase-height)
        (call-interactively 'evil-window-increase-height)
        (call-interactively 'evil-window-increase-height)
        (call-interactively 'evil-window-increase-height)))


                                        ; (defadvice! hy/evil-scroll-advice (fn count)
                                        ;   :around #'evil-scroll-down
                                        ;   :around #'evil-scroll-up
                                        ;   (setq count (/ (window-body-height) 8))
                                        ;   (funcall fn count))

(setq which-key-idle-delay 0.25)

(use-package! lsp
  :config
  ;; NOTE: I monkey-patched this function because I don't like the
  ;; default behavior of auto executing the first suggestion when
  ;; there's a single one.
  (defun lsp--select-action (actions)
    "Select an action to execute from ACTIONS."
    (cond
     ((seq-empty-p actions) (signal 'lsp-no-code-actions nil))
     ;; NOTE: Here's the commented-out code
     ;;((and (eq (seq-length actions) 1) lsp-auto-execute-action)
     ;;(lsp-seq-first actions))
     (t (let ((completion-ignore-case t))
          (lsp--completing-read "Select code action: "
                                (seq-into actions 'list)
                                (-compose (lsp--create-unique-string-fn)
                                          #'lsp:code-action-title)
                                nil t))))))

;;;;;;;;;;;;;;;;;;
;; Lua settings ;;
;;;;;;;;;;;;;;;;;;

(defun call-stylua-on-current-buffer ()
  (interactive)
  (let ((original-point (point)))
    (shell-command-on-region
     (point-min)
     (point-max)
     "~/.cargo/bin/stylua --search-parent-directories -"
     (current-buffer))
    (set-window-point (selected-window) original-point)))

(map! :localleader
      :map lua-mode-map

      :desc "Format buffer"
      "f b" (lambda () (interactive)
              (call-interactively 'call-stylua-on-current-buffer)))

;;;;;;;;;;;;;;;;;;;
;; GLSL settings ;;
;;;;;;;;;;;;;;;;;;;

(defun call-clang-format-on-current-buffer ()
  (interactive)
  (let ((original-point (point)))
    (shell-command-on-region
     (point-min)
     (point-max)
     "clang-format -"
     (current-buffer))
    (set-window-point (selected-window) original-point)))

(map! :localleader
      :map glsl-mode-map

      :desc "Format buffer"
      "-" (lambda () (interactive)
            (call-interactively 'call-clang-format-on-current-buffer)))

;; Prevents huge minibuffer popup when writing
(setq! lsp-signature-render-documentation 't)
(setq! lsp-signature-doc-lines 2)
