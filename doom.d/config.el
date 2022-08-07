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
(setq doom-font (font-spec :family "Fantasque Sans Mono" :size 22 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Fantasque Sans Mono" :size 20))
; (setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;       doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
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

;; -- Keybindings --

(map! :localleader
      :map rust-mode-map

      :desc "Go to references"
      "g r" (lambda () (interactive)
              (call-interactively 'lsp-find-references))

      :desc "Go to definition"
      "g d" (lambda () (interactive)
              (call-interactively 'lsp-find-definition))

      :desc "Go to type definition"
      "g t" (lambda () (interactive)
              (call-interactively 'lsp-goto-type-definition))

      :desc "Open error list"
      "e l" (lambda () (interactive) (lsp-treemacs-errors-list))

      :desc "Next error"
      "e n" (lambda () (interactive) (next-error))

      :desc "Previous error"
      "e N" (lambda () (interactive) (previous-error))

      :desc "Code actions"
      "c a" (lambda () (interactive)
              (call-interactively 'lsp-execute-code-action))

      :desc "Go to references"
      "g r" (lambda () (interactive)
              (call-interactively 'lsp-find-references))
      :desc "Go to definition"
      "g d" (lambda () (interactive)
              (call-interactively 'lsp-find-definition))

      :desc "Go to type definition"
      "g t" (lambda () (interactive)
              (call-interactively 'lsp-goto-type-definition))

      :desc "Go to implementation"
      "g i" (lambda () (interactive)
              (call-interactively 'lsp-goto-implementation))

      :desc "Rename symbol"
      "r r" (lambda () (interactive)
              (call-interactively 'lsp-rename))

      :desc "Format buffer"
      "b f" (lambda () (interactive)
              (call-interactively 'lsp-format-buffer)))

(after! company
    (setq company-dabbrev-downcase 0)
    (setq company-idle-delay 0))

(setq display-line-numbers-type nil)

(setq scroll-margin 5)

(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "C-c") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "C-c") 'evil-exit-visual-state)

(define-key evil-normal-state-map (kbd ",,") 'evil-buffer)
(define-key evil-normal-state-map (kbd ",f") 'projectile-find-file)

;; Make C-e, C-d, C-k behave same as in Emacs when in insert mode.
(define-key evil-insert-state-map (kbd "C-e") nil)
(define-key evil-insert-state-map (kbd "C-d") nil)
(define-key evil-insert-state-map (kbd "C-k") nil)

;; Makes C-e behave same as in Emacs. C-a works out of the box
(define-key evil-motion-state-map (kbd "C-e") nil)

(define-key evil-normal-state-map (kbd "<left>") (lambda () (interactive)
                                                (call-interactively 'evil-window-decrease-width)
                                                (call-interactively 'evil-window-decrease-width)
                                                (call-interactively 'evil-window-decrease-width)
                                                (call-interactively 'evil-window-decrease-width)
                                                (call-interactively 'evil-window-decrease-width)
                                                ))

(define-key evil-normal-state-map (kbd "<right>") (lambda () (interactive)
                                                (call-interactively 'evil-window-increase-width)
                                                (call-interactively 'evil-window-increase-width)
                                                (call-interactively 'evil-window-increase-width)
                                                (call-interactively 'evil-window-increase-width)
                                                (call-interactively 'evil-window-increase-width)
                                                ))


(define-key evil-normal-state-map (kbd "<up>") (lambda () (interactive)
                                                (call-interactively 'evil-window-decrease-height)
                                                (call-interactively 'evil-window-decrease-height)
                                                (call-interactively 'evil-window-decrease-height)
                                                (call-interactively 'evil-window-decrease-height)
                                                (call-interactively 'evil-window-decrease-height)
                                                ))

(define-key evil-normal-state-map (kbd "<down>") (lambda () (interactive)
                                                (call-interactively 'evil-window-increase-height)
                                                (call-interactively 'evil-window-increase-height)
                                                (call-interactively 'evil-window-increase-height)
                                                (call-interactively 'evil-window-increase-height)
                                                (call-interactively 'evil-window-increase-height)
                                                ))


;; Switching between windows with C-hjkl
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)

(define-key evil-normal-state-map (kbd "C-x h k") 'describe-key)
(define-key evil-normal-state-map (kbd "C-x h f") 'describe-function)
(define-key evil-normal-state-map (kbd "C-x h v") 'describe-variable)

; (define-key evil-normal-state-map (kbd "C-d") 'scroll-up-command)
; (define-key evil-normal-state-map (kbd "C-u") 'scroll-down-command)

;; Insert mode as well
(define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)

(define-key evil-insert-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-insert-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-insert-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-insert-state-map (kbd "C-k") 'evil-window-up)

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

;; Prevents huge minibuffer popup when writing
(setq! lsp-signature-render-documentation 't)
(setq! lsp-signature-doc-lines 2)
