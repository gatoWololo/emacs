(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; Issues with certificate.
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)



(use-package hide-mode-line
  :ensure t
  )

; Install 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))

(use-package diminish
  :ensure t)

(use-package ob-rust
  :ensure t)

(use-package projectile
  :ensure t
  :diminish projectile-mode)

(use-package autorevert
  :diminish auto-revert-mode
  )

(use-package eldoc
  :diminish eldoc-mode
  )

(setq compilation-read-command nil)

;; Autocompletion engine used by ivy/counsel/?swiper?
(use-package amx
  :ensure t
  :config
  (amx-mode))

;; Counsel: a collection of Ivy-enhanced versions of common Emacs commands
;; Ivy: a generic completion mechanism for Emacs
;; Swiper: an Ivy-enhanced alternative to isearch
(use-package counsel
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-count-format "")
  :custom
  (ivy-use-virtual-buffers t)
  ;; (ivy-initial-inputs-alist nil "No ^ when ivy searching.")
  )

 (use-package org
  :init
  ;; Spell checking for org mode.
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'company-mode)
  :config
  (setq org-todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "SOMEDAY(s)" "ASSIGNED(a@)" "|" "WONT_DO(w)" "DONE(d)")))
  )


;; (use-package flyspell
;;   :init
;;   ;; (define-key flyspell-mouse-map (kbd "<mouse-3>") #'flyspell-correct-word)
;;   :custom
;;   (ispell-program-name "hunspell")
;;   (ispell-local-dictionary "en_US")
;;   (ispell-hunspell-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
;;   :diminish flyspell-mode
;;   )

(use-package markdown-mode
  :init
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  )

(use-package auctex
  :defer t
  :ensure t
  :init
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  )

(use-package magit
  :defer t
  :ensure t)

;; Rust racer
(use-package racer
  :defer t
  :ensure t
  :diminish racer-mode
  )
;; TODO Multicompile

;; Python
(use-package elpy
  :ensure t
  :config (elpy-enable)
  :init (add-hook 'python-mode-hook 'elpy-mode)
  :defer t
  :custom
  (elpy-rpc-python-command "python3")
  :diminish elpy-mode
  )

(use-package company
  :ensure t
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  ;; (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'company-mode)
  (add-hook 'rust-mode-hook 'company-mode)
  ;; (add-hook 'c++-mode-hook
  ;;           (lambda () (setq company-clang-arguments "-std=c++14")))
  :config
  (setq company-idle-delay 0.5
        company-tooltip-idle-delay 0.5
        company-minimum-prefix-length 3
        ;; company-backends '(company-capf company-files
                                        ;; (company-dabbrev company-ispell) company-keywords)
        ;; Case sensitive autocompletion!
        company-dabbrev-downcase nil
        company-tooltip-align-annotations t)
  ;; (add-hook 'c++-mode-hook
            ;; (lambda ()
            ;;   (add-to-list (make-local-variable 'company-backends)
            ;;                '(company-irony-c-headers company-irony))))
  :diminish company-mode
  )

(use-package rust-mode
  :ensure t
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'rust-mode-hook #'racer-mode)
  ;; (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)

  ;; :config
  ;; (add-hook 'rust-mode-hook #'lsp)
  :diminish rust-mode)

(use-package cargo
  :ensure t
  :diminish cargo-minor-mode)

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme)
 )

(use-package which-key
  :ensure t
  :init
  ;; Allow us to give custom names to define-key arguments.
  (setq which-key-enable-extended-define-key t)
  :config
  (which-key-mode)
  :diminish which-key-mode)

;; Have ido-not automatically search for file in other directories.
(setq ido-auto-merge-work-directories-length -1)

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(defun nothing ()
  (interactive))

;; This way of doing it doesn't really work too well as I cannot have per buffer
;; binding defined per package in their declaration.
(defun my-xah-bindings ()
  "My keybinding for xah fly keys."

  ;; Main key bindings.
  (define-key xah-fly-key-map (kbd "<f3>") 'save-buffer)
  ;; Figure out what to bind.
  (define-key xah-fly-key-map (kbd "<f4>") 'ido-find-file)
  (define-key xah-fly-key-map (kbd "<f5>") 'nothing)
  (define-key xah-fly-key-map (kbd "<f6>") 'xah-close-current-buffer)
  (define-key xah-fly-key-map (kbd "<f8>") 'nothing)
  (define-key xah-fly-key-map (kbd "<f7>") 'xah-fly-command-mode-activate)
  (define-key xah-fly-key-map (kbd "<home>") 'xah-fly-command-mode-activate)
  ;; (define-key xah-fly-key-map (kbd "<f6>") 'man)
  ;; (define-key xah-fly-key-map (kbd "<f7>") 'man)
  ;; (define-key xah-fly-key-map (kbd "<f10>") 'man)
  ;; (define-key xah-fly-key-map (kbd "<f11>") 'man)
  ;; (define-key xah-fly-key-map (kbd "<f12>") 'man)

  (define-key xah-fly-key-map (kbd "/") 'man)
  (define-key xah-fly-key-map (kbd "DEL") 'delete-backward-char)
  (define-key xah-fly-key-map (kbd "d") 'delete-forward-char)
  (define-key xah-fly-key-map (kbd "g") 'kill-line)
  (define-key xah-fly-key-map (kbd "s") 'counsel-ibuffer)
  (define-key xah-fly-key-map (kbd "p") 'dired)
  ;; amx will rebind execute-extended-command to point to amx command.
  (define-key xah-fly-key-map (kbd "a") 'nothing)
  (define-key xah-fly-key-map (kbd "4") 'split-window-right)
  (define-key xah-fly-key-map (kbd "5") 'counsel-M-x)
  (define-key xah-fly-key-map (kbd "w") 'recenter-top-bottom)
  (define-key xah-fly-key-map (kbd "n") 'swiper)
  (define-key xah-fly-key-map (kbd "0") 'delete-window)

  ;; Get rid of old open and close buffer mappings.
  (define-key xah-fly-t-keymap (kbd "j") 'nothing)
  (define-key xah-fly-c-keymap (kbd "e") 'nothing)

  (define-key xah-fly-key-map "." '("jump-to-symbol-definition" . per-mode-dot-keybindings))

  ;; Usually paste, but when in term mode, we want to use special paste.
  (define-key xah-fly-key-map (kbd "v") 'per-mode-v-keybindings)
  (define-key xah-fly-key-map (kbd "b") 'my-org-mode-keymap)
  ;; Needed since we want our hook called.
  (define-key xah-fly-key-map (kbd "M-SPC") 'nothing)
  ;; (define-key xah-fly-key-map (kbd "f") 'test)
  ;; All these keys need to be bound to something useful.
  ;; (define-key xah-fly-key-map (kbd "q") 'TODO)

  (define-key xah-fly-key-map "m" '("mode specific bindings" . per-mode-keybindings))
  ;; (define-key  (kbd "c-d") (lambda () (message "heello")))

  ;; Space + Key
  (define-key xah-fly-leader-key-map (kbd "4") 'split-window-below)
  (define-key xah-fly-leader-key-map (kbd "m") 'magit-status)
  ;; Y and M to go to top and bottom.
  (define-key xah-fly-leader-key-map (kbd "y") 'beginning-of-buffer)
  (define-key xah-fly-leader-key-map (kbd "h") 'end-of-buffer)
  ;; Counter part to ivy search with just "n"
  (define-key xah-fly-leader-key-map (kbd "n") 'ivy-resume)
  (define-key xah-fly-leader-key-map (kbd "l") 'goto-line)
  (define-key xah-fly-leader-key-map (kbd "g") 'omar-rg)
  (define-key xah-fly-leader-key-map (kbd "j") 'nothing)
  (define-key xah-fly-leader-key-map (kbd ";") 'nothing)
  (define-key xah-fly-leader-key-map (kbd ".") 'per-mode-spc-dot-keybindings)
  (define-key xah-fly-leader-key-map (kbd "t")
    '(lambda ()  (interactive) (ansi-term "/usr/bin/fish")))
  )


;; We use | as a special symbol to get my keybindings to work.
;; So here we clear it out for use in key insert mode.
(defun my-xah-insert-mode ()
  (define-key xah-fly-key-map (kbd "|") nil)
  )

;; Special key bindings for pressing v "paste" since for a terminal,
;; we need to use term-paste instead.
(defun per-mode-v-keybindings ()
  (interactive)
  (cond
   ((eq major-mode 'term-mode)
    (term-paste))
   (t
    (xah-paste-or-paste-previous))))


(defun per-mode-spc-dot-keybindings ()
  (interactive)
  (cond
   ;; Rust
   ((eq major-mode 'rust-mode)
    (call-interactively 'racer-find-definition-other-window))

   ;; Else...
   (t
    (setq this-command 'xref-find-definitions-other-window)
    (call-interactively 'xref-find-definitions-other-window)
    )))

(defun per-mode-dot-keybindings ()
  (interactive)
  (cond
   ;; Rust
   ((eq major-mode 'rust-mode)
    (call-interactively 'racer-find-definition))

   ;; Else...
   (t
    (setq this-command 'xref-find-definitions)
    (call-interactively 'xref-find-definitions)
    )))

;; Each mode based key binding must be a key map which will attached to "m" for modes :)
(defun per-mode-keybindings ()
  (interactive)
  (cond
   ((eq major-mode 'conf-toml-mode)
    (use-map 'my-rust-mode-keymap))

   ((eq major-mode 'rust-mode)
    (use-map 'my-rust-mode-keymap))

   ;; TODO I don't now what this is: Programming modes?
   ((derived-mode-p 'prog-mode)
    (use-map 'my-prog-mode-keymap))))

(progn
  (define-prefix-command 'my-rust-mode-keymap)
  (define-key my-rust-mode-keymap (kbd "b") 'cargo-process-build)
  (define-key my-rust-mode-keymap (kbd "t") 'cargo-process-test)
  (define-key my-rust-mode-keymap (kbd "d") 'cargo-process-doc-open)
  (define-key my-rust-mode-keymap (kbd "e") 'cargo-process-check)
  (define-key my-rust-mode-keymap (kbd "o") 'cargo-process-doc-open)
  (define-key my-rust-mode-keymap (kbd "c") 'cargo-process-check-tests)
  (define-key my-rust-mode-keymap (kbd "x") 'cargo-process-check-examples)
  (define-key my-rust-mode-keymap (kbd "s") 'cargo-std-doc)
  )

(defun use-map (keymap)
  (progn
    (define-key xah-fly-key-map (kbd "|") keymap)
    (setq unread-command-events (listify-key-sequence "|"))))

;;; Org Mode commands for my keymap.
(progn
  ;; Define a new keymap and populate it with my bindings.
  (define-prefix-command 'my-org-mode-keymap)
  ;; Bindings:
  (define-key my-org-mode-keymap (kbd "l") 'org-insert-link)
  (define-key my-org-mode-keymap (kbd "a") 'org-agenda)
  (define-key my-org-mode-keymap (kbd "j") 'org-clock-jump-to-current-clock)
  (define-key my-org-mode-keymap (kbd "t") 'org-todo)
  (define-key my-org-mode-keymap (kbd "s") 'org-time-stamp)
  (define-key my-org-mode-keymap (kbd "d") 'org-deadline)
  (define-key my-org-mode-keymap (kbd "i") 'org-clock-in)
  (define-key my-org-mode-keymap (kbd "o") 'org-clock-out)
  (define-key my-org-mode-keymap (kbd "c") 'org-ctrl-c-ctrl-c)
  (define-key my-org-mode-keymap (kbd "r") 'org-archive-subtree)
  (define-key my-org-mode-keymap (kbd "<left>") 'org-promote-subtree)
  (define-key my-org-mode-keymap (kbd "<right>") 'org-demote-subtree)
  )

(use-package xah-fly-keys
  :ensure t
  :init
  (setq xah-fly-use-control-key nil) ; must come before loading xah-fly-keys
  :config
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys 1)
  ;; ;; remove a hook for autosave.
  (remove-hook 'xah-fly-command-mode-activate-hook 'xah-fly-save-buffer-if-file)
  (my-xah-bindings)
  ;; Call on every switch to command mode and on init.
  (add-hook 'xah-fly-command-mode-activate-hook 'my-xah-bindings)
  (add-hook 'xah-fly-insert-mode-activate-hook 'my-xah-insert-mode)

  :diminish xah-fly-keys
  )

;; Only required once! Comment out after setup.
;; (use-package solarized-theme
;;  :ensure t
;;  :config
;;  (custom-set-variables
;;  '(custom-enabled-themes (quote (solarized-light))))
;;  )

;; (use-package irony
;;   :defer t
;;   :ensure t
;;   :init
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'c-mode-hook 'irony-mode)
;;   :config
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;;   :diminish irony
;;   )

;; (use-package company-irony
;;   :defer t
;;   :ensure t
;;   :after (company)
;;   ;; :config
;;   ;; (add-to-list 'company-backends 'company-irony)
;; )


;; Eldoc
;; (add-hook 'c-mode-hook 'eldoc-mode)
;; (add-hook 'c++-mode-hook 'eldoc-mode)


;; (use-package ggtags
;;   :defer t
;;   :ensure t
;;   ;; :init
;;   :config
;;   (add-hook 'c++-mode-hook 'ggtags-mode)
;;   (add-hook 'c-mode-hook 'ggtags-mode)
;;   :diminish ggtags-mode
;;   )

;; Haskell
(setq-default haskell-font-lock-symbols t)

;; My Emacs custom variable settings.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (\` (("." . "~/.saves"))))
 '(column-number-mode t)
 '(company-quickhelp-delay 0.1)
 '(compilation-ask-about-save nil)
 '(compilation-auto-jump-to-first-error t)
 '(compilation-read-command t)
 '(compilation-skip-threshold 2)
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(ediff-diff-options "-w")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(eldoc-idle-delay 0.4)
 '(elpy-rpc-python-command "python3")
 '(gud-tooltip-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ivy-extra-directories (quote ("./")))
 '(ivy-initial-inputs-alist nil)
 '(ivy-use-virtual-buffers nil)
 '(lsp-inhibit-message nil)
 '(org-agenda-files (quote ("~/Mozilla/log.org")))
 '(org-agenda-only-exact-dates t t)
 '(org-confirm-babel-evaluate nil)
 '(org-log-into-drawer t)
 '(package-selected-packages
   (quote
    (smart-mode-line rg wgrep hide-mode-line ob-rust org-present ccls company-lsp lsp-mode ggtags lsp-rust lsp amx fish-mode company-quickhelp counsel haskell-mode markdown-mode cargo use-package xah-fly-keys which-key solarized-theme smex racer powerline magit intero flycheck-rust flycheck-pos-tip flycheck-irony flx-ido elpy diminish company-irony-c-headers clojure-mode better-defaults auctex)))
 '(projectile-completion-system (quote ivy))
 '(projectile-mode t nil (projectile))
 '(racer-rust-src-path
   "/home/gatowololo/.rustup/toolchains/nightly-2019-06-19-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(swiper-include-line-number-in-search nil)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(user-full-name "gatowololo")
 '(user-mail-address "gatowololo@gmail.com"))

;; My Emacs functions for settings.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Ido when searching for files only, I like this better than the way ivy does it.
(ido-mode 1)

;; (ido-everywhere t)
(add-to-list `completion-ignored-extensions ".d")
(scroll-bar-mode -1)
(menu-bar-mode -1)
(electric-pair-mode 1)

;; Dired mode omit hidden files:
(require 'dired-x)
;; TODO Should probably be a toggle.
(setq-default dired-omit-files-p t) ; Buffer-local variable
;; Omit all files starting with "." except "." and ".."
(setq dired-omit-files "^\\.[^.]+")
(add-hook 'dired-mode-hook
          (lambda() (dired-hide-details-mode)))
;; (add-hook 'dired-mode-hook
;;           (lambda() (toggle-truncate-lines)))



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cargo-process--error-face ((t (:inherit error :weight bold))))
 '(org-block-begin-line ((t (:inherit org-meta-line :underline nil))))
 '(org-block-end-line ((t (:inherit org-meta-line :overline nil)))))

;; =============================================================================
;;
;; GDB mode
(setq gdb-show-main 1)
;; Stolen from: https://stackoverflow.com/questions/3860028/customizing-emacs-gdb
(setq gdb-many-windows nil)

(defun set-gdb-layout(&optional c-buffer)
  (if (not c-buffer)
      (setq c-buffer (window-buffer (selected-window)))) ;; save current buffer

  ;; from http://stackoverflow.com/q/39762833/846686
  (set-window-dedicated-p (selected-window) nil) ;; unset dedicate state if needed
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows) ;; clean all

  (let* (
         (w-source (selected-window)) ;; left top
         (w-gdb (split-window w-source nil 'right)) ;; right bottom
         (w-locals (split-window w-gdb nil 'above)) ;; right middle bottom
         (w-stack (split-window w-locals nil 'above)) ;; right middle top
         ;; (w-breakpoints (split-window w-stack nil 'above)) ;; right top
         (w-io (split-window w-source (floor(* 0.9 (window-body-height)))
                             'below)) ;; left bottom
         )
    (set-window-buffer w-io (gdb-get-buffer-create 'gdb-inferior-io))
    (set-window-dedicated-p w-io t)
    ;; (set-window-buffer w-breakpoints (gdb-get-buffer-create 'gdb-breakpoints-buffer))
    ;; (set-window-dedicated-p w-breakpoints t)
    (set-window-buffer w-locals (gdb-get-buffer-create 'gdb-locals-buffer))
    (set-window-dedicated-p w-locals t)
    (set-window-buffer w-stack (gdb-get-buffer-create 'gdb-stack-buffer))
    (set-window-dedicated-p w-stack t)

    (set-window-buffer w-gdb gud-comint-buffer)

    (select-window w-source)
    (set-window-buffer w-source c-buffer)
    ))

(defadvice gdb (around args activate)
  "Change the way to gdb works."
  (setq global-config-editing (current-window-configuration)) ;; to restore: (set-window-configuration c-editing)
  (let (
        (c-buffer (window-buffer (selected-window))) ;; save current buffer
        )
    ad-do-it
    (set-gdb-layout c-buffer))
  )

(defadvice gdb-reset (around args activate)
  "Change the way to gdb exit."
  ad-do-it
  (set-window-configuration global-config-editing))

;; Make text bigger.
(set-face-attribute 'default nil :height 140)

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(global-unset-key (kbd "C-a"))

; Do not highlight whitespce for terminal modes.
(add-hook 'term-mode-hook
          (lambda() (setq show-trailing-whitespace nil)))

(defun term-paste ()
  "Special function to paste in term mode."
  (interactive)
  (term-line-mode)
  (xah-paste-or-paste-previous)
  (term-char-mode)
  )

(defun reformat-lines-80 ( &optional @length)
  "Reformat current text block into 1 long line or multiple short lines.
When there is a text selection, act on the selection, else, act on a text block separated by blank lines.

When the command is called for the first time, it checks the current line's length to decide to go into 1 line or multiple lines. If current line is short, it'll reformat to 1 long lines. And vice versa.

Repeated call toggles between formatting to 1 long line and multiple lines.

If `universal-argument' is called first, use the number value for min length of line. By default, it's 80.

URL `http://ergoemacs.org/emacs/emacs_reformat_lines.html'
Version 2017-10-22"
  (interactive)
  ;; This command symbol has a property “'is-longline-p”, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let* (
         (@length (if @length
                      @length
                    (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 80 )))
         (is-longline-p
          (if (eq last-command this-command)
              (get this-command 'is-longline-p)
            (> (- (line-end-position) (line-beginning-position)) @length)))
         ($blanks-regex "\n[ \t]*\n")
         $p1 $p2
         )
    (if (use-region-p)
        (progn (setq $p1 (region-beginning))
               (setq $p2 (region-end)))
      (save-excursion
        (if (re-search-backward $blanks-regex nil "move")
            (progn (re-search-forward $blanks-regex)
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (if (re-search-forward $blanks-regex nil "move")
            (progn (re-search-backward $blanks-regex)
                   (setq $p2 (point)))
          (setq $p2 (point)))))
    (progn
      (if current-prefix-arg
          (xah-reformat-to-multi-lines $p1 $p2 @length)
        (if is-longline-p
            (xah-reformat-to-multi-lines $p1 $p2 @length)
          (xah-reformat-whitespaces-to-one-space $p1 $p2)))
      (put this-command 'is-longline-p (not is-longline-p)))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; (defun my--advice-irony-start-process (orig-func &rest args)
;;   (let ((shell-file-name "/bin/sh"))
;;     (apply orig-func args)))
;; (advice-add 'irony--start-server-process :around 'my--advice-irony-start-process)

;; (use-package flycheck
  ;; :ensure t)

(use-package ccls
  :ensure t
  :init
  (setq ccls-execution "/home/gatowololo/InstalledPrograms/ccls/Release/ccls")
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp)))
  )
  ;; make sure we have lsp-imenu everywhere we have LSP
  ;; (require 'lsp-imenu)
  ;; (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  ;; todo move to it's own use-package line.
  ;; (add-hook 'programming-mode-hook 'lsp)
  ;; (add-hook 'rust-mode-hook #'lsp)
  ;; :custom
  ;; (lsp-inhibit-message t)
  ;; :custom
  ;; (lsp-auto-configure 'nil)
;; )

;; Do not use on the fly syntax checking.
(setq lsp-prefer-flymake :none)
(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  (add-hook 'c++-mode-hook #'lsp)
  )

(use-package company-lsp
  :ensure t
  :commands company-lsp
  :after (company)
  :init
  (push 'company-lsp company-backends)
  )

(use-package rg
  :ensure t)
;; (use-package lsp-rust
;;   :ensure t
;;   :after lsp-mode)
;;   ;; with-eval-after-load
  ;; :mode ("\\.rs\\'" . lsp-mode)
  ;; :config
  ;; (add-hook 'rust-mode-hook #'lsp-rust-enable)
  ;; (with-eval-after-load 'lsp-mode
  ;;   (setq lsp-rust-rls-command '("rustup" "run" "rls"))
  ;;   (require 'lsp-rust))
  ;; (setq lsp-rust-rls-command '("rustup" "run" "rls"))
  ;; (add-hook 'rust-mode-hook #'lsp-rust)

(defun omar-goto-init ()
  "Open init.el configuratoin file"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun omar-rg ()
  (interactive)
  (if (projectile-project-p)
      ;; Make it interactively query
      (let ((current-prefix-arg '(4)))
        (call-interactively 'rg-project))
    (progn
      (ivy-mode 0)
      (call-interactively 'rg)
      (ivy-mode 1)
      )))

;;; Not provided by cargo, so I make my own.
(defun cargo-process-check-tests ()
  (interactive)
  (cargo-process--start "Check Tests" "check --tests"))

(defun cargo-process-check-examples ()
  (interactive)
  (cargo-process--start "Check Examples" "check --examples"))

(defun cargo-std-doc ()
  (interactive)
  (shell-command "xdg-open file:///home/gatowololo/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/share/doc/rust/html/std/index.html")
  )

(autoload 'org-present "org-present" nil t)
(add-hook 'org-present-mode-hook
          (lambda ()
            (org-present-big)
            (org-display-inline-images)
            (toggle-truncate-lines)
            ;; (delete-window)
            (flyspell-mode-off)
            ;; (org-present-hide-cursor)
            ;; (org-present-read-only)
            ))

(add-hook 'org-present-mode-quit-hook
          (lambda ()
            (org-present-small)
            (org-remove-inline-images)
            (toggle-truncate-lines)
            (org-present-show-cursor)
            (flyspell-mode-on)
            ;; (org-present-read-write)
            ))

(defun org-rust-block ()
  "Insert a code block for rust"
  (interactive)
  (insert "#+BEGIN_SRC rust\n\n#+END_SRC")
  (previous-line)
  )

(use-package smart-mode-line
  :ensure t
  :config (sml/setup)
  ;; :diminish
  )

;; Temporary fix to Rust Mode for properly parsing regex for compilation mode.
;; TODO May need to be moved into rust-mode and called before loading...
(setq rustc-compilation-regexps
  (let ((file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)"))
    (let ((re (concat "^\\(?:\\(?:error\\)\\|\\(?:error\\[E[0-9]+\\]\\)\\|\\(warning\\)\\)[^\0]+?--> \\(" file ":" start-line ":" start-col "\\)")))
      (cons re '(3 4 5 (1) 2)))))

(defun rust-multiline-error-filter ()
  (save-excursion
    (let ((start compilation-filter-start)
          (end (point)))
      (goto-char start)
      (beginning-of-line)  ; is this necessary? should not harm ...
      (while (re-search-forward "^\\(error\\|warning\\)\\(?:\\[E[0-9]+\\]\\)?:[^\n]*[\n]" end t)
        (put-text-property (match-beginning 0) (match-end 0) 'compilation-multiline t)))))
(add-hook 'rust-mode-hook
          (lambda () (add-hook 'compilation-filter-hook #'rust-multiline-error-filter)))

;; TODO
;; (define-key cargo-process-mode-map (kbd "n") 'next-error)
;; (define-key cargo-process-mode-map (kbd "p") 'previous-error-no-select)
