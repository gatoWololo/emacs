(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; Issues with certificate.
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)
;
; Install 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))

(use-package diminish :ensure t)

(use-package company-irony-c-headers
  :ensure t)

;; (use-package company-quickhelp
  ;; :ensure t
  ;; :config
  ;; (company-quickhelp-mode)
;; )

(use-package irony-eldoc
  :defer t
  :ensure t)
(use-package flycheck-irony
  :defer t
  :ensure t)
(use-package flycheck-pos-tip
  :defer t
  :ensure t)
(use-package flycheck-rust
  :defer t
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  :ensure t)
(use-package magit
  :defer t
  :ensure t)

;; Rust racer
(use-package racer
  :defer t
  :ensure t)

(use-package browse-kill-ring
  :defer t
  :ensure t)

;; TODO Multicompile

;; TODO Org mode?
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  )

;; Python
(use-package elpy
  :config (elpy-enable)
  :init (add-hook 'python-mode-hook 'elpy-enable)
  :defer t
  :custom
  (elpy-rpc-python-command "python3")
  )

(use-package company
  :ensure t
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'company-mode)
  (add-hook 'rust-mode-hook 'company-mode)
  (add-hook 'c++-mode-hook
            (lambda () (setq company-clang-arguments "-std=c++14")))
  :config
  (setq company-idle-delay 0.1
        company-tooltip-idle-delay 0.1
        company-minimum-prefix-length 3
        company-backends '(company-capf company-files
                                        (company-dabbrev company-ispell) company-keywords)
        ;; Case sensitive autocompletion!
        company-dabbrev-downcase nil
        company-tooltip-align-annotations t)
  (add-hook 'c++-mode-hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends)
                           '(company-irony-c-headers company-irony))

              ))
  :diminish company-mode
  )

(use-package flycheck
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'rust-mode-hook 'flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically nil
        flycheck-disabled-checkers '(c/c++-clang c/c++-gcc c/c++-cppcheck)
        flycheck-display-errors-delay 0.0)
  (flycheck-pos-tip-mode)
  :after
  ;; Needed for xah fly key map
  (xah-fly-keys)
  ;; TODO
  ;; rust-mode-hook
  )

(use-package rust-mode
  :ensure t
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  :diminish rust-mode)

(use-package cargo
  :ensure t
  :diminish cargo)

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme)
  )

(use-package which-key
  :config
  (which-key-mode)
  :diminish which-key-mode)

(use-package flx-ido :ensure t)
(use-package ido-completing-read+
  :ensure t
  :config (ido-ubiquitous-mode 1)
  )

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'c++-mode-hook 'eldoc-mode)


(progn
  (setq org-mode-keys (make-sparse-keymap))
  (define-key org-mode-keys (kbd "l") 'org-insert-link)
  (define-key org-mode-keys (kbd "a") 'org-agenda)
  (define-key org-mode-keys (kbd "c") 'org-capture)
  (define-key org-mode-keys (kbd "i") 'org-iswitchb)
  (define-key org-mode-keys (kbd "f") 'org-agenda-file-to-front)
  (define-key org-mode-keys (kbd "t") 'org-todo)
  (define-key org-mode-keys (kbd "s") 'org-time-stamp)
  (define-key org-mode-keys (kbd "d") 'org-deadline)
  )

;; This way of doing it doesn't really work too well as I cannot have per buffer
;; binding defined per package in their declaration.
(defun my-xah-bindings ()
  "My keybinding for xah fly keys."
  (define-key xah-fly-key-map (kbd "7") 'flycheck-buffer)
  (define-key xah-fly-key-map (kbd "DEL") 'delete-backward-char)
  (define-key xah-fly-key-map (kbd "d") 'delete-forward-char)
  (define-key xah-fly-key-map (kbd "g") 'kill-line)
  (define-key xah-fly-key-map (kbd "s") 'switch-to-buffer)
  (define-key xah-fly-key-map (kbd "p") 'dired)
  (define-key xah-fly-key-map (kbd "a") 'amx)
  (define-key xah-fly-key-map (kbd "4") 'split-window-right)
  (define-key xah-fly-key-map (kbd "w") 'recenter-top-bottom)
  ;; Needed since we want our hook called.
  (define-key xah-fly-key-map (kbd "M-SPC") 'xah-fly-command-mode-activate)
  ;; Hmm, we could have a major mode hook to our mode :3
  (define-key xah-fly-key-map (kbd "m") org-mode-keys)
  ;; All these keys need to be bound to something useful.
  ;; (define-key xah-fly-key-map (kbd "b") 'TODO)
  ;; (define-key xah-fly-key-map (kbd "q") 'TODO)
  (define-key xah-fly-key-map (kbd ".") 'xref-find-definitions)
  ;; (define-key xah-fly-key-map (kbd "/") 'TODO)
  (define-key xah-fly-leader-key-map (kbd "m") 'magit-status)

  ;; One day I'll figure out how to make per major-mode key bindings.
  (define-key xah-fly-leader-key-map (kbd ".") 'ggtags-find-tag-dwim)

  (define-key xah-fly-leader-key-map (kbd "4") 'split-window-below)
  (define-key xah-fly-leader-key-map (kbd "m") 'magit-status)
  (global-unset-key (kbd "C-a"))
  )

(use-package xah-fly-keys
  :config
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys 1)
  ;; ;; remove a hook for autosave.
  (remove-hook 'xah-fly-command-mode-activate-hook 'xah-fly-save-buffer-if-file)
  (my-xah-bindings)
  ;; Call on every switch to command mode and on init.
  (add-hook 'xah-fly-command-mode-activate-hook 'my-xah-bindings)
  ;; :hook
  ;; (xah-fly-command-mode-activate . my-xah-bindings)
  :diminish xah-fly-keys
  )

;; Only required once!
;; (use-package solarized-theme
;; :ensure t
;; :config
;; (custom-set-variables
;; '(custom-enabled-themes (quote (solarized-light))))
;; )

;; Smex replacement for auto completion.
(use-package amx
  :ensure t
  )

(use-package irony
  :defer t
  :ensure t
  :init
  (add-hook 'flycheck-mode-hook 'flycheck-irony-setup)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-eldoc)
  :config
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  :after (flycheck)
  :diminish irony
  )

;; Eldoc
(add-hook 'c-mode-hook 'eldoc-mode)
(add-hook 'c++-mode-hook 'eldoc-mode)


(use-package ggtags
  :defer t
  :init
  (add-hook 'c++-mode-hook 'ggtags-mode)
  (add-hook 'c-mode-hook 'ggtags-mode)
  :diminish ggtags-mode
  )

;; Haskell
(setq-default haskell-font-lock-symbols t)

;; TODO
;; :hook ((emacs-lisp-mode
;; text-mode-hook
;; racer-mode-hook) . company-mode)

;; My Emacs custom variable settings.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (\` (("." . "~/.saves"))))
 '(column-number-mode t)
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(ediff-diff-options "-w")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(eldoc-idle-delay 0.4)
 '(elpy-rpc-python-command "python3" t)
 '(gud-tooltip-mode t)
 '(ido-auto-merge-work-directories-length -1)
 '(ido-enable-flex-matching t)
 '(ido-ignore-extensions t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(org-agenda-files
   (quote
    ("~/Logs/Rust/rust.org" "~/Logs/GradSchool/gradSchool.org" "~/Logs/DeterministicProject/detProject.org")))
 '(org-agenda-only-exact-dates t t)
 '(package-selected-packages
   (quote
    (cargo use-package xah-fly-keys which-key solarized-theme smex racer powerline magit intero ido-ubiquitous ggtags flycheck-rust flycheck-pos-tip flycheck-irony flx-ido elpy diminish company-irony-c-headers company-irony clojure-mode better-defaults auctex)))
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(user-full-name "gatoWololo")
 '(user-mail-address "gatoWololo@gmail.com"))

;; My Emacs functions for settings.
(defalias 'yes-or-no-p 'y-or-n-p)
(ido-mode 1)
(ido-everywhere t)
(add-to-list `completion-ignored-extensions ".d")
(scroll-bar-mode -1)
(menu-bar-mode -1)
(electric-pair-mode 1)

;; Dired mode omit hidden files:
(require 'dired-x)
(setq-default dired-omit-files-p t) ; Buffer-local variable
;; Omit all files starting with "." except "." and ".."
(setq dired-omit-files "^\\.[^.]+")


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

(set-face-attribute 'default nil :height 130)

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(global-unset-key (kbd "C-a"))

; Do not highlight whitespce for terminal modes.
(add-hook 'term-mode-hook
 (lambda() (setq show-trailing-whitespace nil)))

(setq initial-buffer-choice
      (lambda () (switch-to-buffer "*dashboard*")))
