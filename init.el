(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; Issues with certificate.
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

; Install 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))

(use-package diminish
  :ensure t)

;; Counsel: a collection of Ivy-enhanced versions of common Emacs commands
;; Ivy: a generic completion mechanism for Emacs
;; Swiper: an Ivy-enhanced alternative to isearch
(use-package counsel
  :ensure t
  :config
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-initial-inputs-alist nil "No ^ when ivy searching.")
  :diminish ivy
  )

(use-package company-irony-c-headers
  :ensure t)

(use-package org
  :init
  ;; Spell checking for org mode.
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'company-mode)
  )


(use-package flyspell
  :init
  ;; (define-key flyspell-mouse-map (kbd "<mouse-3>") #'flyspell-correct-word)
  :custom
  (ispell-program-name "hunspell")
  :diminish flyspell-mode
  )

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
  :custom
  (racer-rust-src-path
   "/home/gatowololo/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")
  )

(use-package browse-kill-ring
  :defer t
  :ensure t)

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
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'company-mode)
  ;; (add-hook 'rust-mode-hook 'company-mode)
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
  :ensure t
  :config
  (which-key-mode)
  :diminish which-key-mode)
(setq ido-auto-merge-work-directories-length -1)

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; This way of doing it doesn't really work too well as I cannot have per buffer
;; binding defined per package in their declaration.
(defun my-xah-bindings ()
  "My keybinding for xah fly keys."

  ;; Main key bindings.
  (define-key xah-fly-key-map (kbd "DEL") 'delete-backward-char)
  (define-key xah-fly-key-map (kbd "d") 'delete-forward-char)
  (define-key xah-fly-key-map (kbd "g") 'kill-line)
  (define-key xah-fly-key-map (kbd "s") 'counsel-ibuffer)
  (define-key xah-fly-key-map (kbd "p") 'dired)
  (define-key xah-fly-key-map (kbd "a") 'counsel-M-x)
  (define-key xah-fly-key-map (kbd "4") 'split-window-right)
  (define-key xah-fly-key-map (kbd "w") 'recenter-top-bottom)
  (define-key xah-fly-key-map (kbd "n") 'swiper)
  (define-key xah-fly-key-map (kbd ".") 'per-mode-dot-keybindings)
  (define-key xah-fly-key-map (kbd "v") 'per-mode-v-keybindings)
  ;; Needed since we want our hook called.
  (define-key xah-fly-key-map (kbd "M-SPC") 'xah-fly-command-mode-activate)
  ;; (define-key xah-fly-key-map (kbd "f") 'test)
  ;; All these keys need to be bound to something useful.
  ;; (define-key xah-fly-key-map (kbd "q") 'TODO)

  (define-key xah-fly-key-map (kbd "m") 'per-mode-keybindings)
  ;; (define-key  (kbd "c-d") (lambda () (message "heello")))

  ;; Space + Key
  (define-key xah-fly-leader-key-map (kbd "4") 'split-window-below)
  (define-key xah-fly-leader-key-map (kbd "m") 'magit-status)
  (define-key xah-fly-leader-key-map (kbd "g") 'goto-line)
  )


(defun my-xah-insert-mode ()
  (define-key xah-fly-key-map (kbd "|") nil)
  )

(defun per-mode-v-keybindings ()
  (interactive)
  (cond
   ((eq major-mode 'term-mode)
    (term-paste))
   (t
    (xah-paste-or-paste-previous))))

(defun per-mode-dot-keybindings ()
  (interactive)
  (cond
   ((eq major-mode 'c++-mode)
    (call-interactively 'ggtags-find-definition))
   (t
    (setq this-command 'xref-find-definitions)
    (call-interactively 'xref-find-definitions)
    )))

;; Each mode based key binding must be a key map which will attached to "m" for modes :)
(defun per-mode-keybindings ()
  (interactive)
  (cond
   ;; Org Mode
   ((eq major-mode 'org-mode)
    (use-map 'my-org-mode-keymap))

   ((derived-mode-p 'prog-mode)
    (use-map 'my-prog-mode-keymap))))

(defun use-map (keymap)
  (progn
    (define-key xah-fly-key-map (kbd "|") keymap)
    (setq unread-command-events (listify-key-sequence "|"))))

(progn
  (define-prefix-command 'my-org-mode-keymap)
  (define-key my-org-mode-keymap (kbd "l") 'org-insert-link)
  (define-key my-org-mode-keymap (kbd "a") 'org-agenda)
  (define-key my-org-mode-keymap (kbd "c") 'org-capture)
  (define-key my-org-mode-keymap (kbd "i") 'org-iswitchb)
  (define-key my-org-mode-keymap (kbd "f") 'org-agenda-file-to-front)
  (define-key my-org-mode-keymap (kbd "t") 'org-todo)
  (define-key my-org-mode-keymap (kbd "s") 'org-time-stamp)
  (define-key my-org-mode-keymap (kbd "d") 'org-deadline)
  )

(use-package xah-fly-keys
  :ensure t
  :config
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys 1)
  ;; ;; remove a hook for autosave.
  (remove-hook 'xah-fly-command-mode-activate-hook 'xah-fly-save-buffer-if-file)
  (my-xah-bindings)
  ;; Call on every switch to command mode and on init.
  (add-hook 'xah-fly-command-mode-activate-hook 'my-xah-bindings)
  (add-hook 'xah-fly-insert-mode-activate-hook 'my-xah-insert-mode)

  ;; (add-hook 'xah-fly-command-mode-activate-hook 'mode-specific-xfk-bindings)

  :diminish xah-fly-keys
  )

;; Only required once! Comment out after setup.
;; (use-package solarized-theme
;; :ensure t
;; :config
;; (custom-set-variables
;; '(custom-enabled-themes (quote (solarized-light))))
;; )

(use-package irony
  :defer t
  :ensure t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  :config
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  :diminish irony
  )

(use-package company-irony
  :defer t
  :ensure t
)
;; (eval-after-load 'company
  ;; '(add-to-list 'company-backends 'company-irony))

;; Eldoc
;; (add-hook 'c-mode-hook 'eldoc-mode)
;; (add-hook 'c++-mode-hook 'eldoc-mode)


(use-package ggtags
  :defer t
  :ensure t
  ;; :init
  :config
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
 '(company-quickhelp-delay 0.1)
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
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-program-name "hunspell")
 '(ivy-count-format "(%d/%d) ")
 '(ivy-initial-inputs-alist nil t)
 '(ivy-use-virtual-buffers t)
 '(org-agenda-files
   (quote
    ("~/Logs/DeterministicProject/detProject.org" "~/Logs/Rust/rust.org" "~/Logs/GradSchool/gradSchool.org")))
 '(org-agenda-only-exact-dates t t)
 '(package-selected-packages
   (quote
    (company-quickhelp counsel haskell-mode markdown-mode cargo use-package xah-fly-keys which-key solarized-theme smex racer powerline magit intero ido-ubiquitous ggtags flycheck-rust flycheck-pos-tip flycheck-irony flx-ido elpy diminish company-irony-c-headers company-irony clojure-mode better-defaults auctex)))
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(user-full-name "gatowololo")
 '(user-mail-address "gatowololo@gmail.com"))

;; My Emacs functions for settings.
(defalias 'yes-or-no-p 'y-or-n-p)
;; Ido when searching for file, I like this better.
(ido-mode 1)
;; (ido-everywhere t)
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

;; Make text bigger.
(set-face-attribute 'default nil :height 130)

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

(setq explicit-shell-file-name "/usr/bin/fish")

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

(defun my--advice-irony-start-process (orig-func &rest args)
  (let ((shell-file-name "/bin/sh"))
    (apply orig-func args)))

(advice-add 'irony--start-server-process :around 'my--advice-irony-start-process)
