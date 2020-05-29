;; =================================================================================
;; Package Mode Init.
(require 'package)
;; Faster init times supposedly?
(setq package-enable-at-startup nil)

;;(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; Install 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))

;; =================================================================================
;; Local file settings.
(setq omar-local-file "~/.emacs.d/local.el")
(if (file-exists-p omar-local-file)
    (load-file omar-local-file))
;; =================================================================================
;; Xah Fly Keys
(defun nothing ()
  (interactive))

;; This way of doing it doesn't really work too well as I cannot have per buffer
;; binding defined per package in their declaration.
(defun my-xah-bindings ()
  "My keybinding for xah fly keys."

  ;; Main key bindings.
  (define-key xah-fly-key-map (kbd "<f3>") 'save-buffer)
  ;; Figure out what to bind.
  (define-key xah-fly-key-map (kbd "<f4>") 'helm-find-files)
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

  (define-key xah-fly-key-map (kbd "/") 'helm-man-woman)
  (define-key xah-fly-key-map (kbd "DEL") 'delete-backward-char)
  (define-key xah-fly-key-map (kbd "d") 'delete-forward-char)
  (define-key xah-fly-key-map (kbd "g") 'kill-line)
  (define-key xah-fly-key-map (kbd "s") 'helm-buffers-list)
  (define-key xah-fly-key-map (kbd "p") 'dired)
  ;; amx will rebind execute-extended-command to point to amx command.
  (define-key xah-fly-key-map (kbd "a") 'nothing)
  (define-key xah-fly-key-map (kbd "q") 'nothing)
  (define-key xah-fly-key-map (kbd "4") 'split-window-right)
  (define-key xah-fly-key-map (kbd "5") 'helm-M-x)
  (define-key xah-fly-key-map (kbd "w") 'recenter-top-bottom)
  (define-key xah-fly-key-map (kbd "n") 'helm-swoop)
  (define-key xah-fly-key-map (kbd "0") 'delete-window)

  (global-set-key (kbd "M-n") 'omar-next-error)
  (global-set-key (kbd "M-p") 'omar-prev-error)

  ;; Get rid of old open and close buffer mappings.
  (define-key xah-fly-t-keymap (kbd "j") 'nothing)
  (define-key xah-fly-c-keymap (kbd "e") 'nothing)

  (define-key xah-fly-key-map "." '("jump-to-symbol-definition" . per-mode-dot-keybindings))

  ;; Usually paste, but when in term mode, we want to use special paste.
  (define-key xah-fly-key-map (kbd "v") 'per-mode-v-keybindings)
  (define-key xah-fly-key-map (kbd "b") 'my-b-keymap)
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
  (define-key xah-fly-leader-key-map (kbd "t") 'omar-highlight-symbol)
  (define-key xah-fly-leader-key-map (kbd "s") 'alternate-buffer)
  )

(defun alternate-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))


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


;; What happens when we press space + '.'
(defun per-mode-spc-dot-keybindings ()
  (interactive)
  (cond
   (t
    (setq this-command 'xref-find-definitions-other-window)
    (call-interactively 'xref-find-definitions-other-window)
    )))

;; What happens when we press '.'
(defun per-mode-dot-keybindings ()
  (interactive)
  (cond
   ((eq major-mode 'dired-mode)
		(dired-find-file-other-window))

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

   ((eq major-mode 'dired-mode)
    (use-map 'my-dired-mode-keymap))

   ((eq major-mode 'org-mode)
    (use-map 'my-org-mode-keymap))

   ((eq major-mode 'latex-mode)
    (use-map 'my-latex-mode-keymap))

   ((eq major-mode 'emacs-lisp-mode)
    (use-map 'my-emacs-lisp-mode-keymap))))

(progn
  (define-prefix-command 'my-latex-mode-keymap)
  (define-key my-latex-mode-keymap (kbd "e") 'TeX-command-run-all)
  (define-key my-latex-mode-keymap (kbd "n") 'TeX-next-error)
  )

(progn
  (define-prefix-command 'my-emacs-lisp-mode-keymap)
  (define-key my-emacs-lisp-mode-keymap (kbd "e") 'eval-buffer)
  (define-key my-emacs-lisp-mode-keymap (kbd "s") 'eval-last-sexp)
  )


(defun omar-swiper ()
  "like swiper but uses the highlighted work if one is there."
  (interactive)
  (if (region-active-p)
      (let ((word (buffer-substring (region-beginning) (region-end))))
        (deactivate-mark)
        (swiper word))
    (swiper)))

(defun omar-highlight-symbol ()
  (interactive)
  (let (bounds pos1 pos2 mything)
    (setq bounds (bounds-of-thing-at-point 'symbol))
    (setq pos1 (car bounds))
    (setq pos2 (cdr bounds))
    ;; (setq word (buffer-substring-no-properties pos1 pos2))
    (goto-char pos2)
    (push-mark pos1)
    (setq mark-active t)))

(progn
  (define-prefix-command 'my-dired-mode-keymap)
  (define-key my-dired-mode-keymap (kbd "m") 'dired-mark)
  (define-key my-dired-mode-keymap (kbd "u") 'dired-unmark)
  (define-key my-dired-mode-keymap (kbd "h") 'dired-hide-details-mode)
  (define-key my-dired-mode-keymap (kbd "i") 'dired-up-directory)
  (define-key my-dired-mode-keymap (kbd "y") 'dired-create-directory)
  (define-key my-dired-mode-keymap (kbd "r") 'dired-do-rename)
  (define-key my-dired-mode-keymap (kbd "c") 'dired-do-copy)
  (define-key my-dired-mode-keymap (kbd "d") 'dired-flag-file-deletion)
  (define-key my-dired-mode-keymap (kbd "g") 'revert-buffer)
  (define-key my-dired-mode-keymap (kbd "x") 'dired-do-flagged-delete)
  )

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
	(define-key xah-fly-key-map (kbd "|") keymap)
	(setq unread-command-events (listify-key-sequence "|")))

;;; Org Mode commands for my keymap.
(progn
  ;; Define a new keymap and populate it with my bindings.
  (define-prefix-command 'my-org-mode-keymap)
  ;; Bindings:
  (define-key my-org-mode-keymap (kbd "l") 'org-insert-link)
  (define-key my-org-mode-keymap (kbd "t") 'org-todo)
  (define-key my-org-mode-keymap (kbd "s") 'org-time-stamp)
  (define-key my-org-mode-keymap (kbd "d") 'org-deadline)
  (define-key my-org-mode-keymap (kbd "i") 'org-clock-in)
  (define-key my-org-mode-keymap (kbd "o") 'org-clock-out)
  (define-key my-org-mode-keymap (kbd "u") 'org-clock-update-time-maybe)
  (define-key my-org-mode-keymap (kbd "c") 'org-ctrl-c-ctrl-c)

  ;; TODO Make better
  ;; (define-key my-org-mode-keymap (kbd "r") 'org-archive-subtree)
  ;; (define-key my-org-mode-keymap (kbd "<left>") 'org-promote-subtree)
  ;; (define-key my-org-mode-keymap (kbd "<right>") 'org-demote-subtree)
  )

;;; Key bindings that should be avaliable from any buffer i.e. not major mode specific.
(progn
  (define-prefix-command 'my-b-keymap)
  (define-key my-b-keymap (kbd "t") 'org-capture)
  (define-key my-b-keymap (kbd "a") 'org-agenda)
  (define-key my-b-keymap (kbd "j") 'org-clock-jump-to-current-clock)
  (define-key my-b-keymap (kbd "f") 'describe-function)
  (define-key my-b-keymap (kbd "k") 'describe-key)
  (define-key my-b-keymap (kbd "v") 'describe-variable)
  (define-key my-b-keymap (kbd "i") 'omar-goto-init)
  (define-key my-b-keymap (kbd "s") 'start-kbd-macro)
  (define-key my-b-keymap (kbd "e") 'end-kbd-macro)
  (define-key my-b-keymap (kbd "s") 'start-kbd-macro)
  (define-key my-b-keymap (kbd "c") 'call-last-kbd-macro)
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
  ;;(add-hook 'xah-fly-command-mode-activate-hook 'my-xah-bindings)
  ;;(add-hook 'xah-fly-insert-mode-activate-hook 'my-xah-insert-mode)

  :diminish xah-fly-keys
  )
;; =================================================================================
;; Use Package Modes

(use-package diminish
  :ensure t)

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "c-l")

;; (use-package flycheck
	;; :ensure t)

(use-package lsp-mode
	:defer t
	:ensure t
	:hook (
	       (rust-mode . lsp)
	       (lsp-mode . lsp-enable-which-key-integration))
	:config
	(setq lsp-rust-server 'rust-analyzer)
	:commands lsp)

;; optionally
(use-package lsp-ui
	:ensure t
	:commands lsp-ui-mode)

;; if you are helm user
(use-package helm-lsp
	:ensure t
	:commands helm-lsp-workspace-symbol)

(use-package org
  :init
  ;; Spell checking for org mode.
  ;; (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'company-mode)
  (add-hook 'org-agenda-mode-hook 'my-no-show-whitespace)
  :config
  (setq org-todo-keywords
  '((sequence  "REOCCURING(r)" "IDEA(e)" "IMPROVING(i)" "TODO(t)" "NEXT(n)" "SOMEDAY(s)" "ASSIGNED(a@)" "|" "WONT_DO(w)" "DONE(d)")))
  )

(defun my-no-show-whitespace ()
  (setq show-trailing-whitespace nil))

(use-package helm-swoop
  :ensure t
  :defer t
  :config
  (setq helm-swoop-split-direction 'split-window-vertically)
  (setq helm-swoop-speed-or-color t)
  ;; (setq helm-swoop-use-fuzzy-match t)
  (setq helm-swoop-use-line-number-face nil)
  ;; If a symbol or phrase is selected, use it as the initial query.
  (setq helm-swoop-pre-input-function
				(lambda ()
					(if mark-active
							(buffer-substring-no-properties (mark) (point))
						"")))
  )

;; Used for emacs presentations. Allows us to hide the mode-line completely.
(use-package hide-mode-line
  :defer t
  :ensure t
  )

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme)
  )

(use-package keyfreq
	:ensure t
	:init
	(keyfreq-mode 1)
	(keyfreq-autosave-mode 1)
	(setq keyfreq-excluded-commands
				'(self-insert-command
					forward-char
					backward-char
					previous-line
					next-line))
	)

;; Only required once! Comment out after setup.
(use-package solarized-theme
  :ensure t
  :init (load-theme 'solarized-light t))

(use-package magit
  :defer t
  :ensure t)

(use-package org-superstar
  :defer t
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  )


(use-package helm
  :ensure t
  :bind ("M-x" . helm-M-x)
  :config
  (helm-mode)
	(setq helm-split-window-default-side 'same)
  :diminish helm-mode
  )

(use-package which-key
  :ensure t
  :init
  ;; Allow us to give custom names to define-key arguments.
  (setq which-key-enable-extended-define-key t)
  :config
  (which-key-mode)
  :diminish which-key-mode)

(use-package projectile
  :ensure t
  :diminish projectile-mode)

(use-package autorevert
	:ensure t
  :diminish auto-revert-mode
	:init
	(global-auto-revert-mode t)
  )

(use-package eldoc
  :diminish eldoc-mode
  )

;; "text-mode" is a major mode for editing files of text in a human language"
;; most major modes for non-programmers inherit from text-mode
(defun text-mode-hook-setup ()
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-ispell)
	)

(use-package rust-mode
  :ensure t
	:defer t
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  :diminish rust-mode)

(use-package cargo
  :ensure t
	:defer t
  :diminish cargo-minor-mode)

(use-package company
  :ensure t
  :defer t
  :hook (
	 (prog-mode . company-mode)
	 (text-mode . company-mode)
	 (text-mode . text-mode-hook-setup))
	:config
	(setq company-idle-delay 0.0
				company-tooltip-idle-delay 0.0
				company-minimum-prefix-length 3
				;; company-backends '(company-capf company-files
				;; (company-dabbrev company-ispell) company-keywords)

				;; Case sensitive autocompletion!
				company-dabbrev-downcase nil
				company-dabbrev-ignore-case nil
				company-tooltip-align-annotations t)
	:diminish company-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(org-agenda-files
   (quote
    ("/home/gatowololo/Logs/Life/life.org" "/home/gatowololo/Logs/Life/schedule.org" "/home/gatowololo/Logs/Programming/blog.org" "/home/gatowololo/Logs/Programming/emacs.org" "/home/gatowololo/Logs/Programming/rust.org" "/home/gat
owololo/Logs/Work/gradSchool.org" "/home/gatowololo/Logs/Work/rr_channel.org" "/home/gatowololo/Logs/Work/process-cache.org")))
 '(package-selected-packages
   (quote
    (aweshell keyfreq org-superstar flycheck helm-lsp lsp-ui lsp-mode cargo rust-mode company projectile helm-swoop which-key helm-config magit solarized-theme powerline hide-mode-line xah-fly-keys use-package)))
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

(show-paren-mode)
(setq show-trailing-whitespace t)
(setq tab-width 2)
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(setq user-full-name "gatowololo")
(setq user-mail-address "gatowololo@gmail.com")
(defalias 'yes-or-no-p 'y-or-n-p)

;; Auto complete other parenthese.
(electric-pair-mode 1)
(setq inhibit-startup-screen t)
(setq-default create-lockfiles nil)
(setq find-function-C-source-directory "/home/gatowololo/InstalledPrograms/emacs27/src")

;; File Backup
;; https://www.emacswiki.org/emacs/BackupFiles
(setq
 backup-by-copying t     ; don't clobber symlinks
 kept-new-versions 10    ; keep 10 latest versions
 kept-old-versions 0     ; don't bother with old versions
 delete-old-versions t   ; don't ask about deleting old versions
 version-control t       ; number backups
 vc-make-backup-files t) ; backup version controlled files

;; backup every save                                                      ;;

;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
;; https://www.emacswiki.org/emacs/backup-each-save.el
(defvar bjm/backup-file-size-limit (* 5 1024 1024)
  "Maximum size of a file (in bytes) that should be copied at each savepoint.

If a file is greater than this size, don't make a backup of it.
Default is 5 MB")

(defvar bjm/backup-location (expand-file-name "~/.emacs-backups")
  "Base directory for backup files.")

(defvar bjm/backup-trash-dir (expand-file-name "~/.local/share/Trash/files/")
  "Directory for unwanted backups.")

(defvar bjm/backup-exclude-regexp nil
  "Don't back up files matching this regexp.
Files whose full name matches this regexp are backed up to `bjm/backup-trash-dir'. Set to nil to disable this.")

;; Default and per-save backups go here:
;; N.B. backtick and comma allow evaluation of expression
;; when forming list
(setq backup-directory-alist
      `(("" . ,(expand-file-name "per-save" bjm/backup-location))))

;; add trash dir if needed
(if bjm/backup-exclude-regexp
    (add-to-list 'backup-directory-alist `(,bjm/backup-exclude-regexp . ,bjm/backup-trash-dir)))

(defun bjm/backup-every-save ()
  "Backup files every time they are saved.
Files are backed up to `bjm/backup-location' in subdirectories \"per-session\" once per Emacs session, and \"per-save\" every time a file is saved.
Files whose names match the REGEXP in `bjm/backup-exclude-regexp' are copied to `bjm/backup-trash-dir' instead of the normal backup directory.
Files larger than `bjm/backup-file-size-limit' are not backed up."

  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;;
    ;; Override the default parameters for per-session backups.
    ;;
    (let ((backup-directory-alist
           `(("." . ,(expand-file-name "per-session" bjm/backup-location))))
          (kept-new-versions 3))
      ;;
      ;; add trash dir if needed
      ;;
      (if bjm/backup-exclude-regexp
          (add-to-list
           'backup-directory-alist
           `(,bjm/backup-exclude-regexp . ,bjm/backup-trash-dir)))
      ;;
      ;; is file too large?
      ;;
      (if (<= (buffer-size) bjm/backup-file-size-limit)
          (progn
            (message "Made per session backup of %s" (buffer-name))
            (backup-buffer))
        (message "WARNING: File %s too large to backup - increase value of bjm/backup-file-size-limit" (buffer-name)))))
  ;;
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  ;;
  (let ((buffer-backed-up nil))
    ;;
    ;; is file too large?
    ;;
    (if (<= (buffer-size) bjm/backup-file-size-limit)
        (progn
          (message "Made per save backup of %s" (buffer-name))
          (backup-buffer))
      (message "WARNING: File %s too large to backup - increase value of bjm/backup-file-size-limit" (buffer-name)))))

;; add to save hook
(add-hook 'before-save-hook 'bjm/backup-every-save)
(setq backup-directory-alist '(("." . "~/MyEmacsBackups")))
