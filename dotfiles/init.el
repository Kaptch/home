(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(defun dos2unix ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
    (goto-char (point-min))
      (while (search-forward "\r" nil t) (replace-match "")))

(require 'bind-key)

(use-package esup
   :ensure t)

(use-package dired
  :ensure nil
  :preface
  (autoload 'dired-get-filename "dired")
  (autoload 'term-set-escape-char "term")
  (defun dired-run-command (&optional filename)
    "Run file at point in a new buffer."
    (interactive)
    (unless filename
      (setq filename (expand-file-name
                      (dired-get-filename t t)
                      default-directory)))
    (let ((buffer (make-term
                   (file-name-nondirectory filename)
                   filename))
          (buffer-read-only nil))
      (with-current-buffer buffer
        ;; (term-mode)
        (term-char-mode)
        (term-set-escape-char ?\C-x))
      (set-process-sentinel
       (get-buffer-process buffer)
       (lambda (proc event)
         (when (not (process-live-p proc))
           (kill-buffer (process-buffer proc)))))
      (switch-to-buffer buffer)))
  :bind (("C-c J" . dired-double-jump)
         :package dired
         :map dired-mode-map
         ("C-c C-c" . compile)
         ("r" . term)
         ("M-@" . shell)
         ("M-*" . eshell)
         ("W" . browse-url-of-dired-file)
         ("@" . dired-run-command)))

(use-package dired-subtree
  :ensure nil
  :bind (:package dired
                  :map dired-mode-map
                  ("<tab>" . dired-subtree-toggle)
                  ("TAB" . dired-subtree-toggle)
                  ("<backtab>" . dired-subtree-cycle)))

(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(use-package gnus
  :commands gnus
  :hook ((dired-mode . turn-on-gnus-dired-mode)))

(use-package ivy
  :ensure t
  :defer t
  :demand
  :diminish
  :commands (ivy-read)
  :bind (([remap list-buffers] . ivy-switch-buffer)
         ([remap switch-to-buffer] . ivy-switch-buffer)
         ([remap switch-to-buffer-other-window] . ivy-switch-buffer-other-window)
         ("C-c C-r" . ivy-resume)
         :package ivy
         :map ivy-minibuffer-map
         ("<escape>" . abort-recursive-edit))
  :init
  (defvar projectile-completion-system)
  (defvar magit-completing-read-function)
  (defvar projector-completion-system)
  (setq projectile-completion-system 'ivy
        magit-completing-read-function 'ivy-completing-read)
  (ivy-mode)
  :commands (ivy-completing-read
             ivy-completion-in-region
             swiper)
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t))

(use-package counsel
  :after ivy
  :init (counsel-mode))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package eldoc
  :ensure t
  :defer t
  :diminish
  :hook (prog-mode . turn-on-eldoc-mode))

(use-package which-key
  :ensure t
  :defer nil
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package async
  :ensure t)

(use-package saveplace
  :ensure t
  :defer t
  :config
  (save-place-mode))

(use-package elf-mode
  :ensure t
  :magic ("ELF" . elf-mode))

(use-package elisp-mode
  :ensure nil
  :interpreter (("emacs" . emacs-lisp-mode)))

(use-package imenu-anywhere
  :ensure t
  :defer t
  :bind
  ("M-i" . helm-imenu-anywhere))

(use-package bm
  :ensure t
  :demand t
  :init
  (setq bm-restore-repository-on-load t)
  :config
  (setq bm-cycle-all-buffers t)
  (setq bm-repository-file "~/.emacs.d/bm-repository")
  (setq-default bm-buffer-persistence t)
  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))
  (add-hook 'after-save-hook #'bm-buffer-save)
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  :bind (("C-b" . bm-next)
         ("C-S-b" . bm-previous)
         ("M-b" . bm-toggle)))

(use-package company
  :ensure t
  :init
  (global-company-mode))

(use-package tuareg
  :ensure t
  :commands (camldebug tuareg-imenu-set-imenu)
  :hook ((tuareg-mode-hook . tuareg-imenu-set-imenu)
         (tuareg-mode-hook . yas-minor-mode)))

(use-package emacs
  :init
  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease))

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode
          inferior-emacs-lisp-mode
          ielm-mode
          lisp-mode
          inferior-lisp-mode
          lisp-interaction-mode
          slime-repl-mode)
         . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook ((emacs-lisp-mode
          inferior-emacs-lisp-mode
          ielm-mode
          lisp-mode
          inferior-lisp-mode
          lisp-interaction-mode
          slime-repl-mode
          web-mode
          less-css-mode
          html-mode
          css-mode)
         . rainbow-mode))

(use-package browse-kill-ring
  :ensure t
  :bind ("C-c y" . 'browse-kill-ring))

(use-package discover
  :ensure t)

(use-package python-mode
  :ensure t
  :init
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-enabled-clients 'pylsp))
  :hook ((python-mode . lsp-deferred)
         (python-mode . yas-minor-mode)
         (before-save . lsp-format-buffer)
         )
  :config
  (use-package pipenv
    :ensure t
    :hook (python-mode . pipenv-mode)
    :init
    (setq
     pipenv-projectile-after-switch-function
     #'pipenv-projectile-after-switch-extended)))

(use-package haskell-mode
  :ensure t
  :commands haskell-mode
  :init
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-enabled-clients 'lsp-haskell))
  :config
  (use-package lsp-haskell
    :ensure t)
  :hook ((haskell-mode . lsp-deferred)
         (haskell-literate-mode . lsp-deferred)
         (haskell-mode . yas-minor-mode)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports)))

(use-package go-mode
  :ensure t
  :init
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-enabled-clients 'gopls))
  (defun go-run-main ()
    (interactive)
    (shell-command
     (format "go run %s"
             (shell-quote-argument (buffer-file-name)))))
  (defun kaptch/go-hook ()
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go test -v && go vet")))
  :config
  (use-package gorepl-mode
    :ensure t)
  :hook ((go-mode . lsp-deferred)
         (go-mode . gorepl-mode)
         (go-mode . yas-minor-mode)
         (go-mode . kaptch/go-hook)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  :bind (("C-c C-c C-r" . go-run-main)))

(use-package erlang
  :ensure t
  :init
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-enabled-clients 'erlang-ls))
  :hook ((erlang-mode . lsp-deferred)
         (erlang-mode . yas-minor-mode)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-position 'bottom)
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands (lsp lsp-deferred)
  :custom
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  :hook ((lsp-mode . lsp-enable-which-key-integration)))

(use-package dap-mode
  :ensure t
  :commands dap-debug
  :config
  (dap-register-debug-template "Rust::GDB Run Configuration"
                               (list :type "gdb"
                                     :request "launch"
                                     :name "GDB::Run"
                                     :gdbpath "rust-gdb"
                                     :target nil
                                     :cwd nil))
  (dap-register-debug-template "Existing Erlang Node"
                               (list :type "erlang"
                                     :request "attach"
                                     :name "Existing Erlang Node"
                                     :gdbpath nil
                                     :target nil
                                     :cwd nil))
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)
  (require 'dap-python)
  (require 'dap-erlang)
  (require 'dap-dlv-go)
  (require 'dap-gdb-lldb)
  :custom
  (dap-python-debugger 'debugpy))

(use-package flymake
  :ensure t)

(use-package ht
  :ensure t)

(use-package f
  :ensure t)

(use-package dash
  :ensure t)

(use-package lv
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package ispell
  :ensure t)

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(use-package ssh
  :ensure t
  :init
  (add-hook 'ssh-mode-hook
            (lambda ()
              (setq ssh-directory-tracking-mode t)
              (shell-dirtrack-mode t)
              (setq dirtrackp nil))))

(use-package proof-general
  :ensure t
  :config
  (use-package company-coq
    :ensure t)
  :custom
  (coq-compile-before-require t)
  (coq-indent-box-style t)
  (coq-maths-menu-enable t)
  (coq-smie-monadic-tokens
   '((";;" . ";; monadic")
     ("do" . "let monadic")
     ("<-" . "<- monadic")
     (";" . "in monadic")
     (";;;" . ";; monadic")))
  (coq-smie-user-tokens
   '(("∗" . "*")
     ("-∗" . "->")
     ("∗-∗" . "<->")
     ("==∗" . "->")
     ("⊢" . "->")
     ("⊣⊢" . "<->")
     ("⋅" . "*")
     (":>" . ":=")
     ("by" . "now")
     ("forall" . "now")))
  (coq-unicode-tokens-enable nil)
  (proof-delete-empty-windows t)
  (proof-disappearing-proofs nil)
  (proof-shell-eager-annotation-end
   "\377\\|done\\]\\|</infomsg>\\|</warning>\\|\\*\\*\\*\\*\\*\\*\\|) >")
  (proof-shell-eager-annotation-start
   "\376\\|\\[Reinterning\\|Warning:\\|TcDebug \\|<infomsg>\\|<warning>")
  (proof-shell-init-cmd
   '("Add Search Blacklist \"Private_\" \"_subproof\". " "Set Suggest Proof Using. "))
  (proof-splash-enable nil)
  (proof-three-window-enable t)
  (overlay-arrow-string "")
  :hook ((coq-mode . (lambda () (progn (undo-tree-mode 1))))
         (coq-mode . company-coq-mode)))

(use-package rust-mode
  :ensure t
  :init
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'rls)
    (add-to-list 'lsp-enabled-clients 'rust-analyzer))
  :hook ((flycheck-mode flycheck-rust-setup)
         (rust-mode . lsp-deferred)
         (rust-mode . (lambda () (setq indent-tabs-mode nil)))
         (rust-mode . (lambda () (prettify-symbols-mode)))
         (rust-mode . yas-minor-mode)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  :config
  (setq rust-format-on-save t)
  (use-package rustic
    :ensure t
    :bind (:map rustic-mode-map
                ("M-j" . lsp-ui-imenu)
                ("M-?" . lsp-find-references)
                ("C-c C-c l" . flycheck-list-errors)
                ("C-c C-c a" . lsp-execute-code-action)
                ("C-c C-c r" . lsp-rename)
                ("C-c C-c q" . lsp-workspace-restart)
                ("C-c C-c Q" . lsp-workspace-shutdown)
                ("C-c C-c s" . lsp-rust-analyzer-status))
    :config
    (defun rk/rustic-mode-hook ()
      (when buffer-file-name
        (setq-local buffer-save-without-query t)))
    :hook (rustic-mode . rk/rustic-mode-hook))
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil))

(use-package slime
  :ensure t
  :init
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-fancy slime-quicklisp slime-asdf))
  (defun cliki:start-slime ()
    (unless (slime-connected-p)
      (save-excursion (slime))))
  :hook (slime-mode . cliki:start-slime)
  :config
  (use-package helm-slime
    :ensure t)
  (setq global-helm-slime-mode t))

(use-package macrostep
  :ensure t)

(use-package spinner
  :ensure t)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package yasnippet
  :ensure t)

(use-package workgroups2
  :ensure t
  :init
  (workgroups-mode 1))

(use-package magit
  :ensure t
  :diminish auto-revert-mode
  :bind
  (("C-c C-m" . magit-status)
   :map magit-status-mode-map
   ("q"       . magit-quit-session))
  :config
  (setq magit-process-password-prompt-regexps
        '("^\\(Enter \\)?[Pp]assphrase\\( for \\(RSA \\)?key '.*'\\)?: ?$"
          ;; match-group 99 is used to identify a host
          "^\\(Enter \\)?[Pp]assword\\( for '\\(?99:.*\\)'\\)?: ?$"
          "^.*'s password: ?$"
          "^Yubikey for .*: ?$"
          "^Enter PIN for '.*': ?$"))
  (defadvice magit-status (around magit-fullscreen activate)
    "Make magit-status run alone in a frame."
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defun magit-quit-session ()
    "Restore the previous window configuration and kill the magit buffer."
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)))

(use-package magit-todos
  :ensure t
  :commands (magit-todos-list))

(use-package git-commit
  :ensure t
  :hook ((git-commit-mode . flyspell-mode)
	 (git-commit-mode . git-commit-save-message)
	 (git-commit-mode . turn-on-auto-fill)))

(use-package nix-mode
  :ensure t)

(use-package epg
  :ensure t
  :config
  (setq epg-pinentry-mode 'loopback))

(use-package ido
  :ensure t
  :diminish ido-mode
  :bind ("C-x C-b" . 'ibuffer)
  :init
  (setq ido-everywhere t)
  (setq ido-ubiquitous-mode t)
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point nil)
  (setq ido-auto-merge-work-directories-length -1)
  (setq ido-use-virtual-buffers t)
  (ido-mode))

(use-package tex
  :defer t
  :ensure auctex
  :ensure ispell
  :ensure flyspell
  :init
  (add-hook 'TeX-mode-hook 'flyspell-mode)
  (add-hook 'TeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook
            (lambda()
              (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
              (setq TeX-save-query nil)
              (setq TeX-show-compilation t)))
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  :config
  (defadvice TeX-LaTeX-sentinel
      (around mg-TeX-LaTeX-sentinel-open-output activate)
    "Open output when there are errors."
    ad-do-it
    (when
        (with-current-buffer TeX-command-buffer
          (plist-get TeX-error-report-switches (intern (TeX-master-file))))
      (TeX-recenter-output-buffer nil)))
  (add-to-list 'TeX-expand-list
	       '("%sn" (lambda () server-name)))
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-show-compilation 0)
  (setq-default TeX-master nil)
  (setq ispell-dictionary "english")
  (setq TeX-PDF-mode t
        TeX-source-correlate-mode t
	TeX-source-correlate-start-server t)
  (add-to-list 'TeX-view-program-list
	       '("Zathura"
	         ("zathura %o"
		  (mode-io-correlate " --synctex-forward %n:0:\"%b\" -x \"emacsclient --socket-name=%sn +%{line} %{input}\""))
	         "zathura"))
  (setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) "Zathura"))

(use-package org
  :ensure t
  :custom
  (copy-face font-lock-constant-face 'calendar-iso-week-face)
  (set-face-attribute 'calendar-iso-week-face nil
                      :height 0.7)
  (calendar-intermonth-text
        '(propertize
          (format "%2d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'calendar-iso-week-face))
  (org-log-done t)
  :bind
  ("C-c c" . org-capture))

(use-package org-agenda
  :ensure nil
  :after org
  :bind
  ("C-c a" . org-agenda)
  :custom
  (org-agenda-files nil)
  (org-agenda-include-diary t)
  (org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                              (todo . " %i %-12:c%l")
                              (tags . " %i %-12:c")
                              (search . " %i %-12:c")))
  (org-agenda-start-on-weekday nil))

(use-package ujelly-theme
  :ensure t
  :config
  (load-theme 'ujelly t))

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame (load-theme 'ujelly t))))
  (load-theme 'ujelly t))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (define-key emacs-lisp-mode-map
              "\C-x\C-e" 'pp-eval-last-sexp)
            (add-hook (make-local-variable 'after-save-hook)
                      (lambda ()
                        (byte-force-recompile default-directory)))
            (define-key emacs-lisp-mode-map
              "\r" 'reindent-then-newline-and-indent)))
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(display-time-mode 1)
(global-visual-line-mode 1)
(global-hl-line-mode 1)
(display-battery-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq indent-tabs-mode nil)
(setq show-paren-mode 1)
(setq inhibit-startup-screen t)
(setq visible-bell 1)
(setq column-number-mode t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)
(setq scroll-conservatively 10000
      scroll-preserve-screen-position t)

(defalias 'yes-or-no-p 'y-or-n-p)

(set-frame-parameter nil 'alpha nil)

(epa-file-enable)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(unless (fboundp 'x-select-font)
  (defalias 'x-select-font 'pgtk-popup-font-panel
    "Pop up the font panel. This function has been overloaded in Nextstep."))

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
