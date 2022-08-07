(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(defun package--save-selected-packages (&optional value)
  "Set and save `package-selected-packages' to VALUE."
  (when value
    (setq package-selected-packages value)))

(defun dos2unix ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
    (goto-char (point-min))
      (while (search-forward "\r" nil t) (replace-match "")))

;; Text completion
(use-package company
  :init
  (global-company-mode)
  )

;; Ocaml support
(use-package tuareg
  :config
  (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
  (autoload 'camldebug "camldebug" "Run the Caml debugger" t)
  (autoload 'tuareg-imenu-set-imenu "tuareg-imenu"
    "Configuration of imenu for tuareg" t)        
  (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
  (setq auto-mode-alist 
	(append '(("\\.ml[ily]?$" . tuareg-mode)
	          ("\\.topml$" . tuareg-mode))
		auto-mode-alist))
  )

;; Kill ring
(use-package browse-kill-ring
  :bind ("C-c y" . 'browse-kill-ring)
  )

;; ???
(use-package discover)

;;; Haskell language server
(use-package lsp-haskell)

(use-package haskell-mode
  :commands haskell-mode
  :init
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp)
  )

(use-package gorepl-mode
  :ensure)

(use-package go-mode
  :config
  (add-hook 'go-mode-hook 'lsp-deferred)
  (add-hook 'go-mode-hook 'gorepl-mode)
  (defun go-run-main ()
    (interactive)
    (shell-command
     (format "go run %s"
             (shell-quote-argument (buffer-file-name)))))
  ;; (defun go-run-this-file ()
  ;;   "go run"
  ;;   (interactive)
  ;;   (recompile (format "go run %s" (buffer-file-name))))
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go test -v && go vet"))
    ;; (let ((map go-mode-map))
    ;;   (define-key map (kbd "C-c a") 'go-test-current-project)
    ;;   (define-key map (kbd "C-c m") 'go-test-current-file)
    ;;   (define-key map (kbd "C-c .") 'go-test-current-test)
    ;;   (define-key map (kbd "C-c b") 'go-run))
    )
  (define-key go-mode-map (kbd "C-c C-c C-r") 'go-run-main)
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'yas-minor-mode)
  )

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; Hast-tables
(use-package ht)

;; Files
(use-package f)

;; Lists
(use-package dash)

;; ???
(use-package lv)

(use-package markdown-mode)

(use-package ispell)

(use-package direnv
  :config
  (direnv-mode))

(use-package ssh
  :init 
  (add-hook 'ssh-mode-hook
            (lambda ()
              (setq ssh-directory-tracking-mode t)
              (shell-dirtrack-mode t)
              (setq dirtrackp nil)))
  )

(use-package proof-general
  :config
  (setq coq-compile-before-require t)
  (setq coq-indent-box-style t)
  (setq coq-maths-menu-enable t)
  (setq coq-smie-monadic-tokens
	'((";;" . ";; monadic")
	  ("do" . "let monadic")
	  ("<-" . "<- monadic")
	  (";" . "in monadic")
	  (";;;" . ";; monadic")))
  (setq coq-smie-user-tokens
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
  (setq coq-unicode-tokens-enable nil)
  (setq proof-delete-empty-windows t)
  (setq proof-disappearing-proofs nil)
  (setq proof-shell-eager-annotation-end
	"\377\\|done\\]\\|</infomsg>\\|</warning>\\|\\*\\*\\*\\*\\*\\*\\|) >")
  (setq proof-shell-eager-annotation-start
	"\376\\|\\[Reinterning\\|Warning:\\|TcDebug \\|<infomsg>\\|<warning>")
  (setq proof-shell-init-cmd
	'("Add Search Blacklist \"Private_\" \"_subproof\". " "Set Suggest Proof Using. "))
  (setq proof-splash-enable nil)
  (setq proof-three-window-enable t)
  :init
  (add-hook 'coq-mode-hook
	    (lambda () (progn
			 (undo-tree-mode 1)
			 (global-set-key "\C-cy" '(lambda ()
						    (interactive)
						    (popup-menu 'yank-menu))))))
  (add-hook 'coq-mode-hook #'company-coq-mode)
  )

;; Coq support
(use-package company-coq)

(use-package rustic
  :ensure
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
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(use-package rust-mode
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  ;; (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil)))
  (add-hook 'rust-mode-hook
            (lambda () (prettify-symbols-mode)))
  (add-hook 'rust-mode-hook #'lsp)
  :config
  (setq rust-format-on-save t)
  ;; (define-key rust-mode-map (kbd "C-c C-c") 'rust-run)
  )

;; Lisp support
(use-package slime)

;; Macro-expander
(use-package macrostep)

;; Progress bars
(use-package spinner)

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  )

;; Templates
(use-package yasnippet)

;; Session manager
(use-package workgroups2
  :init
  (workgroups-mode 1)
  )

;; Git
(use-package magit
  :config
  (setq magit-process-password-prompt-regexps
        '("^\\(Enter \\)?[Pp]assphrase\\( for \\(RSA \\)?key '.*'\\)?: ?$"
          ;; match-group 99 is used to identify a host
          "^\\(Enter \\)?[Pp]assword\\( for '\\(?99:.*\\)'\\)?: ?$"
          "^.*'s password: ?$"
          "^Yubikey for .*: ?$"
          "^Enter PIN for '.*': ?$"))
  )

(use-package nix-mode)

(use-package epg
  :config
  (setq epg-pinentry-mode 'loopback)
  )

;; Interactive file traverse
(use-package ido
  :diminish ido-mode
  :bind ("C-x C-b" . 'ibuffer) ;; Shows a list of buffers
  :init
  (setq ido-everywhere t)
  (setq ido-ubiquitous-mode t)
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point nil)
  (setq ido-auto-merge-work-directories-length -1)
  (setq ido-use-virtual-buffers t)
  )

(use-package tex
  :defer t
  :ensure auctex
  :ensure ispell
  :ensure flyspell
  :init
  (add-hook 'LaTeX-mode-hook 
            (lambda()
              (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
              ;; (setq TeX-command-default "XeLaTeX")
              (setq TeX-save-query nil)
              (setq TeX-show-compilation t)))
  (add-hook 'TeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
  (add-hook 'TeX-mode-hook 'turn-on-reftex)
  :config
  (defadvice TeX-LaTeX-sentinel
      (around mg-TeX-LaTeX-sentinel-open-output activate)
    "Open output when there are errors."
    ;; Run `TeX-LaTeX-sentinel' as usual.
    ad-do-it
    ;; Check for the presence of errors.
    (when
        (with-current-buffer TeX-command-buffer
          (plist-get TeX-error-report-switches (intern (TeX-master-file))))
      ;; If there are errors, open the output buffer.
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
  ;; (eval-after-load "tex"
  ;;   '(setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) "Okular"))
  (add-to-list 'TeX-view-program-list
	       '("Zathura"
	         ("zathura %o"
		  (mode-io-correlate " --synctex-forward %n:0:\"%b\" -x \"emacsclient --socket-name=%sn +%{line} %{input}\""))
	         "zathura"))
  (setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) "Zathura")
  )

(use-package ujelly-theme
  :config
  (load-theme 'ujelly t)
  )

;; Runs the theme in the client mode
(if (daemonp) 
    (add-hook 'after-make-frame-functions 
	      (lambda (frame) 
		(with-selected-frame frame (load-theme 'ujelly t)))) 
  (load-theme 'ujelly t))

;; Global config 
(display-time-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-visual-line-mode 1)
(global-hl-line-mode 1)
(display-battery-mode 1)
(setq-default indent-tabs-mode nil)
(setq show-paren-mode 1)
(setq inhibit-startup-screen t)
(setq visible-bell 1)
(setq column-number-mode t)
(setq overlay-arrow-string "")
(setq org-agenda-files nil)
(setq org-log-done t)
(setq org-agenda-include-diary t)
(defalias 'yes-or-no-p 'y-or-n-p)
(set-frame-parameter nil 'alpha nil)
(define-key global-map "\C-ca" 'org-agenda)

;; ???
(epa-file-enable)

;; TODO: move it to relevant sections
(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil
                    :height 0.7)
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (push 'company-elisp company-backends)))

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
