;;; package --- my emacs configuration
;;; Commentary:
;;; Code:
;;; TODOs:
;;; - Move functions in a different file for better readability
;;; - Group packages by their purpose
;;; - Refactor Org-mode configuration
;;; - Deal with shit-ton of keybindings (via hydra)
;;; - Think about some sort of Org-files cloud sync
(setq message-log-max t)

(setq warning-minimum-level :error)

(setq native-comp-async-report-warnings-errors nil)

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

(setq package-enable-at-startup nil)

(straight-use-package 'use-package)
(straight-use-package 'org)
(package-initialize)

(use-package straight
  :custom (straight-use-package-by-default t))

(defun reload-emacs-configuration ()
  "Reload Emacs configuration."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun dos2unix ()
  "Replace DOS eolns CR LF with Unix eolns CR."
  (interactive)
    (goto-char (point-min))
    (while (search-forward "\r" nil t) (replace-match "")))

(defun window-horizontal-to-vertical ()
  "Switch from a horizontal split to a vertical split."
  (interactive)
  (let ((one-buf (window-buffer (selected-window)))
        (buf-point (point)))
    (other-window 1)
    (delete-other-windows)
    (split-window-horizontally)
    (switch-to-buffer one-buf)
    (goto-char buf-point)))

(defun window-vertical-to-horizontal ()
  "Switch from a vertical split to a horizontal split."
  (interactive)
  (let ((one-buf (window-buffer (selected-window)))
        (buf-point (point)))
    (other-window 1)
    (delete-other-windows)
    (split-window-vertically)
    (switch-to-buffer one-buf)
    (goto-char buf-point)))

(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; emacsclient -e "(lookup-password :host \"facebook.com\" :user \"zuck\")" | cut -d '"' -f2
(defun lookup-password (&rest keys)
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
      nil)))

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defun sudo-file-name (filename)
  "Prepend '/sudo:root@`system-name`:' to FILENAME if appropriate.
This is, when it doesn't already have a sudo-prefix."
  (if (not (or (string-prefix-p "/sudo:root@localhost:"
                                filename)
               (string-prefix-p (format "/sudo:root@%s:" system-name)
                                filename)))
      (format "/sudo:root@%s:%s" system-name filename)
    filename))

(defun sudo-save-buffer ()
  "Save FILENAME with sudo if the user approves."
  (interactive)
  (when buffer-file-name
    (let ((file (sudo-file-name buffer-file-name)))
      (if (yes-or-no-p (format "Save file as %s ? " file))
          (write-file file)))))

(advice-add 'save-buffer :around
            #'(lambda (fn &rest args)
               (when (or (not (buffer-file-name))
                         (not (buffer-modified-p))
                         (file-writable-p (buffer-file-name))
                         (not (sudo-save-buffer)))
                 (call-interactively fn args))))

(defun add-hook-lsp-organize-imports ()
  (add-hook 'before-save-hook 'lsp-organize-imports))

(defun add-hook-lsp-format-buffer ()
  (add-hook 'before-save-hook 'lsp-format-buffer))

(defadvice server-visit-files (before parse-numbers-in-lines (files proc &optional nowait) activate)
  "Open file with emacsclient with cursors positioned on requested line.
Most of console-based utilities prints filename in format
'filename:linenumber'.  So you may wish to open filename in that format.
Just call:
  emacsclient filename:linenumber
and file 'filename' will be opened and cursor set on line 'linenumber'"
  (ad-set-arg 0
              (mapcar (lambda (fn)
                        (let ((name (car fn)))
                          (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
                              (cons
                               (match-string 1 name)
                               (cons (string-to-number (match-string 2 name))
                                     (string-to-number (or (match-string 3 name) ""))))
                            fn)))
                      files)))

(defconst my-diary-file (expand-file-name "diary" "~/"))
(unless (file-exists-p my-diary-file)
  (shell-command (concat "touch " my-diary-file)))
(setq diary-file my-diary-file)

(setq org-directory "~/Org/")
(unless (file-exists-p org-directory)
  (make-directory org-directory t))

(setq org-default-notes-file (expand-file-name "main.org" org-directory))
(unless (file-exists-p org-default-notes-file)
  (append-to-file "* Agenda\n" nil org-default-notes-file))

;; (setq agenda-file (expand-file-name "agenda.org" org-directory))
;; (unless (file-exists-p agenda-file)
;;   (shell-command (concat "touch " agenda-file)))

;; (setq work-file (expand-file-name "work.org" org-directory))
;; (unless (file-exists-p work-file)
;;   (shell-command (concat "touch " work-file)))

(use-package bind-key
  :straight (:type built-in))

(use-package tramp
  :straight (:type built-in)
  :init
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  (setq tramp-default-method "ssh")
  (setq tramp-auto-save-directory
        (expand-file-name "tramp-auto-save" user-emacs-directory))
  (setq tramp-persistency-file-name
        (expand-file-name "tramp-connection-history" user-emacs-directory))
  (setq password-cache-expiry nil)
  (setq tramp-use-ssh-controlmaster-options nil)
  (setq remote-file-name-inhibit-cache nil))

(use-package smtpmail
  :config
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq mml-secure-message-sign-pgp t
        mml-secure-openpgp-sign-with-sender t
        mml-secure-openpgp-encrypt-to-self t)
  (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)
  (setq message-kill-buffer-on-exit t))

(use-package dashboard
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  :config
  (setq dashboard-set-navigator t)
  (setq dashboard-set-init-info t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-projects-switch-function 'counsel-projectile)
  (setq dashboard-item-names '(("Recent Files:" . "Recently opened files:")
                               ("Agenda for today:" . "Today's agenda:")
                               ("Agenda for the coming week:" . "Agenda:")))
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (setq dashboard-match-agenda-entry
      "TODO=\"TODO\"|TODO=\"IN-PROGRESS\"")
  ;; (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (setq dashboard-items '((recents  . 10)
                          (projects . 5)
                          (agenda . 7)))
  (setq dashboard-agenda-release-buffers t)
  (dashboard-setup-startup-hook))

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-modified-marker "*")
  (setq centaur-tabs-enable-key-bindings t)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-show-navigation-buttons t)
  (centaur-tabs-mode t)
  (setq uniquify-separator "/")
  (setq uniquify-buffer-name-style 'forward)
  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

 Group centaur-tabs with mode if buffer is derived from `eshell-mode' `erc-mode'
`emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
      ;; "Remote")
      ((or (string-equal "*" (substring (buffer-name) 0 1))
	   (memq major-mode '(magit-process-mode
			      magit-status-mode
			      magit-diff-mode
			      magit-log-mode
			      magit-file-mode
			      magit-blob-mode
			      magit-blame-mode
			      )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'erc-mode)
       "IRC")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
			  help-mode))
       "Help")
      ((memq major-mode '(org-mode
			  org-agenda-clockreport-mode
			  org-src-mode
			  org-agenda-mode
			  org-beamer-mode
			  org-indent-mode
			  org-bullets-mode
			  org-cdlatex-mode
			  org-agenda-log-mode
			  diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  :hook
  (dired-mode . centaur-tabs-local-mode)
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  ("C-c t s" . centaur-tabs-counsel-switch-group)
  ("C-c t p" . centaur-tabs-group-by-projectile-project)
  ("C-c t g" . centaur-tabs-group-buffer-groups))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-tokyo-night t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package all-the-icons
  :if (display-graphic-p)
  :custom
  (all-the-icons-scale-factor 1.0))

;; (use-package all-the-icons-ivy
;;   :if (display-graphic-p))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :config
  :hook (dired-mode . (lambda ()
                        (interactive)
                        (unless (file-remote-p default-directory)
                          (all-the-icons-dired-mode)))))

(use-package doom-modeline
  :after eshell
  :hook (after-init . doom-modeline-mode)
  :custom-face
  (mode-line ((t (:height 0.95))))
  (mode-line-inactive ((t (:height 0.85))))
  :custom
  (doom-modeline-project-detection 'auto)
  (doom-modeline-height 25)
  (doom-modeline-bar-width 4)
  (doom-modeline-lsp t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e t)
  (doom-modeline-irc t)
  (doom-modeline-irc-stylize 'identity)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name t)
  (doom-modeline-persp-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-support-imenu t)
  :config
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info remote-host " "
          hud buffer-position selection-info battery " "
          input-method buffer-encoding media-info pdf-pages " "
          window-number objed-state indent-info modals lsp checker " "
          debug repl irc mu4e misc-info)
    '(minor-modes major-mode process vcs time))
  (defun setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'main 'default))
  (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline))

(use-package lsp-ltex
  :ensure t
  :hook (text-mode . (lambda ()
                       (require 'lsp-ltex)
                       (lsp)))
  :init
  (setq lsp-ltex-version "15.2.0"))

(use-package git-timemachine
  :straight (:host nil :type git :repo "https://codeberg.org/pidu/git-timemachine.git")
  :defer 1
  :bind
  (("C-c C-c g" . git-timemachine-toggle)))

(use-package multiple-cursors
  :bind (("C-M-SPC" . set-rectangular-region-anchor)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-c C-SPC" . mc/edit-lines)))

(use-package restclient
  :bind (("C-c C-c q" . restclient-mode)))

(use-package counsel-projectile
  :after projectile
  :bind (("C-M-p" . counsel-projectile-find-file))
  :config
  (counsel-projectile-mode))

(use-package procress
  :straight (:host github :repo "haji-ali/procress")
  :commands procress-auctex-mode
  :init
  (add-hook 'LaTeX-mode-hook #'procress-auctex-mode)
  :config
  (procress-load-default-svg-images))

(use-package hydra
  :defer 1)

(use-package ivy-hydra
  :defer t
  :after (ivy hydra))

(use-package password-store
  :config
  (setq password-store-password-length 16))

(use-package auth-source-pass
  :config
  ;; (auth-source-pass-enable)
  (setq auth-source-debug 'trivia)
  (setq auth-source-do-cache nil)
  (setq auth-sources
        '((:source "~/.authinfo.gpg")))
  ;; (setq auth-sources '(password-store))
  )

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :demand t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (defun switch-project-action ()
    "Switch to a workspace with the project name and start `magit-status'."
    (persp-switch (projectile-project-name))
    (magit-status))
  (setq projectile-switch-project-action #'switch-project-action))

(use-package counsel-projectile
  :after projectile
  :bind (("C-M-p" . counsel-projectile-find-file))
  :config
  (counsel-projectile-mode))

(use-package docker
  :commands docker)

(use-package docker-tramp
  :defer t
  :after docker)

(use-package emms
  :after dired
  :commands emms
  :init
  (add-hook 'emms-player-started-hook 'emms-show)
  (defun play-smooth-jazz ()
    "Start up some nice Jazz"
    (interactive)
    (emms-play-streamlist "http://thejazzgroove.com/itunes.pls"))
  :bind (("C-S-<return>" . 'emms-pause)
         ("C-S-n" . 'emms-next)
         ("C-S-p" . 'emms-previous)
         ("C-c C-c t" . 'emms-smart-browse))
  :config
  (require 'emms-setup)
  (setq emms-show-format "Playing: %s")
  (emms-all)
  (emms-default-players)
  (setq emms-source-file-default-directory "~/Music/")
  (defun emms-notify-and-next ()
    "Send a notification of track and start next."
    (emms-next-noerror)
    (let ((track-name (emms-track-description (emms-playlist-current-selected-track))))
      (notifications-notify
       :title "🎵"
       :body (format "EMMS is now playing: %s" track-name)
       :actions '("Confirm" "I agree" "Refuse" "I disagree")
       ;; FIXME
       :on-action #'(lambda (&rest _args) (switch-to-buffer emms-browser-buffer t t))
       :on-close #'(lambda (&rest _args) nil))))
  (setq emms-player-next-function 'emms-notify-and-next)
  (define-key dired-mode-map "`" 'emms-add-dired))

(use-package calfw
  :bind (("C-c C-a a" . my-open-calendar))
  :config
  (use-package calfw-ical)
  (use-package calfw-cal)
  (use-package calfw-org)
  (defun my-open-calendar ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (cfw:org-create-source "Green")
      (cfw:cal-create-source "Orange")
      ;; (cfw:ical-create-source "Moon" "~/moon.ics" "Gray")  ; ICS source1
      ;; (cfw:ical-create-source "gcal" "https://..../basic.ics" "IndianRed")
                                        ; google calendar ICS
      ))))

(use-package org-mu4e
  :straight (:local-repo
             "~/.nix-profile/share/emacs/site-lisp/mu4e"
             :type built-in))

(use-package mu4e-contrib
  :straight (:local-repo
             "~/.nix-profile/share/emacs/site-lisp/mu4e"
             :type built-in))

(use-package mu4e-icalendar
  :straight (:local-repo
             "~/.nix-profile/share/emacs/site-lisp/mu4e"
             :type built-in))

(use-package mu4e
  :straight (:local-repo
             "~/.nix-profile/share/emacs/site-lisp/mu4e"
             :type built-in)
  :commands (mu4e)
  :bind ("C-c m" . mu4e)
  :config
  (setq mu4e-html2text-command 'mu4e-shr2text)
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)
  (setq shr-use-colors t)
  (setq mu4e-view-html-plaintext-ratio-heuristic  most-positive-fixnum)
  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
  (setq mu4e-compose-crypto-reply-plain-policy 'sign)
  (setq mu4e-attachment-dir "~/Downloads")
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-completing-read-function #'ivy-completing-read)
  (setq mu4e-compose-complete-addresses t)
  (setq mu4e-compose-context-policy 'ask-if-none)
  (setq mu4e-compose-dont-reply-to-self t)
  (setq mu4e-compose-keep-self-cc nil)
  (setq mu4e-compose-format-flowed t)
  (setq mu4e-view-show-images t
        mu4e-show-images t
        mu4e-view-image-max-width 800)
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-maildir "~/Maildir")
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-headers-date-format "%d-%m-%Y %H:%M")
  (setq mu4e-headers-fields '((:human-date . 20)
                              (:flags . 6)
                              (:mailing-list . 10)
                              (:from . 22)
                              (:subject)))
  (setq mu4e-headers-include-related t)
  (setq mu4e-sent-messages-behavior 'sent)
  (setq mu4e-view-show-addresses t)
  (setq mm-sign-option 'guided)
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-view-fields '(:from :to :cc
                                 :bcc :subject :flags
                                 :date :maildir :mailing-list
                                 :tags :attachments :signature :decryption))
  (setq mu4e-headers-date-format "%+4Y-%m-%d")
  (setq mail-user-agent 'mu4e-user-agent)
  ;; (setq mu4e-icalendar-diary-file diary-file)
  (setq mu4e-compose-signature-auto-include t)
  (setq mu4e-compose-signature "Kind regards/Med venlig hilsen,\nSergei")
  (mu4e-icalendar-setup)
  (setq gnus-icalendar-org-capture-file org-default-notes-file)
  (setq gnus-icalendar-org-capture-headline '("Agenda"))
  (gnus-icalendar-org-setup)

  (add-hook 'mu4e-view-mode-hook
            (lambda()
              (local-set-key (kbd "<RET>") 'mu4e~view-browse-url-from-binding)
              (local-set-key (kbd "<tab>") 'shr-next-link)
              (local-set-key (kbd "<backtab>") 'shr-previous-link)))

  (setq mu4e-contexts
        `(
	  ,(make-mu4e-context
	    :name "Gmail Account"
	    :match-func (lambda (msg)
			  (when msg
			    (mu4e-message-contact-field-matches
			     msg '(:from :to :cc :bcc) "kaptch@gmail.com")))
	    :vars '(
		    (mu4e-trash-folder . "/kaptch/Trash")
		    (mu4e-refile-folder . "/kaptch/[Gmail]/Archive")
		    (mu4e-drafts-folder . "/kaptch/[Gmail]/Drafts")
		    (mu4e-sent-folder . "/kaptch/[Gmail]/Sent Mail")
		    (user-mail-address  . "kaptch@gmail.com")
		    (user-full-name . "Sergei Stepanenko")
                    (smtpmail-smtp-user . "kaptch@gmail.com")
                    (mu4e-compose-signature
                     . "Kind regards/Med venlig hilsen,\nSergei")
                    (smtpmail-default-smtp-server . "smtp.gmail.com")
                    (smtpmail-smtp-server . "smtp.gmail.com")
                    (smtpmail-smtp-service . 587)))

	  ,(make-mu4e-context
	    :name "Outlook Account"
	    :match-func
            (lambda (msg) (when msg
			    (string-prefix-p "/au"
                                             (mu4e-message-field msg :maildir))))
	    :vars '(
		    (mu4e-trash-folder . "/au/Junk")
		    (mu4e-refile-folder . "/au/Archive")
		    (mu4e-drafts-folder . "/au/Drafts")
		    (mu4e-sent-folder . "/au/Sent")
		    (user-mail-address . "sergei.stepanenko@cs.au.dk")
                    (smtpmail-smtp-user . "au671308@uni.au.dk")
                    (mu4e-compose-signature
                     . "Kind regards/Med venlig hilsen,\nSergei")
                    (smtpmail-smtp-server . "localhost")
                    (smtpmail-smtp-service . 1025))))))

(use-package mu4e-alert
  :init
  (defun perso--mu4e-notif ()
    "Display both mode line and desktop alerts for incoming new emails."
    (interactive)
    (mu4e-update-mail-and-index 1)
    (mu4e-alert-enable-mode-line-display)
    (mu4e-alert-enable-notifications))
  (defun perso--mu4e-refresh ()
    "Refresh emails every 300 seconds and display desktop alerts."
    (interactive)
    (mu4e t)
    (run-with-timer 0 300 'perso--mu4e-notif))
  :after mu4e
  :bind ("C-c C-u" . perso--mu4e-refresh)
  :config
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  (mu4e-alert-set-default-style 'libnotify)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  (setq mu4e-alert-interesting-mail-query
        (concat
         "flag:unread AND maildir:/kaptch/Inbox "
         "OR "
         "flag:unread AND maildir:/au/Inbox "
         )))

(use-package esup)

(use-package dired
  :straight nil
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
         ("@" . dired-run-command))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq-default dired-listing-switches "-alh")
  (setq dired-recursive-copies 'always))

(use-package dired-subtree
  :straight nil
  :bind (:package dired
                  :map dired-mode-map
                  ("<tab>" . dired-subtree-toggle)
                  ("TAB" . dired-subtree-toggle)
                  ("<backtab>" . dired-subtree-cycle)))

(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar)
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar)
         ("C-x C-m" . dired-sidebar-jump-to-sidebar))
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

(use-package popper
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
	  "\\*Warnings\\*"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package bbdb)

(use-package counsel-bbdb)

(use-package ivy
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
  (ivy-mode)
  :config
  (defvar projectile-completion-system)
  (defvar magit-completing-read-function)
  (defvar projector-completion-system)
  (setq projectile-completion-system 'ivy
        magit-completing-read-function 'ivy-completing-read)
  :commands (ivy-completing-read
             ivy-completion-in-region
             swiper)
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t))

(use-package lsp-ivy
  :after (ivy lsp))

(use-package counsel
  :after ivy
  :bind (("C-c y" . counsel-yank-pop)
         ("M-i" . counsel-imenu)
         ("C-x b" . counsel-ibuffer)
         ("C-c r" . counsel-recentf)
         ("C-c C-j" . counsel-org-goto)
         ("C-c o" . counsel-semantic-or-imenu))
  :init (counsel-mode))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package fill-column-indicator
  :commands (fci-mode))

(use-package eldoc
  :defer t
  :diminish
  :hook (prog-mode . turn-on-eldoc-mode))

(use-package which-key
  :defer nil
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3)
  (which-key-setup-side-window-bottom))

(use-package async
  :config (require 'smtpmail-async))

(use-package saveplace
  :defer t
  :config
  (save-place-mode 1))

(use-package elf-mode
  :magic ("ELF" . elf-mode))

(use-package elisp-mode
  :straight nil
  :interpreter (("emacs" . emacs-lisp-mode)))

(use-package bm
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
  :init
  (global-company-mode))

(use-package tuareg
  :commands (camldebug tuareg-imenu-set-imenu)
  :custom
  (tuareg-opam-insinuate t)
  :hook ((tuareg-mode-hook . tuareg-imenu-set-imenu)
         (tuareg-mode-hook . lsp-deferred)
         (tuareg-mode-hook . yas-minor-mode)))

(use-package dune)

(use-package utop
  :config
  (add-hook 'tuareg-mode-hook #'utop-minor-mode))

(use-package emacs
  :init
  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  :bind (("C-c d" . 'gud-gdb)))

(use-package ibuf-ext
  :straight nil
  :bind (("C-x C-b" . 'ibuffer)))

(use-package eww
  :straight nil
  :bind (("C-c w" . 'eww))
  :custom
  (browse-url-browser-function 'eww-browse-url)
  (shr-use-colors nil)
  (shr-bullet "• ")
  (shr-folding-mode t)
  (eww-search-prefix "https://duckduckgo.com/html?q=")
  (url-privacy-level '(email agent cookies lastloc)))

(use-package erc
  :straight nil
  :custom
  (erc-autojoin-channels-alist '(("libera.chat" "#ocaml" "#haskell" "#bash"
                                  "#emacs" "#nixos" "#coq" "#latex" "#org-mode"
                                  "#rust" "#typetheory")))
  (erc-autojoin-timing 'ident)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-threshold-time 43200)
  (erc-prompt-for-nickserv-password nil)
  (erc-prompt-for-password nil)
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))
  :config
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)
  (erc-services-mode 1)
  (erc-update-modules)
  (defun erc-start-or-switch ()
    "Connects to ERC, or switch to last active buffer."
    (interactive)
    (if (get-buffer "irc.libera.chat:6667")
        (erc-track-switch-buffer 1)
      (when (y-or-n-p "Start ERC? ")
        (erc-tls :server "irc.libera.chat" :port 6697))))
  :bind (("C-c i" . 'erc-start-or-switch)))

(use-package erc-hl-nicks
  :after erc)

(use-package erc-image
  :after erc)

(use-package shell
  :straight nil
  :bind (("M-@" . 'shell)))

(use-package ielm
  :straight nil
  :bind ("C-c :" . ielm))

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

(use-package discover)

(use-package python-mode
  :init
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-enabled-clients 'pylsp))
  :hook ((python-mode . lsp-deferred)
         (python-mode . yas-minor-mode)
         (python-mode . add-hook-lsp-organize-imports)
         )
  :config
  (use-package pipenv
    :hook (python-mode . pipenv-mode)
    :init
    (setq
     pipenv-projectile-after-switch-function
     #'pipenv-projectile-after-switch-extended)))

(use-package haskell-mode
  :commands haskell-mode
  :init
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-enabled-clients 'lsp-haskell))
  :config
  (use-package lsp-haskell)
  :hook ((haskell-mode . lsp-deferred)
         (haskell-mode . yas-minor-mode)
         (haskell-mode . add-hook-lsp-organize-imports)
         (haskell-mode . add-hook-lsp-format-buffer)))

(use-package solidity-mode
  :config
  (use-package company-solidity)
  (setq solidity-comment-style 'slash)
  (setq solidity-flycheck-solc-checker-active t)
  (setq flycheck-solidity-solc-addstd-contracts t)
  (define-key solidity-mode-map (kbd "C-c C-g") 'solidity-estimate-gas-at-point))

(use-package purescript-mode
  :init
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-enabled-clients 'pursls))
  :hook ((purescript-mode . lsp-deferred)
         (purescript-mode . yas-minor-mode)
         (purescript-mode . add-hook-lsp-format-buffer)
         (purescript-mode . add-hook-lsp-organize-imports)
         (purescript-mode . turn-on-purescript-indentation)))

;; (use-package web-mode
;;   :mode (("\\.js\\'" . web-mode)
;;          ("\\.jsx\\'" . web-mode)
;;          ("\\.ts\\'" . web-mode)
;;          ("\\.tsx\\'" . web-mode)
;;          ("\\.html\\'" . web-mode)
;;          ("\\.vue\\'" . web-mode)
;; 	 ("\\.json\\'" . web-mode))
;;   :commands web-mode
;;   :config
;;   (setq web-mode-content-types-alist
;; 	'(("jsx" . "\\.js[x]?\\'")))
;;   )

(use-package js
  :init
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-enabled-clients 'ts-ls))
  :hook ((js-mode . lsp-deferred)
         (js-mode . yas-minor-mode)
         (js-mode . add-hook-lsp-format-buffer)
         (js-mode . add-hook-lsp-organize-imports)))

(use-package go-mode
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
  (use-package gorepl-mode)
  :hook ((go-mode . lsp-deferred)
         (go-mode . gorepl-mode)
         (go-mode . yas-minor-mode)
         (go-mode . kaptch/go-hook)
         (go-mode . add-hook-lsp-format-buffer)
         (go-mode . add-hook-lsp-organize-imports))
  :bind (("C-c C-c C-r" . go-run-main)))

(use-package erlang
  :init
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-enabled-clients 'erlang-ls))
  :hook ((erlang-mode . lsp-deferred)
         (erlang-mode . yas-minor-mode)
         (erlang-mode . add-hook-lsp-format-buffer)
         (erlang-mode . add-hook-lsp-organize-imports)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-position 'bottom)
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-mode
  :init
  :commands (lsp lsp-deferred)
  :custom
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-keymap-prefix "C-c k")
  :config
  (define-key lsp-mode-map (kbd "C-c k") lsp-command-map)
  :hook ((lsp-mode . lsp-enable-which-key-integration)))

(use-package dap-mode
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

(use-package flycheck
  :init
  (setq-default flycheck-disabled-checkers '(coq))
  (global-flycheck-mode))

(use-package ht)

(use-package f)

(use-package dash)

(use-package lv)

(use-package markdown-mode)

(use-package ispell)

(use-package direnv
  :demand t
  :config
  (direnv-mode)
  (eval-after-load 'flycheck
    '(setq flycheck-executable-find
           (lambda (cmd)
             (direnv-update-environment default-directory)
             (executable-find cmd))))
  :hook
  (coq-mode . direnv-update-environment))

(use-package ssh
  :init
  (add-hook 'ssh-mode-hook
            (lambda ()
              (setq ssh-directory-tracking-mode t)
              (shell-dirtrack-mode t)
              (setq dirtrackp nil))))

(use-package proof-general
  :init
  (setq company-coq-live-on-the-edge t)
  :config
  (use-package alectryon)
  (use-package company-coq
    :custom
    (company-coq-disabled-features '(prettify-symbols)))
  (add-hook 'coq-mode-hook #'company-coq-mode)
  (add-hook 'coq-mode #'(lambda () (progn (undo-tree-mode 1))))
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
  (proof-three-window-mode-policy 'hybrid)
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
  :init
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'rls)
    (add-to-list 'lsp-enabled-clients 'rust-analyzer))
  :hook ((rust-mode . lsp-deferred)
         (rust-mode . (lambda () (setq indent-tabs-mode nil)))
         (rust-mode . (lambda () (prettify-symbols-mode)))
         (rust-mode . yas-minor-mode)
         (rust-mode . add-hook-lsp-format-buffer)
         (rust-mode . add-hook-lsp-organize-imports))
  :config
  (setq rust-format-on-save t)
  (use-package rustic
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
  :init
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-fancy slime-quicklisp slime-asdf))
  (defun cliki:start-slime ()
    (unless (slime-connected-p)
      (save-excursion (slime))))
  :hook (slime-mode . cliki:start-slime))

(use-package macrostep
  :bind (("C-c e" . 'macrostep-expand)))

(use-package spinner)

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package perspective
  :demand t
  :bind (("C-M-k" . persp-switch)
         ("C-M-n" . persp-next)
         ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-mode-prefix-key (kbd "C-c C-p"))
  (persp-initial-frame-name "Main")
  :config
  (persp-mode))

(use-package magit
  :diminish auto-revert-mode
  :bind
  (("C-c C-m" . magit-status)
   :map magit-status-mode-map
   ("q" . magit-quit-session))
  :config
  (setq magit-process-password-prompt-regexps
        '("^\\(Enter \\)?[Pp]assphrase\\( for \\(RSA \\)?key '.*'\\)?: ?$"
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
  :commands (magit-todos-list))

(use-package magit-delta
  :config
  (add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))
  (setq magit-delta-delta-args
        '("--24-bit-color" "always"
          "--features" "magit-delta"
          "--color-only")))

(use-package git-commit
  :hook ((git-commit-mode . flyspell-mode)
	 (git-commit-mode . git-commit-save-message)
	 (git-commit-mode . turn-on-auto-fill)))

(use-package nix-mode)

(use-package epg
  :config
  (setq epa-pinentry-mode 'ask))

(use-package pinentry
  :config
  (pinentry-start))

(use-package pdf-tools
  :init
  (pdf-tools-install)
  :config
  (add-hook 'pdf-view-mode-hook
            (lambda () (local-set-key (kbd "C-s") #'isearch-forward)))
  (add-hook 'pdf-view-mode-hook
            (lambda () (local-set-key (kbd "C-r") #'isearch-backward)))
  )

(use-package tex
  :defer t
  :straight auctex
  :straight ispell
  :straight flyspell
  :init
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-enabled-clients 'ltex-ls))
  (add-hook 'TeX-mode-hook 'flyspell-mode)
  (add-hook 'TeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook
            (lambda()
              (add-to-list 'TeX-command-list
                           '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
              (setq TeX-save-query nil)
              (setq TeX-show-compilation nil)))
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  :hook ((TeX-mode . lsp-deferred))
  :config
  (use-package lsp-latex)
  (defun BibTeX-auto-store ()
    "This function should be called from `bibtex-mode-hook'.
It will setup BibTeX to store keys in an auto file."
    ;; We want this to be early in the list, so we do not
    ;; add it before we enter BibTeX mode the first time.
    (add-hook 'write-contents-functions #'TeX-safe-auto-write nil t)
    (TeX-bibtex-set-BibTeX-dialect)
    (set (make-local-variable 'TeX-auto-untabify) nil)
    (set (make-local-variable 'TeX-auto-parse-length) 999999)
    (set (make-local-variable 'TeX-auto-regexp-list) BibTeX-auto-regexp-list)
    (set (make-local-variable 'TeX-master) t))
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
  (setq synctex-number "1")
  (setq pdf-sync-backward-display-action t
        pdf-sync-forward-display-action t
        TeX-source-correlate-method '(
                                      (dvi . source-specials)
                                      (pdf . synctex))
        reftex-plug-into-AUCTeX t
        shell-escape-mode "-shell-escape"
        auto-update-latex-preview-pane 'off)
  (add-hook 'TeX-after-compilation-finished-functions
		        #'TeX-revert-document-buffer)
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
  (setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) "Zathura")
  ;; (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
  ;;       TeX-source-correlate-start-server t)

  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  :bind
  (("C-c q" . pdf-sync-forward)))

(use-package latex-change-env
  :after latex
  :commands latex-change-env
  :bind (:map LaTeX-mode-map ("C-c r" . latex-change-env))
  :custom
  (latex-change-env-math-display '("\\[" . "\\]"))
  (latex-change-env-math-inline  '("$"   . "$")))

(use-package ob-async)

(use-package org-contrib
  :after org
  :config
  (setq org-src-fontify-natively t)
  (setq org-confirm-babel-evaluate nil)
  (setq ob-async-no-async-languages-alist '("ipython"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (shell . t)
     (python . t)
     (ocaml . t)
     (haskell . t)
     (coq . t)
     (latex . t)
     )))

(use-package org
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
  (("C-c c" . org-capture)
   ("C-c l" . org-store-link))
  :config
  (setq org-default-priority ?A)
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?D)
  (setq org-priority-faces '((?A . (:foreground "#FF0000" :weight bold))
                             (?B . (:foreground "#FF9815" :weight bold))
                             (?C . (:foreground "#68DF40"))
                             (?D . (:foreground "#11D3FF"))))
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "IN-PROGRESS(p!)" "WAIT(w!)" "CANCELED(c!)" "DONE(d!)"))
        org-todo-keyword-faces
        '(("TODO" . (:foreground "magenta" :weight bold))
          ("WAIT" . (:foreground "green" :weight bold))
          ("IN-PROGRESS" . (:foreground "blue" :weight bold))
          ("CANCELED" . (:foreground "white" :background "#4d4d4d" :weight bold))
          ("DONE" . "#333"))
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t)
  (add-to-list 'org-file-apps
               '("\\.pdf\\'" . (lambda (file link)
                                 (org-pdfview-open link))))
  (setq org-agenda-archives-mode t)
  (setq org-capture-templates
        '(("t" "Task" entry (file+headline org-default-notes-file "Agenda")
           "* %^{This is a?||TODO |IN-PROGRESS |WAIT |DONE} [#%^{Priority?||A|B|C|D}] %^{Title}\nScheduled: %^t\n%u\n%?\n\n\n" :empty-lines-after 1)
          ("i" "Idea" entry (file+headline org-default-notes-file "Ideas")
           "* %?\n  %u\n\n" :empty-lines-after 1)
          ("c" "Code Snippet" entry (file+headline org-default-notes-file "Code Snippets")
           "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC" :empty-lines-after 1)
          ("r" "Kill ring" entry (file+headline org-default-notes-file "Kill ring")
           "%^C" :empty-lines-after 1)
          ("m" "Meeting" entry (file+headline org-default-notes-file "Notes")
	   "* Meeting with %?\n:MEETING:\n%t\n\n" :empty-lines-after 1 :clock-in t :clock-resume t)
          ))
  (defun add-newline-at-end-if-none ()
    "Add a newline at the end of the buffer if there isn't any."
    (save-excursion
      (save-restriction
        (goto-char (1- (point-max)))
        (if (not (looking-at "\n\n"))
            (progn
              (goto-char (point-max))
              (insert "\n"))))))

  (add-hook 'org-capture-before-finalize-hook 'add-newline-at-end-if-none)

  (setq org-startup-folded t)

  (defun set-org-agenda-files ()
    "Set different org-files to be used in `org-agenda`."
    (setq org-agenda-files (cons org-default-notes-file nil)))

  (set-org-agenda-files)

  (defun org-main-file ()
    "Open main 'org-mode' file and start 'org-agenda' for today."
    (interactive)
    (find-file org-default-notes-file)
    (set-org-agenda-files)
    (org-agenda-list)
    (org-agenda-day-view)
    (shrink-window-if-larger-than-buffer)
    (other-window 1))

  (setq org-deadline-warning-days 3))

(use-package org-journal
  :defer t
  :bind
  ("M-p o" . org-journal-new-entry)
  ("M-p s" . org-journal-new-scheduled-entry)
  ("M-p n" . org-journal-next-entry)
  ("M-p p" . org-journal-previous-entry)
  :init
  (setq
   org-journal-prefix-key "M-p "
   org-journal-enable-agenda-integration t)
  :config
  (setq org-journal-dir (concat org-directory "/journal")
        org-journal-file-type 'weekly
        org-journal-file-format "%Y%m%d.org"
        org-journal-date-format "%A, %Y-%m-%d"))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-agenda
  :straight nil
  :after org
  :bind
  ("C-c a" . org-agenda)
  :custom
  (org-agenda-include-diary t)
  (org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                              (todo . " %i %-12:c%l")
                              (tags . " %i %-12:c")
                              (search . " %i %-12:c")))
  (org-agenda-start-on-weekday nil))

(use-package go-translate
  :commands (gts-do-translate)
  :bind
  (("C-c C-t t" . gts-do-translate))
  :init
  (setq gts-translate-list '(("dk" "en") ("en" "dk") ("fr" "en") ("en" "fr")))
  :config
  (setq gts-default-translator
        (gts-translator
         :picker
         (lambda ()
           (cond ((equal major-mode 'pdf-view-mode)
                  (gts-noprompt-picker :texter (gts-current-or-selection-texter)))
                 (t (gts-prompt-picker))))
         :engines
         (gts-google-engine :parser (gts-google-summary-parser))
         :render
         (lambda ()
           (cond ((equal major-mode 'pdf-view-mode)
                  (gts-posframe-pop-render))
                 (t (gts-buffer-render)))))))

(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package typing)

(use-package elisp-mode
  :straight nil
  :bind (("C-j" . eval-last-sexp)))

(use-package lean4-mode
  :straight (lean4-mode
	     :type git
	     :host github
	     :repo "leanprover/lean4-mode"
	     :files ("*.el" "data"))
  :commands (lean4-mode))

;; (use-package codegpt
;;   :custom
;;   (codegpt (getenv "OPENAI_API_KEY"))
;;   (codegpt-focus-p nil))

;; (use-package ujelly-theme
;;   :config
;;   (load-theme 'ujelly t))

;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions
;; 	      (lambda (frame)
;; 		(with-selected-frame frame (load-theme 'ujelly t))))
;;   (load-theme 'ujelly t))

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

(add-to-list 'auto-mode-alist '(".*mutt.*" . message-mode))

(add-hook 'compilation-finish-functions
          (lambda (buf str)
            (if (null (string-match ".*exited abnormally.*" str))
                (progn
                  (run-at-time
                   "2 sec" nil 'delete-windows-on
                   (get-buffer-create "*compilation*"))
                  (message "No Compilation Errors!")))))

(display-time-mode 1)
(global-visual-line-mode 1)
(global-hl-line-mode 1)
(display-battery-mode 1)
(tool-bar-mode -1)
;; (scroll-bar-mode -1)

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

(setq display-time-default-load-average nil)
(setq display-time-load-average nil)

(setq gc-cons-threshold 20000000)

(setq make-backup-files nil)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(setq sentence-end-double-space nil)

(setq initial-scratch-message "")

;; (setq debug-on-error t)

(global-auto-revert-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)

(set-frame-parameter nil 'alpha nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq
 whitespace-line-column 80
 whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook #'whitespace-mode)

;; (setq warning-suppress-types '((emacs) (emacs) (emacs) (emacs)))

(unless (fboundp 'x-select-font)
  (defalias 'x-select-font 'pgtk-popup-font-panel
    "Pop up the font panel. This function has been overloaded in Nextstep."))

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(require 'agda2-mode)

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

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
