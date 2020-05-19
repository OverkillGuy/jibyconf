;; Only useful if emacs < 26.3
;; Fixes "bad request" from ELPA
(if (not (string= emacs-version "26.3"))
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(setq package-archives
      '(("Gnu" . "https://elpa.gnu.org/packages/")
       ;("marmalade" . "https://marmalade-repo.org/packages/")
	("Melpa" . "https://melpa.org/packages/")
	("Org" . "https://orgmode.org/elpa/"))
      package-archive-priorities
      '(("Org"    . 10)
	("Melpa"  . 5)
	("Gnu"    . 0)))

; For "Failed to verify signature [pkgname]":
; see https://emacs.stackexchange.com/a/52823
; or https://elpa.gnu.org/packages/gnu-elpa-keyring-update.html

(package-initialize)

(if (not (locate-library "use-package"))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

(setq use-package-always-ensure t)

(use-package bug-hunter)

(use-package org-plus-contrib
  :pin "Org"
  :config (require 'org-man))

(use-package esup)

(use-package beacon
  :config
  (beacon-mode 1)
  :diminish beacon-mode
  :bind ("<pause>" . beacon-blink))

(use-package rainbow-mode)

(use-package diminish)

(use-package rainbow-delimiters
  :init
  (setq rainbow-delimiters-max-face-count 7)
  :config
  (set-face-foreground 'rainbow-delimiters-depth-1-face "dark orange")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "deep pink")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "chartreuse")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "deep sky blue")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "yellow")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "orchid")
  (set-face-foreground 'rainbow-delimiters-depth-7-face "spring green")
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :inverse-video t
                      :foreground "red"
                      :inherit 'rainbow-delimiters-base-face)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key))

(use-package which-key
  :diminish which-key-mode
  :custom
  (which-key-popup-type 'side-window)
  (which-key-side-window-location '(right bottom))
  :config
  (which-key-mode))

(use-package projectile
  :diminish projectile-mode
  :bind-keymap
  ("<f7>" . projectile-command-map)
  :custom (projectile-project-search-path
	   '("~/dev/" "~/org/"))
  :config
  (projectile-mode 1))

(use-package helm
  :diminish 'helm-mode
  :config
  (helm-mode 1)
  (global-set-key (kbd "C-x b") 'helm-mini))

(use-package helm-projectile
  :config (helm-projectile-on)
  ;; Show git status when project is switched
  (setq projectile-switch-project-action 'magit-status))

(global-set-key (kbd "S-<f7>") 'helm-projectile-switch-project)

(global-set-key (kbd "C-c s") 'projectile-ag)

(global-set-key (kbd "C-c %") 'projectile-replace)
(global-set-key (kbd "C-c M-%") 'projectile-replace-regexp)

;; Remap TAB for completion
;; Source: https://emacs.stackexchange.com/questions/33727/how-does-spacemacs-allow-tab-completion-in-helm#38235
;; https://writequit.org/denver-emacs/presentations/2016-03-01-helm.html
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
;; ;; make TAB works in terminal, C-i is tha same as TAB
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)

(use-package wgrep)

(use-package wgrep-ag
  :config
  (autoload 'wgrep-ag-setup "wgrep-ag")
  (add-hook 'ag-mode-hook 'wgrep-ag-setup)
  :after wgrep ag)

(setq org-startup-folded nil)

(setq org-startup-indented t)

(setq org-image-actual-width nil)

(setq org-src-ask-before-returning-to-edit-buffer nil)

(setq org-startup-with-inline-images t)

(add-hook 'org-mode-hook 'auto-fill-mode)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
; (global-set-key (kbd "C-c b")  'org-iswitchb)
(global-set-key (kbd "C-c C-x C-o") 'org-clock-out)
(global-set-key (kbd "C-c C-x C-j") 'org-clock-jump-to-current-clock)

;; Orgmode code-execution support my languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (plantuml . t)
   (ditaa . t)
   (dot . t)
   (C . t)
   (shell . t)
   ))

(setq org-babel-default-header-args:sh
      '((:results . "output") (:shebang . "#!/bin/bash -l")))

(setq org-babel-default-header-args:python
      '((:results . "output")))

(setq org-capture-templates
      '(("h" "Command line trick idea" entry
         (file "~/dev/notes/command_line_tricks.org")
         "* FLUFF %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n\n#+BEGIN_SRC shell\n%i\n#+END_SRC\n  %a")
        ("p" "Dev project idea" entry
         (file "~/org/dev_projects.org")
         "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
        ("c" "Calendar entry" entry
         (file "~/dev/notes/calendar.org")
         "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%a")
	("P" "Protocol" entry
	 (file "~/org/notes.org")
        "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
	("L" "Protocol Link" entry
	 (file "~/org/notes.org")
        "* %? [[%:link][%:description]] \nCaptured On: %U")))

(setq org-cycle-separator-lines 1)

(setq org-export-with-section-numbers nil)

(setq org-todo-keywords
      '((sequence "TODO(t@)" "MEETING" "WAIT(w@/!)" "DOING(i!)" "|" "DONE(d!@)" "CANCELED(c@)")))

(setq org-log-into-drawer t)

(setq org-time-clocksum-use-effort-durations t)

(setq org-src-fontify-natively t
      org-adapt-indentation nil
      org-src-preserve-indentation t)

(defface org-block-background
  '((t (:background "#444")))
  "Face used for the source block background.")

(setq org-src-block-faces '(("emacs-lisp" org-block-background)
			    ("c++" org-block-background)
			    ("python" org-block-background)
			    ("shell" org-block-background)))

(setq org-ditaa-jar-path  "~/.emacs.d/scripts/ditaa.jar")

(setq org-confirm-babel-evaluate nil)

(add-hook 'ediff-prepare-buffer-hook #'show-all)

(use-package ob-async)

(setq org-export-with-sub-superscripts "{}")
(setq org-use-sub-superscripts "{}")

(use-package org-re-reveal
  :custom
  ; First slide is title + subtitle + author + #+REVEAL_TALK_URL
  (org-re-reveal-title-slide
	"<h1>%t</h1><h4>%s</h4><p>%a - <a href=\"%u\">%u</a><p>\n<p>%d </p>"))

;; (setq org-re-reveal-root (concat "file://" (getenv "HOME") "/dev/foss/reveal.js/"))

(setq org-re-reveal-script-files '("js/reveal.js"))

(setq org-html-validation-link nil)



(use-package ox-hugo
  :after ox
  :custom ( org-hugo-section "post"))

(use-package org-ref
  :defer t
  :config
  (setq org-ref-insert-cite-key "C-c )"))

(require 'org-id)
(add-hook 'org-insert-heading-hook #'org-id-get-create)

(defvar org-created-property-name "CREATED"
  "The name of the org-mode property that stores the creation date of the entry")

(defun org-set-created-property (&optional active NAME)
  "Set a property on the entry giving the creation time.

By default the property is called CREATED. If given the `NAME'
argument will be used instead. If the property already exists, it
will not be modified."
  (interactive)
  (let* ((created (or NAME org-created-property-name))
         (fmt (if active "<%s>" "[%s]"))
         (now  (format fmt (format-time-string "%Y-%m-%d %a %H:%M"))))
    (unless (org-entry-get (point) created nil)
      (org-set-property created now))))

(add-hook 'org-insert-heading-hook #'org-set-created-property)

(setq org-catch-invisible-edits 'show-and-error)

(setq org-cycle-separator-lines 0)

(use-package company
  :diminish 'company-mode
  :config (add-hook 'after-init-hook 'global-company-mode))

(use-package company-quickhelp
  :config (company-quickhelp-mode))

(use-package modern-cpp-font-lock
  :config (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

(use-package irony
  :disabled
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :disabled
  :config
  (add-to-list 'company-backends 'company-irony))

(add-to-list 'auto-mode-alist '("\\.action\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.srv\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.msg\\'" . yaml-mode))

(use-package slime
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy slime-asdf)))

(defun compilation-finished-unfocused-notify (buffer desc)
  "Popup via libnotify on compilation finished with unfocused window"
  (interactive)
  (if (not (eq buffer
	       (window-buffer (selected-window))))
      (alert
       (format "Compilation %s"
	       (if (string-equal "finished\n" desc)
		   "succeeded"
		 "failed"))
       :title "Emacs"
       :category 'emacs :style 'libnotify
       :icon "gnome-inhibit-applet")))
(add-hook 'compilation-finish-functions 'compilation-finished-unfocused-notify)

(use-package rmsbolt)

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (add-to-list 'yas-snippet-dirs
	       (expand-file-name "snippets/"))
  (yas-global-mode 1)
  ;; Fix indentation of snippets in yaml
  ;; https://github.com/joaotavora/yasnippet/issues/1020#issuecomment-539787929
  (add-hook 'yaml-mode-hook
          '(lambda () (set (make-local-variable 'yas-indent-line) 'fixed))))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package git-gutter
  :config (global-git-gutter-mode +1)
  :demand t  ;; no lazy-loading allowed!
  :diminish 'git-gutter-mode
  :bind
  ("C-x v s" .  git-gutter:stage-hunk)
  ("M-n" .  git-gutter:next-hunk)
  ("M-p" .  git-gutter:previous-hunk))

(use-package magit
  :custom
  ; don't ask before saving files
  (magit-save-repository-buffers 'dontask)
  ;; Only use 10 chars for log margin (not 18)
  (magit-log-margin '(t age magit-log-margin-width t 10)))

(setq magit-diff-refine-hunk 'all)

(global-set-key (kbd "M-<f12>") 'magit-status)

(global-set-key (kbd "S-<f12>") 'magit-log-all-branches)

(use-package orgit
  :after magit)

(use-package tex-mode
  :ensure auctex
  :config
  (setq tex-engine 'xetex))

(use-package latex-preview-pane
  :after tex-mode
  :config
  (latex-preview-pane-enable))

(use-package lorem-ipsum)

(use-package ox-rst)

(use-package markdown-mode)

(use-package ldap)

(use-package ansible)

(use-package dockerfile-mode)

(use-package docker-compose-mode)

(add-to-list 'auto-mode-alist '("Vagrantfile\\'" . ruby-mode))

(use-package json-mode)

(use-package php-mode)

(use-package feature-mode)

(use-package cmake-mode)

(use-package protobuf-mode)

(use-package geiser
  :custom ( geiser-default-implementation 'guile))

(use-package groovy-mode
  :custom (groovy-indent-offset 2))

(use-package apache-mode)

(use-package restclient
  :config
  ;; Use json-mode instead of default js-mode
  (add-to-list 'restclient-content-type-modes
		'(("application/json" . json-mode))))

(use-package ob-restclient)

(use-package company-restclient
  :disabled  ;; WHAT IS THERE TO COMPLETE ANYWAY
  :after restclient company
  :config (add-to-list 'company-backends 'company-restclient))

(use-package impatient-mode
 :custom ( httpd-host "0.0.0.0"))

(use-package ace-window
  :config
  (global-set-key (kbd "C-;") 'ace-window))

(use-package hideshow
  :bind (("C-c TAB" . hs-toggle-hiding)
         ("C-\\" . hs-toggle-hiding)
         ("M-+" . hs-show-all))
  :init (add-hook #'prog-mode-hook #'hs-minor-mode)
  :diminish hs-minor-mode
  :config
  (setq hs-special-modes-alist
        (mapcar 'purecopy
                '((c-mode "{" "}" "/[*/]" nil nil)
                  (c++-mode "{" "}" "/[*/]" nil nil)
                  (java-mode "{" "}" "/[*/]" nil nil)
                  (js-mode "{" "}" "/[*/]" nil)
                  (json-mode "{" "}" "/[*/]" nil)
                  (javascript-mode  "{" "}" "/[*/]" nil)))))

(use-package json-snatcher)

(use-package goto-addr
  :hook ((compilation-mode . goto-address-mode)
	 (prog-mode . goto-address-prog-mode)
	 (eshell-mode . goto-address-mode)
	 (shell-mode . goto-address-mode)
	 (term-mode . goto-address-mode)
	 (magit-revision-mode . goto-address-mode))
  :bind (:map goto-address-highlight-keymap
	      ("<RET>" . goto-address-at-point)
	      ("M-<RET>" . newline))
  :commands (goto-address-prog-mode
	     goto-address-mode))

(use-package goto-addr
  :hook ((compilation-mode . goto-address-mode)
	 (prog-mode . goto-address-prog-mode)
	 (eshell-mode . goto-address-mode)
	 (shell-mode . goto-address-mode)
	 (term-mode . goto-address-mode)
	 (magit-revision-mode . goto-address-mode))
  :bind (:map goto-address-highlight-keymap
	      ("<RET>" . goto-address-at-point)
	      ("M-<RET>" . newline))
  :commands (goto-address-prog-mode
	     goto-address-mode))

(use-package tramp
  :ensure t
  :custom (tramp-default-method "ssh"))

(use-package docker-tramp
  :after tramp)

(use-package eww-lnum
  :config
  (eval-after-load "eww"
    '(progn (define-key eww-mode-map "f" 'eww-lnum-follow)
	    (define-key eww-mode-map "F" 'eww-lnum-universal))))

(use-package sr-speedbar
  :config
  ; show all filetypes (not just indexed ones)
  (setq speedbar-show-unknown-files t))

(use-package undo-tree
  :config (global-undo-tree-mode)
  :diminish 'undo-tree-mode)

(use-package multi-term
  :config
  (global-set-key (kbd "<f5>") 'multi-term)
  (global-set-key (kbd "<C-next>") 'multi-term-next)
  (global-set-key (kbd "<C-prior>") 'multi-term-prev)
  (setq multi-term-buffer-name "term"
        multi-term-program "/bin/bash"
	term-bind-key-alist
        (list
         (cons "C-c C-j" 'term-line-mode)
         (cons "C-c C-k" 'term-char-mode)
         (cons "C-c C-l" 'comint-clear-buffer)
       (cons "C-c C-c"  'term-interrupt-subjob)
       (cons "C-p" 'previous-line)
       (cons "C-n" 'next-line)
       (cons "M-f" 'term-send-forward-word)
       (cons "M-b" 'term-send-backward-word)
       (cons "M-DEL" 'term-send-backward-kill-word)
       (cons "M-d" 'term-send-forward-kill-word)
       (cons "<C-left>" 'term-send-backward-word)
       (cons "<C-right>" 'term-send-forward-word)
       (cons "C-r" 'term-send-reverse-search-history)
       (cons "M-p" 'term-send-raw-meta)
       (cons "M-y" 'term-send-raw-meta)
       (cons "C-y" 'term-send-raw))))

(add-hook 'term-mode-hook
          (lambda ()
            ;; 下面设置multi-term buffer的长度无限
            (setq term-buffer-maximum-size 0)
            ;; (add-to-list 'term-bind-key-alist '("C-c C-c" . term-interrupt-subjob))
            ; (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
            ; (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))
            ; (add-to-list 'term-bind-key-alist '("C-a" . ab/move-beginning-of-line))
            ; (add-to-list 'term-bind-key-alist '("M-k" . ab/kill-line))
            ; (add-to-list 'term-bind-key-alist '("C-d" . ab/delete-char))
            ; (add-to-list 'term-bind-key-alist '("C-b" . ab/backward-char))
            ; (add-to-list 'term-bind-key-alist '("C-f" . ab/forward-char))
            ; (add-to-list 'term-bind-key-alist '("M-l" . ab/extend-selection)) ;; error
(setq show-trailing-whitespace nil)))

;;narrow dired to match filter
(use-package dired-narrow
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package alert)

(use-package org-wild-notifier
  :defer t
  :config
  ;; Any even in calendar should ring me up
  (setq org-wild-notifier-keyword-whitelist nil
	;; Use property NOTIFY to specify when to remind me (n minutes before)
	org-wild-notifier-alert-times-property "NOTIFY"
	;; By default, notify X minutes before event
	org-wild-notifier-alert-time 3)
  ;; Toggle alerts on launch if not Android, as async fails
  ;; https://github.com/akhramov/org-wild-notifier.el/issues/22
  (if (not (string-match "u[0-9]_a[0-9+]" (user-login-name)))
      (org-wild-notifier-mode)))

(add-to-list 'alert-user-configuration
'(((:title . "Agenda"))
   libnotify nil))

(use-package org-vcard)

(use-package vlf
  :config
  (require 'vlf-setup))

(use-package csv-mode)

;; For Firefox text-editing support, using plugin
;; https://addons.mozilla.org/en-US/firefox/addon/edit-with-emacs1
(use-package edit-server
  :init (edit-server-start)
  :custom
  (edit-server-url-major-mode-alist
        '(("github\\.com" . markdown-mode))))
(use-package company-lsp
    :config
    (push 'company-lsp company-backends))

(use-package dired-launch
  :custom (dired-launch-default-launcher '("xdg-open"))
 :config (dired-launch-enable))

;; Matched with the scheme handler in
;; ~/.local/share/applications/org-protocol.desktop
;; Then cmd `update-desktop-database ~/.local/share/applications/`
(require 'org-protocol)
(use-package dired-rsync)

(use-package exec-path-from-shell
  :config (when (memq window-system '(mac ns x))
	    (exec-path-from-shell-initialize)))

(use-package anzu
  :diminish
  :config
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-anzu-mode +1))
