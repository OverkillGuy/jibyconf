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

(if (not (locate-library "use-package"))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

(setq use-package-always-ensure t)

(use-package esup)

(use-package bug-hunter)

(use-package org
  :ensure org-plus-contrib
  :pin "Org"
  :custom (org-modules  '(ol-man org-id ol-w3m ol-bbdb ol-bibtex ol-docview ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-eww))
  :init ;; Set these variables before load, see `org-export-backends' docstring
  (setq org-export-backends '(koma-letter ascii html latex md)))

(use-package beacon
  :config
  (beacon-mode 1)
  :diminish beacon-mode
  :bind ("<pause>" . beacon-blink))

(use-package rainbow-mode)

(use-package diminish)

(use-package rainbow-delimiters
  :init
  (setq rainbow-delimiters-max-face-count 8)
  :config
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#FF8811")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#8F0")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#55DDFF")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#DBDB59")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#AA22FF")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#080")
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#5978DB")
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#F8F")

  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :inverse-video t
                      :foreground "red"
                      :inherit 'rainbow-delimiters-base-face)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

; Test on parens (uncomment to visualize)
; (((((((()))))))))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  (load-theme 'doom-one t)
  (set-face-foreground 'font-lock-comment-face "#869099")
  (set-face-foreground 'show-paren-match "#ffb44c"))

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
  :bind-keymap ("<f7>" . projectile-command-map)
  :bind
  ("C-c s" . projectile-ag)
  ("C-c %" . projectile-replace)
  ("C-c M-%" . projectile-replace-regexp)
  :custom (projectile-project-search-path
	   '("~/dev/" "~/org/"))
  :config (projectile-mode 1))

(use-package helm
  :diminish helm-mode
  :config
  (helm-mode 1)
  (global-set-key (kbd "C-x b") 'helm-mini))

(use-package helm-projectile
  ;; Show git status when project is switched
  :custom (projectile-switch-project-action 'magit-status)
  :config (helm-projectile-on))

(global-set-key (kbd "S-<f7>") 'helm-projectile-switch-project)

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

(use-package anzu
  :diminish
  :config
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-anzu-mode +1))

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
   ;; (rust . t)
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

(defun jb/open-devlog ()
  (interactive)
  (find-file "~/dev/notes/devlog.org")
  (end-of-buffer))

(global-set-key (kbd "<f8>") 'jb/open-devlog)
(global-set-key (kbd "S-<f8>") 'org-capture)

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

(setq org-re-reveal-script-files '("js/reveal.js"))

(setq org-html-validation-link nil)

(require 'org-tempo)

(use-package ox-hugo
  :after ox
  :custom ( org-hugo-section "post"))

(use-package org-ref
  :defer t
  :config
  (setq org-ref-insert-cite-key "C-c )"))

(add-to-list 'org-modules 'org-id)
(add-hook 'org-insert-heading-hook #'org-id-get-create)

(add-to-list 'org-modules 'ol-man)

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

(use-package orglink
  :init
  (add-hook 'prog-mode #'orglink-mode)
  (add-hook 'text-mode #'orglink-mode))

(use-package company
  :diminish company-mode
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

(use-package geiser
  :custom (geiser-default-implementation 'guile))

(use-package geiser-racket)
  ;; :custom (geiser-default-implementation 'racket)


(use-package geiser-guile)
  ;; :custom (geiser-default-implementation 'guile)

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
  ;; Fix indentation of snippets in yaml
  ;; https://github.com/joaotavora/yasnippet/issues/1020#issuecomment-539787929
  (add-hook 'yaml-mode-hook
          '(lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package git-gutter
  :config (global-git-gutter-mode +1)
  :demand t  ;; no lazy-loading allowed I need that one!
  :diminish git-gutter-mode
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

(use-package forge
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

(use-package groovy-mode
  :custom (groovy-indent-offset 2))

(use-package apache-mode)

(use-package csv-mode)

(use-package terraform-mode)

(use-package rust-mode
  :custom (rust-format-on-save t)
  :hook (rust-mode-hook . (lambda () (setq indent-tabs-mode nil))))

(use-package cargo
  :hook (rust-mode-hook . cargo-minor-mode))

(use-package k8s-mode
  ;; Workaround for https://github.com/TxGVNN/emacs-k8s-mode/issues/9
  ;; :init  (add-hook 'k8s-mode-hook
  ;; 		   (lambda () (yas-load-directory k8s-snip-dir)))
  )

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

(use-package poetry
  :ensure t
  :hook
  ;; activate poetry-tracking-mode when python-mode is active
  (python-mode . poetry-tracking-mode)
  (python-mode . (lambda () (when (poetry-venv-exist-p)
                              (setq-local lsp-pyls-server-command '("poetry" "run" "pyls"))
                              (poetry-venv-workon))))
  )

;; ....

;; lsp-mode configs
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-auto-guess-root +1)
  :config
  (lsp-enable-imenu)
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
	 (lsp-after-open . 'lsp-enable-imenu)
	 )
  :commands (lsp lsp-deferred))

;; ;; lsp Python
;; (use-package lsp-python-ms
;;   :after poetry
;;   :ensure t
;;   :init
;;   (setq lsp-python-ms-auto-install-server t)
;;   :config
;;   (put 'lsp-python-ms-python-executable 'safe-local-variable 'stringp)
;; 		    ;; attempt to activate Poetry env first
;; 		    (when (stringp (poetry-find-project-root))
;; 		      (poetry-venv-workon)
;; 		      )
;;   :hook
;;   (
;;    (python-mode . (lambda ()
;;                     (require 'lsp-python-ms)
;;                     (lsp-deferred)
;; 		    ))
;;    ;; if .dir-locals exists, read it first, then activate mspyls
;;    (hack-local-variables . (lambda ()
;; 			     (when (derived-mode-p 'python-mode)
;; 			       (require 'lsp-python-ms)
;; 			       (lsp-deferred))
;; 			     ))
;;    )
;;   )

(use-package lsp-python-ms
  :custom (lsp-python-ms-auto-install-server t))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb

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
  :custom (tramp-default-method "ssh")
  :config
  ; Fix TRAMP to GuixSD machines where ls/gzip/etc isn't in default PATH
  (add-to-list 'tramp-remote-path "/run/current-system/profile/bin")
  ; Fix TRAMP to Termux too
  (add-to-list 'tramp-remote-path "/data/data/com.termux/files/usr/bin"))

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
  :diminish undo-tree-mode)

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

(use-package dired-rsync)

(use-package all-the-icons)

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

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(use-package vlf
  :config
  (require 'vlf-setup))
