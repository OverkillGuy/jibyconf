;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq! user-full-name "Jb Doyon"
      user-mail-address "jb@jiby.tech")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq! doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq! org-directory "~/dev/notes/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq! doom-font (font-spec :family "Fira Mono" :size 20))

;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(setq sentence-end-double-space nil)

;; Show RGB hex colors in programming modes or text modes
(use-package! rainbow-mode
  :hook ((prog-mode text-mode) . rainbow-mode))

(use-package! rainbow-delimiters
  :after doom-themes                    ; Ensure themes don't override us
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (setq! rainbow-delimiters-max-face-count 8)
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
                      :inherit 'rainbow-delimiters-base-face))

(use-package! company
  :config
  ; Remove company-ispell (dictionary-based completes) from any completion
  (setq +company-backend-alist (assq-delete-all 'text-mode +company-backend-alist))
  (add-to-list '+company-backend-alist '(text-mode (:separate company-dabbrev company-yasnippet))))

(defvar jb/compilation-complete-icon "/home/jiby/dev/foss/emacs-dragon-icon/AppIcons/emacs-dragon-icon.iconset/icon_128x128.png")

(defun jb/compilation-finished-unfocused-notify (buffer desc)
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
       :icon jb/compilation-complete-icon)))

(add-hook 'compilation-finish-functions 'jb/compilation-finished-unfocused-notify)

(use-package! projectile
  :bind ("S-<f7>" . projectile-switch-project)
  :custom (projectile-project-search-path '("~/dev/")))

(use-package! eww
  :after evil
  :config (evil-set-initial-state 'eww-mode 'emacs))

(use-package! eww-lnum
  :after eww
  :bind (:map eww-mode-map
              ("f" . eww-lnum-follow)
              ("F" . eww-lnum-universal)))


(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "xdg-open" nil 0 nil file)
    (message "Opening %s done" file)))

(use-package! irfc
  :load-path "~/.emacs.d/jb/packages/"
  :mode ("[rR][fF][cC].*\\.txt" . irfc-mode)
  :custom-face
  (irfc-rfc-link-face ((t (:bold t :foreground "deep sky blue"))))
  (irfc-head-name-face ((t (:foreground "orange red"))))
  :custom
  (irfc-directory "~/dev/doc/rfc/")
  (irfc-assoc-mode t)
  :config (defalias 'rfc 'irfc-visit))


(use-package! edit-server
  :defer t
  :init (edit-server-start)
  :custom
  (edit-server-url-major-mode-alist
        '(("github\\.com" . markdown-mode)))
  (edit-server-new-frame nil))

;; No persistent history
(use-package! undo-tree
  :custom
  (undo-tree-auto-save-history nil))

;; Enable time in the mode-line
(after! doom-modeline
  (setq! display-time-string-forms
        '((propertize (concat 24-hours ":" minutes))))
  (display-time-mode 1)
  ;; On laptops it's nice to know how much power you have
  (unless (string-match-p "^Power N/A" (battery))
    (display-battery-mode 1)))


;; From https://github.com/hlissner/doom-emacs/issues/2223#issuecomment-568202866
;; Don't auto-insert parens!
(use-package! smartparens
  :custom
  (smartparens-global-mode -1))

(defvar org-created-property-name
  "CREATED"
  "The name of the org-mode property that stores the creation date of the entry")

(defun jb/org-set-created-property (&optional active NAME)
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

(add-hook! 'org-insert-heading-hook
           'org-id-get-create
           'jb/org-set-created-property)

;; Disabled because it warns on org-roam, and feature is meh anyway
;; ;; From https://emacs.stackexchange.com/a/36483
;; (defun yas-org-very-safe-expand ()
;;   (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))

;; (add-hook! 'org-tab-first-hook 'yas-org-very-safe-expand)
(define-key! yas-keymap [tab] 'yas-next-field)


; Don't use the recently-default "/posts/", but "/post/" as my blog does
(setq! org-hugo-section "post")


;; Fix magit-blame-mode ENTER key not jumping to commit anymore
(use-package! magit
  :config
  (evil-set-initial-state 'magit-blame-mode-hook 'emacs)
;; Fix magit-status buffer's SPC prompting for showing commits instead of doom menu
  (define-key! magit-status-mode-map "SPC" #'doom/leader)
;; Margin default of 18 is way too much for authorship
  (setq! magit-log-margin '(t age magit-log-margin-width t 8))
  (setq! magit-diff-visit-prefer-worktree t)
  :bind ("M-<f12>" . magit-status)
        ("S-<f12>" . magit-log-all-branches))

(setq! +snippets-dir "~/.emacs.d/jb/snippets/")

;; Start maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-hook 'doom-first-buffer-hook #'global-display-fill-column-indicator-mode)

(defun python-src-fill-black-mode-hook ()
  (setq fill-column 88))

(add-hook 'python-mode-hook #'python-src-fill-black-mode-hook)
(evil-set-initial-state 'dired-mode 'emacs)


;; Add "take-both" option to merge, bound to d
(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(setq comint-buffer-maximum-size 16384)


(use-package! plantuml-mode
  :custom
  (plantuml-default-exec-mode 'executable)
  ;; (plantuml-jar-path "/opt/plantuml/plantuml.jar")
  (plantuml-output-type "png"))

(setq! org-re-reveal-title-slide
  "<h1>%t</h1><h4>%s</h4><p>%a - <a href=\"%u\">%u</a><p>\n<p>%d </p>")

(use-package! unfill
  :config
  (undefine-key! "M-Q")
  ;; (define-key! unfill-region "M-Q" #'doom/leader))

  (define-key! evil-normal-state-map "gQ" 'unfill-paragraph)
  (define-key! evil-visual-state-map "gQ" 'unfill-region))

;; FIXME: Check if still useful now we use Copier?
(defun jb-j2template-mode-override ()
  ;; Override the major mode if project path contains jinja2 template chars
  (if
      (string-match-p "{{cookiecutter" (or (buffer-file-name) ""))
      (progn
        (jinja2-mode)
        (poetry-tracking-mode -1))))

(add-hook 'find-file-hook #'jb-j2template-mode-override)

;; (setq! lsp-enable-suggest-server-download nil)
;; (setq! lsp-disabled-clients '("mspyls"))
;; (after! lsp-mode
;;   (setq! lsp-pylsp-plugins-flake8-ignore
;;          (list "D400"))) ;; "Docstrings first line must end in a period"

;; (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))

;; (setq-default eglot-workspace-configuration
;;         '((:pylsp .
;;            (:configurationSources ["flake8"]
;;             :plugins
;;             (:pycodestyle (:enabled nil)
;;              :mccabe (:enabled nil)
;;              :flake8 (:enabled t))
;;             (:configurationSources ["python-mypy"]
;;              ::enabled nil)))))



;; Fallback font for unicode symbols, both for standalone & client modes
(when (display-graphic-p)
  (let ((symbols-font "Noto Sans Symbols 2-16"))
    (set-fontset-font t 'symbol symbols-font)
    (add-hook 'server-after-make-frame-hook
              `(lambda ()
                 (when (display-graphic-p)
                   (set-fontset-font nil 'symbol ,symbols-font))))))


;; Fix info mode navigation broken in evil-mode
(evil-set-initial-state 'Info-mode 'emacs)

(use-package! org
  :config
  ;; Org buffers' indentation is sacred (fill-column + autofill)
  (setq! org-hide-leading-stars nil
         org-startup-indented nil
         org-export-with-smart-quotes nil
         org-export-with-entities nil
         org-id-prefix "jiborg"))
;; TODO: Change evil-snipe to whole visible buffer not just line
(use-package! evil-snipe
  ;; Search across the whole visible area, not just current line
  :custom (evil-snipe-scope 'visible))

;; TODO: Hunt down which-key "paging" button, none seem to work due to doom:


(use-package! org-roam
  :after org evil
  ;; HACK evil-set-initial-state only works for MAJOR modes, org-capture = minor
  ;; See https://github.com/emacs-evil/evil/issues/1115#issuecomment-450480141
  :hook (org-capture-mode . evil-insert-state)
  :config
  ;; Bind Shift+F8 globally to org-roam-capture
  (map! "<S-f8>" #'org-roam-capture)
  (setq! org-roam-directory
         (expand-file-name "roam"
                         (file-name-as-directory
                          (expand-file-name org-directory))))
  (require 'org-roam-protocol)
  (setq-default
   org-roam-capture-templates
   '(
     ("c" "concept" plain "%?"
      :target
      (file+head "%<%Y>/%<%m>/${slug}.org" "#+TITLE: ${title}\n#+DATE: %U\n\n")
      :unnarrowed t)
     ("e" "encrypted concept" plain "%?" 
      :target (file+head "%<%Y>/%<%m>/${slug}.org.gpg" "#+TITLE: ${title}\n#+DATE: %U\n#+FILETAGS: :gpg:encrypted:\n")
      :unnarrowed t)
     ))
  (setq-default
   org-roam-dailies-capture-templates
   '(
     ("d" "daily" plain
      (file "~/.emacs.d/jb/capture-templates/daily_template.org")
      :if-new (file+head "%<%Y>/%<%m>/%<%Y-%m-%d>.org" "#+TITLE: Daily notes for %<%Y-%m-%d>\n#+CATEGORY: journal\n\n"))
     ("L" "daily w/ link" plain
      (file "~/.emacs.d/jb/capture-templates/daily_template_with_link.org")
      :if-new (file+head "%<%Y>/%<%m>/%<%Y-%m-%d>.org" "#+TITLE: Daily notes for %<%Y-%m-%d>\n#+CATEGORY: journal\n\n"))
     ))
  ;; Replace SPC-X with dailies capture
  (map! :leader
        "X" #'org-roam-dailies-capture-today)
  )

;; TODO: Darken the org_src code blocks, Lighten the start/end lines
;; TODO: Set up org-ql / org-roam-ql for searching easier
;; TODO: Add consult-org-roam too?

(use-package! evil
  :after undo-tree
  :custom
  ;; Allow VI mode to select character after EOL like in Emacs
  (evil-move-beyond-eol t)
  (evil-want-C-d-scroll nil)
  (evil-cross-lines t)
  (evil-undo-system 'undo-tree)
  (evil-kill-on-visual-paste nil))


(defun show-gherkin ()
  "Show the gherkin features of this buffer in a separate window"
  (interactive)
  (occur "\\(Given\\|When\\|Then\\|And\\|But\\|Scenario\\|Background\\|Feature\\|In order to\\|As a\\|I want to\\|I need to\\|So that\\)"))

(defun jb/openai-config ()
  "Configure gptel for OpenAI online setup"
  (setopt gptel-api-key
          (password-store-get "openai/openai_api_token")))

(defun jb/mistral-config ()
  "Configure gptel for Mistral AI online setup"
  (setopt gptel-model 'mistral-medium-latest
          gptel-quick-model 'mistral-small-latest
          gptel-backend
          (gptel-make-openai "Mistral"
            :host "api.mistral.ai"
            :endpoint "/v1/chat/completions"
            :protocol "https"
            :key (password-store-get "mistral/mistral_api_key_emacs_hally")
            :models '("mistral-small-latest"
                      "mistral-medium-latest"
                      "mistral-large-latest"  ;; image, tools
                      "magistral-medium-latest"
                      "devstral-latest"))
          gptel-quick-backend gptel-backend
          gptel-quick-model 'mistral-small-latest))

(defun jb/llamafile-config ()
  "Configure gtel for local llamafile config"
  (setopt gptel-api-key nil
          gptel-model 'test
          gptel-backend
          (gptel-make-openai "llamafile"
            :stream t
            :protocol "http"
            :host "localhost:8081"
            :models '(test))))

(use-package! gptel
  :init
  (require 'gptel-agent)  ;; Force-load gptel-agent eagerly too
  :config
  (setopt gptel-org-set-properties t  ;; Store session info in org props on save
          gptel-org-set-topic t  ;; Save topic in org props on save
          gptel-default-mode 'org-mode
         ;; Do not highlight responses (we have separate org headers)
         gptel-highlight-mode nil)
  ;; (jb/llamafile-config)  ;; or using online via (jb/openai-config)
  (jb/mistral-config)
  ;; Ensure org-mode headings are part of prompts 
  (add-to-list 'gptel-prompt-prefix-alist `(org-mode . ,(concat "* Jiby\n")))
  (add-to-list 'gptel-response-prefix-alist `(org-mode . ,(concat "** Answer\n")))
  ;; Ensure cursor moves to bottom of response
  (add-hook! 'gptel-post-response-functions 'gptel-end-of-response))

(use-package! gptel-agent
  :after gptel
  :bind (:map doom-leader-map
              ("v" . gptel))
  (:map gptel-mode-map
        ("C-c RET" . gptel-menu)
        ("C-c C-c" . gptel-send))
  :config
  (gptel-agent-update))

(use-package! gptel-quick
  :after gptel
  :bind (:map doom-leader-map
              ("V" . gptel-quick)))

(use-package! gptel-prompts
  :after gptel
  :config
  (gptel-prompts-update)
  ;; Ensure prompts are updated if prompt files change
  (gptel-prompts-add-update-watchers))

(use-package! typst-preview
  :custom
  (typst-preview-invert-colors "never"))

;; Activate org-mode style links in all programming modes
;; Solves the missing goto-address-mode too
(use-package! orglink
  :config (global-orglink-mode)
  :custom (orglink-activate-in-modes '(prog-mode)))


(use-package agent-shell
  :bind (:map agent-shell-mode-map
              ("RET" . newline)
              ("C-c RET" . shell-maker-submit)
              ("C-c C-k" . agent-shell-interrupt))
  :config
  ;; Evil state-specific RET behavior: insert mode = newline, normal mode = send
  (evil-define-key 'insert agent-shell-mode-map (kbd "RET") #'newline)
  (evil-define-key 'normal agent-shell-mode-map (kbd "RET") #'comint-send-input)

  ;; Configure *agent-shell-diff* buffers to start in Emacs state
  (add-hook 'diff-mode-hook
	    (lambda ()
	      (when (string-match-p "\\*agent-shell-diff\\*" (buffer-name))
		(evil-emacs-state)))))

;; Keep track of what the agent is doing while in another buffer
(use-package agent-shell-knockknock
  :after agent-shell
  :hook (agent-shell-mode . agent-shell-knockknock-mode))
;; Get clipboard links for org-capture
(use-package org-web-tools
  :commands org-web-tools--get-first-url)
