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

(use-package! rainbow-delimiters
  :init
  :hook (prog-mode . rainbow-delimiters-mode)
  :defer t
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

(global-set-key (kbd "S-<f7>") 'projectile-switch-project)


(global-set-key (kbd "M-<f12>") 'magit-status)

(global-set-key (kbd "S-<f12>") 'magit-log-all-branches)
(defun jb/open-devlog ()
  (interactive)
  (find-file "~/dev/notes/devlog.org")
  (end-of-buffer))

;; (use-package! projectile
;;   :defer t
;;   :bind-keymap ("<f7>" . projectile-command-map)
;;   :config (setq! projectile-project-search-path
;;                  '("~/dev/" "~/org/"))
;;   (setq! projectile-switch-project-action 'magit-status))

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
;; (setq irfc-head-name-face :foreground "orange red")
  :config
  (setq! irfc-directory "~/dev/doc/rfc/")
  (setq! irfc-assoc-mode t)
  ; (set-face-attribute 'irfc-head-name-face nil :foreground "orange red")
  (add-to-list 'auto-mode-alist '("[rR][fF][cC].*\\.txt" . irfc-mode))
  (defalias 'rfc 'irfc-visit))

(use-package! fic-mode
  :load-path "~/.emacs.d/jb/packages/"
  :hook (prog-mode-hook . turn-on-fic-mode))

(defun set-docs-as-readonly ()
  "Make buffers readonly by default when folder matches pattern"
  (dolist (pattern '("~/dev/doc/.*"
                                        ; Anything else?
                     ))
    (if (string-match (expand-file-name pattern) buffer-file-name)
        (read-only-mode))))

(add-hook 'find-file-hook 'set-docs-as-readonly)

(setq doc-view-continuous t)

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
(display-time-mode 1)

(unless (string-match-p "^Power N/A" (battery))   ; On laptops...
  (display-battery-mode 1))                       ; it's nice to know how much power you have

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

(add-hook! 'org-mode-hook 'auto-fill-mode)

;; Disabled because it warns on org-roam, and feature is meh anyway
;; ;; From https://emacs.stackexchange.com/a/36483
;; (defun yas-org-very-safe-expand ()
;;   (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))

;; (add-hook! 'org-tab-first-hook 'yas-org-very-safe-expand)
(define-key! yas-keymap [tab] 'yas-next-field)



(use-package! ob-http :defer t)
(use-package! k8s-mode :defer t)
(use-package! feature-mode :defer t)
(use-package! htmlize :defer t)
(use-package! lorem-ipsum :defer t)

; Don't use the recently-default "/posts/", but "/post/" as my blog does
(setq! org-hugo-section "post")


;; Fix magit-blame-mode ENTER key not jumping to commit anymore
(use-package! magit
  :config
  (evil-set-initial-state 'magit-blame-mode-hook 'emacs)
;; Fix magit-status buffer's SPC prompting for showing commits instead of doom menu
  (define-key! magit-status-mode-map "SPC" #'doom/leader)
;; Margin default of 18 is way too much for authorship
  (setq! magit-log-margin '(t age magit-log-margin-width t 8)))

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
  (plantuml-default-exec-mode 'jar)
  (plantuml-jar-path "/opt/plantuml/plantuml.jar")
  (plantuml-output-type "png"))

(setq! org-re-reveal-title-slide
  "<h1>%t</h1><h4>%s</h4><p>%a - <a href=\"%u\">%u</a><p>\n<p>%d </p>")

(use-package! unfill
  :config
  (undefine-key! "M-Q")
  (define-key! unfill-region "M-Q" #'doom/leader))

;; FIXME: Check if still useful now we use Copier?
(defun jb-j2template-mode-override ()
  ;; Override the major mode if project path contains jinja2 template chars
  (if
      (string-match-p "{{cookiecutter" (or (buffer-file-name) ""))
      (progn
        (jinja2-mode)
        (poetry-tracking-mode -1))))

(add-hook 'find-file-hook #'jb-j2template-mode-override)

(add-hook 'text-mode-hook #'auto-fill-mode)

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

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  ;; (global-tree-sitter-mode)
  ;; (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  )


(use-package! ellama
  :init
  (require 'llm-ollama)
  (setq! ellama-provider
         (make-llm-ollama :scheme "http" :host "localhost" :port 11434
                          ;;:chat-model "zephyr:7b-alpha-q5_K_M" :embedding-model "zephyr:7b-alpha-q5_K_M"))
                          :chat-model "llama3:8b"))
)


;; Fallback font for unicode symbols, both for standalone & client modes
(let ((symbols-font "Noto Sans Symbols 2-16"))
  (set-fontset-font t 'symbol symbols-font)
  (add-hook 'server-after-make-frame-hook
            `(lambda ()
               (set-fontset-font nil 'symbol ,symbols-font))))

(setq! org-id-prefix "jiborg")

;; Fix info mode navigation broken in evil-mode
(evil-set-initial-state 'Info-mode 'emacs)


;; (global-set-key (kbd "<f8>") 'jb/open-devlog)
(global-set-key (kbd "S-<f8>") 'org-roam-capture)
;; Capture templates, from defaults

(use-package! org-roam
  :after org evil
  ;; HACK evil-set-initial-state only works for MAJOR modes, org-capture = minor
  ;; See https://github.com/emacs-evil/evil/issues/1115#issuecomment-450480141
  :hook (org-capture-mode . evil-insert-state)
  :config
  (setq! org-roam-directory "~/dev/notes/")
  ;; Auto-enable insert mode on captures
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))
  (require 'org-roam-protocol)
  (setq! org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n")
      :unnarrowed t)
     ("t" "Task" entry
      "* %?\n:PROPERTIES:\n:CREATED:  %U\n:ID:       %(org-id-new)\n:END:\n\n"
      :target (file "todo.org")
      :if-new (file+head "todo.org" "#+TITLE: ${title}\n#+FILETAGS: task\n\n")
      :unnarrowed t
      :prepend t)
     ("c" "Concept" plain
      "%?"
      :if-new (file+head "concepts/${slug}.org" "#+TITLE: ${title}\n#+FILETAGS: concept\n\n")
      :unnarrowed t)
     ))
  (setq! org-roam-dailies-capture-templates
         '(("d" "default" entry "* %?\n:PROPERTIES:\n:CREATED:  %U\n:ID:       %(org-id-new)\n:END:\n\n" :target
            (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>\n#+FILETAGS: daily\n\n"))))
  )

(use-package! evil
  :after undo-tree
  :custom
  ;; Allow VI mode to select character after EOL like in Emacs
  (evil-move-beyond-eol t)
  (evil-want-C-d-scroll nil)
  (evil-cross-lines t)
  (evil-undo-system 'undo-tree)
  (evil-kill-on-visual-paste nil))
