;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jb Doyon"
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
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


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
(use-package! rainbow-delimiters
  :init
  (setq rainbow-delimiters-max-face-count 8)
  :hook (prog-mode . rainbow-delimiters-mode)
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
                      :inherit 'rainbow-delimiters-base-face))

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

(global-set-key (kbd "S-<f7>") 'projectile-switch-project)


(global-set-key (kbd "M-<f12>") 'magit-status)

(global-set-key (kbd "S-<f12>") 'magit-log-all-branches)
(defun jb/open-devlog ()
  (interactive)
  (find-file "~/dev/notes/devlog.org")
  (end-of-buffer))

(use-package! projectile
  :bind-keymap ("<f7>" . projectile-command-map)
  :config (setq! projectile-project-search-path
	         '("~/dev/" "~/org/"))
  (setq! projectile-switch-project-action 'magit-status))


(global-set-key (kbd "<f8>") 'jb/open-devlog)
(global-set-key (kbd "S-<f8>") 'org-capture)
;; Capture templates, from defaults
(setq! org-capture-templates `(
        ("p" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
        "* %?\n:PROPERTIES:\n:CREATED:  %U\n:ID:       %(org-id-new)\n:END:\n\nFrom [[%:link][%:description]]:\n\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n")
        ("L" "Protocol Link" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
        "* %?\n:PROPERTIES:\n:CREATED:  %U\n:ID:       %(org-id-new)\n:END:\nFrom [[%:link][%:description]].\n")
        ("h" "hally " entry (file ,(concat org-directory "cattle/hally.org"))
        "* %?\n:PROPERTIES:\n:CREATED:  %U\n:ID:       %(org-id-new)\n:END:\n\n")
))
;; Auto-enable insert mode on captures
(add-hook 'org-capture-mode-hook 'evil-insert-state)


(use-package! eww-lnum
  :defer t
  :config
  (eval-after-load "eww"
    '(progn (define-key eww-mode-map "f" 'eww-lnum-follow)
            (define-key eww-mode-map "F" 'eww-lnum-universal))))

(setq sentence-end-double-space nil)

(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "xdg-open" nil 0 nil file)
    (message "Opening %s done" file)))

;; (load-file "~/.emacs.d.BKP/jb/packages/irfc.el")
;; (setq irfc-directory "~/dev/doc/rfc/")
;; (setq irfc-assoc-mode t)

;; ;; (setq irfc-head-name-face :foreground "orange red")
;; (set-face-attribute 'irfc-head-name-face nil :foreground "orange red")

;; (when (featurep 'irfc)
;;   (add-to-list 'auto-mode-alist '("[rR][fF][cC].*\\.txt" . irfc-mode))
;;   (defalias 'rfc 'irfc-visit))

;; (load-file "~/.emacs.d.BKP/jb/packages/fic-mode.el")
;; (add-hook 'prog-mode-hook 'turn-on-fic-mode)

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
  :init (edit-server-start)
  :custom
  (edit-server-url-major-mode-alist
        '(("github\\.com" . markdown-mode)))
  (edit-server-new-frame nil))

; No persistent history
(after! undo-tree
  (setq undo-tree-auto-save-history nil))
(after! lsp-mode
  (setq! lsp-pylsp-plugins-flake8-ignore
         (list "D400"))) ;; "Docstrings first line must end in a period"


(display-time-mode 1)                             ; Enable time in the mode-line

(unless (string-match-p "^Power N/A" (battery))   ; On laptops...
  (display-battery-mode 1))                       ; it's nice to know how much power you have

;; From https://github.com/hlissner/doom-emacs/issues/2223#issuecomment-568202866
;; Don't auto-insert parens!
(after! smartparens
  (smartparens-global-mode -1))

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

(add-to-list 'org-modules 'org-id)
(add-hook 'org-insert-heading-hook #'org-id-get-create)

(add-hook 'org-mode-hook #'auto-fill-mode)

(use-package! ob-http)
(use-package! k8s-mode)
(use-package! feature-mode)
(use-package! htmlize)
(use-package! lorem-ipsum)

; Don't use the recently-default "/posts/", but "/post/" as my blog does
(setq! org-hugo-section "post")

(setq! evil-want-C-d-scroll nil
       evil-move-beyond-eol t
       evil-cross-lines t
       evil-undo-system 'undo-tree
       evil-kill-on-visual-paste nil)

;; Fix magit-blame-mode ENTER key not jumping to commit anymore
(add-hook 'magit-blame-mode-hook #'evil-emacs-state)
;; Fix magit-status buffer's SPC prompting for showing commits instead of doom menu
(define-key! magit-status-mode-map "SPC" #'doom/leader)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/jb/snippets/")

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
(defun plantuml-force-local-exec-hook ()
  "Ensure plantuml executes from local JAR, not from internet server"
  (setq! plantuml-exec-mode 'jar))

(add-hook 'plantuml-mode-hook #'plantuml-force-local-exec-hook)

(setq! org-re-reveal-title-slide
  "<h1>%t</h1><h4>%s</h4><p>%a - <a href=\"%u\">%u</a><p>\n<p>%d </p>")
(use-package! unfill
  :config
  (undefine-key! "M-Q")
  (define-key! unfill-region "M-Q" #'doom/leader))

(defun jb-j2template-mode-override ()
  ;; Override the major mode if project path contains jinja2 template chars
  (if
      (string-match-p "{{cookiecutter" (or (buffer-file-name) ""))
      (progn
        (jinja2-mode)
        (poetry-tracking-mode -1))))

(add-hook 'find-file-hook #'jb-j2template-mode-override)

(add-hook 'text-mode-hook #'auto-fill-mode)
