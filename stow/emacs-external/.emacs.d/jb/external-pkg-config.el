(use-package ledger-mode
  :if '(executable-find "ledger"))

(use-package plantuml-mode
; TODO try out the plantuml-download-jar func change pkgcheck to if
; java+dot available ensure downloaded
  :if (file-exists-p "/opt/plantuml/jar/plantuml.jar")
  :custom
  (plantuml-jar-path (expand-file-name "/opt/plantuml/jar/plantuml.jar"))
  (org-plantuml-jar-path plantuml-jar-path)
  (plantuml-default-exec-mode 'jar)
  (plantuml-output-type "png"))

(add-hook 'plantuml-mode-hook (lambda () (electric-indent-local-mode -1)))

(if (and
  (featurep 'plantuml-mode)
  (featurep 'rainbow-mode))
    (add-hook 'plantuml-mode-hook 'rainbow-mode))

(use-package ag
  :if '(executable-find "ag")
  :config (add-to-list 'ag-arguments "--follow"))

(use-package helm-ag
  :after ag helm)

(use-package mu4e
  :if (executable-find "mu")
  :load-path "~/.emacs.d/lisp/mu4e/"
  :bind ("C-M-4" . mu4e)
  :config
  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))
  ;; Allow HTML email to be read via PDF when text rendering fails
  (if-let (msg2pdf (executable-find "msg2pdf"))
      (setq mu4e-msg2pdf msg2pdf))
  (add-to-list 'mu4e-view-actions
	       '("bBrowser View" . mu4e-action-view-in-browser) t)
  ;; Hide the stupid empty update buffer when fetching mail
  (add-hook 'mu4e~update-mail-mode-hook 'bury-buffer)
  :custom
  ;; Don't keep message buffers around
  (message-kill-buffer-on-exit t)
  ;; Don't use cool icons for display (they are cool but are not of
  ;; correct length, messing up layouts)
  (mu4e-use-fancy-chars nil)
  ;; Get email every 5 minutes
  (mu4e-update-interval 300)
  (mu4e-get-mail-command "mbsync -aq")
  (mu4e-attachment-dir  "~/Downloads")
  (mu4e-view-show-images t)
  ;; Wrap lines softly via format=flowed
  (mu4e-compose-format-flowed t)
  (mu4e-view-show-addresses t)
  (mail-user-agent 'mu4e-user-agent))

(require 'gnus-dired)
;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (and (derived-mode-p 'message-mode)
		   (null message-sent-message-via))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(when (featurep 'mu4e)
  (require 'org-mu4e))

(use-package mu4e-alert
  :after mu4e
  :config
  (mu4e-alert-set-default-style 'libnotify)
  (mu4e-alert-enable-notifications)
  (mu4e-alert-enable-mode-line-display))

(when (featurep 'mu4e)
  (require 'mu4e-contrib))

(setq mu4e-html2text-command 'mu4e-shr2text)
(setq shr-color-visible-luminance-min 60)
(setq shr-color-visible-distance-min 5)
(setq shr-use-colors nil)
(advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest ignore)))

(setq mu4e-maildir (expand-file-name "~/.mail/jiby.tech"))
(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-sent-folder   "/Sent")
(setq mu4e-trash-folder  "/Trash")

(setq mu4e-maildir-shortcuts
      '(("/INBOX"        . ?i)
        ("/Sent"         . ?s)
        ("/Trash"        . ?t)
        ("/Drafts"       . ?d)))

(require 'smtpmail)
(setq user-mail-address "FILLMEHERE@jb.jiby.tech"
   message-send-mail-function 'smtpmail-send-it
   smtpmail-starttls-credentials '(("smtp.fastmail.com" 465 nil nil))
   smtpmail-auth-credentials
     '(("smtp.fastmail.com" 587 "jb@jiby.tech" nil))
   smtpmail-default-smtp-server "smtp.fastmail.com"
   smtpmail-smtp-server "smtp.fastmail.com"
   smtpmail-smtp-service 587)

(defun convert-to-org-calendar-attachment (msg attachnum)
  "Converts to org format an (ical) attachment"
  (mu4e-view-pipe-attachment msg attachnum "ical2orgpy - - >> ~/dev/notes/calendar.org"))

;; defining 'n' as the shortcut (if mu4e is loaded)
(when (featurep 'mu4e)
  (add-to-list 'mu4e-view-attachment-actions
	       '("cSave to calendar" . convert-to-org-calendar-attachment) t))

(defun auto-preview-org-latex ()
  "Toggles latex-preview when a dollar (latex equation) is followed by space"
  (when (looking-back (rx "$"))
    (save-excursion
      (backward-char 1)
      (org-toggle-latex-fragment))))

(defun preview-org-latex-hook ()
  "Hook to auto-preview latex fragments in org buffers"
  (org-cdlatex-mode)
  (diminish 'org-cdlatex-mode)
  (add-hook 'post-self-insert-hook #'auto-preview-org-latex 'append 'local))

(use-package cdlatex
  :if '(executable-find "dvipng")
  :custom (org-format-latex-options
	   (plist-put org-format-latex-options :scale 2.0))
  :hook (org-mode . preview-org-latex-hook))

(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

; NOTE: Intentionally overridden below if latexmk (bibliography support) is available
(setq org-latex-pdf-process
      '("pdflatex --shell-escape --interaction nonstopmode -output-directory %o %f"
        "pdflatex --shell-escape --interaction nonstopmode -output-directory %o %f"
        "pdflatex --shell-escape --interaction nonstopmode -output-directory %o %f"))

(if (executable-find "latexmk")
    (setq org-latex-pdf-process
	  (list
	   "latexmk  -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -bibtex -f  %f")))

(use-package company-math
  :after company
  :config
  (add-to-list 'company-backends 'company-math-symbols-latex))

(use-package edit-server
  :init (edit-server-start)
  :custom
  (edit-server-url-major-mode-alist
        '(("github\\.com" . markdown-mode))))

(require 'org-protocol)
