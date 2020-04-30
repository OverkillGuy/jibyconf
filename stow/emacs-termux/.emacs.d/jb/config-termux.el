  (advice-add 'browse-url-default-browser :override
              (lambda (url &rest args)
                (start-process-shell-command "open-url" nil (concat "termux-open-url " url))))

  ; Colors when hunk is selected
  (set-face-attribute 'magit-diff-added-highlight nil  :background "color-28")
  (set-face-attribute 'magit-diff-removed-highlight nil  :background "color-88")
  ; Colors when hunk is not selected
  (set-face-attribute 'magit-diff-added nil  :background "color-71")
  (set-face-attribute 'magit-diff-removed nil  :background "color-124")

(require 'woman)
(add-to-list 'woman-manpath "/data/data/com.termux/files/usr/share/man")

(require 'info)
(add-to-list 'Info-directory-list "/data/data/com.termux/files/home/dev/doc/info")

(defun smerge-refine-subst-wrapper (smerge-refine-subst-real &rest arguments)
  (let ((diff-command "bnudiff"))
    (apply smerge-refine-subst-real arguments)))

(advice-add 'smerge-refine-regions :around #'smerge-refine-subst-wrapper)

(use-package alert
  :config
  (setq alert-default-style 'termux))
