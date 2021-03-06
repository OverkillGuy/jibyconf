(setq gc-cons-threshold 999999999)
(message "gc-cons-threshold temporarily set to %S"
	 gc-cons-threshold)

(let ((file-name-handler-alist nil))

(if (file-exists-p "vanilla.el")
    (load-file "vanilla.el"))

(if (file-exists-p "package-config.el")
    (load-file "package-config.el"))

(defun jb-post-frame-config-load (&optional frame)
  "Make frame- and/or terminal-local changes."
  (with-selected-frame (or frame (selected-frame))
    (if (display-graphic-p)
        (load-file "~/.emacs.d/jb/config-x11.el"))))

(if (file-exists-p "config-x11.el")
    (add-hook 'after-make-frame-functions 'jb-post-frame-config-load))

(if (file-exists-p "termux-config.el")
    (load-file "termux-config.el"))

(if (file-exists-p "external-pkg-config.el")
    (load-file "external-pkg-config.el"))

(setq gc-cons-threshold 800000)
(message "gc-cons-threshold restored to %S"  gc-cons-threshold))
