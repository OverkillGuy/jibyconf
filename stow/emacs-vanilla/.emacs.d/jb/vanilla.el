(load-theme 'tango-dark t)

(menu-bar-mode -1)

(if (display-graphic-p)
    (scroll-bar-mode -1))

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(setq ring-bell-function 'ignore)

(setq system-time-locale "en_GB")

(setq sentence-end-double-space nil)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq calendar-week-start-day 1)

(setq Info-additional-directory-list '("~/dev/doc/info"))

(setq make-backup-files nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-x C-c ESC") 'save-buffers-kill-terminal)

(c-add-style "my-style"
	     '("stroustrup"
	       (indent-tabs-mode . nil)        ; use spaces rather than tabs
	       (c-basic-offset . 2)            ; indent by two spaces
	       (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
				   (brace-list-open . 0)
				   (statement-case-open . +)))))

(defun my-c++-mode-hook ()
  (c-set-style "my-style")        ; use my-style defined above
  (auto-fill-mode)
  (c-toggle-auto-hungry-state -1))
;; Trigger my-style when entering c++-mode
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'prog-mode-hook 'show-paren-mode)

;; Adds ANSI Color support to Compilation window
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(global-set-key (kbd "<f9>") 'recompile)
(global-set-key (kbd "S-<f9>") 'compile)

(setq compilation-scroll-output 'first-error)

(setq async-shell-command-display-buffer nil)

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
		   (concat
		    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
		    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(defun show-gherkin ()
  "Show the gherkin features of this buffer in a separate window"
  (interactive)
  (occur "\\(Given\\|When\\|Then\\|And\\|But\\|Scenario\\|Background\\|Feature\\|In order to\\|As a\\|I want to\\|I need to\\|So that\\)"))

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

;; Handy key definition
(define-key global-map (kbd "C-M-Q") 'unfill-region)

    ;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
	;; This would override `fill-column' if it's an integer.
	(emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; Handy key definition
(define-key global-map (kbd "M-Q") 'unfill-paragraph)

(defun find-first-non-ascii-char ()
  "Find the first non-ascii character from point onwards."
  (interactive)
  (let (point)
    (save-excursion
      (setq point
	    (catch 'non-ascii
	      (while (not (eobp))
		(or (eq (char-charset (following-char))
			'ascii)
		    (throw 'non-ascii (point)))
		(forward-char 1)))))
    (if point
	(goto-char point)
	(message "No non-ascii characters."))))

(defun to-snakecase ()
  (interactive)
  (progn
    (replace-regexp "\\([A-Z]\\)" "_\\1" nil (region-beginning) (region-end))
    (downcase-region (region-beginning) (region-end))))

(defun to-snakecase ()
  (interactive)
  (progn
    (replace-regexp "\\([A-Z]\\)" "_\\1" nil (region-beginning) (region-end))
    (downcase-region (region-beginning) (region-end)))
    (replace-regexp "\s" "_" nil (region-beginning) (region-end)))

(global-unset-key (kbd "M-u"))
(global-unset-key (kbd "M-l"))
(global-unset-key (kbd "M-c"))
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

(defun yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))

(global-set-key "\M-Y" 'yank-pop-forwards)

(defun unix-werase-or-kill (arg)
  (interactive "*p")
  (if (and transient-mark-mode
	   mark-active)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))
(global-set-key (kbd "C-w") 'unix-werase-or-kill)

(delete-selection-mode t)

(setq fill-column 79)

(global-set-key (kbd "C-x k") 'kill-this-buffer)

(require 'dired)

(setq wdired-allow-to-change-permissions t)

(add-hook 'dired-load-hook
               (lambda ()
                 (load "dired-x")
                 ;; Set dired-x global variables here.  For example:
                 ;; (setq dired-guess-shell-gnutar "gtar")
                 ;; (setq dired-x-hands-off-my-keys nil)
                 ))
     ;; (add-hook 'dired-mode-hook
     ;;           (lambda ()
     ;;             ;; Set dired-x buffer-local variables here.  For example:
     ;;             ;; (dired-omit-mode 1)
     ;;             ))

(global-set-key (kbd "C-x C-j") 'dired-jump)

(setq dired-dwim-target t)

;; -*- lexical-binding: t -*-
(defun ora-ediff-files ()
  (interactive)
  (let ((files (dired-get-marked-files))
	(wnd (current-window-configuration)))
    (if (<= (length files) 2)
	(let ((file1 (car files))
	      (file2 (if (cdr files)
			 (cadr files)
		       (read-file-name "file: "
				       (dired-dwim-target-directory)))))
	  (if (file-newer-than-file-p file1 file2)
	      (ediff-files file2 file1)
	    (ediff-files file1 file2))
	  (add-hook 'ediff-after-quit-hook-internal
		    (lambda ()
		      (setq ediff-after-quit-hook-internal nil)
		      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))

(add-hook 'dired-load-hook
	  (lambda ()
	    (define-key dired-mode-map "=" 'ora-ediff-files)))

(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "xdg-open" nil 0 nil file)
    (message "Opening %s done" file)))

(define-key dired-mode-map (kbd "E") 'dired-open-file)

(load-file "packages/irfc.el")
(setq irfc-directory "~/dev/doc/rfc/")
(setq irfc-assoc-mode t)

;; (setq irfc-head-name-face :foreground "orange red")
(set-face-attribute 'irfc-head-name-face nil :foreground "orange red")

(when (featurep 'irfc)
  (add-to-list 'auto-mode-alist '("[rR][fF][cC].*\\.txt" . irfc-mode))
  (defalias 'rfc 'irfc-visit))

(load-file "packages/fic-mode.el")
(add-hook 'prog-mode-hook 'turn-on-fic-mode)

(defun set-docs-as-readonly ()
  "Make buffers readonly by default when folder matches pattern"
  (dolist (pattern '("~/dev/doc/.*"
					; Anything else?
		     ))
    (if (string-match (expand-file-name pattern) buffer-file-name)
        (read-only-mode))))

(add-hook 'find-file-hook 'set-docs-as-readonly)

(setq doc-view-continuous t)
