(tool-bar-mode -1)
(scroll-bar-mode -1)

(set-face-attribute 'org-verbatim nil
		    :box
		    '(:line-width 2 :color "grey75" :style released-button)
		    :inherit
		    'shadow)
(setq org-hide-emphasis-markers t)

; Test char and monospace:
; 0123456789abcdefghijklmnopqrstuvwxyz [] () :;,. !@#$^&*
; 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ {} <> "'`  ~-_/|\?
;; (cond
;;  (;; (find-font (font-spec :name "Monaco"))
;;   ;; (set-frame-font "Monaco-15"))
;;   ;; ((find-font (font-spec :name "courier"))
;;   ;;  (set-frame-font "courier-20"))
;;   ;; ((find-font (font-spec :name "inconsolata"))
;;   ;;  (set-frame-font "inconsolata-20"))
;;   ;; ((find-font (font-spec :name "Lucida Console"))
;;   ;;  (set-frame-font "Lucida Console-20"))
;;   (t (set-frame-font "mono-14"))))
(set-frame-font "mono-14")

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(set-fontset-font "fontset-default" nil
                  (font-spec :name "Symbola"))

;; Cosmetic : change modeline aspect
(use-package powerline
  :config
  (powerline-default-theme)
  (set-face-attribute 'mode-line nil
                      :foreground "White"
                      :background "DarkBlue"
                      :box nil))

(if (display-mouse-p) (mouse-avoidance-mode 'animate))

(setq browse-url-browser-function 'browse-url-firefox)

(require 'iso-transl)

(use-package pdf-tools
  :after tex-mode latex-preview-pane
  :config
  (pdf-loader-install)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-source-correlate-start-server t)
  ;; revert pdf-view after compilation
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  ;; Fix pdf-tools not updating properly in latex-preview-pane
  ;; Workaround of https://github.com/jsinglet/latex-preview-pane/issues/47
  (advice-add 'doc-view-revert-buffer :before
	    'pdf-view-revert-buffer))

;; Enable horizontal scroll
(setq mouse-wheel-tilt-scroll t)
;; However it's flipped on my machine: reverse it
(setq mouse-wheel-flip-direction t)

