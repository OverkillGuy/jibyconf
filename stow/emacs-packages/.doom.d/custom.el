(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-modules
   '(ol-bibtex org-crypt ol-doi ol-eww org-id ol-info org-mouse org-protocol
     ol-eshell org-annotate-file ol-bookmark org-checklist org-choose
     org-collector org-expiry ol-git-link ol-man orgtbl-sqlinsert org-toc))
 '(safe-local-variable-values
   '((typst-preview-cmd-options
      . "--font-path ./fontawesome-free-6.7.2-desktop/otfs/")
     (typst-ts-compile-option
      . "--font-path fontawesome-free-7.0.1-desktop/otfs/")
     (typst-ts-compile-option . "--font-path fontawesome-free-7.0.1-desktop/")
     (projectile-project-compilation-cmd . "make debug"))))
(put 'customize-group 'disabled nil)
