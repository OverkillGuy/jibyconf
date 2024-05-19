(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-modules
   '(ol-bibtex
     org-crypt
     ol-doi
     ol-eww
     org-id
     ol-info
     org-mouse
     org-protocol
     ol-eshell
     org-annotate-file
     ol-bookmark
     org-checklist
     org-choose
     org-collector
     org-expiry
     ol-git-link
     ol-man
     org-notify
     orgtbl-sqlinsert
     org-toc))
'(org-capture-templates `(
        ("p" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
        "* %?\n:PROPERTIES:\n:CREATED:  %U\n:ID:       %(org-id-new)\n:END:\n\nFrom [[%:link][%:description]]:\n\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n")
        ("L" "Protocol Link" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
        "* %?\n:PROPERTIES:\n:CREATED:  %U\n:ID:       %(org-id-new)\n:END:\nFrom [[%:link][%:description]].\n")
        ("h" "hally" entry (file "~/org/cattle/hally.org")
        "* %?\n:PROPERTIES:\n:CREATED:  %U\n:ID:       %(org-id-new)\n:END:\n\n")
        ))
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'customize-group 'disabled nil)
