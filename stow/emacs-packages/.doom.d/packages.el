;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)
(package! feature-mode :pin "8d43c37ddf986af769870da27c31c1911f35b205")
(package! htmlize :pin "fa644880699adea3770504f913e6dddbec90c076")
(package! lorem-ipsum :pin "4e87a899868e908a7a9e1812831d76c8d072f885")
(package! ob-http :pin "b1428ea2a63bcb510e7382a1bf5fe82b19c104a7")
(package! unfill :pin "4a15511876983eeaa75e57fcab8d4d51fe9b3840")
(package! eww-lnum :pin "4b0ecec769919ecb05ca4fb15ec51911ba589929")
(package! edit-server :pin "b39a8761916118afb024fdf9a7187def5805fa26")
(package! k8s-mode :pin "39a189d1e030aa108e90a82fd40f0042b1e69b21")
(package! ellama :pin "c9cf0ae36cac6d4a65bafb8f947e3195e2e2f5fd")
(package! rainbow-delimiters :pin "f40ece58df8b2f0fb6c8576b527755a552a5e763")

(package! hurl-mode
  :pin "93f992dea8e4fc2aaca7ef2d80a0e8229ed3a0f8"
  :recipe (:host github
           :repo "Orange-OpenSource/hurl"
           :files ("contrib/emacs/*.el")))

;; Disable some useless python modules of doom emacs
;; Free up keybinds shadowed by pkgs, like C-c C-p = run-python
(package! gptel
  :pin "cdbcdcbcef2152be97420dd737da08c0c51a324b"
  :recipe
  (:nonrecursive t))
(package! gptel-agent :pin "0d1534b203ea756c76d5161dfcd57ddc146f774e")
(package! gptel-quick
  :pin "018ff2be8f860a1e8fe3966eec418ad635620c38"
  :recipe (:host github :repo "karthink/gptel-quick"))
(package! ob-gptel
  :pin "cbed018a7d81de9ba8dc3220e1c4d10b7bb29b11"
  :recipe (:host github :repo "jwiegley/ob-gptel"))
(package! gptel-prompts
  :pin "7ce497590b006bb4b167abcdcbf2f069d9d72549"
  :recipe (:host github :repo "jwiegley/gptel-prompts"))

(package! ob-restclient :pin "94dd9cd98ff50717135ed5089afb378616faf11a")
(package! evil-tutor :pin "4e124cd3911dc0d1b6817ad2c9e59b4753638f28")

(package! rainbow-mode :pin "2e6b18609c2fdd1a2dc513937a64d276fd6cf24c")

(package! typst-ts-mode
  :pin "cf4d5282988068841efd51dc83f99091b41b91dd"
  :recipe
  (:host codeberg :repo "meow_king/typst-ts-mode"))
(package! typst-preview
  :pin "7e89cf105e4fef5e79977a4a790d5b3b18d305f6"
  :recipe (:host github :repo "havarddj/typst-preview.el"))
(package! orglink :pin "0de830edc6ffc0b07b95284f545ffe7d7c37dfb8")
(package! agent-shell :pin "73718e228c2011b0f645ed4c6b3f2377965b1940")
(package! knockknock
  ;; Original author:
  ;; :recipe '(:host 'github :repo "konrad1977/knockknock"))
  ;; Variant from xenodium to allow icons from more formats:
  ;; https://github.com/konrad1977/knockknock/pull/4
  :pin "7a6ab46503554317b639a7333ec8046d7d181520"
  :recipe (:host github :repo "xenodium/knockknock"))

(package! agent-shell-knockknock
  :pin "56732434067fe1874dcda62c491f7800bdc0a2f3"
  :recipe (:host github :repo "xenodium/agent-shell-knockknock"))

(package! org-modern :pin "713beb72aed4db43f8a10feed72136e931eb674a")
(package! org-appear :pin "32ee50f8fdfa449bbc235617549c1bccb503cb09")
;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! which-key-posframe :pin "e4a9ce9a1b20de550fca51f14d055821980d534a")

(package! org-web-tools :pin "7a6498f442fc7f29504745649948635c7165d847")
(package! macher :pin "16672b88967c3ea452d8670285e2ab7fc705ce17")
(package! ox-chameleon
  :pin "d52696836cee30eb911a57a640aee16d8fd4c015"
  :recipe (:host github :repo "tecosaur/ox-chameleon"))
(package! org-glossary
  :pin "eab0ffa07d02e6a406610db0dae56b4bda3760e7"
  :recipe (:host github :repo "tecosaur/org-glossary"))
(package! org-fragtog :pin "562f6590843eeab30ac8aa2ced285aff8d590861")
(package! systemd :pin "8742607120fbc440821acbc351fda1e8e68a8806")
