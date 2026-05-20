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
(package! ellama :pin "d6e7603f0218ec4e089e2957da1dd31d1d39ec5c")
(package! rainbow-delimiters :pin "f40ece58df8b2f0fb6c8576b527755a552a5e763")

(package! hurl-mode
  :pin "0f76060868d566139f6c3439ae813a16339f9daa"
  :recipe (:host github
           :repo "Orange-OpenSource/hurl"
           :files ("contrib/emacs/*.el")))

(package! gptel
  :pin "677eb955918ad5290432040bfa44d0ac4e05036d"
  :recipe (:nonrecursive t))
(package! macher :pin "55e75f2ee27eedc3cbf7e8bfcef27bcce11e3540")
(package! gptel-agent :pin "753e722778fcdefc165f049d27cbfea4fb909236")
(package! gptel-quick
  :pin "018ff2be8f860a1e8fe3966eec418ad635620c38"
  :recipe (:host github :repo "karthink/gptel-quick"))
(package! ob-gptel
  :pin "71584eb30e8317cf36104cec78b6d53c4433cae7"
  :recipe (:host github :repo "jwiegley/ob-gptel"))
(package! gptel-prompts
  :pin "7ce497590b006bb4b167abcdcbf2f069d9d72549"
  :recipe (:host github :repo "jwiegley/gptel-prompts"))

(package! ob-restclient :pin "94dd9cd98ff50717135ed5089afb378616faf11a")
(package! evil-tutor :pin "4e124cd3911dc0d1b6817ad2c9e59b4753638f28")

(package! rainbow-mode :pin "2e6b18609c2fdd1a2dc513937a64d276fd6cf24c")

(package! typst-ts-mode
  :pin "278562d702de429f5c4369c007913ca0ef1584f3"
  :recipe
  (:host codeberg :repo "meow_king/typst-ts-mode"))
(package! typst-preview
  :pin "7e89cf105e4fef5e79977a4a790d5b3b18d305f6"
  :recipe (:host github :repo "havarddj/typst-preview.el"))
(package! orglink :pin "e3c3999c4a88acd46208bef4f014fc2245f494a8")
(package! agent-shell :pin "3f3f91ea5292af36d6f31824dfde901636047bd6")
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

(package! org-modern :pin "4855ade77ab17de7587c37bde12a0afeab342783")
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
(package! ox-chameleon
  :pin "d52696836cee30eb911a57a640aee16d8fd4c015"
  :recipe (:host github :repo "tecosaur/ox-chameleon"))
(package! org-glossary
  :pin "7397aa00f13d782c0150de96a3d720c7bbbb4fea"
  :recipe (:host github :repo "tecosaur/org-glossary"))
(package! org-fragtog :pin "562f6590843eeab30ac8aa2ced285aff8d590861")
(package! systemd :pin "8742607120fbc440821acbc351fda1e8e68a8806")

(package! ob-sql-mode :pin "2eaf436a6ac2178b94442d80f84fc6c02aa644d8")
(package! org-transclusion :pin "2b010733bd888519d0dacb64f4acf023a2334a38")
(package! ox-hugo
  :recipe (:host github :repo "kaushalmodi/ox-hugo" :nonrecursive t)
  :pin "b7dc44dc28911b9d8e3055a18deac16c3b560b03")

(package! lua-mode :pin "2f6b8d7a6317e42c953c5119b0119ddb337e0a5f")
(package! dumb-jump :pin "9ce4598e9c485821a6e639fa48854d8e05acd970")
(package! easy-escape :pin "938497a21e65ba6b3ff8ec90e93a6d0ab18dc9b4")
(package! sql-indent :pin "0e93fb4878aa3788084ce44b8e8ec8919af708a4")
