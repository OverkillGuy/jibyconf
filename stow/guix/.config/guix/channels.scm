;; Inspired by:
;; https://github.com/mbroeders/dotfiles/blob/51cbf0d8afe7f79b640b388869bf2ee44d8a44da/.config/guix/channels.scm
(list (channel
        (name 'flat)
        (url "https://github.com/flatwhatson/guix-channel.git")
        (commit
          "302f8a4f7e56cb3b484de9fe86617a3aaf20098c")
        (introduction
          (make-channel-introduction
            "33f86a4b48205c0dc19d7c036c85393f0766f806"
            (openpgp-fingerprint
              "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
      (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix"))
      (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (commit
          "b1cabedd28b92324259875fc52ca5d52d411a026")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
