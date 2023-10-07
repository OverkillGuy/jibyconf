# Jibyconf - Jiby's dotfiles and config

## Dependencies

- [GNU Stow](https://www.gnu.org/software/stow/) for config files movement


## Usage


With GNU Stow installed, deploy first the "stow" package:

``` shell
stow --target ~ --dir stow
```

This will deploy the main `~/.stowrc` and `~/.stowignore`, allowing the rest to
be trivialized.

Deploy the next packages:

``` shell
stow bash
stow emacs-packages
# etc etc
```
