# .emacs.d

Lightweight [`use-package`](https://github.com/jwiegley/use-package) & [`evil-mode`](https://github.com/emacs-evil/evil) based emacs config.

## Directory Structure

- `core/`: Core emacs configuration
- `base/`: Base non-core emacs modules
- `lang/`: Programming language specific package configuration

## Debugging

The default warning level is `:error` and `use-package`'s verbose mode is
turned off. To reset the warning level to it's default and enable
`use-package`'s verbose mode you can set the `EMACS_INIT_DEBUG` environment
variable.

## Author

Tobias Svensson, [@endofunky](https://twitter.com/endofunky), [http://github.com/endofunky](http://github.com/endofunky)
