# The only true ~/.emacs.d config

This is my 4th emacs configuration, starting from scratch. This time I'm
trying to keep things minimal and only add them when I find the need for
them, instead of installing every single plugin in the universe.

To install simply clone the repo

    $ git clone https://github.com/darthdeus/emacs-rebirth ~/.emacs.d

So far this configuration is mostly setup for doing Haskell development.
There is a small amount of config for Rails, but not yet fully usable.

The main focus is on enabling `evil-mode` in order to get the same
benefits of editing as when using VIM, but to keep the interactivity of
emacs, such as the Interactive Haskell Mode.

## Known issues

The biggest challenge so far has been to unify the huge amount of
keybindings available in both modes. For example while `q` is very
useful in plain VIM, it becomes unusable in Emacs as many buffers use
`q` to close them, but other buffers use it to define a macro. I
couldn't yet figure out what the rule here is, as some buffers appear as
a _menu-ish_ one, but still want to define a macro. For that reason I've
disabled `q` completely when in normal mode.

### Tab completion conflicts between ~AC~ company mode and YASnippet

I'm not really sure how this is suppsed to work, but when in insert mode
in evil, `<tab>` is bound to `yas-expand-snippet`, which seems correct
... but sometimes it does invoke autocomplete instead of doing the
expansion, for example on `fun` in `haskell-mode` it almost always
autocompletes to `Functor` instead of expanding to a function.

## Haskell Mode

One of the main reasons why I'm using Emacs instead of VIM these days is
that haskell-mode is just far superior to vim2hs.

## License

MIT
