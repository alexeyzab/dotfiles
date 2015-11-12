# Dotfiles

This repo uses [rcm][]. Also, all dotfiles are kept in tag-specific
directories. Inspired by [pbrisbin's
dotfiles](https://github.com/pbrisbin/dotfiles).

For example, if you want only my neovim setup:

```
% git clone https://github.com/alexeyzab/dotfiles .alexeyzab-dotfiles
% rcup -d .alexeyzab-dotfiles -x README.md -t nvim
```

These options could be made the default in your own `~/.rcrc`.

See more details with `man 7 rcm`.

[rcm]: https://github.com/thoughtbot/rcm
