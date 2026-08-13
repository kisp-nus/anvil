# Vim support for AnvilHDL

This runtime plugin provides filetype detection, syntax highlighting, comment
settings, and indentation for `.anvil` files.

## Installation

Add this directory to Vim's `runtimepath`. For example, from an Anvil checkout:

```vim
set runtimepath+=/path/to/anvil/editors/vim
filetype plugin indent on
syntax enable
```

For Neovim, use the equivalent Lua configuration:

```lua
vim.opt.runtimepath:append('/path/to/anvil/editors/vim')
vim.cmd('filetype plugin indent on')
vim.cmd('syntax enable')
```

Alternatively, symlink this directory into a native package location:

- Vim: `~/.vim/pack/anvil/start/anvil`
- Neovim: `~/.local/share/nvim/site/pack/anvil/start/anvil`
