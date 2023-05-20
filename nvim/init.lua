local fn = vim.fn
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
  fn.system({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path })
  vim.cmd("packadd packer.nvim")
end

-- local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
-- if not vim.loop.fs_stat(lazypath) then
--   vim.fn.system({
--     "git",
--     "clone",
--     "--filter=blob:none",
--     "--single-branch",
--     "https://github.com/folke/lazy.nvim.git",
--     lazypath,
--   })
-- end
-- vim.opt.runtimepath:prepend(lazypath)

require("packer").startup(function()
  use("wbthomason/packer.nvim")
  use("nvim-lua/plenary.nvim")

  use("b0o/mapx.nvim")

  use({
    "rmagatti/goto-preview",
    config = function()
      require("goto-preview").setup({})
    end,
  })

  use({
    "folke/which-key.nvim",
    config = function()
      require("which-key").setup({
        triggers = { "<leader>" },
      })
    end,
  })


  use("nvim-lua/popup.nvim")
  use("nvim-telescope/telescope.nvim")

  use("junegunn/fzf")
  use("junegunn/fzf.vim")

  use("whatsthatsmell/codesmell_dark.vim")
  use("RRethy/nvim-base16")

  use("jansedivy/jai.vim")
  use("jose-elias-alvarez/null-ls.nvim")
  use("Pocco81/auto-save.nvim")

  use("jaawerth/fennel.vim")
  use("RRethy/vim-illuminate")

  use({
    "akinsho/toggleterm.nvim",
    tag = '*',
    config = function()
      require("toggleterm").setup()
    end
  })

  use("tpope/vim-fugitive")
  use("tpope/vim-sensible")
  use("tpope/vim-eunuch")
  use("tpope/vim-surround")
  use("tpope/vim-repeat")
  use("tpope/vim-rsi")
  use("EdenEast/nightfox.nvim")

  use("elihunter173/dirbuf.nvim")

  use("tikhomirov/vim-glsl")

  use("kevinhwang91/nvim-bqf")

  use("editorconfig/editorconfig-vim")

  -- use("ggandor/leap.nvim")
  -- require("leap").add_default_mappings()

  use("mileszs/ack.vim")
  use("benmills/vimux")

  use("junegunn/vim-easy-align")

  use("terrortylor/nvim-comment")

  use("jiangmiao/auto-pairs")

  use("itchyny/lightline.vim")

  use("sjl/gundo.vim")

  use("sbdchd/neoformat")

  use("rust-lang/rust.vim")
  use("simrat39/rust-tools.nvim")

  use("chrisbra/vim-zsh")
  use("habamax/vim-godot")
  use("ron-rs/ron.vim")
  use("gutenye/json5.vim")
  use("cespare/vim-toml")

  use("othree/html5.vim")
  use("pangloss/vim-javascript")
  use("evanleck/vim-svelte")

  use("mattn/emmet-vim")
  use("chaimleib/vim-renpy")

  use("DingDean/wgsl.vim")

  use("preservim/nerdtree")

  if vim.fn.has("win32") ~= 1 then
    -- use("nvim-treesitter/playground")

    use("nvim-treesitter/nvim-treesitter", { run = ":TSUpdate", })
    use("nvim-treesitter/playground")
  end

  use("tenxsoydev/size-matters.nvim")

  -- -------------------------------
  use("williamboman/mason.nvim")
  use("williamboman/mason-lspconfig.nvim")
  use("neovim/nvim-lspconfig")

  -- use("nvim-lua/lsp_extensions.nvim")

  use("delphinus/cmp-ctags")

  use("ray-x/lsp_signature.nvim")

  use("mfussenegger/nvim-dap")
  -- use "zhimsel/vim-stay"

  -- --------------------------------

  use("chrisbra/unicode.vim")

  -- TODO: try https://github.com/L3MON4D3/LuaSnip ?

  use("github/copilot.vim")

  use("hrsh7th/vim-vsnip")
  use("hrsh7th/vim-vsnip-integ")

  use("hrsh7th/cmp-nvim-lsp")
  use("hrsh7th/cmp-buffer")
  use("hrsh7th/cmp-path")
  use("hrsh7th/cmp-cmdline")
  use("hrsh7th/nvim-cmp")
  use("hrsh7th/cmp-vsnip")
  use("hrsh7th/cmp-copilot")
end)

require("user")
require("user.settings")
require("user.remap")
require("user.reload")

-- TODO: do this with nvim_utils?
-- https://github.com/norcalli/nvim_utils
-- https://www.reddit.com/r/neovim/comments/n80hdb/autocmd_execution_in_neovim_lua_config/
-- https://github.com/norcalli/nvim_utils/blob/master/lua/nvim_utils.lua#L554-L567
-- https://github.com/neovim/neovim/pull/14661

vim.api.nvim_exec(
  [[
" autocmd CursorMoved,InsertLeave,BufEnter,BufWinEnter,TabEnter,BufWritePost * lua require'lsp_extensions'.inlay_hints{ prefix = ' » ', highlight = "Comment", enabled = {"TypeHint", "ChainingHint", "ParameterHint"} }

" Remember last location in file
aug last_location
  au!
  au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g'\"" | endif
aug END

aug various_file_types
  autocmd!
  autocmd BufNewFile,BufRead *vimrc set filetype=vim
  autocmd BufWritePost .Xresources,Xresources silent execute '!xrdb ~/.Xresources' | redraw | echom 'Xresources reloaded'
aug END


au BufRead,BufNewFile */funcs/* setfiletype zsh

" when you enter a (new) buffer
augroup set-commentstring-ag
autocmd!
autocmd BufEnter .zshrc.dot set ft=zsh
autocmd BufEnter *.clj,*.cljs :lua vim.api.nvim_buf_set_option(0, "commentstring", ";; %s")
autocmd BufEnter *.jai,*.wgsl,*.glsl,*.vert,*.frag :lua vim.api.nvim_buf_set_option(0, "commentstring", "// %s")
autocmd BufFilePost *.jai,*.wgsl,*.glsl,*.vert,*.frag :lua vim.api.nvim_buf_set_option(0, "commentstring", "// %s")
augroup END
]],
  false
)

require("nvim_comment").setup({
  create_mappings = false,
})

if vim.g.neovide or vim.g.goneovim or vim.g.nvui or vim.g.gnvim then
  require("size-matters").setup({
    default_mappings = true,
    step_size = 1,                                   -- font resize step size
    notifications = false,                           -- default value is true if notify is installed else false
    reset_font = vim.api.nvim_get_option("guifont"), -- Font loaded when using the reset cmd / shortcut
  })
end

vim.g.neoformat_try_node_exe = 1
vim.g.neoformat_only_msg_on_error = 1

-- vim.o.guifont = "Fantasque Sans Mono:8"
vim.g.neovide_cursor_animation_length = 0.00

vim.cmd([[
    let guifont="Arial:h12"
]])

-----------------------------------
-----------------------------------
-----------------------------------
-----------------------------------
-----------------------------------


-- vim.cmd([[
--   augroup packer_user_config
--     autocmd!
--     autocmd BufWritePost plugins.lua source <afile> | PackerCompile
--   augroup end
-- ]])

vim.o.exrc = true
vim.o.secure = true

-- TODO:
-- - https://github.com/bfredl/nvim-luadev
-- - https://github.com/tjdevries/nlua.nvim
-- - https://github.com/nvim-lua/plenary.nvim
-- - https://github.com/nvim-lua/popup.nvim
-- - https://github.com/famiu/bufdelete.nvim
-- use 'rafcamlet/nvim-luapad'
-- use "liuchengxu/vista.vim"
-- use "glepnir/lspsaga.nvim"
-- use "tpope/vim-scriptease"
-- use 'ggandor/lightspeed.nvim'
-- use "kevinhwang91/nvim-hlslens"
-- use 'lewis6991/gitsigns.nvim'
-- use { "wfxr/minimap.vim", run = "cargo install --locked code-minimap" }

-- TODO: maybe without icons?
-- use {
--     "kyazdani42/nvim-tree.lua",
--     requires = "kyazdani42/nvim-web-devicons",
-- }
-- TODO: maybe try again?
-- use 'junegunn/goyo.vim'
-- use 'junegunn/limelight.vim'
-- use 'ludovicchabant/vim-gutentags'
-- use 'krisajenkins/vim-projectlocal'
-- use "eshock/vim-matchit"
--
-- TODO: maybe try these too?
-- TODO: try tjdevries/colorbuddy.nvim ?
-- https://github.com/JohnnyMorganz/StyLua
-- https://github.com/Koihik/LuaFormatter
-- use_rocks { "luaformatter", server = "https://luarocks.org/dev" }
