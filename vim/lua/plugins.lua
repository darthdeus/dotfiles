local fn = vim.fn
local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
    fn.system { "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path }
    vim.cmd "packadd packer.nvim"
end

-- TODO:
-- - https://github.com/bfredl/nvim-luadev
-- - https://github.com/tjdevries/nlua.nvim
-- - https://github.com/nvim-lua/plenary.nvim
-- - https://github.com/nvim-lua/popup.nvim

return require("packer").startup(function()
    use "wbthomason/packer.nvim"

    -- use 'rafcamlet/nvim-luapad'

    use "tpope/vim-fugitive"
    use "tpope/vim-sensible"
    use "tpope/vim-eunuch"
    use "tpope/vim-surround"
    use "tpope/vim-scriptease"
    use "tpope/vim-repeat"
    use "tpope/vim-rsi"

    -- use 'ggandor/lightspeed.nvim'
    use "kevinhwang91/nvim-bqf"
    use "kevinhwang91/nvim-hlslens"
    use "TimUntersberger/neogit"
    -- use 'lewis6991/gitsigns.nvim'

    use "editorconfig/editorconfig-vim"

    use "mileszs/ack.vim"

    -- use 'calviken/vim-gdscript3'
    use "habamax/vim-godot"

    -- TODO: maybe try again?
    -- use 'junegunn/goyo.vim'
    -- use 'junegunn/limelight.vim'

    use "darthdeus/a.vim"
    use "benmills/vimux"

    use "chrisbra/vim-zsh"
    -- use 'ludovicchabant/vim-gutentags'

    use "LnL7/vim-nix"

    use {
        "junegunn/fzf",
        run = function()
            vim.fn["fzf#install"]()
        end,
    }
    use "junegunn/fzf.vim"
    use "junegunn/vim-easy-align"

    use "terrortylor/nvim-comment"
    -- use "tomtom/tcomment_vim"
    -- use "tomtom/tlib_vim"

    use "jiangmiao/auto-pairs"

    use "itchyny/lightline.vim"

    use { "scrooloose/nerdtree", cmd = "NERDTree" }
    -- TODO: , { 'on': 'NERDTree' }

    -- use 'krisajenkins/vim-projectlocal'
    use "AndrewRadev/switch.vim"
    -- use "eshock/vim-matchit"

    use "sjl/gundo.vim"

    -- use "othree/html5.vim"
    -- use "mattn/emmet-vim"

    use "sbdchd/neoformat"

    use "rust-lang/rust.vim"
    -- use 'rhysd/rust-doc.vim'

    use "ron-rs/ron.vim"
    use "gutenye/json5.vim"
    use "cespare/vim-toml"

    -- use 'Shougo/unite.vim'
    -- if has('nvim')
    --   use 'Shougo/denite.nvim', { 'do': ':UpdateRemotePlugins' }
    -- else
    --   use 'Shougo/denite.nvim'
    --   use 'roxma/nvim-yarp'
    --   use 'roxma/vim-hug-neovim-rpc'
    -- endif

    -- <TAB>: completion for deoplete from https://github.com/Shougo/deoplete.nvim/issues/816
    -- inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
    -- inoremap <expr><s-TAB>  pumvisible() ? "\<C-p>" : "\<TAB>"

    -- ------------------------------------

    -- We recommend updating the parsers on update
    use { "nvim-treesitter/nvim-treesitter", run = ":TSUpdate" }
    -- TODO: try tjdevries/colorbuddy.nvim ?
    use "nvim-treesitter/playground"

    -- TODO: maybe try these too?
    -- https://github.com/JohnnyMorganz/StyLua
    -- https://github.com/Koihik/LuaFormatter
    use_rocks { "luaformatter", server = "https://luarocks.org/dev" }

    -- -------------------------------
    -- use 'zxqfl/tabnine-vim'
    -- -------------------------------
    use "neovim/nvim-lspconfig"
    use "kabouzeid/nvim-lspinstall"
    use "nvim-lua/lsp_extensions.nvim"

    use "simrat39/rust-tools.nvim"

    use "nvim-lua/plenary.nvim"
    use "nvim-lua/popup.nvim"
    use "nvim-telescope/telescope.nvim"

    use "mfussenegger/nvim-dap"

    use "zhimsel/vim-stay"

    -- --------------------------------

    use "chrisbra/unicode.vim"

    use "hrsh7th/nvim-compe"

    use "hrsh7th/vim-vsnip"
    use "hrsh7th/vim-vsnip-integ"

    -- " Install nvim-cmp
    -- use 'hrsh7th/nvim-cmp'
    -- " Install snippet engine (This example installs [hrsh7th/vim-vsnip](https://github.com/hrsh7th/vim-vsnip))
    -- use 'hrsh7th/vim-vsnip'
    -- " Install the buffer completion source
    -- use 'hrsh7th/cmp-buffer'

    -- use 'nvim-lua/completion-nvim'
    -- use 'steelsojka/completion-buffers'
    -- use 'aca/completion-tabnine', { 'do': './install.sh' }
end)
