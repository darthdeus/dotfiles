local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
	"nvim-lua/plenary.nvim",

	"folke/neodev.nvim",
	{
		"folke/neoconf.nvim",
		config = function()
			require("neoconf").setup({})
		end,
	},

	"b0o/mapx.nvim",

	"rmagatti/goto-preview",

	{
		"folke/which-key.nvim",
		config = function()
			require("which-key").setup({
				triggers = { "<leader>" },
			})
		end,
	},

	"nvim-lua/popup.nvim",
	"nvim-telescope/telescope.nvim",

	"junegunn/fzf",
	"junegunn/fzf.vim",

	"whatsthatsmell/codesmell_dark.vim",
	"RRethy/nvim-base16",

	"jansedivy/jai.vim",
	"jose-elias-alvarez/null-ls.nvim",
	"Pocco81/auto-save.nvim",

	-- {
	--   "folke/noice.nvim",
	--   config = function()
	--     require("noice").setup({
	--       -- add any options here
	--       lsp = {
	--         signature = {
	--           enabled = false
	--         }
	--       }
	--     })
	--   end,
	--   dependencies = {
	--     -- if you lazy-load any plugin below, make sure to add proper `module="..."` entries
	--     "MunifTanjim/nui.nvim",
	--     -- OPTIONAL:
	--     --   `nvim-notify` is only needed, if you want to use the notification view.
	--     --   If not available, we use `mini` as the fallback
	--     -- "rcarriga/nvim-notify",
	--   }
	-- },

	{
		"folke/trouble.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		config = function()
			require("trouble").setup({})
		end,
	},

	"jaawerth/fennel.vim",
	"RRethy/vim-illuminate",

	-- "akinsho/toggleterm.nvim",

	"tpope/vim-fugitive",
	"tpope/vim-sensible",
	"tpope/vim-eunuch",
	"tpope/vim-surround",
	"tpope/vim-repeat",
	"tpope/vim-rsi",
	"EdenEast/nightfox.nvim",

	"elihunter173/dirbuf.nvim",

	"tikhomirov/vim-glsl",

	"kevinhwang91/nvim-bqf",

	"editorconfig/editorconfig-vim",

	-- "ggandor/leap.nvim",
	-- require("leap").add_default_mappings()

	"mileszs/ack.vim",
	"benmills/vimux",

	"junegunn/vim-easy-align",

	"terrortylor/nvim-comment",

	"jiangmiao/auto-pairs",

	"itchyny/lightline.vim",

	-- "sjl/gundo.vim",
	"mbbill/undotree",

	"sbdchd/neoformat",

	"rust-lang/rust.vim",
	"simrat39/rust-tools.nvim",

	"chrisbra/vim-zsh",
	"habamax/vim-godot",
	"ron-rs/ron.vim",
	"gutenye/json5.vim",
	"cespare/vim-toml",

	"othree/html5.vim",
	"pangloss/vim-javascript",
	"evanleck/vim-svelte",

	"mattn/emmet-vim",
	"chaimleib/vim-renpy",

	"DingDean/wgsl.vim",

	"preservim/nerdtree",

	-- if vim.fn.has("win32") ~= 1 then
	-- use("nvim-treesitter/playground")

	-- { "nvim-treesitter/nvim-treesitter", { build = ":TSUpdate", }},
	-- {
	--   "nvim-treesitter/nvim-treesitter",
	--   version = false, -- last release is way too old and doesn't work on Windows
	--   build = ":TSUpdate",
	--   event = { "BufReadPost", "BufNewFile" },
	-- },

	{ "nvim-treesitter/nvim-treesitter", build = ":TSUpdate" },
	{
		"nvim-treesitter/nvim-treesitter-context",
		config = function()
			require("treesitter-context").setup({})
		end,
	},

	"folke/tokyonight.nvim",

	"nvim-treesitter/playground",
	-- end

	"tenxsoydev/size-matters.nvim",

	------------------------------------------------------
	------------------------------------------------------
	------------------------------------------------------

	{
		"VonHeikemen/lsp-zero.nvim",
		branch = "v2.x",
		dependencies = {
			-- LSP Support
			{ "neovim/nvim-lspconfig" }, -- Required
			{ -- Optional
				"williamboman/mason.nvim",
				build = function()
					pcall(vim.cmd, "MasonUpdate")
				end,
			},
			{ "williamboman/mason-lspconfig.nvim" }, -- Optional

			-- Autocompletion
			{ "hrsh7th/nvim-cmp" }, -- Required
			{ "hrsh7th/cmp-nvim-lsp" }, -- Required
			{ "L3MON4D3/LuaSnip" }, -- Required
		},
	},

	------------------------------------------------------
	------------------------------------------------------
	------------------------------------------------------
	-- "williamboman/mason.nvim",
	-- "williamboman/mason-lspconfig.nvim",
	-- "neovim/nvim-lspconfig",
	-- {
	-- 	"L3MON4D3/LuaSnip",
	-- 	-- follow latest release.
	-- 	-- version = "1.*", -- Replace <CurrentMajor> by the latest released major (first number of latest release)
	-- 	-- install jsregexp (optional!).
	-- 	build = "make install_jsregexp",
	-- },

	-- "hrsh7th/vim-vsnip",
	-- "hrsh7th/vim-vsnip-integ",

	-- "hrsh7th/nvim-cmp",
	-- "hrsh7th/cmp-nvim-lsp",
	"hrsh7th/cmp-buffer",
	"hrsh7th/cmp-path",
	"hrsh7th/cmp-cmdline",
	------------------------------------------------------
	------------------------------------------------------
	------------------------------------------------------

	-- "hrsh7th/cmp-vsnip",
	"hrsh7th/cmp-copilot",
	"github/copilot.vim",

	-- -------------------------------

	-- "nvim-lua/lsp_extensions.nvim",

	"delphinus/cmp-ctags",

	"ray-x/lsp_signature.nvim",

	"mfussenegger/nvim-dap",
	-- use "zhimsel/vim-stay"

	-- --------------------------------

	"chrisbra/unicode.vim",
	{
		"nvim-telescope/telescope-fzf-native.nvim",
		build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
	},
	-- 'ryanoasis/vim-devicons',

	-- TODO: try https://github.com/L3MON4D3/LuaSnip ?
})

require("user")
require("user.settings")
require("user.remap")
require("user.reload")

require('telescope').load_extension('fzf')
-- require("toggleterm").setup()
-- require("base16-colorscheme").setup({ })

local dap = require("dap")
dap.adapters.codelldb = {
	type = "server",
	host = "127.0.0.1",
	port = 13000, -- 💀 Use the port printed out or specified with `--port`
}

-- vim.cmd('colorscheme base16-gruvbox-dark-soft')
-- vim.cmd('colorscheme base16-default-dark')
vim.cmd("colorscheme b16")

-- require("base16-colorscheme").setup {
--     base00 = "#16161D",
--     base01 = "#2c313c",
--     base02 = "#3e4451",
--     base03 = "#6c7891",
--     base04 = "#565c64",
--     base05 = "#abb2bf",
--     base06 = "#9a9bb3",
--     base07 = "#c5c8e6",
--     base08 = "#e06c75",
--     base09 = "#d19a66",
--     base0A = "#e5c07b",
--     base0B = "#98c379",
--     base0C = "#56b6c2",
--     base0D = "#0184bc",
--     base0E = "#c678dd",
--     base0F = "#a06949",
--
--     -- base00 = "#111111",
--     -- base01 = "#282828",
--     -- base02 = "#383838",
--     -- base03 = "#585858",
--     -- base04 = "#b8b8b8",
--     -- base05 = "#d8d8d8",
--     -- base06 = "#e8e8e8",
--     -- base07 = "#f8f8f8",
--     -- base08 = "#ab4642",
--     -- base09 = "#dc9656",
--     -- base0A = "#f7ca88",
--     -- -- base0B = "#a1b56c",
--     -- base0B = "#818cc5",
--     -- base0C = "#86c1b9",
--     -- base0D = "#7cafc2",
--     -- base0E = "#ba8baf",
--     -- base0F = "#a16946",
-- }

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
autocmd BufEnter *.lua setlocal shiftwidth=4
autocmd BufEnter *.rbl set ft=rbl
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
		step_size = 1, -- font resize step size
		notifications = false, -- default value is true if notify is installed else false
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

-- vim.o.exrc = true
-- vim.o.secure = true

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
