-- vim.g.loaded_netrw = 1
-- vim.g.loaded_netrwPlugin = 1

-- set termguicolors to enable highlight groups
vim.opt.termguicolors = true

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

	{ "rose-pine/neovim", name = "rose-pine" },

	{
		"folke/trouble.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		config = function()
			require("trouble").setup({})
		end,
	},

	"Pocco81/auto-save.nvim",
	{
		"j-hui/fidget.nvim",
		tag = "legacy",
		config = function()
			require("fidget").setup()
		end,
	},

	{
		"nvim-lualine/lualine.nvim",
		-- requires = { "nvim-tree/nvim-web-devicons", opt = true },
		requires = { "RRethy/nvim-base16" },
	},

	{
		"nvim-tree/nvim-tree.lua",
		config = function()
			require("nvim-tree").setup({
				view = {
					width = "10%",
				},
				actions = {
					open_file = {
						window_picker = {
							enable = false,
						},
					},
				},
			})
		end,
	},

	"rmagatti/auto-session",
	-- "zwhitchcox/auto-session-nvim-tree",

	"jansedivy/jai.vim",

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
		"folke/todo-comments.nvim",
		dependencies = { "nvim-lua/plenary.nvim" },
		config = function()
			require("todo-comments").setup({
				signs = false,
			})
		end,
	},

	{
		"gbprod/yanky.nvim",
		config = function()
			require("yanky").setup({
				preserve_cursor_position = {
					enabled = true,
				},

				highlight = {
					on_put = false,
					on_yank = false,
					timer = 0,
				},
			})
		end,
	},

	"jose-elias-alvarez/null-ls.nvim",

	"jaawerth/fennel.vim",
	"RRethy/vim-illuminate",

	-- "akinsho/toggleterm.nvim",

	"tpope/vim-rhubarb",
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

	{
		"terrortylor/nvim-comment",
		config = function()
			require("nvim_comment").setup({
				create_mappings = false,
			})
		end,
	},

	"jiangmiao/auto-pairs",

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

	{
		"saecki/crates.nvim",
		dependencies = { "jose-elias-alvarez/null-ls.nvim" },
		config = function()
			require("crates").setup({
				null_ls = {
					enabled = true,
					name = "crates.nvim",
				},
			})
		end,
	},

	-- "preservim/nerdtree",

	-- if vim.fn.has("win32") ~= 1 then
	-- use("nvim-treesitter/playground")

	-- {
	--   "nvim-treesitter/nvim-treesitter",
	--   version = false, -- last release is way too old and doesn't work on Windows
	--   build = ":TSUpdate",
	--   event = { "BufReadPost", "BufNewFile" },
	-- },

	{ "nvim-treesitter/nvim-treesitter", build = ":TSUpdate" },
	-- {
	-- 	"nvim-treesitter/nvim-treesitter-context",
	-- 	config = function()
	-- 		require("treesitter-context").setup({})
	-- 	end,
	-- },

	"folke/tokyonight.nvim",

	"nvim-treesitter/playground",
	-- end

	{
		"tenxsoydev/size-matters.nvim",
		config = function()
			if vim.g.neovide or vim.g.goneovim or vim.g.nvui or vim.g.gnvim then
				require("size-matters").setup({
					default_mappings = true,
					step_size = 1, -- font resize step size
					notifications = false, -- default value is true if notify is installed else false
					reset_font = vim.api.nvim_get_option("guifont"), -- Font loaded when using the reset cmd / shortcut
				})
			end
		end,
	},

	------------------------------------------------------
	------------------------------------------------------
	------------------------------------------------------

	{
		"VonHeikemen/lsp-zero.nvim",
		branch = "v2.x",
		dependencies = {

			-- Autocompletion
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
	{ "hrsh7th/cmp-nvim-lua" },
	{ "hrsh7th/cmp-nvim-lsp-signature-help" },
	{ "hrsh7th/nvim-cmp" }, -- Required
	{ "hrsh7th/cmp-nvim-lsp" }, -- Required
	{ "L3MON4D3/LuaSnip" }, -- Required
	-- LSP Support
	{ "neovim/nvim-lspconfig" }, -- Required
	{ -- Optional
		"williamboman/mason.nvim",
	},
	{ "williamboman/mason-lspconfig.nvim" }, -- Optional

	{
		"ibhagwan/fzf-lua",
		-- optional for icon support
		dependencies = { "nvim-tree/nvim-web-devicons" },
		config = function()
			-- calling `setup` is optional for customization
			require("fzf-lua").setup({})
		end,
	},

	------------------------------------------------------
	------------------------------------------------------
	------------------------------------------------------

	--	"hrsh7th/cmp-copilot",
	--"github/copilot.vim",

	-- -------------------------------

	-- "nvim-lua/lsp_extensions.nvim",

	"delphinus/cmp-ctags",

	"ray-x/lsp_signature.nvim",

	"mfussenegger/nvim-dap",
	"rcarriga/nvim-dap-ui",
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

require("user.settings")
require("user.remap")
require("user.reload")

local auto_session = require("auto-session")
-- local auto_session_nvim_tree = require("auto-session-nvim-tree")

auto_session.setup({
	log_level = "error",
	cwd_change_handling = {
		restore_upcoming_session = true, -- This is necessary!!
	},
})

-- auto_session_nvim_tree.setup(auto_session)

local telescope = require("telescope")
-- TODO:
-- telescope.load_extension("fzf")
telescope.load_extension("yank_history")


-- require("toggleterm").setup()

local dap = require("dap")
dap.adapters.codelldb = {
	type = "server",
	host = "127.0.0.1",
	port = 13000, -- 💀 Use the port printed out or specified with `--port`
}

-- dap.configurations.rust = {
-- 	{
-- 		type = "python",
-- 		request = "launch",
-- 		name = "Launch file",
-- 		program = "${file}",
-- 		pythonPath = function()
-- 			return "/usr/bin/python"
-- 		end,
-- 	},
-- }

require("neodev").setup({
	library = { plugins = { "nvim-dap", "nvim-dap-ui" }, types = true },
})

require("dapui").setup()

vim.keymap.set("n", "]t", function()
	require("todo-comments").jump_next()
end, { desc = "Next todo comment" })

vim.keymap.set("n", "[t", function()
	require("todo-comments").jump_prev()
end, { desc = "Previous todo comment" })

-- vim.cmd('colorscheme base16-gruvbox-dark-soft')
-- vim.cmd('colorscheme base16-default-dark')
vim.cmd("colorscheme b16")
-- vim.cmd('colorscheme rose-pine')

require("lualine").setup({})


-- require("base16-colorscheme").setup {
--     -- base00 = "#16161D",
--     -- base01 = "#2c313c",
--     -- base02 = "#3e4451",
--     -- base03 = "#6c7891",
--     -- base04 = "#565c64",
--     -- base05 = "#abb2bf",
--     -- base06 = "#9a9bb3",
--     -- base07 = "#c5c8e6",
--     -- base08 = "#e06c75",
--     -- base09 = "#d19a66",
--     -- base0A = "#e5c07b",
--     -- base0B = "#98c379",
--     -- base0C = "#56b6c2",
--     -- base0D = "#0184bc",
--     -- base0E = "#c678dd",
--     -- base0F = "#a06949",
--
--     base00 = "#111111",
--     base01 = "#282828",
--     base02 = "#383838",
--     base03 = "#585858",
--     base04 = "#b8b8b8",
--     base05 = "#d8d8d8",
--     base06 = "#e8e8e8",
--     base07 = "#f8f8f8",
--     base08 = "#ab4642",
--     base09 = "#dc9656",
--     base0A = "#f7ca88",
--     base0B = "#a1b56c",
--     -- base0B = "#818cc5",
--     base0C = "#86c1b9",
--     base0D = "#7cafc2",
--     base0E = "#ba8baf",
--     base0F = "#a16946",
-- }

-- TODO: do this with nvim_utils?
-- https://github.com/norcalli/nvim_utils
-- https://www.reddit.com/r/neovim/comments/n80hdb/autocmd_execution_in_neovim_lua_config/
-- https://github.com/norcalli/nvim_utils/blob/master/lua/nvim_utils.lua#L554-L567
-- https://github.com/neovim/neovim/pull/14661

vim.api.nvim_exec2(
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

let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit',
  \ 'ctrl-q': 'fill_quickfix'}
]],
	{}
)

vim.g.neoformat_try_node_exe = 1
vim.g.neoformat_only_msg_on_error = 1

-- vim.o.guifont = "Fantasque Sans Mono:8"
vim.g.neovide_cursor_animation_length = 0.00

vim.cmd([[
    let guifont="Arial:h12"
]])

-----------------------------------
-----------------------------------
---------- LSP --------------------
-----------------------------------

require("mason").setup()
require("mason-lspconfig").setup({
	ensure_installed = {
		"rust_analyzer",
		"taplo",
		"clangd",
		"lua_ls",
		"jsonls",
		"pylsp",
	},
})

local my_lsp = require("user.lsp_shared")

my_lsp.setup_keymaps()
my_lsp.setup_cmp()
my_lsp.setup_lsp_servers()

vim.api.nvim_exec2(
	[[
command! Ner :NvimTreeToggle
]],
	{}
)

-----------------------------------
-----------------------------------
---------- TREESITTEr--------------
-----------------------------------

local parsers = require("nvim-treesitter.parsers")
local parser_config = parsers.get_parser_configs()

parser_config.rebel = {
	install_info = {
		url = "~/projects/tree-sitter-rebel", -- local path or git repo
		files = { "src/parser.c" }, -- note that some parsers also require src/scanner.c or src/scanner.cc
		-- optional entries:
		branch = "master", -- default branch in case of git repo if different from master
		generate_requires_npm = true, -- if stand-alone parser without npm dependencies
		requires_generate_from_grammar = true, -- if folder contains pre-generated src/parser.c
	},
	filetype = "rbl", -- if filetype does not match the parser name
}

require("nvim-treesitter.configs").setup({
	ensure_installed = {
		"c",
		"json",
		"javascript",
		"python",
		"rust",
		"lua",
		"wgsl",
		"fennel",
		"commonlisp",
		"jsonc",
		"bash",
		"markdown",
		"markdown_inline",
		"regex",
		"html",
		"clojure",
	},
	highlight = {
		enable = true,
	},
	incremental_selection = {
		enable = true,
		keymaps = {
			-- init_selection = "gnn",
			-- node_incremental = "grn",
			-- scope_incremental = "rc",
			-- node_decremental = "grm",
			init_selection = "`",
			node_incremental = "`",
			node_decremental = "~",
			scope_incremental = "rc",
		},
	},
	textobjects = {
		select = {
			enable = true,
			keymaps = {
				-- You can use the capture groups defined in textobjects.scm
				["af"] = "@function.outer",
				["if"] = "@function.inner",
				["ac"] = "@class.outer",
				["ic"] = "@class.inner",

				-- Or you can define your own textobjects like this
				["iF"] = {
					python = "(function_definition) @function",
					cpp = "(function_definition) @function",
					c = "(function_definition) @function",
					java = "(method_declaration) @function",
				},
			},
		},
		move = {
			enable = true,
			goto_next_start = {
				["]a"] = "@function.outer",
				["]]"] = "@class.outer",
			},
			goto_next_end = {
				["]A"] = "@function.outer",
				["]["] = "@class.outer",
			},
			goto_previous_start = {
				["[a"] = "@function.outer",
				["[["] = "@class.outer",
			},
			goto_previous_end = {
				["[A"] = "@function.outer",
				["[]"] = "@class.outer",
			},
		},

		swap = {
			enable = true,
			swap_next = {
				["<leader>a"] = "@parameter.inner",
			},
			swap_previous = {
				["<leader>A"] = "@parameter.inner",
			},
		},
	},
})

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
