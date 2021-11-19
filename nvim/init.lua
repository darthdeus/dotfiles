local fn = vim.fn
local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
    fn.system { "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path }
    vim.cmd "packadd packer.nvim"
end

require("packer").startup(function()
    use "wbthomason/packer.nvim"
    use "b0o/mapx.nvim"

    use {
        "rmagatti/goto-preview",
        config = function()
            require("goto-preview").setup {}
        end,
    }

    use {
        "folke/which-key.nvim",
        config = function()
            require("which-key").setup {
                triggers = { "<leader>" },
            }
        end,
    }

    use "nvim-lua/plenary.nvim"
    use "nvim-lua/popup.nvim"
    use "nvim-telescope/telescope.nvim"

    use "whatsthatsmell/codesmell_dark.vim"
    use "RRethy/nvim-base16"

    use "tpope/vim-fugitive"
    use "tpope/vim-sensible"
    use "tpope/vim-eunuch"
    use "tpope/vim-surround"
    use "tpope/vim-repeat"
    use "tpope/vim-rsi"

    use "kevinhwang91/nvim-bqf"
    use "TimUntersberger/neogit"

    use "editorconfig/editorconfig-vim"

    use "mileszs/ack.vim"
    use "benmills/vimux"

    use {
        "jameshiew/nvim-magic",
        config = function()
            require("nvim-magic").setup()
        end,
        tag = "v0.2.2", -- recommended to pin to a tag and update manually as there may be breaking changes
        requires = {
            "nvim-lua/plenary.nvim",
            "MunifTanjim/nui.nvim",
        },
    }

    use {
        "junegunn/fzf",
        dir = "~/.fzf",
        run = "./install --all",
    }
    use "junegunn/fzf.vim"
    use "junegunn/vim-easy-align"

    use "terrortylor/nvim-comment"

    use "jiangmiao/auto-pairs"

    use "itchyny/lightline.vim"

    use { "scrooloose/nerdtree", cmd = "NERDTree" }

    use "sjl/gundo.vim"

    use "sbdchd/neoformat"

    use "rust-lang/rust.vim"
    --     -- use 'rhysd/rust-doc.vim'
    use "simrat39/rust-tools.nvim"

    use "chrisbra/vim-zsh"
    use "habamax/vim-godot"
    use "LnL7/vim-nix"
    use "ron-rs/ron.vim"
    use "gutenye/json5.vim"
    use "cespare/vim-toml"
    use "othree/html5.vim"
    use "mattn/emmet-vim"

    if vim.fn.has("win32") ~= 1 then
      use { "nvim-treesitter/nvim-treesitter", run = ":TSUpdate" }
      use "nvim-treesitter/playground"
    end

    -- -------------------------------
    use "neovim/nvim-lspconfig"
    use "kabouzeid/nvim-lspinstall"
    use "nvim-lua/lsp_extensions.nvim"

    use "ray-x/lsp_signature.nvim"
    use "akinsho/toggleterm.nvim"

    use "mfussenegger/nvim-dap"
    -- use "zhimsel/vim-stay"

    -- --------------------------------

    use "chrisbra/unicode.vim"

    use "github/copilot.vim"
    use "hrsh7th/nvim-compe"
    -- use { "tzachar/compe-tabnine", run = "./install.sh", requires = "hrsh7th/nvim-compe" }
    use "hrsh7th/vim-vsnip"
    use "hrsh7th/vim-vsnip-integ"
end)

vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.incsearch = true
vim.o.hlsearch = true

vim.o.hidden = true

vim.o.scrolloff = 9
vim.o.number = false
vim.o.relativenumber = false
vim.o.showmode = false

vim.o.timeoutlen = 500

vim.o.wrap = false
vim.o.tabstop = 2
vim.o.shiftwidth = 2
vim.o.softtabstop = 2
vim.o.expandtab = true

vim.o.encoding = "utf-8"
vim.o.fileencoding = "utf-8"
vim.o.fileencodings = "utcs-bom,utf8,latin2"

-- Use modeline overrides
vim.o.modeline = true
vim.o.modelines = 10

vim.o.winwidth = 75

vim.o.wildmode = "list:longest,list:full"
vim.o.wildignore = "obj,*.o,*.obj,.git,*.rbc,*.class,.svn,vendor/gems/*,node_modules,tmp,project/target,target,tags,CMakeFiles,bower_components,dist,_darcs,vcr,app/assets/images,*.dSYM,*.pyc,_build,rel,*.a,priv/static,*.aux,*.dvi,*.xmpi,*.out,*.lot,*.lof,*.blg,*.bbl,*.toc,__pycache__,build,logs,tags"

-- TODO: default is menu,preview?
vim.o.completeopt = "menuone,preview,noinsert,noselect"

vim.g.mapleader = ","
vim.g.maplocalleader = ","

-- vim.api.nvim_exec([[
-- nnoremap <silent> <leader> :<C-u>WhichKey ','<CR>
-- nnoremap <silent> <localleader> :<C-u>WhichKey ','<CR>
-- ]], false)
-- map ("n", "<leader>", ":WhichKey ','<CR>")

vim.o.backupcopy = "yes"
vim.o.list = false
vim.o.listchars = "tab:--,trail:."
vim.o.pastetoggle = "<F3>"
vim.o.undofile = true

-- vim.g.base16colorspace = 256
-- TODO: true?
-- vim.o.termguicolors = true
-- vim.o.term = "xterm-256color"

vim.api.nvim_exec(
    [[
if exists('+termguicolors')
  let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
endif
]],
    false
)

-- vim.cmd "set fillchars+=vert:|"
vim.cmd "set fillchars+=vert:│"
-- vim.cmd "colorscheme codesmell_dark"
-- vim.cmd "color base16-default-dark"
vim.cmd "color b16"

-- require("base16-colorscheme").setup {
--     base00 = "#141414",
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
--     base0C = "#86c1b9",
--     base0D = "#7cafc2",
--     base0E = "#ba8baf",
--     base0F = "#a16946",
-- }

vim.g.VimuxOrientation = "h"

-- TODO: figure out how to remap these?
-- nunmap ]f
-- nunmap [f

-- nn <leader>v :Vista!!<cr>
vim.g.vista_default_executive = "nvim_lsp"

require("mapx").setup { global = true }

nnoremap("<leader>sd", ":Telescope help_tags<CR>")
nnoremap("<leader>sa", ":Telescope commands<CR>")

-- map("n", "<leader>sf", "<cmd>Telescope live_grep<CR>")
-- map("n", "<leader>h", "<cmd>Telescope help_tags<CR>")
-- map("n", "<leader>h", "<cmd>Telescope help_tags<CR>")

-- Open files with <leader>f
nnoremap("<leader>f", ":Files ./<CR>")
-- nnoremap("<leader>f", "<cmd>Telescope find_files<CR>")
nnoremap("<leader>F", ":FZF %%<CR>")
nnoremap("<leader>gt", ":Tags<cr>")
-- map("n", "<leader>gt", "<cmd>Telescope tags<CR>")
nnoremap("<leader>ga", ":Rg<cr>")
-- map("n", "<leader>ga", ":Telescope live_grep<cr>")

nnoremap("<leader>gd", ":Rg <C-r><C-w><cr>")
-- map("n", "<leader>gd", ":Telescope live_grep <C-r><C-w><cr>")
nnoremap("<leader>b", ":Buffers<cr>")
-- map("n", "<leader>b", ":Telescope buffers<cr>")
nnoremap("<leader>B", ":BTags<cr>")
-- map("n", "<leader>B", ":Telescope current_buffer_tags<cr>")

-- Mapping selecting mappings
nnoremap("<leader><tab>", "<plug>(fzf-maps-n)")
xnoremap("<leader><tab>", "<plug>(fzf-maps-x)")
onoremap("<leader><tab>", "<plug>(fzf-maps-o)")

nnoremap("gp", "<cmd>lua require('goto-preview').goto_preview_definition()<CR>")

inoremap("<silent><expr>", "<C-Space> compe#complete()")
-- map("n", "<silent><expr>", "<CR>      compe#confirm('<CR>')")
inoremap("<silent><expr>", "<C-e>     compe#close('<C-e>')")
inoremap("<silent><expr>", "<C-f>     compe#scroll({ 'delta': +4 })")
inoremap("<silent><expr>", "<C-d>     compe#scroll({ 'delta': -4 })")

-- Expand
inoremap("<expr>", "<C-j>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-j>'")
snoremap("<expr>", "<C-j>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-j>'")

-- Expand or jump
inoremap("<expr>", "<C-l>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'")
snoremap("<expr>", "<C-l>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'")

nnoremap("<C-_><C-_>", ":CommentToggle<CR>")
vnoremap("<C-_><C-_>", ":CommentToggle<CR>")

nnoremap("<CR>", ":nohlsearch<CR>/<BS>")

-- Buffer resizing with arrow keys
nnoremap("<Up>", "<C-w>5-")
nnoremap("<Down>", "<C-w>5+")
nnoremap("<Left>", "<C-w>5<")
nnoremap("<Right>", "<C-w>5>")

nnoremap("<C-a>", "^")
nnoremap("<C-e>", "$")

inoremap("<C-a>", "<Home>")
inoremap("<C-e>", "<End>")

-- For easier navigation between windows
nnoremap("<C-j>", "<C-w><C-j>")
nnoremap("<C-k>", "<C-w><C-k>")
nnoremap("<C-h>", "<C-w><C-h>")
tnoremap("<C-h>", "<C-\\><C-n><C-w><C-h>")
nnoremap("<C-l>", "<C-w><C-l>")

vnoremap("-", ":Neoformat<cr>")

-- Bubble multiple lines
vnoremap("<C-Up>", "<C-w><C-k>")
vnoremap("<C-Down>", "<C-w><C-j>")
vnoremap("<C-Left>", "<C-w><C-h>")
vnoremap("<C-Right>", "<C-w><C-l>")

inoremap("<C-X><C-@>", "<C-A>")

nnoremap("-", ":Neoformat<cr>")

if vim.fn.has("win32") == 1 then
  nnoremap("<leader>ge", ":vs C:/users/jakub/dotfiles/nvim/init.lua<CR>")
  nnoremap("<leader>e", ':TermExec cmd="make" direction=vertical size=80 go_back=0<cr>')
  -- nnoremap("<leader>e", ":TermExec cmd='make' direction='vertical' size=80<cr>")
  -- nnoremap("<leader>e", ":TermExec cmd='make' direction='vertical' open=0 size=80<cr>")
  --
  nnoremap("<leader>ma", ":cd C:/dev/cubes-of-flesh<CR>")
  nnoremap("<leader>mb", ":cd C:/dev/BITGUN<CR>")
else
  nnoremap("<leader>ge", ":vs ~/.config/nvim/init.lua<CR>")
  nnoremap("<Leader>e", ":call VimuxRunCommand('c')<cr>")
end

-- Expand %% to directory path of current buffer
cnoremap("%%", "<C-R>=expand('%:h').'/'<CR>")



nnoremap("<F8>", ":ToggleTerm<cr>")
inoremap("<F8>", ":ToggleTerm<cr>")
tnoremap("<F8>", "<C-\\><C-n>:ToggleTerm<cr>")
tnoremap("<Esc>", "<C-\\><C-n>:ToggleTerm<cr>")
nnoremap("<F5>", ":call VimuxRunCommand('make')<cr>")
nnoremap("<F4>", ":call VimuxRunCommand('make')<cr>")
-- map("n", <leader>r :call VimuxRunCommand("make ". expand("%h"))<cr>
nnoremap("<leader>r", ":call VimuxRunCommand('make test')<cr>")
nnoremap("<leader>c", ":call VimuxRunCommand('make clean')<cr>")

-- Inserts the path of the currently edited file in command mode
cnoremap("<C-P>", "<C-R>=expand('%:p:h') . '/' <CR>")
-- imap <c-x><c-k> <plug>(fzf-complete-word)
inoremap("<c-x><c-f>", "<plug>(fzf-complete-path)")
inoremap("<c-x><c-j>", "<plug>(fzf-complete-file-ag)")
-- imap <c-x><c-l> <plug>(fzf-complete-line)
xnoremap("ga", "<Plug>(EasyAlign)")
nnoremap("ga", "<Plug>(EasyAlign)")
nnoremap("<leader>ws", ":%s/ *$//g<cr><c-o><cr>")
nnoremap("Q", "<NOP>")
nnoremap("<leader><leader>", "<c-^>")

nnoremap(
    "<leader>lt",
    ":!ctags --extras=+f --exclude=build --exclude=public --exclude=target --exclude=node_modules --exclude=.git -R *<CR>"
)
nnoremap("<C-\\>", ":tnext<CR>")

nnoremap("<silent>", "<leader>y :<C-u>silent '<,'>w !pbcopy<CR>")

nnoremap("<F9>", ":Neogit<CR>")

-- TODO: do this with nvim_utils?
-- https://github.com/norcalli/nvim_utils
-- https://www.reddit.com/r/neovim/comments/n80hdb/autocmd_execution_in_neovim_lua_config/
-- https://github.com/norcalli/nvim_utils/blob/master/lua/nvim_utils.lua#L554-L567
-- https://github.com/neovim/neovim/pull/14661

vim.api.nvim_exec(
    [[
" "autocmd CursorMoved,InsertLeave,BufEnter,BufWinEnter,TabEnter,BufWritePost * lua require'lsp_extensions'.inlay_hints{ prefix = ' » ', highlight = "Comment", enabled = {"TypeHint", "ChainingHint", "ParameterHint"} }

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
]],
    false
)

require("nvim_comment").setup {
    create_mappings = false,
}

require("compe").setup {
    enabled = true,
    autocomplete = true,
    debug = false,
    min_length = 1,
    preselect = "enable",
    throttle_time = 80,
    source_timeout = 200,
    resolve_timeout = 800,
    incomplete_delay = 400,
    max_abbr_width = 100,
    max_kind_width = 100,
    max_menu_width = 100,
    documentation = {
        border = { "", "", "", " ", "", "", "", " " }, -- the border option is the same as `|help nvim_open_win|`
        winhighlight = "NormalFloat:CompeDocumentation,FloatBorder:CompeDocumentationBorder",
        max_width = 120,
        min_width = 60,
        max_height = math.floor(vim.o.lines * 0.3),
        min_height = 1,
    },

    source = {
        path = true,
        buffer = true,
        calc = true,
        nvim_lsp = true,
        nvim_lua = true,
        vsnip = true,
        ultisnips = false,
        luasnip = false,
        tabnine = true,
    },
}

local t = function(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end

local check_back_space = function()
    local col = vim.fn.col "." - 1
    return col == 0 or vim.fn.getline("."):sub(col, col):match "%s" ~= nil
end

-- Use (s-)tab to:
--- move to prev/next item in completion menuone
--- jump to prev/next snippet's placeholder
_G.tab_complete = function()
    if vim.fn.pumvisible() == 1 then
        return t "<C-n>"
    elseif vim.fn["vsnip#available"](1) == 1 then
        return t "<Plug>(vsnip-expand-or-jump)"
    elseif check_back_space() then
        return t "<Tab>"
    else
        return vim.fn["compe#complete"]()
    end
end
_G.s_tab_complete = function()
    if vim.fn.pumvisible() == 1 then
        return t "<C-p>"
    elseif vim.fn["vsnip#jumpable"](-1) == 1 then
        return t "<Plug>(vsnip-jump-prev)"
    else
        -- If <S-Tab> is not working in your terminal, change it to <C-h>
        return t "<S-Tab>"
    end
end

vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", { expr = true })
vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", { expr = true })
vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", { expr = true })
vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", { expr = true })

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities.textDocument.completion.completionItem.resolveSupport = {
    properties = {
        "documentation",
        "detail",
        "additionalTextEdits",
    },
}

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(_client, bufnr)
    local function buf_set_keymap(...)
        vim.api.nvim_buf_set_keymap(bufnr, ...)
    end
    local function buf_set_option(...)
        vim.api.nvim_buf_set_option(bufnr, ...)
    end

    --Enable completion triggered by <c-x><c-o>
    buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")

    -- Mappings.
    local opts = { noremap = true, silent = true }

    -- See `:help vim.lsp.*` for documentation on any of the below functions
    buf_set_keymap("n", "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
    buf_set_keymap("n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
    buf_set_keymap("n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
    buf_set_keymap("n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
    buf_set_keymap("n", "<C-m>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
    buf_set_keymap("n", "<space>wa", "<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>", opts)
    buf_set_keymap("n", "<space>wr", "<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>", opts)
    buf_set_keymap("n", "<space>wl", "<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>", opts)
    buf_set_keymap("n", "<space>D", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
    buf_set_keymap("n", "<space>rn", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
    buf_set_keymap("n", "L", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
    buf_set_keymap("v", "<C-w>", "<cmd>lua vim.lsp.buf.range_code_action()<CR>", opts)
    buf_set_keymap("v", "L", "<cmd>lua vim.lsp.buf.range_code_action()<CR>", opts)
    buf_set_keymap("v", "<C-w>", "<cmd>lua vim.lsp.buf.range_code_action()<CR>", opts)
    buf_set_keymap("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
    buf_set_keymap("n", "<space>e", "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>", opts)
    buf_set_keymap("n", "[d", "<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>", opts)
    buf_set_keymap("n", "]d", "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>", opts)
    buf_set_keymap("n", "<space>q", "<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>", opts)
    buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)

    require("lsp_signature").on_attach()
end

local lspconfig = require "lspconfig"

local opts = {
    capabilities = capabilities,
    on_attach = on_attach,
}

lspconfig.rust_analyzer.setup(opts)
lspconfig.vimls.setup(opts)
lspconfig.clangd.setup(opts)
lspconfig.pyright.setup(opts)
lspconfig.yamlls.setup(opts)

lspconfig.sumneko_lua.setup {
    cmd = { "lua-language-server" },
    on_attach = on_attach,
    capabilities = capabilities,
    settings = {
        Lua = {
            diagnostics = {
                globals = {
                    "vim",
                    "use",
                    "use_rocks",
                    "nnoremap",
                    "nmap",
                    "inoremap",
                    "imap",
                    "map",
                    "vnoremap",
                    "vmap",
                    "tnoremap",
                    "tmap",
                    "cnoremap",
                    "cmap",
                    "snoremap",
                    "smap",
                    "onoremap",
                    "omap",
                    "xnoremap",
                    "xmap",
                },
            },
        },
    },
}

-----------------------------------
-----------------------------------
-----------------------------------
-----------------------------------
-----------------------------------

if vim.fn.has("win32") ~= 1 then
  require("nvim-treesitter.configs").setup {
      ensure_installed = { "c", "cpp", "json", "javascript", "go", "python", "rust", "query", "lua" },
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
  }
end

vim.cmd [[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerCompile
  augroup end
]]

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
--
-- " Install nvim-cmp
-- use 'hrsh7th/nvim-cmp'
-- " Install snippet engine (This example installs [hrsh7th/vim-vsnip](https://github.com/hrsh7th/vim-vsnip))
-- use 'hrsh7th/vim-vsnip'
-- " Install the buffer completion source
-- use 'hrsh7th/cmp-buffer'
--
-- use 'nvim-lua/completion-nvim'
-- use 'steelsojka/completion-buffers'
-- use 'aca/completion-tabnine', { 'do': './install.sh' }

-- local cmp = require'cmp'
-- cmp.setup({
--   snippet = {
--     expand = function(args)
--       vim.fn["vsnip#anonymous"](args.body)
--     end,
--   },
--   mapping = {
--       ['<C-d>'] = cmp.mapping.scroll_docs(-4),
--       ['<C-f>'] = cmp.mapping.scroll_docs(4),
--       ['<C-Space>'] = cmp.mapping.complete(),
--       ['<C-e>'] = cmp.mapping.close(),
--       ['<CR>'] = cmp.mapping.confirm({
--         behavior = cmp.ConfirmBehavior.Replace,
--         select = true,
--       })
--   },
--   sources = {
--   }
-- })
