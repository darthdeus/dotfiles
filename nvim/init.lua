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
-- - https://github.com/famiu/bufdelete.nvim

require("packer").startup(function()
    use "wbthomason/packer.nvim"

    -- use 'rafcamlet/nvim-luapad'
    use "ray-x/lsp_signature.nvim"
    use "liuchengxu/vista.vim"

    use "akinsho/toggleterm.nvim"

    use {
        "rmagatti/goto-preview",
        config = function()
            require("goto-preview").setup {}
        end,
    }
    -- TODO: maybe without icons?
    -- use {
    --     "kyazdani42/nvim-tree.lua",
    --     requires = "kyazdani42/nvim-web-devicons",
    -- }

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

-- Make searches case-sensitive only if they contain upper-case characters
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.incsearch = true
vim.o.hlsearch = true

vim.o.hidden = true

vim.o.scrolloff = 9
vim.o.number = false
vim.o.relativenumber = false
vim.o.showmode = false

-- Whitespace stuff
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
-- TODO: += ?
vim.o.wildignore =
    "obj,*.o,*.obj,.git,*.rbc,*.class,.svn,vendor/gems/*,node_modules,tmp,project/target,target,tags,CMakeFiles,bower_components,dist,_darcs,vcr,app/assets/images,*.dSYM,*.pyc,_build,rel,*.a,priv/static,*.aux,*.dvi,*.xmpi,*.out,*.lot,*.lof,*.blg,*.bbl,*.toc,__pycache__,build,logs,tags"

-- TODO: default is menu,preview?
vim.o.completeopt = "menuone,preview,noinsert,noselect"

vim.g.mapleader = ","
vim.g.maplocalleader = ","

vim.o.backupcopy = "yes"
vim.o.list = true
vim.o.listchars = "tab:--,trail:."
vim.o.pastetoggle = "<F3>"
vim.o.undofile = true

vim.g.base16colorspace = 256
-- TODO: true?
vim.o.termguicolors = false
vim.cmd "color base16-default"

vim.g.VimuxOrientation = "h"

local function map(mode, lhs, rhs, opts)
    local options = { noremap = true }
    if opts then
        options = vim.tbl_extend("force", options, opts)
    end
    vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

-- TODO: figure out how to remap these?
-- nunmap ]f
-- nunmap [f


-- nn <leader>v :Vista!!<cr>
vim.g.vista_default_executive = 'nvim_lsp'

map("n", "gp", "<cmd>lua require('goto-preview').goto_preview_definition()<CR>")

map("i", "<silent><expr>", "<C-Space> compe#complete()")
-- map("n", "<silent><expr>", "<CR>      compe#confirm('<CR>')")
map("i", "<silent><expr>", "<C-e>     compe#close('<C-e>')")
map("i", "<silent><expr>", "<C-f>     compe#scroll({ 'delta': +4 })")
map("i", "<silent><expr>", "<C-d>     compe#scroll({ 'delta': -4 })")

-- Expand
map("i", "<expr>", "<C-j>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-j>'")
map("s", "<expr>", "<C-j>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-j>'")

-- Expand or jump
map("i", "<expr>", "<C-l>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'")
map("s", "<expr>", "<C-l>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'")

map("n", "<C-_><C-_>", ":CommentToggle<CR>")
map("v", "<C-_><C-_>", ":CommentToggle<CR>")

map("n", "<CR>", ":nohlsearch<CR>/<BS>")

-- Buffer resizing with arrow keys
map("n", "<Up>", "<C-w>5-")
map("n", "<Down>", "<C-w>5+")
map("n", "<Left>", "<C-w>5<")
map("n", "<Right>", "<C-w>5>")

map("n", "<C-a>", "^")
map("n", "<C-e>", "$")

map("i", "<C-a>", "<Home>")
map("i", "<C-e>", "<End>")

-- For easier navigation between windows
map("n", "<C-j>", "<C-w><C-j>")
map("n", "<C-k>", "<C-w><C-k>")
map("n", "<C-h>", "<C-w><C-h>")
map("t", "<C-h>", "<C-\\><C-n><C-w><C-h>")
map("n", "<C-l>", "<C-w><C-l>")

map("v", "-", ":Neoformat<cr>")

-- Bubble multiple lines
map("v", "<C-Up>", "<C-w><C-k>")
map("v", "<C-Down>", "<C-w><C-j>")
map("v", "<C-Left>", "<C-w><C-h>")
map("v", "<C-Right>", "<C-w><C-l>")

map("i", "<C-X><C-@>", "<C-A>")

map("n", "-", ":Neoformat<cr>")

map("n", "<leader>ge", ":vs ~/.config/nvim/init.lua<CR>")

-- Expand %% to directory path of current buffer
map("c", "%%", "<C-R>=expand('%:h').'/'<CR>")

map("n", "<Leader>e", ":call VimuxRunCommand('c')<cr>")
-- map("n", "<Leader>e", ":TermExec cmd='make' direction='vertical' size=80 go_back=0<cr>")
-- map("n", "<Leader>e", ":TermExec cmd='make' direction='vertical' size=80<cr>")
-- map("n", "<Leader>e", ":TermExec cmd='make' direction='vertical' open=0 size=80<cr>")

map("n", "<F8>", ":ToggleTerm<cr>")
map("i", "<F8>", ":ToggleTerm<cr>")
map("t", "<F8>", "<C-\\><C-n>:ToggleTerm<cr>")
map("t", "<Esc>", "<C-\\><C-n>:ToggleTerm<cr>")
map("n", "<F5>", ":call VimuxRunCommand('make')<cr>")
map("n", "<F4>", ":call VimuxRunCommand('make')<cr>")
-- map("n", <leader>r :call VimuxRunCommand("make ". expand("%h"))<cr>
map("n", "<leader>r", ":call VimuxRunCommand('make test')<cr>")
map("n", "<leader>c", ":call VimuxRunCommand('make clean')<cr>")

-- Inserts the path of the currently edited file in command mode
map("c", "<C-P>", "<C-R>=expand('%:p:h') . '/' <CR>")

-- Open files with <leader>f
map("n", "<leader>f", ":Files ./<CR>")
-- Open files, limited to the directory of the current files, with <leader>gf
map("n", "<leader>F", ":FZF %%<CR>")

map("n", "<leader>gt", ":Tags<cr>")
map("n", "<leader>ga", ":Rg<cr>")
map("n", "<leader>gd", ":Rg <C-r><C-w><cr>")

map("n", "<leader>b", ":Buffers<cr>")
map("n", "<leader>B", ":BTags<cr>")

-- Mapping selecting mappings
map("n", "<leader><tab>", "<plug>(fzf-maps-n)")
map("x", "<leader><tab>", "<plug>(fzf-maps-x)")
map("o", "<leader><tab>", "<plug>(fzf-maps-o)")

-- Insert mode completion
-- imap <c-x><c-k> <plug>(fzf-complete-word)
map("i", "<c-x><c-f>", "<plug>(fzf-complete-path)")
map("i", "<c-x><c-j>", "<plug>(fzf-complete-file-ag)")
-- imap <c-x><c-l> <plug>(fzf-complete-line)

map("x", "ga", "<Plug>(EasyAlign)")
map("n", "ga", "<Plug>(EasyAlign)")

-- remove unnecessary whitespaces
map("n", "<leader>ws", ":%s/ *$//g<cr><c-o><cr>")

-- Disable accidental ex mode
map("n", "Q", "<NOP>")

-- Switching between active files in a buffer.
map("n", "<leader><leader>", "<c-^>")

map(
    "n",
    "<leader>lt",
    ":!ctags --extras=+f --exclude=build --exclude=public --exclude=target --exclude=node_modules --exclude=.git -R *<CR>"
)
map("n", "<C-\\>", ":tnext<CR>")

map("n", "<silent>", "<leader>y :<C-u>silent '<,'>w !pbcopy<CR>")

map("n", "<F9>", ":Neogit<CR>")

-- TODO: do this with nvim_utils?
-- https://github.com/norcalli/nvim_utils
-- https://www.reddit.com/r/neovim/comments/n80hdb/autocmd_execution_in_neovim_lua_config/
-- https://github.com/norcalli/nvim_utils/blob/master/lua/nvim_utils.lua#L554-L567
-- https://github.com/neovim/neovim/pull/14661

vim.api.nvim_exec(
    [[
autocmd CursorMoved,InsertLeave,BufEnter,BufWinEnter,TabEnter,BufWritePost * lua require'lsp_extensions'.inlay_hints{ prefix = ' » ', highlight = "Comment", enabled = {"TypeHint", "ChainingHint", "ParameterHint"} }

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

-- if has("nvim")
--   au TermOpen * tnoremap <buffer> <Esc> <c-\><c-n>
--   au FileType fzf tunmap <buffer> <Esc>
-- endif


-- vim.o.showcmd = true
-- vim.o.cursorline = true
-- vim.o.cursorcolumn = false

-- vim.o.history = 5000

-- require'lightspeed'.setup { }
-- require('gitsigns').setup()
--
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
local on_attach = function(client, bufnr)
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

    require "lsp_signature".on_attach()
end

local lspconfig = require "lspconfig"

local opts = {
    capabilities = capabilities,
    on_attach = on_attach,
}

lspconfig.rust_analyzer.setup(opts)
lspconfig.vimls.setup(opts)

lspconfig.sumneko_lua.setup {
    cmd = { "lua-language-server" },
    on_attach = on_attach,
    capabilities = capabilities,
    settings = {
      Lua = {
        diagnostics = {
          globals = {"vim", "use", "use_rocks"}
        }
      }
  }
}

-- require("rust-tools").setup {
--     server = opts,
-- }
--
-- require("rust-tools.inlay_hints").set_inlay_hints()

-- require("lsp_extensions").inlay_hints { enabled = { "TypeHint", "ChainingHint", "ParameterHint" } }
-- require("lsp_extensions").inlay_hints()

-- -- Get the counts from your curreent workspace:
-- local ws_errors = require("lsp_extensions.workspace.diagnostic").get_count(0, "Error")
-- local ws_hints = require("lsp_extensions.workspace.diagnostic").get_count(0, "Hint")
--
-- -- Set the qflist for the current workspace
-- --  For more information, see `:help vim.lsp.diagnostic.set_loc_list()`, since this has some of the same configuration.
-- require("lsp_extensions.workspace.diagnostic").set_qf_list()

-- local servers = { 'vim-language-server', 'rust_analyzer' }
-- for _, lsp in ipairs(servers) do
--   nvim_lsp[lsp].setup {
--     on_attach = on_attach,
--     capabilities = capabilities,
--     flags = {
--       debounce_text_changes = 150,
--     }
--   }
-- end

-- require'lspconfig'.rust_analyzer.setup{on_attach=require'completion'.on_attach}

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

-----------------------------------
-- -- TODO COMPLETION
-- local nvim_lsp = require('lspconfig')
--

-- -- TODO: is this duplicate of the inlay hints that already work?
--
--
-- local function setup_servers()
--   require'lspinstall'.setup()
--   local servers = require'lspinstall'.installed_servers()
--   -- for _, server in pairs(servers) do
--   --   nvim_lsp[server].setup{
--   --     on_attach = on_attach,
--   --     flags = {
--   --       debounce_text_changes = 150,
--   --     }
--   --   }
--   -- end
--

-- end

-- setup_servers()
--
-- -- Automatically reload after `:LspInstall <server>` so we don't have to restart neovim
-- require'lspinstall'.post_install_hook = function ()
--   setup_servers() -- reload installed servers
--   vim.cmd("bufdo e") -- this triggers the FileType autocmd that starts the server
-- end
--
--
-- -- Enable diagnostics
-- vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
--   vim.lsp.diagnostic.on_publish_diagnostics, {
--     virtual_text = true,
--     signs = true,
--     update_in_insert = false,
--     severity_sort = true,
--   }
-- )
-----------------------------------
-----------------------------------
-----------------------------------
-----------------------------------
-----------------------------------

-- require'nvim-treesitter.configs'.setup {
--   ensure_installed = { "rust" }, -- one of "all", "maintained" (parsers with maintainers), or a list of languages
--   ignore_install = { "javascript" }, -- List of parsers to ignore installing
--   highlight = {
--     enable = true,              -- false will disable the whole extension
--     disable = { },  -- list of language that will be disabled
--     -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
--     -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
--     -- Using this option may slow down your editor, and you may see some duplicate highlights.
--     -- Instead of true it can also be a list of languages
--     additional_vim_regex_highlighting = false,
--   },
-- }

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

vim.o.exrc = true
vim.o.secure = true
