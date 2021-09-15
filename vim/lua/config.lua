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
vim.o.listchars="tab:--,trail:."

vim.g.base16colorspace = 256
-- TODO: true?
vim.o.termguicolors = false
vim.cmd("color base16-default")

vim.o.pastetoggle = "<F3>"

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
    buf_set_keymap("n", "<C-w>", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
    buf_set_keymap("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
    buf_set_keymap("n", "<space>e", "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>", opts)
    buf_set_keymap("n", "[d", "<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>", opts)
    buf_set_keymap("n", "]d", "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>", opts)
    buf_set_keymap("n", "<space>q", "<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>", opts)
    buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
end

local lspconfig = require "lspconfig"

local opts = {
    capabilities = capabilities,
    on_attach = on_attach,
}

lspconfig.rust_analyzer.setup(opts)
lspconfig.vimls.setup(opts)

lspconfig.sumneko_lua.setup {
    cmd = { "sumneko-lua-language-server" },
    on_attach = on_attach,
    capabilities = capabilities,
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
            init_selection = "gnn",
            node_incremental = "grn",
            scope_incremental = "rc",
            node_decremental = "grm",
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
