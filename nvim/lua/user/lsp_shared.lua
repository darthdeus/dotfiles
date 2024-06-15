local M = {}

function M.setup_keymaps() -- See `:help vim.lsp.*` for documentation on any of the below functions
  -- Mappings.
  local map_opts = { noremap = true, silent = true }

  vim.api.nvim_set_keymap("n", "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", map_opts)
  vim.api.nvim_set_keymap("n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", map_opts)
  vim.api.nvim_set_keymap("n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", map_opts)
  vim.api.nvim_set_keymap("n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", map_opts)
  vim.api.nvim_set_keymap("n", "<C-m>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", map_opts)
  vim.api.nvim_set_keymap("n", "<space>wa", "<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>", map_opts)
  vim.api.nvim_set_keymap("n", "<space>wr", "<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>", map_opts)
  vim.api.nvim_set_keymap(
    "n",
    "<space>wl",
    "<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>",
    map_opts
  )
  vim.api.nvim_set_keymap("n", "<space>D", "<cmd>lua vim.lsp.buf.type_definition()<CR>", map_opts)
  vim.api.nvim_set_keymap("n", "<space>rn", "<cmd>lua vim.lsp.buf.rename()<CR>", map_opts)
  vim.api.nvim_set_keymap("n", "<f2>", "<cmd>lua vim.lsp.buf.rename()<CR>", map_opts)
  vim.api.nvim_set_keymap("n", "L", "<cmd>lua vim.lsp.buf.code_action()<CR>", map_opts)
  vim.api.nvim_set_keymap("v", "<C-w>", "<cmd>lua vim.lsp.buf.range_code_action()<CR>", map_opts)
  vim.api.nvim_set_keymap("v", "L", "<cmd>lua vim.lsp.buf.range_code_action()<CR>", map_opts)
  vim.api.nvim_set_keymap("v", "<C-w>", "<cmd>lua vim.lsp.buf.range_code_action()<CR>", map_opts)
  vim.api.nvim_set_keymap("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", map_opts)
  vim.api.nvim_set_keymap("n", "<space>e", "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>", map_opts)
  vim.api.nvim_set_keymap("n", "[e", "<cmd>lua vim.diagnostic.goto_prev()<CR>", map_opts)
  vim.api.nvim_set_keymap("n", "]e", "<cmd>lua vim.diagnostic.goto_next()<CR>", map_opts)
  vim.api.nvim_set_keymap("n", "[d", "<cmd>lua vim.diagnostic.goto_prev()<CR>", map_opts)
  vim.api.nvim_set_keymap("n", "]d", "<cmd>lua vim.diagnostic.goto_next()<CR>", map_opts)
  vim.api.nvim_set_keymap("n", "<space>q", "<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>", map_opts)
  vim.api.nvim_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", map_opts)
  -- vim.api.nvim_set_keymap("n", "-", "<cmd>lua vim.lsp.buf.format()<cr>", map_opts)
  -- vim.api.nvim_set_keymap("v", "-", "<cmd>lua vim.lsp.buf.format()<cr>", map_opts)
end

function M.setup_lsp_servers()
  local on_attach = function(_, bufnr)
    --Enable completion triggered by <c-x><c-o>
    vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

    require("lsp_signature").on_attach()
  end
  --
  -- -- Setup lspconfig.
  local capabilities = require("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities())

  local opts = {
    capabilities = capabilities,
    on_attach = on_attach,
  }

  local ra_opts = {
    -- on_attach = function(_, bufnr)
    -- 	vim.keymap.set("n", "<leader>ca", rust_tools.hover_actions.hover_actions, { buffer = bufnr })
    -- end,
    capabilities = capabilities,
    on_attach = on_attach,

    settings = {
      ["rust-analyzer"] = {
        checkOnSave = {
          command = "clippy",
          -- overrideCommand = { "killall", "nvim" }
        },
        cargo = {
          extraEnv = { CARGO_TARGET_DIR = ".ra_target" },
          -- features = { "dev", "comfy/quick-exit", "comfy/tracy" },
          -- rust-analyzer.cargo.features
        },
        diagnostics = {
          -- TODO: check include_dir
          disabled = { "inactive-code", "unresolved-proc-macro" },
        },
        rustfmt = {},
      },
    },

    handlers = {
      -- ["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
      --   -- Disable signs in gutter
      --   signs = false,
      --   -- Ensure virtual text (inline diagnostics) is enabled
      --   virtual_text = true,
      --   -- Enable underlining
      --   underline = true,
      --   -- You can also disable the update in the statusline if you wish with
      --   -- update_in_insert = false,
      -- }),
    },
  }

  require("rust-tools").setup { server = ra_opts }

  local lspconfig = require "lspconfig"

  -- lspconfig.jails = {
  --   default_config = {
  --     cmd = { "/home/darth/.dotfiles/bin/jails" },
  --     filetypes = { "jai" },
  --     root_dir = function(_)
  --         return  vim.fn.getcwd()
  --       -- return util.find_git_ancestor(fname) or vim.loop.os_homedir()
  --     end,
  --     settings = {},
  --   },
  -- }
  -- lspconfig.jails.setup(opts)

  local configs = require "lspconfig.configs"
  local util = require "lspconfig.util"

  configs.jails = {
    default_config = {
      cmd = { "/home/darth/projects/jails/bin/jails" },
      filetypes = { "jai" },
      root_dir = util.path.dirname,
    },
  }

  require("lspconfig").jails.setup {
    root_dir = function(fname)
      return vim.fn.getcwd()
    end,
  }

  -- lspconfig.jai.setup(opts)
  lspconfig.bashls.setup(opts)
  lspconfig.taplo.setup(opts)
  lspconfig.clangd.setup(opts)
  lspconfig.html.setup(opts)
  lspconfig.jsonls.setup(opts)
  lspconfig.pyright.setup(opts)
  -- lspconfig.csharp_ls.setup(opts)

  lspconfig.lua_ls.setup {
    capabilities = capabilities,
    on_attach = on_attach,
    settings = {
      Lua = {
        format = {
          enable = false,
        },
        diagnostics = {
          groupFileStatus = {
            global = "None",
          },
          globals = {
            "vim",
            "use",
            "nnoremap",
            "xnoremap",
            "vnoremap",
            "onoremap",
            "inoremap",
            "cnoremap",
            "tnoremap",
          },
        },
      },
    },
  }

  require("rust-tools").inlay_hints.enable()
end

local function get_cmp()
  local ok_cmp, cmp = pcall(require, "cmp")
  return ok_cmp and cmp or {}
end

local function get_luasnip()
  local ok_luasnip, luasnip = pcall(require, "luasnip")
  return ok_luasnip and luasnip or {}
end

function M.luasnip_supertab(select_opts)
  local cmp = get_cmp()
  local luasnip = get_luasnip()

  return cmp.mapping(function(fallback)
    local col = vim.fn.col "." - 1

    if cmp.visible() then
      cmp.select_next_item(select_opts)
    elseif luasnip.expand_or_jumpable() then
      luasnip.expand_or_jump()
    elseif col == 0 or vim.fn.getline("."):sub(col, col):match "%s" then
      fallback()
    else
      cmp.complete()
    end
  end, {
    "i",
    "s",
  })
end

function M.luasnip_shift_supertab(select_opts)
  local cmp = get_cmp()
  local luasnip = get_luasnip()

  return cmp.mapping(function(fallback)
    if cmp.visible() then
      cmp.select_prev_item(select_opts)
    elseif luasnip.jumpable(-1) then
      luasnip.jump(-1)
    else
      fallback()
    end
  end, {
    "i",
    "s",
  })
end

-- local luasnip = get_luasnip()

-- Setup nvim-cmp.
function M.setup_cmp()
  local cmp = require "cmp"

  -- local has_words_before = function()
  --   local cursor = vim.api.nvim_win_get_cursor(0)
  --   return (vim.api.nvim_buf_get_lines(0, cursor[1] - 1, cursor[1], true)[1] or ""):sub(cursor[2], cursor[2]):match "%s"
  -- end

  local shared_sources = {
    { name = "nvim_lsp" },
    { name = "nvim_lsp_signature_help" },
    { name = "nvim_lua" },
    { name = "luasnip" },
    { name = "path" },
    { name = "crates" },
  }

  local sources = cmp.config.sources(shared_sources, {
    { name = "buffer" },
  })

  if _G.copilot_enabled then
    sources = cmp.config.sources(shared_sources, {
      { name = "copilot" },
    }, {
      { name = "buffer" },
    })
  end

  --		sources = cmp.config.sources({
  --			{ name = "nvim_lsp" },
  --
  --			{ name = "ctags" },
  --			{ name = "luasnip" },
  --		}, {
  --			{ name = "buffer" },
  --		}),
  --
  local lspkind = require "lspkind"

  cmp.setup {
    completion = {
      keyword_length = 1,
      debounce = 0,
    },

    formatting = {
      format = lspkind.cmp_format {
        mode = "symbol", -- show only symbol annotations
        maxwidth = 50, -- prevent the popup from showing more than provided characters (e.g 50 will not show more than 50 characters)
        ellipsis_char = "...", -- when popup menu exceed maxwidth, the truncated part would show ellipsis_char instead (must define maxwidth first)
        symbol_map = { Copilot = "" },

        -- The function below will be called before any actual modifications from lspkind
        -- so that you can provide more controls on popup customization. (See [#30](https://github.com/onsails/lspkind-nvim/pull/30))
        before = function(_, vim_item)
          return vim_item
        end,
      },
    },

    window = {
      completion = {
        -- winhighlight = "Normal:Pmenu,FloatBorder:Pmenu,CursorLine:PmenuSel,Search:None",
        -- border = "rounded",
        -- Set the number of items to display in the completion menu
        items = 10,
        max_width = 80,
      },
    },

    experimental = {
      ghost_text = true,
    },

    snippet = {
      expand = function(args)
        require("luasnip").lsp_expand(args.body)
      end,
    },

    mapping = cmp.mapping.preset.insert {
      ["<C-b>"] = cmp.mapping.scroll_docs(-4),
      ["<C-f>"] = cmp.mapping.scroll_docs(4),
      ["<C-Space>"] = cmp.mapping.complete(),
      ["<C-e>"] = cmp.mapping.abort(),
      -- Accept currently selected item. Set `select` to `false` to only
      -- confirm explicitly selected items.

      ["<CR>"] = cmp.mapping.confirm { select = true },
      -- ["<CR>"] = cmp.mapping.confirm({ select = false }),
      -- ["<CR>"] = cmp.mapping(function(fallback)
      -- 	if cmp.visible() then
      -- 		cmp.complete()
      -- 	else
      -- 		fallback()
      -- 	end
      -- end),

      [","] = cmp.mapping(function(fallback)
        fallback()
      end),

      ["{"] = cmp.mapping(function(fallback)
        fallback()
      end),

      ["<C-j>"] = cmp.mapping.confirm { select = true },
      ["<C-l>"] = cmp.mapping.confirm { select = true },

      ["<C-p>"] = cmp.mapping(function()
        if cmp.visible() then
          cmp.select_prev_item(cmp_select_opts)
        else
          cmp.complete()
        end
      end),
      ["<C-n>"] = cmp.mapping(function()
        if cmp.visible() then
          cmp.select_next_item(cmp_select_opts)
        else
          cmp.complete()
        end
      end),

      ["<Tab>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_next_item()
          -- You could replace the expand_or_jumpable() calls with expand_or_locally_jumpable()
          -- they way you will only jump inside the snippet region
          -- elseif luasnip.expand_or_jumpable() then
          -- 	luasnip.expand_or_jump()
          -- elseif has_words_before() then
          -- 	cmp.complete()
        else
          fallback()
        end
      end, {
        "i",
        "s",
      }),

      ["<S-Tab>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_prev_item()
          -- elseif luasnip.jumpable(-1) then
          -- 	luasnip.jump(-1)
        else
          fallback()
        end
      end, {
        "i",
        "s",
      }),
    },

    sources = sources,
  }

  -- Set configuration for specific filetype.
  cmp.setup.filetype("gitcommit", {
    sources = cmp.config.sources({
      { name = "cmp_git" }, -- You can specify the `cmp_git` source if you were installed it.
    }, {
      { name = "buffer" },
    }),
  })

  -- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline("/", {
    mapping = cmp.mapping.preset.cmdline(),
    sources = {
      { name = "buffer" },
    },
  })

  -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline(":", {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
      { name = "path" },
    }, {
      { name = "cmdline" },
    }),
  })
end

return M
