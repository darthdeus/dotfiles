require("nvim-treesitter.configs").setup({
  ensure_installed = {
    "c", "json", "javascript", "python",
    "rust", "lua", "wgsl", "fennel",
    "commonlisp", "jsonc", "bash",
    "markdown", "markdown_inline",
    "regex", "html", "clojure"
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
