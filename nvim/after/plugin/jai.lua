local null_ls = require("null-ls")
local null_ls_helpers = require("null-ls.helpers")

null_ls.setup()

local jai_compile = {
  method = null_ls.methods.DIAGNOSTICS_ON_SAVE,
  filetypes = { "jai" },
  generator = null_ls.generator({
    command = "make",
    args = { "compile" },
    format = "line",
    from_stderr = true,
    on_output = null_ls_helpers.diagnostics.from_patterns({
      {
        pattern = [[(.+):(%d+),(%d+): Error: (.+)]],
        groups = { "file", "row", "col", "message" }
      }
    })
  })
}

null_ls.register(jai_compile)