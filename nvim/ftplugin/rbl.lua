vim.lsp.start({
  cmd = { "rebel-lsp" },
  root_dir = vim.fn.getcwd(), -- Use PWD as project root dir.
})
