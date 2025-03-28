require("mapx").setup { global = "force" }

vim.api.nvim_set_keymap("n", "<leader>tr", "<cmd>lua ReloadConfig()<CR>", { noremap = true, silent = false })

-- vim.api.nvim_set_keymap("n", "<leader>pv", vim.cmd.Ex, {})
vim.keymap.set("n", "<leader>pv", vim.cmd.Ex)

-- vim.keymap.set({ "n", "x" }, "y", "<Plug>(YankyYank)")

vim.keymap.set({ "n", "x" }, "p", "<Plug>(YankyPutAfter)")
vim.keymap.set({ "n", "x" }, "P", "<Plug>(YankyPutBefore)")
vim.keymap.set({ "n", "x" }, "gp", "<Plug>(YankyGPutAfter)")
vim.keymap.set({ "n", "x" }, "gP", "<Plug>(YankyGPutBefore)")

vim.keymap.set("n", "<c-n>", "<Plug>(YankyCycleForward)")
vim.keymap.set("n", "<c-p>", "<Plug>(YankyCycleBackward)")

vim.keymap.set("n", "]p", "<Plug>(YankyPutIndentAfterLinewise)")
vim.keymap.set("n", "[p", "<Plug>(YankyPutIndentBeforeLinewise)")
vim.keymap.set("n", "]P", "<Plug>(YankyPutIndentAfterLinewise)")
vim.keymap.set("n", "[P", "<Plug>(YankyPutIndentBeforeLinewise)")

vim.keymap.set("n", ">p", "<Plug>(YankyPutIndentAfterShiftRight)")
vim.keymap.set("n", "<p", "<Plug>(YankyPutIndentAfterShiftLeft)")
vim.keymap.set("n", ">P", "<Plug>(YankyPutIndentBeforeShiftRight)")
vim.keymap.set("n", "<P", "<Plug>(YankyPutIndentBeforeShiftLeft)")

vim.keymap.set("n", "=p", "<Plug>(YankyPutAfterFilter)")
vim.keymap.set("n", "=P", "<Plug>(YankyPutBeforeFilter)")

nnoremap("<leader>sd", "<cmd>Telescope help_tags<CR>")
nnoremap("<leader>sa", "<cmd>Telescope commands<CR>")

-- map("n", "<leader>sf", "<cmd>Telescope live_grep<CR>")
-- map("n", "<leader>h", "<cmd>Telescope help_tags<CR>")
-- map("n", "<leader>h", "<cmd>Telescope help_tags<CR>")

-- Open files with <leader>f
-- nnoremap("<leader>f", ":Telescope find_files<CR>")
-- nnoremap("<leader>gt", ":Telescope tags<CR>")
-- nnoremap("<leader>ga", ":Telescope live_grep<cr>")
-- nnoremap("<leader>gd", ":Telescope live_grep <C-r><C-w><cr>")
-- nnoremap("<leader>b", ":Telescope buffers<cr>")
-- nnoremap("<leader>B", ":Telescope current_buffer_tags<cr>")

nnoremap("<leader>f", "<cmd>Files ./<CR>")
nnoremap("<leader>F", "<cmd>FZF %%<CR>")
nnoremap("<leader>gt", "<cmd>Tags<cr>")
nnoremap("<leader>ga", "<cmd>Rg<cr>")
nnoremap("<leader>gs", ":Rg <C-r><C-w><cr>")
nnoremap("<leader>gd", ":Rg <C-r><C-w><cr>")
nnoremap("<leader>b", "<cmd>Buffers<cr>")
nnoremap("<leader>B", "<cmd>BTags<cr>")

-- Mapping selecting mappings
nnoremap("<leader><tab>", "<plug>(fzf-maps-n)")
xnoremap("<leader><tab>", "<plug>(fzf-maps-x)")
onoremap("<leader><tab>", "<plug>(fzf-maps-o)")

nnoremap("gp", "<cmd>lua require('goto-preview').goto_preview_definition()<CR>")

inoremap("<M-S-l>", "<C-o><Plug>(sexp_capture_next_element)")
-- inoremap("<M-o>", "vec2()<C-o>i")
-- inoremap("<M-9>", "egui::vec2()<C-o>i")
inoremap("<M-o>", "vec2()<Esc>i")
inoremap("<M-9>", "egui::vec2()<Esc>i")
inoremap("<M-0>", "0.0,")
nnoremap("<M-0>", "i0.0,")
-- imap("<C-0>", "0.0,")

-- inoremap("<silent><expr>", "<C-Space> compe#complete()")
-- unused -- map("n", "<silent><expr>", "<CR>      compe#confirm('<CR>')")
-- inoremap("<silent><expr>", "<C-e>     compe#close('<C-e>')")
-- inoremap("<silent><expr>", "<C-f>     compe#scroll({ 'delta': +4 })")
-- inoremap("<silent><expr>", "<C-d>     compe#scroll({ 'delta': -4 })")

-- -- Expand
-- inoremap("<expr>", "<C-j>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-j>'")
-- snoremap("<expr>", "<C-j>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-j>'")
-- -- inoremap("<expr>", "<C-j>   vsnip#available(1) == 1  ? '<Plug>(vsnip-expand-or-jump)'         : '<C-j>'")
-- -- snoremap("<expr>", "<C-j>   vsnip#available(1) == 1  ? '<Plug>(vsnip-expand-or-jump)'         : '<C-j>'")
--
-- -- Expand or jump
-- inoremap("<expr>", "<C-l>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'")
-- snoremap("<expr>", "<C-l>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'")

nnoremap("<C-_><C-_>", "<cmd>CommentToggle<CR>")
vnoremap("<C-_><C-_>", ":'<,'>CommentToggle<CR>")

nnoremap("<CR>", "<cmd>nohlsearch<CR>")

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
nnoremap("-", "<cmd>Neoformat<cr>")
-- nnoremap("-", "<cmd>lua vim.lsp.buf.format()<CR>")

-- Bubble multiple lines
vnoremap("<C-Up>", "<C-w><C-k>")
vnoremap("<C-Down>", "<C-w><C-j>")
vnoremap("<C-Left>", "<C-w><C-h>")
vnoremap("<C-Right>", "<C-w><C-l>")

inoremap("<C-X><C-@>", "<C-A>")

if vim.fn.has "win32" == 1 then
  nnoremap("<leader>ge", "<cmd>vs C:/users/jakub/dotfiles/nvim/init.lua<CR>")
  -- nnoremap("<leader>e", ':TermExec cmd="make" direction=vertical size=80 go_back=0<cr>')
  -- nnoremap("<leader>r", ":Cargo run<cr>")
  -- nnoremap("<leader>e", ':ToggleTerm cmd="cargo run"<cr>')
  -- nnoremap("<leader>e", ":TermExec cmd='make' direction='vertical' size=80<cr>")
  -- nnoremap("<leader>e", ":TermExec cmd='make' direction='vertical' open=0 size=80<cr>")
  --

  nnoremap("<leader>r", "<Cmd>TermExec cmd=c direction=vertical size=60<CR>")
  -- nnoremap("<Leader>q", "<Cmd>ToggleTermToggleAll<CR>")
  -- nnoremap("<Leader>w", "<Cmd>ToggleTerm direction=vertical size=60<CR>")
  nnoremap("<leader>ma", ":cd C:/dev/flesh-monster<CR>")
  nnoremap("<leader>mb", ":cd C:/dev/BITGUN<CR>")
else
  vim.keymap.set("n", "<leader>ge", "<cmd>vs ~/.config/nvim/init.lua<CR>")
  vim.keymap.set("n", "<leader>r", "<cmd>VimuxRunCommand('c')<CR>")
  -- nnoremap("<Leader>q", "<cmd>call VimuxRunCommand('c')<CR>")
  -- nnoremap("<Leader>w", "<Cmd>call VimuxRunCommand('c')<CR>")
end

vim.api.nvim_set_keymap('n', '<leader>q', ':silent !touch game/setup.rock<CR>', { noremap = true, silent = true })

-- Expand %% to directory path of current buffer
cnoremap("%%", "<C-R>=expand('%:h').'/'<CR>")

nnoremap("<F6>", "<cmd>Rerebuild<cr>")
nnoremap("<F7>", "<cmd>TSPlaygroundToggle<cr>")
nnoremap("<F8>", "<cmd>ToggleTerm direction=vertical size=60<cr>")
inoremap("<F8>", "<cmd>ToggleTerm direction=vertical size=60<cr>")
tnoremap("<F8>", "<C-\\><C-n>:ToggleTerm<cr>")
-- tnoremap("<Esc>", "<C-\\><C-n>:ToggleTerm<cr>")
nnoremap("<F5>", "<cmd>VimuxRunCommand('make')<cr>")
nnoremap("<F4>", "<cmd>TSContextToggle<cr>")
-- map("n", <leader>r :call VimuxRunCommand("make ". expand("%h"))<cr>
-- nnoremap("<leader>t", ":call VimuxRunCommand('make test')<cr>")
-- nnoremap("<leader>c", ":call VimuxRunCommand('make clean')<cr>")

-- Inserts the path of the currently edited file in command mode
cnoremap("<C-P>", "<C-R>=expand('%:p:h') . '/' <CR>")
-- imap <c-x><c-k> <plug>(fzf-complete-word)
inoremap("<c-x><c-f>", "<plug>(fzf-complete-path)")
inoremap("<c-x><c-j>", "<plug>(fzf-complete-file-ag)")
-- imap <c-x><c-l> <plug>(fzf-complete-line)
xnoremap("ga", "<Plug>(EasyAlign)")
nnoremap("ga", "<Plug>(EasyAlign)")
nnoremap("<leader>ws", "<cmd>%s/ *$//g<cr><c-o><cr>")
nnoremap("Q", "<NOP>")
nnoremap("<leader><leader>", "<c-^>")

vim.keymap.set("n", "<leader>ps", function()
  local builtin = require "telescope.builtin"
  builtin.grep_string { search = vim.fn.input "Grep > " }
end)

nnoremap(
  "<leader>lt",
  ":!ctags --extras=+f --exclude=build --exclude=public --exclude=target --exclude=node_modules --exclude=.git --exclude='*.css' --exclude='*.json' --exclude='*.js' --exclude='*.svg' --exclude=venv --exclude='*.html' -R *<CR>"
)
nnoremap("<C-\\>", ":tnext<CR>")

-- local dap = require "dap"
-- local dap_w = require "dap.ui.widgets"
--
-- local dapui = require "dapui"
--
-- dap.listeners.after.event_initialized["dapui_config"] = function()
--   dapui.open()
-- end
-- dap.listeners.before.event_terminated["dapui_config"] = function()
--   dapui.close()
-- end
-- dap.listeners.before.event_exited["dapui_config"] = function()
--   dapui.close()
-- end
--
-- vim.keymap.set("n", "<F5>", function()
--   dap.continue()
-- end)
-- vim.keymap.set("n", "<F10>", function()
--   dap.step_over()
-- end)
-- vim.keymap.set("n", "<F11>", function()
--   dap.step_into()
-- end)
-- vim.keymap.set("n", "<F12>", function()
--   dap.step_out()
-- end)
-- -- vim.keymap.set("n", "<Leader>b", function()
-- --     dap.toggle_breakpoint()
-- -- end)
-- vim.keymap.set("n", "<Leader>B", function()
--   dap.toggle_breakpoint()
-- end)
-- vim.keymap.set("n", "<Leader>lp", function()
--   dap.set_breakpoint(nil, nil, vim.fn.input "Log point message: ")
-- end)
-- vim.keymap.set("n", "<Leader>dr", function()
--   dap.repl.open()
-- end)
-- vim.keymap.set("n", "<Leader>dl", function()
--   dap.run_last()
-- end)
-- vim.keymap.set({ "n", "v" }, "<Leader>dh", function()
--   dap_w.hover()
-- end)
-- vim.keymap.set({ "n", "v" }, "<Leader>dp", function()
--   dap_w.preview()
-- end)
-- vim.keymap.set("n", "<Leader>df", function()
--   dap_w.centered_float(dap_w.frames)
-- end)
-- vim.keymap.set("n", "<Leader>ds", function()
--   dap_w.centered_float(dap_w.scopes)
-- end)
