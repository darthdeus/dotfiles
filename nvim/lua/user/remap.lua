require("mapx").setup({ global = "force" })

vim.api.nvim_set_keymap("n", "<leader>tr", "<cmd>lua ReloadConfig()<CR>", { noremap = true, silent = false })

-- vim.api.nvim_set_keymap("n", "<leader>pv", vim.cmd.Ex, {})
vim.keymap.set("n", "<leader>pv", vim.cmd.Ex)

nnoremap("<leader>sd", ":Telescope help_tags<CR>")
nnoremap("<leader>sa", ":Telescope commands<CR>")

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

nnoremap("<leader>f", ":Files ./<CR>")
nnoremap("<leader>F", ":FZF %%<CR>")
nnoremap("<leader>gt", ":Tags<cr>")
nnoremap("<leader>ga", ":Rg<cr>")
nnoremap("<leader>gs", ":Rg <C-r><C-w><cr>")
nnoremap("<leader>gd", ":Rg <C-r><C-w><cr>")
nnoremap("<leader>b", ":Buffers<cr>")
nnoremap("<leader>B", ":BTags<cr>")

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

vim.api.nvim_exec([[
" Expand
imap <expr> <C-j>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-j>'
smap <expr> <C-j>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-j>'

" Expand or jump
imap <expr> <C-l>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'
smap <expr> <C-l>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'

" Jump forward or backward
imap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
smap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
imap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'
smap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'

" Select or cut text to use as $TM_SELECTED_TEXT in the next snippet.
" See https://github.com/hrsh7th/vim-vsnip/pull/50
]], true)


-- -- Expand
-- inoremap("<expr>", "<C-j>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-j>'")
-- snoremap("<expr>", "<C-j>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-j>'")
-- -- inoremap("<expr>", "<C-j>   vsnip#available(1) == 1  ? '<Plug>(vsnip-expand-or-jump)'         : '<C-j>'")
-- -- snoremap("<expr>", "<C-j>   vsnip#available(1) == 1  ? '<Plug>(vsnip-expand-or-jump)'         : '<C-j>'")
--
-- -- Expand or jump
-- inoremap("<expr>", "<C-l>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'")
-- snoremap("<expr>", "<C-l>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'")

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

vnoremap("-", ":lua vim.lsp.buf.format()<cr>")
nnoremap("-", ":lua vim.lsp.buf.format()<cr>")
-- vnoremap("-", ":Neoformat<cr>")
-- nnoremap("-", ":Neoformat<cr>")

-- Bubble multiple lines
vnoremap("<C-Up>", "<C-w><C-k>")
vnoremap("<C-Down>", "<C-w><C-j>")
vnoremap("<C-Left>", "<C-w><C-h>")
vnoremap("<C-Right>", "<C-w><C-l>")

inoremap("<C-X><C-@>", "<C-A>")

if vim.fn.has("win32") == 1 then
  nnoremap("<leader>ge", ":vs C:/users/jakub/dotfiles/nvim/init.lua<CR>")
  -- nnoremap("<leader>e", ':TermExec cmd="make" direction=vertical size=80 go_back=0<cr>')
  -- nnoremap("<leader>r", ":Cargo run<cr>")
  -- nnoremap("<leader>e", ':ToggleTerm cmd="cargo run"<cr>')
  -- nnoremap("<leader>e", ":TermExec cmd='make' direction='vertical' size=80<cr>")
  -- nnoremap("<leader>e", ":TermExec cmd='make' direction='vertical' open=0 size=80<cr>")
  --

  nnoremap("<Leader>r", "<Cmd>TermExec cmd=c direction=vertical size=60<CR>")
  nnoremap("<Leader>q", "<Cmd>ToggleTermToggleAll<CR>")
  nnoremap("<Leader>w", "<Cmd>ToggleTerm direction=vertical size=60<CR>")
  nnoremap("<leader>ma", ":cd C:/dev/flesh-monster<CR>")
  nnoremap("<leader>mb", ":cd C:/dev/BITGUN<CR>")
else
  nnoremap("<leader>ge", ":vs ~/.config/nvim/init.lua<CR>")
  nnoremap("<Leader>r", ":VimuxRunCommand('c <C-R>=expand('%:t')<CR>')<CR>")
  nnoremap("<Leader>q", "<Cmd>call VimuxRunCommand('c')<CR>")
  nnoremap("<Leader>w", "<Cmd>call VimuxRunCommand('c')<CR>")
end


-- Expand %% to directory path of current buffer
cnoremap("%%", "<C-R>=expand('%:h').'/'<CR>")

nnoremap("<F8>", ":ToggleTerm direction=vertical size=60<cr>")
inoremap("<F8>", ":ToggleTerm direction=vertical size=60<cr>")
tnoremap("<F8>", "<C-\\><C-n>:ToggleTerm<cr>")
tnoremap("<Esc>", "<C-\\><C-n>:ToggleTerm<cr>")
nnoremap("<F5>", ":call VimuxRunCommand('make')<cr>")
nnoremap("<F4>", ":call VimuxRunCommand('make')<cr>")
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
nnoremap("<leader>ws", ":%s/ *$//g<cr><c-o><cr>")
nnoremap("Q", "<NOP>")
nnoremap("<leader><leader>", "<c-^>")

nnoremap(
  "<leader>lt",
  ":!ctags --extras=+f --exclude=build --exclude=public --exclude=target --exclude=node_modules --exclude=.git -R *<CR>"
)
nnoremap("<C-\\>", ":tnext<CR>")