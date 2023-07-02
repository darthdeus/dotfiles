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
-- vim.o.fileencoding = "utf-8"
-- vim.o.fileencodings = "utcs-bom,utf8,latin2"

-- Use modeline overrides
vim.o.modeline = true
vim.o.modelines = 10
vim.o.laststatus = 0
vim.o.cmdheight = 0

vim.o.winwidth = 75

vim.o.wildmode = "list:longest,list:full"
vim.o.wildignore =
    "obj,*.o,*.obj,.git,*.rbc,*.class,.svn,vendor/gems/*,node_modules,tmp,project/target,ra_target,.ra_target,target,tags,CMakeFiles,bower_components,dist,_darcs,vcr,app/assets/images,*.dSYM,*.pyc,_build,rel,*.a,priv/static,*.aux,*.dvi,*.xmpi,*.out,*.lot,*.lof,*.blg,*.bbl,*.toc,__pycache__,build,logs,tags,*.rpyc"

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
vim.o.termguicolors = true
-- vim.o.term = "xterm-256color"

-- vim.api.nvim_exec(
--   [[
-- if exists('+termguicolors')
--   let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
--   let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"
--   set termguicolors
-- endif
-- ]] ,
--   false
-- )

-- vim.cmd "set fillchars+=vert:|"
vim.cmd "set fillchars+=vert:│"
-- vim.cmd "colorscheme codesmell_dark"
-- vim.cmd "color base16-default-dark"
-- vim.cmd "color b16"

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

-- nn <leader>v :Vista!!<cr>
vim.g.vista_default_executive = "nvim_lsp"

-- disable netrw at the very start of your init.lua (strongly advised)
-- vim.g.loaded_netrw = 1
-- vim.g.loaded_netrwPlugin = 1

vim.g.NERDTreeRespectWildIgnore = 1
