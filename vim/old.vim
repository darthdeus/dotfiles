" This must be first, because it changes other options as a side effect.
set nocompatible

set shell=/bin/sh

" let g:python_host_prog = $HOME . '/.venvs/neovim2/bin/python'
" let g:python3_host_prog = $HOME . '/.venvs/neovim3/bin/python'

if has('python3')
  silent! python3 1
endif

if has('macunix')
  let g:python3_host_prog = '/Users/darth/projects/homebrew/bin/python3'
else
  let g:python3_host_prog = '/usr/bin/python'
endif

Plug 'bounceme/dim-jump'
Plug 'junegunn/gv.vim'

Plug 'tpope/vim-endwise'
Plug 'tpope/vim-markdown'

Plug 'mgedmin/python-imports.vim'

Plug 'godlygeek/tabular'


" Plug 'Konfekt/FastFold'
"
" nmap zuz <Plug>(FastFoldUpdate)
" let g:fastfold_savehook = 1
" let g:fastfold_fold_command_suffixes =  ['x','X','a','A','o','O','c','C']
" let g:fastfold_fold_movement_commands = [']z', '[z', 'zj', 'zk']

" Plug 'vim-airline/vim-airline' | Plug 'vim-airline/vim-airline-themes'
"
Plug 'drmikehenry/vim-headerguard'

" -------- COQ -----------------
" " main one
Plug 'ms-jpq/coq_nvim', {'branch': 'coq'}
" " 9000+ Snippets
" Plug 'ms-jpq/coq.artifacts', {'branch': 'artifacts'}
" ---------- DEOPLETE ---------------
let g:coq_settings = {
      \ 'auto_start': v:false,
      \ 'clients.tabnine.enabled': v:true
      \ }

let g:airline_powerline_fonts = 1
" let g:airline_theme='simple'
let g:airline_theme='base16_default'

if has('mac')
  let g:gist_clip_command = 'pbcopy'
elseif has('unix')
  let g:gist_clip_command = 'xclip -selection clipboard'
endif
let g:gist_detect_filetype = 1
let g:gist_open_browser_after_post = 1
let g:gist_post_private = 1

" local coq = require('coq')

" fat fingers
" noremap <F1> <Esc>


" Plug 'Shougo/unite.vim'
" if has('nvim')
"   Plug 'Shougo/denite.nvim', { 'do': ':UpdateRemotePlugins' }
" else
"   Plug 'Shougo/denite.nvim'
"   Plug 'roxma/nvim-yarp'
"   Plug 'roxma/vim-hug-neovim-rpc'
" endif

" Plug 'fatih/vim-go'

" Plug 'RRethy/vim-hexokinase'
" Plug 'lilydjwg/colorizer'

Plug 'rhysd/vim-clang-format'
Plug 'kana/vim-operator-user'

Plug 'mattn/gist-vim'
Plug 'mattn/webapi-vim'

" Plug 'rking/ag.vim'
Plug 'octol/vim-cpp-enhanced-highlight'

" Plug 'lervag/vimtex'
" let g:vimtex_view_method = 'zathura'
" let g:tex_flavor = 'latex'

" Plug 'jdonaldson/vaxe'

" Plug 'Shougo/echodoc.vim'
" let g:echodoc#enable_at_startup = 1
" let g:echodoc#type = 'virtual'
" let g:echodoc#type = 'floating'

Plug 'prettier/vim-prettier'
let g:prettier#autoformat_require_pragma = 0
let g:prettier#autoformat_config_present = 1
let g:prettier#autoformat_config_files = ['.prettierrc']
Plug 'jparise/vim-graphql'
Plug 'pangloss/vim-javascript'
let g:javascript_plugin_flow = 1
Plug 'maxmellon/vim-jsx-pretty'
Plug 'styled-components/vim-styled-components', { 'branch': 'main' }

let g:ycm_python_binary_path = 'python2'

" Plug 'leafgarland/typescript-vim'
" Plug 'Quramy/tsuquyomi'
" Plug 'peitalin/vim-jsx-typescript'

" Plug 'rdnetto/YCM-Generator', { 'branch': 'stable' }
Plug 'tell-k/vim-autopep8'

" let g:lsp_settings_filetype_javascript = ['typescript-language-server', 'eslint-language-server']
" let g:lsp_settings_filetype_javascript = ['eslint-language-server']
" let g:lsp_settings_filetype_javascript = ['rome']

" " settings for pyls
" if executable('pyls')
"     " pip install python-language-server
"     au User lsp_setup call lsp#register_server({
"         \ 'name': 'pyls',
"         \ 'cmd': {server_info->['pyls']},
"         \ 'allowlist': ['python'],
"         \ })
" endif

" Plug 'w0rp/ale'
" Plug 'Valloric/YouCompleteMe'
"
" Plug 'neoclide/coc.nvim', {'branch': 'release'}
" -------------------------------
" --- SWITCH TO NVIM-LSP --------
" Plug 'prabirshrestha/vim-lsp'
" Plug 'mattn/vim-lsp-settings'
" setlocal omnifunc=lsp#complete
" Plug 'lighttiger2505/deoplete-vim-lsp'
" --------------------------------
"

" function! s:on_lsp_buffer_enabled() abort
"     " use omnifunc if you are fine with it.
"     " setlocal omnifunc=lsp#complete
"     if exists('+tagfunc') | setlocal tagfunc=lsp#tagfunc | endif
"     " some mappings to use, tweak as you wish.
"     nmap <buffer> gd <plug>(lsp-definition)
"     nmap <buffer> gr <plug>(lsp-references)
"     nmap <buffer> gi <plug>(lsp-implementation)
"     nmap <buffer> gt <plug>(lsp-type-definition)
"     nmap <buffer> gq <plug>(lsp-code-action)
"     nmap <buffer> <leader>rn <plug>(lsp-rename)
"     nmap <buffer> [g <Plug>(lsp-previous-diagnostic)
"     nmap <buffer> ]g <Plug>(lsp-next-diagnostic)
"     nmap <buffer> K <Plug>(lsp-hover)
"     nmap <buffer> ga <Plug>(lsp-code-action)
" endfunction
"

" whether to enable diagnostics for vim-lsp (we may want to use ALE for other
" plugins for that.
let g:lsp_diagnostics_enabled = 1

" let g:lsp_log_verbose = 1
" let g:lsp_log_file = expand('~/vim-lsp.log')

" for asyncomplete.vim log
" let g:asyncomplete_log_file = expand('~/asyncomplete.log')

" " show diagnostic signs
let g:lsp_diagnostics_signs_enabled = 1
" let g:lsp_diagnostics_signs_error = {'text': '✗'}
" let g:lsp_diagnostics_signs_warning = {'text': '!'}
" let g:lsp_highlights_enabled = 0

" " Do not use virtual text, they are far too obtrusive.
let g:lsp_virtual_text_enabled = 0
" " echo a diagnostic message at cursor position
let g:lsp_diagnostics_echo_cursor = 1
let g:lsp_diagnostics_echo_delay = 0
" disabled for now since it breaks in neovim
" TODO: only in neovim?
" let g:lsp_diagnostics_float_cursor = 1
" let g:lsp_diagnostics_float_delay = 0
" " whether to enable highlight a symbol and its references
let g:lsp_highlight_references_enabled = 1
let g:lsp_diagnostics_highlights_enabled = 1
let g:lsp_diagnostics_highlights_delay = 0
let g:lsp_preview_max_width = 80

" -------------------------------

" Plug 'ervandew/supertab'
" Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'


" Plug 'numirias/semshi', { 'do': ':UpdateRemotePlugins' }
"
" Plug 'davidhalter/jedi-vim'

" Plug 'terryma/vim-multiple-cursors'

let g:jedi#show_call_signatures = "1"
let g:jedi#show_call_signatures_delay = "0"
" let g:jedi#completions_enabled = 1

let g:ycm_global_ycm_extra_conf = '~/.ycm_extra_conf.py'
let g:ycm_extra_conf_globlist = ['*']
let g:ycm_show_diagnostics_ui = 1

" let g:ycm_rust_src_path = '~/projects/rust/rust-src/src'

if !exists('g:ycm_semantic_triggers')
  let g:ycm_semantic_triggers = {}
endif
" let g:ycm_semantic_triggers.tex = g:vimtex#re#youcompleteme

" set completeopt-=preview

let g:ale_c_build_dir = "./build"
let g:ale_cpp_gcc_options = '-std=c++11 -Iversions/1.12.2/include'

" let g:ale_completion_enabled = 1
let g:ale_fixers = {
      \ 'python': [
      \ 'trim_whitespace',
      \ 'add_blank_lines_for_python_control_statements',
      \ 'isort',
      \ ]
      \ }

let g:ale_python_mypy_options = '--ignore-missing-imports'
" TODO: add pylint when 2.1 is released, check https://pypi.org/project/pylint/
let g:ale_linters = {
      \ 'python': [ 'flake8', 'mypy' ],
      \ 'sh': ['shell', 'shellcheck'],
      \ 'go': ['gofmt', 'govet', 'revive', 'staticcheck', 'golangci-lint'],
      \ 'zsh': ['shell']
      \ }
" \ 'autopep8'
"
let g:ale_go_revive_options = '-config=.revive.toml'

augroup lsp_install
    au!
    " call s:on_lsp_buffer_enabled only for languages that has the server registered.
    autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END

let g:clang_format#code_style = 'google'

aug cpp_types
  autocmd!
  autocmd FileType h,cc,c,cpp nnoremap <buffer><C-e> :<C-u>ClangFormat<CR>
  autocmd FileType h,cc,c,cpp nnoremap <buffer><leader>ha :HeaderguardAdd<CR>
  autocmd FileType h,cc,c,cpp vnoremap <buffer><C-e> :ClangFormat<CR>
aug END

" noremap <leader>a :TestSuite<cr>
" " Run this file
" noremap <leader>t :TestFile<cr>
" " Run only the example under the cursor
" noremap <leader>T :TestNearest<cr>

" map <Leader>tt :call RunCurrentSpecFile()<CR>
" map <Leader>T :call RunNearestSpec()<CR>
" map <Leader>l :call RunLastSpec()<CR>
" map <Leader>a :call RunAllSpecs()<CR>

" TODO: this or coc-python format?
" noremap <leader>gf :GFiles ./<CR>

" TODO: find a better hotkey
" noremap <leader>gd :GFiles?<cr>


" Rails specific keystrokes
" noremap <leader>gr :topleft :split config/routes.rb<CR>
noremap <leader>gg :topleft 50 :split Gemfile<CR>
" noremap <leader>gv :FZF app/views<cr>
" noremap <leader>gc :FZF app/controllers<cr>
" noremap <leader>gm :FZF app/models<cr>
" noremap <leader>gs :FZF app/services<cr>
" noremap <leader>gh :FZF app/helpers<cr>
" noremap <leader>gl :FZF lib<cr>
" noremap <leader>gk :FZF app/assets/stylesheets<cr>
" noremap <leader>gj :FZF app/assets/javascripts<cr>
" noremap <leader>gr :FZF spec/<CR>

" nnoremap <leader>jd :YcmCompleter GoTo<CR>
" nnoremap <leader>jf :YcmCompleter GoToDefinition<CR>
" nnoremap <leader>je :YcmCompleter GoToDeclaration<CR>
" nnoremap <leader>gt :YcmCompleter GetType<CR>
" nnoremap <leader>gq :YcmCompleter GetDoc<CR>
nnoremap <leader>gn :ALENext<CR>
nnoremap <leader>gp :ALEPrevious<CR>
" nnoremap <leader>gr :Semshi rename<CR>


" " Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
" let g:UltiSnipsExpandTrigger='<c-j>'
" let g:UltiSnipsJumpForwardTrigger='<c-j>'
" let g:UltiSnipsJumpBackwardTrigger='<c-k>'
"
" " If you want :UltiSnipsEdit to split your window.
" let g:UltiSnipsEditSplit='vertical'
"
" " let g:UltiSnipsExpandTrigger       = '<tab>'
" " let g:UltiSnipsJumpForwardTrigger  = '<tab>'
" " let g:UltiSnipsJumpBackwardTrigger = '<s-tab>'
" " let g:UltiSnipsSnippetDirectories  = ['snips']
"
" function! g:UltiSnips_Complete()
"   call UltiSnips#ExpandSnippet()
"   if g:ulti_expand_res == 0
"     if pumvisible()
"       return "\<C-n>"
"     else
"       call UltiSnips#JumpForwards()
"       if g:ulti_jump_forwards_res == 0
"         return "\<TAB>"
"       endif
"     endif
"   endif
"   return ''
" endfunction
"
" aug ultisnips_insert_enter_expand
"   autocmd!
"   autocmd InsertEnter * exec "inoremap <silent> " . g:UltiSnipsExpandTrigger . " <C-R>=g:UltiSnips_Complete()<cr>"
" aug END

" Ultisnips YouCompleteMe integration https://stackoverflow.com/questions/14896327/ultisnips-and-youcompleteme
" function! g:UltiSnips_Complete()
"   call UltiSnips#ExpandSnippet()
"   if g:ulti_expand_res == 0
"     if pumvisible()
"       return "\<C-n>"
"     else
"       call UltiSnips#JumpForwards()
"       if g:ulti_jump_forwards_res == 0
"         return "\<TAB>"
"       endif
"     endif
"   endif
"   return ""
" endfunction
"
" function! g:UltiSnips_Reverse()
"   call UltiSnips#JumpBackwards()
"   if g:ulti_jump_backwards_res == 0
"     return "\<C-P>"
"   endif
"
"   return ""
" endfunction
"
"
" if !exists("g:UltiSnipsJumpForwardTrigger")
"   let g:UltiSnipsJumpForwardTrigger = "<tab>"
" endif
"
" if !exists("g:UltiSnipsJumpBackwardTrigger")
"   let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
" endif
"
" au InsertEnter * exec "inoremap <silent> " . g:UltiSnipsExpandTrigger     . " <C-R>=g:UltiSnips_Complete()<cr>"
" au InsertEnter * exec "inoremap <silent> " .     g:UltiSnipsJumpBackwardTrigger . " <C-R>=g:UltiSnips_Reverse()<cr>"


" set foldlevelstart=200
" let g:haskell_conceal       = 0
" let g:haskell_quasi         = 1
" let g:haskell_interpolation = 0
" let g:haskell_regex         = 0
" let g:haskell_jmacro        = 0
" let g:haskell_shqq          = 0
" let g:haskell_sql           = 0
" let g:haskell_json          = 0
" let g:haskell_xml           = 0

" " SuperTab like snippets behavior.
" " Note: It must be "imap" and "smap".  It uses <Plug> mappings.
" "imap <expr><TAB>
" " \ pumvisible() ? "\<C-n>" :
" " \ neosnippet#expandable_or_jumpable() ?
" " \    "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
" smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
" \ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
"
" " For conceal markers.
" if has('conceal')
"   set conceallevel=2 concealcursor=niv
" endif


" nnoremap <F7> :set keymap=czech-qwerty<CR>
" nnoremap <F8> :set keymap=<CR>
" inoremap <F7> <C-o>:set keymap=czech-qwerty<CR>
" inoremap <F8> <C-o>:set keymap=<CR>

nnoremap <F12> :e ++enc=iso-8859-2<CR>

if has('user_commands')
  command! -bang -nargs=? -complete=file E e<bang> <args>
  command! -bang -nargs=? -complete=file W w<bang> <args>
  command! -bang -nargs=? -complete=file Wq wq<bang> <args>
  command! -bang -nargs=? -complete=file WQ wq<bang> <args>
  command! -bang Wa wa<bang>
  command! -bang WA wa<bang>
  command! -bang Q q<bang>
  command! -bang QA qa<bang>
  command! -bang Qa qa<bang>
endif

"alphsubs ---------------------- {{{
        execute "digraphs ks " . 0x2096
        execute "digraphs as " . 0x2090
        execute "digraphs es " . 0x2091
        execute "digraphs hs " . 0x2095
        execute "digraphs is " . 0x1D62
        execute "digraphs ks " . 0x2096
        execute "digraphs ls " . 0x2097
        execute "digraphs ms " . 0x2098
        execute "digraphs ns " . 0x2099
        execute "digraphs os " . 0x2092
        execute "digraphs ps " . 0x209A
        execute "digraphs rs " . 0x1D63
        execute "digraphs ss " . 0x209B
        execute "digraphs ts " . 0x209C
        execute "digraphs us " . 0x1D64
        execute "digraphs vs " . 0x1D65
        execute "digraphs xs " . 0x2093
"}}

" color base16-material-palenight
" color Tomorrow-Night-Eighties

" TODO - what is the default behavior?
" Remap the tab key to do autocompletion or indentation depending on the
" context (from http://www.vim.org/tips/tip.php?tip_id=102)
" function! InsertTabWrapper()
"     let col = col('.') - 1
"     if !col || getline('.')[col - 1] !~ '\k'
"         return "\<tab>"
"     else
"         return "\<c-p>"
"     endif
" endfunction
"
" inoremap <tab> <c-r>=InsertTabWrapper()<cr>
" inoremap <s-tab> <c-n>

cmap w!! w !sudo tee > /dev/null %
command! W :w

" TODO: add back yapf when https://github.com/sbdchd/neoformat/issues/126 is fixed
let g:neoformat_enabled_python = ['autopep8', 'docformatter', 'black']
" let g:neoformat_enabled_python = ['autopep8', 'docformatter']
let g:neoformat_run_all_formatters = 1

" TODO - does this help things?
" set smartindent
"
let g:limelight_paragraph_span = 2
" autocmd BufRead,BufNewFile * Limelight 0.5

" https://github.com/junegunn/fzf.vim/issues/544
" if has('nvim')
"   au TermOpen * tnoremap <Esc> <c-\><c-n>
"   au TermOpen * tnoremap <C-c> <c-\><c-n>
"   au TermOpen * tnoremap <C-g> <c-\><c-n>
"   au FileType fzf tunmap <Esc>
" endif

" TODO m1 ... bug? disabled for now
" au FileType fzf tunmap <Esc>
" au FileType fzf tunmap <C-c>
" au FileType fzf tunmap <C-g>
" TODO m1 ... bug?

" nnoremap - :Switch<cr>
" nnoremap - :RustFmt<cr>

" nnoremap ; :
" nnoremap : ;

aug various_file_types
  autocmd!
  autocmd BufNewFile,BufRead *.conf set filetype=conf

  " Rakefile, Vagrantfile and Gemfile are Ruby
  autocmd BufRead,BufNewFile {Capfile,Gemfile,Rakefile,Vagrantfile,config.ru} set ft=ruby
  autocmd BufRead,BufNewFile *.asm set ft=nasm

  " add json syntax highlighting
  " autocmd BufNewFile,BufRead *.tsx,*.jsx set filetype=typescript.tsx

  autocmd BufNewFile,BufRead *.slime set filetype=slim

  autocmd BufNewFile,BufRead *.md set wrap
  autocmd BufNewFile,BufRead *.markdown inoremap <buffer>_ \_
  autocmd BufNewFile,BufRead *.markdown inoremap <buffer>\\ \\\\\\\\
  autocmd BufNewFile,BufRead *.markdown inoremap <buffer><C-b> \boldsymbol
  " autocmd BufNewFile,BufRead *.markdown inoremap <buffer><C-m> \mathcal
aug END

" TODO: maybe this isn't needed with neovim anymore?
if has('gui_running')
  " Automatically resize splits when resizing MacVim window
  aug vim_gui_resized
    autocmd!
    autocmd VimResized * wincmd =
  aug END

  set visualbell
  set lines=40
  set columns=120

  " Remove scrollbars
  set guioptions-=L
  set guioptions-=r
  set guioptions-=T
endif

" let NERDTreeRespectWildIgnore=1
