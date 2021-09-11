" let g:python_host_prog = $HOME . '/.venvs/neovim2/bin/python'
" let g:python3_host_prog = $HOME . '/.venvs/neovim3/bin/python'

Plug 'drmikehenry/vim-headerguard'

Plug 'prettier/vim-prettier'
let g:prettier#autoformat_require_pragma = 0
let g:prettier#autoformat_config_present = 1
let g:prettier#autoformat_config_files = ['.prettierrc']
Plug 'jparise/vim-graphql'
Plug 'pangloss/vim-javascript'
let g:javascript_plugin_flow = 1
Plug 'maxmellon/vim-jsx-pretty'
Plug 'styled-components/vim-styled-components', { 'branch': 'main' }

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
