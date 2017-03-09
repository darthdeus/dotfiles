setlocal omnifunc=necoghc#omnifunc
let g:ycm_semantic_triggers = {'haskell' : ['.']}
let g:necoghc_enable_detailed_browse = 1

" noremap <leader>t :HdevtoolsType<cr>
" noremap <leader>e :HdevtoolsClear<cr>

" noremap <leader>t :HdevtoolsType<cr>
" noremap <leader>r :HdevtoolsClear<cr>

noremap <leader>t :GhcModType<cr>
noremap <leader>T :call VimuxRunCommand(":te")<cr>
noremap <leader>e :GhcModTypeClear<cr>
nmap <silent> <leader>q :call VimuxOpenRunner()<cr>:call VimuxSendKeys("C-c :so Enter")<cr>

" set softtabstop=4
" set shiftwidth=4
" set tabstop=4

set foldlevelstart=200
let g:haskell_conceal       = 0
let g:haskell_quasi         = 1
let g:haskell_interpolation = 0
let g:haskell_regex         = 0
let g:haskell_jmacro        = 0
let g:haskell_shqq          = 0
let g:haskell_sql           = 0
let g:haskell_json          = 0
let g:haskell_xml           = 0

