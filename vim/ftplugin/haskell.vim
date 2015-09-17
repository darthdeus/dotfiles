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
