" nmap <buffer> <leader>e :!mix test<cr>
" nmap <buffer> <leader>t :VimuxRunCommand("mix test")<cr>
noremap <buffer> <leader>gt :CtrlP ./test<cr>

noremap <buffer> <leader>gv :CtrlP web/templates<cr>
noremap <buffer> <leader>gc :CtrlP web/controllers<cr>
noremap <buffer> <leader>gm :CtrlP web/models<cr>
noremap <buffer> <leader>gh :CtrlP web/views<cr>

noremap <buffer> <leader>gl :CtrlP lib<cr>

noremap <buffer> <leader>gk :CtrlP web/static/css<cr>
noremap <buffer> <leader>gj :CtrlP web/static/js<cr>
