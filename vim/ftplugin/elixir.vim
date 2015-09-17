nmap <buffer> <leader>e :!mix test<cr>
nmap <buffer> <leader>t :VimuxRunCommand("mix test")<cr>
noremap <buffer> <leader>gt :CommandTFlush<cr>\|:CommandT ./test<cr>

noremap <buffer> <leader>gv :CommandTFlush<cr>\|:CommandT web/templates<cr>
noremap <buffer> <leader>gc :CommandTFlush<cr>\|:CommandT web/controllers<cr>
noremap <buffer> <leader>gm :CommandTFlush<cr>\|:CommandT web/models<cr>
noremap <buffer> <leader>gh :CommandTFlush<cr>\|:CommandT web/views<cr>

noremap <buffer> <leader>gl :CommandTFlush<cr>\|:CommandT lib<cr>

noremap <buffer> <leader>gk :CommandTFlush<cr>\|:CommandT web/static/css<cr>
noremap <buffer> <leader>gj :CommandTFlush<cr>\|:CommandT web/static/js<cr>
