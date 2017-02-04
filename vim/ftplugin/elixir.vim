" nmap <buffer> <leader>e :!mix test<cr>
" nmap <buffer> <leader>t :VimuxRunCommand("mix test")<cr>
noremap <buffer> <leader>gt :FZF ./test<cr>

noremap <buffer> <leader>gv :FZF web/templates<cr>
noremap <buffer> <leader>gc :FZF web/controllers<cr>
noremap <buffer> <leader>gm :FZF web/models<cr>
noremap <buffer> <leader>gh :FZF web/views<cr>

noremap <buffer> <leader>gl :FZF lib<cr>

noremap <buffer> <leader>gk :FZF web/static/css<cr>
noremap <buffer> <leader>gj :FZF web/static/js<cr>
