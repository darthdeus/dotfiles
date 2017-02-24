" nmap <buffer> <leader>e :!mix test<cr>
noremap <buffer> <leader>q :VimuxRunCommand("mix dialyzer")<cr>
noremap <buffer> <leader>gt :FZF ./test<cr>

noremap <buffer> <leader>gr :topleft :split web/router.ex<CR>
noremap <buffer> <leader>gv :FZF web/templates<cr>
noremap <buffer> <leader>gc :FZF web/controllers<cr>
noremap <buffer> <leader>gm :FZF web/models<cr>
noremap <buffer> <leader>gh :FZF web/views<cr>

noremap <buffer> <leader>gl :FZF lib<cr>

noremap <buffer> <leader>gk :FZF web/static/css<cr>
noremap <buffer> <leader>gj :FZF web/static/js<cr>
