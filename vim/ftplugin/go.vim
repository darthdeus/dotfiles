" autocmd BufWritePre *.go :Fmt
set noexpandtab
set softtabstop=4
set shiftwidth=4
set tabstop=4
set nolist

nnoremap <leader>e :call VimuxRunCommand("go run " . expand("%"))<cr>
nnoremap <leader>k :exec "!pkill " . expand("%:t:r")<cr><cr>
