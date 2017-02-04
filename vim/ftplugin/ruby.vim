set nowrap
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set list listchars=tab:\ \ ,trail:·

noremap <buffer> <Leader>gt :FZF spec<cr>
" " Run all test files
noremap <buffer> <Leader>a :!bundle exec rspec spec<cr>

