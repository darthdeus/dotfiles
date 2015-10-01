set nowrap
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set list listchars=tab:\ \ ,trail:·

noremap <buffer> <Leader>gt :CommandTFlush<cr>\|:CommandT ./spec<cr>
" Run this file
noremap <buffer> <Leader>t :call RunTestFile()<cr>
" " Run only the example under the cursor
noremap <buffer> <Leader>T :call RunNearestTest()<cr>
" " Run all test files
noremap <buffer> <Leader>a :!bundle exec rspec spec<cr>

