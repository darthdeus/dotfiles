" User Makefile for node projects
function s:setupMake()
  nmap <leader>r :!make<CR>
endfunction

au BufNewFile,BufRead *.js,*.coffee call s:setupMake()
