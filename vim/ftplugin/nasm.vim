set nowrap
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set list listchars=tab:\ \ ,trail:·

if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal indentexpr=GetNasmIndent(v:lnum)
setlocal indentkeys+=<:>

if exists("*GetNasmIndent")
  finish
endif

function! s:GetPrevNonCommentLineNum(line_num)
  let SKIP_LINES = '^\s*;.*'
  let nline = a:line_num
  while nline > 0
    let nline = prevnonblank(nline - 1)
    if getline(nline) !~? SKIP_LINES
      break
    endif
  endwhile
  return nline
endfunction

function! GetNasmIndent(lnum)
  if a:lnum == 0
    return 0
  endif
  let this_line = getline(a:lnum)
  let prev_code_num = s:GetPrevNonCommentLineNum(a:lnum)
  let prev_code = getline(prev_code_num)
  let indnt = indent(prev_code_num)
  if this_line =~ '^\s;'
    return indent(a:lnum)
  endif
  if this_line =~ '^\s*.*:'
    return 0
  endif
  if this_line =~ '\v.*d(b|w|d)'
    return 0
  endif
  if prev_code =~ '.*:'
    return indnt + &shiftwidth
  endif
  return indnt
endfunction
