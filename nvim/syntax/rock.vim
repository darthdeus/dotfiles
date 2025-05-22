" Vim syntax file
" Language:     Rock

if exists("b:current_syntax")
  finish
endif

" ----------------------
" Comments (FIRST!)
" ----------------------
syn region rockCommentLine start=+//+ end=+$+ keepend contains=@Spell
syn region rockCommentBlock start="/\*" end="\*/" contains=rockCommentBlock,@Spell

" ----------------------
" Attributes
" ----------------------
syn match rockAttribute "@[a-zA-Z_]\w*" contained
syn match rockPreAttr     "@[a-zA-Z_]\w*" contains=rockAttribute

" ----------------------
" Keywords
" ----------------------
syn keyword rockKeyword fn let return global struct extern if else for in while match loop break continue
syn keyword rockKeyword true false null
syn keyword rockKeyword type any

" ----------------------
" Builtin Types (like Rust types)
" ----------------------
syn keyword rockType int float bool string any void
syn match rockType /\<[A-Z][a-zA-Z0-9_]*\>/

" ----------------------
" Function names (basic)
" ----------------------
syn match rockFuncDef /\<fn\>\s\+\zs[a-zA-Z_][a-zA-Z0-9_]*/ contained
syn match rockFuncCall /\<[a-zA-Z_][a-zA-Z0-9_]*\s*(/me=e-1

" ----------------------
" Strings and Numbers
" ----------------------
syn region rockString start=+"+ skip=+\\"+ end=+"+ contains=rockEscape
syn match rockEscape /\\./ contained

syn match rockNumber /\<\d\+\(\.\d\+\)\=\([eE][-+]\=\d\+\)\=\>/
syn match rockBoolean /\<\(true\|false\)\>/

" ----------------------
" Operators and Delimiters
" ----------------------
syn match rockOperator /[-+*%&|=<>!]=\|\.\.\|::\|\.\|[-+*%=<>!&|]/
syn match rockDelimiter /[()\[\]{},;]/

" ----------------------
" Highlight Groups
" ----------------------
hi def link rockKeyword       Keyword
hi def link rockAttribute     PreProc
hi def link rockPreAttr       PreProc
hi def link rockType          Type
hi def link rockFuncDef       Function
hi def link rockFuncCall      Function
hi def link rockCommentLine   Comment
hi def link rockCommentBlock  Comment
hi def link rockString        String
hi def link rockEscape        SpecialChar
hi def link rockNumber        Number
hi def link rockBoolean       Boolean
hi def link rockOperator      Operator
hi def link rockDelimiter     Delimiter

let b:current_syntax = "rock"

