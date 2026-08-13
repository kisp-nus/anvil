" Vim syntax file
" Language: AnvilHDL

if exists('b:current_syntax')
  finish
endif

syntax case match

syntax keyword anvilTodo TODO FIXME XXX NOTE contained
syntax region anvilBlockComment start=/\/\*/ end=/\*\// contains=anvilBlockComment,anvilTodo,@Spell
syntax match anvilLineComment /\/\/.*$/ contains=anvilTodo,@Spell
syntax region anvilString start=/"/ skip=/\\"/ end=/"/ keepend

syntax match anvilNumber /\<\d\+\>/
syntax match anvilNumber /\<\d\+'\%(d\d\+\|b[01]\+\|h[0-9A-Fa-f]\+\)\>/

syntax match anvilIdentifier /\<[A-Za-z_][A-Za-z0-9_]*\>/
syntax keyword anvilType logic int void dyn
syntax keyword anvilModifier left right extern foreign shared eternal
syntax keyword anvilKeyword assigned by call chan const cycle dfinish dprint
syntax keyword anvilKeyword else enum flat func generate generate_seq if import
syntax keyword anvilKeyword in let loop match probe proc put ready recv recurse
syntax keyword anvilKeyword recursive reg send set shl shr spawn struct sync try
syntax keyword anvilKeyword type with

syntax match anvilOperator /:=\|::\|--\|>>\|<=\|>=\|=>\|==\|!=\|&&\|||\|[=+*^&|@~.<>:#-]/

highlight default link anvilTodo Todo
highlight default link anvilBlockComment Comment
highlight default link anvilLineComment Comment
highlight default link anvilString String
highlight default link anvilNumber Number
highlight default link anvilIdentifier Identifier
highlight default link anvilType Type
highlight default link anvilModifier StorageClass
highlight default link anvilKeyword Keyword
highlight default link anvilOperator Operator

let b:current_syntax = 'anvil'
