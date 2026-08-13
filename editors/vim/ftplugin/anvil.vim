if exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1

setlocal comments=s1:/*,mb:*,ex:*/,://
setlocal commentstring=//\ %s
setlocal formatoptions-=t
setlocal formatoptions+=cro
setlocal suffixesadd=.anvil

let b:undo_ftplugin = 'setlocal comments< commentstring< formatoptions< suffixesadd<'
