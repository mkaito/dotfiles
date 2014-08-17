:nnoremap <buffer> <C-D> :call ledger#transaction_date_set(line('.'), 'auxiliary')<CR>
:nnoremap <buffer> <C-T> :call ledger#transaction_state_toggle(line('.'), ' *')<CR>
:nnoremap <buffer> <C-C> :r!ledger xact 
