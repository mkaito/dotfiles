:nnoremap <buffer> <C-D> :call ledger#transaction_date_set(line('.'), 'auxiliary')<CR>
:nnoremap <buffer> <C-T> :call ledger#transaction_state_toggle(line('.'), ' *')<CR>
:nnoremap <buffer> <C-C> :r!ledger xact
:nnoremap <buffer> qq vipgq
" gq using `ledger print` will indent by 4 spaces, but I like my file indented
" with a single 4-width tab instead. Fix that when leaving insert mode, or when
" writing to disk.
:autocmd BufWritePre *.ledger :%s/^    /\t/e
:autocmd InsertLeave *.ledger :%s/^    /\t/e
