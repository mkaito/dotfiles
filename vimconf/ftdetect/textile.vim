" Textile
autocmd BufNewFile,BufRead *.textile setf textile|
        setlocal formatoptions=l
        setlocal lbr
        map j gj
        map k gk
        setlocal smartindent
        setlocal spell spelllang=en_gb
