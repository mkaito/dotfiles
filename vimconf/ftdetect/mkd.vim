" Markdown
function! SetMarkdown()
  setf mkd
  set ai comments=n:&gt;
  set formatoptions=l
  set lbr
  map j gj
  map k gk
  set smartindent
  set spell spelllang=en_gb
endfunction

autocmd BufNewFile,BufRead *.mkd call SetMarkdown()

