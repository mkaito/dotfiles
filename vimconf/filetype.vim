" markdown filetype file
if exists("did\_load\_filetypes")
  finish
endif
runtime! ftdetect/*.vim
