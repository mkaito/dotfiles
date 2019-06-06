hi clear
if exists("syntax on")
	syntax reset
endif
let g:color_name="erosion"
 
" Colors
hi Normal ctermfg=none ctermbg=none
hi Comment ctermfg=Red
hi Constant ctermfg=White
hi Identifier ctermfg=DarkRed
hi Statement ctermfg=DarkGreen
hi PreProc ctermfg=Brown
hi Type ctermfg=1
hi Special ctermfg=DarkCyan
hi Underlined ctermfg=Grey
hi Ignore ctermfg=Blue
hi Error ctermfg=Cyan
hi Todo ctermfg=1
hi NonText ctermfg=Black ctermbg=none
hi Directory ctermfg=Red
 
hi VertSplit ctermfg=Black
hi StatusLine ctermfg=Grey
hi StatusLineNC ctermfg=DarkBlue
 
hi Folded ctermbg=Black ctermfg=DarkGrey
 
hi Pmenu ctermfg=10 ctermbg=Black
hi PmenuSel ctermfg=Black ctermbg=Yellow
hi LineNr ctermfg=Black ctermbg=none
hi CursorLine ctermfg=none ctermbg=none cterm=none
hi CursorLineNr ctermfg=none ctermbg=Black
hi CursorColumn ctermfg=none ctermbg=Black
hi ErrorMsg ctermbg=none ctermfg=Brown
 
" Syntax checker colors
highlight SignColumn ctermbg=none
hi SyntasticErrorSign ctermfg=1 ctermbg=none
hi SyntasticWarningSign ctermfg=DarkCyan ctermbg=none
hi SyntasticStyleErrorSign ctermfg=1 ctermbg=none
hi SyntasticStyleWarningSign ctermfg=DarkCyan ctermbg=none
hi SyntasticErrorLine ctermfg=none ctermbg=none
hi SyntasticWarningLine ctermfg=none ctermbg=none
hi SyntasticStyleErrorLine ctermfg=none ctermbg=none
hi SyntasticStyleWarningLine ctermfg=none ctermbg=none
hi SpellBad ctermfg=Black ctermbg=DarkCyan
hi SpellCap ctermfg=Black ctermbg=1
