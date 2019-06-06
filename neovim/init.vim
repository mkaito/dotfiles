call plug#begin('~/.local/share/nvim/plugged')

Plug '/usr/share/vim/vimfiles'
Plug 'LnL7/vim-nix'
Plug 'PotatoesMaster/i3-vim-syntax'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'SirVer/ultisnips'
Plug 'StanAngeloff/php.vim'
Plug 'Yggdroot/indentLine'
Plug 'chr4/nginx.vim'
Plug 'chriskempson/base16-vim'
Plug 'christoomey/vim-tmux-navigator'
Plug 'dag/vim-fish'
Plug 'elzr/vim-json'
Plug 'godlygeek/tabular'
Plug 'honza/vim-snippets'
Plug 'jiangmiao/auto-pairs'
" Plug 'ctrlpvim/ctrlp.vim'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'lucy/term.vim'
Plug 'mhinz/vim-grepper'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'smancill/conky-syntax.vim'
Plug 'thoughtbot/vim-rspec'
Plug 'tmux-plugins/vim-tmux'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-dotenv'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-haml'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-ragtag'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-vinegar'
Plug 'w0rp/ale'
Plug 'elixir-editors/vim-elixir'
Plug 'cespare/vim-toml'
Plug 'LokiChaos/vim-tintin'

call plug#end()

" Tabs instead of spaces
set expandtab
set tabstop=2
set shiftwidth=2
set softtabstop=2

set autoindent
set nostartofline

set encoding=utf-8

set hidden
set scrolloff=3
set showmode
set showcmd
set ruler
set backspace=indent,eol,start
set laststatus=2
set undofile

set wildmenu
set wildmode=list:longest
set wildignore=*.swp,*.bak,*.so,*.zip,*.lock
set wildignore+=*.xcf,*.gif,*.png,*.jpg,*.jpeg,*.psd,*.svg
set wildignore+=*.woff,*.woff2,*.eot,*.ttf
set wildignore+=*.pyc,*.class,*.sln,*.Master,*.csproj,*.csproj.user,*.cache,*.dll,*.pdb,*.min.*
set wildignore+=*/.git/**/*,*/.hg/**/*,*/.svn/**/*
set wildignore+=*/vendor/**/*,*/node_modules/**/*
set wildignore+=tags
set wildignore+=*.tar.*
set wildignorecase

set splitbelow
set splitright

" Show me what I'm doing
set concealcursor="nc"
set conceallevel=0

let mapleader = " "
let g:mapleader = " "

nnoremap ; :
vnoremap ; :
nnoremap / /\v
vnoremap / /\v
nnoremap <C-e> :e#<CR>
set ignorecase
set smartcase
set gdefault
set incsearch
set showmatch
map <tab> g%
cmap w!! %!sudo tee > /dev/null %

" ALE statusline
function! LinterStatus() abort
    let l:counts = ale#statusline#Count(bufnr(''))

    let l:all_errors = l:counts.error + l:counts.style_error
    let l:all_non_errors = l:counts.total - l:all_errors

    return l:counts.total == 0 ? 'OK' : printf(
    \   '%dW %dE',
    \   all_non_errors,
    \   all_errors
    \)
endfunction

set statusline=%<\ %f\ %{fugitive#statusline()}\ %{ObsessionStatus()}\ %{LinterStatus()}\ %r%y%m%=\ Line:\ \%l\/\%L\ Column:\ \%c

set wrap
set textwidth=79
set formatoptions=qrn1
"set colorcolumn=79

set list
set listchars=tab:»\ ,extends:›,precedes:‹,nbsp:·,trail:·

set nuw=6
" set number
" set relativenumber
set cursorline
set clipboard+=unnamedplus
set autowriteall
set autoread

" Limit the amount of filesystem events
set nowritebackup
set nobackup

augroup vimrcEx
	au!

	" When editing a file, always jump to the last known cursor position.
	" Don't do it when the position is invalid or when inside an event handler
	" (happens when dropping a file on gvim).
	autocmd BufReadPost *
				\ if line("'\'") > 0 && line("'\'") <= line("$") |
				\ exe "normal g`\"" |
				\ endif

	au BufWritePre * :%s/\s\+$//e	 " Remove trailing whitespace upon saving
	" au FocusLost,InsertLeave * bufdo call functions#AutoSave()
	" au InsertLeave * :wa
augroup END

if filereadable(expand("~/.vimrc_background"))
  let base16colorspace=256
  source ~/.vimrc_background
endif

set background=dark
let base16colorspace=256
colorscheme base16-ashes
" colorscheme term

" Create vertical split and switch to it
nnoremap <leader>w <C-w>v<C-w>l

" Split window navigation
" nnoremap <C-h> <C-w>h
" nnoremap <C-j> <C-w>j
" nnoremap <C-k> <C-w>k
" nnoremap <C-l> <C-w>l

" Move around split lines
nnoremap j gj
nnoremap k gk

" Fast editing of the .vimrc
map <leader>e :e! $MYVIMRC<CR>
map <leader>r :source $MYVIMRC<CR>
" autocmd bufwritepost init.vim source $MYVIMRC | echom "Reloaded " . $MYVIMRC | redraw

" Hide search highlighting
nnoremap <esc> :noh<return><esc>

" CtrlP options
" let g:ctrlp_cmd = 'CtrlP'
" let g:ctrlp_working_path_mode = 'rc'
" let g:ctrlp_root_markers = ['Gemfile']

" FZF
let g:fzf_buffers_jump = 1
let g:fzf_layout = { 'window': '10split enew' }
map <leader>f :Files<CR>
map <leader>r :History<CR>
map <leader>b :Buffers<CR>
map <leader>/ :Rg!<CR>

" Mapping selecting mappings
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)

" Insert mode completion
imap <c-x><c-k> <plug>(fzf-complete-word)
imap <c-x><c-f> <plug>(fzf-complete-path)
imap <c-x><c-j> <plug>(fzf-complete-file-ag)
imap <c-x><c-l> <plug>(fzf-complete-line)

autocmd! FileType fzf
autocmd  FileType fzf set laststatus=0 noshowmode noruler
  \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler

command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
  \   <bang>0)

if executable('rg')
  set grepprg=rg\ --color=never
  " let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
  " let g:ctrlp_use_caching = 0
endif

" Ripgrep project
nnoremap <leader>/ :Grepper<CR>
let g:grepper = {
      \ 'tools': ['rg', 'git'],
      \   'rg': {
      \   'grepprg': 'rg --vimgrep'
      \ }
      \}

" indentLine options
let g:indentLine_char = '¦'

"" Neomake
" Run neomake on buffer save
" autocmd! BufWritePost * Neomake
" let g:neomake_open_list = 2

nnoremap <leader>ln :lne<CR>
nnoremap <leader>lp :lp<CR>
nnoremap <leader>en :cn<CR>
nnoremap <leader>ep :cp<CR>
nnoremap <leader>ee :copen<CR>
nnoremap <leader>ec :cclose<CR>

" Abbreviations
"   Unix Timestamp in Command-line mode (for file names)
cab ts <C-R>=strftime("%s")<CR>
"  shrug asciimoji
iab shrug ¯\_(ツ)_/¯

"rebind my favorite commands from Git.vim for Fugitive
nmap <leader>gs :Gstatus<cr>
nmap <leader>gc :Gcommit<cr>
nmap <leader>ga :Gwrite<cr>
nmap <leader>gl :Glog<cr>
nmap <leader>gd :Gdiff<cr>

" Some buffer bindings
nmap <leader>bd :bdelete<cr>

" Do not hide characters in JSON files
let g:vim_json_syntax_conceal = 0

" ALE Config
" Set this in your vimrc file to disabling highlighting
let g:ale_set_highlights = 0
" ALE Bindings
nmap <silent> <leader>ep <Plug>(ale_previous_wrap)
nmap <silent> <leader>en <Plug>(ale_next_wrap)

" .envrc files should be ft=sh
autocmd BufRead,BufNewFile .envrc set filetype=sh
autocmd BufRead,BufNewFile .env   set filetype=sh
autocmd BufRead,BufNewFile .env.* set filetype=sh

" Find out what highlighting is applied at point
nm <silent> <F1> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name")
    \ . '> trans<' . synIDattr(synID(line("."),col("."),0),"name")
    \ . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name")
    \ . ">"<CR>

" Allow netrw to remove non-empty local directories
"
let g:netrw_localrmdir='rm -r'

" Deoplete autocomplete
" let g:deoplete#enable_at_startup = 1

" UltiSnips
let g:UltiSnipsSnippetsDir = '~/dev/dotfiles/neovim'
" let g:UltiSnipsSnippetDirectories = [ g:UltiSnipsSnippetsDir, 'UltiSnips' ]
let g:UltiSnipsListSnippets = "<s-tab>"
