" -----------------------------------------------------------------------------
" --------------- Plugin installation -----------------------------------------
" -----------------------------------------------------------------------------

" --------------- Vundle initialization script --------------------------------
" source: https://github.com/VundleVim/Vundle.vim

set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin('~/.vim/bundle') " INSTALL PLUGINS IN DROPBOX FOR VC

" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
":Plugin 'tpope/vim-fugitive'
" plugin from http://vim-scripts.org/vim/scripts.html
":Plugin 'L9'
" Git plugin not hosted on GitHub
":Plugin 'git://git.wincent.com/command-t.git'
" git repos on your local machine (i.e. when working on your own plugin)
":Plugin 'file:///home/gmarik/path/to/plugin'
" The sparkup vim script is in a subdirectory of this repo called vim.
" Pass the path to set the runtimepath properly.
":Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}
" Install L9 and avoid a Naming conflict if you've already installed a
" different version somewhere else.
":Plugin 'ascenator/L9', {'name': 'newL9'}

" Global plugins
Plugin 'benmills/vimux' " talk to tmux from vim
Plugin 'scrooloose/nerdtree' " file system navigation
Plugin 'SirVer/ultisnips' " create and manage snippets
Plugin 'honza/vim-snippets' "snippet library
Plugin 'vim-syntastic/syntastic' " syntax checker
Plugin 'tmhedberg/SimpylFold' " fold manager
Plugin 'Konfekt/FastFold' " faster folding
Plugin 'Valloric/YouCompleteMe' " autocompletion (has external dependencies)
Plugin 'easymotion/vim-easymotion' " enhanced motion capabilities
" Plugin 'ctrlpvim/ctrlp.vim' " fuzzy search engine for content
" Plugin 'FelikZ/ctrlp-py-matcher' " speed up CtrlP with python-based matcher
Plugin 'tpope/vim-commentary' " easy comment blocks
Plugin 'tpope/vim-surround' " edit brackets and surround symbols
Plugin 'Raimondi/delimitMate' " autocompletion for brackets and quotes

" highlight line indents
Plugin 'Yggdroot/indentLine'

" Filetype-specific plugins
Plugin 'vim-scripts/indentpython.vim' " conform with PEP-8 when auto-indenting
Plugin 'jeetsukumaran/vim-pythonsense' " easy python file navigation

" Plugins with custom color schemes (HAD TO EDIT .bashrc AND ADD
"export TERM=xterm-256color" ENABLING 256-COLOR SUPPORT FOR THIS TO WORK)
Plugin 'tyrannicaltoucan/vim-quantum'
Plugin 'AlessandroYorba/Despacio'
Plugin 'jacoborus/tender.vim'
" Plugin 'dfrunza/vim'
Plugin 'fcpg/vim-fahrenheit'
Plugin 'pbrisbin/vim-colors-off'
Plugin 'robertmeta/nofrils'
Plugin 'bruth/vim-newsprint-theme'
Plugin 'fxn/vim-monochrome'
Plugin 'ryanpcmcquen/true-monochrome_vim'
Plugin 'Blevs/vim-dzo'
Plugin 'arcticicestudio/nord-vim'
Plugin 'garybernhardt/dotfiles'
Plugin 'Lokaltog/vim-distinguished'
Plugin 'nanotech/jellybeans.vim'
Plugin 'altercation/vim-colors-solarized'
Plugin 'jnurmine/Zenburn'

" Other cosmetic plugins
Plugin 'felixhummel/setcolors.vim' " change color schemes interactively
Plugin 'vim-airline/vim-airline' " chosen over powerline as simpler version
Plugin 'vim-airline/vim-airline-themes' " custom airline themes
Plugin 'edkolev/tmuxline.vim' " inherit airline theme in tmux

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
"PluginList       -lists configured plugins
"PluginInstall    -installs plugins; append `!` to update or just :PluginUpdate
"PluginSearch foo -searches for foo; append `!` to refresh local cache
"PluginClean -confirms removal of unused plugins; append `!` to auto-approve
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

" -----------------------------------------------------------------------------
" --------------- Plugin settings ---------------------------------------------
" -----------------------------------------------------------------------------


" ---------------- NERDTree ---------------------------------------------------

" toggle NERDTree with ctrl-n
nnoremap <c-n> :NERDTreeToggle<CR>
 

" ---------------- EasyMotion -------------------------------------------------
" do not fade out text when quick-jumping
let g:EasyMotion_do_shade=0
" use simply <Leader> instead of the default <Leader><Leader>
map <Leader> <Plug>(easymotion-prefix)

" ---------------- VIMUX ------------------------------------------------------

" Save file and run in tmux
au FileType sh nnoremap <buffer> <Leader>r :update<CR>:call VimuxRunCommand('sudo bash ' . expand('%:p'))<CR>
" Run vim selection as tmux command
vnoremap <Leader>r :call VimuxRunCommand(@*)<CR>
" Prompt for a command to run
nnoremap <Leader>vp :VimuxPromptCommand<CR>
" Run last command executed by VimuxRunCommand
nnoremap <Leader>vl :VimuxRunLastCommand<CR>
" Inspect runner pane
nnoremap <Leader>vi :VimuxInspectRunner<CR>
" Close vim tmux runner opened by VimuxRunCommand
nnoremap <Leader>vq :VimuxCloseRunner<CR>
" Interrupt any command running in the runner pane
nnoremap <Leader>vx :VimuxInterruptRunner<CR>
" Zoom the runner pane (use <bind-key> z to restore runner pane)
nnoremap <Leader>vz :call VimuxZoomRunner()<CR>


" ---------------- UltiSnips --------------------------------------------------

let g:UltiSnipsExpandTrigger='<c-Space>'
let g:UltiSnipsListSnippets='<c-j>'
let g:UltiSnipsJumpForwardTrigger='<C-l>'
let g:UltiSnipsJumpBackwardTrigger='<C-h>'
let g:UltiSnipsEditSplit='horizontal'
let g:UltiSnipsSnippetDirectories=[ 'UltiSnips', 'mySnippets']


" ---------------- CtrlP ------------------------------------------------------

" let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch' } " speed up matching
" let g:ctrlp_show_hidden = 1 " also search in dotfiles
" let g:ctrlp_max_files = 0 " scan unlimited number of files
" let g:ctrlp_max_depth = 1000 " set unlimited search recursion depth

" ---------------- DelimitMate ------------------------------------------------

" try to balance matching pairs
let delimitMate_balance_matchpairs = 1

" do not duplicate spaces
" let delimitMate_expand_space = 0

" do not auto-fill inside strings
" let delimitMate_excluded_regions = "Comment"

" ---------------- indentLine -------------------------------------------------

let g:indentLine_char='|'

" ---------------- SYNTASTIC CHECKER ------------------------------------------

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
" let g:syntastic_python_checkers = ['pycodestyle']

" jump to first/previous/next error with <leader>e/p/n
nnoremap <leader>e :ll<CR>
nnoremap <leader>n :lnext<CR>
nnoremap <leader>p :lprev<CR>


" ---------------- AIRLINE ----------------------------------------------------

" the separator used on the left side >
let g:airline_left_sep='}'
" the separator used on the right side >
let g:airline_right_sep='{'
" enable modified detection >
let g:airline_detect_modified=1
" enable paste detection >
let g:airline_detect_paste=1
" enable crypt detection >
let g:airline_detect_crypt=1
" enable spell detection >
let g:airline_detect_spell=1
" enable iminsert detection >
let g:airline_detect_iminsert=0
" determine whether inactive windows should have the left section collapsed to
" only the filename of that buffer.  >
let g:airline_inactive_collapse=1
" themes are automatically selected based on the matching colorscheme. this
" can be overridden by defining a value. >
let g:airline_theme='tender' " 'base16_bright'
" airline symbol array
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.crypt = '🔒'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.maxlinenr = '☰'
let g:airline_symbols.maxlinenr = ''
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.spell = 'Ꞩ'
let g:airline_symbols.notexists = '∄'
let g:airline_symbols.whitespace = 'Ξ'

" ---------------- PYTHONSENSE ------------------------------------------------

""" These settings will be overridden in python_dbargman.vim

" map <buffer> ac <Plug>(PythonsenseOuterClassTextObject)
" map <buffer> ic <Plug>(PythonsenseInnerClassTextObject)
" map <buffer> af <Plug>(PythonsenseOuterFunctionTextObject)
" map <buffer> if <Plug>(PythonsenseInnerFunctionTextObject)
" map <buffer> ad <Plug>(PythonsenseOuterDocStringTextObject)
" map <buffer> id <Plug>(PythonsenseInnerDocStringTextObject)

" map <buffer> ]] <Plug>(PythonsenseStartOfNextPythonClass)
" map <buffer> ][ <Plug>(PythonsenseEndOfPythonClass)
" map <buffer> [[ <Plug>(PythonsenseStartOfPythonClass)
" map <buffer> [] <Plug>(PythonsenseEndOfPreviousPythonClass)
" map <buffer> ]m <Plug>(PythonsenseStartOfNextPythonFunction)
" map <buffer> ]M <Plug>(PythonsenseEndOfPythonFunction)
" map <buffer> [m <Plug>(PythonsenseStartOfPythonFunction)
" map <buffer> [M <Plug>(PythonsenseEndOfPreviousPythonFunction)

" map <buffer> g: <Plug>(PythonsensePyWhere)


" =============================================================================
