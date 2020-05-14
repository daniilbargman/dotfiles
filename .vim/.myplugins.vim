" -----------------------------------------------------------------------------
" --------------- Plugin installation -----------------------------------------
" -----------------------------------------------------------------------------

" --------------- Vundle initialization script --------------------------------

" manage plugins with vim-plug (installed externally)
call plug#begin()

" Global plugins
Plug 'scrooloose/nerdtree' " file system navigation
Plug 'SirVer/ultisnips' " create and manage snippets
Plug 'honza/vim-snippets' "snippet library
Plug 'tmhedberg/SimpylFold' " fold manager
Plug 'Konfekt/FastFold' " faster folding
Plug 'easymotion/vim-easymotion' " enhanced motion capabilities
Plug 'tpope/vim-commentary' " easy comment blocks
Plug 'tpope/vim-surround' " edit brackets and surround symbols
Plug 'Raimondi/delimitMate' " autocompletion for brackets and quotes

" highlight line indents
Plug 'Yggdroot/indentLine'

" Filetype-specific plugins
Plug 'vim-scripts/indentpython.vim' " conform with PEP-8 when auto-indenting
" Plug 'gko/vim-coloresque' " change background in color codes to code value
Plug 'chrisbra/Colorizer' " conform with PEP-8 when auto-indenting
" better python syntax highlighting
Plug 'vim-python/python-syntax'

" language server protocol
Plug 'prabirshrestha/async.vim' " asynchronous operations - required
Plug 'prabirshrestha/vim-lsp' " actual LSP plugin
Plug 'prabirshrestha/asyncomplete.vim' " autocompletion
Plug 'prabirshrestha/asyncomplete-lsp.vim' " autocompletion integratio with LSP
Plug 'mattn/vim-lsp-settings' " easy load of LSP settings
Plug 'thomasfaingnaert/vim-lsp-snippets' " LSP integration with snippets
Plug 'thomasfaingnaert/vim-lsp-ultisnips' " LSP integration with snippets
Plug 'prabirshrestha/asyncomplete-ultisnips.vim' " ultisnips in asyncomplete
Plug 'prabirshrestha/asyncomplete-file.vim' " autocomplete file paths

" tmux integration
Plug 'benmills/vimux' " talk to tmux from vim
Plug 'christoomey/vim-tmux-navigator' " easily jump between vim/tmux panes
Plug 'tmux-plugins/vim-tmux-focus-events' " bug fix for file auto-reloading
Plug 'edkolev/tmuxline.vim' " inherit airline theme in tmux
" Plug 'wellle/tmux-complete.vim'  " asyncomplete from tmux panes

" Plugins with custom color schemes (HAD TO EDIT .bashrc AND ADD
Plug 'jacoborus/tender.vim'

" Other cosmetic plugins
Plug 'felixhummel/setcolors.vim' " change color schemes interactively
Plug 'vim-airline/vim-airline' " chosen over powerline as simpler version
Plug 'vim-airline/vim-airline-themes' " custom airline themes

" end plugin load
call plug#end()


" -----------------------------------------------------------------------------
" --------------- Plugin settings ---------------------------------------------
" -----------------------------------------------------------------------------

" --------------- vim-lsp -----------------------------------------------------

" do not use default folding method
let g:lsp_fold_enabled = 0

" enable autocompletion preview window
let g:asyncomplete_auto_popup = 1
set completeopt+=preview

" register language servers
let g:lsp_settings = {
\   'pyls': {
\     'workspace_config': {
\       'pyls': {
\         'configurationSources': ['flake8']
\       }
\     }
\   },
\}

" register snippets
if has('python3')
    call asyncomplete#register_source(asyncomplete#sources#ultisnips#get_source_options({
        \ 'name': 'ultisnips',
        \ 'priority': 0,
        \ 'whitelist': ['*'],
        \ 'completor': function('asyncomplete#sources#ultisnips#completor'),
        \ }))
endif

" register files
au User asyncomplete_setup call asyncomplete#register_source(asyncomplete#sources#file#get_source_options({
    \ 'name': 'file',
    \ 'whitelist': ['*'],
    \ 'priority': 10,
    \ 'completor': function('asyncomplete#sources#file#completor')
    \ }))

" autocompletion
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <c-l>    pumvisible() ? "\<C-y>" : "\<c-l>"

" auto-close preview window when completion is done
autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif

" navigate between diagnostice with <leader>e
nnoremap <leader>e :LspNextDiagnostic<cr>
nnoremap <leader>p :LspNextDiagnostic<cr>

" toggle location window with <leader><leader>{e,q}
nnoremap <leader><leader>e :LspDocumentDiagnostics<cr><c-w><c-p>
nnoremap <leader><leader>q :lclose<cr>

" enable highlighting
let g:lsp_highlights_enabled = 1
let g:lsp_textprop_enabled = 0
" highlight LspErrorHighlight ctermfg=red guifg=red ctermbg=green guibg=green

" diagnostic signs
let g:lsp_signs_enabled = 1         " enable signs
let g:lsp_diagnostics_echo_cursor = 1 " enable echo under cursor when in normal mode

" error highlighting
let g:lsp_signs_error = {'text': '✗'}
let g:lsp_signs_warning = {'text': '!'}
let g:lsp_signs_information = {'text': 'i'}
let g:lsp_signs_hint = {'text': 'h'}

" enable references under cursor
let g:lsp_highlight_references_enabled = 1
highlight lspReference ctermfg=black guifg=red ctermbg=white guibg=green

" " debug
" let g:lsp_log_verbose = 1
" let g:lsp_log_file = expand('~/vim-lsp.log')
" let g:asyncomplete_log_file = expand('~/asyncomplete.log')


" ---------------- UltiSnips --------------------------------------------------

let g:UltiSnipsExpandTrigger='<c-e>'
let g:UltiSnipsListSnippets='<c-u>'
let g:UltiSnipsJumpForwardTrigger='<C-l>'
let g:UltiSnipsJumpBackwardTrigger='<C-h>'
let g:UltiSnipsEditSplit='horizontal'
let g:UltiSnipsSnippetDirectories=['plugged/ultisnips', 'mySnippets', 'plugged/vim-snippets/UltiSnips']

" ---------------- NERDTree ---------------------------------------------------

" toggle NERDTree with ctrl-n
nnoremap <c-n> :NERDTreeToggle<CR>
 

" ---------------- EasyMotion -------------------------------------------------
" do not fade out text when quick-jumping
let g:EasyMotion_do_shade=0
" use simply <Leader> instead of the default <Leader><Leader>
map <Leader> <Plug>(easymotion-prefix)


" ---------------- DelimitMate ------------------------------------------------

" try to balance matching pairs
let delimitMate_balance_matchpairs = 1

" do not duplicate spaces
let delimitMate_expand_space = 0
" duplicate newline
let delimitMate_expand_cr = 1
" in python, replicate triple quotes
au FileType python let b:delimitMate_nesting_quotes = ['"', "'"]
" let delimitMate_expand_inside_quotes = 1
" jump out of a filled pair with ctrl-l (does not clash with ultisnips)
imap <expr> <C-l> "<Plug>delimitMateS-Tab"


" ---------------- indentLine -------------------------------------------------

let g:indentLine_char = '┊'
let g:indentLine_showFirstIndentLevel=1
" let g:indentLine_char_list = ['|', '¦', '┆', '┊']
" let g:indentLine_leadingSpaceEnabled=1
" let g:indentLine_leadingSpaceChar='·'
" let g:indentLine_conceallevel=0
" let g:vim_json_syntax_conceal=0
" let g:vim_markdown_conceal=0
" let g:indentLine_fileTypeExclude = ['json', 'markdown']
let g:indentLine_concealcursor = ""

" ---------------- VIMUX ------------------------------------------------------

" Save file and run in tmux
au FileType sh nnoremap <buffer> <Leader>r :update<CR>:call VimuxRunCommand('sudo bash ' . expand('%:p'))<CR>
" Run vim selection as tmux command
vnoremap <Leader>r :<C-U>call VimuxRunCommand(@*)<CR>
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


" ---------------- AIRLINE ----------------------------------------------------

" " the separator used on the left side >
" let g:airline_left_sep='}'
" " the separator used on the right side >
" let g:airline_right_sep='{'
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
let g:airline_inactive_collapse=0
" themes are automatically selected based on the matching colorscheme. this
" can be overridden by defining a value. >
let g:airline_theme='tender' " 'base16_bright'
" airline symbol array
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
" " unicode symbols
" let g:airline_left_sep = '»'
" let g:airline_left_sep = '▶'
" let g:airline_right_sep = '«'
" let g:airline_right_sep = '◀'
" let g:airline_symbols.crypt = '🔒'
" let g:airline_symbols.linenr = '␊'
" let g:airline_symbols.linenr = '␤'
" let g:airline_symbols.linenr = '¶'
" let g:airline_symbols.maxlinenr = '☰'
" let g:airline_symbols.maxlinenr = ''
" let g:airline_symbols.branch = '⎇'
" let g:airline_symbols.paste = 'ρ'
" let g:airline_symbols.paste = 'Þ'
" let g:airline_symbols.paste = '∥'
" let g:airline_symbols.spell = 'Ꞩ'
" let g:airline_symbols.notexists = '∄'
" let g:airline_symbols.whitespace = 'Ξ'
let g:airline_powerline_fonts = 1
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
" let g:airline_symbols.branch = ''
" let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = '☰'
" let g:airline_symbols.maxlinenr = ''
" let g:airline_symbols.dirty=⚡

" YCM integration
let g:airline#extensions#ycm#enabled = 1


" disable tmuxline extension as a snapshot has been created and exists as
" .tmux/tmuxline_snapshot
let g:airline#extensions#tmuxline#enabled = 0


" ---------------- Colorizer --------------------------------------------------
"
let g:colorizer_auto_filetype='css,html'

" =============================================================================

