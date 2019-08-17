" =============================================================================
" ==================== Daniil's minimum-settings vimrc file ===================
" =============================================================================

" Initially based on: A Byte of Vim:
" (https://ia600208.us.archive.org/12/items/byteofvim/byteofvim.pdf)

" Partly inspired by video by Martin Brochhaus
" (https://www.youtube.com/watch?v=YhqsjUUHj6g)


" --------------- Overriding the leader key -----------------------------------
let mapleader = ','


" --------------- Sourcing all my vim plugins ---------------------------------
source ~/.vim/.myplugins.vim


" --------------- Basic settings ----------------------------------------------

" Use vim settings and not vi settings
set nocompatible

" Set zero hold time on switch to normal mode, quarter-second for bindings
set ttimeoutlen=0
set timeoutlen=250

" Main environment variables
if has('win32') || has('win64')
    let $MYVIMRC = '%HOME%/_vimrc'
    let $MYGVIMRC = '%HOME%/_gvimrc'
else
    let $MYVIMRC = '$HOME/.vimrc'
    let $MYGVIMRC = '$HOME/.gvimrc'
end

" Set color scheme
if has('gui_running')
    colo delek
else
    colo tender " fahrenheit, sialoquent, distinguished, monochrome, dzo
end

" Set highlight group for trailing whitespaces
au ColorScheme * highlight ExtraWhitespace ctermbg=lightgreen guibg=red

" Display hybrid line numbers
set number relativenumber

" Display cursor position
set ruler

" Don't wrap text with line breaks...
set textwidth=0
set wrapmargin=0

" ... however, set vertical line at 80 characters (pep-8 recommends 79)
set colorcolumn=80

" Show current mode at the bottom
set showmode

" Show which line is being edited
set cursorline

" Set long command history
set history=1000

" Show incomplete commands
set showcmd

" Remove toolbar and menu bar if in gui
if has('gui_running')
    set go-=m
    set go-=T
end

" Enable persistent undo
let myUndoDir = $HOME . '/.vim/undodir'
call system('mkdir ' . myUndoDir)
let &undodir = myUndoDir
set undofile

" Save with control-s (also had to edit .bashrc file using code from
" http://vim.wikia.com/wiki/Map_Ctrl-S_to_save_current_or_new_files)
nnoremap <silent> <C-S> :update<CR>
vnoremap <silent> <C-S> <Esc>:update<CR>
inoremap <silent> <C-S> <Esc>:update<CR>

" Quit window by typing (capital) Q
nnoremap Q :q<CR>


" --------------- Navigation options ------------------------------------------

" Margin above and below cursor
set scrolloff=3

" Lines to scroll when at top/bottom of screen
set scrolljump=1

" Map enter key to newline in normal mode
nnoremap <CR> o<Esc>

" Map j and k to previous/next line in wrapped text
nnoremap j gj
nnoremap k gk

" Map h and l to got to end of next/start of previous line when edge of line
nnoremap <expr> h (col('.')==1) ? 'k$' : 'h'
nnoremap <expr> l (col('.')==col('$')-1) ? 'j0' : 'l'

" Delete line break with backspace when at the beginning of a line
set backspace=indent,eol,start

" Navigate between windows with ctrl-h,j,k,l
nnoremap <c-h> <c-w>h
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-l> <c-w>l

" Move windows around with lowercase h,j,k,l
nnoremap <c-w><c-h> <c-w><s-h>
nnoremap <c-w><c-j> <c-w><s-j>
nnoremap <c-w><c-k> <c-w><s-k>
nnoremap <c-w><c-l> <c-w><s-l>

" New split windows should appear below / on the right
set splitbelow
set splitright

" Move back one tab with GT
nnoremap GT gT

" Use <C-b> to preview a file in firefox
nnoremap <C-b> :!firefox %<CR>


" --------------- Search and highlight options --------------------------------

" Turn on syntax highlighting
syntax on

" Set smart (automatic) indentation
set autoindent
set smartindent

" Use pythonic settings for file indentation by default
set shiftwidth=4
set tabstop=4
set expandtab
set smarttab

" Use the > and < keys in normal mode for one-line indentation
nnoremap > >l
nnoremap < <l

" In visual mode, don't drop selection after indenting
vnoremap > >gv
vnoremap < <gv

" Set unix file format (PEP-8 compliant)
set fileformat=unix

" Use UTF-8 encoding
set encoding=utf-8

" Show autocomplete menu
set wildmenu


" --------------- Back up main dotfiles on save -------------------------------

" Auto-source vimrc and make backup when saving changes
augroup myvimrc " {
    au!
    au BufWritePost .vimrc,_vimrc,vimrc
             \ | so $MYVIMRC
             \ | let b:backupname = '~/.backups/vimrc/vimrc_'
                                           \ . strftime('%Y%m%d_%H-%M-%S')
             \ | exe 'silent w!' b:backupname
             \ | exe 'redraw'
augroup END " }


" Auto-source plugin settings and make backup when saving changes
augroup myvimplugins " {
    au!
    au BufWritePost .myVimPlugins.vim
             \ | so $MYVIMRC
             \ | let b:backupname = '~/.backups/myVimPlugins/myVimPlugins_'
                                           \ . strftime('%Y%m%d_%H-%M-%S')
             \ | exe 'silent w!' b:backupname
             \ | exe 'redraw'
augroup END " }

" Make backup when saving changes to ftplugins
augroup myftplugin " {
    au!
    au BufWritePost ~/.vim/ftplugin/python_dbargman.vim
             \ | let b:backupname = '~/.backups/ftplugins/'
                                           \ . strftime('%Y%m%d_%H-%M-%S')
             \ | exe '!mkdir' b:backupname
             \ | exe '!cp -r ~/.vim/ftplugin/*' b:backupname
             \ | exe 'redraw'
augroup END " }

" Back up and source .myBashSettings (called by .bashrc) when making changes
augroup bash_settings " {
    au!
    au BufWritePost .myBashSettings
             \ | exe 'silent !source ' . $HOME . '/.myBashSettings'
             \ | let b:backupname = '~/.backups/myBashSettings/myBashSettings_'
                                          \ . strftime('%Y%m%d_%H-%M-%S')
             \ | exe ':silent w!' b:backupname
             \ | exe 'redraw'
augroup END " }

" Back up and source .tmux.conf when making changes
augroup tmux_conf " {
    au!
    au BufWritePost .tmux.conf
             \ | exe 'silent !tmux source-file ~/.tmux.conf'
             \ | let b:backupname = '~/.backups/tmux_conf/tmux_conf_'
                                          \ . strftime('%Y%m%d_%H-%M-%S')
             \ | exe ':silent w!' b:backupname
             \ | exe 'redraw'
augroup END " }

" Back up .gitignore when making changes
augroup gitignore " {
    au!
    au BufWritePost ~/.gitignore
             \ | let b:backupname = '~/.backups/gitignore_global/gitignore_'
                                          \ . strftime('%Y%m%d_%H-%M-%S')
             \ | exe ':silent w!' b:backupname
             \ | exe 'redraw'
augroup END " }

" --------------- Handling of project-specific and session-specific settings --

" Set settion options (things that a saved session will contain)
set sessionoptions=blank,folds,help,winsize


" Function: switch to project directory, load project settings and last session
function! SwitchDirAndLoadSession(newDir)

    " String variable with new working directory
    let b:newdir = a:newDir

    " String variable with the name of the session file (if one exists)
    let b:lastsession = '.LastVimSession.vim'

    " Try changing directory; throw error if no such directory
    try
        exe 'cd' b:newdir
    catch
        echoerr 'No such directory exists'
        return
    endtry

    " Try loading session; display message if no autosaved session available
    if (filereadable(b:lastsession))
        exe 'source' b:lastsession
    else
        echo 'No previous session available'
    endif
endfunction

" Map user command to switch directory and load new directory's session
com! -nargs=1 CD call SwitchDirAndLoadSession(<f-args>)

" Map user command to autosave session and close all windows
nnoremap :Q :mksession!<space>.LastVimSession.vim<CR>:qa

" Map user command to save all files, autosave session and close all windows
nnoremap :WQ :mksession!<space>.LastVimSession.vim<CR>:wa<CR>:qa

" -------------- Removing old swap files after crashed sessions ---------------

function! DeleteFileSwaps()
    write
    let l:output = ''
    redir => l:output 
    silent exec ':sw' 
    redir END 
    let l:current_swap_file = substitute(l:output, '\n', '', '')
    let l:base = substitute(l:current_swap_file, '\v\.\w+$', '', '')
    let l:swap_files = split(glob(l:base.'\.s*'))
    " delete all except the current swap file
    for l:swap_file in l:swap_files
        if !empty(glob(l:swap_file)) && l:swap_file != l:current_swap_file 
            call delete(l:swap_file)
            echo "swap file removed: ".l:swap_file
        endif
    endfor
    " Reset swap file extension to `.swp`.
    set swf! | set swf!
    echo "Reset swap file extension for file: ".expand('%')
endfunction
com! DeleteFileSwaps :call DeleteFileSwaps()

" -----------------------------------------------------------------------------
" -----------------------------------------------------------------------------
" -----------------------------------------------------------------------------
