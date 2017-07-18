" Plugin for handling of python code blocks and block-wise execution in tmux
"
" Author: Daniil Bargman, daniil.bargman@gmail.com


" =============================================================================
" -------------------------- Global settings ----------------------------------
" =============================================================================

let b:blockDelim="^\\s*#\\s%%.*"
let b:blockLineFG = 16
let b:blockLineBG=240
let b:blockBG=0


" =============================================================================
" --------- Custom highlighting of code blocks and block separators -----------
" =============================================================================

" clear block highlighting and initiate list of block lines to an empty object
sign unplace *
let b:blockLines=[]

" function to clear current block line separators
function! ClearBlockLines()
    for i in b:blockLines
        exe ":sign unplace " . i . " file=" . expand("%:p")
    endfor
    let b:blockLines=[]
endfunction

" function to highlight block separators
function! HighlightBlockLines()

    " clear previous block lines
    call ClearBlockLines()
     
    " place new block lines and save their positions
    let b:lineNum=1
    while b:lineNum<=line('$')
        if getline(b:lineNum)=~#'^\s*#\s%%.*'
            exe ":sign place " . b:lineNum . " line=" . b:lineNum .
                    \ " name=pylineHL file=" . expand("%:p")
            let b:blockLines+=[b:lineNum]
        endif
        let b:lineNum+=1
    endwhile
endfunction

" Create highlight group for lines that separate blocks
function! HighlightBlock()
    " clear all signs and redraw block lines
    sign unplace *
    let b:blockLines=[]
    call HighlightBlockLines()
    " Highlight every line in block with specified background
    let b:lineNum=line('.')
    while !(getline(b:lineNum)=~#'^\s*#\s%%.*')
        if b:lineNum==line('$')
            break
        endif
        exe ":sign place " . b:lineNum .
            \ " line=" . b:lineNum . " name=pyBlockHL file=" . expand("%:p")
        let b:lineNum+=1
    endwhile
    let b:lineNum=line('.')
    while !(getline(b:lineNum)=~#'^\s*#\s%%.*')
        if b:lineNum==1
            break
        endif
        exe ":sign place " . b:lineNum .
            \ " line=" . b:lineNum . " name=pyBlockHL file=" . expand("%:p")
        let b:lineNum-=1
    endwhile
    " place a marker to save which block is selected
    normal mb
    " redraw the editor
    exe 'redraw!'
endfunction

" Autocommand to highlight block separators
augroup pyBlockLineHL
    au!
    au BufEnter,BufNew *.py
        \ | exe ':highlight pyBlockLines cterm=underline ctermbg=' . 
            \ b:blockLineBG . ' ctermfg=' . b:blockLineFG
        \ | sign define pylineHL linehl=pyBlockLines
    au BufEnter,BufNew *.py
        \ | exe ':highlight pyBlocks cterm=bold ctermbg=' . b:blockBG
        \ | sign define pyBlockHL linehl=pyBlocks
    au BufEnter,BufNew *.py call HighlightBlockLines()
        \ | exe 'redraw!'
    au InsertEnter,TextChanged *.py call HighlightBlock()
augroup END


"inoremap <Esc> <Esc>:call HighlightBlockLines()<CR>:exe 'redraw!'<CR>

" =============================================================================
" ---------------------- Navigation between code blocks -----------------------
" =============================================================================

" Jump to current active block with leader-b
nnoremap <Leader>b 'b

" Jump to previous/next block with leader-k/leader-j
nnoremap <Leader>j :exe "/".b:blockDelim<CR>j
nnoremap <Leader>k :exe "?".b:blockDelim<CR>nj


" =============================================================================
" ------------------------- Blockwise code execution --------------------------
" =============================================================================

" Block-wise code execution
function! SendToIPython()
    " Execute current selection in tmux pane running IPython below
    call VimuxRunCommand("cpaste -s %%")
    call VimuxRunCommand(@1)
    call VimuxRunCommand("%%")
endfunction
function! ExecutePythonBlock()
    " Set marker at current cursor position
    normal mp
    " Find end of current execution block
    exe "/".b:blockDelim
    " Select all of current execution block
    normal kVNj"1y
    " Execute current selection in tmux pane below
    call SendToIPython()
    " Go back the original cursor position
    normal 'p
endfunction

function! ExePyBlockAndMove()
    " Find end of current execution block
    exe "/".b:blockDelim
    " Set marker at current cursor position
    normal mp
    " Select all of current execution block
    normal kVNj"1y
    " Execute current selection in tmux pane below
    call SendToIPython()
    " Go back the original cursor position
    normal 'pj
endfunction

" Map functions in python files
nnoremap <buffer> <Leader>rr 'b:call ExecutePythonBlock()<CR>
vnoremap <buffer> <Leader>r "1y:call SendToIPython()<CR>
nnoremap <buffer> <C-x> 'b:call ExePyBlockAndMove()<CR>
                    \:call HighlightBlock()<CR>

" =============================================================================
