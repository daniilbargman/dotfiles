" Plugin for handling of python code blocks and block-wise execution in tmux
"
" Author: Daniil Bargman, daniil.bargman@gmail.com


" =============================================================================
" -------------------------- Global settings ----------------------------------
" =============================================================================

" Code block separator text: by default, "# %%" followed by any type of text
let b:blockDelim='^\s*#\s%%.*'

" remove background color by default
hi Normal ctermbg=None

" background color to highlight lines that separate code blocks
let b:blockLineBG=240

" text color in lines that separate code blocks
let b:blockLineFG = 16

" background color to highlight the current active code block
let b:blockBG="NONE"

" indentLine color to match background color of matched block
let indentLine_bgcolor_term="NONE"

" =============================================================================
" --------- Custom highlighting of code blocks and block separators -----------
" =============================================================================

" change highlighting for normal mode

" change highlighting for visual mode
hi Visual ctermbg=white ctermfg=black

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
        if getline(b:lineNum) =~# b:blockDelim
            exe ":sign place " . b:lineNum . " line=" . b:lineNum .
                    \ " name=pylineHL file=" . expand("%:p")
            let b:blockLines+=[b:lineNum]
        endif
        let b:lineNum+=1
    endwhile
endfunction

" create a highlight group for the active block
function! HighlightBlock()
    " clear all signs and redraw block lines
    sign unplace *
    let b:blockLines=[]
    call HighlightBlockLines()

    " find current line number
    let b:lineNum=line('.')

    " figure out closest neighbouring code block separators
    let b:activeBlockStart=1  " start of block at start of file by default
    let b:activeBlockEnd=line('$')  " end of block at end of file by default
    for i in b:blockLines  " assume blocklines is a sorted array
        if i > b:lineNum
            let b:activeBlockEnd=i-1
            break
        endif
        let b:activeBlockStart=i+1
    endfor

    " loop over block and place sings
    while b:activeBlockStart <= b:activeBlockEnd
        exe ":sign place " . b:activeBlockStart .
            \ " line=" . b:activeBlockStart
            \ . " name=pyBlockHL file=" . expand("%:p")
        let b:activeBlockStart+=1
    endwhile

    " place a marker to save which block is selected
    normal mb

    " redraw the editor
    exe 'redraw!'

endfunction

" Autocommand to highlight block separators
augroup pyBlockLineHL
    au!
    au BufEnter *.py if line('$')==1&&getline(1)==''
                \|exe ":normal! idbpy"
                \|endif
    au BufEnter,BufRead *.py
        \ | exe ':highlight pyBlockLines cterm=underline ctermbg=' . 
            \ b:blockLineBG . ' ctermfg=' . b:blockLineFG
        \ | sign define pylineHL linehl=pyBlockLines
    " au BufEnter,BufRead *.py
    "     \ | exe ':highlight pyBlocks ctermbg=' . b:blockBG
    "     \ | sign define pyBlockHL linehl=pyBlocks
    au BufEnter,BufRead *.py
        \ | call HighlightBlockLines()
        \ | exe 'redraw!'
    " au InsertEnter,TextChanged *.py call HighlightBlock()
    au InsertEnter,TextChanged *.py call HighlightBlockLines()
    au BufEnter *.py if line('$')==1&&getline(1)=='dbpy'
                \|exe ":startinsert!"
                \|call feedkeys("\<C-j>")
                \|call feedkeys("1\<CR>")
                \|endif
augroup END


"inoremap <Esc> <Esc>:call HighlightBlockLines()<CR>:exe 'redraw!'<CR>

" =============================================================================
" ---------------------- Navigation between code blocks -----------------------
" =============================================================================

" " Jump to current active block with leader-b
" nnoremap <Leader>b 'b

" " Highlight block under cursor with leader-x
" nnoremap <Leader>x :call HighlightBlock()<CR>

" Jump to previous/next block with leader-k/leader-j
nnoremap <Leader>j :exe "/".b:blockDelim<CR>j
nnoremap <Leader>k :exe "?".b:blockDelim<CR>nj
" nnoremap <Leader>j :exe "/".b:blockDelim<CR>j:call HighlightBlock()<CR>
" nnoremap <Leader>k :exe "?".b:blockDelim<CR>nj:call HighlightBlock()<CR>



" =============================================================================
" ------------------------- Blockwise code execution --------------------------
" =============================================================================

" select a block of python code in visual mode
function! SelectPyBlock()
    " Find end of current execution block
    exe "/".b:blockDelim
    " Select all of current execution block
    normal kVNj
endfunction

" function to copy code block to clipboard
function! CopyPyBlockToClipboard()
    " Set marker at current cursor position
    normal mp
    " Find end of current execution block
    call SelectPyBlock()
    " Select all of current execution block
    normal "+y
    " go back to original cursor position
    normal 'p
endfunction

" helper function for sending code to ipython
function! SendToIPython()
    " Execute current selection in tmux pane running IPython below
    " sleep 50m
    call VimuxRunCommand("%cpaste")
    sleep 50m
    call VimuxRunCommand(@1 . "--")
    " call VimuxRunCommand("--\n")
endfunction

" execute a block of pyton code
function! ExecutePythonBlock()
    " Set marker at current cursor position
    normal mp
    " Find end of current execution block
    call SelectPyBlock()
    " Select all of current execution block
    normal "1y
    " Execute current selection in tmux pane below
    call SendToIPython()
    " Go back the original cursor position
    normal 'p
endfunction

function! ExePyBlockAndMove()
    " Execute block
    call ExecutePythonBlock()
    " Go back the original cursor position
    normal nj
endfunction

" Map functions in python files
nnoremap <buffer> <leader>v :call SelectPyBlock()<CR>
nnoremap <buffer> <leader>c :call CopyPyBlockToClipboard()<CR>
nnoremap <buffer> <Leader>rr :call ExecutePythonBlock()<CR>
vnoremap <buffer> <Leader>r "1y:call SendToIPython()<CR>
nnoremap <buffer> <C-x> :call ExePyBlockAndMove()<CR>
                    " \:call HighlightBlock()<CR>

" =============================================================================
" ------------------------- Other python functionality ------------------------
" =============================================================================

" Expanding or folding a list in brackets with ctrl-e
function! ExpandBrackets()

    " Check that correct thing is found; otherwise, return
    let b:brType = getline(".")[col(".")-1]
    if b:brType != "(" && b:brType != "[" && b:brType != "{"
        return
    endif

    " set cursor to save where the bracket starts
    normal mq

    " Otherwise, find where the bracket ends
    normal %
    let b:matchingBr = getline(".")[col(".")-1]
    let b:bracketEnd = line(".")
    normal %
    let b:bracketStart = line(".")

    " if the bracket is single-line, it needs to be semi-expanded
    if b:bracketStart == b:bracketEnd
        try
            exe ':s/\%>' . col(".") . 'c \<\(for\|if\|else\)\> /\r\1 /g'
        catch
            exe ':s/\%>' . col(".") . 'c, /,\r/ge'
        endtry
        let b:bracketEnd = line(".")
        let b:i = b:bracketEnd-b:bracketStart
        exe ':normal ' . b:i . 'k$'
        exe ':normal F' . b:brType
        while b:i > 0
            exe ":normal Jr\<CR>"
            let b:i -= 1
        endwhile
        normal 'q
        exe ':normal f' . b:brType
        return
    endif

    " If we are at the end of the line, the bracket should be made compact
    if col(".") == col("$")-1
        normal Jxh%
        let b:bracketEnd = line(".")
        normal %$
        let b:i = b:bracketEnd-b:bracketStart
        while b:i > 0
            normal J
            let b:i -= 1
        endwhile
        if getline(".")[col(".")-1] == " "
            normal xh
        endif
        if getline(".")[col(".")-2] == " "
            normal hx
        endif
        if getline(".")[col(".")-1] == ","
            normal x
        endif
        if getline(".")[col(".")-2] == ","
            normal hx
        endif
        exe ':normal F' . b:brType
        return
    endif

    " if the bracket is multi-line, it is semi-expanded and should be expanded
    if b:bracketStart != b:bracketEnd
        exe ":normal! a\<CR>" 
        while line(".") <= b:bracketEnd
            exe ":normal Jr\<CR>"
        endwhile
        exe ':normal ' . (line(".")-b:bracketStart) . 'k$'
        if getline(".")[col(".")-1] == ","
            exe ':normal F' . b:brType
            normal %
            exe ":normal! i,\<CR>"
            normal %
        else
            exe ':normal F' . b:brType
            normal %
            exe ":normal! i\<CR>"
            normal %
        endif
        return
    endif

endfunction

nnoremap <C-e> :call ExpandBrackets()<CR>

" Jump between function / method / property definitions with <leader>m/n
map <buffer> <Leader>m :exe '/^ *def \\|^ *class '<CR>
map <buffer> <Leader>n :exe '?^ *def \\|^ *class '<CR>

" =============================================================================
