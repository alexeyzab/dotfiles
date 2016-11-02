" Leader
let mapleader = " "

" Color scheme
syntax enable
set background=dark
colorscheme solarized

" Make <c-h> work like <c-h> again (this is a problem with libterm)
if has('nvim')
  nnoremap <BS> <C-w>h
endif

" Default font size
set guifont=Fantasque\ Sans\ Mono:h12

set backspace=2   " Backspace deletes like most programs in insert mode
set nobackup
set nowritebackup
set noswapfile    " http://robots.thoughtbot.com/post/18739402579/global-gitignore#comment-458413287
set history=50
set ruler         " show the cursor position all the time
set showcmd       " display incomplete commands
set incsearch     " do incremental searching
set laststatus=2  " Always display the status line
set autowrite     " Automatically :write before running commands

if filereadable(expand("~/.config/nvim/.nvimrc.bundles"))
  source ~/.config/nvim/.nvimrc.bundles
endif

filetype plugin indent on

augroup vimrcEx
  autocmd!

  " When editing a file, always jump to the last known cursor position.
  " Don't do it for commit messages, when the position is invalid, or when
  " inside an event handler (happens when dropping a file on gvim).
  autocmd BufReadPost *
    \ if &ft != 'gitcommit' && line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

  " Set syntax highlighting for specific file types
  autocmd BufRead,BufNewFile Appraisals set filetype=ruby
  autocmd BufRead,BufNewFile *.md set filetype=markdown

  " Enable spellchecking for Markdown
  autocmd FileType markdown setlocal spell

  " Automatically wrap at 80 characters for Markdown
  autocmd BufRead,BufNewFile *.md setlocal textwidth=80

  " Automatically wrap at 72 characters and spell check git commit messages
  autocmd FileType gitcommit setlocal textwidth=72
  autocmd FileType gitcommit setlocal spell

  " Allow stylesheets to autocomplete hyphenated words
  autocmd FileType css,scss,sass setlocal iskeyword+=-
augroup END

" Softtabs, 2 spaces
set tabstop=2
set shiftwidth=2
set shiftround
set expandtab

" Display extra whitespace
set list listchars=tab:»·,trail:·,nbsp:·

" Make it obvious where 80 characters is
set textwidth=80
set colorcolumn=+1

" Numbers
set number
set numberwidth=5

" Tab completion
" will insert tab at beginning of line,
" will use completion if not at beginning
set wildmode=list:longest,list:full
function! InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    else
        return "\<c-p>"
    endif
endfunction
inoremap <Tab> <c-r>=InsertTabWrapper()<cr>
inoremap <S-Tab> <c-n>

" Switch between the last two files
nnoremap <leader><leader> <c-^>

" Run commands that require an interactive shell
nnoremap <Leader>r :RunInInteractiveShell<space>

" Treat <li> and <p> tags like the block tags they are
let g:html_indent_tags = 'li\|p'

" Open new split panes to right and bottom, which feels more natural
set splitbelow
set splitright

" Quicker window movement
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

" Fix NeoVim ESC issue
if !has('gui_running')
    set ttimeoutlen=10
    augroup FastEscape
        autocmd!
        au InsertEnter * set timeoutlen=0
        au InsertLeave * set timeoutlen=1000
    augroup END
endif

" Set spellfile to location that is guaranteed to exist, can be symlinked to
" Dropbox or kept in Git and managed outside of thoughtbot/dotfiles using rcm.
set spellfile=$HOME/.vim-spell-en.utf-8.add

" Always use vertical diffs
set diffopt+=vertical

" Lightline config
let g:lightline = {
      \'colorscheme':'solarized',
      \}
" Tmuxline config
let g:tmuxline_preset = {
      \'a'    : '#S',
      \'b'    : '#W',
      \'win'  : '#I: #W',
      \'cwin' : '#I: #W' ,
      \'y'    : ['#(echo "mail:") #(tmuxmail)'],
      \'z'    : '#H'}
let g:tmuxline_separators = {
      \'left' : '',
      \'left_alt' : '>',
      \'right' : '',
      \'right_alt' : '<',
      \'space' : ' '}

" Remove trailing whitespace
nnoremap <Leader>tw :%s/\s\+$//e<CR>

" Easymotion settings for h j k l
map <Leader><Leader> <Plug>(easymotion-prefix)
map <Leader><Leader>l <Plug>(easymotion-lineforward)
map <Leader><Leader>j <Plug>(easymotion-j)
map <Leader><Leader>k <Plug>(easymotion-k)
map <Leader><Leader>h <Plug>(easymotion-linebackward)

" <Leader>f{char} to move to {char}
map  <Leader>f <Plug>(easymotion-bd-f)
nmap <Leader>f <Plug>(easymotion-overwin-f)

" s{char}{char} to move to {char}{char}
nmap s <Plug>(easymotion-overwin-f2)

" Move to line
map <Leader>L <Plug>(easymotion-bd-jk)
nmap <Leader>L <Plug>(easymotion-overwin-line)

" Move to word
map  <Leader>w <Plug>(easymotion-bd-w)
nmap <Leader>w <Plug>(easymotion-overwin-w)

let g:EasyMotion_startofline = 0 " keep cursor colum when JK motion

" vim-easy-align settings
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap <Leader>ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap <Leader>ga <Plug>(EasyAlign)

" vim2hs disable folding
let g:haskell_conceal = 0
let g:haskell_conceal_enumerations = 0

" disable folding all over
set nofoldenable

" FZF command
nnoremap <C-p> :FZF<CR>

" This is the default extra key bindings
let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }

" Default fzf layout
let g:fzf_layout = { 'down': '-40%' }

" nohl mapping
nnoremap <C-n> :nohl<CR>

" Use deoplete
let g:deoplete#enable_at_startup = 1

" Neomake
autocmd! BufWritePost * Neomake
let g:neomake_haskell_enabled_makers = ['hlint']
let g:neomake_ruby_enabled_makers = ['rubocop']
let g:neomake_javascript_enabled_makers = ['jshint']
let g:neomake_json_enabled_makers = ['jsonlint']
let g:neomake_sh_enabled_makers = ['shellcheck']

" Options for Haskell Syntax Check
let g:neomake_haskell_ghc_mod_args = '-g-Wall'

" Matching brackets fix
set matchtime=0

" Hdevtools Integration
au FileType haskell nnoremap <buffer> <leader>d :HdevtoolsType<CR>
au FileType haskell nnoremap <buffer> <silent> <leader>m :HdevtoolsClear<CR>
let g:hdevtools_options = '-g-Wall'

" Haskell-vim indentation
let g:haskell_indent_if = 2
let g:haskell_indent_guard = 2

" Neovim terminal
tnoremap <Esc> <C-\><C-n>

" intero-neovim
" Process management:
nnoremap <Leader>io :InteroOpen<CR>
nnoremap <Leader>ik :InteroKill<CR>
nnoremap <Leader>ic :InteroHide<CR>
nnoremap <Leader>il :InteroLoadCurrentModule<CR>

" REPL commands
nnoremap <Leader>ie :InteroEval<CR>
nnoremap <Leader>it :InteroGenericType<CR>
nnoremap <Leader>iT :InteroType<CR>
nnoremap <Leader>ii :InteroInfo<CR>
nnoremap <Leader>iI :InteroTypeInsert<CR>

" Go to definition:
nnoremap <Leader>id :InteroGoToDef<CR>

" Highlight uses of identifier:
nnoremap <Leader>iu :InteroUses<CR>

" Reload the file in Intero after saving
" autocmd! BufWritePost *.hs InteroReload

" Python binaries
let g:python_host_prog = '/usr/bin/python2'
let g:python3_host_prog = '/usr/bin/python3'

" hindent style
let g:hindent_style = "johan-tibell"

" Vim Mundo
" Show undo tree
nmap <silent> <leader>u :MundoToggle<CR>
" Enable persistent undo so that undo history persists across vim sessions
set undofile
set undodir=~/.config/nvim/undo

" Hoogle {{{
" Hoogle the word under the cursor
nnoremap <silent> <leader>og :Hoogle<CR>

" Hoogle and prompt for input
nnoremap <leader>oG :Hoogle

" Hoogle for detailed documentation (e.g. "Functor")
nnoremap <silent> <leader>oi :HoogleInfo<CR>

" Hoogle for detailed documentation and prompt for input
nnoremap <leader>oI :HoogleInfo

" Hoogle, close the Hoogle window
nnoremap <silent> <leader>oz :HoogleClose<CR>

" }}}

" Git {{{

let g:extradite_width = 60
" Hide messy Ggrep output and copen automatically
function! NonintrusiveGitGrep(term)
  execute "copen"
  " Map 't' to open selected item in new tab
  execute "nnoremap <silent> <buffer> t <C-W><CR><C-W>T"
  execute "silent! Ggrep " . a:term
  execute "redraw!"
endfunction

command! -nargs=1 GGrep call NonintrusiveGitGrep(<q-args>)
nmap <leader>gs :Gstatus<CR>
nmap <leader>gg :copen<CR>:GGrep
nmap <leader>gl :Extradite!<CR>
nmap <leader>gd :Gdiff<CR>
nmap <leader>gb :Gblame<CR>

function! CommittedFiles()
  " Clear quickfix list
  let qf_list = []
  " Find files committed in HEAD
  let git_output = system("git diff-tree --no-commit-id --name-only -r HEAD\n")
  for committed_file in split(git_output, "\n")
    let qf_item = {'filename': committed_file}
    call add(qf_list, qf_item)
  endfor
  " Fill quickfix list with them
  call setqflist(qf_list)
endfunction

" Show list of last-committed files
nnoremap <silent> <leader>g? :call CommittedFiles()<CR>:copen<CR>

" }}}

" Tags {{{
map <leader>tt :TagbarToggle<CR>

set tags=tags;/
set cst
set csverb

set tags+=codex.tags;/

let g:tagbar_type_haskell = {
    \ 'ctagsbin'  : 'hasktags',
    \ 'ctagsargs' : '-x -c -o-',
    \ 'kinds'     : [
        \  'm:modules:0:1',
        \  'd:data: 0:1',
        \  'd_gadt: data gadt:0:1',
        \  't:type names:0:1',
        \  'nt:new types:0:1',
        \  'c:classes:0:1',
        \  'cons:constructors:1:1',
        \  'c_gadt:constructor gadt:1:1',
        \  'c_a:constructor accessors:1:1',
        \  'ft:function types:1:1',
        \  'fi:function implementations:0:1',
        \  'o:others:0:1'
    \ ],
    \ 'sro'        : '.',
    \ 'kind2scope' : {
        \ 'm' : 'module',
        \ 'c' : 'class',
        \ 'd' : 'data',
        \ 't' : 'type'
    \ },
    \ 'scope2kind' : {
        \ 'module' : 'm',
        \ 'class'  : 'c',
        \ 'data'   : 'd',
        \ 'type'   : 't'
    \ }
\ }

" Generate haskell tags with codex and hscope
map <leader>tg :!codex update --force<CR>:call system("git-hscope -X TemplateHaskell")<CR><CR>:call LoadHscope()<CR>

set csprg=hscope
set csto=1 " search codex tags first

nnoremap <silent> <C-\> :cs find c <C-R>=expand("<cword>")<CR><CR>
" Automatically make cscope connections
function! LoadHscope()
  let db = findfile("hscope.out", ".;")
  if (!empty(db))
    let path = strpart(db, 0, match(db, "/hscope.out$"))
    set nocscopeverbose " suppress 'duplicate connection' error
    exe "cs add " . db . " " . path
    set cscopeverbose
  endif
endfunction
au BufEnter /*.hs call LoadHscope()

" }}}

" hlint-refactor-vim keybindings
map <silent> <leader>hr :call ApplyOneSuggestion()<CR>
map <silent> <leader>hR :call ApplyAllSuggestions()<CR>

" Open window splits in various places
nmap <leader>sh :leftabove  vnew<CR>
nmap <leader>sl :rightbelow vnew<CR>
nmap <leader>sk :leftabove  new<CR>
nmap <leader>sj :rightbelow new<CR>

" Location list shortcuts
nmap <leader>l :lop <CR>
nmap <leader>q :lcl <CR>
