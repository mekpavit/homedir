-- START my vim general config
---- Remap leader to Space
vim.g.mapleader = ' '
vim.api.nvim_set_keymap('i', 'jk', '<ESC>', {noremap = true})
vim.api.nvim_set_keymap('i', '<C-g>', '<ESC>', {noremap = true})

vim.o.splitright = true -- always create new split windows on right side
vim.o.relativenumber = true -- use relative line number 
vim.o.undofile = true -- keep undo history
vim.api.nvim_exec('let loaded_matchparen = 1', false) -- remove highlight on matching parentheses

-- Remap for dealing with word wrap
vim.api.nvim_set_keymap('n', 'k', "v:count == 0 ? 'gk' : 'k'", { noremap=true, expr = true, silent = true})
vim.api.nvim_set_keymap('n', 'j', "v:count == 0 ? 'gj' : 'j'", {noremap= true, expr = true, silent = true})
-- END

-- START install packer.nvim for package management
if vim.fn.empty(vim.fn.glob(vim.fn.stdpath('data')..'/site/pack/packer/opt/packer.nvim')) > 0 then
    vim.api.nvim_command('!git clone https://github.com/wbthomason/packer.nvim '..vim.fn.stdpath('data')..'/site/pack/packer/opt/packer.nvim')
end
vim.cmd [[packadd packer.nvim]]
-- END

-- START install neovim plugins
require('packer').startup(
    function(use)

        use 'wbthomason/packer.nvim'

        -- must have plugins
        use 'tpope/vim-surround' -- remap S to add one character surround the visual selected text
        use 'tpope/vim-fugitive' -- add magit-like features to neovim
        use 'tpope/vim-commentary' -- add gc action to comment code
        use 'tpope/vim-sleuth' -- auto detect indent based on opening file
        use 'itchyny/lightline.vim' -- beatiful status bar
        use 'windwp/nvim-autopairs' -- auto pair parentheses

        -- ready-to-use neovim's native lsp configurations
        use 'neovim/nvim-lspconfig'

        -- various plugins for completion and snippeting
        use 'hrsh7th/nvim-compe' -- Completion
        use 'hrsh7th/vim-vsnip' -- Snippet engine
        use 'hrsh7th/vim-vsnip-integ' -- Provide integration between vim-vsnip and neovim's builtin lsp client
        use 'golang/vscode-go' -- Snippets for Go
        use 'Dart-Code/Dart-Code' -- Snippets for Dart and Flutter

        use 'preservim/nerdtree' -- File Explorer

        --[[
            Fuzzy finder that can do many useful things. What I uses here are
            - LSP -> 1. Perform code action 2. See/Goto code references
            - will try more...
        --]]
        use {'nvim-telescope/telescope.nvim', requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}}}

        -- abstract all languages test runner command into 4 simple vim commands
        use 'vim-test/vim-test'

        -- better syntax highlighting
        use 'nvim-treesitter/nvim-treesitter'

    end
)
-- END

-- START config LSP
local nvim_lsp = require('lspconfig')

local on_attach = function(client, bufnr)
    local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
    local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

    -- Mappings.
    local opts = { noremap=true, silent=true }
    buf_set_keymap('n', '<Leader>cD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
    buf_set_keymap('n', '<Leader>cd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
    buf_set_keymap('n', 'gh', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
    buf_set_keymap('n', '<Leader>ci', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
    buf_set_keymap('n', '<Leader>cr', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
    buf_set_keymap('n', '<Leader>ce', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
    buf_set_keymap('n', '[e', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
    buf_set_keymap('n', ']e', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)

    -- Set some keybinds conditional on server capabilities
    if client.resolved_capabilities.document_formatting then
        buf_set_keymap("n", "<space>cf", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
    end

    -- Set autocommands conditional on server_capabilities
    if client.resolved_capabilities.document_highlight then
        vim.api.nvim_exec([[
            hi LspReferenceRead cterm=bold ctermbg=red guibg=LightYellow
            hi LspReferenceText cterm=bold ctermbg=red guibg=LightYellow
            hi LspReferenceWrite cterm=bold ctermbg=red guibg=LightYellow
            augroup lsp_document_highlight
              autocmd! * <buffer>
              autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
              autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
            augroup END
        ]], false)
    end
end

-- Use a loop to conveniently both setup defined servers 
-- and map buffer local keybindings when the language server attaches
local servers = { "gopls", "tsserver" }
for _, lsp in ipairs(servers) do
    nvim_lsp[lsp].setup { on_attach = on_attach }
end 

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

nvim_lsp.dartls.setup{
    on_attach = on_attach,
    capabilities = capabilities,
}
-- END

-- START config telescope
require('telescope').setup({
    defaults = {
        mappings = {
            i = {
                ["<C-j>"] = require('telescope.actions').move_selection_next,
                ["<C-k>"] = require('telescope.actions').move_selection_previous,
                ["<C-g>"] = require('telescope.actions').close,
            },
            n = {
                ["<C-g>"] = require('telescope.actions').close,
            },
        },
    },
})
vim.api.nvim_set_keymap('n', '<Leader>ff', '<cmd>:Telescope find_files<cr>', {noremap = true})
vim.api.nvim_set_keymap('n', '<Leader>ca', '<cmd>:Telescope lsp_code_actions<cr>', {noremap = true})
vim.api.nvim_set_keymap('n', '<Leader>cR', '<cmd>:Telescope lsp_references<cr>', {noremap = true})
-- END


-- START config nvim-compe
vim.o.completeopt = 'menuone,noselect'
require('compe').setup({
  enabled = true;
  autocomplete = true;
  debug = false;
  min_length = 1;
  preselect = 'enable';
  throttle_time = 80;
  source_timeout = 200;
  incomplete_delay = 400;
  max_abbr_width = 100;
  max_kind_width = 100;
  max_menu_width = 100;
  documentation = true;

  source = {
    path = true;
    buffer = true;
    calc = true;
    nvim_lsp = true;
    nvim_lua = true;
    vsnip = true;
  };
})
vim.api.nvim_set_keymap('i', '<C-j>', '<C-n>', {noremap = true} )
vim.api.nvim_set_keymap('i', '<C-k>', '<C-p>', {noremap = true} )
vim.api.nvim_set_keymap('s', '<C-j>', '<Tab>', {noremap = true} )
vim.api.nvim_set_keymap('s', '<C-k>', '<S-Tab>', {noremap = true} )
-- END

-- START config nerdtree
vim.api.nvim_set_keymap('n', '<Leader>op', ':NERDTreeToggle<CR>', {})
-- END
--
-- START config lightline
vim.g.lightline = { colorscheme = 'powerline';
    active = { left = { { 'mode', 'paste' }, { 'gitbranch', 'readonly', 'filename', 'modified' } } };
    component_function = { gitbranch = 'fugitive#head', };
}
-- END

-- START config vim-test
vim.api.nvim_set_keymap("t", "jk", '<C-\\><C-n>', {noremap = true}) -- to be able to use jk on test result buffer
vim.api.nvim_set_keymap("n", "<Leader>mts", "<cmd>:TestNearest -strategy=neovim<CR>", {noremap = true})
vim.api.nvim_set_keymap("n", "<Leader>mta", "<cmd>:TestFile -strategy=neovim<CR>", {noremap = true})
vim.api.nvim_set_keymap("n", "<Leader>mtt", "<cmd>:TestLast -strategy=neovim<<CR>", {noremap = true})
-- END

-- START config fugitive
vim.api.nvim_set_keymap("n", "<Leader>gg", "<cmd>:Git<cr>", {noremap = true})
-- END

-- START config treesitter
require('nvim-treesitter.configs').setup({
  ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  highlight = {
    enable = true,              -- false will disable the whole extension
  },
})
-- END

-- START config nvim-autopairs
require('nvim-autopairs').setup()
-- END
