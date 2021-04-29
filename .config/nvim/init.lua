-- START my vim general config
---- Remap leader to Space
vim.g.mapleader = ' ' -- <Space> to be leader shortcut
vim.api.nvim_set_keymap('i', 'jk', '<ESC>', {noremap = true}) -- to exit to normal mode
vim.api.nvim_set_keymap('i', '<C-g>', '<ESC>', {noremap = true}) -- <C-g> to exit to normal mode
vim.api.nvim_set_keymap('x', '<C-g>', '<ESC>', {noremap = true}) -- <C-g> to clear visual select
vim.api.nvim_set_keymap('n', '<C-g>', '<cmd>:noh<cr>', {noremap = true}) -- <C-g> to clear highlight
----
vim.o.splitright = true -- always create new split windows on right side
vim.o.splitbelow = true -- always create new split windows on bottom
vim.o.relativenumber = true -- use relative line number
vim.o.number = true -- show absolute line number of the current line
vim.o.undofile = true -- keep undo history
vim.o.tabstop = 4 -- set tab size to 4 space
vim.o.clipboard = 'unnamed' -- make yank and put working with macOS native copy and paste
vim.api.nvim_exec('let loaded_matchparen = 1', false) -- remove highlight on matching parentheses
---- Hightlight on yank
vim.api.nvim_exec([[
  augroup YankHighlight
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank()
  augroup end
]], false)
----
---- Y yank until the end of line
vim.api.nvim_set_keymap('n', 'Y', 'y$', { noremap = true})
----
---- Remap for dealing with word wrap
vim.api.nvim_set_keymap('n', 'k', "v:count == 0 ? 'gk' : 'k'", { noremap=true, expr = true, silent = true})
vim.api.nvim_set_keymap('n', 'j', "v:count == 0 ? 'gj' : 'j'", {noremap= true, expr = true, silent = true})
----
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

        -- better lsp diagnostic buffer
        use {
            "folke/lsp-trouble.nvim",
            requires = "kyazdani42/nvim-web-devicons",
            config = function()
                require("trouble").setup({})
            end
        }

        -- for using custom linters
        use 'mfussenegger/nvim-lint'

        -- dracula theme
        use 'folke/tokyonight.nvim'

    end
)
-- END

-- START config LSP
local nvim_lsp = require('lspconfig')

local on_attach = function(client, bufnr)
    local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end

    -- Mappings.
    local opts = { noremap=true, silent=true }
    buf_set_keymap('n', '<Leader>cD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
    buf_set_keymap('n', '<Leader>cd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
    buf_set_keymap('n', 'gh', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
    buf_set_keymap('n', '<Leader>ci', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
    buf_set_keymap('n', '<Leader>cr', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
    buf_set_keymap('n', '[e', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
    buf_set_keymap('n', ']e', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)

    -- Set some keybinds conditional on server capabilities
    if client.resolved_capabilities.document_formatting then
        buf_set_keymap("n", "<space>cf", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
    end


end

-- Use a loop to conveniently both setup defined servers
-- and map buffer local keybindings when the language server attaches
local servers = { "tsserver", "dartls" }
for _, lsp in ipairs(servers) do
    nvim_lsp[lsp].setup { on_attach = on_attach }
end

nvim_lsp.gopls.setup({
  on_attach = on_attach,
  settings = {
    gopls = {
      buildFlags = {"-tags=integration"}
    },
  },
})

local system_name
if vim.fn.has("mac") == 1 then
  system_name = "macOS"
elseif vim.fn.has("unix") == 1 then
  system_name = "Linux"
elseif vim.fn.has('win32') == 1 then
  system_name = "Windows"
else
  print("Unsupported system for sumneko")
end

-- set the path to the sumneko installation; if you previously installed via the now deprecated :LspInstall, use
local sumneko_root_path = '/Users/mek.kiatkrai/lua-language-server'
local sumneko_binary = sumneko_root_path.."/bin/"..system_name.."/lua-language-server"

nvim_lsp.sumneko_lua.setup {
    cmd = {sumneko_binary, "-E", sumneko_root_path .. "/main.lua"};
    settings = {
        Lua = {
            runtime = {
                -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
                version = 'LuaJIT',
                -- Setup your lua path
                path = vim.split(package.path, ';'),
            },
            diagnostics = {
                -- Get the language server to recognize the `vim` global
                globals = {'vim'},
            },
            workspace = {
                -- Make the server aware of Neovim runtime files
                library = {
                    [vim.fn.expand('$VIMRUNTIME/lua')] = true,
                    [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true,
                },
            },
            -- Do not send telemetry data containing a randomized but unique identifier
            telemetry = {
                enable = false,
            },
        },
    },
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
vim.api.nvim_set_keymap('n', '<Leader>bi', '<cmd>:Telescope buffers<cr>', {noremap = true})
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
    preselect = 'always';
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
vim.api.nvim_set_keymap('c', '<C-j>', '<C-n>', {noremap = true} )
vim.api.nvim_set_keymap('c', '<C-k>', '<C-p>', {noremap = true} )
vim.api.nvim_set_keymap('s', '<C-j>', '<Tab>', {noremap = true} )
vim.api.nvim_set_keymap('s', '<C-k>', '<S-Tab>', {noremap = true} )
-- END

-- START config vim-vsnip
vim.api.nvim_exec("inoremap <silent><expr> <CR>      compe#confirm('<CR>')", false)
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
vim.api.nvim_set_keymap("n", "<Leader>mtt", "<cmd>:TestLast -strategy=neovim<CR>", {noremap = true})
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

-- START config lsp-trouble
vim.api.nvim_set_keymap('n', '<Leader>ce', '<cmd>:LspTroubleToggle<CR>', {noremap = true})
-- END

-- START config lint
require('lint').linters_by_ft = {
  go = {'golangcilint',}
}
vim.api.nvim_exec("au FileType go au BufWritePost <buffer> lua require('lint').try_lint()", false) -- auto run golangci-lint on *.go file
-- END

-- START config theme
vim.g.tokyonight_style = "night"
vim.cmd[[colorscheme tokyonight]]
-- END
