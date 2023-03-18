{ pkgs, ... }:
let
  Coqtail-coq-lsp = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "Coqtail";
    version = "47846d5e59c6b810bd3126543a430e09362bb5e7";
    src = pkgs.fetchFromGitHub {
      owner = "whonore";
      repo = "Coqtail";
      rev = "47846d5e59c6b810bd3126543a430e09362bb5e7";
      sha256 = "sha256-6Lj2mKyWgeFfd6NgGkOnBl+oMwzhbOkSVyadK3kH6AM=";
    };
  };
  coq-lsp = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "coq-lsp";
    version = "b387455d3f2801b0cf59dd10ddd891318a1a0126";
    src = pkgs.fetchFromGitHub {
      owner = "tomtomjhj";
      repo = "coq-lsp.nvim";
      rev = "b387455d3f2801b0cf59dd10ddd891318a1a0126";
      sha256 = "sha256-y28nOR7WSThcPi1X//JQRQuAhpMXe3VeBswx6dUW7+g=";
    };
  };
in
{
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    plugins = with pkgs.vimPlugins; [
      ale

      vim-lsp
      nvim-lspconfig
      nvim-lsputils

      coq-lsp
      Coqtail

      rust-tools-nvim

      (nvim-treesitter.withPlugins (
        plugins: with plugins; [
          rust
          toml
        ]
      ))

      vim-nix

      vimtex

      direnv-vim

      nerdtree

      telescope-dap-nvim
      nvim-dap
      plenary-nvim
      nvim-dap-ui

      cmp-buffer
      cmp-nvim-lsp
      cmp-nvim-lsp-signature-help
      cmp-nvim-lua
      cmp-path
      cmp-vsnip
      nvim-cmp
      vim-vsnip

      vim-airline
      vim-airline-themes
      nord-nvim

      which-key-nvim

      vim-bufferline

      vim-commentary

      vim-fugitive

      vim-gitgutter

      undotree
    ];
    extraConfig = ''
      set number
      set cursorline
      set expandtab
      set tabstop=4
      set hlsearch
      set foldlevel=99
      set nocompatible
      filetype on
      filetype plugin on
      filetype indent on
      syntax on
      set splitright
      set splitbelow
      set termguicolors
      set clipboard+=unnamedplus

      let g:mapleader=';'
      let g:maplocalleader = ','
      inoremap ;; <Esc>

      colorscheme nord
      let g:airline_theme='violet'

      nnoremap <silent> <Space> :NERDTreeToggle<CR>
      nnoremap <C-T> :terminal<CR>
      nnoremap <C-U> :UndotreeToggle<CR>
      nnoremap <M-;> :Commentary<CR>
      nnoremap <C-J> <C-W><C-J>
      nnoremap <C-K> <C-W><C-K>
      nnoremap <C-L> <C-W><C-L>
      nnoremap <C-H> <C-W><C-H>
      nnoremap <C-Q> :bdel<CR>
      nnoremap <Space>o :only<CR>

      let g:LanguageClient_serverCommands = {
      \ 'rust': ['rust-analyzer'],
      \ }
      let g:ale_linters = {'rust': ['analyzer']}

      let g:vimtex_quickfix_open_on_warning = 0
      let g:vimtex_compiler_latexmk = {
        \ 'executable' : 'latexmk',
        \ 'options' : [
        \   '-xelatex',
        \   '-file-line-error',
        \   '-synctex=1',
        \   '-interaction=nonstopmode',
        \ ],
        \}
      let g:vimtex_view_method = 'zathura'

      let g:loaded_coqtail = 1
      let g:coqtail#supported = 0

      lua << EOF
        local wk = require("which-key")
        wk.register(mappings, opts)

        local coqlsp = require("coq-lsp")
        coqlsp.setup({
          -- configuration for coq-lsp.nvim
          coq_lsp_nvim = {
            -- to be added
          },
          -- configuration forwarded `:help lspconfig-setup`
          lsp = {
            on_attach = function(client, bufnr)
              -- your mappings, etc
              vim.keymap.set("n", "<Leader>v", coqlsp.panels, { buffer = bufnr })
              vim.keymap.set("n", "<Leader>s", coqlsp.stop, { buffer = bufnr })
            end,
            init_options = {
              show_notices_as_diagnostics = true,
            },
          },
        })

        local rt = require("rust-tools")

        rt.setup({
          tools = {
            debuggables = {
              use_telescope = true
            },
          },
          server = {
            on_attach = function(_, bufnr)
            -- Hover actions
            vim.keymap.set("n", "<C-space>", rt.hover_actions.hover_actions, { buffer = bufnr })
            -- Code action groups
            vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
            vim.keymap.set("n", "gd", vim.lsp.buf.definition, { buffer = bufnr })
            vim.keymap.set("n", "gD", vim.lsp.buf.implementation, { buffer = bufnr })
          end,
          -- settings = {
          --  ["rust-analyzer"] = {
          --      checkOnSave = {
          --          command = "clippy"
          --      },
          --  },
          -- }
          },
          dap = {
            adapter = require('rust-tools.dap').get_codelldb_adapter("${pkgs.vscode-extensions.vadimcn.vscode-lldb}/share/vscode/extensions/vadimcn.vscode-lldb/adapter/codelldb", "${pkgs.vscode-extensions.vadimcn.vscode-lldb}/share/vscode/extensions/vadimcn.vscode-lldb/lldb/lib/liblldb.so"),
          },
        })

      local dap = require('dap')
      dap.adapters.codelldb = {
        type = 'server',
        port = "13000",
        executable = {
          command = "${pkgs.vscode-extensions.vadimcn.vscode-lldb}/share/vscode/extensions/vadimcn.vscode-lldb/adapter/codelldb",
          args = {"--port", "13000"},
        }
      }

      dap.configurations.rust = {
        {
          type = 'codelldb',
          showDisassembly = "never",
          request = 'launch',
          program = function()
            return vim.fn.input('Path to executable: ', vim.fn.getcwd()..'/target/debug/', 'file')
          end,
          cwd = "''${workspaceFolder}",
          terminal = 'integrated',
          sourceLanguages = { 'rust' },
          stopOnEntry = true
        }
                              }

      local dapui = require("dapui")
      dapui.setup()

      dap.listeners.after.event_initialized["dapui_config"] = function()
        dapui.open()
      end
      dap.listeners.before.event_terminated["dapui_config"] = function()
        dapui.close()
      end
      dap.listeners.before.event_exited["dapui_config"] = function()
        dapui.close()
      end

      vim.keymap.set("n", "<leader>dk", function() dap.continue() end)
      vim.keymap.set("n", "<leader>dl", function() dap.run_last() end)
      vim.keymap.set("n", "<leader>b", function() dap.toggle_breakpoint() end)


      -- LSP Diagnostics Options Setup
      local sign = function(opts)
        vim.fn.sign_define(opts.name, {
          texthl = opts.name,
          text = opts.text,
          numhl = '''
        })
      end

      sign({name = 'DiagnosticSignError', text = ''})
      sign({name = 'DiagnosticSignWarn', text = ''})
      sign({name = 'DiagnosticSignHint', text = ''})
      sign({name = 'DiagnosticSignInfo', text = ''})

      vim.diagnostic.config({
          virtual_text = false,
          signs = true,
          update_in_insert = true,
          underline = true,
          severity_sort = false,
          float = {
              border = 'rounded',
              source = 'always',
              header = ''',
              prefix = ''',
          },
      })

      vim.cmd([[
      set signcolumn=yes
      autocmd CursorHold * lua vim.diagnostic.open_float(nil, { focusable = false })
      ]])

      vim.keymap.set("n", "g[", vim.diagnostic.goto_prev, keymap_opts)
      vim.keymap.set("n", "g]", vim.diagnostic.goto_next, keymap_opts)

      --Set completeopt to have a better completion experience
      -- :help completeopt
      -- menuone: popup even when there's only one match
      -- noinsert: Do not insert text until a selection is made
      -- noselect: Do not select, force to select one from the menu
      -- shortness: avoid showing extra messages when using completion
      -- updatetime: set updatetime for CursorHold
      vim.opt.completeopt = {'menuone', 'noselect', 'noinsert'}
      vim.opt.shortmess = vim.opt.shortmess + { c = true}
      vim.api.nvim_set_option('updatetime', 300)

      -- Fixed column for diagnostics to appear
      -- Show autodiagnostic popup on cursor hover_range
      -- Goto previous / next diagnostic warning / error
      -- Show inlay_hints more frequently
      vim.cmd([[
      set signcolumn=yes
      autocmd CursorHold * lua vim.diagnostic.open_float(nil, { focusable = false })
      ]])

      -- Completion Plugin Setup
      local cmp = require'cmp'
      cmp.setup({
        -- Enable LSP snippets
        snippet = {
          expand = function(args)
              vim.fn["vsnip#anonymous"](args.body)
          end,
        },
        mapping = {
          ['<C-p>'] = cmp.mapping.select_prev_item(),
          ['<C-n>'] = cmp.mapping.select_next_item(),
          -- Add tab support
          ['<S-Tab>'] = cmp.mapping.select_prev_item(),
          ['<Tab>'] = cmp.mapping.select_next_item(),
          ['<C-S-f>'] = cmp.mapping.scroll_docs(-4),
          ['<C-f>'] = cmp.mapping.scroll_docs(4),
          ['<C-Space>'] = cmp.mapping.complete(),
          ['<C-e>'] = cmp.mapping.close(),
          ['<CR>'] = cmp.mapping.confirm({
            behavior = cmp.ConfirmBehavior.Insert,
            select = true,
          })
        },
        -- Installed sources:
        sources = {
          { name = 'path' },                              -- file paths
          { name = 'nvim_lsp', keyword_length = 3 },      -- from language server
          { name = 'nvim_lsp_signature_help'},            -- display function signatures with current parameter emphasized
          { name = 'nvim_lua', keyword_length = 2},       -- complete neovim's Lua runtime API such vim.lsp.*
          { name = 'buffer', keyword_length = 2 },        -- source current buffer
          { name = 'vsnip', keyword_length = 2 },         -- nvim-cmp source for vim-vsnip
          { name = 'calc'},                               -- source for math calculation
        },
        window = {
            completion = cmp.config.window.bordered(),
            documentation = cmp.config.window.bordered(),
        },
        formatting = {
            fields = {'menu', 'abbr', 'kind'},
            format = function(entry, item)
                local menu_icon ={
                    nvim_lsp = 'λ',
                    vsnip = '⋗',
                    buffer = 'Ω',
                    path = '🖫',
                }
                item.menu = menu_icon[entry.source.name]
                return item
            end,
        },
      })

      require('nvim-treesitter.configs').setup {
        highlight = {
          enable = true,
          additional_vim_regex_highlighting=false,
        },
        ident = { enable = true },
        rainbow = {
          enable = true,
          extended_mode = true,
          max_file_lines = nil,
        }
      }

      vim.wo.foldmethod = 'expr'
      vim.wo.foldexpr = 'nvim_treesitter#foldexpr()'

      local format_sync_grp = vim.api.nvim_create_augroup("Format", {})
      vim.api.nvim_create_autocmd("BufWritePre", {
        pattern = "*.rs",
        callback = function()
          vim.lsp.buf.format({ timeout_ms = 200 })
        end,
        group = format_sync_grp,
      })
      EOF
    '';
  };
}
