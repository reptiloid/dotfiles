-- You can add your own plugins here or in other files in this directory!
--  I promise not to create any merge conflicts in this directory :)
--
-- See the kickstart.nvim README for more information
return {
  {
    'max397574/better-escape.nvim',
    event = 'InsertEnter',
    config = function()
      require('better_escape').setup()
    end,
  },

  {
    'rhysd/clever-f.vim',
    enabled = true,
    lazy = false,
    -- event = "InsertEnter",
    config = function()
      require 'custom.configs.clever-f'
    end,
  },

  {
    'monaqa/dial.nvim',
    lazy = false,
    config = function()
      require 'custom.configs.dial'
    end,
  },

  { 'gorkunov/smartpairs.vim', lazy = false },

  { 'tpope/vim-surround', lazy = false },

  {
    'echasnovski/mini.completion',
    version = false,
    config = function()
      require('mini.completion').setup()
    end,
  },

  { 'tmhedberg/SimpylFold', lazy = false },

  {
    'windwp/nvim-autopairs',
    event = 'InsertEnter',
    config = true,
    -- use opts = {} for passing setup options
    -- this is equivalent to setup({}) function
  },
}
