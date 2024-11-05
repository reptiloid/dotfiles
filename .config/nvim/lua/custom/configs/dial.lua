local status_ok, augend = pcall(require, 'dial.augend')
if not status_ok then
  return
end

require('dial.config').augends:register_group {
  -- default augends used when no group name is specified
  default = {
    augend.integer.alias.decimal, -- nonnegative decimal number (0, 1, 2, 3, ...)
    augend.constant.alias.bool, -- boolean value (true <-> false)
    augend.integer.alias.hex, -- nonnegative hex number  (0x01, 0x1a1f, etc.)
    augend.date.alias['%Y/%m/%d'], -- date (2022/02/19, etc.)
    augend.constant.new {
      elements = { 'and', 'or' },
      word = true, -- if false, "sand" is incremented into "sor", "doctor" into "doctand", etc.
      cyclic = true, -- "or" is incremented into "and".
    },
    augend.constant.new { elements = { 'True', 'False' }, word = true, cyclic = true },
  },

  -- augends used when group with name `mygroup` is specified
  mygroup = {
    augend.integer.alias.decimal,
    augend.constant.alias.bool, -- boolean value (true <-> false)
    augend.date.alias['%m/%d/%Y'], -- date (02/19/2022, etc.)
  },
}

vim.api.nvim_set_keymap('n', '<C-a>', require('dial.map').inc_normal(), { noremap = true })
vim.api.nvim_set_keymap('n', '<C-x>', require('dial.map').dec_normal(), { noremap = true })
