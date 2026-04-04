---@diagnostic disable:unused-local
--# selene: allow(unused_variable)
local ls = require "luasnip"
local s = ls.snippet
local sn = ls.snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local d = ls.dynamic_node
local c = ls.choice_node

return {
  -- s({ trig = "=", dscr = "Eruby insert tag" }, {
  --   t "<%= ",
  --   i(0),
  --   t " %>",
  -- }),
}
