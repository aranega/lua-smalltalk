local st2lua = require("st2lua")
local inspect = require("inspect")

local parser = st2lua.Parser.new()

local txt = [[
timesRepeat
    Transcript show: 'OK'; show: i printString; cr
]]

print(inspect(parser:parse(txt)))
print(st2lua.compile(txt).code)
