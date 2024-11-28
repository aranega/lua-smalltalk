local st = require("object_model")
-- local dbg = require("debugger")

local is_luajit = (type(jit) == "table")

local function chain(from)
	setmetatable(from, { __index = _G })
	return setmetatable({}, { __index = from })
end
if is_luajit then
	setfenv(1, chain(st))
else
	_ENV = chain(st)
end

X = Class:subclass("X")
X:addMethod("a", function(self)
	print("In a")
	local b = (Block:create(function()
		print("BLOCK EXEC")
		return_("BLOCKRETURN")
	end))
	self:b(b)
	print("In a, end (should not display)")
end)
X:addMethod("b", function(self, ablock)
	print("In b")
	self:c(ablock)
	print("In b, after value (should not display)")
end)
X:addMethod("c", function(self, ablock)
	print("In c")
	ablock:value()
	print("In c, after value (should not display)")
end)

local x = X:new()
print(x:a())

X:addStMethod(
	[[
d
    ^ self isKindOf: X
]],
	{ debug = false }
)

print(x:d())

local function wrap(obj)
	if type(obj) == "table" and obj.__st then
		return obj
	end
	if obj == nil then
		return World["nil"]
	elseif obj == true then
		return World["true"]
	elseif obj == false then
		return World["false"]
	elseif type(obj) == "number" then
		return World.Integer:create(obj)
	end
end

World.Integer:addMethod("factorial", function(self)
	self:_BIN_EQ(World["0"]):ifTrue_((Block:create(function()
		return_(World["1"])
	end)))
	self:_BIN_SUP(World["0"]):ifTrue_((Block:create(function()
		return_(self:_BIN_STAR((self:_BIN_MIN(World["1"])):factorial()))
	end)))
	return self
end)
World.Integer:addMethod("_BIN_EQ", function(self, other)
	return wrap(self.value == other.value)
end)
World.Integer:addMethod("_BIN_SUP", function(self, other)
	return wrap(self.value > other.value)
end)
World.Integer:addMethod("_BIN_STAR", function(self, other)
	return wrap(self.value * other.value)
end)
World.Integer:addMethod("_BIN_MIN", function(self, other)
	return wrap(self.value - other.value)
end)
World.Integer:addMethod("_BIN_INFEQ", function(self, other)
	return wrap(self.value <= other.value)
end)

World.Integer:addStMethod(
	[[
factorial
	self = 0 ifTrue: [ ^ 1].
	self > 0 ifTrue: [ ^ self * (self - 1) factorial ].
]],
	{ debug = true }
)

print(World["10"]:factorial())

World.Integer:addStMethod(
	[[
factorialWithAccumulator: acc
	self = 0 ifTrue: [ ^ acc ].
	self > 0 ifTrue: [ ^ (self - 1) factorialWithAccumulator: acc * self ].
]],
	{ debug = false }
)

print(World["127"]:factorialWithAccumulator_(World["1"]))
print(World["127"]:factorial())

World.Integer:addStMethod([[
fibonacci
    self = 0 ifTrue: [ ^ 0 ].
    self = 1 ifTrue: [ ^ 1 ].
    self > 1 ifTrue: [ ^ (self - 1) fibonacci + (self - 2) fibonacci ].
    self < 0 ifTrue: [ self error: 'Fibonacci is not defined for negative numbers' ].
]])

World.Integer:addStMethod(
	[[
timesRepeat: aBlock
	| count |
	count := 1.
	[count <= self]
		whileTrue:
			[ aBlock value.
			count := count + 1.
			count ]
]],
	{ debug = false }
)

X:addStMethod(
	[[
test
| CC cc |
	CC := Object subclass: 'CC'.

	cc := CC new.

	Transcript show: cc; cr.

	^ cc
]],
	{ debug = false }
)

print(x:test())

local X = Object:subclass("X")
local x = X:new()
print(x)  -- displays "a X"

X:addMethod("m1_", function(self, other)
    print("Hello from Lua", other)
end)

X:addStMethod([[
m2: other
    Transcript show: 'Hello from Smalltalk (actually Lua) '.
    Transcript show: other; cr
]], { debug = true })

X.class:addMethod("create", function (self)
    return self:new()
end)

local x2 = X:create()
x2:m1_('in Lua')
x2:m2_('in Lua-Smalltalk')