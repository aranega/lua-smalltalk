# lua-smalltalk

Lua-Smalltalk is a small bootstrapped extensible Smalltalk object model implemented in Lua.

This project is a small proof-of-concept/experiment to try to patially map Smalltalk object model to the table/metatable model of Lua, as well as to give the possibility to experiment with small kernels. It implements a simple metaclass/class model over metatables with a small API to let you easily create object/classes in Lua. It includes a first extremely minimal Kernel where methods are either coded in Smalltalk or in Lua code.

The project also includes a Smalltalk parser written in Lua, and a Smalltalk to Lua compiler that uses the Lua API defined to manipulate the object model (a little bit like the C API for object manipulation in CPython). Consequently, there is no bytecode interpreter or AST interpreter, the Smalltalk code is directly compiled in Lua, which is compiled then in Lua-bytecode and executed by the Lua-VM.

All works with Lua and Lua-JIT, but it's recommended to use Lua-JIT, which seems to deal better with deep recursion (or there is a problem in the non-local return implementation).

Currently is supported:

* classes/metaclasses
* add methods coded in Lua in classes (class-side methods are supported also)
* add methods coded in Smalltalk in classes (class-side methods are supported also)
* Smalltalk lookup, and primitive `doesNotUnderstand` support
* super (using a dedicated function)
* cascade (using a dedicated function)
* non-local return (using a dedicated function)
* blocks
* Smalltalk to Lua compiler

What's not yet supported:

* instance variable access from Smatallk code
* optimized Smalltalk code compilation, currently, every message send is compiled as Lua method call (no `ifTrue:ifFalse:` rewritten for example)
* debugger
* threads

One of the goal would also be to add a minimal graphical interface to extend/code directly the kernel in a Smalltalk environment with support for debugging.


## Code Organization

The code is currently organized as such:

* `object_model.lua` contains the basis for the object model and the basic Lua API for the object model, as well as a first very minimal kernel.
* `st2lua.lua` contains the Smalltalk parser and the Smalltalk to Lua compiler.
* `test_parser1.lua` is a simple small script I use to debug the parser. This script relies on `inspect.lua` (not included here)
* `exp.lua` that is just some examples that I use to test the object model

## Use the Object Model in Lua

The implementation is still rough on the edges, but it's going step by step towards something easier to use.
To use the object model in Lua requires to get access to `Object` or any class from the kernel in the first place. Those can be accessed from `World` which contains the classes as well as some literals (`nil`, `true`, `false`, ...).

The simplest way to integrate the object model and have a direct access to the functions is to chain the project to the global env:

```lua
 local st = require("object_model")

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
```

Once it's loaded, here is the way to create a simple class and an instance:

```lua
local X = Object:subclass("X")
local x = X:new()
print(x)  -- displays "a X"
```

With a class created, it's possible to add methods in Lua, in Smalltalk, and to add some in the class-side:

```lua
X:addMethod("m1_", function(self, other)
    print("Hello from Lua", other)
end)

X:addStMethod([[
m2: other
    Transcript show: 'Hello from Smalltalk (actually Lua) '.
    Transcript show: other; cr
]], { debug = true})  -- the "debug = true" option displays the compiled Lua code

X.class:addMethod("create", function (self)
    return self:new()
end)

local x2 = X:create()
x2:m1_('in Lua')
x2:m2_('in Lua-Smalltalk')  -- keyword names are translated automatically by replacing ":" by "_" in the selector
```

Binary selectors and keyword selectors are translated automatically. To use them from Lua, you can replace `:` by `_` for keyword selectors, or use the `translate_selector(...)` function that works for binary selectors and keyword selectors.

Here is how is defined the boolean class and some methods over it:

```lua
Boolean = Object:subclass("Boolean")
True = Boolean:subclass("True")
False = Boolean:subclass("False")
True:addStMethod([[
isTrue
    ^ self
]])
True:addStMethod([[
isFalse
    ^ false
]])
True:addStMethod([[
ifTrue: trueBlock ifFalse: falseBlock
    ^ trueBlock value
]])
```

The function `super(...)` is used to implement `super`. As example, here is how `printString` is defined for `Method`:

```lua
Method:addMethod("printString", function(self)
	return super(self, "printString")() .. " named " .. self.name
end)
```

To deal with non-local return, inside of a `Block`, the `return_(...)` function needs to be used:

```lua
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
-- => Displays
-- In a
-- In b
-- In c
-- BLOCK EXEC
-- BLOCKRETURN
```

Here is how to extend the kernel classes with some methods:

```lua
World.Integer:addStMethod([[
factorial
	self = 0 ifTrue: [ ^ 1].
	self > 0 ifTrue: [ ^ self * (self - 1) factorial ].
]])
```

And here is how this code is compiled in Lua. We can see that the access to literals is done through `World`, which is mandatory to gain access to the API exposed by the `Integer` inside of the language kernel.

```lua
function factorial(self)
	self:_BIN_EQ(World["0"]):ifTrue_((Block:create(function()
		return_(World["1"])
	end)))
	self:_BIN_SUP(World["0"]):ifTrue_((Block:create(function()
		return_(self:_BIN_STAR((self:_BIN_MIN(World["1"])):factorial()))
	end)))
	return self
end
```
