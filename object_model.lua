-- local dbg = require("debugger")
-- local inspect = require("inspect")
local st2lua = require("st2lua")

local function _c(self, foo, ...)
	self[foo](self, ...)
	return self
end

local World = {
	_c = _c,
}
World.World = World

local function lookup(start, key, super)
	local superclass
	-- local isSuper = rawget(start, "__super")

	-- print("START", inspect(start, {depth=1}))
	-- print("LOOKING FOR", key)
	if super then
		superclass = start.class.superclass
	else
		superclass = start.class
	end
	while superclass ~= World["nil"] do
		-- print("ASKING TO", inspect(superclass, {depth=1}))

		local res = rawget(superclass, key)
		if res then
			-- print("FOUND SELF IS", inspect(start, {depth=2}))
			return res
		end
		-- print("NO LUCK")
		superclass = rawget(superclass, "superclass")
	end
	return start:doesNotUnderstand(key)
end

-- local function ssuper(self)
-- 	local wrap = { __self = self }
-- 	setmetatable(wrap, {
-- 		__index = function(t, k)
-- 			return lookup(self, k, true)
-- 		end,
-- 		__newindex = self,
-- 	})
-- 	return wrap
-- end

local function super(t, key)
	local res = lookup(t, key, true)
	if res then
		return function(...)
			return res(t, ...)
		end
	end
	return nil
end
World.super = super

local function copy(t)
	local u = {}
	for k, v in pairs(t) do
		u[k] = v
	end
	return setmetatable(u, getmetatable(t))
end

local bootstrapped = false
local basic_allocate_object

local ActiveContexts = {
	stack = {},
	current = nil,
	push = function(self, ctx)
		table.insert(self.stack, ctx)
		self.current = ctx
	end,
	pop = function(self)
		table.remove(self.stack, #self.stack)
		self.current = self.stack[#self.stack]
	end,
}

local function str_context(ctx)
	return tostring(ctx)
		.. " <- "
		.. tostring(ctx.sender)
		.. "["
		.. tostring(ctx.receiver)
		.. "] ("
		.. tostring(ctx.method)
		.. ")"
end

local function print_full_stack()
	for i, ctx in ipairs(ActiveContexts.stack) do
		print(i, str_context(ctx))
	end
end

local LuaContext
LuaContext = {
	new = function(self, sender, receiver, method, closureOrNil)
		if type(closureOrNil) and closureOrNil.class.name == "Block" then
		else
			closureOrNil = nil
		end
		local instance = {
			sender = sender,
			receiver = receiver,
			method = method,
			closureOrNil = closureOrNil,
		}
		setmetatable(instance, { __index = LuaContext })
		return instance
	end,
	wrap = function(self)
		local ctx = Context:new()
		if self.sender == nil then
			ctx._sender = World["nil"]
		else
			ctx._sender = self.sender
		end
		ctx._receiver = self.receiver
		ctx._method = self.method
		ctx._closureOrNil = self.closureOrNil
		return ctx
	end,
}

-- Add non-local return function
local function return_(value)
	local currentContext = ActiveContexts.current
	if not currentContext or not currentContext.closureOrNil or not currentContext.closureOrNil._outerContext then
		error("Cannot return from here - no enclosing context found")
	end
	error({
		__nlr = true,
		value = value,
		targetContext = currentContext.closureOrNil._outerContext,
	})
end
World.return_ = return_

local function run_method(method, self, ...)
	-- if bootstrapped then
	-- local context = basic_allocate_object({
	-- 	class = Context,
	-- 	sender = ActiveContexts.current,
	-- 	receiver = self,
	-- 	method = method,
	-- 	closureOrNil = World["nil"]
	-- })
	local ctx = LuaContext:new(ActiveContexts.current, self, method, self)
	ActiveContexts:push(ctx)
	-- end
	local res = method.body(self, ...)
	ActiveContexts:pop()
	return res
end

local general_metatable = {
	__index = lookup,
	__tostring = function(t)
		return t:printString()
	end,
	__call = run_method,
}

-- used for reference, the nil values are not inserted in the table
local class_layout
class_layout = {
	layout = class_layout,
	superclass = nil, -- tmp
	class = nil, -- tmp
	name = nil, -- tmp
}
setmetatable(class_layout, general_metatable)

-- used for reference, the nil values are not inserted in the table
local object_layout = {
	class = nil, -- tmp
}
setmetatable(object_layout, general_metatable)

local function basic_allocate(layout, options)
	local instance = copy(layout)
	for key, value in pairs(options) do
		instance[key] = value
	end
	instance.__st = true
	return instance
end

local function basic_allocate_class(name, options)
	options.name = name
	local class_instance = basic_allocate(class_layout, options)
	World[name] = class_instance
	return class_instance
end

function basic_allocate_object(options)
	return basic_allocate(object_layout, options)
end

Object = basic_allocate_class("Object", {
	superclass = nil, -- tmp, set to World["nil"] later
	isKindOf = function(self, aClass)
		return self.class == aClass or self.class:inheritsFrom(aClass)
	end,
	inheritsFrom = function(self, aClass)
		local aSuperClass = self.superclass
		while aSuperClass ~= World["nil"] do
			if aSuperClass == aClass then
				return true
			end
			aSuperClass = aSuperClass.superclass
		end
		return false
	end,
	initialize = function(self)
		return self
	end,
	printString = function(self)
		return "a " .. self.class.name
	end,
	doesNotUnderstand = function(self, key, params)
		print("Does not understand", key, "for", self)
		return World["nil"]
	end,
	perform = function(self, key, ...)
		local st_method_name = st2lua.translate_selector(key)
		return self[st_method_name]:perform(self, ...)
		-- return self[st_method_name](self, ...)
	end,
})

ClassDescription = basic_allocate_class("ClassDescription", {
	superclass = Object,
	class = nil, -- tmp set later

	-- primitive methods (can be rewritten later as stMethods)
	new = function(self)
		local instance = self:basicNew()
		instance:initialize()
		return instance
	end,
	basicNew = function(self)
		local instance = basic_allocate(self.layout, {
			class = self,
		})
		return instance
	end,
	subclass = function(self, name)
		local newMetaclass = Metaclass:new()
		newMetaclass.name = name .. " class"
		newMetaclass.superclass = self.class

		local newClass = newMetaclass:new()
		newClass.name = name
		newClass.superclass = self

		World[newClass.name] = newClass
		World[newMetaclass.name] = newMetaclass

		return newClass
	end,
	addMethod = function(self, st_name, body, pragmas)
		local method = Method:new()
		local lua_name = st2lua.translate_selector(st_name)
		method.name = lua_name
		method.body = body
		self[lua_name] = method
	end,
	addStMethod = function(self, body, opts)
		local method = st2lua.compile(body)

		if opts and opts.debug then
			print("Compiled method", method.lua_name, method.st_selector)
			print(method.code)
			print()
		end

		local compiled, err = load(method.code, method.lua_name, "t", World)

		local st_method = Method:new()
		st_method.name = method.st_selector
		st_method.body = compiled()

		self[method.lua_name] = st_method
	end,
})

Class = basic_allocate_class("Class", {
	superclass = ClassDescription,
	class = nil, -- tmp set later
	layout = object_layout,
})

Metaclass = basic_allocate_class("Metaclass", {
	superclass = ClassDescription,
	class = nil, -- tmp set later,
	layout = class_layout,
})

Nil = basic_allocate_class("Nil", {
	superclass = Object,
	name = "Nil",
	class = nil, -- tmp set later
	printString = function()
		return "mySmalltalkNil"
	end,
})

ObjectClass = basic_allocate_class("Object class", {
	superclass = Class,
	class = nil, -- tmp set later
})

ClassDescriptionClass = basic_allocate_class("ClassDescriptionClass class", {
	superclass = ObjectClass,
	class = nil, -- tmp set later
})

ClassClass = basic_allocate_class("Class class", {
	superclass = ClassDescriptionClass,
	class = nil, -- tmp set later
})

MetaclassClass = basic_allocate_class("Metaclass class", {
	superclass = ClassDescriptionClass,
	class = nil, -- tmp set later
	layout = class_layout,
})

NilClass = basic_allocate_class("Nil class", {
	superclass = ObjectClass,
	class = nil, -- tmp set later
})

local function bootstrap()
	Object.class = ObjectClass
	Class.class = ClassClass
	Metaclass.class = MetaclassClass
	ClassDescription.class = ClassDescriptionClass
	Nil.class = NilClass

	ObjectClass.class = Metaclass
	ClassClass.class = Metaclass
	MetaclassClass.class = Metaclass
	ClassDescriptionClass.class = Metaclass
	NilClass.class = Metaclass

	-- Create nil instance by hand
	local nil_instance = {
		class = Nil,
	}
	local nil_metatable = copy(general_metatable)
	nil_metatable.__call = function(t, ...)
		-- do nothing in this case
	end
	setmetatable(nil_instance, nil_metatable)
	World["nil"] = nil_instance
	Object.superclass = World["nil"]
end

bootstrap()

-- Create base kernel
-- Method class
Method = Object:subclass("Method")
Method:addMethod("printString", function(self)
	return super(self, "printString")() .. " named " .. self.name
end)
Method:addMethod("perform", function(self, ...)
	local receiver = ...
	local args = select(2, ...)
	return run_method(self, receiver, args)
end)

-- Context class
Context = Object:subclass("Context")
Context:addMethod("receiver", function(self)
	return self._receiver
end)
Context:addMethod("sender", function(self)
	if self._sender ~= World["nil"] then
		return self._sender:wrap()
	end
	return World["nil"]
end)

-- Shift functions as Method instance
local function shift_world_functions()
	local toshift = {}
	for _, obj in pairs(World) do
		if type(obj) == "table" and obj.__st and (obj:isKindOf(Class) or obj:isKindOf(Metaclass)) then
			table.insert(toshift, obj)
		end
	end
	for _, obj in ipairs(toshift) do
		for key, func in pairs(obj) do
			if type(func) == "function" then
				local nb_params = debug.getinfo(func, "u").nparams
				local selector = key
				if nb_params > 1 then
					selector = key .. "_"
				end
				obj:addMethod(selector, func)
			end
		end
	end
	bootstrapped = true
end
shift_world_functions()

-- Block class
Block = Object:subclass("Block")
Block.class:addMethod("create", function(self, block, args)
	local instance = self:new()
	instance.body = block
	instance.args = args
	if ActiveContexts.current then
		instance._outerContext = ActiveContexts.current.sender
	else
		instance._outerContext = World["nil"]
	end
	return instance
end)
Block:addMethod("value", function(self, ...)
	-- return self.body(...)
	return run_method(self, self, ...)
end)

local original_run_method = run_method
local function new_run_method(method, self, ...)
	local function handleNonLocalReturn(err)
		-- It's a non-local return, we force the error as result
		-- if type(err) == "table" and err.__nlr then
		-- 	return err
		-- end
		-- Not a non-local return, propagate the error
		-- error(err)
		-- print_full_stack()
		-- print("STACK SIZE", #ActiveContexts.stack, ActiveContexts.current.method)
		return err
	end
	local status, res = xpcall(function(...)
		return original_run_method(method, self, ...)
	end, handleNonLocalReturn, ...)
	if status == false then
		if type(res) == "table" and res.__nlr then
			if ActiveContexts.current == res.targetContext then
				-- print("Found contxt", str_context(ActiveContexts.current))
				-- print("Returning", res.value)
				ActiveContexts:pop()
				return res.value
			else
				-- print("Popping", str_context(ActiveContexts.current))
				ActiveContexts:pop()
				-- error(res)
			end
		end
		-- Not a non-local return that triggered the error
		-- we propagate it
		error(res)
	end
	return res
end

run_method = new_run_method
general_metatable.__call = run_method

Block:addMethod("outerContext", function(self, ...)
	if self._outerContext then
		return self._outerContext:wrap()
	end
	return self._outerContext
end)

Block:addStMethod(
	[[
whileTrue: body
    self value ifTrue: [ body value. self whileTrue: body ].
    ^ nil
]])

-- Boolean class and sub-classes
Boolean = Object:subclass("Boolean")
True = Boolean:subclass("True")
False = Boolean:subclass("False")
-- inject them in World to use them later
World["true"] = True:new()
World["false"] = False:new()

-- True:addMethod("isTrue", function (self) return self end)
-- True:addMethod("isFalse", function (self) return World["false"] end)
-- True:addMethod("ifTrue", function (self, block) return block:value() end)
-- True:addMethod("ifTrue_ifFalse", function (self, blockTrue, blockFalse) return blockTrue:value() end)
-- True:addMethod("ifFalse", function (self, block) return World["nil"] end)
-- False:addMethod("isFalse", function (self) return World["true"] end)
-- False:addMethod("isTrue", function (self) return self end)
-- False:addMethod("ifTrue", function (self, block) return World["nil"] end)
-- False:addMethod("ifTrue_ifFalse", function (self, blockTrue, blockFalse) return blockFalse:value() end)
-- False:addMethod("ifFalse", function (self, block) return block:value() end)
True:addMethod("printString", function(self)
	return "trueSt"
end)
False:addMethod("printString", function(self)
	return "falseSt"
end)
True:addStMethod([[
isTrue
    ^ self
]])
True:addStMethod([[
isFalse
    ^ false
]])
True:addStMethod([[
ifTrue: block
    ^ block value
]])
True:addStMethod([[
ifTrue: trueBlock ifFalse: falseBlock
    ^ trueBlock value
]])
True:addStMethod([[
ifFalse: block
    ^ nil
]])

False:addStMethod([[
isFalse
    ^ true
]])
False:addStMethod([[
isTrue
    ^ self
]])
False:addStMethod([[
ifTrue: block
    ^ nil
]])
False:addStMethod([[
ifFalse: block
    ^ block value
]])
-- False:addMethod("ifTrue_ifFalse_", function (self, blockTrue, blockFalse) return blockFalse:value() end)
False:addStMethod([[
ifTrue: trueBlock ifFalse: falseBlock
    ^ falseBlock value
]])

-- Literal, Integer, String classes
Literal = Object:subclass("Literal")
Literal:addMethod("printString", function(self)
	return tostring(self.value)
end)
Literal:addMethod("initialize", function(self)
	self.value = World["nil"]
end)
Literal.class:addMethod("create", function(self, x)
	if type(x) == "table" then
		return x
	end
	local v = World[x]
	if v == nil then
		local new = self:new()
		new.value = x
		World[x] = new
		v = new
	end
	return v
end)

Integer = Literal:subclass("Integer")
Integer:addMethod("+", function(self, x)
	return self.class:create(self.value + self.class:create(x).value)
end)
Integer.class:addMethod("create", function(self, x)
	if type(x) == "table" then
		return x
	end
	local v = super(self, "create")(x)
	v.value = tonumber(x)
	return v
end)

String = Literal:subclass("String")
String:addMethod(",", function(self, other)
	return self.class:create(self.value .. self.class:create(other).value)
end)

-- Allocate -127 .. 127 first integers
for i = -127, 128, 1 do
	Integer:create(tostring(i))
end

Transcript = Object:subclass("Transcript")
Transcript.class:addMethod("show:", function(self, str)
	io.write(tostring(str))
end)
Transcript.class:addMethod("cr", function()
	print()
end)

return World
