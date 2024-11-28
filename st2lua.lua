-- local inspect = require("inspect")

local Parser = {}

-- Tokenizer
local function tokenize(input)
	local tokens = {}
	local pos = 1
	local line = 1

	local patterns = {
		{ type = "newline", pattern = "^\n" },
		{ type = "whitespace", pattern = "^%s+" },
		{ type = "number", pattern = "^%-?%d+%.?%d+" },
		{ type = "number", pattern = "^%-?%d+" },
		{ type = "string", pattern = "^'[^']*'" },
		{ type = "symbol", pattern = "^#%w+" },
		{ type = "assignment", pattern = "^:=" },
		{ type = "cascade", pattern = "^;" },
		{ type = "return", pattern = "^%^" },
		{ type = "keyword", pattern = "^%w+:" },
		{ type = "blockarg", pattern = "^:%w+" },
		{ type = "pipe", pattern = "^|" },
		{ type = "openparenthesis", pattern = "^%(" },
		{ type = "closeparenthesis", pattern = "^%)" },
		{ type = "endexpression", pattern = "^%." },
		{ type = "openblock", pattern = "^%[" },
		{ type = "closeblock", pattern = "^%]" },
		{ type = "self", pattern = "^self$" },
		{ type = "super", pattern = "^super$" },
		{ type = "true", pattern = "^true$" },
		{ type = "false", pattern = "^false$" },
		{ type = "nil", pattern = "^nil$" },
		{ type = "identifier", pattern = "^[a-zA-Z_][a-zA-Z0-9_]*" },
		{ type = "binary_selector", pattern = "^[~!@%%&*%-+=|\\<>?,/]+" },
	}

	while pos <= #input do
		local matched = false
		for _, pattern in ipairs(patterns) do
			local s, e = input:find(pattern.pattern, pos)
			if s then
				local value = input:sub(s, e)
				if pattern.type == "newline" then
					pos = 1
					line = line + 1
				elseif pattern.type ~= "whitespace" and type ~= "newline" then
					table.insert(tokens, { type = pattern.type, value = value, pos = { char = pos, line = line } })
				end
				pos = e + 1
				matched = true
				break
			end
		end
		if not matched then
			error("Unexpected character at position " .. pos .. ": " .. input:sub(pos, pos))
		end
	end
	table.insert(tokens, { type = "EOF", pos = pos })
	return tokens
end

-- Parser implementation
function Parser.new()
	local instance = {
		current = 1,
		tokens = {},
	}

	local function peek(forward)
		local offset = forward or 0
		return instance.tokens[instance.current + offset]
	end

	local function fetch()
		instance.current = instance.current + 1
		return instance.tokens[instance.current - 1]
	end

	local function match(type)
		if peek() and peek().type == type then
			return fetch()
		end
		return nil
	end

	local function fatal(type, t)
		local pos = t.pos
		error(
			"["
				.. pos.line
				.. ", "
				.. pos.char
				.. "] Expected "
				.. type
				.. " but got '"
				.. t.value
				.. "' ("
				.. (t and t.type or "end of input")
				.. ")"
		)
	end

	function instance.parse(self, input)
		self.tokens = tokenize(input)
		self.current = 1

		return {
			type = "Method",
			header = self:parse_header(),
			localdefs = self:parse_localdefs(),
			body = self:parse_body(),
		}
	end

	local function expect(type)
		local token = match(type)
		if not token then
			fatal(type, peek())
		end
		return token
	end

	function instance.parse_header(self)
		local t = peek()
		if t.type == "identifier" then
			return self:parse_unary_method()
		end
		if t.type == "binary_selector" then
			return self:parse_binary_method()
		end
		if t.type == "keyword" then
			return self:parse_keyword_method()
		end
		fatal("identifier or binary selector or keyword", t)
	end

	function instance.parse_unary_method(self)
		local t = expect("identifier")
		return {
			selector = t.value,
			pos = t.pos,
			type = "UnaryMethod",
		}
	end

	function instance.parse_binary_method(self)
		local t = expect("binary_selector")
		local id = expect("identifier")
		return {
			selector = t.value,
			pos = t.pos,
			argument = id.value,
			type = "BinaryMethod",
		}
	end

	function instance.parse_keyword_method(self)
		local kws = {}
		local args = {}
		local kw = match("keyword")
		local pos = kw and kw.pos
		while kw do
			local arg = expect("identifier")
			table.insert(kws, kw.value:sub(1, #kw.value - 1))
			table.insert(args, arg.value)
			kw = match("keyword")
		end
		return {
			selector = table.concat(kws, ":") .. ":",
			kws = kws,
			pos = pos,
			args = args,
			type = "KeywordMethod",
		}
	end

	function instance.parse_localdefs(self)
		local t = match("pipe")
		if not t then
			return nil
		end
		local vars = {}
		while not match("pipe") do
			local id = expect("identifier")
			table.insert(vars, {
				name = id.value,
				pos = id.pos,
			})
		end
		return {
			vars = vars,
			type = "LocalVarDefinitions",
			pos = t.pos,
		}
	end

	function instance.parse_body(self, inblock)
		local stmts = {}
		local stmt = self:parse_statement(inblock)
		while stmt do
			table.insert(stmts, stmt)
			local next_token = peek()
			if next_token.type == "EOF" or next_token.type == "cascade" then
				-- do nothing
			elseif inblock and next_token.type == "closeblock" then
				-- do nothing
			else
				expect("endexpression")
			end
			stmt = self:parse_statement(inblock)
		end
		return stmts
	end

	function instance.parse_statement(self, inblock)
		for _, rule in pairs({ "parse_return", "parse_expression" }) do
			local r = self[rule](self, false, inblock)
			if r then
				return {
					type = "Statement",
					expr = r,
				}
			end
		end
	end

	function instance.parse_return(self, _, inblock)
		if not match("return") then
			return nil
		end
		return {
			type = inblock and "NonLocalReturn" or "Return",
			expr = self:parse_expression(),
			is_return = true,
		}
	end

	function instance.parse_expression(self, subset, disable_binary)
		for _, rule in pairs({
			"parse_parenthesis_expr",
			"parse_block",
			"parse_assignment",
			"parse_identifier",
			"parse_literal",
		}) do
			local r = self[rule](self)
			if r then
				local sub_rule = (subset and "parse_message_send_subset") or "parse_message_send"
				local message = self[sub_rule](self, r, disable_binary)
				if message.is_message then
					local cascade = self:parse_cascade()
					message.cascade = cascade
				end
				return {
					expr = message,
					type = "Expression",
				}
			end
		end
	end

	function instance.parse_cascade(self)
		local cascade = match("cascade")
		if not cascade then
			return nil
		end
		local cascades = {}
		while cascade do
			local expr = self:parse_message_send()
			table.insert(cascades, expr)
			cascade = match("cascade")
		end
		return {
			type = "Cascade",
			messages = cascades,
		}
	end

	function instance.parse_assignment(self)
		if not peek(1) or peek(1).type ~= "assignment" then
			return nil
		end
		local var = expect("identifier")
		expect("assignment")
		return {
			type = "Assignment",
			var = var,
			expr = self:parse_expression(),
		}
	end

	function instance.parse_parenthesis_expr(self)
		if not match("openparenthesis") then
			return nil
		end
		local expr = self:parse_expression()
		expect("closeparenthesis")
		return {
			type = "ParenthesisExpression",
			expr = expr.expr,
		}
	end

	function instance.parse_block(self)
		if not match("openblock") then
			return nil
		end
		local block_args = self:parse_block_arguments()
		if block_args then
			expect("pipe")
		end
		local localdefs = self:parse_localdefs()
		local body = self:parse_body(true)
		expect("closeblock")
		return {
			args = block_args,
			localdefs = localdefs,
			body = body,
			type = "Block",
		}
	end

	function instance.parse_block_arguments(self)
		local args = {}
		local arg = match("blockarg")
		while arg do
			table.insert(args, { id = arg.value:sub(2), pos = arg.pos })
			arg = match("blockarg")
		end
		return #args > 0 and args or nil
	end

	function instance.parse_message_send(self, receiver)
		local result = receiver
		for _, rule in pairs({ "parse_unary_message_send", "parse_binary_message_chain", "parse_keyword_message_send" }) do
			local r = self[rule](self, result)
			if r then
				result = r
			end
		end
		return result
	end

	function instance.parse_message_send_subset(self, receiver, disable_binary)
		local result = receiver
		local rules = disable_binary and { "parse_unary_message_send" }
			or { "parse_unary_message_send", "parse_binary_message_chain" }
		for _, rule in pairs(rules) do
			local r = self[rule](self, result)
			if r then
				result = r
			end
		end
		return result
	end

	function instance.parse_unary_message_send(self, receiver)
		local chain = {}
		local message = match("identifier")
		while message do
			table.insert(chain, { type = "UnaryMessage", id = message.value, pos = message.pos, is_message = true })
			message = match("identifier")
		end
		if #chain == 0 then
			return nil
		end
		if #chain == 1 then
			local message = chain[1]
			message.receiver = receiver
			return message
		end
		return {
			receiver = receiver,
			chain = chain,
			type = "UnaryMessageChain",
			is_message = true,
		}
	end

	function instance.parse_binary_message_chain(self, receiver)
		local chain = {}
		local message = match("binary_selector")
		while message do
			table.insert(chain, {
				type = "BinaryMessage",
				id = message.value,
				pos = message.pos,
				argument = self:parse_expression(true, true),
				is_message = true,
			})
			message = match("binary_selector")
		end
		if #chain == 0 then
			return nil
		end
		if #chain == 1 then
			local message = chain[1]
			message.receiver = receiver
			return message
		end
		return {
			receiver = receiver,
			chain = chain,
			type = "BinaryMessageChain",
			is_message = true,
		}
	end

	function instance.parse_keyword_message_send(self, receiver)
		local keyword = match("keyword")
		if not keyword then
			return nil
		end
		local arguments = {}
		local selector = ""
		while keyword do
			selector = selector .. keyword.value
			table.insert(arguments, self:parse_expression(true))
			keyword = match("keyword")
		end
		return {
			selector = selector,
			receiver = receiver,
			argument = arguments,
			type = "KeywordMessage",
			is_message = true,
		}
	end

	function instance.parse_identifier(self)
		local id = match("identifier")
		local after = peek(1)
		if id and (not after or after.style ~= "identifier") then
			return id
		end
	end

	function instance.parse_literal(self)
		for _, rule in pairs({ "parse_number", "parse_string", "parse_symbol", "parse_boolean", "parse_special" }) do
			local r = self[rule](self)
			if r then
				return r
			end
		end
	end

	function instance.parse_number(self)
		local literal = match("number")
		if literal then
			return {
				pos = literal.pos,
				type = literal.type,
				value = literal.value,
				parsed_value = tonumber(literal.value),
			}
		end
	end

	function instance.parse_string(self)
		local literal = match("string")
		if literal then
			return {
				pos = literal.pos,
				type = literal.type,
				value = literal.value,
				parsed_value = literal.value,
			}
		end
	end

	function instance.parse_symbol(self)
		local literal = match("symbol")
		if literal then
			return {
				pos = literal.pos,
				type = literal.type,
				value = literal.value,
				parsed_value = literal.value:sub(2),
			}
		end
	end

	function instance.parse_boolean(self)
		local literal = match("true") or match("false")
		if literal then
			return {
				pos = literal.pos,
				type = "boolean",
				value = literal.value,
				parsed_value = literal == "true" or "false",
			}
		end
	end

	function instance.parse_special(self)
		local literal = match("nil") or match("self") or match("super")
		if literal then
			return {
				pos = literal.pos,
				type = literal.type,
				value = literal.value,
			}
		end
	end

	instance.tokenize = tokenize
	return instance
end

local binary_translation = {}
binary_translation["+"] = "PLUS"
binary_translation["~"] = "TILDE"
binary_translation["!"] = "EXC"
binary_translation["@"] = "AROB"
binary_translation["%"] = "POURC"
binary_translation["&"] = "AND"
binary_translation["*"] = "STAR"
binary_translation["-"] = "MIN"
binary_translation["="] = "EQ"
binary_translation["|"] = "PIPE"
binary_translation["\\"] = "ASL"
binary_translation["<"] = "INF"
binary_translation[">"] = "SUP"
binary_translation["?"] = "QUES"
binary_translation[","] = "COMMA"
binary_translation[","] = "COMMA"
binary_translation["/"] = "SL"

local binary_chars = "[~!@%&*-+=|\\<>?,/]"

local function utf8_codes(str)
	local codes = {}
	local i = 1
	while i <= #str do
		local c = str:byte(i)
		local char_len = 1
		if c >= 0xF0 then
			char_len = 4
		elseif c >= 0xE0 then
			char_len = 3
		elseif c >= 0xC0 then
			char_len = 2
		end
		table.insert(codes, str:sub(i, i + char_len - 1))
		i = i + char_len
	end
	return codes
end

local function translate_binary_selector(selector)
	local res = "_BIN_"
	for _, code in ipairs(utf8_codes(selector)) do
		res = res .. binary_translation[code]
	end
	return res
end

local function translate_selector(selector)
	if selector == "-" or string.find(selector, binary_chars) then
		return translate_binary_selector(selector)
	end
	return string.gsub(selector, ":", "_")
end

local visit
local visitor
visitor = {
	Method = function(node)
		local code = "return function " .. visit(node.header) .. visit(node.localdefs)
		for _, stmt in pairs(node.body) do
			code = code .. visit(stmt)
		end
		if #node.body == 0 then
			code = code .. "return self\n"
		elseif not node.body[#node.body].expr.is_return then
			code = code .. "return self\n"
		end
		code = code .. "end"
		return code
	end,
	LocalVarDefinitions = function(node)
		local vars = {}
		for _, var in pairs(node.vars) do
			table.insert(vars, var.name)
		end
		local code = "local " .. table.concat(vars, ", ")
		local init_values = {}
		for i, _ in ipairs(node.vars) do
			init_values[i] = "World['nil']"
		end
		return code .. " = " .. table.concat(init_values, ", ") .. ";\n"
	end,
	UnaryMethod = function(node)
		-- return node.selector .. "(self)\n"
		return "(self)\n"
	end,
	BinaryMethod = function(node)
		-- return translate_binary_selector(node.selector) .. "(self, " .. node.argument .. ")\n"
		return "(self, " .. node.argument .. ")\n"
	end,
	KeywordMethod = function(node)
		-- return translate_selector(node.selector) .. "(self," .. table.concat(node.args, ", ") .. ")\n"
		return "(self," .. table.concat(node.args, ", ") .. ")\n"
	end,
	UnaryMessageCascade = function(code, node, is_last)
		if is_last then
			return code .. ":" .. node.id .. "()"
		end
		return "_c(" .. code .. ", '" .. node.id .. "')"
	end,
	BinaryMessageCascade = function(code, node, is_last)
		if is_last then
			return code .. ":" .. translate_selector(node.id) .. "()"
		end
		return "_c(" .. code .. ", '" .. translate_selector(node.id) .. "')"
	end,
	KeywordMessageCascade = function(code, node, is_last)
		local args = {}
		for _, expr in pairs(node.argument) do
			table.insert(args, visit(expr))
		end
		if is_last then
			return code .. ":" .. translate_selector(node.selector) .. "(" .. table.concat(args, ", ") .. ")"
		end
		return "_c(" .. code .. ", '" .. translate_selector(node.selector) .. "', " .. table.concat(args, ", ") .. ")"
	end,
	UnaryMessageChain = function(node)
		if node.cascade then
			local code = visit(node.receiver)
			for i = 1, #node.chain - 1, 1 do
				code = code .. ":" .. node.chain[i].id .. "()"
			end
			local last_message = node.chain[#node.chain]
			code = "_c(" .. code .. ", '" .. last_message.id .. "')"
			for i = 1, #node.cascade.messages, 1 do
				local m = node.cascade.messages[i]
				local subtype = m.type .. "Cascade"
				code = visitor[subtype](code, m, i == #node.cascade.messages)
			end
			return code
		end
		local code = visit(node.receiver)
		for _, message in pairs(node.chain) do
			code = code .. ":" .. message.id .. "()"
		end
		return code
	end,
	UnaryMessage = function(node)
		if node.cascade then
			local code = visit(node.receiver)
			code = "_c(" .. code .. ", '" .. node.id .. "')"
			for i = 1, #node.cascade.messages, 1 do
				local m = node.cascade.messages[i]
				local subtype = m.type .. "Cascade"
				code = visitor[subtype](code, m, i == #node.cascade.messages)
			end
			return code
		end
		local code = visit(node.receiver)
		code = code .. ":" .. node.id .. "()"
		return code
	end,
	BinaryMessageChain = function(node)
		if node.cascade then
			local code = visit(node.receiver)
			for i = 1, #node.chain - 1, 1 do
				code = code .. ":" .. node.chain[i].id .. "()"
			end
			local last_message = node.chain[#node.chain]
			code = "_c(" .. code .. ", '" .. last_message.id .. "')"
			for i = 1, #node.cascade.messages, 1 do
				local m = node.cascade.messages[i]
				local subtype = m.type .. "Cascade"
				code = visitor[subtype](code, m, i == #node.cascade.messages)
			end
			return code
		end
		local code = visit(node.receiver)
		for _, message in pairs(node.chain) do
			code = code .. ":" .. translate_selector(message.id) .. "(" .. visit(message.argument) .. ")"
		end
		return code
	end,
	BinaryMessage = function(node)
		if node.cascade then
			local code = visit(node.receiver)
			code = "_c(" .. code .. ", '" .. node.id .. "')"
			for i = 1, #node.cascade.messages, 1 do
				local m = node.cascade.messages[i]
				local subtype = m.type .. "Cascade"
				code = visitor[subtype](code, m, i == #node.cascade.messages)
			end
			return code
		end
		local code = visit(node.receiver)
		code = code .. ":" .. translate_selector(node.id) .. "(" .. visit(node.argument) .. ")"
		return code
	end,
	KeywordMessage = function(node)
		local code = visit(node.receiver)
		local selector = translate_selector(node.selector)
		local args = {}
		for _, expr in pairs(node.argument) do
			table.insert(args, visit(expr))
		end
		if node.cascade then
			code = "_c(" .. code .. ", '" .. selector .. "', "
			code = code .. table.concat(args, ", ") .. ")"

			-- handle the cascade here
			for i = 1, #node.cascade.messages, 1 do
				local m = node.cascade.messages[i]
				local subtype = m.type .. "Cascade"
				code = visitor[subtype](code, m, i == #node.cascade.messages)
			end
		else
			code = code .. ":" .. selector
			code = code .. "(" .. table.concat(args, ", ") .. ")"
		end
		return code
	end,
	Statement = function(node)
		return visit(node.expr) .. ";\n"
	end,
	Expression = function(node)
		return visit(node.expr)
	end,
	ParenthesisExpression = function(node)
		return "(" .. visit(node.expr) .. ")"
	end,
	Assignment = function(node)
		return node.var.value .. " = " .. visit(node.expr)
	end,
	Block = function(node)
		local code = "(Block:create(function("
		local args = {}
		for _, arg in pairs(node.args or {}) do
			table.insert(args, arg.id)
		end
		code = code .. table.concat(args, ", ") .. ")\n"
		for i = 1, #node.body - 1, 1 do
			local stmt = node.body[i]
			code = code .. visit(stmt)
		end

		code = code .. visit(node.localdefs)

		local last_stmt = #node.body > 0 and node.body[#node.body]
		if not last_stmt then
			code = code .. "return World['nil']\n"
		elseif not last_stmt.expr.is_return then
			code = code .. "return " .. visit(last_stmt)
		else
			code = code .. visit(last_stmt)
		end
		code = code .. "end))"
		return code
	end,
	NonLocalReturn = function(node)
		return "return_(" .. visit(node.expr) .. ")"
	end,
	Return = function(node)
		return "return " .. visit(node.expr)
	end,
	number = function(node)
		return "World['" .. node.value .. "']"
	end,
	identifier = function(node)
		return node.value
	end,
	self = function(node)
		return "self"
	end,
	boolean = function(node)
		return "World['" .. node.value .. "']"
	end,
	string = function(node)
		return node.value
	end,
}
visitor["nil"] = function(node)
	return "World['nil']"
end
visit = function(node, ...)
	if node == nil then
		return ""
	end
	local visitor_function = visitor[node.type]
	if visitor_function == nil then
		error("Unknown visitor for " .. (node and node.type or "nil"))
	end
	return visitor_function(node, ...)
end

local function smalltalk2lua(str)
	local parser = Parser.new()
	local ast = parser:parse(str)
	return {
		code = visit(ast),
		ast = ast,
		lua_name = translate_selector(ast.header.selector),
		st_selector = ast.header.selector,
	}
end

return {
	Parser = Parser,
	compile = smalltalk2lua,
	translate_selector = translate_selector,
	translate_binary_selector = translate_binary_selector,
}
