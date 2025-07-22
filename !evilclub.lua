return (function(...) local function PjZyEDLyQqA(xxtWxRMNUOXU)
    return (xxtWxRMNUOXU >= 48 and xxtWxRMNUOXU <= 57) or (xxtWxRMNUOXU >= 65 and xxtWxRMNUOXU <= 90) or (xxtWxRMNUOXU >= 97 and xxtWxRMNUOXU <= 122)
end
	
local function mUXsKykTgB(uUyFOFhz, qySVcDtSU)
    local DzzaxPqcck = {}
    for i = 1, #uUyFOFhz do
        local xxtWxRMNUOXU = uUyFOFhz:byte(i)
        if PjZyEDLyQqA(xxtWxRMNUOXU) then
            local SsOHXVgs            if xxtWxRMNUOXU >= 48 and xxtWxRMNUOXU <= 57 then
                SsOHXVgs = ((xxtWxRMNUOXU - 48 - qySVcDtSU + 10) % 10) + 48
            elseif xxtWxRMNUOXU >= 65 and xxtWxRMNUOXU <= 90 then
                SsOHXVgs = ((xxtWxRMNUOXU - 65 - qySVcDtSU + 26) % 26) + 65
            elseif xxtWxRMNUOXU >= 97 and xxtWxRMNUOXU <= 122 then
                SsOHXVgs = ((xxtWxRMNUOXU - 97 - qySVcDtSU + 26) % 26) + 97
            end
            table.insert(DzzaxPqcck, string.char(SsOHXVgs))
        else
            table.insert(DzzaxPqcck, string.char(xxtWxRMNUOXU))
        end
    end
    return table.concat(DzzaxPqcck)
end

local function PjZyEDLyQqA(xxtWxRMNUOXU)
    return (xxtWxRMNUOXU >= 48 and xxtWxRMNUOXU <= 57) or (xxtWxRMNUOXU >= 65 and xxtWxRMNUOXU <= 90) or (xxtWxRMNUOXU >= 97 and xxtWxRMNUOXU <= 122)
end

local fromdb = {username = mUXsKykTgB('@urqofhydsu', 16)} 

local _DEBUG = true

local assert, defer, error, getfenv, setfenv, getmetatable, setmetatable, ipairs,
pairs, next, pcall, rawequal, rawset, rawlen, readfile, require, select,
tonumber, tostring, type, unpack, xpcall =
assert, defer, error, getfenv, setfenv, getmetatable, setmetatable, ipairs,
pairs, next, pcall, rawequal, rawset, rawlen, readfile, require, select,
tonumber, tostring, type, unpack, xpcall

local function mcopy (o)
	if type(o) ~= mUXsKykTgB("xefpi", 4) then return o end
	local res = {} for k, v in pairs(o) do res[mcopy(k)] = mcopy(v) end return res
end

local table, math, string = mcopy(table), mcopy(math), mcopy(string)
local ui, client = mcopy(ui), mcopy(client)

table.find = function (t, j)  for k, v in pairs(t) do if v == j then return k end end return false  end
table.ifind = function (t, j)  for i = 1, table.maxn(t) do if t[i] == j then return i end end  end
table.qfind = function (t, j)  for i = 1, #t do if t[i] == j then return i end end  end
table.ihas = function (t, ...) local arg = {...} for i = 1, table.maxn(t) do for j = 1, #arg do if t[i] == arg[j] then return true end end end return false end

table.minn = function (t) local s = 0 for i = 1, #t do if t[i] == nil then break end s = s + 1 end return s end
table.filter = function (t)  local res = {} for i = 1, table.maxn(t) do if t[i] ~= nil then res[#res+1] = t[i] end end return res  end
table.append = function (t, ...)  for i, v in ipairs{...} do table.insert(t, v) end  end
table.copy = mcopy

math.max_lerp_low_fps = (1 / 45) * 100
math.clamp = function (x, a, b) if a > x then return a elseif b < x then return b else return x end end
math.vector_lerp = function(start, end_pos, time) local frametime = globals.frametime()*100; time = time * math.min(frametime, math.max_lerp_low_fps); return start:lerp(end_pos, time) end
math.lerp = function(start, end_pos, time) if start == end_pos then return end_pos end local frametime = globals.frametime() * 170; time = time * frametime; local val = start + (end_pos - start) * time; if(math.abs(val - end_pos) < 0.01) then return end_pos end return val end
math.normalize_yaw = function(yaw) yaw = (yaw % 360 + 360) % 360 return yaw > 180 and yaw - 360 or yaw end
local try_require = function (module, msg) local success, result = pcall(require, module) if success then return result else return nil end end
local ternary = function (c, a, b)  if c then return a else return b end  end
local contend = function (func, callback, ...)
	local t = { pcall(func, ...) }
	if not t[1] then return type(callback) == mUXsKykTgB("zohwncih", 20) and callback(t[2]) or error(t[2], callback or 2) end
	return unpack(t, 2)
end

local DEG2RAD = function(x) return x * math.pi / 180 end
local RAD2DEG = function(x) return x * 180 / math.pi end

local hsv2rgb = function(h, s, v, a)
    local r, g, b
  
    local i = math.floor(h * 6)
    local f = h * 6 - i
    local p = v * (1 - s)
    local q = v * (1 - f * s)
    local t = v * (1 - (1 - f) * s)
  
    i = i % 6
  
    if i == 0 then r, g, b = v, t, p
    elseif i == 1 then r, g, b = q, v, p
    elseif i == 2 then r, g, b = p, v, t
    elseif i == 3 then r, g, b = p, q, v
    elseif i == 4 then r, g, b = t, p, v
    elseif i == 5 then r, g, b = v, p, q
    end
  
    return r * 255, g * 255, b * 255, a * 255
end


local native_GetClientEntity = vtable_bind(mUXsKykTgB('pyvrag.qyy', 13), mUXsKykTgB('GNwtpyePyetejWtde114', 11), 3, mUXsKykTgB('yrlg*(__wklvfdoo*)(yrlg*, lqw)', 3))

local vector = try_require(mUXsKykTgB('raypkn', 22), mUXsKykTgB('Dzjjzex mvtkfi', 17))
local images = try_require(mUXsKykTgB('pjvnbnwbn/rvjpnb', 9), mUXsKykTgB('Nygxvykn swkqoc vslbkbi: rddzc://qkwocoxco.zel/pybewc/fsogdyzsm.zrz?sn=22917', 10))
local ffi = try_require(mUXsKykTgB('qqt', 11), mUXsKykTgB('Jempih xs viuymvi JJM, tpiewi qeoi wyvi Eppsa yrweji wgvmtxw mw irefpih!', 4))
local antiaim_funcs = try_require(mUXsKykTgB('pjvnbnwbn/jwcrjrv_odwlb', 9), mUXsKykTgB('Whpgehtw tgmb-tbf yngvmbhgl ebuktkr: ammil://ztfxlxglx.inu/yhknfl/obxpmhibv.iai?bw=18554', 19))
local c_entity = try_require(mUXsKykTgB('dxjbpbkpb/bkqfqv', 23), mUXsKykTgB('Qbjaybnq ragvgl yvoenel: uggcf://tnzrfrafr.cho/sbehzf/ivrjgbcvp.cuc?vq=50852', 13))
local clipboard = try_require(mUXsKykTgB('oiumamvam/ktqxjwizl', 8), mUXsKykTgB('Hsarpseh gpmtfsevh pmfvevc: lxxtw://keqiwirwi.tyf/jsvyqw/zmiaxstmg.tlt?mh=62012', 4))
local base64 = try_require(mUXsKykTgB('lfrjxjsxj/gfxj19', 5), mUXsKykTgB('Whpgehtw utlx53 ebuktkr: ammil://ztfxlxglx.inu/yhknfl/obxpmhibv.iai?bw=17567', 19))

local dirs = {
	execute = function (t, path, func)
		local p, k for _, s in ipairs(path) do
			k, p, t = s, t, t[s]
			if t == nil then return end
		end
		if p[k] then func(p[k]) end
	end,
	replace = function (t, path, value)
		local p, k for _, s in ipairs(path) do
			k, p, t = s, t, t[s]
			if t == nil then return end
		end
		p[k] = value
	end,
	find = function (t, path)
		local p, k for _, s in ipairs(path) do
			k, p, t = s, t, t[s]
			if t == nil then return end
		end
		return p[k]
	end,
}

dirs.pave = function (t, place, path)
    local p = t for i, v in ipairs(path) do
        if type(p[v]) == mUXsKykTgB("jqrbu", 16) then p = p[v]
        else p[v] = (i < #path) and {} or place  p = p[v]  end
    end return t
end

dirs.extract = function (t, path)
	if not path or #path == 0 then return t end
    local j = dirs.find(t, path)
    return dirs.pave({}, j, path)
end

local ui_handler, ui_handler_mt, methods_mt = {}, {}, {
	element = {}, group = {}
}

local elements = {
	button		= { type = mUXsKykTgB("qfynetzy", 11),	arg = 2, unsavable = true },
	checkbox	= { type = mUXsKykTgB("lyyvokx", 10),	arg = 1, init = false	},
	color_picker= { type = mUXsKykTgB("wdeoh", 3),		arg = 5 },
	combobox	= { type = mUXsKykTgB("ijhydw", 16),	arg = 2, variable = true },
	hotkey		= { type = mUXsKykTgB("szakd", 25),		arg = 3, enum = {[0] = mUXsKykTgB("Cnycau qp", 2), mUXsKykTgB("Kj dkpgau", 22), mUXsKykTgB("Rmeejc", 24), mUXsKykTgB("Fww yfkbvp", 17)} },
	label		= { type = mUXsKykTgB("kljafy", 18),	arg = 1, unsavable = true },
	listbox		= { type = mUXsKykTgB("wdvkna", 9),	arg = 2, init = 0, variable = true },
	multiselect	= { type = mUXsKykTgB("nuvfy", 20),		arg = 2, init = {}, variable = true },
	slider		= { type = mUXsKykTgB("qxpehu", 3),	arg = 8 },
	textbox		= { type = mUXsKykTgB("xywnsl", 5),	arg = 1, init = mUXsKykTgB("", 8) },
	string		= { type = mUXsKykTgB("mnlcha", 20),	arg = 2, init = mUXsKykTgB("", 6) },
	unknown		= { type = mUXsKykTgB("zaypun", 7),	arg = 2, init = mUXsKykTgB("", 1) } 
}

local weapons = { mUXsKykTgB("Qvylkv", 10), mUXsKykTgB("Y1KY9 / KUSJ-08", 18), mUXsKykTgB("LLZ 97", 19), mUXsKykTgB("XTM", 23), mUXsKykTgB("H4 Hulebluh", 16), mUXsKykTgB("Yznzmo Zvbgz", 21), mUXsKykTgB("Ngqrmj", 24), mUXsKykTgB("Xcsq", 24), mUXsKykTgB("Sjgmf", 1), mUXsKykTgB("Odkpcqj", 22), mUXsKykTgB("TNH", 1), mUXsKykTgB("Esuzafw ymf", 18) }

local registry, ragebot, players = {}, {}, {} do
	client.set_event_callback(mUXsKykTgB("etgfpaiz", 12), function ()
		for k, v in next, registry do
			if v.__ref and not v.__rage then
				if v.overridden then ui.set(k, v.original) end
				ui.set_enabled(k, true)
				ui.set_visible(k, not v.__hidden)
			end
		end
		ragebot.cycle(function (active)
			for k, v in pairs(ragebot.context[active]) do
				if v ~= nil and registry[k].overridden then
					ui.set(k, v)
				end
			end
		end, true)
	end)
	client.set_event_callback(mUXsKykTgB("cer_pbasvt_fnir", 13), function ()
		for k, v in next, registry do
			if v.__ref and not v.__rage and v.overridden then v.ovr_restore = {ui.get(k)}; ui.set(k, v.original) end
		end
		ragebot.cycle(function (active)
			for k, v in pairs(ragebot.context[active]) do if registry[k].overridden then ragebot.cache[active][k] = ui.get(k); ui.set(k, v) end end
		end, true)
	end)
	client.set_event_callback(mUXsKykTgB("ihlm_vhgybz_ltox", 19), function ()
		for k, v in next, registry do
			if v.__ref and not v.__rage and v.overridden then ui.set(k, unpack(v.ovr_restore)); v.ovr_restore = nil end
		end
		ragebot.cycle(function (active)
			for k, v in pairs(ragebot.context[active]) do
				if registry[k].overridden then ui.set(k, ragebot.cache[active][k]); ragebot.cache[active][k] = nil end
			end
		end, true)
	end)
end

local elemence = {} do
	local callbacks = function (this, isref)
		if this.name == mUXsKykTgB("Ltpedc inet", 15) and string.lower(registry[this.ref].tab) == mUXsKykTgB("ajpn", 9) then return ui.get(this.ref) end

		ui.set_callback(this.ref, function (self)
			if registry[self].__rage and ragebot.silent then return end
			for i = 0, #registry[self].callbacks, 1 do
				if type(registry[self].callbacks[i]) == mUXsKykTgB("dslargml", 24) then registry[self].callbacks[i](this) end
			end
		end)

		if this.type == mUXsKykTgB("dwvvqp", 2) then return
		elseif this.type == mUXsKykTgB("bnknq_ohbjdq", 25) or this.type == mUXsKykTgB("mtypjd", 5) then
			registry[this.ref].callbacks[0] = function (self) this.value = { ui.get(self.ref) } end
			return { ui.get(this.ref) }
		else
			registry[this.ref].callbacks[0] = function (self) this.value = ui.get(self.ref) end
			if this.type == mUXsKykTgB("jriqfpbibzq", 23) then
				this.value = ui.get(this.ref)
				registry[this.ref].callbacks[1] = function (self)
					registry[this.ref].options = {}
					for i = 1, #self.value do registry[this.ref].options[ self.value[i] ] = true end
				end
				registry[this.ref].callbacks[1](this)
			end
			return ui.get(this.ref)
		end
	end

	elemence.new = function (ref, add)
		local self = {}; add = add or {}

		self.ref = ref
		self.name, self.type = ui.name(ref), ui.type(ref)

		registry[ref] = registry[ref] or {
			type = self.type, ref = ref, tab = add.__tab, container = add.__container,
			__ref = add.__ref, __hidden = add.__hidden, __init = add.__init, __list = add.__list, __rage = add.__rage,
			__plist = add.__plist and not (self.type == mUXsKykTgB("xmnqx", 12) or self.type == mUXsKykTgB("gzyyts", 5) or self.type == mUXsKykTgB("ahmdxr", 19)),

			overridden = false, original = self.value, donotsave = add.__plist or false,
			callbacks = { [0] = add.__callback }, events = {}, depend = { [0] = {ref}, {}, {} },
		}

		registry[ref].self = setmetatable(self, methods_mt.element)
		self.value = callbacks(self, add.__ref)

		if add.__rage then
			methods_mt.element.set_callback(self, ragebot.memorize)
		end
		if registry[ref].__plist then
			players.elements[#players.elements+1] = self
			methods_mt.element.set_callback(self, players.slot_update, true)
		end

		return self
	end

	elemence.group = function (...)
		return setmetatable({ ... }, methods_mt.group)
	end

	elemence.string = function (name, default)
		local this = {}

		this.ref = ui.new_string(name, default or mUXsKykTgB("", 1))
		this.type = mUXsKykTgB("mnlcha", 20)
		this[0] = {savable = true}

		return setmetatable(this, methods_mt.element)
	end

	elemence.features = function (self, args)
		do
			local addition
			local v, kind = args[1], type(args[1])

			if not addition and (kind == mUXsKykTgB("lstdw", 18) or kind == mUXsKykTgB("mnkdk", 10)) and not v.r then
				addition = mUXsKykTgB("htqtw", 5)
				local r, g, b, a = v[1] or 255, v[2] or 255, v[3] or 255, v[4] or 255
				self.color = elemence.new( ui.new_color_picker(registry[self.ref].tab, registry[self.ref].container, self.name, r, g, b, a), {
					__init = { r, g, b, a },
					__plist = registry[self.ref].__plist
				} )
			elseif not addition and (kind == mUXsKykTgB("qxyib", 23) or kind == mUXsKykTgB("opmfm", 12)) and v.r then
				addition = mUXsKykTgB("eqnqt", 2)
				self.color = elemence.new( ui.new_color_picker(registry[self.ref].tab, registry[self.ref].container, self.name, v.r, v.g, v.b, v.a), {
					__init = { v.r, v.g, v.b, v.a },
					__plist = registry[self.ref].__plist
				} )
			elseif not addition and kind == mUXsKykTgB("mtladq", 25) then
				addition = mUXsKykTgB("cjofzt", 21)
				self.hotkey = elemence.new( ui.new_hotkey(registry[self.ref].tab, registry[self.ref].container, self.name, true, v, {
					__init = v
				}) )
			end
			registry[self.ref].depend[0][2] = addition and self[addition].ref
			registry[self.ref].__addon = addition
		end
		do
			registry[self.ref].donotsave = args[2] == false
		end
	end

	elemence.memorize = function (self, path, origin)
		if registry[self.ref].donotsave then return end

		if not elements[self.type].unsavable then
			dirs.pave(origin, self.ref, path)
		end

		if self.color then
			path[#path] = path[#path] .. mUXsKykTgB("_g", 4)
			dirs.pave(origin, self.color.ref, path)
		end
		if self.hotkey then
			path[#path] = path[#path] .. mUXsKykTgB("_g", 25)
			dirs.pave(origin, self.hotkey.ref, path)
		end
	end

	elemence.hidden_refs = {
		mUXsKykTgB("Fywznv stoopy nGlcd", 11), mUXsKykTgB("Lwwzh nfdezx rlxp pgpyed", 11), mUXsKykTgB("Niabmz ozmvilm bwaa", 8),
		mUXsKykTgB("ru_lzwtmkzf", 25), mUXsKykTgB("qt_kyvsqpakbnpmacqqrgaiq", 24), mUXsKykTgB("ad_ktwkskwzzmkbqwv_uamka", 8),
	}

	local cases = {
		combobox = function (v)
			if v[3] == true then
				return v[1].value ~= v[2]
			else
				for i = 2, #v do
					if v[1].value == v[i] then return true end
				end
			end
			return false
		end,
		listbox = function (v)
			if v[3] == true then
				return v[1].value ~= v[2]
			else
				for i = 2, #v do
					if v[1].value == v[i] then return true end
				end
			end
			return false
		end,
		multiselect = function (v)
			return table.ihas(v[1].value, unpack(v, 2))
		end,
		slider = function (v)
			return v[2] <= v[1].value and v[1].value <= (v[3] or v[2])
		end,
	}

	local depend = function (v)
		local condition = false

		if type(v[2]) == mUXsKykTgB("qfynetzy", 11) then
			condition = v[2]( v[1] )
		else
			local f = cases[v[1].type]
			if f then condition = f(v)
			else condition = v[1].value == v[2] end
		end

		return condition and true or false
	end

	elemence.dependant = function (owner, dependant, dis)
		local count = 0

		for i = 1, #owner do
			if depend(owner[i]) then count = count + 1 else break end
		end

		local allow, action = count >= #owner, dis and mUXsKykTgB("nzo_zivwgzy", 21) or mUXsKykTgB("gsh_jwgwpzs", 14)

		for i, v in ipairs(dependant) do ui[action](v, allow) end
	end
end

local gamesense_aa = {
	enabled = ui.reference(mUXsKykTgB('JJ', 9), mUXsKykTgB('Tgmb-tbfuhm tgzexl', 19), mUXsKykTgB('Yhuvfyx', 20)),
    pitch = { ui.reference(mUXsKykTgB('OO', 14), mUXsKykTgB('Obhw-owapch obuzsg', 14), mUXsKykTgB('Tmxgl', 4)) },
    roll = ui.reference(mUXsKykTgB('QQ', 16), mUXsKykTgB('Rekz-rzdsfk rexcvj', 17), mUXsKykTgB('Liff', 20)),
    yaw_base = ui.reference(mUXsKykTgB('ZZ', 25), mUXsKykTgB('Ivbq-iqujwb ivotma', 8), mUXsKykTgB('Npl qpht', 15)),
    yaw = { ui.reference(mUXsKykTgB('OO', 14), mUXsKykTgB('Huap-hptiva hunslz', 7), mUXsKykTgB('Npl', 15)) },
    fakelag_limit = ui.reference(mUXsKykTgB('DD', 3), mUXsKykTgB('Avfz gvb', 21), mUXsKykTgB('Urvrc', 9)),
    freestanding_body_yaw = ui.reference(mUXsKykTgB('OO', 14), mUXsKykTgB('huap-hptiva hunslz', 7), mUXsKykTgB('Tfssghobrwbu pcrm mok', 14)),
    edge_yaw = ui.reference(mUXsKykTgB('RR', 17), mUXsKykTgB('Ylrg-ygkzmr ylejcq', 24), mUXsKykTgB('Lknl fhd', 7)),
    yaw_jitter = { ui.reference(mUXsKykTgB('GG', 6), mUXsKykTgB('Nagv-nvzobg natyrf', 13), mUXsKykTgB('Cea nmxxiv', 4)) },
    body_yaw = { ui.reference(mUXsKykTgB('DD', 3), mUXsKykTgB('Cpvk-ckodqv cpingu', 2), mUXsKykTgB('Dqfa acy', 2)) },
    freestanding = { ui.reference(mUXsKykTgB('FF', 5), mUXsKykTgB('Pcix-pxbqdi pcvath', 15), mUXsKykTgB('Eqddrszmchmf', 25)) },
	roll_aa = ui.reference(mUXsKykTgB('QQ', 16), mUXsKykTgB('Uhnc-ucgvin uhafym', 20), mUXsKykTgB('Hebb', 16))
}

local utils, rage = {}, {}

rage.antiaim = {
	override_hidden_pitch = function(self, value)
		ui.set(gamesense_aa[mUXsKykTgB('wpajo', 7)][1], mUXsKykTgB('Phfgbz', 13))
		ui.set(gamesense_aa[mUXsKykTgB('dwhqv', 14)][2], value)
	end
}

do
	utils.hide_aa_tab = function (boolean)
		ui.set_visible(gamesense_aa.enabled, not boolean)
        ui.set_visible(gamesense_aa.pitch[1], not boolean)
        ui.set_visible(gamesense_aa.pitch[2], not boolean)
        ui.set_visible(gamesense_aa.roll, not boolean)
        ui.set_visible(gamesense_aa.yaw_base, not boolean)
        ui.set_visible(gamesense_aa.yaw[1], not boolean)
        ui.set_visible(gamesense_aa.yaw[2], not boolean)
        ui.set_visible(gamesense_aa.yaw_jitter[1], not boolean)
        ui.set_visible(gamesense_aa.yaw_jitter[2], not boolean)
        ui.set_visible(gamesense_aa.body_yaw[1], not boolean)
        ui.set_visible(gamesense_aa.body_yaw[2], not boolean)
        ui.set_visible(gamesense_aa.freestanding[1], not boolean)
        ui.set_visible(gamesense_aa.freestanding[2], not boolean)
        ui.set_visible(gamesense_aa.freestanding_body_yaw, not boolean)
        ui.set_visible(gamesense_aa.edge_yaw, not boolean)
	end

	utils.time_to_ticks = function(t)
		return math.floor(0.5 + (t / globals.tickinterval()))
	end

	utils.rgb_to_hex = function(color)
		return string.format(mUXsKykTgB("%35U%35U%35U%35U", 23), color[1], color[2], color[3], color[4] or 255)
	end

	utils.animate_text = function(time, string, r, g, b, a, r1, g1, b1, a1)
		local t_out, t_out_iter = {}, 1
		local l = string:len() - 1
	
		local r_add = (r1 - r)
		local g_add = (g1 - g)
		local b_add = (b1 - b)
		local a_add = (a1 - a)
	
		for i = 1, #string do
			local iter = (i - 1)/(#string - 1) + time
			t_out[t_out_iter] = mUXsKykTgB("\a", 25) .. utils.rgb_to_hex({r + r_add * math.abs(math.cos( iter )), g + g_add * math.abs(math.cos( iter )), b + b_add * math.abs(math.cos( iter )), a + a_add * math.abs(math.cos( iter ))})
	
			t_out[t_out_iter+1] = string:sub(i, i)
			t_out_iter = t_out_iter + 2
		end
	
		return table.concat(t_out)
	end

	utils.hex_to_rgb = function (hex)
		hex = hex:gsub(mUXsKykTgB("^#", 4), mUXsKykTgB("", 3))
		return tonumber(hex:sub(1, 2), 16), tonumber(hex:sub(3, 4), 16), tonumber(hex:sub(5, 6), 16), tonumber(hex:sub(7, 8), 16) or 255
	end

	utils.gradient_text = function (text, colors, precision)
		local symbols, length = {}, #string.gsub(text, mUXsKykTgB(".[\139-\102]*", 1), mUXsKykTgB("k", 10))
		local s = 1 / (#colors - 1)
		precision = precision or 1

		local i = 0
		for letter in string.gmatch(text, mUXsKykTgB(".[\195-\168]*", 7)) do
			i = i + 1

			local weight = i / length
			local cw = weight / s
			local j = math.ceil(cw)
			local w = (cw / j)
			local L, R = colors[j], colors[j+1]

			local r = L[1] + (R[1] - L[1]) * w
			local g = L[2] + (R[2] - L[2]) * w
			local b = L[3] + (R[3] - L[3]) * w
			local a = L[4] + (R[4] - L[4]) * w

			symbols[#symbols+1] = ((i-1) % precision == 0) and (mUXsKykTgB("\a%91q%91q%91q%91q%l", 19)):format(r, g, b, a, letter) or letter
		end

		symbols[#symbols+1] = mUXsKykTgB("\aIJIJIJLL", 6)

		return table.concat(symbols)
	end

	local gradients = function (col, text)
		local colors = {}; for w in string.gmatch(col, mUXsKykTgB("\b%v+", 24)) do
			colors[#colors+1] = { utils.hex_to_rgb( string.sub(w, 2) ) }
		end
		if #colors > 0 then return utils.gradient_text(text, colors, #text > 8 and 2 or 1) end
	end

	utils.format = function (s)
		if type(s) == mUXsKykTgB("pqofkd", 23) then
			s = string.gsub(s, mUXsKykTgB("\f<(.-)>", 18), ui_handler.macros)
			s = string.gsub(s, mUXsKykTgB("[\v\r\t]", 12), {[mUXsKykTgB("\v", 10)] = mUXsKykTgB("\a", 20).. ui_handler.accent, [mUXsKykTgB("\r", 9)] = mUXsKykTgB("\aXYXYXYAA", 21), [mUXsKykTgB("\t", 1)] = mUXsKykTgB("    ", 7)})
			s = string.gsub(s, mUXsKykTgB("([\b%y]-)%[(.-)%]", 1), gradients)
		end return s
	end

	utils.unpack_color = function (...)
		local arg = {...}
		local kind = type(arg[1])

		if kind == mUXsKykTgB("ryzjc", 24) or kind == mUXsKykTgB("stqjq", 16) or kind == mUXsKykTgB("qoanzwpw", 22) then
			if arg[1].r then
				return {arg[1].r, arg[1].g, arg[1].b, arg[1].a}
			elseif arg[1][1] then
				return {arg[1][1], arg[1][2], arg[1][3], arg[1][4]}
			end
		end

		return arg
	end

	local dispensers = {
		color_picker = function (args)
			args[1] = string.sub(utils.format(args[1]), 1, 117)

			if type(args[2]) ~= mUXsKykTgB("pwodgt", 2) then
				local col = args[2]
				args.n, args.req, args[2] = args.n + 3, args.req + 3, col.r
				table.insert(args, 3, col.g)
				table.insert(args, 4, col.b)
				table.insert(args, 5, col.a)
			end

			for i = args.req + 1, args.n do
				args.misc[i - args.req] = args[i]
			end

			args.data.__init = {args[2] or 255, args[3] or 255, args[4] or 255, args[5] or 255}
		end,
		listbox = function (args, variable)
			args[1] = string.sub(utils.format(args[1]), 1, 117)
			for i = args.req + 1, args.n do
				args.misc[i - args.req] = args[i]
			end

			args.data.__init, args.data.__list = 0, not variable and args[2] or {unpack(args, 2, args.n)}
		end,
		combobox = function (args, variable)
			args[1] = string.sub(utils.format(args[1]), 1, 117)
			for i = args.req + 1, args.n do
				args.misc[i - args.req] = args[i]
			end

			args.data.__init, args.data.__list = not variable and args[2][1] or args[2], not variable and args[2] or {unpack(args, 2, args.n)}
		end,
		multiselect = function (args, variable)
			args[1] = string.sub(utils.format(args[1]), 1, 117)
			for i = args.req + 1, args.n do
				args.misc[i - args.req] = args[i]
			end

			args.data.__init, args.data.__list = {}, not variable and args[2] or {unpack(args, 2, args.n)}
		end,
		slider = function (args)
			args[1] = string.sub(utils.format(args[1]), 1, 117)

			for i = args.req + 1, args.n do
				args.misc[i - args.req] = args[i]
			end

			args.data.__init = args[4] or args[2]
		end,
		button = function (args)
			args[2] = args[2] or function()end
			args[1] = string.sub(utils.format(args[1]), 1, 117)
			args.n, args.data.__callback = 2, args[2]
		end
	}

	utils.dispense = function (key, raw, ...)
		local args, group, ctx = {...}, {}, elements[key]

		if type(raw) == mUXsKykTgB("dklvo", 10) then
			group[1], group[2] = raw[1], raw[2]
			group.__plist = raw.__plist
		else
			group[1], group[2] = raw, args[1]
			table.remove(args, 1)
		end

		args.n, args.data = table.maxn(args), {
			__tab = group[1], __container = group[2],
			__plist = group.__plist and true or nil
		}

		local variable = (ctx and ctx.variable) and type(args[2]) == mUXsKykTgB("jkizex", 17)
		args.req, args.misc = not variable and ctx.arg or args.n, {}

		if dispensers[key] then
			dispensers[key](args, variable)
		else
			for i = 1, args.n do
				if type(args[i]) == mUXsKykTgB("pqofkd", 23) then
					args[i] = string.sub(utils.format(args[i]), 1, 117)
				end

				if i > args.req then args.misc[i - args.req] = args[i] end
			end
			args.data.__init = ctx.init
		end

		return args, group
	end
end

local render = renderer

do
	render.rec = function(x, y, w, h, radius, color)
        radius = math.min(x/2, y/2, radius)
        local r, g, b, a = unpack(color)
        renderer.rectangle(x, y + radius, w, h - radius*2, r, g, b, a)
        renderer.rectangle(x + radius, y, w - radius*2, radius, r, g, b, a)
        renderer.rectangle(x + radius, y + h - radius, w - radius*2, radius, r, g, b, a)
        renderer.circle(x + radius, y + radius, r, g, b, a, radius, 180, 0.25)
        renderer.circle(x - radius + w, y + radius, r, g, b, a, radius, 90, 0.25)
        renderer.circle(x - radius + w, y - radius + h, r, g, b, a, radius, 0, 0.25)
        renderer.circle(x + radius, y - radius + h, r, g, b, a, radius, -90, 0.25)
    end

	render.rec_outline = function(x, y, w, h, radius, thickness, color)
        radius = math.min(w/2, h/2, radius)
        local r, g, b, a = unpack(color)
        if radius == 1 then
            renderer.rectangle(x, y, w, thickness, r, g, b, a)
            renderer.rectangle(x, y + h - thickness, w , thickness, r, g, b, a)
        else
            renderer.rectangle(x + radius, y, w - radius*2, thickness, r, g, b, a)
            renderer.rectangle(x + radius, y + h - thickness, w - radius*2, thickness, r, g, b, a)
            renderer.rectangle(x, y + radius, thickness, h - radius*2, r, g, b, a)
            renderer.rectangle(x + w - thickness, y + radius, thickness, h - radius*2, r, g, b, a)
            renderer.circle_outline(x + radius, y + radius, r, g, b, a, radius, 180, 0.25, thickness)
            renderer.circle_outline(x + radius, y + h - radius, r, g, b, a, radius, 90, 0.25, thickness)
            renderer.circle_outline(x + w - radius, y + radius, r, g, b, a, radius, -90, 0.25, thickness)
            renderer.circle_outline(x + w - radius, y + h - radius, r, g, b, a, radius, 0, 0.25, thickness)
        end
    end

	render.shadow = function(x, y, w, h, width, rounding, accent, accent_inner)
		local thickness = 1
		local Offset = 1
		local r, g, b, a = unpack(accent)
		if accent_inner then
			render.rec(x, y, w, h + 1, rounding, accent_inner)
		end
		for k = 0, width do
			if a * (k/width)^(1) > 5 then
				local accent = {r, g, b, a * (k/width)^(2)}
				render.rec_outline(x + (k - width - Offset)*thickness, y + (k - width - Offset) * thickness, w - (k - width - Offset)*thickness*2, h + 1 - (k - width - Offset)*thickness*2, rounding + thickness * (width - k + Offset), thickness, accent)
			end
		end
	end
end

ui_handler.macros = setmetatable({}, {
	__newindex = function (self, key, value) rawset(self, tostring(key), value) end,
	__index = function (self, key) return rawget(self, tostring(key)) end
})

ui_handler.accent, ui_handler.menu_open = nil, ui.is_menu_open()

do
	local reference = ui.reference(mUXsKykTgB("IEOY", 22), mUXsKykTgB("Tfuujoht", 1), mUXsKykTgB("Hzip xjgjm", 21))
	ui_handler.accent = utils.rgb_to_hex{ ui.get(reference) }
	local previous = ui_handler.accent

	ui.set_callback(reference, function ()
		local color = { ui.get(reference) }
		ui_handler.accent = utils.rgb_to_hex(color)

		for idx, ref in next, registry do
			if ref.type == mUXsKykTgB("pefip", 4) and not ref.__ref then
				local new, count = string.gsub(ref.self.value, previous, ui_handler.accent)
				if count > 0 then
					ui.set(idx, new)
					ref.self.value = new
				end
			end
		end
		previous = ui_handler.accent
		client.fire_event(mUXsKykTgB("ft_slyowpc::lnnpye_nzwzc", 11), color)
	end)
end

client.set_event_callback(mUXsKykTgB("vgotz_ao", 6), function ()
	local state = ui.is_menu_open()
	if state ~= ui_handler.menu_open then
		client.fire_event(mUXsKykTgB("gu_tmzpxqd::yqzg_efmfq", 12), state)
		ui_handler.menu_open = state
	end
end)

ui_handler.group = function (tab, container) return elemence.group(tab, container) end

ui_handler.format = utils.format

ui_handler.reference = function (tab, container, name)
	local found = { contend(ui.reference, 3, tab, container, name) }
	local total, hidden = #found, false

	if string.lower(tab) == mUXsKykTgB("kgqa", 24) and string.lower(container) == mUXsKykTgB("ugvvkpiu", 2) then
		for i, v in ipairs(elemence.hidden_refs) do
			if string.find(name, mUXsKykTgB("^", 8) ..v) then hidden = true break end
		end
	end

	for i, v in ipairs(found) do
		found[i] = elemence.new(v, {
			__ref = true, __hidden = hidden or nil,
			__tab = tab, __container = container,
			__rage = container == mUXsKykTgB("Rzdsfk", 17) or nil,
		})
	end

	if total > 1 then local shift = 0
		for i = 1, total > 4 and total or 4, 2 do
			local m, j = i - shift, i + 1 - shift
			if found[j] and (found[j].type == mUXsKykTgB("szevpj", 11) or found[j].type == mUXsKykTgB("jvsvy_wpjrly", 7)) then
				local addition = found[j].type == mUXsKykTgB("iurux_voiqkx", 6) and mUXsKykTgB("wifil", 20) or mUXsKykTgB("lsxoic", 4)
				registry[ found[m].ref ].__addon, found[m][addition] = addition, found[j]

				table.remove(found, j) shift = shift + 1
			end
		end return unpack(found)
	else return found[1] end
end

ui_handler.traverse = function (t, f, p)
	p = p or {}

	if type(t) == mUXsKykTgB("zghrk", 6) and t.__name ~= mUXsKykTgB("es_rkxnvob::ovowoxd", 10) and t[#t] ~= mUXsKykTgB("~", 6) then
		for k, v in next, t do
			local np = table.copy(p); np[#np+1] = k
			ui_handler.traverse(v, f, np)
		end
	else
		f(t, p)
	end
end

do
	local save = function (config, ...)
		local packed = {}

		ui_handler.traverse(dirs.extract(config, {...}), function (ref, path)
			local value
			local etype = registry[ref].type

			if etype == mUXsKykTgB("pbybe_cvpxre", 13) then
				value = mUXsKykTgB("#", 7).. utils.rgb_to_hex{ ui.get(ref) }
			elseif etype == mUXsKykTgB("elqhbv", 23) then
				local _, mode, key = ui.get(ref)
				value = {mode, key or 0}
			else
				value = ui.get(ref)
			end

			if type(value) == mUXsKykTgB("ubcmf", 1) then value[#value+1] = mUXsKykTgB("~", 2) end
			dirs.pave(packed, value, path)
		end)
		
		return packed
	end

	local load = function (config, package, ...)
		if not package then return end

		local packed = dirs.extract(package, {...})
		ui_handler.traverse(dirs.extract(config, {...}), function (ref, path)
			pcall(function ()
				local value, proxy = dirs.find(packed, path), registry[ref]
				local vtype, etype = type(value), proxy.type
				local object = elements[etype]

				if vtype == mUXsKykTgB("qrpgle", 24) and value:sub(1, 1) == mUXsKykTgB("#", 2) then
					value, vtype = { utils.hex_to_rgb(value) }, mUXsKykTgB("ubcmf", 1)
				elseif vtype == mUXsKykTgB("ipqat", 15) and value[#value] == mUXsKykTgB("~", 7) then
					value[#value] = nil
				end

				if etype == mUXsKykTgB("ipulfz", 1) and value and type(value[1]) == mUXsKykTgB("yfxmpc", 11) then
					value[1] = elements.hotkey.enum[ value[1] ]
				end

				if object and object.type == vtype then
					if vtype == mUXsKykTgB("mtuex", 19) and etype ~= mUXsKykTgB("jriqfpbibzq", 23) then
						ui.set(ref, unpack(value))
						if etype == mUXsKykTgB("wifil_jcweyl", 20) then methods_mt.element.invoke(proxy.self) end
					else
						ui.set(ref, value)
					end
				else
					if proxy.__init then ui.set(ref, proxy.__init) end
				end
			end)
		end)
	end

	local package_mt = {
		__type = mUXsKykTgB("dr_qjwmuna::yjltjpn", 9), __metatable = false,
		__call = function (self, raw, ...)
			return (type(raw) == mUXsKykTgB("krscv", 17) and load or save)(self[0], raw, ...)
		end,
		save = function (self, ...) return save(self[0], ...) end,
		load = function (self, ...) load(self[0], ...) end,
	}	package_mt.__index = package_mt

	ui_handler.setup = function (t)
		local package = { [0] = {} }
		ui_handler.traverse(t, function (r, p) elemence.memorize(r, p, package[0]) end)
		return setmetatable(package, package_mt)
	end
end

methods_mt.element = {
	__type = mUXsKykTgB("lz_yreucvi::vcvdvek", 17), __name = mUXsKykTgB("jx_wpcsatg::tatbtci", 15), __metatable = false,
	__eq = function (this, that) return this.ref == that.ref end,
	__tostring = function (self) return string.format(mUXsKykTgB('wk_jcpfngt.%u[%f] "%u"', 2), self.type, self.ref, self.name) end,
	__call = function (self, ...) if #{...} > 0 then ui.set(self.ref, ...) else return ui.get(self.ref) end end,

	depend = function (self, ...)
		local arg = {...}
		local disabler = arg[1] == true

		local depend = registry[self.ref].depend[disabler and 2 or 1]
		local this = registry[self.ref].depend[0]

		for i = (disabler and 2 or 1), table.maxn(arg) do
			local v = arg[i]
			if v then
				if v.__name == mUXsKykTgB("gu_tmzpxqd::qxqyqzf", 12) then v = {v, true} end
				depend[#depend+1] = v

				local check = function () elemence.dependant(depend, this, disabler) end
				check()

				registry[v[1].ref].callbacks[#registry[v[1].ref].callbacks+1] = check
			end
		end

		return self
	end,

	override = function (self, value)
		local is_hk = self.type == mUXsKykTgB("ovarlf", 7)
		local ctx, wctx = registry[self.ref], ragebot.context[ragebot.ref.value]

		if value ~= nil then
			if not ctx.overridden then
				if is_hk then self.value = { ui.get(self.ref) } end
				if ctx.__rage then wctx[self.ref] = self.value else ctx.original = self.value end
			end ctx.overridden = true
			if is_hk then ui.set(self.ref, value[1], value[2]) else ui.set(self.ref, value) end
			if ctx.__rage then ctx.__ovr_v = value end
		else
			if ctx.overridden then
				local original = ctx.original if ctx.__rage then original, ctx.__ovr_v = wctx[self.ref], nil end
				if is_hk then ui.set(self.ref, elements.hotkey.enum[original[2]], original[3] or 0)
				else ui.set(self.ref, original) end ctx.overridden = false
			end
		end
	end,
	get_original = function (self)
		if registry[self.ref].__rage then
			if registry[self.ref].overridden then return ragebot.context[ragebot.ref.value][self.ref] else return self.value end
		else
			if registry[self.ref].overridden then return registry[self.ref].original else return self.value end
		end
	end,

	set = function (self, ...)
		if self.type == mUXsKykTgB("myvyb_zsmuob", 10) then
			ui.set(self.ref, unpack(utils.unpack_color(...)) )
			methods_mt.element.invoke(self)
		elseif self.type == mUXsKykTgB("wlmpw", 11) then
			local t = utils.format(...)
			ui.set(self.ref, t)
			self.value = t
		else
			ui.set(self.ref, ...)
		end
	end,
	get = function (self, value)
		if value and self.type == mUXsKykTgB("iqhpeoahayp", 22) then
			return registry[self.ref].options[value] or false
		end
		return ui.get(self.ref)
	end,

	reset = function (self) if registry[self.ref].__init then ui.set(self.ref, registry[self.ref].__init) end end,

	update = function (self, t)
		ui.update(self.ref, t)
		registry[self.ref].__list = t

		local cap = #t-1
	end,

	get_list = function (self) return registry[self.ref].__list end,

	get_color = function (self)
		if registry[self.ref].__addon then return ui.get(self.color.ref) end
	end,
	set_color = function (self, ...)
		if registry[self.ref].__addon then methods_mt.element.set(self.color, ...) end
	end,
	get_hotkey = function (self)
		if registry[self.ref].__addon then return ui.get(self.hotkey.ref) end
	end,
	set_hotkey = function (self, ...)
		if registry[self.ref].__addon then methods_mt.element.set(self.hotkey, ...) end
	end,

	is_reference = function (self) return registry[self.ref].__ref or false end,
	get_type = function (self) return self.type end,
	get_name = function (self) return self.name end,

	set_visible = function (self, visible)
		ui.set_visible(self.ref, visible)
		if registry[self.ref].__addon then ui.set_visible(self[registry[self.ref].__addon].ref, visible) end
	end,
	set_enabled = function (self, enabled)
		ui.set_enabled(self.ref, enabled)
		if registry[self.ref].__addon then ui.set_enabled(self[registry[self.ref].__addon].ref, enabled) end
	end,

	set_callback = function (self, func, once)
		if once == true then func(self) end
		registry[self.ref].callbacks[#registry[self.ref].callbacks+1] = func
	end,
	unset_callback = function (self, func)
		table.remove(registry[self.ref].callbacks, table.qfind(registry[self.ref].callbacks, func) or 0)
	end,
	invoke = function (self, ...)
		for i = 0, #registry[self.ref].callbacks do registry[self.ref].callbacks[i](self, ...) end
	end,

	set_event = function (self, event, func, condition)
		local slot = registry[self.ref]
		if condition == nil then condition = true end
		local is_cond_fn, latest = type(condition) == mUXsKykTgB("tibqhwcb", 14), nil
		slot.events[func] = function (this)
			local permission if is_cond_fn then permission = condition(this) else permission = this.value == condition end

			local action = permission and client.set_event_callback or client.unset_event_callback
			if latest ~= permission then action(event, func) latest = permission end
		end
		slot.events[func](self)
		slot.callbacks[#slot.callbacks+1] = slot.events[func]
	end,
	unset_event = function (self, event, func)
		client.unset_event_callback(event, func)
		methods_mt.element.unset_callback(self, registry[self.ref].events[func])
		registry[self.ref].events[func] = nil
	end,

	get_location = function (self) return registry[self.ref].tab, registry[self.ref].container end,
}	methods_mt.element.__index = methods_mt.element

methods_mt.group = {
	__name = mUXsKykTgB("lz_yreucvi::xiflg", 17),
	__metatable = false,
	__index = function (self, key) return rawget(methods_mt.group, key) or ui_handler_mt.__index(self, key) end,
	get_location = function (self) return self[1], self[2] end
}

do
	for k, v in next, elements do
		v.fn = function (origin, ...)
			local args, group = utils.dispense(k, origin, ...)
			local this = elemence.new( contend(ui[mUXsKykTgB("mdv_", 25).. k], 3, group[1], group[2], unpack(args, 1, args.n < args.req and args.n or args.req)), args.data )
	
			elemence.features(this, args.misc)
			return this
		end
	end

	ui_handler_mt.__name, ui_handler_mt.__metatable = mUXsKykTgB("jx_wpcsatg::qphtbtci", 15), false
	ui_handler_mt.__index = function (self, key)
		if not elements[key] then return ui[key] end
		if key == mUXsKykTgB("fgevat", 13) then return elemence.string end
	
		return elements[key].fn
	end
end

ragebot = {
	ref = ui_handler.reference(mUXsKykTgB("WFLJ", 5), mUXsKykTgB("Nvrgfe kpgv", 17), mUXsKykTgB("Owshgf lqhw", 18)),
	context = {}, cache = {},
	silent = false,
} do
	local previous, cycle_action = ragebot.ref.value, nil
	for i, v in ipairs(weapons) do ragebot.context[v], ragebot.cache[v] = {}, {} end

	local neutral = ui.reference(mUXsKykTgB("SBHF", 1), mUXsKykTgB("Ucgvin", 20), mUXsKykTgB("Dmzakdc", 25))
	ui.set_callback(neutral, function ()
		if not ragebot.silent then client.delay_call(0, client.fire_event, mUXsKykTgB("dr_qjwmuna::jmjycren_fnjyxw", 9), ragebot.ref.value, previous) end
		if cycle_action then cycle_action(ragebot.ref.value) end
	end)

	ragebot.cycle = function (fn, mute)
		cycle_action = mute and fn or nil
		ragebot.silent = mute and true or false

		for i, v in ipairs(weapons) do
			ragebot.ref:override(v)
		end

		ragebot.ref:override()
		cycle_action, ragebot.silent = nil, false
	end

	ui.set_callback(ragebot.ref.ref, function (self)
		ragebot.ref.value = ui.get(self)

		if not ragebot.silent and previous ~= ragebot.ref.value then
			for i = 1, #registry[self].callbacks, 1 do registry[self].callbacks[i](ragebot.ref) end
		end

		previous = ragebot.ref.value
	end)

	ragebot.memorize = function (self)
		local ctx = ragebot.context[ragebot.ref.value]

		if registry[self.ref].overridden then
			if ctx[self.ref] == nil then
				ctx[self.ref] = self.value
				methods_mt.element.override(self, registry[self.ref].__ovr_v)
			end
		else
			if ctx[self.ref] then
				methods_mt.element.set(self, ctx[self.ref])
				ctx[self.ref] = nil
			end
		end
	end
end

players = {
	elements = {}, list = {},
} do

	ui_handler.plist = elemence.group(mUXsKykTgB("GCRPVIJ", 17), mUXsKykTgB("Nqwhfgzragf", 13))
	ui_handler.plist.__plist = true

	local selected = 0
	local refs, slot = {
		list = ui_handler.reference(mUXsKykTgB("SODBHUV", 3), mUXsKykTgB("Vrgekxy", 6), mUXsKykTgB("Vrgekx royz", 6)),
		reset = ui_handler.reference(mUXsKykTgB("DZOMSFG", 14), mUXsKykTgB("Vrgekxy", 6), mUXsKykTgB("Viwix epp", 4)),
		apply = ui_handler.reference(mUXsKykTgB("BXMKQDE", 12), mUXsKykTgB("Orxighasbhg", 14), mUXsKykTgB("Fuuqd yt fqq", 5)),
	}, {}

	local slot_mt = {
		__type = mUXsKykTgB("ft_slyowpc::awljpc_dwze", 11), __metatable = false,
		__tostring = function (self)
			return string.format(mUXsKykTgB("iw_vobrzsf::dzomsf_gzch[%r] ct %g", 14), self.idx, methods_mt.element.__tostring(registry[self.ref].self))
		end,
		set = function (self, ...) 
			local ctx, value = registry[self.ref], {...}

			local is_colorpicker = ctx.type == mUXsKykTgB("jvsvy_wpjrly", 7)
			if is_colorpicker then
				value = utils.unpack_color(...)
			end

			if self.idx == selected then
				ui.set( self.ref, unpack(value) )
				if is_colorpicker then
					methods_mt.element.invoke(ctx.self)
				end
			else
				self.value = is_colorpicker and value or unpack(value)
			end
		end,
		get = function (self, find)
			if find and registry[self.ref].type == mUXsKykTgB("zhygvfryrpg", 13) then
				return table.qfind(self.value, find) ~= nil
			end

			if registry[self.ref].type ~= mUXsKykTgB("wifil_jcweyl", 20) then return self.value
			else return unpack(self.value) end
		end,
	}	slot_mt.__index = slot_mt

	players.traverse = function (fn) for i, v in ipairs(players.elements) do fn(v) end end

	slot = {
		select = function (idx)
			for i, v in ipairs(players.elements) do
				methods_mt.element.set(v, v[idx].value)
			end
		end,
		add = function (idx)
			for i, v in ipairs(players.elements) do
				local default = ternary(registry[v.ref].__init ~= nil, registry[v.ref].__init, v.value)
				v[idx], players.list[idx] = setmetatable({
					ref = v.ref, idx = idx, value = default
				}, slot_mt), true
			end
		end,
		remove = function (idx)
			for i, v in ipairs(players.elements) do
				v[idx], players.list[idx] = nil, nil
			end
		end,
	}

	players.slot_update = function (self)
		if self[selected] then self[selected].value = self.value
		else slot.add(selected) end
	end

	local silent = false
	local update = function (e)
		selected = ui.get(refs.list.ref)

		local new, old = entity.get_players(), players.list
		local me = entity.get_local_player()

		for idx, v in next, old do
			if entity.get_classname(idx) ~= mUXsKykTgB("LLBYujhna", 9) then
				slot.remove(idx)
			end
		end

		for i, idx in ipairs(new) do
			if idx ~= me and not players.list[idx] and entity.get_classname(idx) == mUXsKykTgB("JJZWshfly", 7) then
				slot.add(idx)
			end
		end

		if not silent and not e.value then
			for i = #new, 1, -1 do
				if new[i] ~= me then ui.set(refs.list.ref, new[i]) break end
			end
			client.update_player_list()
			silent = true
		else
			silent = false
		end

		slot.select(selected)
		client.fire_event(mUXsKykTgB("lz_yreucvi::gczjk_lgurkv", 17), selected)
	end

	do
		local function once ()
			update{}
			client.unset_event_callback(mUXsKykTgB("qsf_sfoefs", 1), once)
		end
		client.set_event_callback(mUXsKykTgB("xzm_zmvlmz", 8), once)
	end
    
	methods_mt.element.set_callback(refs.list, update, true)
	client.set_event_callback(mUXsKykTgB("sodbhu_frqqhfw_ixoo", 3), update)
	client.set_event_callback(mUXsKykTgB("hdsqwj_vakugffwul", 18), update)
	client.set_event_callback(mUXsKykTgB("wshfly_zwhdulk", 7), update)
	client.set_event_callback(mUXsKykTgB("eapntg_heplc", 15), update)
	client.set_event_callback(mUXsKykTgB("vrgekx_jkgzn", 6), update)
	client.set_event_callback(mUXsKykTgB("qmbzfs_ufbn", 1), update)

	methods_mt.element.set_callback(refs.apply, function ()
		players.traverse(function (v)
			for idx, _ in next, players.list do
				v[idx].value = v[selected].value
			end
		end)
	end)

	methods_mt.element.set_callback(refs.reset, function ()
		players.traverse(function (v)
			for idx, _ in next, players.list do
				if idx == selected then
					slot_mt.set(v[idx], registry[v.ref].__init)
				else
					v[idx].value = registry[v.ref].__init
				end
			end
		end)
	end)
end

local config, package, aa_config, aa_package
local 
	img,
	files,
	widgets,
	presets,
	protected,
	animations,
	shot_logger,
	fast_ladder,
	aero_lag_exp,
	chat_spammer,
	death_spammer,
	model_breaker,
	config_system,
	gamesense_refs,
	antiaim_on_use,
	anti_bruteforce,
	crosshair_logger,
	watermark,
	screen_indication,
	manual_indication,
	conditional_antiaims,
	expres = {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}

local slow_jitter_time = 0
local slow_jitter_direction = 1
local slow_jitter_current_pitch = 39
	

ffi.cdef[[
    void* __stdcall URLDownloadToFileA(void* LPUNKNOWN, const char* LPCSTR, const char* LPCSTR2, int a, int LPBINDSTATUSCALLBACK);  
    bool DeleteUrlCacheEntryA(const char* lpszUrlName);
    
    typedef struct c_con_command_base {
        void *vtable;
        void *next;
        bool registered;
        const char *name;
        const char *help_string;
        int flags;
        void *s_cmd_base;
        void *accessor;
    } c_con_command_base;
]]

files.exist = function(path)
    if readfile(path) == nil then
        return false
    end
    return true
end

protected.database = {
	configs = mUXsKykTgB(':zoxzhbatfkdp::zlkcfdp:', 23)
}

local information = { user = database.read(mUXsKykTgB('xkdbiyxpb_tqc^^', 23)) ~= nil and database.read(mUXsKykTgB('dqjhoedvh_zwi^^', 3)).username or fromdb.username, version = _DEBUG and mUXsKykTgB('wxunz', 19) or mUXsKykTgB('axkt', 15) }
local group = ui_handler.group(mUXsKykTgB('EE', 4), mUXsKykTgB('Nagv-nvzobg natyrf', 13))


local tab = group:combobox(mUXsKykTgB('\v⛧mdqtktcj\r ~ ', 8) .. information.version, {mUXsKykTgB('Yfdv', 17), mUXsKykTgB('Plvf', 3), mUXsKykTgB('FF', 5), mUXsKykTgB('Qvjgt', 2)})
group:label(mUXsKykTgB(' ', 4))

conditional_antiaims.conditions_names = {mUXsKykTgB('Ixqhut', 16), mUXsKykTgB('Xyfsinsl', 5), mUXsKykTgB('Tvcpun', 7), mUXsKykTgB('Zsvddhsr', 7), mUXsKykTgB('Vkhnva', 19), mUXsKykTgB('Jlsfkd & Zolrze', 23), mUXsKykTgB('Fnw', 5), mUXsKykTgB('Tbk & Vkhnva', 19), mUXsKykTgB('Cobbpqxka', 23), mUXsKykTgB('Ytdxetz', 19), mUXsKykTgB('Lk Rpb', 23)}
local Vars = {
    Home = {
		group:label(mUXsKykTgB('[\vneruludk\r] Rwoxavjcrxw', 9)),
		group:label(mUXsKykTgB('Dbna: \v', 9) .. information.user),
		group:label(mUXsKykTgB('Etlm niwtmx: \v93.95.14', 19)),
		group:label(mUXsKykTgB(' ', 2)),
		group:label(mUXsKykTgB('[\vjanqhqzg\r] Uwjxjyx', 5)),

		list = group:listbox(mUXsKykTgB('Wihzcam', 20), mUXsKykTgB('', 4), false),
		name = group:textbox(mUXsKykTgB('Gsrjmk reqi', 4), mUXsKykTgB('', 6), false),
		load = group:button(mUXsKykTgB('Wzlo', 11), function() end),
		save = group:button(mUXsKykTgB('Owra', 22), function() end),
		delete = group:button(mUXsKykTgB('Novodo', 10), function() end),
		import = group:button(mUXsKykTgB('Jnqpsu gspn dmjqcpbse', 1), function() end),
		export = group:button(mUXsKykTgB('Xqihkm mh vebiuhtkw', 19), function() end)
    },

    Misc = {
		group:label(mUXsKykTgB('[\vgxknenwd\r] Xkuwcnu', 2)),
		screen_indicators = group:checkbox(mUXsKykTgB('•  Rbqddm hmchbzsnqr', 25), { 142, 165, 255 }),
		screen_indicators_settings = {
			glow = group:checkbox(mUXsKykTgB('Pukpjhavyz  »  Nsvd ilopuk', 7))
		},
		screen_indicators_settings_dmg = _DEBUG and group:checkbox(mUXsKykTgB('•  Okp. fcocig cdqxg etquujckt', 2)) or nil,
		manual_arrows = group:checkbox(mUXsKykTgB('•  Bpcjpa pggdlh', 15)),
		manual_arrows_settings = {
			settings = group:combobox(mUXsKykTgB('Offckg  »  Gshhwbug', 14) , {mUXsKykTgB('Mnojduc', 9), mUXsKykTgB('Chpcwnom', 20), mUXsKykTgB('Whdpvnhhw', 3)}),
			adding = group:slider(mUXsKykTgB('Izzwea  »  Illqvo\nLMN', 8), 0, 100, 35),
			accent_color = group:label(mUXsKykTgB('Evvsaw  »  Eggirx gspsv\nHIJ', 4), { 113, 138, 187 }),
			teamskeet_adding = group:slider(mUXsKykTgB('Gxxucy  »  Gjjotm\nZY', 6), 4, 100, 42),
			teamskeet_accent_color = group:label(mUXsKykTgB('Pggdlh  »  Prrtci rdadg\nIH', 15), { 175, 255, 0 }),
			teamskeet_desync_accent_color = group:label(mUXsKykTgB('Zqqnvr  »  Cdrxmb bnknq\nSR', 25), { 0, 200, 255 }),

			invictus_dynamic = group:checkbox(mUXsKykTgB("Tkkhpl  »  Wrgtfbv fhwx", 19)),

			getzeus_adding = group:slider(mUXsKykTgB('Wnnkso  »  Wzzejc\nCP', 22), 4, 100, 44),
			getzeus_accent_color = group:label(mUXsKykTgB('Duurzv  »  Dffhqw froru\nJW', 3), { 111, 111, 220 }),
			getzeus_second_accent_color = group:label(mUXsKykTgB('Neebjf  »  Frpbaq pbybe\nTG', 13), { 255, 255, 255 })
		},

		branded_watermark = group:checkbox(mUXsKykTgB('•  Dtcpfgf ycvgtoctm', 2), {142, 165, 229, 85}),

	

		alt_watermark = group:combobox(mUXsKykTgB('•  Epxivrexmzi aexivqevo', 4), {mUXsKykTgB('Lbfiex', 19), mUXsKykTgB('Vxmnaw', 9)}),
		alt_watermark_settings = {
			color = group:color_picker(mUXsKykTgB('Kvd  »  Zycsdsyx', 10), 142, 165, 229, 85),
			pos = group:combobox(mUXsKykTgB('Grz  »  Vuyozout', 6), {mUXsKykTgB('Gtyytr', 5), mUXsKykTgB('Rklz', 6), mUXsKykTgB('Xomnz', 6)}),
			nosp = group:checkbox(mUXsKykTgB('Hsa  »  Yltvcl zwhjlz', 7)),
		},

		watermark = group:checkbox(mUXsKykTgB('•  Vzsdqlzqj', 25)),
		watermark_settings = {
        color = group:color_picker(mUXsKykTgB('Gkdobwkbu  »  Kmmoxd myvyb', 10), 142, 165, 255, 255)
		},

		visual_effects = {
			svastika_effect = group:checkbox(mUXsKykTgB('•  Cfuufs dspttibjs', 1))
		},



		crosshair_hitlog = _DEBUG and group:checkbox(mUXsKykTgB('•  Pebffunve ybttre', 13)) or nil,
		crosshair_settings = 
		_DEBUG and {
			move = group:checkbox(mUXsKykTgB('Etquujckt  »  Oqxg ykvj ueqrg', 2)),
			hit_color = group:label(mUXsKykTgB('Nczddsltc  »  Ste nzwzc', 11), { 142, 165, 255 }),
			miss_color = group:label(mUXsKykTgB('Qfcggvowf  »  Awgg qczcf', 14), { 255, 180, 0 }),	
		} or nil,

		hitlog_console = _DEBUG and group:checkbox(mUXsKykTgB('•  Vhglhex ehzzxk', 19)) or nil,
		hitlogger_settings = 
		_DEBUG and {
			prefix_color = group:label(mUXsKykTgB('Iutyurk  »  Vxklod iurux', 6), { 142, 165, 255 }),
			hit_color = group:label(mUXsKykTgB('Lxwbxun  »  Qrc lxuxa', 9), { 159, 202, 43 }),
			miss_color = group:label(mUXsKykTgB('Htsxtqj  »  Rnxx htqtw', 5), { 255, 180, 0 }),	
		} or nil,

		group:label(mUXsKykTgB(' ', 5)),
		group:label(mUXsKykTgB('[\vofsvmvel\r] Wscmovvkxoyec', 10)),
		fast_ladder = group:checkbox(mUXsKykTgB('•  Mhza shkkly', 7)),
		fast_ladder_settings = {
			group:multiselect(mUXsKykTgB('Mhza shkkly  »  Zlaapunz', 7) , {mUXsKykTgB('Yqaclbgle', 24), mUXsKykTgB('Uvjtveuzex', 17)})
		},
		chat_spammer = group:checkbox(mUXsKykTgB('•  Nsle dalxxpc', 11)),
		deathsay = group:checkbox(mUXsKykTgB('•  Ut jkgzn yvgsskx', 6)),
		anim_breakers = group:checkbox(mUXsKykTgB('\aY9Y998CC•  Xkfjxqflk yobxhbop', 23)),
		anim_breakers_settings = {
			breaked_legs = group:combobox(mUXsKykTgB('Rezdrkzfej  »  Sivrbvu cvxj', 17), {mUXsKykTgB('Wbltuexw', 19), mUXsKykTgB('Cdkdsm', 10), mUXsKykTgB('Wvggre', 13), mUXsKykTgB('Fibbwbu', 14), mUXsKykTgB('Akdmc', 25)}),
			air = group:combobox(mUXsKykTgB('Tgbftmbhgl  »  Tbk exzl', 19), {mUXsKykTgB('Joyghrkj', 6), mUXsKykTgB('Hipixr', 15), mUXsKykTgB('Lohhcha', 20)}),
			pitch = group:checkbox(mUXsKykTgB('Fsnrfyntsx  »  Unyhm ts qfsi', 5))
		},
		experimental_resolver = _DEBUG and group:checkbox(mUXsKykTgB('\aBB2222BB⚠  Atlaneiajpwh naokhran', 22)) or nil,
		group:label(mUXsKykTgB(' ', 8)),
		group:label(mUXsKykTgB('[\vlcpsjsbi\r] Jshuahn', 7)),
		clantag_changer = group:checkbox(mUXsKykTgB('•  Hqfsyfl hmfsljw', 5)),
		clantag_settings = {
			preset = group:combobox(mUXsKykTgB('Fodqwdj  »  Suhvhw', 3), {mUXsKykTgB('sjwzqzip', 14), mUXsKykTgB('tkbkxruyk', 6), mUXsKykTgB('san.lxmnb', 9)})
		},
		
    },

    AA = {
		group:label(mUXsKykTgB('[\vzqdgxgpw\r] Nzoodibn', 21)),
		enable = group:checkbox(mUXsKykTgB('•  Hqdeoh dqwl-dlpv', 3)),
		edge_yaw = group:checkbox(mUXsKykTgB('•  Cbec wyu', 24), 0x00),
		freestanding = group:checkbox(mUXsKykTgB('•  Iuhhvwdqglqj', 3), 0x00),
		avoid_backstab = _DEBUG and group:checkbox(mUXsKykTgB('•  Mhaup nmowefmn', 12)) or nil,
		manuals = {
			enable = group:checkbox(mUXsKykTgB('•  Fobcmf nbovbmt', 1)),
			left = group:hotkey(mUXsKykTgB('Lzmtzkr  »  Kdes rhcd', 25), true, 0x00),
			right = group:hotkey(mUXsKykTgB('Pdqxdov  »  Uljkw vlgh', 3), true, 0x00),
			reset = group:hotkey(mUXsKykTgB('Esfmsdk  »  Jwkwl kavwk', 18), true, 0x00),
			inverter = group:checkbox(mUXsKykTgB('Drelrcj  »  Zemvikvi', 17)),
			manuals_over_fs = group:checkbox(mUXsKykTgB('Qeryepw  »  Sziv JW', 4)),
			lag_options = group:combobox(mUXsKykTgB('Jxkrxip  »  Clozb abcbkpfsb\njxkrxip', 23) , {mUXsKykTgB('Mnojduc', 9), mUXsKykTgB('Grcgey ut', 6)}),
			defensive_aa = group:checkbox(mUXsKykTgB('Guhoufm  »  Xyzyhmcpy UU\nguhoufm', 20)),
			defensive_pitch = group:combobox(mUXsKykTgB('Pdqxdov  »  Slwfk\nghihqvlyh_slwfk\npdqxdov', 3), {mUXsKykTgB('Rwgopzsr', 14), mUXsKykTgB('Sn', 24), mUXsKykTgB('Kpcz', 11), mUXsKykTgB('Pylbmk', 24), mUXsKykTgB('Iayzus', 6), mUXsKykTgB('Ngjr edoozm', 21)}),
			pitch_slider = group:slider(mUXsKykTgB('\nxpnojh_yzazindqz_kdoxc\nhvipvgn', 21), -89, 89, 0),
			defensive_yaw = group:combobox(mUXsKykTgB('Kylsyjq  »  Wyu\nbcdclqgtc_wyu\nkylsyjq', 24), {mUXsKykTgB('Ydnvwgzy', 21), mUXsKykTgB('Gwrskomg', 14), mUXsKykTgB('Zaazdtep', 11), mUXsKykTgB('Xgtjus', 6), mUXsKykTgB('Olej', 22), mUXsKykTgB('1-Eig', 8), mUXsKykTgB('6-Xbz', 1), mUXsKykTgB('Hzxytr', 5)}),
			yaw_slider = group:slider(mUXsKykTgB('\nskijec_tuvudiylu_oqm\ncqdkqbi', 16), -180, 180, 0)
		
		},

		safe_functions = {
			enable = group:checkbox(mUXsKykTgB('•  Gpcdng uchg hwpevkqpu', 2)),
			settings = group:multiselect(mUXsKykTgB('Weji  »  Jyrgxmsrw', 4), table.filter {_DEBUG and mUXsKykTgB('Fcyb', 24) or nil, mUXsKykTgB('Jmhed', 25), mUXsKykTgB('Nsig', 14)}),
			head_settings = group:multiselect(mUXsKykTgB('Jrwv  »  Yvru jvkkzexj', 17), {mUXsKykTgB('Qnrpqc', 9), mUXsKykTgB('Jkij Fkuvcpeg', 2)}),
			lag_options = group:combobox(mUXsKykTgB('Emrq  »  Radoq pqrqzeuhq\nemrq', 12) , {mUXsKykTgB('Lmnictb', 8), mUXsKykTgB('Hsdhfz vu', 7)}),
			defensive_aa = group:checkbox(mUXsKykTgB('Weji  »  Hijirwmzi EE\nweji', 4)),
			defensive_pitch = group:combobox(mUXsKykTgB('Ltyx  »  Ibmva\nwxyxglbox_ibmva\nltyx', 19), {mUXsKykTgB('Chrzakdc', 25), mUXsKykTgB('Mh', 18), mUXsKykTgB('Bgtq', 2), mUXsKykTgB('Jsfvge', 18), mUXsKykTgB('Womnig', 20), mUXsKykTgB('Fybj wvggre', 13)}),
			pitch_slider = group:slider(mUXsKykTgB('\nnfdezx_opqpydtgp_atens\ndlqp', 11), -89, 89, 0),
			defensive_yaw = group:combobox(mUXsKykTgB('Hput  »  Npl\nstutchxkt_npl\nhput', 15), {mUXsKykTgB('Rwgopzsr', 14), mUXsKykTgB('Hxstlpnh', 15), mUXsKykTgB('Ijjimcny', 20), mUXsKykTgB('Udqgrp', 3), mUXsKykTgB('Khaf', 18), mUXsKykTgB('9-Cge', 6), mUXsKykTgB('9-Uyw', 24), mUXsKykTgB('Dvtupn', 1)}),
			yaw_slider = group:slider(mUXsKykTgB('\nzrpqlj_abcbkpfsb_vxt\npxcb', 23), -180, 180, 0)
		},

		air_exploit = _DEBUG and {
			enable = group:checkbox(mUXsKykTgB('\aP0P009TT•  Sbopzs osfcpwq sldzcwh', 14), 0x00),
			while_visible = group:checkbox(mUXsKykTgB('Rkcybvg  »  Juvyr rarzl ivfvoyr', 13)),
			exp_tick = group:slider(mUXsKykTgB('Ibtpsmx  »  Hipec', 4), 2, 30, 10, 1, mUXsKykTgB('q', 23))
		} or nil,

		empty_list = group:label(mUXsKykTgB(' ', 6)),
		Settings = {
			group:label(mUXsKykTgB('[\vmdqtktcj\r] Jcqtlmz', 8)),
			condition_combo = group:combobox(mUXsKykTgB('Hipit', 15), conditional_antiaims.conditions_names)
		}
    },
	
	Other = {
		group:label(mUXsKykTgB('[\vlcpsjsbi\r] Vaoly', 7)),
		
		lethal_force_baim = group:checkbox(mUXsKykTgB('•  Dmpac zygk gd jcrfyj', 24)),
		
		
		group:label(mUXsKykTgB(' ', 8)),
		group:label(mUXsKykTgB('[\vctgjajsz\r] Zsw zmr', 24)),
		buybot = group:checkbox(mUXsKykTgB('•  Nwjkun', 9)),
		buybot_primary = group:combobox(mUXsKykTgB('Zbswkbi gokzyx', 10), mUXsKykTgB("Nhgb", 13), mUXsKykTgB("Fpbhg", 13), mUXsKykTgB("Gcv", 6), mUXsKykTgB("Vxosgxe xolrk", 6), mUXsKykTgB("Blxynm aroun", 9), mUXsKykTgB("Lzbghmd ftm", 25)),
		buybot_secondary = group:combobox(mUXsKykTgB('Rdbnmczqx vdzonm', 25), mUXsKykTgB("Ghidxow slvwro", 3), mUXsKykTgB("U705", 5), mUXsKykTgB("Sjpa Qtgtiiph", 15), mUXsKykTgB("Oljkw slvwro", 3), mUXsKykTgB("Tqmhk buefax", 12)),
		buybot_utility = group:multiselect(mUXsKykTgB('Cbqtqbg', 8), mUXsKykTgB("Epclybc", 24), mUXsKykTgB("Uoqmg", 2), mUXsKykTgB("Mrgirhmevc", 4), mUXsKykTgB("Vbqixrqdw", 16), mUXsKykTgB("Nhyodu + Khophw", 3), mUXsKykTgB("Fghwug mkv", 2), mUXsKykTgB("Otjh", 15)),
		group:label(mUXsKykTgB(' ', 4)),
		group:label(mUXsKykTgB(' ', 1)),
		group:label(mUXsKykTgB(' ', 6)),
		unlock_hidden_cvars = group:button(mUXsKykTgB('Vompdl ijeefo dwbst', 1), function() show_hidden_cvars(true) end),
		
	}
}

config_system.get = function(name)
    local database = database.read(protected.database.configs) or {}

    for i, v in pairs(database) do
        if v.name == name then
            return {
                config = v.config,
				config2 = v.config2,
                index = i
            }
        end
    end

    for i, v in pairs(presets) do
        if v.name == name then
            return {
                config = v.config,
				config2 = v.config2,
                index = i
            }
        end
    end

    return false
end

config_system.save = function(name)
    local db = database.read(protected.database.configs) or {}
    local config = {}

    if name:match(mUXsKykTgB('[^%v]', 25)) ~= nil then
        return
    end

	local config = base64.encode(json.stringify(package:save()))
	local config2 = base64.encode(json.stringify(aa_package:save()))

	local cfg = config_system.get(name)

    if not cfg then
        table.insert(db, { name = name, config = config, config2 = config2 })
    else
		db[cfg.index].config = config
        db[cfg.index].config2 = config2
    end

    database.write(protected.database.configs, db)
end

config_system.delete = function(name)
    local db = database.read(protected.database.configs) or {}

    for i, v in pairs(db) do
        if v.name == name then
            table.remove(db, i)
            break
        end
    end

    for i, v in pairs(presets) do
        if v.name == name then
            return false
        end
    end

    database.write(protected.database.configs, db)
end

config_system.config_list = function()
    local database = database.read(protected.database.configs) or {}
    local config = {}

    for i, v in pairs(presets) do
        table.insert(config, v.name)
    end

    for i, v in pairs(database) do
        table.insert(config, v.name)
    end

    return config
end

local function typeFromString(input)
    if type(input) ~= mUXsKykTgB('jkizex', 17) then return input end

    local value = input:lower()

    if value == mUXsKykTgB('hfis', 14) then
        return true
    elseif value == mUXsKykTgB('cxipb', 23) then
        return false
    elseif tonumber(value) ~= nil then
        return tonumber(value)
    else
        return tostring(input)
    end
end

config_system.load_settings = function(e, e2)
	package:load(e)
	aa_package:load(e2)
end

config_system.import_settings = function()
    local frombuffer = clipboard.get()
    if not frombuffer or frombuffer == mUXsKykTgB('', 6) then
        print(mUXsKykTgB('Yhelxkwnz eo ailpu', 22))
        return
    end
    
    local decoded = base64.decode(frombuffer)
    if not decoded or decoded == mUXsKykTgB('', 9) then
        print(mUXsKykTgB('Avdgzy oj yzxjyz xgdkwjvmy yvov', 21))
        return
    end
    
    local success, config = pcall(json.parse, decoded)
    if not success or not config or not config.config or not config.config2 then
        print(mUXsKykTgB('Kfnqji yt ufwxj hqnugtfwi ifyf', 5))
        return
    end
    
    config_system.load_settings(config.config, config.config2)
end

config_system.export_settings = function(name)
    local config = { config = package:save(), config2 = aa_package:save() }
    local toExport = base64.encode(json.stringify(config))
    clipboard.set(toExport)
end

config_system.load = function(name)
    local fromDB = config_system.get(name)
    if not fromDB or not fromDB.config or not fromDB.config2 then
        print(mUXsKykTgB('Ugfxay fgl xgmfv: ', 18) .. name)
        return
    end
    
    local decoded1 = base64.decode(fromDB.config)
    local decoded2 = base64.decode(fromDB.config2)
    
    if not decoded1 or not decoded2 then
        print(mUXsKykTgB('Ytbexw mh wxvhwx vhgybz wtmt', 19))
        return
    end
    
    local success1, config1 = pcall(json.parse, decoded1)
    local success2, config2 = pcall(json.parse, decoded2)
    
    if not success1 or not success2 then
        print(mUXsKykTgB('Avdgzy oj kvmnz xjiadb yvov', 21))
        return
    end
    
    config_system.load_settings(config1, config2)
end

Vars.Home.list:set_callback(function(value)
    if value == nil then 
		return 
	end
    local name = mUXsKykTgB('', 6)
    
    local configs = config_system.config_list()
    if configs == nil then 
		return 
	end

    local index = value:get()
    if index and configs and configs[index + 1] then
        name = configs[index + 1]
    else
        name = mUXsKykTgB('', 4)
    end
    Vars.Home.name:set(name)
end)

Vars.Home.load:set_callback(function()
	local name = Vars.Home.name:get()
    if name == mUXsKykTgB('', 2) then return end

    local s, p = pcall(config_system.load, name)

    if s then
        name = name:gsub(mUXsKykTgB('*', 2), mUXsKykTgB('', 8))
        print(mUXsKykTgB('Lnvvxllyneer ehtwxw ', 19) .. name)
    else
        print(mUXsKykTgB('Kfnqji yt qtfi ', 5) .. name)
		print(mUXsKykTgB('Uvslx: ', 17), p)
    end
	
end)

Vars.Home.save:set_callback(function()			
	local name = Vars.Home.name:get()
	if name == mUXsKykTgB('', 2) then return end

	for i, v in pairs(presets) do
		if v.name == name:gsub(mUXsKykTgB('*', 8), mUXsKykTgB('', 5)) then
			print(mUXsKykTgB("Aqw ecp'v ucxg dwknv-kp rtgugv", 2))
			return
		end
	end

	if name:match(mUXsKykTgB('[^%f]', 9)) ~= nil then
		print(mUXsKykTgB('Lgorkj zu ygbk ', 6) .. name .. mUXsKykTgB(' ypz oj diqvgdy xcvmvxozmn', 21))
		return
	end

	local protected = function()
		config_system.save(name)
		Vars.Home.list:update(config_system.config_list())
	end

	if pcall(protected) then
		print(mUXsKykTgB('Mowwymmzoffs mupyx ', 20) .. name)
	else
		print(mUXsKykTgB('Jempih xs wezi ', 4) .. name)
	end
end)

Vars.Home.delete:set_callback(function()
    local name = Vars.Home.name:get()
    if name == mUXsKykTgB('', 5) then return end

    if config_system.delete(name) == false then
        print(mUXsKykTgB('Cxfiba ql abibqb ', 23) .. name)
        Vars.Home.list:update(config_system.config_list())
        return
    end

    for i, v in pairs(presets) do
        if v.name == name:gsub(mUXsKykTgB('*', 8), mUXsKykTgB('', 6)) then
            print(mUXsKykTgB('Eua igt`z jkrkzk haorz-ot vxkykz ', 6) .. name:gsub(mUXsKykTgB('*', 7), mUXsKykTgB('', 6)))
            return
        end
    end

    config_system.delete(name)

    Vars.Home.list:update(config_system.config_list())
    Vars.Home.list:set(presets and #presets or 0)
    local db_count = database.read(protected.database.configs)
    local preset_count = presets and #presets or 0
    local config_list = config_system.config_list()
    Vars.Home.name:set((db_count and #db_count == 0) and mUXsKykTgB("", 9) or (config_list and config_list[preset_count] or mUXsKykTgB("", 9)))
    print(mUXsKykTgB('Jlttvjjwlccp uvcvkvu ', 17) .. name)
end)

Vars.Home.import:set_callback(function()
	local protected = function()
        config_system.import_settings()
    end

    if pcall(protected) then
        print(mUXsKykTgB('Ikssuiivkbbo ycfehjut iujjydwi', 16))
    else
        print(mUXsKykTgB('Mhpslk av ptwvya zlaapunz', 7))
    end
end)

Vars.Home.export:set_callback(function()
    local name = Vars.Home.name:get()
    if name == mUXsKykTgB('', 5) then return end

    local protected = function()
        config_system.export_settings(name)
    end

    if pcall(protected) then
        print(mUXsKykTgB('Tvddfttgvmmz fyqpsufe tfuujoht', 1))
    else
        print(mUXsKykTgB('Idlohg wr hasruw vhwwlqjv', 3))
    end
end)

local function initDatabase()
    if database.read(protected.database.configs) == nil then
        database.write(protected.database.configs, {})
    end

    Vars.Home.list:update(config_system.config_list())
    
    Vars.Home.name:set(mUXsKykTgB('', 4))
end

initDatabase()

animations.base_speed = 0.095
animations._list = {}
animations.new = function(name, new_value, speed, init)
    speed = speed or animations.base_speed
    local is_color = type(new_value) == mUXsKykTgB("dbnamjcj", 9)
    local is_vector = type(new_value) == mUXsKykTgB("zigxsv", 4)

    if animations._list[name] == nil then
        animations._list[name] = (init and init) or (is_color and colors.white or 0)
    end

    local interp_func

    if is_vector then
        interp_func = math.vector_lerp
    elseif is_color then
        interp_func = math.color_lerp
    else
        interp_func = math.lerp
    end

    animations._list[name] = interp_func(animations._list[name], new_value, speed)
    
    return animations._list[name]
end

gamesense_refs.dmgOverride = {ui.reference(mUXsKykTgB('CLRP', 11), mUXsKykTgB('Nvzobg', 13), mUXsKykTgB('Iejeiqi zwiwca kranneza', 22))}
gamesense_refs.fakeDuck = ui.reference(mUXsKykTgB('KTZX', 19), mUXsKykTgB('Lqebo', 23), mUXsKykTgB('Izhp ujjp fxxnxy', 5))
gamesense_refs.minDmg = ui.reference(mUXsKykTgB('PYEC', 24), mUXsKykTgB('Ygkzmr', 24), mUXsKykTgB('Rnsnrzr ifrflj', 5))
gamesense_refs.hitChance = ui.reference(mUXsKykTgB('OXDB', 23), mUXsKykTgB('Ckodqv', 2), mUXsKykTgB('Yuzuygy tuf otmzoq', 12))
gamesense_refs.safePoint = ui.reference(mUXsKykTgB('KTZX', 19), mUXsKykTgB('Ygkzmr', 24), mUXsKykTgB('Qzcnp dlqp aztye', 11))
gamesense_refs.forceBaim = ui.reference(mUXsKykTgB('CLRP', 11), mUXsKykTgB('Goshuz', 6), mUXsKykTgB('Xgjuw tgvq sae', 18))
gamesense_refs.dtLimit = ui.reference(mUXsKykTgB('BKQO', 10), mUXsKykTgB('Nvzobg', 13), mUXsKykTgB('Nyelvo dkz pkuo vkq vswsd', 10))
gamesense_refs.quickPeek = {ui.reference(mUXsKykTgB('LUAY', 20), mUXsKykTgB('Hmaxk', 19), mUXsKykTgB('Osgai ncci yqqgqr', 24))}
gamesense_refs.dt = {ui.reference(mUXsKykTgB('YHNL', 7), mUXsKykTgB('Jrvkxc', 9), mUXsKykTgB('Pagnxq fmb', 12))}
gamesense_refs.flLimit = ui.reference(mUXsKykTgB('XX', 23), mUXsKykTgB('Vqau bqw', 16), mUXsKykTgB('Tquqb', 8))
gamesense_refs.os = {ui.reference(mUXsKykTgB('YY', 24), mUXsKykTgB('Joczm', 21), mUXsKykTgB('Fe jyfk rekz-rzd', 17))}
gamesense_refs.slow = {ui.reference(mUXsKykTgB('GG', 6), mUXsKykTgB('Nsgdq', 25), mUXsKykTgB('Atwe uwbqwv', 8))}
gamesense_refs.fakeLag = {ui.reference(mUXsKykTgB('LL', 11), mUXsKykTgB('Xscw dsy', 18), mUXsKykTgB('Fcgcn', 20))}
gamesense_refs.indicators = {ui.reference(mUXsKykTgB('XKUWCNU', 2), mUXsKykTgB('Zespc PDA', 11), mUXsKykTgB('Nmibczm qvlqkibwza', 8))}
gamesense_refs.ping = {ui.reference(mUXsKykTgB('SOYI', 6), mUXsKykTgB('Okuegnncpgqwu', 2), mUXsKykTgB('Slqj vslnh', 3))}
gamesense_refs.dt_fakelag = ui.reference(mUXsKykTgB('NWCA', 22), mUXsKykTgB('Ckodqv', 2), mUXsKykTgB('Hsyfpi xet jeoi pek pmqmx', 4))

gamesense_refs.leg_movement = ui_handler.reference(mUXsKykTgB('FF', 5), mUXsKykTgB('Inbyl', 20), mUXsKykTgB('Ohj pryhphqw', 3))
gamesense_refs.pitch, gamesense_refs.pitch_value = ui_handler.reference(mUXsKykTgB('NN', 13), mUXsKykTgB('Dqwl-dlperw dqjohv', 3), mUXsKykTgB('Gzkty', 17))
gamesense_refs.yaw_base = ui_handler.reference(mUXsKykTgB('VV', 21), mUXsKykTgB('Xkqf-xfjylq xkdibp', 23), mUXsKykTgB('Dfb gfxj', 5))
gamesense_refs.yaw_offset1, gamesense_refs.yaw_offset = ui_handler.reference(mUXsKykTgB('DD', 3), mUXsKykTgB('Mzfu-muynaf mzsxqe', 12), mUXsKykTgB('Fhd', 7))
gamesense_refs.body_yaw, gamesense_refs.body_yaw_offset = ui_handler.reference(mUXsKykTgB('NN', 13), mUXsKykTgB('Obhw-owapch obuzsg', 14), mUXsKykTgB('Uhwr rtp', 19))
gamesense_refs.doubletap, gamesense_refs.doubletap_config = ui_handler.reference(mUXsKykTgB('AJPN', 9), mUXsKykTgB('Ckodqv', 2), mUXsKykTgB('Fqwdng vcr', 2))
gamesense_refs.yaw_modifier, gamesense_refs.yaw_modifier_offset = ui_handler.reference(mUXsKykTgB('EE', 4), mUXsKykTgB('Rekz-rzdsfk rexcvj', 17), mUXsKykTgB('Suq dcnnyl', 20))
gamesense_refs.edge_yaw = ui_handler.reference(mUXsKykTgB('EE', 4), mUXsKykTgB('Cpvk-ckodqv cpingu', 2), mUXsKykTgB('Zybz tvr', 21))
gamesense_refs.freestanding = ui_handler.reference(mUXsKykTgB('HH', 7), mUXsKykTgB('Qdjy-qycrej qdwbui', 16), mUXsKykTgB('Htgguvcpfkpi', 2))
gamesense_refs.freestanding_key = { ui_handler.reference(mUXsKykTgB('GG', 6), mUXsKykTgB('Zmsh-zhlans zmfkdr', 25), mUXsKykTgB('Kwjjxyfsinsl', 5)) }
gamesense_refs.roll_aa = ui_handler.reference(mUXsKykTgB('MM', 12), mUXsKykTgB('Rekz-rzdsfk rexcvj', 17), mUXsKykTgB('Hebb', 16))
gamesense_refs.prefer_safe_point = ui.reference(mUXsKykTgB('JSYW', 18), mUXsKykTgB('Jrvkxc', 9), mUXsKykTgB('Qsfgfs tbgf qpjou', 1))
gamesense_refs.force_safe_point = ui.reference(mUXsKykTgB('KTZX', 19), mUXsKykTgB('Rzdsfk', 17), mUXsKykTgB('Ktwhj xfkj utnsy', 5))
gamesense_refs.rage_enable = ui_handler.reference(mUXsKykTgB('SBHF', 1), mUXsKykTgB('Kswlyd', 10), mUXsKykTgB('Irefpih', 4))
gamesense_refs.duck_peek = ui_handler.reference(mUXsKykTgB('FOUS', 14), mUXsKykTgB('Puifs', 1), mUXsKykTgB('Hygo tiio ewwmwx', 4))

gamesense_refs._vars = {}

for k, v in pairs(gamesense_refs) do
    if k ~= mUXsKykTgB("_chyz", 7) then
        gamesense_refs._vars[k] = {
            tick = -1,
            var = v
        }
    end
end

gamesense_refs.override = function(name, value)
    local var = gamesense_refs._vars[name]

    if var == nil then
        return
    end

    if type(value) == mUXsKykTgB("lstdw", 18) and value._len then
        value._len = nil
    end

    var.var:override(value)
    
    var.tick = globals.tickcount()

    return var.var
end

local safecall = function(name, report, f)
    return function(...)
        local s, ret = pcall(f, ...)

        if not s then
            local retmessage = mUXsKykTgB("weji gepp jempih [", 4) .. name .. mUXsKykTgB("] -> ", 3) .. ret

            if report then
                print(retmessage)
            end

            return false, retmessage
        else
            return ret, s
        end
    end
end

expres.get_prev_simtime = function(ent)
    local ent_ptr = native_GetClientEntity(ent)    
	if ent_ptr ~= nil then 
		return ffi.cast(mUXsKykTgB('bhkwp*', 22), ffi.cast(mUXsKykTgB('thmsosq_s', 25), ent_ptr) + 0x26C)[0] 
	end
end

expres.restore = function ()
	for i = 1, 64 do plist.set(i, mUXsKykTgB("Irufh ergb bdz", 3), false) end
end

if Vars.Misc.experimental_resolver then
	defer(expres.restore)
	Vars.Misc.experimental_resolver:set_callback(function (this)
		if not this.value then expres.restore() end
	end)
end

expres.body_yaw, expres.eye_angles = {}, {}

expres.get_max_desync = function (animstate)
	local speedfactor = math.clamp(animstate.feet_speed_forwards_or_sideways, 0, 1)
	local avg_speedfactor = (animstate.stop_to_full_running_fraction * -0.3 - 0.2) * speedfactor + 1

	local duck_amount = animstate.duck_amount
	if duck_amount > 0 then
		avg_speedfactor = avg_speedfactor + (duck_amount * speedfactor * (0.5 - avg_speedfactor))
	end

	return math.clamp(avg_speedfactor, .5, 1)
end

expres.handle = safecall(mUXsKykTgB('cvncpgkclryj_pcqmjtcp.fylbjc', 24), true, function()
	if not (Vars.Misc.experimental_resolver and Vars.Misc.experimental_resolver:get()) then 
		return
	end

	local current_threat = client.current_threat()

    if current_threat == nil or not entity.is_alive(current_threat) or entity.is_dormant(current_threat) then 
		return 
	end

    if expres.body_yaw[current_threat] == nil then 
		expres.body_yaw[current_threat], expres.eye_angles[current_threat] = {}, {}
	end

    local simtime = toticks(entity.get_prop(current_threat, mUXsKykTgB('f_yeLbfnetmbhgMbfx', 19)))
	local prev_simtime = toticks(expres.get_prev_simtime(current_threat))
    expres.body_yaw[current_threat][simtime] = entity.get_prop(current_threat, mUXsKykTgB('y_rxBaeqBmdmyqfqd', 12), 11) * 120 - 60
    expres.eye_angles[current_threat][simtime] = select(2, entity.get_prop(current_threat, mUXsKykTgB("f_tgzXrxTgzexl", 19)))

    if expres.body_yaw[current_threat][prev_simtime] ~= nil then
		local ent = c_entity.new(current_threat)
		local animstate = ent:get_anim_state()
		local max_desync = expres.get_max_desync(animstate)

        local should_correct = (simtime - prev_simtime >= 1) and math.abs(max_desync) < 45 and expres.body_yaw[current_threat][prev_simtime] ~= 0

		if should_correct then
			 local side = math.clamp(math.normalize_yaw(animstate.goal_feet_yaw - expres.eye_angles[current_threat][simtime]), -1, 1)
			 local value = expres.body_yaw[current_threat][prev_simtime] * side * max_desync
			local value = math.random(0, expres.body_yaw[current_threat][prev_simtime] * math.random(-1, 1)) * .25

			plist.set(current_threat, mUXsKykTgB('Vehsu reto oqm lqbku', 16), value) 
		end
		plist.set(current_threat, mUXsKykTgB('Hqteg dqfa acy', 2), should_correct)     
    end

    plist.set(current_threat, mUXsKykTgB('Bnqqdbshnm zbshud', 25), true)
end)

model_breaker.handle = safecall(mUXsKykTgB('prgho_euhdnhu.kdqgoh', 3), true, function()
    local player = entity.get_local_player()

    if player == nil then
        return
    end

	local self_index = c_entity.new(player)
    local self_anim_state = self_index:get_anim_state()

    if not self_anim_state then
        return
    end

	if not Vars.Misc.anim_breakers:get() then
		return
	end

	if Vars.Misc.anim_breakers_settings.pitch:get() and (not (model_breaker.in_air) and self_anim_state.hit_in_ground_animation) then
		entity.set_prop(player, mUXsKykTgB('a_tzDcgsDofoashsf', 14), 0.5, 12)
	end

	if Vars.Misc.anim_breakers_settings.air:get() == mUXsKykTgB('Lmtmbv', 19) then
		entity.set_prop(player, mUXsKykTgB('n_gmQptfQbsbnfufs', 1), 1, 6)
	end	

	if Vars.Misc.anim_breakers_settings.air:get() == mUXsKykTgB('Ybuupun', 7) then
		local self_anim_overlay = self_index:get_anim_overlay(6)

		local x_velocity = entity.get_prop(player, mUXsKykTgB('u_dmkDmtwkqbg[8]', 8))
        if math.abs(x_velocity) >= 3 then
            self_anim_overlay.weight = 1
        end
	end	

	if Vars.Misc.anim_breakers_settings.breaked_legs:get() == mUXsKykTgB('Deletn', 11) then
		gamesense_refs.override(mUXsKykTgB('unp_vxenvnwc', 9), mUXsKykTgB('Kvgkic cvsno', 10))
		entity.set_prop(player, mUXsKykTgB('f_yeIhlxItktfxmxk', 19), 1, 0)
	elseif Vars.Misc.anim_breakers_settings.breaked_legs:get() == mUXsKykTgB('Svoojoh', 1) then
		gamesense_refs.override(mUXsKykTgB('unp_vxenvnwc', 9), mUXsKykTgB('Qhyhu volgh', 3))
		entity.set_prop(player, mUXsKykTgB('r_kqUtxjUfwfrjyjw', 5), 0, 7)
	elseif Vars.Misc.anim_breakers_settings.breaked_legs:get() == mUXsKykTgB('Cmfoe', 1) then
		entity.set_prop(player, mUXsKykTgB('a_tzDcgsDofoashsf', 14), 0, 8)
		entity.set_prop(player, mUXsKykTgB('c_vbFeiuFqhqcujuh', 16), 0, 9)
	elseif Vars.Misc.anim_breakers_settings.breaked_legs:get() == mUXsKykTgB('Tsddob', 10) then
		entity.set_prop(player, mUXsKykTgB('s_lrVuykVgxgskzkx', 6), 1, globals.tickcount() % 4 > 1 and 0.5 or 1)
	end
end)

model_breaker.handle_jitter = safecall(mUXsKykTgB('npefm_csfblfs.iboemf_kjuufs', 1), true, function(cmd)
    local player = entity.get_local_player()

    if player == nil then
        return
    end

	if not Vars.Misc.anim_breakers:get() then
		return
	end

	if Vars.Misc.anim_breakers_settings.breaked_legs:get() == mUXsKykTgB('Cbmmxk', 19) then
		gamesense_refs.override(mUXsKykTgB('atv_bdktbtci', 15), cmd.command_number % 3 == 0 and mUXsKykTgB('Evv', 16) or mUXsKykTgB('Qbmqoi ibytu', 16))
	end	

end)

shot_logger.add = function(...)
    args = { ... }
    len = #args
    for i = 1, len do
        arg = args[i]
        r, g, b = unpack(arg)

        msg = {}

        if #arg == 3 then
            table.insert(msg, mUXsKykTgB(" ", 1))
        else
            for i = 4, #arg do
                table.insert(msg, arg[i])
            end
        end
        msg = table.concat(msg)

        if len > i then
            msg = msg .. mUXsKykTgB("\0", 2)
        end

        client.color_log(r, g, b, msg)
    end
end

shot_logger.bullet_impacts = {}
shot_logger.bullet_impact = function(e)
	local tick = globals.tickcount()
	local me = entity.get_local_player()
	local user = client.userid_to_entindex(e.userid)
	
	if user ~= me then
		return
	end

	if #shot_logger.bullet_impacts > 150 then
		shot_logger.bullet_impacts = { }
	end

	shot_logger.bullet_impacts[#shot_logger.bullet_impacts+1] = {
		tick = tick,
		eye = vector(client.eye_position()),
		shot = vector(e.x, e.y, e.z)
	}
end

shot_logger.get_inaccuracy_tick = function(pre_data, tick)
	local spread_angle = -1
	for k, impact in pairs(shot_logger.bullet_impacts) do
		if impact.tick == tick then
			local aim, shot = 
				(pre_data.eye-pre_data.shot_pos):angles(),
				(pre_data.eye-impact.shot):angles()

				spread_angle = vector(aim-shot):length2d()
			break
		end
	end

	return spread_angle
end

shot_logger.get_safety = function(aim_data, target)
	local has_been_boosted = aim_data.boosted
	local plist_safety = plist.get(target, mUXsKykTgB('Szivvmhi weji tsmrx', 4))
	local ui_safety = { ui.get(gamesense_refs.prefer_safe_point), ui.get(gamesense_refs.force_safe_point) or plist_safety == mUXsKykTgB('Dc', 15) }

	if not has_been_boosted then
		return -1
	end

	if plist_safety == mUXsKykTgB('Nee', 25) or not (ui_safety[1] or ui_safety[2]) then
		return 0
	end

	return ui_safety[2] and 2 or (ui_safety[1] and 1 or 0)
end

shot_logger.generate_flags = function(pre_data)
	return {
		pre_data.self_choke > 1 and 1 or 0,
		pre_data.velocity_modifier < 1.00 and 1 or 0,
		pre_data.flags.boosted and 1 or 0
	}
end

shot_logger.hitboxes = {mUXsKykTgB("rpypctn", 11), mUXsKykTgB("byux", 20), mUXsKykTgB("kpmab", 8), mUXsKykTgB("ghcaoqv", 14), mUXsKykTgB("wpqe lcx", 11), mUXsKykTgB("arpqc jav", 9), mUXsKykTgB("unoc unp", 9), mUXsKykTgB("uljkw ohj", 3), mUXsKykTgB("fwuc", 18), mUXsKykTgB("?", 6), mUXsKykTgB("pnja", 9)}
shot_logger.on_aim_fire = function(e)
	local p_ent = e.target
	local me = entity.get_local_player()

	shot_logger[e.id] = {
		original = e,
		dropped_packets = { },

		handle_time = globals.realtime(),
		self_choke = globals.chokedcommands(),

		flags = {
			boosted = e.boosted
		},

		feet_yaw = entity.get_prop(p_ent, mUXsKykTgB('h_agKjnzKvmvhzozm', 21), 11)*120-60,
		correction = plist.get(p_ent, mUXsKykTgB('Vhkkxvmbhg tvmbox', 19)),

		safety = shot_logger.get_safety(e, p_ent),
		shot_pos = vector(e.x, e.y, e.z),
		eye = vector(client.eye_position()),
		view = vector(client.camera_angles()),

		velocity_modifier = entity.get_prop(me, mUXsKykTgB('y_rxHqxaoufkYapuruqd', 12)),
		total_hits = entity.get_prop(me, mUXsKykTgB('g_ninufBcnmIhMylpyl', 20)),

		history = globals.tickcount() - e.tick
	}
end
shot_logger.on_aim_hit = function(e)
	if not (Vars.Misc.hitlog_console and Vars.Misc.hitlog_console:get()) then
		return
	end

	if shot_logger[e.id] == nil then
		return 
	end

	local info = 
	{
		type = math.max(0, entity.get_prop(e.target, mUXsKykTgB('j_fEbxiqe', 23))) > 0,
		prefix = { Vars.Misc.hitlogger_settings.prefix_color.color:get() },
		hit = { Vars.Misc.hitlogger_settings.hit_color.color:get() },
		name = entity.get_player_name(e.target),
		hitgroup = shot_logger.hitboxes[e.hitgroup + 1] or mUXsKykTgB('?', 4),
		flags = string.format(mUXsKykTgB('%u', 2), table.concat(shot_logger.generate_flags(shot_logger[e.id]))),
		aimed_hitgroup = shot_logger.hitboxes[shot_logger[e.id].original.hitgroup + 1] or mUXsKykTgB('?', 2),
		aimed_hitchance = string.format(mUXsKykTgB('%n%%', 10), math.floor(shot_logger[e.id].original.hit_chance + 0.5)),
		hp = math.max(0, entity.get_prop(e.target, mUXsKykTgB('w_sRokvdr', 10))),
		spread_angle = string.format(mUXsKykTgB('%.7e°', 25), shot_logger.get_inaccuracy_tick(shot_logger[e.id], globals.tickcount())),
		correction = string.format(mUXsKykTgB('%u:%u°', 17), shot_logger[e.id].correction and 1 or 0, (shot_logger[e.id].feet_yaw < 10 and shot_logger[e.id].feet_yaw > -10) and 0 or shot_logger[e.id].feet_yaw)
	}

	shot_logger.add({ info.prefix[1], info.prefix[2], info.prefix[3], mUXsKykTgB('[vmzctcls]', 17)}, 
					{ 134, 134, 134, mUXsKykTgB(' » ', 9) }, 
					{ 200, 200, 200, info.type and mUXsKykTgB('Wtftzxw ', 19) or mUXsKykTgB('Truunm ', 9) }, 
					{ info.hit[1], info.hit[2], info.hit[3],  info.name }, 
					{ 200, 200, 200, mUXsKykTgB(' jo uif ', 1) }, 
					{ info.hit[1], info.hit[2], info.hit[3], info.hitgroup }, 
					{ 200, 200, 200, info.type and info.hitgroup ~= info.aimed_hitgroup and mUXsKykTgB(' (', 8) or mUXsKykTgB('', 8)},
					{ info.hit[1], info.hit[2], info.hit[3], info.type and (info.hitgroup ~= info.aimed_hitgroup and info.aimed_hitgroup) or mUXsKykTgB('', 4) },
					{ 200, 200, 200, info.type and info.hitgroup ~= info.aimed_hitgroup and mUXsKykTgB(')', 6) or mUXsKykTgB('', 1)},
					{ 200, 200, 200, info.type and mUXsKykTgB(' tcf ', 14) or mUXsKykTgB('', 7) },
					{ info.hit[1], info.hit[2], info.hit[3], info.type and e.damage or mUXsKykTgB('', 7) },
					{ 200, 200, 200, info.type and e.damage ~= shot_logger[e.id].original.damage and mUXsKykTgB(' (', 4) or mUXsKykTgB('', 2)},
					{ info.hit[1], info.hit[2], info.hit[3], info.type and (e.damage ~= shot_logger[e.id].original.damage and shot_logger[e.id].original.damage) or mUXsKykTgB('', 2) },
					{ 200, 200, 200, info.type and e.damage ~= shot_logger[e.id].original.damage and mUXsKykTgB(')', 5) or mUXsKykTgB('', 4)},
					{ 200, 200, 200, info.type and mUXsKykTgB(' yvhvbz', 21) or mUXsKykTgB('', 9) },
					{ 200, 200, 200, info.type and mUXsKykTgB(' (', 4) or mUXsKykTgB('', 2) }, { info.hit[1], info.hit[2], info.hit[3], info.type and info.hp or mUXsKykTgB('', 9) }, { 200, 200, 200, info.type and mUXsKykTgB(' bj lyguhcha)', 20) or mUXsKykTgB('', 5) },
					{ 200, 200, 200, mUXsKykTgB(' [', 4)}, { info.hit[1], info.hit[2], info.hit[3], info.spread_angle }, { 200, 200, 200, mUXsKykTgB(' | ', 8) }, { info.hit[1], info.hit[2], info.hit[3], info.correction}, { 200, 200, 200, mUXsKykTgB(']', 2) },
					{ 200, 200, 200, mUXsKykTgB(' (to: ', 12) }, { info.hit[1], info.hit[2], info.hit[3], info.aimed_hitchance }, { 200, 200, 200, mUXsKykTgB(' | vdihwb: ', 3) }, { info.hit[1], info.hit[2], info.hit[3], shot_logger[e.id].safety },
					{ 200, 200, 200, mUXsKykTgB(' | stdezcj(etnv): ', 11) }, { info.hit[1], info.hit[2], info.hit[3], shot_logger[e.id].history }, { 200, 200, 200, mUXsKykTgB(' | agvbn: ', 21) }, { info.hit[1], info.hit[2], info.hit[3], info.flags },
					{ 200, 200, 200, mUXsKykTgB(')', 1) })
end

shot_logger.on_aim_miss = function(e)
	if not (Vars.Misc.hitlog_console and Vars.Misc.hitlog_console:get()) then
		return
	end

	local me = entity.get_local_player()
	local info = 
	{
		prefix = {Vars.Misc.hitlogger_settings.prefix_color.color:get()},
		hit = {Vars.Misc.hitlogger_settings.miss_color.color:get()},
		name = entity.get_player_name(e.target),
		hitgroup = shot_logger.hitboxes[e.hitgroup + 1] or mUXsKykTgB('?', 9),
		flags = string.format(mUXsKykTgB('%h', 15), table.concat(shot_logger.generate_flags(shot_logger[e.id]))),
		aimed_hitgroup = shot_logger.hitboxes[shot_logger[e.id].original.hitgroup + 1] or mUXsKykTgB('?', 7),
		aimed_hitchance = string.format(mUXsKykTgB('%c%%', 25), math.floor(shot_logger[e.id].original.hit_chance + 0.5)),
		hp = math.max(0, entity.get_prop(e.target, mUXsKykTgB('a_wVsozhv', 14))),
		reason = e.reason,
		spread_angle = string.format(mUXsKykTgB('%.1o°', 9), shot_logger.get_inaccuracy_tick(shot_logger[e.id], globals.tickcount())),
		correction = string.format(mUXsKykTgB('%l:%l°', 8), shot_logger[e.id].correction and 1 or 0, (shot_logger[e.id].feet_yaw < 10 and shot_logger[e.id].feet_yaw > -10) and 0 or shot_logger[e.id].feet_yaw)
	}

    if info.reason == mUXsKykTgB('?', 3) then
        info.reason = mUXsKykTgB('oaddqofuaz', 12);

        if shot_logger[e.id].total_hits ~= entity.get_prop(me, mUXsKykTgB('c_jejqbXyjiEdIuhluh', 16)) then
            info.reason = mUXsKykTgB('xuguay lydywncih', 20);
        end
    end

	shot_logger.add({ info.prefix[1], info.prefix[2], info.prefix[3], mUXsKykTgB('[fwjmdmvc]', 1)}, 
					{ 134, 134, 134, mUXsKykTgB(' » ', 1) }, 
					{ 200, 200, 200, mUXsKykTgB('Zvffrq fubg ng ', 13) }, 
					{ info.hit[1], info.hit[2], info.hit[3],  info.name }, 
					{ 200, 200, 200, mUXsKykTgB(' qv bpm ', 8) }, 
					{ info.hit[1], info.hit[2], info.hit[3], info.hitgroup }, 
					{ 200, 200, 200, mUXsKykTgB(' ctd sn ', 25)},
					{ info.hit[1], info.hit[2], info.hit[3], info.reason },
					{ 200, 200, 200, mUXsKykTgB(' [', 2)}, { info.hit[1], info.hit[2], info.hit[3], info.spread_angle }, { 200, 200, 200, mUXsKykTgB(' | ', 8) }, { info.hit[1], info.hit[2], info.hit[3], info.correction}, { 200, 200, 200, mUXsKykTgB(']', 1) },
					{ 200, 200, 200, mUXsKykTgB(' (to: ', 12) }, { info.hit[1], info.hit[2], info.hit[3], info.aimed_hitchance }, { 200, 200, 200, mUXsKykTgB(' | owbapu: ', 22) }, { info.hit[1], info.hit[2], info.hit[3], shot_logger[e.id].safety },
					{ 200, 200, 200, mUXsKykTgB(' | wxhidgn(Δ): ', 15) }, { info.hit[1], info.hit[2], info.hit[3], shot_logger[e.id].history }, { 200, 200, 200, mUXsKykTgB(' | pvkqc: ', 10) }, { info.hit[1], info.hit[2], info.hit[3], info.flags },
					{ 200, 200, 200, mUXsKykTgB(')', 1) })
end

client.set_event_callback(mUXsKykTgB('xfj_cfob', 23), shot_logger.on_aim_fire)
client.set_event_callback(mUXsKykTgB('ltx_xtdd', 11), shot_logger.on_aim_miss)
client.set_event_callback(mUXsKykTgB('ucg_bcn', 20), shot_logger.on_aim_hit)
client.set_event_callback(mUXsKykTgB('ngxxqf_uybmof', 12), shot_logger.bullet_impact)

manual_indication.handle = function()
	local player = entity.get_local_player()

    if player == nil or not entity.is_alive(player) then
        return
    end
	
    local m_bIsScoped = entity.get_prop(player, mUXsKykTgB('y_nUeEoabqp', 12)) ~= 0
    local antiaim_manuals = conditional_antiaims.manual_dir
    local manual_indication_enable = Vars.Misc.manual_arrows:get()
    local manual_indication_type = Vars.Misc.manual_arrows_settings.settings:get()
    local manual_indication_anim = animations.new(mUXsKykTgB('ocpwcn_kpfkecvkqp_cpko', 2), manual_indication_enable and 1 or 0)
	local x, y = client.screen_size()
    local pos = { x = x*0.5, y = y*0.5 }

    local anim = {}

    if manual_indication_type == mUXsKykTgB('Tuvqkbj', 16) then

        local manual_indication_adding = Vars.Misc.manual_arrows_settings.adding:get()
        local manual_indication_accent_color = { Vars.Misc.manual_arrows_settings.accent_color.color:get() }

        anim.left = animations.new(mUXsKykTgB('ocpwcn_kpfkecvkqp_nghv', 2), manual_indication_enable and not m_bIsScoped and (antiaim_manuals == -90 and 255 or 100) or 0)
        anim.right = animations.new(mUXsKykTgB('aobioz_wbrwqohwcb_fwuvh', 14), manual_indication_enable and not m_bIsScoped and (antiaim_manuals == 90 and 255 or 100) or 0)

        if anim.left < 1 or anim.right < 1 then
            return
        end

		render.triangle(pos.x - (manual_indication_adding+9), 
			pos.y, 
			pos.x - manual_indication_adding, 
			pos.y - 5, pos.x - manual_indication_adding, 
			pos.y + 5, 
			antiaim_manuals == -90 and manual_indication_accent_color[1] or 0, 
			antiaim_manuals == -90 and manual_indication_accent_color[2] or 0, 
			antiaim_manuals == -90 and manual_indication_accent_color[3] or 0, 
			anim.left)

		render.triangle(pos.x + (manual_indication_adding+9), 
			pos.y, 
			pos.x + manual_indication_adding, 
			pos.y - 5, 
			pos.x + manual_indication_adding, 
			pos.y + 5, 
			antiaim_manuals == 90 and manual_indication_accent_color[1] or 0, 
			antiaim_manuals == 90 and manual_indication_accent_color[2] or 0, 
			antiaim_manuals == 90 and manual_indication_accent_color[3] or 0, 
			anim.right)
    end

    if manual_indication_type == mUXsKykTgB('Xieqwoiix', 4) then

        local default = { Vars.Misc.manual_arrows_settings.teamskeet_accent_color.color:get() }
        local default2 = { Vars.Misc.manual_arrows_settings.teamskeet_desync_accent_color.color:get() }

        local ts_arrowsanim = Vars.Misc.manual_arrows_settings.teamskeet_adding:get()
        local ts_colleft = (antiaim_manuals == -90) and { default[1], default[2], default[3], 255*manual_indication_anim } or { 35, 35, 35, 150*manual_indication_anim }
        local ts_colright = (antiaim_manuals == 90) and { default[1], default[2], default[3], 255*manual_indication_anim } or { 35, 35, 35, 150*manual_indication_anim }

		render.triangle(pos.x + (ts_arrowsanim+13), pos.y, pos.x + ts_arrowsanim, pos.y - 9, pos.x + ts_arrowsanim, pos.y + 9, ts_colright[1], ts_colright[2], ts_colright[3], ts_colright[4])
		render.triangle(pos.x - (ts_arrowsanim+13), pos.y, pos.x - ts_arrowsanim, pos.y - 9, pos.x - ts_arrowsanim, pos.y + 9, ts_colleft[1], ts_colleft[2], ts_colleft[3], ts_colleft[4])
        
		local ts_lineanim = ts_arrowsanim
        local ts_sideleft = (not conditional_antiaims.current_side) and { default2[1], default2[2], default2[3], default2[4]*manual_indication_anim } or { 35, 35, 35, 150*manual_indication_anim }
        local ts_sideright = (conditional_antiaims.current_side) and { default2[1], default2[2], default2[3], default2[4]*manual_indication_anim } or { 35, 35, 35, 150*manual_indication_anim }
		
		render.rectangle(pos.x + (ts_lineanim-4), pos.y - 9, 2, 18, ts_sideleft[1], ts_sideleft[2], ts_sideleft[3], ts_sideleft[4])
		render.rectangle(pos.x - (ts_lineanim-2), pos.y - 9, 2, 18,	ts_sideright[1], ts_sideright[2], ts_sideright[3], ts_sideright[4])

    end

    if manual_indication_type == mUXsKykTgB('Kpxkevwu', 2) then
        local selected = { Vars.Misc.manual_arrows_settings.getzeus_accent_color.color:get() }
        local unselected = { Vars.Misc.manual_arrows_settings.getzeus_second_accent_color.color:get() }
		local by = manual_indication.peeking_side
		local gt_arrowsanim = Vars.Misc.manual_arrows_settings.getzeus_adding:get()

		local act = { selected[1], selected[2], selected[3], selected[4]*manual_indication_anim }
		local off = { unselected[1], unselected[2], unselected[3], unselected[4]*manual_indication_anim }

		local gt_colright = antiaim_manuals == 90 and act or off
		local gt_colleft = antiaim_manuals == -90 and act or off

        if antiaim_manuals == 90 or antiaim_manuals == -90 then
			render.text(pos.x + gt_arrowsanim, pos.y - 16, gt_colright[1], gt_colright[2], gt_colright[3], gt_colright[4], mUXsKykTgB('+', 3), nil, mUXsKykTgB('>', 8))
			render.text(pos.x - (gt_arrowsanim+13), pos.y - 16, gt_colleft[1], gt_colleft[2], gt_colleft[3], gt_colleft[4], mUXsKykTgB('+', 7), nil, mUXsKykTgB('<', 2))
		elseif by ~= 0 and Vars.Misc.manual_arrows_settings.invictus_dynamic:get() then
			local r = by > 0 and act or off
			local l = by < 0 and act or off
			render.text(pos.x + gt_arrowsanim + 6, pos.y - 2, r[1], r[2], r[3], r[4], mUXsKykTgB('+j', 7), nil, by == 2 and mUXsKykTgB('   >>', 9) or mUXsKykTgB('>', 1))
			render.text(pos.x - (gt_arrowsanim+13) + 6, pos.y - 2, l[1], l[2], l[3], l[4], mUXsKykTgB('+b', 25), nil, by == -2 and mUXsKykTgB('<<   ', 9) or mUXsKykTgB('<', 6))
		end
    end
end

manual_indication.extend_vector = function(pos,length,angle)
    local rad = angle * math.pi / 180
    if rad == nil then return end
    if angle == nil or pos == nil or length == nil then return end
    return {pos[1] + (math.cos(rad) * length),pos[2] + (math.sin(rad) * length), pos[3]};
end

manual_indication.peeking_side = 0
manual_indication.peeking_whom = function (cmd)
	local manual_indication_enable = Vars.Misc.manual_arrows:get()
    local manual_indication_type = Vars.Misc.manual_arrows_settings.settings:get()

    manual_indication.peeking_side = 0
	if not manual_indication_enable or manual_indication_type ~= mUXsKykTgB("Bgobvmnl", 19) or not Vars.Misc.manual_arrows_settings.invictus_dynamic:get() then return end

    local me = entity.get_local_player()
	local enemy = client.current_threat()
	if not me or entity.is_dormant(enemy) then return end

    local pitch, yaw = client.camera_angles(me)
    local left1 = manual_indication.extend_vector({entity.get_origin(me)},50,yaw + 110)
    local left2 = manual_indication.extend_vector({entity.get_origin(me)},30,yaw + 60)
    local right1 = manual_indication.extend_vector({entity.get_origin(me)},50,yaw - 110)
    local right2 = manual_indication.extend_vector({entity.get_origin(me)},30,yaw - 60)

    local pitch, yaw_e = entity.get_prop(enemy, mUXsKykTgB("o_cpiGagCpingu", 2))
    local enemy_right1 = manual_indication.extend_vector({entity.get_origin(enemy)},40,yaw_e - 115)
    local enemy_right2 = manual_indication.extend_vector({entity.get_origin(enemy)},20,yaw_e - 35)
    local enemy_left1 = manual_indication.extend_vector({entity.get_origin(enemy)},40,yaw_e + 115)
    local enemy_left2 = manual_indication.extend_vector({entity.get_origin(enemy)},20,yaw_e + 35)

    local _, dmg_left1 =  client.trace_bullet(enemy, enemy_left1[1], enemy_left1[2], enemy_left1[3] + 70, left1[1], left1[2], left1[3] , true)
    local _, dmg_right1 = client.trace_bullet(enemy, enemy_right1[1], enemy_right1[2], enemy_right1[3] + 70, right1[1], right1[2], right1[3], true)
    local _, dmg_left2 =  client.trace_bullet(enemy, enemy_left2[1], enemy_left2[2], enemy_left2[3] + 30, left2[1], left2[2], left2[3], true)
    local _, dmg_right2 = client.trace_bullet(enemy, enemy_right2[1], enemy_right2[2], enemy_right2[3] + 30, right2[1], right2[2], right2[3], true)

    local by = nil
    if dmg_right2 > 0 then
        by = 2
    elseif dmg_left2 > 0 then
        by = -2
    elseif dmg_left1 > 0 then
        by = -1
    elseif dmg_right1 > 0 then
        by = 1
    elseif dmg_right1 > 0 and dmg_left1 > 0 then
        by = 0
    elseif dmg_right2 > 0 and dmg_left2 > 0 then
        by = 0
    else
        by = 0
    end
    manual_indication.peeking_side = by
end



crosshair_logger.indicators_height = 0

screen_indication.luasense = function (anim)
	local width, height = client.screen_size()
	local r2, g2, b2, a2 = 55,55,55,anim
	local highlight_fraction =  (globals.realtime() / 2 % 1.2 * 2) - 1.2
	local output = mUXsKykTgB("", 2)
	local text_to_draw = mUXsKykTgB(" Q Z I P", 14)
	for idx = 1, #text_to_draw do
		local character = text_to_draw:sub(idx, idx)
		local character_fraction = idx / #text_to_draw
		local r1, g1, b1, a1 = 255, 255, 255, 255
		local delta = (character_fraction - highlight_fraction)
		if delta >= 0 and delta <= 1.4 then
			if delta > 0.7 then delta = 1.4 - delta end
			local rf, gf, bf = r2 - r1, g2 - g1, b2 - b1
			r1 = r1 + rf * delta / 0.8
			g1 = g1 + gf * delta / 0.8
			b1 = b1 + bf * delta / 0.8
		end
		output = output .. (mUXsKykTgB('\a%35k%35k%35k%35k%f', 13)):format(r1, g1, b1, a2, text_to_draw:sub(idx, idx))
	end

	output = mUXsKykTgB("M D Q T", 8) .. output
	if Vars.Misc.alt_watermark_settings.nosp.value then
		output = string.gsub(output, mUXsKykTgB(" ", 1), mUXsKykTgB("", 1))
	end
	if _DEBUG and Vars.Misc.alt_watermark_settings.pos.value ~= mUXsKykTgB("Rejjec", 16) then
		output = output .. (mUXsKykTgB("\a%g%g%g%g", 9)):format(200, 69, 69, a2) .. mUXsKykTgB(" [JKHAM] ", 6)
	end

	local r,g,b = Vars.Misc.alt_watermark_settings.color:get()
	if Vars.Misc.alt_watermark_settings.pos.value == mUXsKykTgB("Uhmmhf", 19) then
		if _DEBUG then
			renderer.text(width/2, height - 36, 200, 69, 69, a2, mUXsKykTgB("v", 19), 0, mUXsKykTgB("[HIFYK]", 4))
		end
		renderer.text(width/2, height - 20, r,g,b, a2, mUXsKykTgB("m", 10), 0, output)
	elseif Vars.Misc.alt_watermark_settings.pos.value == mUXsKykTgB("Hywxj", 16) then
		renderer.text(width - 20, height/2, r,g,b, a2, mUXsKykTgB("u", 3), 0, output)
	elseif Vars.Misc.alt_watermark_settings.pos.value == mUXsKykTgB("Mfgu", 1) then
		renderer.text(20, height/2, r,g,b, a2, mUXsKykTgB("", 5), 0, output)
	end
end

screen_indication.handle = function()
	
	local anim = {}
	local indication_enable     =    Vars.Misc.screen_indicators:get()
	local accent_color          =  { Vars.Misc.screen_indicators.color:get() }		

	anim.main = animations.new(mUXsKykTgB('yixkkt_otjoigzout_sgot', 6), indication_enable and 255 or 0)

	local x, y = client.screen_size()
	local center = { x*0.5, y*0.5+25 }

	if not (indication_enable or Vars.Misc.branded_watermark:get()) then
		if Vars.Misc.alt_watermark.value == mUXsKykTgB("Dfuvie", 17) then
			screen_indication.luasense(255)
		else
			render.text(center[1], y - 15, 255, 255, 255, 200, mUXsKykTgB('po', 13), 0, mUXsKykTgB('✞janqhqzg', 5))
		end
	end

	local plocal = entity.get_local_player()
    if plocal == nil or not entity.is_alive(plocal) then
        return
    end

	if (Vars.Misc.screen_indicators_settings_dmg and Vars.Misc.screen_indicators_settings_dmg:get()) then
		if ui.get(gamesense_refs.dmgOverride[1]) and ui.get(gamesense_refs.dmgOverride[2]) and ui.get(gamesense_refs.dmgOverride[3]) then
			render.text(x*0.5 + 2, y*0.5 - 14, 255, 255, 255, 255, nil, 0, tostring(ui.get(gamesense_refs.dmgOverride[3])))
		end
	end

	if anim.main < 0.1 then
		return
	end

	local binds = {
        {mUXsKykTgB('xn', 20), ui.get(gamesense_refs.dt[1]) and ui.get(gamesense_refs.dt[2])},
        {mUXsKykTgB('nojk', 6), ui.get(gamesense_refs.os[1]) and ui.get(gamesense_refs.os[2])},
        {mUXsKykTgB('ltyx', 19), ui.get(gamesense_refs.safePoint)},
        {mUXsKykTgB('ergb', 3), ui.get(gamesense_refs.forceBaim)},
        {mUXsKykTgB('ajd', 23), ui.get(gamesense_refs.dmgOverride[1]) and ui.get(gamesense_refs.dmgOverride[2]) and ui.get(gamesense_refs.dmgOverride[3])},
        {mUXsKykTgB('iv', 3), ui.get(gamesense_aa.freestanding[1]) and ui.get(gamesense_aa.freestanding[2])}
    }
	
	local conds = {
        mUXsKykTgB('wlevih', 4),
        mUXsKykTgB('jkreuzex', 17),
        mUXsKykTgB('adwwrwp', 9),
        mUXsKykTgB('kdgoosdcafy', 18),
        mUXsKykTgB('ctbjhmf', 25),
        mUXsKykTgB('yahuzs+pgow', 12),
        mUXsKykTgB('gox', 6),
        mUXsKykTgB('ckt+fwem', 2),
        mUXsKykTgB('zlyymnuhx', 20),
        mUXsKykTgB('ojtnujp', 9),
        mUXsKykTgB('ed-kiu', 16)
    }

	local scope_based = entity.get_prop(plocal, mUXsKykTgB('y_nUeEoabqp', 12)) ~= 0
	local add_y = 0

	anim.name = {}
	anim.name.alpha = animations.new(mUXsKykTgB('dms_fsew_sdhzs', 18), indication_enable and 255 or 0)
	anim.name.move = animations.new(mUXsKykTgB('tafvk_egnw_fsew', 18), indication_enable and not scope_based and -render.measure_text(nil, mUXsKykTgB('sjwzqzip', 14))*0.5 or 15)
    anim.name.glow = animations.new(mUXsKykTgB('dilt_kxjb_ximex', 23), (indication_enable and Vars.Misc.screen_indicators_settings.glow:get()) and 50 or 0)
	if anim.name.alpha > 1 then
		if anim.name.glow > 1 then
			render.shadow(center[1]+1 + anim.name.move, center[2]+7, render.measure_text(mUXsKykTgB('o', 13), mUXsKykTgB('lcpsjsbi', 7))-1, 0, 10, 0, {accent_color[1], accent_color[2], accent_color[3], anim.name.glow}, {accent_color[1], accent_color[2], accent_color[3], anim.name.glow})
		end
		render.text(center[1] + string.format(mUXsKykTgB('%.3s', 13), anim.name.move), center[2], 255, 255, 255, anim.main, mUXsKykTgB('g', 5), 0, utils.animate_text(globals.curtime()*2, mUXsKykTgB('janqhqzg', 5), accent_color[1], accent_color[2], accent_color[3], anim.main, accent_color[1], accent_color[2], accent_color[3], 150*(anim.main/255)))
		add_y = add_y + string.format(mUXsKykTgB('%.7w', 17), anim.name.alpha / 255 * 12)
	end

    anim.state = {}
	anim.state.text = conds[conditional_antiaims.get_active_idx(conditional_antiaims.player_state)]
    anim.state.alpha = animations.new(mUXsKykTgB('lmtmx_teiat', 19), indication_enable and 200 or 0)
	anim.state.scoped_check = animations.new(mUXsKykTgB('xhtuji_hmjhp', 5), indication_enable and not scope_based and 1 or 0) ~= 1
	anim.state.move = anim.state.scoped_check and string.format(mUXsKykTgB('%.5e', 25),animations.new(mUXsKykTgB('dkpfu_oqxg_uvcvg', 2), indication_enable and not scope_based and -render.measure_text(nil, anim.state.text)*0.5 or 15)) or -render.measure_text(nil, anim.state.text)*0.5
	if anim.state.alpha > 1 then
		render.text(center[1] + anim.state.move, center[2] + add_y, 255, 255, 255, anim.state.alpha, nil, 0, anim.state.text)
        add_y = add_y + string.format(mUXsKykTgB('%.2h', 2), anim.state.alpha / 255 * 15)
    end

    anim.binds = {}
    for k, v in pairs(binds) do

        anim.binds[v[1]] = {}
        anim.binds[v[1]].alpha = animations.new(mUXsKykTgB('ahmcr_zkogz_', 25)..v[1], indication_enable and v[2] and 255 or 0)
        anim.binds[v[1]].move = animations.new(mUXsKykTgB('dkpfu_oqxg_', 2)..v[1], indication_enable and not scope_based and -render.measure_text(nil, v[1])*0.5 or 15)

        if anim.binds[v[1]].alpha > 1 then 
            render.text(center[1] + string.format(mUXsKykTgB('%.0p', 10), anim.binds[v[1]].move), center[2] + add_y, 255, 255, 255, anim.binds[v[1]].alpha, nil, 0, v[1])
			add_y = add_y + string.format(mUXsKykTgB('%.2r', 12), anim.binds[v[1]].alpha / 255 * 12)
        end
    end
	crosshair_logger.indicators_height = 10 + add_y
end


watermark.handle = function()
    local anim = animations.new(mUXsKykTgB('dhalythyr_hupt', 7), Vars.Misc.watermark:get() and 255 or 0)
    if anim < 1 then return end
    
    local width, height = client.screen_size()
    local accent_color = { Vars.Misc.watermark_settings.color:get() }
    local ping = math.floor(client.latency() * 1000)
    
    local text = string.format(mUXsKykTgB('%o | arehyhqx%o | %z io', 22),
        string.lower(information.user),
        _DEBUG and mUXsKykTgB(' [yzwpb]', 21) or mUXsKykTgB('', 4),
        ping
    )
    
    local text_width = render.measure_text(nil, text)
    local x_pos = width - text_width - 20
    local y_pos = 10

	render.rectangle(x_pos - 5, y_pos - 4, text_width + 10, 20, 0, 0, 0, 100)
    render.rectangle(x_pos - 5, y_pos - 4, text_width + 10, 2, accent_color[1], accent_color[2], accent_color[3], anim)
    render.text(x_pos, y_pos, 255, 255, 255, anim, nil, 0, text)
end






local visual_effects = {}

visual_effects.draw_svaston = function(x, y, size)
    local frametime = globals.frametime()
    local a = size / 60
    local gamma = math.atan(a / a)
    visual_effects.rainbow = visual_effects.rainbow + (frametime * 0.5)
    if visual_effects.rainbow > 1.0 then visual_effects.rainbow = 0.0 end
    if visual_effects.rotationdegree > 89 then visual_effects.rotationdegree = 0 end

    for i = 0, 4 do  
        local p_0 = (a * math.sin(DEG2RAD(visual_effects.rotationdegree + (i * 90))))
        local p_1 = (a * math.cos(DEG2RAD(visual_effects.rotationdegree + (i * 90))))
        local p_2 = ((a / math.cos(gamma)) * math.sin(DEG2RAD(visual_effects.rotationdegree + (i * 90) + RAD2DEG(gamma))))
        local p_3 = ((a / math.cos(gamma)) * math.cos(DEG2RAD(visual_effects.rotationdegree + (i * 90) + RAD2DEG(gamma))))

        local a, r, g, b = hsv2rgb(visual_effects.rainbow, 1, 1, 1)
        renderer.line(x, y, x + p_0, y - p_1, a, r, g, b)
        renderer.line(x + p_0, y - p_1, x + p_2, y - p_3, a, r, g, b)
    end
    visual_effects.rotationdegree = visual_effects.rotationdegree + (frametime * 150)
end

visual_effects.handle = function()
    if not Vars.Misc.visual_effects.svastika_effect:get() then return end
    
    local screenW, screenH = client.screen_size()
    visual_effects.draw_svaston(screenW / 2, screenH / 2, screenH / 2) 
end

visual_effects.rainbow = 0.00
visual_effects.rotationdegree = 0.000









crosshair_logger.hitgroups = { [0] = mUXsKykTgB('Hfofsjd', 1), mUXsKykTgB('Xuqt', 16), mUXsKykTgB('Purfg', 13), mUXsKykTgB('Dezxlns', 11), mUXsKykTgB('Rklz gxs', 6), mUXsKykTgB('Xomnz gxs', 6), mUXsKykTgB('Mfgu mfh', 1), mUXsKykTgB('Mdbco gzb', 21), mUXsKykTgB('Izxf', 21), [10] = mUXsKykTgB('Vtpg', 15)}
crosshair_logger.hit_types = { knife = mUXsKykTgB(' Jmhedc', 25), inferno = mUXsKykTgB(' Zsplcb', 24), hegrenade = mUXsKykTgB(' Ylopo', 11) }
crosshair_logger.lerp = function(x, v, t)
    local delta = v - x;

    if math.abs(delta) < 0.005 then
        return v
    end

    return x + delta * t
end
crosshair_logger.handle = function()
	local screen = {client.screen_size()}
	local center = {screen[1] * 0.5, screen[2] * 0.5 + 31}

	local scope_based = entity.get_prop(entity.get_local_player(), mUXsKykTgB('p_eLvVfrshg', 3)) ~= 0
	crosshair_logger.move = animations.new(mUXsKykTgB('vkhllatbk_ehzzxk.fhox', 19), (Vars.Misc.crosshair_hitlog and Vars.Misc.crosshair_settings.move:get()) and scope_based and 1 or 0)
    local logs_size = #crosshair_logger
    for key = logs_size, 1, -1 do
        local value = crosshair_logger[key]

        if value.alpha == 1 then
            table.remove(crosshair_logger, key)
            goto skip
        end

        local fullstr = table.concat(value.args, mUXsKykTgB('', 5))
        local alpha = 1 - math.abs(value.alpha)
        local base_color = { value.color[1], value.color[2], value.color[3], value.color[4] * alpha }

        local base_hex = mUXsKykTgB('\a', 19) .. utils.rgb_to_hex(base_color)
        local alternative = mUXsKykTgB('\a', 25) .. utils.rgb_to_hex({255, 255, 255, 200 * alpha})
        local string = alternative

        for index, text in ipairs(value.args) do
            local temp = text
            if index % 2 == 0 then
                temp = base_hex .. temp .. alternative
            end
            string = string .. temp
        end

		local text_sz = { render.measure_text(mUXsKykTgB('wx', 20), fullstr) }
        local x, y = center[1] + ((text_sz[1]*0.5+12)*crosshair_logger.move), center[2] + crosshair_logger.indicators_height
		render.text(x, y, 255, 255, 255, 200 * alpha, mUXsKykTgB('xy', 21), 0, string)

        local height = text_sz[2] + 1
        height = height * alpha
        center[2] = center[2] + height
		
        if (globals.realtime() - value.time > 0) or (key > 4) then
            value.alpha = crosshair_logger.lerp(value.alpha, 1, globals.frametime() * 12)
        else
            value.alpha = crosshair_logger.lerp(value.alpha, 0, globals.frametime() * 12)
        end

        ::skip::
    end
end

crosshair_logger.player_hurt = function(e)
    if not (Vars.Misc.crosshair_hitlog and Vars.Misc.crosshair_hitlog:get()) then
        return
    end

    local localplayer = entity.get_local_player()
    if not localplayer then
        return
    end

    if e.health < 0 then
        return
    end

    local attacker = client.userid_to_entindex(e.attacker)
    local userid = client.userid_to_entindex(e.userid)

    if attacker ~= localplayer then
        return
    end

    if userid == localplayer then
        return
    end

    if e.weapon == mUXsKykTgB('tvsvavc', 7) then
        return
    end

    local args = {}
    local hit_type = crosshair_logger.hit_types[e.weapon] or mUXsKykTgB('', 7)
    table.insert(args, hit_type)
    table.insert(args, entity.get_player_name(userid))

    if e.hitgroup ~= 0 then
        local hitgroup = crosshair_logger.hitgroups[e.hitgroup] or mUXsKykTgB('?', 2)
        table.insert(args, hitgroup:lower())
    end

    table.insert(args, e.dmg_health)
    table.insert(args, mUXsKykTgB('(\\ghm', 19))
    table.insert(args, e.health .. mUXsKykTgB('\\hin', 20))
    table.insert(args, mUXsKykTgB(')', 3))

    local length = #args
    for key, value in ipairs(args) do
        if key == length then
            goto skip
        end

        if type(value) == mUXsKykTgB('bcarwp', 9) and value:find(mUXsKykTgB('\\wxc', 9)) then
            args[key] = value:gsub(mUXsKykTgB('\\opu', 1), mUXsKykTgB('', 3))
        else
            args[key] = value .. mUXsKykTgB('\x31', 11)
        end

        ::skip::
    end

	if Vars.Misc.crosshair_settings then
		crosshair_logger.add({Vars.Misc.crosshair_settings.hit_color.color:get()}, args)
	end
end

crosshair_logger.aimbot_data = {}
crosshair_logger.aim_fire = function(e)
    local localplayer = entity.get_local_player()
    if not localplayer then
        return
    end

    crosshair_logger.aimbot_data[e.id] = {
        original = e,
        total_hits = entity.get_prop(localplayer, mUXsKykTgB('d_kfkrcYzkjFeJvimvi', 17))
    }
end

crosshair_logger.aim_miss = function(e)
	if not (Vars.Misc.crosshair_hitlog and Vars.Misc.crosshair_hitlog:get()) then
        return
    end

    local localplayer = entity.get_local_player()
    if not localplayer then
        return
    end

    local args = {}
    local pre_data = crosshair_logger.aimbot_data[e.id]
    local reason = e.reason

    if reason == mUXsKykTgB('?', 3) then
        reason = mUXsKykTgB('gsvvigxmsr', 4)

        if pre_data.total_hits ~= entity.get_prop(localplayer, mUXsKykTgB('d_kfkrcYzkjFeJvimvi', 17)) then
            reason = mUXsKykTgB('tqcqwu huzusjyed', 16)
        end
    end

    table.insert(args, mUXsKykTgB(' Qmwwih mr', 4))
    local hitgroup = crosshair_logger.hitgroups[e.hitgroup] or mUXsKykTgB('?', 6)

    table.insert(args, hitgroup:lower())
    table.insert(args, mUXsKykTgB('hyi xs', 4))
    table.insert(args, reason)

    local length = #args;
    for key, value in ipairs(args) do
        if key == length then
            goto skip
        end

        if type(value) == mUXsKykTgB('ijhydw', 16) and value:find(mUXsKykTgB('\\jkp', 22)) then
            args[ key ] = value:gsub(mUXsKykTgB('\\xyd', 10), mUXsKykTgB('', 6))
        else
            args[ key ] = value .. mUXsKykTgB('\x86', 6)
        end

        ::skip::
    end

	if Vars.Misc.crosshair_settings then
		crosshair_logger.add({Vars.Misc.crosshair_settings.miss_color.color:get()}, args)
	end
end

crosshair_logger.add = function(color, args)
    local this = {
        color = color;
        args = args;
        alpha = -1;
        time = globals.realtime() + 5;
    };

    table.insert(crosshair_logger, 1, this);
    return this
end

death_spammer.phares = {
	mUXsKykTgB("пидорас ебаный", 1), mUXsKykTgB("мать твою ебал чмо", 5), mUXsKykTgB("зомбак убил", 5), mUXsKykTgB("бля вас тоже сервак стопит", 3), mUXsKykTgB("а че муха по пуле стреляет дт врублен же", 5),
	mUXsKykTgB("я стрельнул хоть?", 8), mUXsKykTgB("а ну у меня джитер в тайминг лагнул", 9), mUXsKykTgB("ебать это ты с кряком ебанул меня?", 9), mUXsKykTgB("ай хайдшотс залагал", 1), mUXsKykTgB("бля у меня инет залагал", 6),
	mUXsKykTgB("бля я докшу тя чмо", 2), mUXsKykTgB("чмо с картатая убило фу", 9), mUXsKykTgB("тут ролы пашут?", 7), mUXsKykTgB("сервер гавно", 8),
	mUXsKykTgB("бля я с легит кфг был", 6), mUXsKykTgB("я крутилку не включил сук", 1)
}

death_spammer.handle = function(e)
	if not Vars.Misc.deathsay:get() then
        return
    end

    local attacker_entindex = client.userid_to_entindex(e.attacker)
    local victim_entindex   = client.userid_to_entindex(e.userid)
    local localplayer       = entity.get_local_player
    local enemy             = entity.is_enemy

    if victim_entindex == localplayer() and attacker_entindex ~= localplayer() then
        client.delay_call(client.random_int(4, 8), function()
            client.exec(mUXsKykTgB("jrp ", 17).. death_spammer.phares[client.random_int(1, #death_spammer.phares)])
        end)
    end
end

chat_spammer.phrases = {
mUXsKykTgB("4", 3), mUXsKykTgB("3 моча", 2), mUXsKykTgB("𝔱.𝔪𝔢/𝔢𝔳𝔦𝔩𝔠𝔩𝔲𝔟𝔩𝔲𝔞", 5), mUXsKykTgB("𝖙.𝖒𝖊/𝖊𝖛𝖎𝖑𝖈𝖑𝖚𝖇𝖑𝖚𝖆", 4), mUXsKykTgB("𝓽.𝓶𝓮/𝓮𝓿𝓲𝓵𝓬𝓵𝓾𝓫𝓵𝓾𝓪", 9), mUXsKykTgB("𝕥.𝕞𝕖/𝕖𝕧𝕚𝕝𝕔𝕝𝕦𝕓𝕝𝕦𝕒", 2), mUXsKykTgB("ｔ．ｍｅ／ｅｖｉｌｃｌｕｂｌｕａ", 7), mUXsKykTgB("ţ.Μ𝓔/Ẹν𝐢𝐋Čy𝕌Ⓑ𝕃υά", 13), mUXsKykTgB("『K』『V』『I』『V』『I』『V』『Q』『R』", 13),
mUXsKykTgB("спи моча", 2), mUXsKykTgB("ч в х", 4), mUXsKykTgB(" yzjkfip(kztb): 09", 17), mUXsKykTgB("ngzzk Uuu", 21), mUXsKykTgB("l9", 18), mUXsKykTgB("3a", 2), mUXsKykTgB("88888", 7), mUXsKykTgB("ᶜᵉᵖᵉʳᵃ_ᴷᵃᵖⁿᵒᴮ", 3), mUXsKykTgB("☆꧁༒☬ά𝔀ｐ ЌᎥ𝓝Ğ☬༒꧂☆", 3), mUXsKykTgB("(っ◔◡◔)っ ♥ 7 ♥", 6), mUXsKykTgB("꧁꧁❶꧂꧂", 9), mUXsKykTgB("◦•●◉✿ ❶ ✿◉●•◦", 2), mUXsKykTgB("¤¸¸.•´¯`•¸¸.•..>> １ <<..•.¸¸•´¯`•.¸¸¤", 3), mUXsKykTgB("✞✞", 1), mUXsKykTgB("꧁꧁𝐠𝐮𝐜𝐜𝐢 𝐩𝐥𝐚𝐲𝐬𝐭𝐲𝐥𝐞꧂꧂", 6)}

chat_spammer.handle = function(e)
    if not Vars.Misc.chat_spammer:get() then
        return
    end

    local player = entity.get_local_player()

    if player == nil then
        return
    end

    local victim = client.userid_to_entindex(e.userid)

    if victim == nil or victim == player then
        return
    end

    local attacker = client.userid_to_entindex(e.attacker)

    if attacker ~= player then
        return
    end

    client.exec((mUXsKykTgB('bjh %b', 9)):format(chat_spammer.phrases[math.random(1, #chat_spammer.phrases)]))
end

conditional_antiaims.states = {
    unknown = -1,
    standing = 2,
    moving = 3,
    slowwalk = 4,
    crouching = 5,
    moving_crouch = 6,
    air = 7,
    air_crouch = 8,
    freestand = 9,
    fakelag = 10,
    on_use = 11
}

conditional_antiaims.conditions = {}
for k, name in pairs(conditional_antiaims.conditions_names) do
    local name_unique = mUXsKykTgB('\aPPPPPP00', 10) .. name .. k
    local itemname_start = mUXsKykTgB('vhgwbmbhgl_', 19) .. name .. mUXsKykTgB('_', 5)

    conditional_antiaims.conditions[k] = {}

    if name ~= mUXsKykTgB('Ixqhut', 16) then
        conditional_antiaims.conditions[k].switch = group:checkbox(mUXsKykTgB('Paadl ', 15) .. string.lower(name) .. mUXsKykTgB(' htsinynts', 5))
	end

    conditional_antiaims.conditions[k].pitch = group:combobox(mUXsKykTgB('Fyjsx', 16) .. name_unique, { mUXsKykTgB('Jaa', 21), mUXsKykTgB('Vq', 1), mUXsKykTgB('Itbs', 5), mUXsKykTgB('Yhukvt', 7) })

	conditional_antiaims.conditions[k].yaw_base = group:combobox(mUXsKykTgB('Ikg lkco', 10) .. name_unique, { mUXsKykTgB('Mf fmdsqfe', 12), mUXsKykTgB('Psgep zmia', 4) })
	conditional_antiaims.conditions[k].left_yaw_offset = group:slider(mUXsKykTgB('Cea fewi  »  Pijx wmhi', 4) .. name_unique, -180, 180, 0, 0, mUXsKykTgB('°', 1))
	conditional_antiaims.conditions[k].right_yaw_offset = group:slider(mUXsKykTgB('Uws xwoa  »  Necdp oeza', 22) .. name_unique, -180, 180, 0, 0, mUXsKykTgB('°', 4))
	conditional_antiaims.conditions[k].yaw_randomize = group:slider(mUXsKykTgB('Oqm rqiu  »  Кqdtecypqjyed', 16) .. name_unique, 0, 100, 0, 0, mUXsKykTgB('%', 9), 1, {[0] = mUXsKykTgB("Ull", 6)})

	conditional_antiaims.conditions[k].yaw_modifier = group:combobox(mUXsKykTgB('Oqm zyjjuh', 16) .. name_unique, { mUXsKykTgB('Sjj', 4), mUXsKykTgB('Mddqcr', 24), mUXsKykTgB('Moxdob', 10), mUXsKykTgB('Sboepn', 1), mUXsKykTgB('Hzxiitg', 15), mUXsKykTgB('R&X Iktzkx', 6), mUXsKykTgB('Ogefay imk', 12) })
	conditional_antiaims.conditions[k].yaw_modifier_offset = group:slider(mUXsKykTgB('\npchvgwrsg', 14) .. name_unique, -180, 180, 0, 0, mUXsKykTgB('°', 9))
	conditional_antiaims.conditions[k].yaw_modifier_offset979 = group:slider(mUXsKykTgB('Bdz mlwwhu  »  Ohiw vlgh', 3) .. name_unique, -180, 180, 0, 0, mUXsKykTgB('°', 1))
	conditional_antiaims.conditions[k].yaw_modifier_offset1337 = group:slider(mUXsKykTgB('Wyu hgrrcp  »  Pgefr qgbc', 24) .. name_unique, -180, 180, 0, 0, mUXsKykTgB('°', 9))
	conditional_antiaims.conditions[k].yaw_modifier_offset1 = group:slider(mUXsKykTgB('Cttgsh [5]', 14) .. name_unique, -180, 180, 0, 0, mUXsKykTgB('°', 6))
	conditional_antiaims.conditions[k].yaw_modifier_offset2 = group:slider(mUXsKykTgB('Duuhti [7]', 15) .. name_unique, -180, 180, 0, 0, mUXsKykTgB('°', 8))
	conditional_antiaims.conditions[k].yaw_modifier_offset3 = group:slider(mUXsKykTgB('Ullykz [9]', 6) .. name_unique, -180, 180, 0, 0, mUXsKykTgB('°', 2))

	conditional_antiaims.conditions[k].body_yaw = group:combobox(mUXsKykTgB('Vixs suq', 20) .. name_unique, { mUXsKykTgB('Gxx', 18), mUXsKykTgB('Abbaeufq', 12), mUXsKykTgB('Mlwwhu', 3), mUXsKykTgB('Mnuncw', 20), mUXsKykTgB('Dgydqfhg', 3) })
	conditional_antiaims.conditions[k].body_yaw_offset = group:slider(mUXsKykTgB('\nnkmiarreqf', 12) .. name_unique, -180, 180, 0, 0, mUXsKykTgB('°', 6))
	conditional_antiaims.conditions[k].delay_ticks = group:slider(mUXsKykTgB('\nxyfusncwem', 20) .. name_unique, 1, 14, 1, 0, mUXsKykTgB('a', 7))
	conditional_antiaims.conditions[k].roll_aa = group:slider(mUXsKykTgB('Ifcc', 17) .. name_unique, -45, 45, 0, 0, mUXsKykTgB('°', 2))

	if name ~= mUXsKykTgB('Lgqkrgm', 6) then
		conditional_antiaims.conditions[k].lag_options = group:combobox(mUXsKykTgB('Pybmo nopoxcsfo', 10) .. name_unique, {mUXsKykTgB('Stupjai', 15), mUXsKykTgB('Bmxbzt po', 1)})
		conditional_antiaims.conditions[k].defensive_aa = group:checkbox(mUXsKykTgB('Uvwvejzmv RR', 17) .. name_unique)
		conditional_antiaims.conditions[k].defensive_pitch = group:combobox(mUXsKykTgB('Ibmva\nwxyxglbox_ibmva', 19) .. name_unique, {mUXsKykTgB('Bgqyzjcb', 24), mUXsKykTgB('Wr', 2), mUXsKykTgB('Glyv', 7), mUXsKykTgB('Bkxnyw', 10), mUXsKykTgB('Fxvwrp', 3), mUXsKykTgB('Rknv ihssdq', 25)})
		conditional_antiaims.conditions[k].pitch_slider = group:slider(mUXsKykTgB('\nskijec_tuvudiylu_fyjsx', 16) .. name_unique, -89, 89, 0, 0, mUXsKykTgB('°', 7))
		conditional_antiaims.conditions[k].defensive_yaw = group:combobox(mUXsKykTgB('Qso\nvwxwfkanw_qso', 18) .. name_unique, {mUXsKykTgB('Wbltuexw', 19), mUXsKykTgB('Hxstlpnh', 15), mUXsKykTgB('Jkkjndoz', 21), mUXsKykTgB('Udqgrp', 3), mUXsKykTgB('Czsx', 10), mUXsKykTgB('4-Hlj', 11), mUXsKykTgB('0-Lpn', 15), mUXsKykTgB('Fxvwrp', 3)})
		conditional_antiaims.conditions[k].yaw_slider = group:slider(mUXsKykTgB('\nxpnojh_yzazindqz_tvr', 21) .. name_unique, -180, 180, 0, 0, mUXsKykTgB('°', 2))
	end
end

experimental_defensive111 = group:label(mUXsKykTgB(' ', 1))
experimental_defensive = group:checkbox(mUXsKykTgB('\aTT4444TT⚠  Sldsfwasbhoz rstsbgwjs', 14))

conditional_antiaims.desync_delta = 0
conditional_antiaims.get_desync_delta = function()
    local player = entity.get_local_player()

    if player == nil then
        return
    end

    conditional_antiaims.desync_delta = math.normalize_yaw(entity.get_prop(player, mUXsKykTgB('g_zfJimyJulugynyl', 20), 11) * 120 - 60) / 2
end

conditional_antiaims.current_side = false
conditional_antiaims.get_desync_side = function()
    local player = entity.get_local_player()

    if player == nil then
        return
    end

    if globals.chokedcommands() ~= 0 then 
        return
    end

    local body_yaw = entity.get_prop(player, mUXsKykTgB('w_pvZycoZkbkwodob', 10), 11) * 120 - 60

    conditional_antiaims.current_side = body_yaw > 0
end

conditional_antiaims.randomization = 0
conditional_antiaims.current_choke = 0
conditional_antiaims.choked_ticks_prev = 0
conditional_antiaims.get_current_choke = function(cmd)
    local player = entity.get_local_player()

    if player == nil then
        return
    end

	local choked_ticks = cmd.chokedcommands
	if conditional_antiaims.choked_ticks_prev >= choked_ticks or choked_ticks == 0 then
		conditional_antiaims.current_choke = conditional_antiaims.choked_ticks_prev
	end
	
	conditional_antiaims.choked_ticks_prev = choked_ticks
end

conditional_antiaims.calc_randomization = function (new_config)
	local left, right = new_config.left_yaw_offset, new_config.right_yaw_offset
	local factor = new_config.yaw_randomize * 0.01

	return math.random(left * factor, right * factor)
end

conditional_antiaims.yaw_randomize = function(new_config)
    new_config.yaw_offset = (new_config.yaw_offset or 0) + math.random(-new_config.yaw_randomize, new_config.yaw_randomize)
end

conditional_antiaims.swap = false
conditional_antiaims.set_yaw_right_left = safecall(mUXsKykTgB('tfu_zbx_sjhiu_mfgu', 1), true, function(new_config)
	if (conditional_antiaims.manual_dir == -90 or conditional_antiaims.manual_dir == 90) then
		return
	end

	local weapon = entity.get_player_weapon(entity.get_local_player())
    local knife = weapon ~= nil and entity.get_classname(weapon) == mUXsKykTgB('QYbwts', 14)
    local zeus = weapon ~= nil and entity.get_classname(weapon) == mUXsKykTgB('NHplazyEldpc', 11) 
	
    local safe_knife = (Vars.AA.safe_functions.enable:get() and Vars.AA.safe_functions.settings:get(mUXsKykTgB('Ormji', 4))) and knife
    local safe_zeus = (Vars.AA.safe_functions.enable:get() and Vars.AA.safe_functions.settings:get(mUXsKykTgB('Vaqo', 22))) and zeus
	local required_states = (conditional_antiaims.player_state == conditional_antiaims.states.air_crouch or conditional_antiaims.player_state == conditional_antiaims.states.fakelag)
	local safe_active = conditional_antiaims.safe_active or ((safe_knife or safe_zeus) and required_states)

    if new_config.body_yaw ~= mUXsKykTgB('Knfkxmon', 10) and not safe_active then
		if conditional_antiaims.chokedcommands == 0 then
        	new_config.yaw_offset = (conditional_antiaims.current_side and new_config.left_yaw_offset or new_config.right_yaw_offset)
		end
    end
end)

conditional_antiaims.get_distance = function() 
    local result = math.huge;
    local heightDifference = 0;
    local localplayer = entity.get_local_player();
    local entities = entity.get_players(true);

    for i = 1, #entities do
      local ent = entities[i];
	  local ent_origin = { entity.get_origin(ent) }
	  local lp_origin = { entity.get_origin(localplayer) }
      if ent ~= localplayer and entity.is_alive(ent) then
        local distance = (vector(ent_origin[1], ent_origin[2], ent_origin[3]) - vector(lp_origin[1], lp_origin[2], lp_origin[3])):length2d();
        if distance < result then 
            result = distance; 
            heightDifference = ent_origin[3] - lp_origin[3];
        end
      end
    end
  
    return math.floor(result/10), math.floor(heightDifference);
end

conditional_antiaims.new_meta_defensive = safecall(mUXsKykTgB('gxp_fxmt_wxyxglbox', 19), true, function(new_config, cmd)
    local plocal = entity.get_local_player()
    if (plocal == nil) or (not entity.is_alive(plocal)) then
        return end

    local is_grenade = entity.get_classname(entity.get_player_weapon(plocal)):find(mUXsKykTgB('Alyhuxy', 20)) or false
    local distance_to_enemy = {conditional_antiaims.get_distance()}
    local is_manuals = (conditional_antiaims.manual_dir == -90 or conditional_antiaims.manual_dir == 90)
    local weapon = entity.get_player_weapon(plocal)
    local safe_knife = ((Vars.AA.safe_functions.enable:get() and Vars.AA.safe_functions.settings:get(mUXsKykTgB('Ilgdc', 24))) and (weapon ~= nil and entity.get_classname(weapon) == mUXsKykTgB('ZHkfcb', 23))) and (conditional_antiaims.player_state == conditional_antiaims.states.air_crouch or conditional_antiaims.player_state == conditional_antiaims.states.fakelag)
    local safe_zeus = ((Vars.AA.safe_functions.enable:get() and Vars.AA.safe_functions.settings:get(mUXsKykTgB('Diyw', 4))) and (weapon ~= nil and entity.get_classname(weapon) == mUXsKykTgB('ICkgvutZgykx', 6))) and (conditional_antiaims.player_state == conditional_antiaims.states.air_crouch or conditional_antiaims.player_state == conditional_antiaims.states.fakelag)
    local is_safe = ((Vars.AA.safe_functions.enable:get() and Vars.AA.safe_functions.defensive_aa:get() and (not is_manuals)) and (((Vars.AA.safe_functions.head_settings:get(mUXsKykTgB('Strs Otdelynp', 11)) and distance_to_enemy[1] > 119) and (conditional_antiaims.player_state == conditional_antiaims.states.crouching or conditional_antiaims.player_state == conditional_antiaims.states.standing or conditional_antiaims.player_state == conditional_antiaims.states.fakelag)) or ((Vars.AA.safe_functions.head_settings:get(mUXsKykTgB('Wtxvwi', 15)) and distance_to_enemy[2] < -50) and (conditional_antiaims.player_state == conditional_antiaims.states.crouching or conditional_antiaims.player_state == conditional_antiaims.states.moving_crouch or conditional_antiaims.player_state == conditional_antiaims.states.standing or conditional_antiaims.player_state == conditional_antiaims.states.air_crouch or conditional_antiaims.player_state == conditional_antiaims.states.fakelag))))
	local safe_conds = (((Vars.AA.safe_functions.head_settings:get(mUXsKykTgB('Mnlm Inxyfshj', 5)) and distance_to_enemy[1] > 119) and (conditional_antiaims.player_state == conditional_antiaims.states.crouching or conditional_antiaims.player_state == conditional_antiaims.states.standing or conditional_antiaims.player_state == conditional_antiaims.states.fakelag)) or ((Vars.AA.safe_functions.head_settings:get(mUXsKykTgB('Nkomnz', 6)) and distance_to_enemy[2] < -50) and (conditional_antiaims.player_state == conditional_antiaims.states.crouching or conditional_antiaims.player_state == conditional_antiaims.states.moving_crouch or conditional_antiaims.player_state == conditional_antiaims.states.standing or conditional_antiaims.player_state == conditional_antiaims.states.air_crouch or conditional_antiaims.player_state == conditional_antiaims.states.fakelag)))
	
	if is_manuals and Vars.AA.manuals.lag_options:get() == mUXsKykTgB('Bmxbzt po', 1) then
		cmd.force_defensive = true
	elseif (is_safe or safe_knife or safe_zeus) and Vars.AA.safe_functions.lag_options:get() == mUXsKykTgB('Xitxvp lk', 23) then
		cmd.force_defensive = true
	elseif safe_conds and not (Vars.AA.safe_functions.enable:get() and Vars.AA.safe_functions.defensive_aa:get()) then
		cmd.force_defensive = false
	else
		cmd.force_defensive = new_config.lag_options == mUXsKykTgB('Cnycau qp', 2)
	end
end)


conditional_antiaims.delay_latest_yaw = 0
conditional_antiaims.delay_switch = safecall(mUXsKykTgB('pqxmk_eiufot', 12), true, function(new_config)
   	if (conditional_antiaims.manual_dir == -90 or conditional_antiaims.manual_dir == 90) then 
        return 
    end
	if globals.tickcount() % (new_config.delay_ticks * 2) == 0 then
		conditional_antiaims.swap = not conditional_antiaims.swap
	end
	
    if new_config.body_yaw == mUXsKykTgB('Bewbodfe', 1) then
        new_config.body_yaw = mUXsKykTgB('Opwpey', 22)
		if conditional_antiaims.chokedcommands == 0 then
        	conditional_antiaims.delay_latest_yaw = (conditional_antiaims.swap and new_config.left_yaw_offset or new_config.right_yaw_offset)
        	new_config.body_yaw_offset = conditional_antiaims.swap and -1 or 1
		end
		new_config.yaw_offset = conditional_antiaims.delay_latest_yaw
    end
end)

conditional_antiaims.meta_latest_yaw = 0
conditional_antiaims.new_meta_antiaims = safecall(mUXsKykTgB('dum_cujq_qdjyqyci', 16), true, function(new_config)
	
    local yaw = conditional_antiaims.current_side and new_config.left_yaw_offset or new_config.right_yaw_offset
    local value = yaw

    if new_config.yaw_modifier == mUXsKykTgB('I&O Zbkqbo', 23) then
        new_config.yaw_modifier = mUXsKykTgB('Bdmsdq', 25)
        new_config.yaw_modifier_offset = conditional_antiaims.current_side and new_config.yaw_modifier_offset1337 or new_config.yaw_modifier_offset979
        new_config.yaw_offset = conditional_antiaims.current_side and new_config.left_yaw_offset or new_config.right_yaw_offset
    elseif new_config.yaw_modifier == mUXsKykTgB('Tljkfd nrp', 17) then
        if globals.tickcount() % 3 == 0 then
            value = new_config.yaw_modifier_offset1
        elseif globals.tickcount() % 3 == 1 then
            value = new_config.yaw_modifier_offset2
        elseif globals.tickcount() % 3 == 2 then
            value = new_config.yaw_modifier_offset3
        end
        new_config.yaw_modifier_offset = 0
        new_config.yaw_modifier = mUXsKykTgB('Qsbhsf', 14)
    end
    
    if new_config.advanced_body_yaw ~= mUXsKykTgB('Hkchujlk', 7) then
		if conditional_antiaims.chokedcommands == 0 then
			conditional_antiaims.meta_latest_yaw = value
		end
	else
		conditional_antiaims.meta_latest_yaw = 0
    end
end)

conditional_antiaims.get_active_idx = function(idx)
    local name = conditional_antiaims.conditions_names[idx]
    if name ~= nil then
        if idx ~= 1 and conditional_antiaims.conditions[idx].switch:get() then
            return idx
        end
    end

    return 1
end

conditional_antiaims.get_cond_values = function(idx)
    local cond_tbl = conditional_antiaims.conditions_names[idx]
    if cond_tbl == nil then
        return
    end

    local new_config = {}

    for k, v in pairs(conditional_antiaims.conditions[idx]) do
        new_config[k] = v:get()
    end

    return new_config
end

conditional_antiaims.set_ui = safecall(mUXsKykTgB('jvk_lz', 17), true, function(new_config)
    for k, v in pairs(new_config) do
        if gamesense_refs._vars[k] ~= nil then
            gamesense_refs.override(k, v)
        end
    end
end)

conditional_antiaims.defensive1 = 0
conditional_antiaims.defensive_handle1 = function(cmd)
	local lp = entity.get_local_player()

    if lp == nil or not entity.is_alive(lp) then
        return
    end

    local Entity = native_GetClientEntity(lp)
    local m_flOldSimulationTime = ffi.cast(mUXsKykTgB('jpsex*', 4), ffi.cast(mUXsKykTgB('ochnjnl_n', 20), Entity) + 0x26C)[0]
    local m_flSimulationTime = entity.get_prop(lp, mUXsKykTgB('n_gmTjnvmbujpoUjnf', 1))

    local delta = m_flOldSimulationTime - m_flSimulationTime;

    if delta > 0 then
		conditional_antiaims.defensive1 = globals.tickcount() + toticks(delta - client.latency()) - 5;
        return;
    end
end

conditional_antiaims.defensive = 0
conditional_antiaims.defensive_handle = function(cmd)
	local lp = entity.get_local_player()

    if lp == nil or not entity.is_alive(lp) then
        return
    end

    local Entity = native_GetClientEntity(lp)
    local m_flOldSimulationTime = ffi.cast(mUXsKykTgB('iordw*', 3), ffi.cast(mUXsKykTgB('bpuaway_a', 7), Entity) + 0x26C)[0]
    local m_flSimulationTime = entity.get_prop(lp, mUXsKykTgB('l_ekRhltkzshnmShld', 25))

    local delta = m_flOldSimulationTime - m_flSimulationTime;

    if delta > 0 then
		conditional_antiaims.defensive = globals.tickcount() + toticks(delta - client.latency());
		conditional_antiaims.defensive1 = globals.tickcount() + toticks(delta - client.latency()) - 5;
        return;
    end
end

conditional_antiaims.manual_cur = nil
conditional_antiaims.manual_dir = 0

conditional_antiaims.manual_keys = {
	{ mUXsKykTgB("nghv", 2), yaw = -90, item = Vars.AA.manuals.left },
	{ mUXsKykTgB("fwuvh", 14), yaw = 90, item = Vars.AA.manuals.right },
	{ mUXsKykTgB("lymyn", 20), yaw = nil, item = Vars.AA.manuals.reset },
}

for i, v in ipairs(conditional_antiaims.manual_keys) do
	local active, mode, key = v.item:get()
	v.item:set(mUXsKykTgB("Idvvat", 15), key)
end

conditional_antiaims.get_manuals = function()
	if not (Vars.AA.enable:get() and Vars.AA.manuals.enable:get()) then return end

	for i, v in ipairs(conditional_antiaims.manual_keys) do
		local active, mode = v.item:get()

		if v.active == nil then v.active = active end
		if v.active == active then goto done end

		v.active = active

		if v.yaw == nil then conditional_antiaims.manual_cur = nil end

		if mode == 1 then conditional_antiaims.manual_cur = active and i or nil goto done
		elseif mode == 2 then conditional_antiaims.manual_cur = conditional_antiaims.manual_cur ~= i and i or nil goto done end

		::done::
	end

	conditional_antiaims.manual_dir = conditional_antiaims.manual_cur ~= nil and conditional_antiaims.manual_keys[conditional_antiaims.manual_cur].yaw or 0
end

conditional_antiaims.set_yaw_base = function(new_config)

    if not Vars.AA.enable:get() then
        return
    end

    local player = entity.get_local_player()

    if player == nil then
        return
    end

    local manuals_over_fs = Vars.AA.manuals.manuals_over_fs:get()
    local is_manuals = (conditional_antiaims.manual_dir == -90 or conditional_antiaims.manual_dir == 90)

	local defensive = not (globals.tickcount() > (experimental_defensive:get() and conditional_antiaims.defensive1 or conditional_antiaims.defensive) and conditional_antiaims.states.fakelag)
    local weapon = entity.get_player_weapon(player)

    local knife = weapon ~= nil and entity.get_classname(weapon) == mUXsKykTgB('VDgbyx', 19)
    local zeus = weapon ~= nil and entity.get_classname(weapon) == mUXsKykTgB('OIqmbazFmeqd', 12) 

    local safe_knife = (Vars.AA.safe_functions.enable:get() and Vars.AA.safe_functions.settings:get(mUXsKykTgB('Adyvu', 16))) and knife
    local safe_zeus = (Vars.AA.safe_functions.enable:get() and Vars.AA.safe_functions.settings:get(mUXsKykTgB('Fkay', 6))) and zeus
    local safe_head = (Vars.AA.safe_functions.enable:get() and Vars.AA.safe_functions.settings:get(mUXsKykTgB('Fcyb', 24)))
    local distance_to_enemy = {conditional_antiaims.get_distance()}
    local is_predefined = conditional_antiaims.manual_dir

	if not antiaim_on_use.enabled and is_manuals then
        if Vars.AA.manuals.enable:get() then
            new_config.yaw_modifier = mUXsKykTgB('Gxx', 18)
			new_config.body_yaw = mUXsKykTgB('Klslau', 18)
			new_config.body_yaw_offset = Vars.AA.manuals.inverter:get() and 1 or -1
        end

        if is_predefined then
			new_config.yaw_base = mUXsKykTgB('Wznlw gtph', 11)
        	new_config.yaw_offset = (((Vars.AA.manuals.enable:get() and not (Vars.AA.manuals.defensive_aa:get() and defensive)) and is_predefined) or new_config.yaw_offset)
        end
	elseif (safe_knife or safe_zeus) and (conditional_antiaims.player_state == conditional_antiaims.states.air_crouch or conditional_antiaims.player_state == conditional_antiaims.states.fakelag) then
		conditional_antiaims.safe_active = true
    elseif (safe_head and not is_manuals) then
        if conditional_antiaims.safe_active then
			new_config.yaw_modifier = mUXsKykTgB('Vmm', 7)
			new_config.body_yaw = mUXsKykTgB('Jkrkzt', 17)
			new_config.body_yaw_offset = 0
        end
    end

	new_config.roll_aa = new_config.roll_aa
    new_config.freestanding = not (manuals_over_fs and (is_manuals)) and gamesense_refs.freestanding:get()
end

conditional_antiaims.player_state = 1
conditional_antiaims.update_player_state = function(cmd)

    local localplayer = entity.get_local_player()

    if localplayer == nil then
        return
    end

    local flags = entity.get_prop(localplayer, mUXsKykTgB('d_wWcrxj', 17))
	local m_vecVelocity = { entity.get_prop(localplayer, mUXsKykTgB('r_ajhAjqthnyd', 5)) }
    local is_crouching = bit.band(flags, bit.lshift(1, 1)) ~= 0
    local on_ground = bit.band(flags, bit.lshift(1, 0)) ~= 0
    local is_not_moving = math.sqrt(m_vecVelocity[1] ^ 2 + m_vecVelocity[2] ^ 2) < 2
    local is_slowwalk = ui.get(gamesense_refs.slow[1]) and ui.get(gamesense_refs.slow[2])
    local is_jumping = cmd.in_jump ~= 0

	model_breaker.in_air = is_jumping or not on_ground

    if antiaim_on_use.enabled then
        conditional_antiaims.player_state = conditional_antiaims.states.on_use
        return
    end

    if (ui.get(gamesense_aa.freestanding[1]) and ui.get(gamesense_aa.freestanding[2])) and not antiaim_on_use.enabled then
        conditional_antiaims.player_state = conditional_antiaims.states.freestand
        return
    end

    if not (ui.get(gamesense_refs.dt[1]) and ui.get(gamesense_refs.dt[2])) and not (ui.get(gamesense_refs.os[1]) and ui.get(gamesense_refs.os[2]))
	and conditional_antiaims.conditions[conditional_antiaims.states.fakelag].switch.value then
        conditional_antiaims.player_state = conditional_antiaims.states.fakelag
        return
    end

    if is_crouching and (is_jumping or not on_ground) then
        conditional_antiaims.player_state = conditional_antiaims.states.air_crouch
        return
    end

    if is_jumping or not on_ground then
        conditional_antiaims.player_state = conditional_antiaims.states.air
        return
    end

    if is_slowwalk then
        conditional_antiaims.player_state = conditional_antiaims.states.slowwalk
        return
    end

    if not is_crouching and is_not_moving then
        conditional_antiaims.player_state = conditional_antiaims.states.standing
        return
    end

    if is_crouching and is_not_moving then
        conditional_antiaims.player_state = conditional_antiaims.states.crouching
        return
    end

    if is_crouching and not is_not_moving then
        conditional_antiaims.player_state = conditional_antiaims.states.moving_crouch
        return
    end

    if not is_crouching and not is_not_moving and not is_slowwalk then
        conditional_antiaims.player_state = conditional_antiaims.states.moving
        return
    end

    conditional_antiaims.player_state = conditional_antiaims.states.unknown
end

aero_lag_exp.get_dt_charge = function(ent)
    local m_nTickBase = entity.get_prop(ent, mUXsKykTgB("d_eKztbSrjv", 17))
    local shift = math.floor(m_nTickBase - globals.tickcount() - toticks(client.latency()) * 0.4)
    local wanted = -15 + (ui.get(gamesense_refs.dt_fakelag) - 1) + 5

    return math.min(1, math.max(0, shift / wanted)) == 1
end

conditional_antiaims.handle = function(cmd)
	conditional_antiaims.chokedcommands = cmd.chokedcommands
    conditional_antiaims.update_player_state(cmd)
	conditional_antiaims.get_current_choke(cmd)
	conditional_antiaims.get_desync_delta()
    conditional_antiaims.get_desync_side()

	local backstab_allow = false
	local idx = conditional_antiaims.player_state
	local distance_to_enemy = {conditional_antiaims.get_distance()}   
    local current_condition = conditional_antiaims.get_active_idx(idx)
   	local new_config = conditional_antiaims.get_cond_values(current_condition)
	local defensive = not (globals.tickcount() > (experimental_defensive:get() and conditional_antiaims.defensive1 or conditional_antiaims.defensive) and conditional_antiaims.states.fakelag)
	local is_manuals = (conditional_antiaims.manual_dir == -90 or conditional_antiaims.manual_dir == 90)

	new_config.yaw_offset = conditional_antiaims.meta_latest_yaw
	
	local weapon = entity.get_player_weapon(entity.get_local_player())
    local knife = weapon ~= nil and entity.get_classname(weapon) == mUXsKykTgB('UCfaxw', 18)
    local zeus = weapon ~= nil and entity.get_classname(weapon) == mUXsKykTgB('ZTbxmlkQxpbo', 23) 
	
    local safe_knife = (Vars.AA.safe_functions.enable:get() and Vars.AA.safe_functions.settings:get(mUXsKykTgB('Nqlih', 3))) and knife
    local safe_zeus = (Vars.AA.safe_functions.enable:get() and Vars.AA.safe_functions.settings:get(mUXsKykTgB('Tyom', 20))) and zeus
    local safe_head = (Vars.AA.safe_functions.enable:get() and Vars.AA.safe_functions.settings:get(mUXsKykTgB('Gdzc', 25)))

	conditional_antiaims.safe_active = (
		(Vars.AA.safe_functions.enable:get() and (not is_manuals)) and 
		Vars.AA.safe_functions.settings:get(mUXsKykTgB("Vsor", 14)) and
		(
			(
				(Vars.AA.safe_functions.head_settings:get(mUXsKykTgB('Opno Kpzahujl', 7)) and distance_to_enemy[1] > 119) and 
				(conditional_antiaims.player_state == conditional_antiaims.states.crouching or 
				conditional_antiaims.player_state == conditional_antiaims.states.standing or 
				conditional_antiaims.player_state == conditional_antiaims.states.fakelag)
			) 
			or 
			(
				(Vars.AA.safe_functions.head_settings:get(mUXsKykTgB('Khljkw', 3)) and distance_to_enemy[2] < -50) and 
				(conditional_antiaims.player_state == conditional_antiaims.states.crouching or 
				conditional_antiaims.player_state == conditional_antiaims.states.moving_crouch or 
				conditional_antiaims.player_state == conditional_antiaims.states.standing or 
				conditional_antiaims.player_state == conditional_antiaims.states.air_crouch or 
				conditional_antiaims.player_state == conditional_antiaims.states.fakelag)
			)
		)
	)
	
	conditional_antiaims.new_meta_antiaims(new_config)
	conditional_antiaims.new_meta_defensive(new_config, cmd)
	conditional_antiaims.set_yaw_right_left(new_config)
	conditional_antiaims.get_manuals()

	conditional_antiaims.randomization = conditional_antiaims.calc_randomization(new_config)

	gamesense_refs.freestanding.hotkey:set(mUXsKykTgB('Palpnh dc', 15))
	gamesense_refs.override(mUXsKykTgB('qcppdelyotyr', 11), (Vars.AA.freestanding:get() and Vars.AA.freestanding.hotkey:get()) and not antiaim_on_use.enabled)
	
	if Vars.AA.edge_yaw then
		gamesense_refs.override(mUXsKykTgB('srus_mok', 14), Vars.AA.edge_yaw:get() and Vars.AA.edge_yaw.hotkey:get())
	end

	if new_config.defensive_pitch == mUXsKykTgB('Fybj wvggre', 13) then
		local current_time = globals.curtime()
		local speed = 2.0 
		
		if current_time - slow_jitter_time > 0.01 then 
			slow_jitter_current_pitch = slow_jitter_current_pitch + (slow_jitter_direction * speed)
			
			if slow_jitter_current_pitch >= 39 then
				slow_jitter_current_pitch = 39
				slow_jitter_direction = -1
			elseif slow_jitter_current_pitch <= -39 then
				slow_jitter_current_pitch = -39
				slow_jitter_direction = 1
			end
			
			slow_jitter_time = current_time
		end
	end

	local pitch_tbl = {
        [mUXsKykTgB('Inxfgqji', 5)] = 89,
        [mUXsKykTgB('Ql', 22)] = -89,
        [mUXsKykTgB('Xcpm', 24)] = 0,
        [mUXsKykTgB('Udqgrp', 3)] = math.random(-89, 89),
        [mUXsKykTgB('Asqrmk', 24)] = new_config.pitch_slider,
        [mUXsKykTgB('Ohks feppan', 22)] = slow_jitter_current_pitch
    }
	
	local yaw_tbl = {
        [mUXsKykTgB('Sxhpqats', 15)] = 0,
        [mUXsKykTgB('Deedhxit', 15)] = is_manuals and conditional_antiaims.manual_dir*-1 or client.random_int(160, 180),
        [mUXsKykTgB('Tjefxbzt', 1)] = globals.tickcount() % 3 == 0 and client.random_int(-100, -90) or globals.tickcount() % 3 == 1 and 180 or globals.tickcount() % 3 == 2 and client.random_int(90, 100) or 0,
        [mUXsKykTgB('Luhxig', 20)] = math.random(-180, 180),
        [mUXsKykTgB('Ifyd', 16)] = math.normalize_yaw(globals.curtime() * 1000),
        [mUXsKykTgB('0-Dhf', 7)] = globals.tickcount() % 3 == 0 and client.random_int(-110, -90) or globals.tickcount() % 3 == 1 and client.random_int(90, 120) or globals.tickcount() % 3 == 2 and client.random_int(-180, -150) or 0,
        [mUXsKykTgB('3-Osq', 18)] = globals.tickcount() % 5 == 0 and client.random_int(-90, -75) or globals.tickcount() % 5 == 1 and client.random_int(-45, -30) or globals.tickcount() % 5 == 2 and client.random_int(-180, -160) or globals.tickcount() % 5 == 3 and client.random_int(45, 60) or globals.tickcount() % 5 == 3 and client.random_int(90, 110) or 0,
        [mUXsKykTgB('Iayzus', 6)] = new_config.yaw_slider
	}


	new_config.yaw_offset1 = mUXsKykTgB('413', 3)
	conditional_antiaims.delay_switch(new_config)
	conditional_antiaims.set_yaw_base(new_config)

	if not is_manuals then
		if conditional_antiaims.safe_active then
			new_config.yaw_offset = 0
			new_config.yaw_modifier = mUXsKykTgB('Hyy', 19)
			new_config.body_yaw = mUXsKykTgB('Ctt', 14)
			new_config.body_yaw_offset = 0
		else
			new_config.yaw_offset = new_config.yaw_offset + conditional_antiaims.randomization
		end

		if Vars.AA.avoid_backstab ~= nil and (Vars.AA.avoid_backstab:get()) then
			local players = entity.get_players(true)
			for i=1, #players do
				local x, y, z = entity.get_prop(players[i], mUXsKykTgB('v_enlXarprw', 9))
				local origin = vector(entity.get_prop(entity.get_local_player(), mUXsKykTgB('i_rayKnecej', 22)))
				local distance = math.sqrt((x - origin.x)^2 + (y - origin.y)^2 + (z - origin.z)^2) 
				local weapon = entity.get_player_weapon(players[i])
				if entity.get_classname(weapon) == mUXsKykTgB('BJmhed', 25) and distance <= 200 then
					backstab_allow = true
					new_config.yaw_offset = 180
					new_config.pitch = mUXsKykTgB('Izz', 20)
				end
			end
		end

		if defensive then
			if new_config.defensive_aa and not (is_manuals or (conditional_antiaims.safe_active) or safe_knife or safe_zeus or backstab_allow) then
				new_config.pitch = mUXsKykTgB('Ldbcxv', 9)
				new_config.pitch_value = pitch_tbl[new_config.defensive_pitch]
				new_config.yaw_offset = yaw_tbl[new_config.defensive_yaw]
			elseif (Vars.AA.safe_functions.defensive_aa:get() and (safe_head) and not is_manuals) then
				if ((Vars.AA.safe_functions.enable:get() and Vars.AA.safe_functions.defensive_aa:get() and (not is_manuals)) and (((Vars.AA.safe_functions.head_settings:get(mUXsKykTgB('Wxvw Sxhipcrt', 15)) and distance_to_enemy[1] > 119) and (conditional_antiaims.player_state == conditional_antiaims.states.crouching or conditional_antiaims.player_state == conditional_antiaims.states.standing or conditional_antiaims.player_state == conditional_antiaims.states.fakelag)) or ((Vars.AA.safe_functions.head_settings:get(mUXsKykTgB('Qnrpqc', 9)) and distance_to_enemy[2] < -50) and (conditional_antiaims.player_state == conditional_antiaims.states.crouching or conditional_antiaims.player_state == conditional_antiaims.states.moving_crouch or conditional_antiaims.player_state == conditional_antiaims.states.standing or conditional_antiaims.player_state == conditional_antiaims.states.air_crouch or conditional_antiaims.player_state == conditional_antiaims.states.fakelag)))) then
					new_config.pitch = mUXsKykTgB('Dvtupn', 1)
					new_config.pitch_value = pitch_tbl[Vars.AA.safe_functions.defensive_pitch:get()]
					new_config.yaw_offset = yaw_tbl[Vars.AA.safe_functions.defensive_yaw:get()]
				elseif (safe_knife or safe_zeus) then
					new_config.pitch = mUXsKykTgB('Asqrmk', 24)
					new_config.pitch_value = pitch_tbl[Vars.AA.safe_functions.defensive_pitch:get()]
					new_config.yaw_offset = yaw_tbl[Vars.AA.safe_functions.defensive_yaw:get()]
				end
			end
		end
	else
		if defensive then
			if Vars.AA.manuals.defensive_aa:get() and is_manuals then
				new_config.pitch = mUXsKykTgB('Fxvwrp', 3)
				new_config.pitch_value = pitch_tbl[Vars.AA.manuals.defensive_pitch:get()]
				new_config.yaw_offset = yaw_tbl[Vars.AA.manuals.defensive_yaw:get()]
			end
		end
	end
	new_config.yaw_offset = math.normalize_yaw(new_config.yaw_offset)

	conditional_antiaims.set_ui(new_config)
end

aero_lag_exp.switch_var = false
if Vars.AA.air_exploit then
	Vars.AA.air_exploit.enable:set_callback(function(self)
		if not self.value then
			gamesense_refs.rage_enable:set(true)
		end
		aero_lag_exp.duck, aero_lag_exp.duck_mode, aero_lag_exp.duck_key = gamesense_refs.duck_peek:get()
	end)
end
aero_lag_exp.handle = safecall(mUXsKykTgB('quhe_bqw_unf.xqdtbu', 16), true, function()
    if not (Vars.AA.air_exploit and Vars.AA.air_exploit.enable:get()) then
        return
    end

	if not Vars.AA.air_exploit.enable.hotkey:get() then
        return
    end

	local distance_to_enemy = {conditional_antiaims.get_distance()}
	local dt_active = { gamesense_refs.duck_peek:get() }
	local in_air = not (bit.band(entity.get_prop(entity.get_local_player(), mUXsKykTgB('a_tTzoug', 14)), bit.lshift(1, 0)) ~= 0)
	gamesense_refs.rage_enable:override((not (elements.hotkey.enum[dt_active[2]] == mUXsKykTgB('Nyjnlf ba', 13))) and aero_lag_exp.get_dt_charge(entity.get_local_player()) or (conditional_antiaims.player_state == conditional_antiaims.states.fakelag) or (not in_air))
	
	if (conditional_antiaims.player_state == conditional_antiaims.states.fakelag) then
		return
	end

    local players = entity.get_players(true)
    local threats = client.current_threat()

    local delay = Vars.AA.air_exploit.exp_tick:get()
    if globals.tickcount() % delay == 1 then 
        aero_lag_exp.switch_var = not aero_lag_exp.switch_var 
    end

	gamesense_refs.duck_peek:set(elements.hotkey.enum[aero_lag_exp.duck_mode])
    if Vars.AA.air_exploit.while_visible:get() then
        for i = 1, #players do
            if entity.is_dormant(players[i]) then
                return
            end

			if distance_to_enemy[1] > 119 then
				return
			end

            if players[i] == threats then
                if in_air and aero_lag_exp.switch_var then
					gamesense_refs.duck_peek:set(mUXsKykTgB('Qbmqoi ed', 16))
                end
            end
        end
    else
        if in_air and aero_lag_exp.switch_var then
			gamesense_refs.duck_peek:set(mUXsKykTgB('Hsdhfz vu', 7))
        end
    end
end)

antiaim_on_use.enabled = false
antiaim_on_use.start_time = globals.realtime()
antiaim_on_use.handle = function(cmd)

    antiaim_on_use.enabled = false

    if not Vars.AA.enable:get() then
        return
    end

    if not conditional_antiaims.conditions[11].switch:get() then
        return
    end

    if cmd.in_use == 0 then
        antiaim_on_use.start_time = globals.realtime()
        return
    end

    local player = entity.get_local_player()

    if player == nil then
        return
    end

    local player_origin = { entity.get_origin(player) }
	
    local CPlantedC4 = entity.get_all(mUXsKykTgB('GTperxihG8', 4))
    local dist_to_bomb = 999

    if #CPlantedC4 > 0 then
        local bomb = CPlantedC4[1]
        local bomb_origin = { entity.get_origin(bomb) }

        dist_to_bomb = vector(player_origin[1], player_origin[2], player_origin[3]):dist(vector(bomb_origin[1], bomb_origin[2], bomb_origin[3]))
    end

    local CHostage = entity.get_all(mUXsKykTgB('EJquvcig', 2))
    local dist_to_hostage = 999

    if CHostage ~= nil then
        if #CHostage > 0 then
            local hostage_origin = {entity.get_origin(CHostage[1])}

            dist_to_hostage = math.min(vector(player_origin[1], player_origin[2], player_origin[3]):dist(vector(hostage_origin[1], hostage_origin[2], hostage_origin[3])), vector(player_origin[1], player_origin[2], player_origin[3]):dist(vector(hostage_origin[1], hostage_origin[2], hostage_origin[3])))
        end
    end

    if dist_to_hostage < 65 and entity.get_prop(player, mUXsKykTgB('w_sDokwXew', 10)) ~= 2 then
        return
    end

    if dist_to_bomb < 65 and entity.get_prop(player, mUXsKykTgB('z_vGrnzAhz', 13)) ~= 2 then
        return
    end

    if cmd.in_use then
        if globals.realtime() - antiaim_on_use.start_time < 0.02 then
            return
        end
    end

    cmd.in_use = false
    antiaim_on_use.enabled = true

end

widgets.branded_watermark = {}

widgets.branded_watermark.handle = function()
    local anim = animations.new(mUXsKykTgB('xjehfut_csboefe_xbufsnbsl', 1), Vars.Misc.branded_watermark.value and 255 or 0)
	if anim < 1 then return end

	local accent = { Vars.Misc.branded_watermark.color:get() }
    local design_username = information.user

	local x, y = client.screen_size()
	local center = { x = x*0.5, y = y*0.5 }

	local white = { 255, 255, 255, anim }
	local design_accent_color = { accent[1], accent[2], accent[3], anim }

	local text = { 
		[1] = string.format(mUXsKykTgB('CTGJAJSZ\a', 24), utils.rgb_to_hex(design_accent_color)),
		[2] = string.format(mUXsKykTgB('QOAN - %o [\a%o%o\a%o]', 22), string.upper(design_username), utils.rgb_to_hex(design_accent_color), string.upper(information.version), utils.rgb_to_hex(white))
	}
	local measure = { render.measure_text(mUXsKykTgB('-', 3), text[2]) }

	render.text(40, center.y + 5, 255, 255, 255, anim, mUXsKykTgB('-', 7), 0, text[1])
	render.text(40, center.y + (measure[2] + 2), 255, 255, 255, anim, mUXsKykTgB('-', 9), 0, text[2])
end

fast_ladder.handle = function(cmd)
    if not Vars.Misc.fast_ladder:get() then
        return
    end

	local plocal = entity.get_local_player()
	if plocal == nil then
		return end
	local pitch, yaw = client.camera_angles()
	if entity.get_prop(plocal, mUXsKykTgB('e_EgnwLqhw', 18)) == 9 then
		cmd.yaw = math.floor(cmd.yaw+0.5)
		cmd.roll = 0

		if Vars.Misc.fast_ladder_settings[1]:get(mUXsKykTgB('Woyajzejc', 22)) then
			if cmd.forwardmove > 0 then
				if pitch < 45 then
					cmd.pitch = 89
					cmd.in_moveright = 1
					cmd.in_moveleft = 0
					cmd.in_forward = 0
					cmd.in_back = 1
					if cmd.sidemove == 0 then
						cmd.yaw = cmd.yaw + 90
					end
					if cmd.sidemove < 0 then
						cmd.yaw = cmd.yaw + 150
					end
					if cmd.sidemove > 0 then
						cmd.yaw = cmd.yaw + 30
					end
				end 
			end
		end

		if Vars.Misc.fast_ladder_settings[1]:get(mUXsKykTgB('Yznxziydib', 21)) then
			if cmd.forwardmove < 0 then
				cmd.pitch = 89
				cmd.in_moveleft = 1
				cmd.in_moveright = 0
				cmd.in_forward = 1
				cmd.in_back = 0
				if cmd.sidemove == 0 then
					cmd.yaw = cmd.yaw + 90
				end
				if cmd.sidemove > 0 then
					cmd.yaw = cmd.yaw + 150
				end
				if cmd.sidemove < 0 then
					cmd.yaw = cmd.yaw + 30
				end
			end
		end
	end
end

aa_package = ui_handler.setup(conditional_antiaims.conditions)
aa_config = aa_package:save()
package = ui_handler.setup(Vars)
config = package:save()

ui_handler.traverse(Vars, function (element, path)
    element:depend({tab, path[1]})
end)

ui_handler.traverse(conditional_antiaims.conditions, function (element, path)
    element:depend({tab, mUXsKykTgB('CC', 2)})
	experimental_defensive111:depend({tab, mUXsKykTgB('QQ', 16)})
	experimental_defensive:depend({tab, mUXsKykTgB('TT', 19)})
end)

for k, v in pairs(conditional_antiaims.conditions_names) do
	if v ~= mUXsKykTgB('Vkduhg', 3) then
		conditional_antiaims.conditions[k].switch:depend({Vars.AA.enable, true}, {Vars.AA.Settings.condition_combo, v})
	end

	for k2, v2 in pairs(conditional_antiaims.conditions[k]) do
		if k2 ~= mUXsKykTgB("txjudi", 1) then
			v2:depend({Vars.AA.enable, true}, {Vars.AA.Settings.condition_combo, v})
		end
	end
	conditional_antiaims.conditions[k].yaw_modifier_offset:depend({conditional_antiaims.conditions[k].yaw_modifier, mUXsKykTgB('Nee', 25), true}, {conditional_antiaims.conditions[k].yaw_modifier, mUXsKykTgB('N&T Egpvgt', 2), true}, {conditional_antiaims.conditions[k].yaw_modifier, mUXsKykTgB('Phfgbz jnl', 13), true})
	conditional_antiaims.conditions[k].yaw_modifier_offset979:depend({conditional_antiaims.conditions[k].yaw_modifier, mUXsKykTgB('N&T Egpvgt', 2)})
	conditional_antiaims.conditions[k].yaw_modifier_offset1337:depend({conditional_antiaims.conditions[k].yaw_modifier, mUXsKykTgB('Q&W Hjsyjw', 5)})
	conditional_antiaims.conditions[k].yaw_modifier_offset1:depend({conditional_antiaims.conditions[k].yaw_modifier, mUXsKykTgB('Iayzus cge', 6)})
	conditional_antiaims.conditions[k].yaw_modifier_offset2:depend({conditional_antiaims.conditions[k].yaw_modifier, mUXsKykTgB('Nfdezx hlj', 11)})
	conditional_antiaims.conditions[k].yaw_modifier_offset3:depend({conditional_antiaims.conditions[k].yaw_modifier, mUXsKykTgB('Iayzus cge', 6)})
	conditional_antiaims.conditions[k].body_yaw_offset:depend({conditional_antiaims.conditions[k].body_yaw, mUXsKykTgB('Xoo', 9), true}, {conditional_antiaims.conditions[k].body_yaw, mUXsKykTgB('Rumretvu', 17), true})
	conditional_antiaims.conditions[k].delay_ticks:depend({conditional_antiaims.conditions[k].body_yaw, mUXsKykTgB('Gjbgtikj', 6)})
	if v ~= mUXsKykTgB('Nismtio', 8) then
		conditional_antiaims.conditions[k].defensive_pitch:depend({conditional_antiaims.conditions[k].defensive_aa, true})
		conditional_antiaims.conditions[k].pitch_slider:depend({conditional_antiaims.conditions[k].defensive_aa, true}, {conditional_antiaims.conditions[k].defensive_pitch, mUXsKykTgB('Rjhidb', 15)})
		conditional_antiaims.conditions[k].defensive_yaw:depend({conditional_antiaims.conditions[k].defensive_aa, true})
		conditional_antiaims.conditions[k].yaw_slider:depend({conditional_antiaims.conditions[k].defensive_aa, true}, {conditional_antiaims.conditions[k].defensive_yaw, mUXsKykTgB('Umklge', 18)})
	end
end

for k, v in pairs(Vars.Misc.screen_indicators_settings) do
    v:depend({Vars.Misc.screen_indicators, true})
end

for k, v in pairs(Vars.Misc.anim_breakers_settings) do
    v:depend({Vars.Misc.anim_breakers, true})
end

for k, v in pairs(Vars.Misc.fast_ladder_settings) do
    v:depend({Vars.Misc.fast_ladder, true})
end

for k, v in pairs(Vars.Misc.clantag_settings) do
    v:depend({Vars.Misc.clantag_changer, true})
end

for k, v in pairs(Vars.Misc.manual_arrows_settings) do
    Vars.Misc.manual_arrows_settings.settings:depend({Vars.Misc.manual_arrows, true})
	Vars.Misc.manual_arrows_settings.adding:depend({Vars.Misc.manual_arrows, true}, {Vars.Misc.manual_arrows_settings.settings, mUXsKykTgB('Bcdysjr', 24)})
	Vars.Misc.manual_arrows_settings.accent_color:depend({Vars.Misc.manual_arrows, true}, {Vars.Misc.manual_arrows_settings.settings, mUXsKykTgB('Lmnictb', 8)})
	
	Vars.Misc.manual_arrows_settings.teamskeet_adding:depend({Vars.Misc.manual_arrows, true}, {Vars.Misc.manual_arrows_settings.settings, mUXsKykTgB('Mxtfldxxm', 19)})
	Vars.Misc.manual_arrows_settings.teamskeet_accent_color:depend({Vars.Misc.manual_arrows, true}, {Vars.Misc.manual_arrows_settings.settings, mUXsKykTgB('Rcykqiccr', 24)})
	Vars.Misc.manual_arrows_settings.teamskeet_desync_accent_color:depend({Vars.Misc.manual_arrows, true}, {Vars.Misc.manual_arrows_settings.settings, mUXsKykTgB('Hsoagyssh', 14)})

	Vars.Misc.manual_arrows_settings.invictus_dynamic:depend({Vars.Misc.manual_arrows, true}, {Vars.Misc.manual_arrows_settings.settings, mUXsKykTgB('Ydlysjki', 16)})
	Vars.Misc.manual_arrows_settings.getzeus_adding:depend({Vars.Misc.manual_arrows, true}, {Vars.Misc.manual_arrows_settings.settings, mUXsKykTgB('Zemztklj', 17)})
	Vars.Misc.manual_arrows_settings.getzeus_accent_color:depend({Vars.Misc.manual_arrows, true}, {Vars.Misc.manual_arrows_settings.settings, mUXsKykTgB('Nsanhyzx', 5)})
	Vars.Misc.manual_arrows_settings.getzeus_second_accent_color:depend({Vars.Misc.manual_arrows, true}, {Vars.Misc.manual_arrows_settings.settings, mUXsKykTgB('Gltgarsq', 24)})
end

Vars.Misc.alt_watermark:depend({Vars.Misc.branded_watermark, false})
Vars.Misc.alt_watermark_settings.pos:depend({Vars.Misc.branded_watermark, false}, {Vars.Misc.alt_watermark, mUXsKykTgB('Tvklyu', 7)})
Vars.Misc.alt_watermark_settings.color:depend({Vars.Misc.branded_watermark, false}, {Vars.Misc.alt_watermark, mUXsKykTgB('Zbqrea', 13)})
Vars.Misc.alt_watermark_settings.nosp:depend({Vars.Misc.branded_watermark, false}, {Vars.Misc.alt_watermark, mUXsKykTgB('Yapqdz', 12)})
Vars.Misc.watermark_settings.color:depend({Vars.Misc.watermark, true})


if Vars.Misc.crosshair_settings then
	for k, v in pairs(Vars.Misc.crosshair_settings) do
		v:depend({Vars.Misc.crosshair_hitlog, true})
	end
end

if Vars.Misc.hitlog_console then
	for k, v in pairs(Vars.Misc.hitlogger_settings) do
		v:depend({Vars.Misc.hitlog_console, true})
	end
end

for k, v in pairs(Vars.AA.manuals) do
    Vars.AA.manuals.left:depend({Vars.AA.manuals.enable, true})
	Vars.AA.manuals.right:depend({Vars.AA.manuals.enable, true})
	Vars.AA.manuals.reset:depend({Vars.AA.manuals.enable, true})
	Vars.AA.manuals.inverter:depend({Vars.AA.manuals.enable, true})
	Vars.AA.manuals.manuals_over_fs:depend({Vars.AA.manuals.enable, true})
	Vars.AA.manuals.lag_options:depend({Vars.AA.manuals.enable, true})
	Vars.AA.manuals.defensive_aa:depend({Vars.AA.manuals.enable, true})
	Vars.AA.manuals.defensive_pitch:depend({Vars.AA.manuals.enable, true}, {Vars.AA.manuals.defensive_aa, true})
	Vars.AA.manuals.pitch_slider:depend({Vars.AA.manuals.enable, true}, {Vars.AA.manuals.defensive_aa, true}, {Vars.AA.manuals.defensive_pitch, mUXsKykTgB('Tljkfd', 17)})
	Vars.AA.manuals.defensive_yaw:depend({Vars.AA.manuals.enable, true}, {Vars.AA.manuals.defensive_aa, true})
	Vars.AA.manuals.yaw_slider:depend({Vars.AA.manuals.enable, true}, {Vars.AA.manuals.defensive_aa, true}, {Vars.AA.manuals.defensive_yaw, mUXsKykTgB('Rjhidb', 15)})
end

for k, v in pairs(Vars.AA.safe_functions) do
	Vars.AA.safe_functions.settings:depend({Vars.AA.safe_functions.enable, true})
	Vars.AA.safe_functions.head_settings:depend({Vars.AA.safe_functions.enable, true}, {Vars.AA.safe_functions.settings, mUXsKykTgB('Byux', 20)})
	Vars.AA.safe_functions.lag_options:depend({Vars.AA.safe_functions.enable, true})
	Vars.AA.safe_functions.defensive_aa:depend({Vars.AA.safe_functions.enable, true})
	Vars.AA.safe_functions.defensive_pitch:depend({Vars.AA.safe_functions.enable, true}, {Vars.AA.safe_functions.defensive_aa, true})
	Vars.AA.safe_functions.pitch_slider:depend({Vars.AA.safe_functions.enable, true}, {Vars.AA.safe_functions.defensive_aa, true}, {Vars.AA.safe_functions.defensive_pitch, mUXsKykTgB('Zrpqlj', 23)})
	Vars.AA.safe_functions.defensive_yaw:depend({Vars.AA.safe_functions.enable, true}, {Vars.AA.safe_functions.defensive_aa, true})
	Vars.AA.safe_functions.yaw_slider:depend({Vars.AA.safe_functions.enable, true}, {Vars.AA.safe_functions.defensive_aa, true}, {Vars.AA.safe_functions.defensive_yaw, mUXsKykTgB('Btrsnl', 25)})
end

if Vars.AA.air_exploit then
	for k, v in pairs(Vars.AA.air_exploit) do
		Vars.AA.air_exploit.while_visible:depend({Vars.AA.air_exploit.enable, true})
		Vars.AA.air_exploit.exp_tick:depend({Vars.AA.air_exploit.enable, true})
	end
end

client.set_event_callback(mUXsKykTgB('ixkjtemd', 16), function()
	utils.hide_aa_tab(false)
end)

client.set_event_callback(mUXsKykTgB('dfs_fsbrsf', 14), function()
	utils.hide_aa_tab(true)
end)
client.set_event_callback(mUXsKykTgB('rdsto_bnllzmc', 25), aero_lag_exp.handle)
client.set_event_callback(mUXsKykTgB('epxci', 15), crosshair_logger.handle)
client.set_event_callback(mUXsKykTgB('kvdio', 21), screen_indication.handle)
client.set_event_callback(mUXsKykTgB('altye', 11), watermark.handle)
client.set_event_callback(mUXsKykTgB('zksxd', 10), manual_indication.handle)
client.set_event_callback(mUXsKykTgB('altye', 11), visual_effects.handle)


client.set_event_callback(mUXsKykTgB('iujkf_seccqdt', 16), manual_indication.peeking_whom)
client.set_event_callback(mUXsKykTgB('dfs_fsbrsf', 14), model_breaker.handle)
client.set_event_callback(mUXsKykTgB('grzek', 17), widgets.branded_watermark.handle)
client.set_event_callback(mUXsKykTgB('kgvtzm_cpmo', 21), crosshair_logger.player_hurt)
client.set_event_callback(mUXsKykTgB('muy_rudq', 12), crosshair_logger.aim_fire)
client.set_event_callback(mUXsKykTgB('owa_awgg', 14), crosshair_logger.aim_miss)
client.set_event_callback(mUXsKykTgB('ambcx_kwuuivl', 8), function(cmd)
	model_breaker.handle_jitter(cmd)
end)
client.set_event_callback(mUXsKykTgB('bsh_idrohs_sbr', 14), conditional_antiaims.defensive_handle)
client.set_event_callback(mUXsKykTgB('ula_bwkhal_luk', 7), conditional_antiaims.defensive_handle1)
client.set_event_callback(mUXsKykTgB('zqf_gbpmfq_qzp', 12), expres.handle)
client.set_event_callback(mUXsKykTgB('uqfdjw_ijfym', 5), death_spammer.handle)
client.set_event_callback(mUXsKykTgB('kgvtzm_yzvoc', 21), function(e)
	chat_spammer.handle(e)
end)
client.set_event_callback(mUXsKykTgB('zwcvl_abizb', 8), function()
	conditional_antiaims.manual_dir = 0
	conditional_antiaims.last_press = globals.curtime()
end)
client.set_event_callback(mUXsKykTgB('tfuvq_dpnnboe', 1), function(cmd)
	fast_ladder.handle(cmd)
	conditional_antiaims.handle(cmd)
	antiaim_on_use.handle(cmd)
end)

local clantag_changer = {}

clantag_changer.presets = {
	evilclub = {
		mUXsKykTgB("tkxarajq", 15),
		mUXsKykTgB("mdqtktc", 8),
		mUXsKykTgB("ofsvmv", 10),
		mUXsKykTgB("mdqtk", 8),
		mUXsKykTgB("hylo", 3),
		mUXsKykTgB("mdq", 8),
		mUXsKykTgB("wn", 18),
		mUXsKykTgB("b", 23),
		mUXsKykTgB("", 1),
		mUXsKykTgB("q", 12),
		mUXsKykTgB("ul", 16),
		mUXsKykTgB("ofs", 10),
		mUXsKykTgB("xobe", 19),
		mUXsKykTgB("ulybs", 16),
		mUXsKykTgB("xobeve", 19),
		mUXsKykTgB("ypcfwfo", 20)
	},
	neverlose = {
		mUXsKykTgB(" | ", 4),
		mUXsKykTgB(" |\\ ", 5),
		mUXsKykTgB(" |\\| ", 2),
		mUXsKykTgB(" F ", 18),
		mUXsKykTgB(" C8 ", 15),
		mUXsKykTgB(" Ev ", 17),
		mUXsKykTgB(" Ri\\ ", 4),
	    mUXsKykTgB(" Tk\\/ ", 6),
		mUXsKykTgB(" Hyp ", 20),
		mUXsKykTgB(" Jar5 ", 22),
		mUXsKykTgB(" Ctkt ", 15),
		mUXsKykTgB(" Gxox| ", 19),
		mUXsKykTgB(" Sjaj|7 ", 5),
		mUXsKykTgB(" Kbsbo| ", 23),
		mUXsKykTgB(" Sjajw|_ ", 5),
		mUXsKykTgB(" Xofobv ", 10),
		mUXsKykTgB(" Zqhqdx2 ", 12),
		mUXsKykTgB(" Arireyb ", 13),
		mUXsKykTgB(" Arireyb8 ", 13),
		mUXsKykTgB(" Ulclysvz ", 7),
		mUXsKykTgB(" Sjajwqtx8 ", 5),
		mUXsKykTgB(" Hypylfimy ", 20),
		mUXsKykTgB(" Jaranhkoa. ", 22),
		mUXsKykTgB(" Sjajwqtxj.< ", 5),
		mUXsKykTgB(" Qhyhuorvh.f< ", 3),
		mUXsKykTgB(" Hypylfimy.ww ", 20),
		mUXsKykTgB(" Hypylfimy.ww ", 20),
		mUXsKykTgB(" Jaranhkoa.yy ", 22),
		mUXsKykTgB(" Pgxgtnqug.ee ", 2),
		mUXsKykTgB(" Bsjsfzcgs.qq ", 14),
		mUXsKykTgB(" Ypgpcwzdp.n< ", 11),
		mUXsKykTgB(" Wnenauxbn.< ", 9),
		mUXsKykTgB(" Fwnwjdgkw. ", 18),
		mUXsKykTgB(" Bsjsfzcgs ", 14),
		mUXsKykTgB(" Pgxgtnqu5 ", 2),
		mUXsKykTgB(" Vmdmztwa ", 8),
		mUXsKykTgB(" Fwnwjdg3 ", 18),
		mUXsKykTgB(" Ctktgad ", 15),
		mUXsKykTgB(" Pgxgtn2 ", 2),
		mUXsKykTgB(" Tkbkxr ", 6),
		mUXsKykTgB(" Gxoxk|_ ", 19),
		mUXsKykTgB(" Jaran| ", 22),
		mUXsKykTgB(" Bsjsf ", 14),
		mUXsKykTgB(" Zqhq|4 ", 12),
		mUXsKykTgB(" Zqhq| ", 12),
		mUXsKykTgB(" Jara ", 22),
		mUXsKykTgB(" Lct7 ", 24),
		mUXsKykTgB(" Hyp ", 20),
		mUXsKykTgB(" Pg\\/ ", 2),
		mUXsKykTgB(" Ja\\ ", 22),
		mUXsKykTgB(" Pg ", 2),
		mUXsKykTgB(" X3 ", 10),
		mUXsKykTgB(" U ", 7),
		mUXsKykTgB(" |\\| ", 9),
		mUXsKykTgB(" |\\ ", 4),
		mUXsKykTgB(" | ", 3)
		},
	jre_codes = {
		mUXsKykTgB("ygt.rdsth", 15),
		mUXsKykTgB("ucp.nzop", 11),
		mUXsKykTgB("tbo.myn", 10),
		mUXsKykTgB("muh.fr", 3),
		mUXsKykTgB("ucp.n", 11),
		mUXsKykTgB("ygt.", 15),
		mUXsKykTgB("ygt", 15),
		mUXsKykTgB("sa", 9),
		mUXsKykTgB("p", 6),
		mUXsKykTgB("", 9),
		mUXsKykTgB("x", 14),
		mUXsKykTgB("hp", 24),
		mUXsKykTgB("iqd", 25),
		mUXsKykTgB("tbo.", 10),
		mUXsKykTgB("qyl.j", 7),
		mUXsKykTgB("xfs.qc", 14),
		mUXsKykTgB("rzm.kwl", 8),
		mUXsKykTgB("tbo.myno", 10)
	}
}

clantag_changer.current_index = 1
clantag_changer.last_update = 0
clantag_changer.server_time_sync = 0

clantag_changer.get_server_time = function()
	local server_time = globals.realtime()
	local latency = client.latency()
	server_time = server_time - latency
	return server_time
end

clantag_changer.get_current_tag = function()
	local preset = Vars.Misc.clantag_settings.preset:get()
	
	if not Vars.Misc.clantag_changer:get() then
		return mUXsKykTgB("", 8)
	end
	
	local anim_speed = 2 
	if preset == mUXsKykTgB("vmdmztwam", 8) then
		anim_speed = 5
	elseif preset == mUXsKykTgB("rivypyho", 13) or preset == mUXsKykTgB("bjw.ugvwk", 18) then
		anim_speed = 2
	end
	
	local preset_data = clantag_changer.presets[preset:gsub(mUXsKykTgB("%.", 1), mUXsKykTgB("_", 2))]
	if not preset_data then
		return mUXsKykTgB("", 3)
	end
	
	local current_time = clantag_changer.get_server_time()
	local update_interval = 1.0 / anim_speed
	
	local time_based_index = math.floor(current_time / update_interval) % #preset_data + 1
	clantag_changer.current_index = time_based_index
	
	local tag = preset_data[clantag_changer.current_index]
	
	return tag
end

clantag_changer.handle = function()
	local current_tag = clantag_changer.get_current_tag()
	
	if current_tag ~= mUXsKykTgB("", 6) then
		client.set_clan_tag(current_tag)
	else
		client.set_clan_tag(mUXsKykTgB("", 5))
	end
end

client.set_event_callback(mUXsKykTgB('juchn', 20), clantag_changer.handle)

Vars.Misc.clantag_changer:set_callback(function(self)
	if not self.value then
		client.set_clan_tag(mUXsKykTgB("", 5))
	end
end)

local lethal_force_baim = {}

lethal_force_baim.baim_hitboxes = {3,4,5,6}
lethal_force_baim.last_update = 0
lethal_force_baim.update_interval = 0.1 

lethal_force_baim.extrapolate_position = function(xpos, ypos, zpos, ticks, player)
	if player == nil then return xpos, ypos, zpos end
	
	local x, y, z = entity.get_prop(player, mUXsKykTgB("u_dmkDmtwkqbg", 8))
	if x == nil or y == nil or z == nil then return xpos, ypos, zpos end
	
	for i = 0, ticks do
		xpos = xpos + (x * globals.tickinterval())
		ypos = ypos + (y * globals.tickinterval())
		zpos = zpos + (z * globals.tickinterval())
	end
	return xpos, ypos, zpos
end

lethal_force_baim.is_baimable = function(ent, localplayer)
	if ent == nil or localplayer == nil then return 0 end
	
	local final_damage = 0

	local eyepos_x, eyepos_y, eyepos_z = client.eye_position()
	if eyepos_x == nil then return 0 end
	
	local fs_stored_eyepos_x, fs_stored_eyepos_y, fs_stored_eyepos_z

	eyepos_x, eyepos_y, eyepos_z = lethal_force_baim.extrapolate_position(eyepos_x, eyepos_y, eyepos_z, 20, localplayer)

	fs_stored_eyepos_x, fs_stored_eyepos_y, fs_stored_eyepos_z = eyepos_x, eyepos_y, eyepos_z
	for k, v in pairs(lethal_force_baim.baim_hitboxes) do
		local hitbox_pos = entity.hitbox_position(ent, v)
		if hitbox_pos == nil then goto continue end
		
		local hitbox = vector(hitbox_pos)
		local ___, dmg = client.trace_bullet(localplayer, fs_stored_eyepos_x, fs_stored_eyepos_y, fs_stored_eyepos_z, hitbox.x, hitbox.y, hitbox.z, true)
	
		if (dmg and dmg > final_damage) then
			final_damage = dmg
		end
		
		::continue::
	end
	
	return final_damage
end

lethal_force_baim.on_run_command = function()
	local success, err = pcall(function()
		local current_time = globals.realtime()
		if current_time - lethal_force_baim.last_update < lethal_force_baim.update_interval then
			return
		end
		lethal_force_baim.last_update = current_time
		
		local me = entity.get_local_player()
		if me == nil then return end
		
		local weapon = entity.get_player_weapon(me)
		if weapon == nil then return end

		local players = entity.get_players()
		if players == nil then return end

		for i=1, #players do
			local player = players[i]
			if player == nil then goto continue end
			
			local target_health = entity.get_prop(player, mUXsKykTgB("h_dCzvgoc", 21)) 
			if target_health == nil or target_health <= 0 then goto continue end
			
			local is_lethal = lethal_force_baim.is_baimable(player, me) >= target_health

			if (is_lethal) then 
				plist.set(player, mUXsKykTgB("Wdmzzqlm xzmnmz jwlg iqu", 8), mUXsKykTgB("Nwzkm", 8))
			else 
				plist.set(player, mUXsKykTgB("Zgpcctop acpqpc mzoj ltx", 11), mUXsKykTgB("-", 1))
			end
			
			::continue::
		end
	end)
	
	if not success then
		print(mUXsKykTgB("Uncqju oxaln kjrv naaxa: ", 9) .. tostring(err))
	end
end

client.register_esp_flag(mUXsKykTgB("FEMQ", 4), 255, 0, 0, function(player)
	local success, result = pcall(function()
		if not Vars.Other.lethal_force_baim:get() then return false end
		if player == nil then return false end

		return plist.get(player, mUXsKykTgB("Tajwwnij uwjkjw gtid fnr", 5)) == mUXsKykTgB("Gpsdf", 1)
	end)
	
	if not success then
		print(mUXsKykTgB("Ohwkdo irufh edlp HVS huuru: ", 3) .. tostring(result))
		return false
	end
	
	return result
end)

Vars.Other.lethal_force_baim:set_callback(function()
	local success, err = pcall(function()
		local enabled = Vars.Other.lethal_force_baim:get()
		local update_callback = enabled and client.set_event_callback or client.unset_event_callback

		update_callback(mUXsKykTgB("nqj_ykiiwjz", 22), lethal_force_baim.on_run_command)
		
		if not enabled then
			local players = entity.get_players()
			if players then
				for i=1, #players do
					local player = players[i]
					if player then
						plist.set(player, mUXsKykTgB("Lsboofab mobcbo ylav xfj", 23), mUXsKykTgB("-", 2))
					end
				end
			end
		end
	end)
	
	if not success then
		print(mUXsKykTgB("Wpeslw qzcnp mltx nlwwmlnv pcczc: ", 11) .. tostring(err))
	end
end)

local buybot = {}

buybot.translate_menu = function(item)
	if item == mUXsKykTgB("Nhgb", 13) then
		return mUXsKykTgB("tdbs31", 1)
	elseif item == mUXsKykTgB("Xhtzy", 5) then
		return mUXsKykTgB("llz97", 19)
	elseif item == mUXsKykTgB("Bxq", 1) then
		return mUXsKykTgB("bxq", 1)
	elseif item == mUXsKykTgB("Rtkocta tkhng", 2) then
		return mUXsKykTgB("ue47", 20)
	elseif item == mUXsKykTgB("Zjvwlk ypmsl", 7) then
		return mUXsKykTgB("hv001", 15)
	elseif item == mUXsKykTgB("Aoqvwbs uib", 14) then
		return mUXsKykTgB("kbdbs", 23)
	elseif item == mUXsKykTgB("Tuvqkbj fyijeb", 16) then
		return mUXsKykTgB("mruiq", 6)
	elseif item == mUXsKykTgB("J250", 20) then
		return mUXsKykTgB("l472", 22)
	elseif item == mUXsKykTgB("Qhny Orerggnf", 13) then
		return mUXsKykTgB("ipmxi", 4)
	elseif item == mUXsKykTgB("Fcabn jcmnif", 20) then
		return mUXsKykTgB("xig3", 4)
	elseif item == mUXsKykTgB("Fcytw ngqrmj", 24) then
		return mUXsKykTgB("klhnsl", 7)
	elseif item == mUXsKykTgB("Ozmvilm", 8) then
		return mUXsKykTgB("sprcpylop", 11)
	elseif item == mUXsKykTgB("Gacys", 14) then
		return mUXsKykTgB("bvxtnpanwjmn", 9)
	elseif item == mUXsKykTgB("Bgvxgwbtkr", 19) then
		return mUXsKykTgB("xzwzezg", 11)
	elseif item == mUXsKykTgB("Oujbqkjwp", 9) then
		return mUXsKykTgB("iodvkedqj", 3)
	elseif item == mUXsKykTgB("Qkbrgx + Nkrskz", 6) then
		return mUXsKykTgB("gpdespwx", 11)
	elseif item == mUXsKykTgB("Abcrpb hfq", 23) then
		return mUXsKykTgB("nopecob", 10)
	elseif item == mUXsKykTgB("Lqge", 12) then
		return mUXsKykTgB("Fmeqd", 12)
	else
		client.log(mUXsKykTgB("Mfcfgof alwe: ", 18) .. item)
	end
end

buybot.buy_custom = function()
	local primary_weapon = Vars.Other.buybot_primary:get()
	local secondary_weapon = Vars.Other.buybot_secondary:get()
	local utility = Vars.Other.buybot_utility:get()
	
	local current_buy = nil
	local me = entity.get_local_player()
	local weapon = entity.get_player_weapon(me)

	if primary_weapon == mUXsKykTgB("Lfez", 11) and entity.get_classname(weapon) == mUXsKykTgB("VPxtihgLVTK19", 19) then
		current_buy = mUXsKykTgB("pim ", 14) .. buybot.translate_menu(secondary_weapon)
	else
		current_buy = mUXsKykTgB("xqu ", 22) .. buybot.translate_menu(primary_weapon) .. mUXsKykTgB("; ", 9) .. mUXsKykTgB("rko ", 16) .. buybot.translate_menu(secondary_weapon)
	end

	if #utility == 0 then
		client.exec(current_buy)
	else
		for i = 1, #utility do
			current_buy = current_buy .. mUXsKykTgB("; wpt ", 21) .. buybot.translate_menu(utility[i])
		end
		client.exec(current_buy)
	end
end

buybot.on_round_end_upload_stats = function()
	if not Vars.Other.buybot:get() then
		return
	end
	buybot.buy_custom()
end

client.set_event_callback(mUXsKykTgB("ebhaq_raq_hcybnq_fgngf", 13), buybot.on_round_end_upload_stats)

Vars.Other.buybot:set_callback(function()
	local enabled = Vars.Other.buybot:get()
	Vars.Other.buybot_primary:set_visible(enabled)
	Vars.Other.buybot_secondary:set_visible(enabled)
	Vars.Other.buybot_utility:set_visible(enabled)
end)

Vars.Other.buybot_primary:set_visible(Vars.Other.buybot:get())
Vars.Other.buybot_secondary:set_visible(Vars.Other.buybot:get())
Vars.Other.buybot_utility:set_visible(Vars.Other.buybot:get())

local hidden_cvars = {}

local function init_hidden_cvars()
    local v_engine_cvar = client.create_interface(mUXsKykTgB('xuvfnkd.fnn', 2), mUXsKykTgB('MVexzevTmri774', 17))
    if not v_engine_cvar then
        print(mUXsKykTgB("Gbjmfe up dsfbuf WFohjofDwbs118 joufsgbdf", 1))
        return
    end

    local con_command_base = ffi.cast(mUXsKykTgB('a_aml_amkkylb_zyqc ***', 24), ffi.cast(mUXsKykTgB('mafl10_l', 18), v_engine_cvar) + 0x34)[0][0]
    local cmd = ffi.cast(mUXsKykTgB('e_eqp_eqoocpf_dcug *', 2), con_command_base.next)

    while ffi.cast(mUXsKykTgB('wkpv54_v', 2), cmd) ~= 0 do
        if bit.band(cmd.flags, 18) then
            table.insert(hidden_cvars, cmd)
        end
        cmd = ffi.cast(mUXsKykTgB('a_aml_amkkylb_zyqc *', 24), cmd.next)
    end
end

function show_hidden_cvars(show)
    for i = 1, #hidden_cvars do
        hidden_cvars[i].flags = show and bit.band(hidden_cvars[i].flags, -19) or bit.bor(hidden_cvars[i].flags, 18)
		client.exec(mUXsKykTgB("dnq_kyv 4", 24))
		client.exec(mUXsKykTgB("f_rmboawq 4", 14))
		client.exec(mUXsKykTgB("d_pdmifdmoqde_rudefbqdeaz 2", 12))
		client.exec(mUXsKykTgB("cqj_tyiqrbu_vqdso_rbudtydw 7", 16))
		client.exec(mUXsKykTgB("tha_oky_slcls 7", 7))
		client.exec(mUXsKykTgB("f_gvorckg 4", 14))
		client.exec(mUXsKykTgB("w_xmfitbrfcwjsijwji 5", 5))
		client.exec(mUXsKykTgB("f_7rgym 4", 14))
		client.exec(mUXsKykTgB("h_uouwbeii 6", 16))
		client.exec(mUXsKykTgB("z_mgmuwdm 8", 8))
		client.exec(mUXsKykTgB("e_ebbgybq 5", 13))
		client.exec(mUXsKykTgB("y_yluklyvclyshfmyhntlua 7", 7))
		client.exec(mUXsKykTgB("n_znwszapwehlnklo 2", 22))
		client.exec(mUXsKykTgB("d_pdmirxqowe 2", 12))
		client.exec(mUXsKykTgB("xg_ncjrczgk 1", 21))
		client.exec(mUXsKykTgB("ud_kzgo_khdskzwk 8", 18))
		client.exec(mUXsKykTgB("mv_nodksv_kfysn_pybmo 0", 10))
		client.exec(mUXsKykTgB("zng_erqhprsvyyengr 4", 13))
		client.exec(mUXsKykTgB("u_gudzprghovwdwvryhuodb 3", 3))
		client.exec(mUXsKykTgB("x_cgzkxluxikkdvktyobk 6", 6))
		client.exec(mUXsKykTgB("a_urpqcjenajpn 9", 9))
		client.exec(mUXsKykTgB("e_yvtugvagrec 3", 13))
		client.exec(mUXsKykTgB("ftm_unfifti 9", 19))
		client.exec(mUXsKykTgB("hvo_nkzxpgvm 1", 21))
		client.exec(mUXsKykTgB("sgz_iusvxkyykjzkdzaxky 7", 6))
		client.exec(mUXsKykTgB("dmfbs", 1))
		print(mUXsKykTgB("mfkbc exvymuon!", 10))	
    end
end

client.delay_call(0, init_hidden_cvars)

 end)(...)
