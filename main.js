
(function() {
'use strict';

function F2(fun)
{
  function wrapper(a) { return function(b) { return fun(a,b); }; }
  wrapper.arity = 2;
  wrapper.func = fun;
  return wrapper;
}

function F3(fun)
{
  function wrapper(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  }
  wrapper.arity = 3;
  wrapper.func = fun;
  return wrapper;
}

function F4(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  }
  wrapper.arity = 4;
  wrapper.func = fun;
  return wrapper;
}

function F5(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  }
  wrapper.arity = 5;
  wrapper.func = fun;
  return wrapper;
}

function F6(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  }
  wrapper.arity = 6;
  wrapper.func = fun;
  return wrapper;
}

function F7(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  }
  wrapper.arity = 7;
  wrapper.func = fun;
  return wrapper;
}

function F8(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  }
  wrapper.arity = 8;
  wrapper.func = fun;
  return wrapper;
}

function F9(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  }
  wrapper.arity = 9;
  wrapper.func = fun;
  return wrapper;
}

function A2(fun, a, b)
{
  return fun.arity === 2
    ? fun.func(a, b)
    : fun(a)(b);
}
function A3(fun, a, b, c)
{
  return fun.arity === 3
    ? fun.func(a, b, c)
    : fun(a)(b)(c);
}
function A4(fun, a, b, c, d)
{
  return fun.arity === 4
    ? fun.func(a, b, c, d)
    : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e)
{
  return fun.arity === 5
    ? fun.func(a, b, c, d, e)
    : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f)
{
  return fun.arity === 6
    ? fun.func(a, b, c, d, e, f)
    : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g)
{
  return fun.arity === 7
    ? fun.func(a, b, c, d, e, f, g)
    : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h)
{
  return fun.arity === 8
    ? fun.func(a, b, c, d, e, f, g, h)
    : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i)
{
  return fun.arity === 9
    ? fun.func(a, b, c, d, e, f, g, h, i)
    : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

//import Native.Utils //

var _elm_lang$core$Native_Basics = function() {

function div(a, b)
{
	return (a / b) | 0;
}
function rem(a, b)
{
	return a % b;
}
function mod(a, b)
{
	if (b === 0)
	{
		throw new Error('Cannot perform mod 0. Division by zero error.');
	}
	var r = a % b;
	var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r + b) : -mod(-a, -b));

	return m === b ? 0 : m;
}
function logBase(base, n)
{
	return Math.log(n) / Math.log(base);
}
function negate(n)
{
	return -n;
}
function abs(n)
{
	return n < 0 ? -n : n;
}

function min(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) < 0 ? a : b;
}
function max(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) > 0 ? a : b;
}
function clamp(lo, hi, n)
{
	return _elm_lang$core$Native_Utils.cmp(n, lo) < 0
		? lo
		: _elm_lang$core$Native_Utils.cmp(n, hi) > 0
			? hi
			: n;
}

var ord = ['LT', 'EQ', 'GT'];

function compare(x, y)
{
	return { ctor: ord[_elm_lang$core$Native_Utils.cmp(x, y) + 1] };
}

function xor(a, b)
{
	return a !== b;
}
function not(b)
{
	return !b;
}
function isInfinite(n)
{
	return n === Infinity || n === -Infinity;
}

function truncate(n)
{
	return n | 0;
}

function degrees(d)
{
	return d * Math.PI / 180;
}
function turns(t)
{
	return 2 * Math.PI * t;
}
function fromPolar(point)
{
	var r = point._0;
	var t = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(r * Math.cos(t), r * Math.sin(t));
}
function toPolar(point)
{
	var x = point._0;
	var y = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(Math.sqrt(x * x + y * y), Math.atan2(y, x));
}

return {
	div: F2(div),
	rem: F2(rem),
	mod: F2(mod),

	pi: Math.PI,
	e: Math.E,
	cos: Math.cos,
	sin: Math.sin,
	tan: Math.tan,
	acos: Math.acos,
	asin: Math.asin,
	atan: Math.atan,
	atan2: F2(Math.atan2),

	degrees: degrees,
	turns: turns,
	fromPolar: fromPolar,
	toPolar: toPolar,

	sqrt: Math.sqrt,
	logBase: F2(logBase),
	negate: negate,
	abs: abs,
	min: F2(min),
	max: F2(max),
	clamp: F3(clamp),
	compare: F2(compare),

	xor: F2(xor),
	not: not,

	truncate: truncate,
	ceiling: Math.ceil,
	floor: Math.floor,
	round: Math.round,
	toFloat: function(x) { return x; },
	isNaN: isNaN,
	isInfinite: isInfinite
};

}();
//import //

var _elm_lang$core$Native_Utils = function() {

// COMPARISONS

function eq(x, y)
{
	var stack = [];
	var isEqual = eqHelp(x, y, 0, stack);
	var pair;
	while (isEqual && (pair = stack.pop()))
	{
		isEqual = eqHelp(pair.x, pair.y, 0, stack);
	}
	return isEqual;
}


function eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push({ x: x, y: y });
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object')
	{
		if (typeof x === 'function')
		{
			throw new Error(
				'Trying to use `(==)` on functions. There is no way to know if functions are "the same" in the Elm sense.'
				+ ' Read more about this at http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#=='
				+ ' which describes why it is this way and what the better version will look like.'
			);
		}
		return false;
	}

	if (x === null || y === null)
	{
		return false
	}

	if (x instanceof Date)
	{
		return x.getTime() === y.getTime();
	}

	if (!('ctor' in x))
	{
		for (var key in x)
		{
			if (!eqHelp(x[key], y[key], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	// convert Dicts and Sets to lists
	if (x.ctor === 'RBNode_elm_builtin' || x.ctor === 'RBEmpty_elm_builtin')
	{
		x = _elm_lang$core$Dict$toList(x);
		y = _elm_lang$core$Dict$toList(y);
	}
	if (x.ctor === 'Set_elm_builtin')
	{
		x = _elm_lang$core$Set$toList(x);
		y = _elm_lang$core$Set$toList(y);
	}

	// check if lists are equal without recursion
	if (x.ctor === '::')
	{
		var a = x;
		var b = y;
		while (a.ctor === '::' && b.ctor === '::')
		{
			if (!eqHelp(a._0, b._0, depth + 1, stack))
			{
				return false;
			}
			a = a._1;
			b = b._1;
		}
		return a.ctor === b.ctor;
	}

	// check if Arrays are equal
	if (x.ctor === '_Array')
	{
		var xs = _elm_lang$core$Native_Array.toJSArray(x);
		var ys = _elm_lang$core$Native_Array.toJSArray(y);
		if (xs.length !== ys.length)
		{
			return false;
		}
		for (var i = 0; i < xs.length; i++)
		{
			if (!eqHelp(xs[i], ys[i], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	if (!eqHelp(x.ctor, y.ctor, depth + 1, stack))
	{
		return false;
	}

	for (var key in x)
	{
		if (!eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

var LT = -1, EQ = 0, GT = 1;

function cmp(x, y)
{
	if (typeof x !== 'object')
	{
		return x === y ? EQ : x < y ? LT : GT;
	}

	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? EQ : a < b ? LT : GT;
	}

	if (x.ctor === '::' || x.ctor === '[]')
	{
		while (x.ctor === '::' && y.ctor === '::')
		{
			var ord = cmp(x._0, y._0);
			if (ord !== EQ)
			{
				return ord;
			}
			x = x._1;
			y = y._1;
		}
		return x.ctor === y.ctor ? EQ : x.ctor === '[]' ? LT : GT;
	}

	if (x.ctor.slice(0, 6) === '_Tuple')
	{
		var ord;
		var n = x.ctor.slice(6) - 0;
		var err = 'cannot compare tuples with more than 6 elements.';
		if (n === 0) return EQ;
		if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
		if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
		if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
		if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
		if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
		if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
		if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
		return EQ;
	}

	throw new Error(
		'Comparison error: comparison is only defined on ints, '
		+ 'floats, times, chars, strings, lists of comparable values, '
		+ 'and tuples of comparable values.'
	);
}


// COMMON VALUES

var Tuple0 = {
	ctor: '_Tuple0'
};

function Tuple2(x, y)
{
	return {
		ctor: '_Tuple2',
		_0: x,
		_1: y
	};
}

function chr(c)
{
	return new String(c);
}


// GUID

var count = 0;
function guid(_)
{
	return count++;
}


// RECORDS

function update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


//// LIST STUFF ////

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return {
		ctor: '::',
		_0: hd,
		_1: tl
	};
}

function append(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (xs.ctor === '[]')
	{
		return ys;
	}
	var root = Cons(xs._0, Nil);
	var curr = root;
	xs = xs._1;
	while (xs.ctor !== '[]')
	{
		curr._1 = Cons(xs._0, Nil);
		xs = xs._1;
		curr = curr._1;
	}
	curr._1 = ys;
	return root;
}


// CRASHES

function crash(moduleName, region)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '` ' + regionToString(region) + '\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function crashCase(moduleName, region, value)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '`\n\n'
			+ 'This was caused by the `case` expression ' + regionToString(region) + '.\n'
			+ 'One of the branches ended with a crash and the following value got through:\n\n    ' + toString(value) + '\n\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function regionToString(region)
{
	if (region.start.line == region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'between lines ' + region.start.line + ' and ' + region.end.line;
}


// TO STRING

function toString(v)
{
	var type = typeof v;
	if (type === 'function')
	{
		return '<function>';
	}

	if (type === 'boolean')
	{
		return v ? 'True' : 'False';
	}

	if (type === 'number')
	{
		return v + '';
	}

	if (v instanceof String)
	{
		return '\'' + addSlashes(v, true) + '\'';
	}

	if (type === 'string')
	{
		return '"' + addSlashes(v, false) + '"';
	}

	if (v === null)
	{
		return 'null';
	}

	if (type === 'object' && 'ctor' in v)
	{
		var ctorStarter = v.ctor.substring(0, 5);

		if (ctorStarter === '_Tupl')
		{
			var output = [];
			for (var k in v)
			{
				if (k === 'ctor') continue;
				output.push(toString(v[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (ctorStarter === '_Task')
		{
			return '<task>'
		}

		if (v.ctor === '_Array')
		{
			var list = _elm_lang$core$Array$toList(v);
			return 'Array.fromList ' + toString(list);
		}

		if (v.ctor === '<decoder>')
		{
			return '<decoder>';
		}

		if (v.ctor === '_Process')
		{
			return '<process:' + v.id + '>';
		}

		if (v.ctor === '::')
		{
			var output = '[' + toString(v._0);
			v = v._1;
			while (v.ctor === '::')
			{
				output += ',' + toString(v._0);
				v = v._1;
			}
			return output + ']';
		}

		if (v.ctor === '[]')
		{
			return '[]';
		}

		if (v.ctor === 'Set_elm_builtin')
		{
			return 'Set.fromList ' + toString(_elm_lang$core$Set$toList(v));
		}

		if (v.ctor === 'RBNode_elm_builtin' || v.ctor === 'RBEmpty_elm_builtin')
		{
			return 'Dict.fromList ' + toString(_elm_lang$core$Dict$toList(v));
		}

		var output = '';
		for (var i in v)
		{
			if (i === 'ctor') continue;
			var str = toString(v[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return v.ctor + output;
	}

	if (type === 'object')
	{
		if (v instanceof Date)
		{
			return '<' + v.toString() + '>';
		}

		if (v.elm_web_socket)
		{
			return '<websocket>';
		}

		var output = [];
		for (var k in v)
		{
			output.push(k + ' = ' + toString(v[k]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return '<internal structure>';
}

function addSlashes(str, isChar)
{
	var s = str.replace(/\\/g, '\\\\')
			  .replace(/\n/g, '\\n')
			  .replace(/\t/g, '\\t')
			  .replace(/\r/g, '\\r')
			  .replace(/\v/g, '\\v')
			  .replace(/\0/g, '\\0');
	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}


return {
	eq: eq,
	cmp: cmp,
	Tuple0: Tuple0,
	Tuple2: Tuple2,
	chr: chr,
	update: update,
	guid: guid,

	append: F2(append),

	crash: crash,
	crashCase: crashCase,

	toString: toString
};

}();
var _elm_lang$core$Basics$never = function (_p0) {
	never:
	while (true) {
		var _p1 = _p0;
		var _v1 = _p1._0;
		_p0 = _v1;
		continue never;
	}
};
var _elm_lang$core$Basics$uncurry = F2(
	function (f, _p2) {
		var _p3 = _p2;
		return A2(f, _p3._0, _p3._1);
	});
var _elm_lang$core$Basics$curry = F3(
	function (f, a, b) {
		return f(
			{ctor: '_Tuple2', _0: a, _1: b});
	});
var _elm_lang$core$Basics$flip = F3(
	function (f, b, a) {
		return A2(f, a, b);
	});
var _elm_lang$core$Basics$always = F2(
	function (a, _p4) {
		return a;
	});
var _elm_lang$core$Basics$identity = function (x) {
	return x;
};
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<|'] = F2(
	function (f, x) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['|>'] = F2(
	function (x, f) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>>'] = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<<'] = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['++'] = _elm_lang$core$Native_Utils.append;
var _elm_lang$core$Basics$toString = _elm_lang$core$Native_Utils.toString;
var _elm_lang$core$Basics$isInfinite = _elm_lang$core$Native_Basics.isInfinite;
var _elm_lang$core$Basics$isNaN = _elm_lang$core$Native_Basics.isNaN;
var _elm_lang$core$Basics$toFloat = _elm_lang$core$Native_Basics.toFloat;
var _elm_lang$core$Basics$ceiling = _elm_lang$core$Native_Basics.ceiling;
var _elm_lang$core$Basics$floor = _elm_lang$core$Native_Basics.floor;
var _elm_lang$core$Basics$truncate = _elm_lang$core$Native_Basics.truncate;
var _elm_lang$core$Basics$round = _elm_lang$core$Native_Basics.round;
var _elm_lang$core$Basics$not = _elm_lang$core$Native_Basics.not;
var _elm_lang$core$Basics$xor = _elm_lang$core$Native_Basics.xor;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['||'] = _elm_lang$core$Native_Basics.or;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['&&'] = _elm_lang$core$Native_Basics.and;
var _elm_lang$core$Basics$max = _elm_lang$core$Native_Basics.max;
var _elm_lang$core$Basics$min = _elm_lang$core$Native_Basics.min;
var _elm_lang$core$Basics$compare = _elm_lang$core$Native_Basics.compare;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>='] = _elm_lang$core$Native_Basics.ge;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<='] = _elm_lang$core$Native_Basics.le;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>'] = _elm_lang$core$Native_Basics.gt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<'] = _elm_lang$core$Native_Basics.lt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/='] = _elm_lang$core$Native_Basics.neq;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['=='] = _elm_lang$core$Native_Basics.eq;
var _elm_lang$core$Basics$e = _elm_lang$core$Native_Basics.e;
var _elm_lang$core$Basics$pi = _elm_lang$core$Native_Basics.pi;
var _elm_lang$core$Basics$clamp = _elm_lang$core$Native_Basics.clamp;
var _elm_lang$core$Basics$logBase = _elm_lang$core$Native_Basics.logBase;
var _elm_lang$core$Basics$abs = _elm_lang$core$Native_Basics.abs;
var _elm_lang$core$Basics$negate = _elm_lang$core$Native_Basics.negate;
var _elm_lang$core$Basics$sqrt = _elm_lang$core$Native_Basics.sqrt;
var _elm_lang$core$Basics$atan2 = _elm_lang$core$Native_Basics.atan2;
var _elm_lang$core$Basics$atan = _elm_lang$core$Native_Basics.atan;
var _elm_lang$core$Basics$asin = _elm_lang$core$Native_Basics.asin;
var _elm_lang$core$Basics$acos = _elm_lang$core$Native_Basics.acos;
var _elm_lang$core$Basics$tan = _elm_lang$core$Native_Basics.tan;
var _elm_lang$core$Basics$sin = _elm_lang$core$Native_Basics.sin;
var _elm_lang$core$Basics$cos = _elm_lang$core$Native_Basics.cos;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['^'] = _elm_lang$core$Native_Basics.exp;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['%'] = _elm_lang$core$Native_Basics.mod;
var _elm_lang$core$Basics$rem = _elm_lang$core$Native_Basics.rem;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['//'] = _elm_lang$core$Native_Basics.div;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/'] = _elm_lang$core$Native_Basics.floatDiv;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['*'] = _elm_lang$core$Native_Basics.mul;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['-'] = _elm_lang$core$Native_Basics.sub;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['+'] = _elm_lang$core$Native_Basics.add;
var _elm_lang$core$Basics$toPolar = _elm_lang$core$Native_Basics.toPolar;
var _elm_lang$core$Basics$fromPolar = _elm_lang$core$Native_Basics.fromPolar;
var _elm_lang$core$Basics$turns = _elm_lang$core$Native_Basics.turns;
var _elm_lang$core$Basics$degrees = _elm_lang$core$Native_Basics.degrees;
var _elm_lang$core$Basics$radians = function (t) {
	return t;
};
var _elm_lang$core$Basics$GT = {ctor: 'GT'};
var _elm_lang$core$Basics$EQ = {ctor: 'EQ'};
var _elm_lang$core$Basics$LT = {ctor: 'LT'};
var _elm_lang$core$Basics$JustOneMore = function (a) {
	return {ctor: 'JustOneMore', _0: a};
};

var _elm_lang$core$Maybe$withDefault = F2(
	function ($default, maybe) {
		var _p0 = maybe;
		if (_p0.ctor === 'Just') {
			return _p0._0;
		} else {
			return $default;
		}
	});
var _elm_lang$core$Maybe$Nothing = {ctor: 'Nothing'};
var _elm_lang$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		var _p1 = maybeValue;
		if (_p1.ctor === 'Just') {
			return callback(_p1._0);
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$Just = function (a) {
	return {ctor: 'Just', _0: a};
};
var _elm_lang$core$Maybe$map = F2(
	function (f, maybe) {
		var _p2 = maybe;
		if (_p2.ctor === 'Just') {
			return _elm_lang$core$Maybe$Just(
				f(_p2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		var _p3 = {ctor: '_Tuple2', _0: ma, _1: mb};
		if (((_p3.ctor === '_Tuple2') && (_p3._0.ctor === 'Just')) && (_p3._1.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A2(func, _p3._0._0, _p3._1._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map3 = F4(
	function (func, ma, mb, mc) {
		var _p4 = {ctor: '_Tuple3', _0: ma, _1: mb, _2: mc};
		if ((((_p4.ctor === '_Tuple3') && (_p4._0.ctor === 'Just')) && (_p4._1.ctor === 'Just')) && (_p4._2.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A3(func, _p4._0._0, _p4._1._0, _p4._2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map4 = F5(
	function (func, ma, mb, mc, md) {
		var _p5 = {ctor: '_Tuple4', _0: ma, _1: mb, _2: mc, _3: md};
		if (((((_p5.ctor === '_Tuple4') && (_p5._0.ctor === 'Just')) && (_p5._1.ctor === 'Just')) && (_p5._2.ctor === 'Just')) && (_p5._3.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A4(func, _p5._0._0, _p5._1._0, _p5._2._0, _p5._3._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map5 = F6(
	function (func, ma, mb, mc, md, me) {
		var _p6 = {ctor: '_Tuple5', _0: ma, _1: mb, _2: mc, _3: md, _4: me};
		if ((((((_p6.ctor === '_Tuple5') && (_p6._0.ctor === 'Just')) && (_p6._1.ctor === 'Just')) && (_p6._2.ctor === 'Just')) && (_p6._3.ctor === 'Just')) && (_p6._4.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A5(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0, _p6._4._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});

//import Native.Utils //

var _elm_lang$core$Native_List = function() {

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return { ctor: '::', _0: hd, _1: tl };
}

function fromArray(arr)
{
	var out = Nil;
	for (var i = arr.length; i--; )
	{
		out = Cons(arr[i], out);
	}
	return out;
}

function toArray(xs)
{
	var out = [];
	while (xs.ctor !== '[]')
	{
		out.push(xs._0);
		xs = xs._1;
	}
	return out;
}

function foldr(f, b, xs)
{
	var arr = toArray(xs);
	var acc = b;
	for (var i = arr.length; i--; )
	{
		acc = A2(f, arr[i], acc);
	}
	return acc;
}

function map2(f, xs, ys)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]')
	{
		arr.push(A2(f, xs._0, ys._0));
		xs = xs._1;
		ys = ys._1;
	}
	return fromArray(arr);
}

function map3(f, xs, ys, zs)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]' && zs.ctor !== '[]')
	{
		arr.push(A3(f, xs._0, ys._0, zs._0));
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map4(f, ws, xs, ys, zs)
{
	var arr = [];
	while (   ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A4(f, ws._0, xs._0, ys._0, zs._0));
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map5(f, vs, ws, xs, ys, zs)
{
	var arr = [];
	while (   vs.ctor !== '[]'
		   && ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A5(f, vs._0, ws._0, xs._0, ys._0, zs._0));
		vs = vs._1;
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function sortBy(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		return _elm_lang$core$Native_Utils.cmp(f(a), f(b));
	}));
}

function sortWith(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		var ord = f(a)(b).ctor;
		return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
	}));
}

return {
	Nil: Nil,
	Cons: Cons,
	cons: F2(Cons),
	toArray: toArray,
	fromArray: fromArray,

	foldr: F3(foldr),

	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	sortBy: F2(sortBy),
	sortWith: F2(sortWith)
};

}();
var _elm_lang$core$List$sortWith = _elm_lang$core$Native_List.sortWith;
var _elm_lang$core$List$sortBy = _elm_lang$core$Native_List.sortBy;
var _elm_lang$core$List$sort = function (xs) {
	return A2(_elm_lang$core$List$sortBy, _elm_lang$core$Basics$identity, xs);
};
var _elm_lang$core$List$singleton = function (value) {
	return {
		ctor: '::',
		_0: value,
		_1: {ctor: '[]'}
	};
};
var _elm_lang$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return list;
			} else {
				var _p0 = list;
				if (_p0.ctor === '[]') {
					return list;
				} else {
					var _v1 = n - 1,
						_v2 = _p0._1;
					n = _v1;
					list = _v2;
					continue drop;
				}
			}
		}
	});
var _elm_lang$core$List$map5 = _elm_lang$core$Native_List.map5;
var _elm_lang$core$List$map4 = _elm_lang$core$Native_List.map4;
var _elm_lang$core$List$map3 = _elm_lang$core$Native_List.map3;
var _elm_lang$core$List$map2 = _elm_lang$core$Native_List.map2;
var _elm_lang$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			var _p1 = list;
			if (_p1.ctor === '[]') {
				return false;
			} else {
				if (isOkay(_p1._0)) {
					return true;
				} else {
					var _v4 = isOkay,
						_v5 = _p1._1;
					isOkay = _v4;
					list = _v5;
					continue any;
				}
			}
		}
	});
var _elm_lang$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			_elm_lang$core$List$any,
			function (_p2) {
				return !isOkay(_p2);
			},
			list);
	});
var _elm_lang$core$List$foldr = _elm_lang$core$Native_List.foldr;
var _elm_lang$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			var _p3 = list;
			if (_p3.ctor === '[]') {
				return acc;
			} else {
				var _v7 = func,
					_v8 = A2(func, _p3._0, acc),
					_v9 = _p3._1;
				func = _v7;
				acc = _v8;
				list = _v9;
				continue foldl;
			}
		}
	});
var _elm_lang$core$List$length = function (xs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p4, i) {
				return i + 1;
			}),
		0,
		xs);
};
var _elm_lang$core$List$sum = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x + y;
			}),
		0,
		numbers);
};
var _elm_lang$core$List$product = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x * y;
			}),
		1,
		numbers);
};
var _elm_lang$core$List$maximum = function (list) {
	var _p5 = list;
	if (_p5.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$max, _p5._0, _p5._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$minimum = function (list) {
	var _p6 = list;
	if (_p6.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$min, _p6._0, _p6._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$member = F2(
	function (x, xs) {
		return A2(
			_elm_lang$core$List$any,
			function (a) {
				return _elm_lang$core$Native_Utils.eq(a, x);
			},
			xs);
	});
var _elm_lang$core$List$isEmpty = function (xs) {
	var _p7 = xs;
	if (_p7.ctor === '[]') {
		return true;
	} else {
		return false;
	}
};
var _elm_lang$core$List$tail = function (list) {
	var _p8 = list;
	if (_p8.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p8._1);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$head = function (list) {
	var _p9 = list;
	if (_p9.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p9._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List_ops = _elm_lang$core$List_ops || {};
_elm_lang$core$List_ops['::'] = _elm_lang$core$Native_List.cons;
var _elm_lang$core$List$map = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			F2(
				function (x, acc) {
					return {
						ctor: '::',
						_0: f(x),
						_1: acc
					};
				}),
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$filter = F2(
	function (pred, xs) {
		var conditionalCons = F2(
			function (front, back) {
				return pred(front) ? {ctor: '::', _0: front, _1: back} : back;
			});
		return A3(
			_elm_lang$core$List$foldr,
			conditionalCons,
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _p10 = f(mx);
		if (_p10.ctor === 'Just') {
			return {ctor: '::', _0: _p10._0, _1: xs};
		} else {
			return xs;
		}
	});
var _elm_lang$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			_elm_lang$core$List$maybeCons(f),
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$reverse = function (list) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return {ctor: '::', _0: x, _1: y};
			}),
		{ctor: '[]'},
		list);
};
var _elm_lang$core$List$scanl = F3(
	function (f, b, xs) {
		var scan1 = F2(
			function (x, accAcc) {
				var _p11 = accAcc;
				if (_p11.ctor === '::') {
					return {
						ctor: '::',
						_0: A2(f, x, _p11._0),
						_1: accAcc
					};
				} else {
					return {ctor: '[]'};
				}
			});
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$foldl,
				scan1,
				{
					ctor: '::',
					_0: b,
					_1: {ctor: '[]'}
				},
				xs));
	});
var _elm_lang$core$List$append = F2(
	function (xs, ys) {
		var _p12 = ys;
		if (_p12.ctor === '[]') {
			return xs;
		} else {
			return A3(
				_elm_lang$core$List$foldr,
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					}),
				ys,
				xs);
		}
	});
var _elm_lang$core$List$concat = function (lists) {
	return A3(
		_elm_lang$core$List$foldr,
		_elm_lang$core$List$append,
		{ctor: '[]'},
		lists);
};
var _elm_lang$core$List$concatMap = F2(
	function (f, list) {
		return _elm_lang$core$List$concat(
			A2(_elm_lang$core$List$map, f, list));
	});
var _elm_lang$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _p13) {
				var _p14 = _p13;
				var _p16 = _p14._0;
				var _p15 = _p14._1;
				return pred(x) ? {
					ctor: '_Tuple2',
					_0: {ctor: '::', _0: x, _1: _p16},
					_1: _p15
				} : {
					ctor: '_Tuple2',
					_0: _p16,
					_1: {ctor: '::', _0: x, _1: _p15}
				};
			});
		return A3(
			_elm_lang$core$List$foldr,
			step,
			{
				ctor: '_Tuple2',
				_0: {ctor: '[]'},
				_1: {ctor: '[]'}
			},
			list);
	});
var _elm_lang$core$List$unzip = function (pairs) {
	var step = F2(
		function (_p18, _p17) {
			var _p19 = _p18;
			var _p20 = _p17;
			return {
				ctor: '_Tuple2',
				_0: {ctor: '::', _0: _p19._0, _1: _p20._0},
				_1: {ctor: '::', _0: _p19._1, _1: _p20._1}
			};
		});
	return A3(
		_elm_lang$core$List$foldr,
		step,
		{
			ctor: '_Tuple2',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'}
		},
		pairs);
};
var _elm_lang$core$List$intersperse = F2(
	function (sep, xs) {
		var _p21 = xs;
		if (_p21.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var step = F2(
				function (x, rest) {
					return {
						ctor: '::',
						_0: sep,
						_1: {ctor: '::', _0: x, _1: rest}
					};
				});
			var spersed = A3(
				_elm_lang$core$List$foldr,
				step,
				{ctor: '[]'},
				_p21._1);
			return {ctor: '::', _0: _p21._0, _1: spersed};
		}
	});
var _elm_lang$core$List$takeReverse = F3(
	function (n, list, taken) {
		takeReverse:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return taken;
			} else {
				var _p22 = list;
				if (_p22.ctor === '[]') {
					return taken;
				} else {
					var _v23 = n - 1,
						_v24 = _p22._1,
						_v25 = {ctor: '::', _0: _p22._0, _1: taken};
					n = _v23;
					list = _v24;
					taken = _v25;
					continue takeReverse;
				}
			}
		}
	});
var _elm_lang$core$List$takeTailRec = F2(
	function (n, list) {
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$takeReverse,
				n,
				list,
				{ctor: '[]'}));
	});
var _elm_lang$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
			return {ctor: '[]'};
		} else {
			var _p23 = {ctor: '_Tuple2', _0: n, _1: list};
			_v26_5:
			do {
				_v26_1:
				do {
					if (_p23.ctor === '_Tuple2') {
						if (_p23._1.ctor === '[]') {
							return list;
						} else {
							if (_p23._1._1.ctor === '::') {
								switch (_p23._0) {
									case 1:
										break _v26_1;
									case 2:
										return {
											ctor: '::',
											_0: _p23._1._0,
											_1: {
												ctor: '::',
												_0: _p23._1._1._0,
												_1: {ctor: '[]'}
											}
										};
									case 3:
										if (_p23._1._1._1.ctor === '::') {
											return {
												ctor: '::',
												_0: _p23._1._0,
												_1: {
													ctor: '::',
													_0: _p23._1._1._0,
													_1: {
														ctor: '::',
														_0: _p23._1._1._1._0,
														_1: {ctor: '[]'}
													}
												}
											};
										} else {
											break _v26_5;
										}
									default:
										if ((_p23._1._1._1.ctor === '::') && (_p23._1._1._1._1.ctor === '::')) {
											var _p28 = _p23._1._1._1._0;
											var _p27 = _p23._1._1._0;
											var _p26 = _p23._1._0;
											var _p25 = _p23._1._1._1._1._0;
											var _p24 = _p23._1._1._1._1._1;
											return (_elm_lang$core$Native_Utils.cmp(ctr, 1000) > 0) ? {
												ctor: '::',
												_0: _p26,
												_1: {
													ctor: '::',
													_0: _p27,
													_1: {
														ctor: '::',
														_0: _p28,
														_1: {
															ctor: '::',
															_0: _p25,
															_1: A2(_elm_lang$core$List$takeTailRec, n - 4, _p24)
														}
													}
												}
											} : {
												ctor: '::',
												_0: _p26,
												_1: {
													ctor: '::',
													_0: _p27,
													_1: {
														ctor: '::',
														_0: _p28,
														_1: {
															ctor: '::',
															_0: _p25,
															_1: A3(_elm_lang$core$List$takeFast, ctr + 1, n - 4, _p24)
														}
													}
												}
											};
										} else {
											break _v26_5;
										}
								}
							} else {
								if (_p23._0 === 1) {
									break _v26_1;
								} else {
									break _v26_5;
								}
							}
						}
					} else {
						break _v26_5;
					}
				} while(false);
				return {
					ctor: '::',
					_0: _p23._1._0,
					_1: {ctor: '[]'}
				};
			} while(false);
			return list;
		}
	});
var _elm_lang$core$List$take = F2(
	function (n, list) {
		return A3(_elm_lang$core$List$takeFast, 0, n, list);
	});
var _elm_lang$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return result;
			} else {
				var _v27 = {ctor: '::', _0: value, _1: result},
					_v28 = n - 1,
					_v29 = value;
				result = _v27;
				n = _v28;
				value = _v29;
				continue repeatHelp;
			}
		}
	});
var _elm_lang$core$List$repeat = F2(
	function (n, value) {
		return A3(
			_elm_lang$core$List$repeatHelp,
			{ctor: '[]'},
			n,
			value);
	});
var _elm_lang$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(lo, hi) < 1) {
				var _v30 = lo,
					_v31 = hi - 1,
					_v32 = {ctor: '::', _0: hi, _1: list};
				lo = _v30;
				hi = _v31;
				list = _v32;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var _elm_lang$core$List$range = F2(
	function (lo, hi) {
		return A3(
			_elm_lang$core$List$rangeHelp,
			lo,
			hi,
			{ctor: '[]'});
	});
var _elm_lang$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$map2,
			f,
			A2(
				_elm_lang$core$List$range,
				0,
				_elm_lang$core$List$length(xs) - 1),
			xs);
	});

//import Native.Utils //

var _elm_lang$core$Native_Debug = function() {

function log(tag, value)
{
	var msg = tag + ': ' + _elm_lang$core$Native_Utils.toString(value);
	var process = process || {};
	if (process.stdout)
	{
		process.stdout.write(msg);
	}
	else
	{
		console.log(msg);
	}
	return value;
}

function crash(message)
{
	throw new Error(message);
}

return {
	crash: crash,
	log: F2(log)
};

}();
//import Maybe, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_String = function() {

function isEmpty(str)
{
	return str.length === 0;
}
function cons(chr, str)
{
	return chr + str;
}
function uncons(str)
{
	var hd = str[0];
	if (hd)
	{
		return _elm_lang$core$Maybe$Just(_elm_lang$core$Native_Utils.Tuple2(_elm_lang$core$Native_Utils.chr(hd), str.slice(1)));
	}
	return _elm_lang$core$Maybe$Nothing;
}
function append(a, b)
{
	return a + b;
}
function concat(strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join('');
}
function length(str)
{
	return str.length;
}
function map(f, str)
{
	var out = str.split('');
	for (var i = out.length; i--; )
	{
		out[i] = f(_elm_lang$core$Native_Utils.chr(out[i]));
	}
	return out.join('');
}
function filter(pred, str)
{
	return str.split('').map(_elm_lang$core$Native_Utils.chr).filter(pred).join('');
}
function reverse(str)
{
	return str.split('').reverse().join('');
}
function foldl(f, b, str)
{
	var len = str.length;
	for (var i = 0; i < len; ++i)
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function foldr(f, b, str)
{
	for (var i = str.length; i--; )
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function split(sep, str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(sep));
}
function join(sep, strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join(sep);
}
function repeat(n, str)
{
	var result = '';
	while (n > 0)
	{
		if (n & 1)
		{
			result += str;
		}
		n >>= 1, str += str;
	}
	return result;
}
function slice(start, end, str)
{
	return str.slice(start, end);
}
function left(n, str)
{
	return n < 1 ? '' : str.slice(0, n);
}
function right(n, str)
{
	return n < 1 ? '' : str.slice(-n);
}
function dropLeft(n, str)
{
	return n < 1 ? str : str.slice(n);
}
function dropRight(n, str)
{
	return n < 1 ? str : str.slice(0, -n);
}
function pad(n, chr, str)
{
	var half = (n - str.length) / 2;
	return repeat(Math.ceil(half), chr) + str + repeat(half | 0, chr);
}
function padRight(n, chr, str)
{
	return str + repeat(n - str.length, chr);
}
function padLeft(n, chr, str)
{
	return repeat(n - str.length, chr) + str;
}

function trim(str)
{
	return str.trim();
}
function trimLeft(str)
{
	return str.replace(/^\s+/, '');
}
function trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function words(str)
{
	return _elm_lang$core$Native_List.fromArray(str.trim().split(/\s+/g));
}
function lines(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(/\r\n|\r|\n/g));
}

function toUpper(str)
{
	return str.toUpperCase();
}
function toLower(str)
{
	return str.toLowerCase();
}

function any(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return true;
		}
	}
	return false;
}
function all(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (!pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return false;
		}
	}
	return true;
}

function contains(sub, str)
{
	return str.indexOf(sub) > -1;
}
function startsWith(sub, str)
{
	return str.indexOf(sub) === 0;
}
function endsWith(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
}
function indexes(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _elm_lang$core$Native_List.Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _elm_lang$core$Native_List.fromArray(is);
}


function toInt(s)
{
	var len = s.length;

	// if empty
	if (len === 0)
	{
		return intErr(s);
	}

	// if hex
	var c = s[0];
	if (c === '0' && s[1] === 'x')
	{
		for (var i = 2; i < len; ++i)
		{
			var c = s[i];
			if (('0' <= c && c <= '9') || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f'))
			{
				continue;
			}
			return intErr(s);
		}
		return _elm_lang$core$Result$Ok(parseInt(s, 16));
	}

	// is decimal
	if (c > '9' || (c < '0' && c !== '-' && c !== '+'))
	{
		return intErr(s);
	}
	for (var i = 1; i < len; ++i)
	{
		var c = s[i];
		if (c < '0' || '9' < c)
		{
			return intErr(s);
		}
	}

	return _elm_lang$core$Result$Ok(parseInt(s, 10));
}

function intErr(s)
{
	return _elm_lang$core$Result$Err("could not convert string '" + s + "' to an Int");
}


function toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return floatErr(s);
	}
	var n = +s;
	// faster isNaN check
	return n === n ? _elm_lang$core$Result$Ok(n) : floatErr(s);
}

function floatErr(s)
{
	return _elm_lang$core$Result$Err("could not convert string '" + s + "' to a Float");
}


function toList(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split('').map(_elm_lang$core$Native_Utils.chr));
}
function fromList(chars)
{
	return _elm_lang$core$Native_List.toArray(chars).join('');
}

return {
	isEmpty: isEmpty,
	cons: F2(cons),
	uncons: uncons,
	append: F2(append),
	concat: concat,
	length: length,
	map: F2(map),
	filter: F2(filter),
	reverse: reverse,
	foldl: F3(foldl),
	foldr: F3(foldr),

	split: F2(split),
	join: F2(join),
	repeat: F2(repeat),

	slice: F3(slice),
	left: F2(left),
	right: F2(right),
	dropLeft: F2(dropLeft),
	dropRight: F2(dropRight),

	pad: F3(pad),
	padLeft: F3(padLeft),
	padRight: F3(padRight),

	trim: trim,
	trimLeft: trimLeft,
	trimRight: trimRight,

	words: words,
	lines: lines,

	toUpper: toUpper,
	toLower: toLower,

	any: F2(any),
	all: F2(all),

	contains: F2(contains),
	startsWith: F2(startsWith),
	endsWith: F2(endsWith),
	indexes: F2(indexes),

	toInt: toInt,
	toFloat: toFloat,
	toList: toList,
	fromList: fromList
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Char = function() {

return {
	fromCode: function(c) { return _elm_lang$core$Native_Utils.chr(String.fromCharCode(c)); },
	toCode: function(c) { return c.charCodeAt(0); },
	toUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toUpperCase()); },
	toLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLowerCase()); },
	toLocaleUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleUpperCase()); },
	toLocaleLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleLowerCase()); }
};

}();
var _elm_lang$core$Char$fromCode = _elm_lang$core$Native_Char.fromCode;
var _elm_lang$core$Char$toCode = _elm_lang$core$Native_Char.toCode;
var _elm_lang$core$Char$toLocaleLower = _elm_lang$core$Native_Char.toLocaleLower;
var _elm_lang$core$Char$toLocaleUpper = _elm_lang$core$Native_Char.toLocaleUpper;
var _elm_lang$core$Char$toLower = _elm_lang$core$Native_Char.toLower;
var _elm_lang$core$Char$toUpper = _elm_lang$core$Native_Char.toUpper;
var _elm_lang$core$Char$isBetween = F3(
	function (low, high, $char) {
		var code = _elm_lang$core$Char$toCode($char);
		return (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(low)) > -1) && (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(high)) < 1);
	});
var _elm_lang$core$Char$isUpper = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('A'),
	_elm_lang$core$Native_Utils.chr('Z'));
var _elm_lang$core$Char$isLower = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('a'),
	_elm_lang$core$Native_Utils.chr('z'));
var _elm_lang$core$Char$isDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('9'));
var _elm_lang$core$Char$isOctDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('7'));
var _elm_lang$core$Char$isHexDigit = function ($char) {
	return _elm_lang$core$Char$isDigit($char) || (A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('a'),
		_elm_lang$core$Native_Utils.chr('f'),
		$char) || A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('A'),
		_elm_lang$core$Native_Utils.chr('F'),
		$char));
};

var _elm_lang$core$Result$toMaybe = function (result) {
	var _p0 = result;
	if (_p0.ctor === 'Ok') {
		return _elm_lang$core$Maybe$Just(_p0._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$Result$withDefault = F2(
	function (def, result) {
		var _p1 = result;
		if (_p1.ctor === 'Ok') {
			return _p1._0;
		} else {
			return def;
		}
	});
var _elm_lang$core$Result$Err = function (a) {
	return {ctor: 'Err', _0: a};
};
var _elm_lang$core$Result$andThen = F2(
	function (callback, result) {
		var _p2 = result;
		if (_p2.ctor === 'Ok') {
			return callback(_p2._0);
		} else {
			return _elm_lang$core$Result$Err(_p2._0);
		}
	});
var _elm_lang$core$Result$Ok = function (a) {
	return {ctor: 'Ok', _0: a};
};
var _elm_lang$core$Result$map = F2(
	function (func, ra) {
		var _p3 = ra;
		if (_p3.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(
				func(_p3._0));
		} else {
			return _elm_lang$core$Result$Err(_p3._0);
		}
	});
var _elm_lang$core$Result$map2 = F3(
	function (func, ra, rb) {
		var _p4 = {ctor: '_Tuple2', _0: ra, _1: rb};
		if (_p4._0.ctor === 'Ok') {
			if (_p4._1.ctor === 'Ok') {
				return _elm_lang$core$Result$Ok(
					A2(func, _p4._0._0, _p4._1._0));
			} else {
				return _elm_lang$core$Result$Err(_p4._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p4._0._0);
		}
	});
var _elm_lang$core$Result$map3 = F4(
	function (func, ra, rb, rc) {
		var _p5 = {ctor: '_Tuple3', _0: ra, _1: rb, _2: rc};
		if (_p5._0.ctor === 'Ok') {
			if (_p5._1.ctor === 'Ok') {
				if (_p5._2.ctor === 'Ok') {
					return _elm_lang$core$Result$Ok(
						A3(func, _p5._0._0, _p5._1._0, _p5._2._0));
				} else {
					return _elm_lang$core$Result$Err(_p5._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p5._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p5._0._0);
		}
	});
var _elm_lang$core$Result$map4 = F5(
	function (func, ra, rb, rc, rd) {
		var _p6 = {ctor: '_Tuple4', _0: ra, _1: rb, _2: rc, _3: rd};
		if (_p6._0.ctor === 'Ok') {
			if (_p6._1.ctor === 'Ok') {
				if (_p6._2.ctor === 'Ok') {
					if (_p6._3.ctor === 'Ok') {
						return _elm_lang$core$Result$Ok(
							A4(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0));
					} else {
						return _elm_lang$core$Result$Err(_p6._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p6._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p6._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p6._0._0);
		}
	});
var _elm_lang$core$Result$map5 = F6(
	function (func, ra, rb, rc, rd, re) {
		var _p7 = {ctor: '_Tuple5', _0: ra, _1: rb, _2: rc, _3: rd, _4: re};
		if (_p7._0.ctor === 'Ok') {
			if (_p7._1.ctor === 'Ok') {
				if (_p7._2.ctor === 'Ok') {
					if (_p7._3.ctor === 'Ok') {
						if (_p7._4.ctor === 'Ok') {
							return _elm_lang$core$Result$Ok(
								A5(func, _p7._0._0, _p7._1._0, _p7._2._0, _p7._3._0, _p7._4._0));
						} else {
							return _elm_lang$core$Result$Err(_p7._4._0);
						}
					} else {
						return _elm_lang$core$Result$Err(_p7._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p7._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p7._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p7._0._0);
		}
	});
var _elm_lang$core$Result$mapError = F2(
	function (f, result) {
		var _p8 = result;
		if (_p8.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(_p8._0);
		} else {
			return _elm_lang$core$Result$Err(
				f(_p8._0));
		}
	});
var _elm_lang$core$Result$fromMaybe = F2(
	function (err, maybe) {
		var _p9 = maybe;
		if (_p9.ctor === 'Just') {
			return _elm_lang$core$Result$Ok(_p9._0);
		} else {
			return _elm_lang$core$Result$Err(err);
		}
	});

var _elm_lang$core$String$fromList = _elm_lang$core$Native_String.fromList;
var _elm_lang$core$String$toList = _elm_lang$core$Native_String.toList;
var _elm_lang$core$String$toFloat = _elm_lang$core$Native_String.toFloat;
var _elm_lang$core$String$toInt = _elm_lang$core$Native_String.toInt;
var _elm_lang$core$String$indices = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$indexes = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$endsWith = _elm_lang$core$Native_String.endsWith;
var _elm_lang$core$String$startsWith = _elm_lang$core$Native_String.startsWith;
var _elm_lang$core$String$contains = _elm_lang$core$Native_String.contains;
var _elm_lang$core$String$all = _elm_lang$core$Native_String.all;
var _elm_lang$core$String$any = _elm_lang$core$Native_String.any;
var _elm_lang$core$String$toLower = _elm_lang$core$Native_String.toLower;
var _elm_lang$core$String$toUpper = _elm_lang$core$Native_String.toUpper;
var _elm_lang$core$String$lines = _elm_lang$core$Native_String.lines;
var _elm_lang$core$String$words = _elm_lang$core$Native_String.words;
var _elm_lang$core$String$trimRight = _elm_lang$core$Native_String.trimRight;
var _elm_lang$core$String$trimLeft = _elm_lang$core$Native_String.trimLeft;
var _elm_lang$core$String$trim = _elm_lang$core$Native_String.trim;
var _elm_lang$core$String$padRight = _elm_lang$core$Native_String.padRight;
var _elm_lang$core$String$padLeft = _elm_lang$core$Native_String.padLeft;
var _elm_lang$core$String$pad = _elm_lang$core$Native_String.pad;
var _elm_lang$core$String$dropRight = _elm_lang$core$Native_String.dropRight;
var _elm_lang$core$String$dropLeft = _elm_lang$core$Native_String.dropLeft;
var _elm_lang$core$String$right = _elm_lang$core$Native_String.right;
var _elm_lang$core$String$left = _elm_lang$core$Native_String.left;
var _elm_lang$core$String$slice = _elm_lang$core$Native_String.slice;
var _elm_lang$core$String$repeat = _elm_lang$core$Native_String.repeat;
var _elm_lang$core$String$join = _elm_lang$core$Native_String.join;
var _elm_lang$core$String$split = _elm_lang$core$Native_String.split;
var _elm_lang$core$String$foldr = _elm_lang$core$Native_String.foldr;
var _elm_lang$core$String$foldl = _elm_lang$core$Native_String.foldl;
var _elm_lang$core$String$reverse = _elm_lang$core$Native_String.reverse;
var _elm_lang$core$String$filter = _elm_lang$core$Native_String.filter;
var _elm_lang$core$String$map = _elm_lang$core$Native_String.map;
var _elm_lang$core$String$length = _elm_lang$core$Native_String.length;
var _elm_lang$core$String$concat = _elm_lang$core$Native_String.concat;
var _elm_lang$core$String$append = _elm_lang$core$Native_String.append;
var _elm_lang$core$String$uncons = _elm_lang$core$Native_String.uncons;
var _elm_lang$core$String$cons = _elm_lang$core$Native_String.cons;
var _elm_lang$core$String$fromChar = function ($char) {
	return A2(_elm_lang$core$String$cons, $char, '');
};
var _elm_lang$core$String$isEmpty = _elm_lang$core$Native_String.isEmpty;

var _elm_lang$core$Dict$foldr = F3(
	function (f, acc, t) {
		foldr:
		while (true) {
			var _p0 = t;
			if (_p0.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v1 = f,
					_v2 = A3(
					f,
					_p0._1,
					_p0._2,
					A3(_elm_lang$core$Dict$foldr, f, acc, _p0._4)),
					_v3 = _p0._3;
				f = _v1;
				acc = _v2;
				t = _v3;
				continue foldr;
			}
		}
	});
var _elm_lang$core$Dict$keys = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return {ctor: '::', _0: key, _1: keyList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$values = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return {ctor: '::', _0: value, _1: valueList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$toList = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: key, _1: value},
					_1: list
				};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$foldl = F3(
	function (f, acc, dict) {
		foldl:
		while (true) {
			var _p1 = dict;
			if (_p1.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v5 = f,
					_v6 = A3(
					f,
					_p1._1,
					_p1._2,
					A3(_elm_lang$core$Dict$foldl, f, acc, _p1._3)),
					_v7 = _p1._4;
				f = _v5;
				acc = _v6;
				dict = _v7;
				continue foldl;
			}
		}
	});
var _elm_lang$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _p2) {
				stepState:
				while (true) {
					var _p3 = _p2;
					var _p9 = _p3._1;
					var _p8 = _p3._0;
					var _p4 = _p8;
					if (_p4.ctor === '[]') {
						return {
							ctor: '_Tuple2',
							_0: _p8,
							_1: A3(rightStep, rKey, rValue, _p9)
						};
					} else {
						var _p7 = _p4._1;
						var _p6 = _p4._0._1;
						var _p5 = _p4._0._0;
						if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) < 0) {
							var _v10 = rKey,
								_v11 = rValue,
								_v12 = {
								ctor: '_Tuple2',
								_0: _p7,
								_1: A3(leftStep, _p5, _p6, _p9)
							};
							rKey = _v10;
							rValue = _v11;
							_p2 = _v12;
							continue stepState;
						} else {
							if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) > 0) {
								return {
									ctor: '_Tuple2',
									_0: _p8,
									_1: A3(rightStep, rKey, rValue, _p9)
								};
							} else {
								return {
									ctor: '_Tuple2',
									_0: _p7,
									_1: A4(bothStep, _p5, _p6, rValue, _p9)
								};
							}
						}
					}
				}
			});
		var _p10 = A3(
			_elm_lang$core$Dict$foldl,
			stepState,
			{
				ctor: '_Tuple2',
				_0: _elm_lang$core$Dict$toList(leftDict),
				_1: initialResult
			},
			rightDict);
		var leftovers = _p10._0;
		var intermediateResult = _p10._1;
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (_p11, result) {
					var _p12 = _p11;
					return A3(leftStep, _p12._0, _p12._1, result);
				}),
			intermediateResult,
			leftovers);
	});
var _elm_lang$core$Dict$reportRemBug = F4(
	function (msg, c, lgot, rgot) {
		return _elm_lang$core$Native_Debug.crash(
			_elm_lang$core$String$concat(
				{
					ctor: '::',
					_0: 'Internal red-black tree invariant violated, expected ',
					_1: {
						ctor: '::',
						_0: msg,
						_1: {
							ctor: '::',
							_0: ' and got ',
							_1: {
								ctor: '::',
								_0: _elm_lang$core$Basics$toString(c),
								_1: {
									ctor: '::',
									_0: '/',
									_1: {
										ctor: '::',
										_0: lgot,
										_1: {
											ctor: '::',
											_0: '/',
											_1: {
												ctor: '::',
												_0: rgot,
												_1: {
													ctor: '::',
													_0: '\nPlease report this bug to <https://github.com/elm-lang/core/issues>',
													_1: {ctor: '[]'}
												}
											}
										}
									}
								}
							}
						}
					}
				}));
	});
var _elm_lang$core$Dict$isBBlack = function (dict) {
	var _p13 = dict;
	_v14_2:
	do {
		if (_p13.ctor === 'RBNode_elm_builtin') {
			if (_p13._0.ctor === 'BBlack') {
				return true;
			} else {
				break _v14_2;
			}
		} else {
			if (_p13._0.ctor === 'LBBlack') {
				return true;
			} else {
				break _v14_2;
			}
		}
	} while(false);
	return false;
};
var _elm_lang$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			var _p14 = dict;
			if (_p14.ctor === 'RBEmpty_elm_builtin') {
				return n;
			} else {
				var _v16 = A2(_elm_lang$core$Dict$sizeHelp, n + 1, _p14._4),
					_v17 = _p14._3;
				n = _v16;
				dict = _v17;
				continue sizeHelp;
			}
		}
	});
var _elm_lang$core$Dict$size = function (dict) {
	return A2(_elm_lang$core$Dict$sizeHelp, 0, dict);
};
var _elm_lang$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			var _p15 = dict;
			if (_p15.ctor === 'RBEmpty_elm_builtin') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p16 = A2(_elm_lang$core$Basics$compare, targetKey, _p15._1);
				switch (_p16.ctor) {
					case 'LT':
						var _v20 = targetKey,
							_v21 = _p15._3;
						targetKey = _v20;
						dict = _v21;
						continue get;
					case 'EQ':
						return _elm_lang$core$Maybe$Just(_p15._2);
					default:
						var _v22 = targetKey,
							_v23 = _p15._4;
						targetKey = _v22;
						dict = _v23;
						continue get;
				}
			}
		}
	});
var _elm_lang$core$Dict$member = F2(
	function (key, dict) {
		var _p17 = A2(_elm_lang$core$Dict$get, key, dict);
		if (_p17.ctor === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var _elm_lang$core$Dict$maxWithDefault = F3(
	function (k, v, r) {
		maxWithDefault:
		while (true) {
			var _p18 = r;
			if (_p18.ctor === 'RBEmpty_elm_builtin') {
				return {ctor: '_Tuple2', _0: k, _1: v};
			} else {
				var _v26 = _p18._1,
					_v27 = _p18._2,
					_v28 = _p18._4;
				k = _v26;
				v = _v27;
				r = _v28;
				continue maxWithDefault;
			}
		}
	});
var _elm_lang$core$Dict$NBlack = {ctor: 'NBlack'};
var _elm_lang$core$Dict$BBlack = {ctor: 'BBlack'};
var _elm_lang$core$Dict$Black = {ctor: 'Black'};
var _elm_lang$core$Dict$blackish = function (t) {
	var _p19 = t;
	if (_p19.ctor === 'RBNode_elm_builtin') {
		var _p20 = _p19._0;
		return _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$Black) || _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$BBlack);
	} else {
		return true;
	}
};
var _elm_lang$core$Dict$Red = {ctor: 'Red'};
var _elm_lang$core$Dict$moreBlack = function (color) {
	var _p21 = color;
	switch (_p21.ctor) {
		case 'Black':
			return _elm_lang$core$Dict$BBlack;
		case 'Red':
			return _elm_lang$core$Dict$Black;
		case 'NBlack':
			return _elm_lang$core$Dict$Red;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a double black node more black!');
	}
};
var _elm_lang$core$Dict$lessBlack = function (color) {
	var _p22 = color;
	switch (_p22.ctor) {
		case 'BBlack':
			return _elm_lang$core$Dict$Black;
		case 'Black':
			return _elm_lang$core$Dict$Red;
		case 'Red':
			return _elm_lang$core$Dict$NBlack;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a negative black node less black!');
	}
};
var _elm_lang$core$Dict$LBBlack = {ctor: 'LBBlack'};
var _elm_lang$core$Dict$LBlack = {ctor: 'LBlack'};
var _elm_lang$core$Dict$RBEmpty_elm_builtin = function (a) {
	return {ctor: 'RBEmpty_elm_builtin', _0: a};
};
var _elm_lang$core$Dict$empty = _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
var _elm_lang$core$Dict$isEmpty = function (dict) {
	return _elm_lang$core$Native_Utils.eq(dict, _elm_lang$core$Dict$empty);
};
var _elm_lang$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {ctor: 'RBNode_elm_builtin', _0: a, _1: b, _2: c, _3: d, _4: e};
	});
var _elm_lang$core$Dict$ensureBlackRoot = function (dict) {
	var _p23 = dict;
	if ((_p23.ctor === 'RBNode_elm_builtin') && (_p23._0.ctor === 'Red')) {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p23._1, _p23._2, _p23._3, _p23._4);
	} else {
		return dict;
	}
};
var _elm_lang$core$Dict$lessBlackTree = function (dict) {
	var _p24 = dict;
	if (_p24.ctor === 'RBNode_elm_builtin') {
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$lessBlack(_p24._0),
			_p24._1,
			_p24._2,
			_p24._3,
			_p24._4);
	} else {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	}
};
var _elm_lang$core$Dict$balancedTree = function (col) {
	return function (xk) {
		return function (xv) {
			return function (yk) {
				return function (yv) {
					return function (zk) {
						return function (zv) {
							return function (a) {
								return function (b) {
									return function (c) {
										return function (d) {
											return A5(
												_elm_lang$core$Dict$RBNode_elm_builtin,
												_elm_lang$core$Dict$lessBlack(col),
												yk,
												yv,
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, xk, xv, a, b),
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, zk, zv, c, d));
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _elm_lang$core$Dict$blacken = function (t) {
	var _p25 = t;
	if (_p25.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p25._1, _p25._2, _p25._3, _p25._4);
	}
};
var _elm_lang$core$Dict$redden = function (t) {
	var _p26 = t;
	if (_p26.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Native_Debug.crash('can\'t make a Leaf red');
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, _p26._1, _p26._2, _p26._3, _p26._4);
	}
};
var _elm_lang$core$Dict$balanceHelp = function (tree) {
	var _p27 = tree;
	_v36_6:
	do {
		_v36_5:
		do {
			_v36_4:
			do {
				_v36_3:
				do {
					_v36_2:
					do {
						_v36_1:
						do {
							_v36_0:
							do {
								if (_p27.ctor === 'RBNode_elm_builtin') {
									if (_p27._3.ctor === 'RBNode_elm_builtin') {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._3._0.ctor) {
												case 'Red':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																		break _v36_2;
																	} else {
																		if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																			break _v36_3;
																		} else {
																			break _v36_6;
																		}
																	}
																}
															}
														case 'NBlack':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																		break _v36_4;
																	} else {
																		break _v36_6;
																	}
																}
															}
														default:
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	break _v36_6;
																}
															}
													}
												case 'NBlack':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															}
														case 'NBlack':
															if (_p27._0.ctor === 'BBlack') {
																if ((((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																	break _v36_4;
																} else {
																	if ((((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															} else {
																break _v36_6;
															}
														default:
															if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																break _v36_5;
															} else {
																break _v36_6;
															}
													}
												default:
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	break _v36_6;
																}
															}
														case 'NBlack':
															if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																break _v36_4;
															} else {
																break _v36_6;
															}
														default:
															break _v36_6;
													}
											}
										} else {
											switch (_p27._3._0.ctor) {
												case 'Red':
													if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
														break _v36_0;
													} else {
														if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
															break _v36_1;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
														break _v36_5;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										}
									} else {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._4._0.ctor) {
												case 'Red':
													if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
														break _v36_2;
													} else {
														if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
															break _v36_3;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
														break _v36_4;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										} else {
											break _v36_6;
										}
									}
								} else {
									break _v36_6;
								}
							} while(false);
							return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._3._1)(_p27._3._3._2)(_p27._3._1)(_p27._3._2)(_p27._1)(_p27._2)(_p27._3._3._3)(_p27._3._3._4)(_p27._3._4)(_p27._4);
						} while(false);
						return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._1)(_p27._3._2)(_p27._3._4._1)(_p27._3._4._2)(_p27._1)(_p27._2)(_p27._3._3)(_p27._3._4._3)(_p27._3._4._4)(_p27._4);
					} while(false);
					return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._3._1)(_p27._4._3._2)(_p27._4._1)(_p27._4._2)(_p27._3)(_p27._4._3._3)(_p27._4._3._4)(_p27._4._4);
				} while(false);
				return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._1)(_p27._4._2)(_p27._4._4._1)(_p27._4._4._2)(_p27._3)(_p27._4._3)(_p27._4._4._3)(_p27._4._4._4);
			} while(false);
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_elm_lang$core$Dict$Black,
				_p27._4._3._1,
				_p27._4._3._2,
				A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3, _p27._4._3._3),
				A5(
					_elm_lang$core$Dict$balance,
					_elm_lang$core$Dict$Black,
					_p27._4._1,
					_p27._4._2,
					_p27._4._3._4,
					_elm_lang$core$Dict$redden(_p27._4._4)));
		} while(false);
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$Black,
			_p27._3._4._1,
			_p27._3._4._2,
			A5(
				_elm_lang$core$Dict$balance,
				_elm_lang$core$Dict$Black,
				_p27._3._1,
				_p27._3._2,
				_elm_lang$core$Dict$redden(_p27._3._3),
				_p27._3._4._3),
			A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3._4._4, _p27._4));
	} while(false);
	return tree;
};
var _elm_lang$core$Dict$balance = F5(
	function (c, k, v, l, r) {
		var tree = A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
		return _elm_lang$core$Dict$blackish(tree) ? _elm_lang$core$Dict$balanceHelp(tree) : tree;
	});
var _elm_lang$core$Dict$bubble = F5(
	function (c, k, v, l, r) {
		return (_elm_lang$core$Dict$isBBlack(l) || _elm_lang$core$Dict$isBBlack(r)) ? A5(
			_elm_lang$core$Dict$balance,
			_elm_lang$core$Dict$moreBlack(c),
			k,
			v,
			_elm_lang$core$Dict$lessBlackTree(l),
			_elm_lang$core$Dict$lessBlackTree(r)) : A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
	});
var _elm_lang$core$Dict$removeMax = F5(
	function (c, k, v, l, r) {
		var _p28 = r;
		if (_p28.ctor === 'RBEmpty_elm_builtin') {
			return A3(_elm_lang$core$Dict$rem, c, l, r);
		} else {
			return A5(
				_elm_lang$core$Dict$bubble,
				c,
				k,
				v,
				l,
				A5(_elm_lang$core$Dict$removeMax, _p28._0, _p28._1, _p28._2, _p28._3, _p28._4));
		}
	});
var _elm_lang$core$Dict$rem = F3(
	function (color, left, right) {
		var _p29 = {ctor: '_Tuple2', _0: left, _1: right};
		if (_p29._0.ctor === 'RBEmpty_elm_builtin') {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p30 = color;
				switch (_p30.ctor) {
					case 'Red':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
					case 'Black':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBBlack);
					default:
						return _elm_lang$core$Native_Debug.crash('cannot have bblack or nblack nodes at this point');
				}
			} else {
				var _p33 = _p29._1._0;
				var _p32 = _p29._0._0;
				var _p31 = {ctor: '_Tuple3', _0: color, _1: _p32, _2: _p33};
				if ((((_p31.ctor === '_Tuple3') && (_p31._0.ctor === 'Black')) && (_p31._1.ctor === 'LBlack')) && (_p31._2.ctor === 'Red')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._1._1, _p29._1._2, _p29._1._3, _p29._1._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/LBlack/Red',
						color,
						_elm_lang$core$Basics$toString(_p32),
						_elm_lang$core$Basics$toString(_p33));
				}
			}
		} else {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p36 = _p29._1._0;
				var _p35 = _p29._0._0;
				var _p34 = {ctor: '_Tuple3', _0: color, _1: _p35, _2: _p36};
				if ((((_p34.ctor === '_Tuple3') && (_p34._0.ctor === 'Black')) && (_p34._1.ctor === 'Red')) && (_p34._2.ctor === 'LBlack')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._0._1, _p29._0._2, _p29._0._3, _p29._0._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/Red/LBlack',
						color,
						_elm_lang$core$Basics$toString(_p35),
						_elm_lang$core$Basics$toString(_p36));
				}
			} else {
				var _p40 = _p29._0._2;
				var _p39 = _p29._0._4;
				var _p38 = _p29._0._1;
				var newLeft = A5(_elm_lang$core$Dict$removeMax, _p29._0._0, _p38, _p40, _p29._0._3, _p39);
				var _p37 = A3(_elm_lang$core$Dict$maxWithDefault, _p38, _p40, _p39);
				var k = _p37._0;
				var v = _p37._1;
				return A5(_elm_lang$core$Dict$bubble, color, k, v, newLeft, right);
			}
		}
	});
var _elm_lang$core$Dict$map = F2(
	function (f, dict) {
		var _p41 = dict;
		if (_p41.ctor === 'RBEmpty_elm_builtin') {
			return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
		} else {
			var _p42 = _p41._1;
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_p41._0,
				_p42,
				A2(f, _p42, _p41._2),
				A2(_elm_lang$core$Dict$map, f, _p41._3),
				A2(_elm_lang$core$Dict$map, f, _p41._4));
		}
	});
var _elm_lang$core$Dict$Same = {ctor: 'Same'};
var _elm_lang$core$Dict$Remove = {ctor: 'Remove'};
var _elm_lang$core$Dict$Insert = {ctor: 'Insert'};
var _elm_lang$core$Dict$update = F3(
	function (k, alter, dict) {
		var up = function (dict) {
			var _p43 = dict;
			if (_p43.ctor === 'RBEmpty_elm_builtin') {
				var _p44 = alter(_elm_lang$core$Maybe$Nothing);
				if (_p44.ctor === 'Nothing') {
					return {ctor: '_Tuple2', _0: _elm_lang$core$Dict$Same, _1: _elm_lang$core$Dict$empty};
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Dict$Insert,
						_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, k, _p44._0, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty)
					};
				}
			} else {
				var _p55 = _p43._2;
				var _p54 = _p43._4;
				var _p53 = _p43._3;
				var _p52 = _p43._1;
				var _p51 = _p43._0;
				var _p45 = A2(_elm_lang$core$Basics$compare, k, _p52);
				switch (_p45.ctor) {
					case 'EQ':
						var _p46 = alter(
							_elm_lang$core$Maybe$Just(_p55));
						if (_p46.ctor === 'Nothing') {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Remove,
								_1: A3(_elm_lang$core$Dict$rem, _p51, _p53, _p54)
							};
						} else {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Same,
								_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p46._0, _p53, _p54)
							};
						}
					case 'LT':
						var _p47 = up(_p53);
						var flag = _p47._0;
						var newLeft = _p47._1;
						var _p48 = flag;
						switch (_p48.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, newLeft, _p54)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, newLeft, _p54)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, newLeft, _p54)
								};
						}
					default:
						var _p49 = up(_p54);
						var flag = _p49._0;
						var newRight = _p49._1;
						var _p50 = flag;
						switch (_p50.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, _p53, newRight)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, _p53, newRight)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, _p53, newRight)
								};
						}
				}
			}
		};
		var _p56 = up(dict);
		var flag = _p56._0;
		var updatedDict = _p56._1;
		var _p57 = flag;
		switch (_p57.ctor) {
			case 'Same':
				return updatedDict;
			case 'Insert':
				return _elm_lang$core$Dict$ensureBlackRoot(updatedDict);
			default:
				return _elm_lang$core$Dict$blacken(updatedDict);
		}
	});
var _elm_lang$core$Dict$insert = F3(
	function (key, value, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(
				_elm_lang$core$Maybe$Just(value)),
			dict);
	});
var _elm_lang$core$Dict$singleton = F2(
	function (key, value) {
		return A3(_elm_lang$core$Dict$insert, key, value, _elm_lang$core$Dict$empty);
	});
var _elm_lang$core$Dict$union = F2(
	function (t1, t2) {
		return A3(_elm_lang$core$Dict$foldl, _elm_lang$core$Dict$insert, t2, t1);
	});
var _elm_lang$core$Dict$filter = F2(
	function (predicate, dictionary) {
		var add = F3(
			function (key, value, dict) {
				return A2(predicate, key, value) ? A3(_elm_lang$core$Dict$insert, key, value, dict) : dict;
			});
		return A3(_elm_lang$core$Dict$foldl, add, _elm_lang$core$Dict$empty, dictionary);
	});
var _elm_lang$core$Dict$intersect = F2(
	function (t1, t2) {
		return A2(
			_elm_lang$core$Dict$filter,
			F2(
				function (k, _p58) {
					return A2(_elm_lang$core$Dict$member, k, t2);
				}),
			t1);
	});
var _elm_lang$core$Dict$partition = F2(
	function (predicate, dict) {
		var add = F3(
			function (key, value, _p59) {
				var _p60 = _p59;
				var _p62 = _p60._1;
				var _p61 = _p60._0;
				return A2(predicate, key, value) ? {
					ctor: '_Tuple2',
					_0: A3(_elm_lang$core$Dict$insert, key, value, _p61),
					_1: _p62
				} : {
					ctor: '_Tuple2',
					_0: _p61,
					_1: A3(_elm_lang$core$Dict$insert, key, value, _p62)
				};
			});
		return A3(
			_elm_lang$core$Dict$foldl,
			add,
			{ctor: '_Tuple2', _0: _elm_lang$core$Dict$empty, _1: _elm_lang$core$Dict$empty},
			dict);
	});
var _elm_lang$core$Dict$fromList = function (assocs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p63, dict) {
				var _p64 = _p63;
				return A3(_elm_lang$core$Dict$insert, _p64._0, _p64._1, dict);
			}),
		_elm_lang$core$Dict$empty,
		assocs);
};
var _elm_lang$core$Dict$remove = F2(
	function (key, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(_elm_lang$core$Maybe$Nothing),
			dict);
	});
var _elm_lang$core$Dict$diff = F2(
	function (t1, t2) {
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, v, t) {
					return A2(_elm_lang$core$Dict$remove, k, t);
				}),
			t1,
			t2);
	});

//import Native.List //

var _elm_lang$core$Native_Array = function() {

// A RRB-Tree has two distinct data types.
// Leaf -> "height"  is always 0
//         "table"   is an array of elements
// Node -> "height"  is always greater than 0
//         "table"   is an array of child nodes
//         "lengths" is an array of accumulated lengths of the child nodes

// M is the maximal table size. 32 seems fast. E is the allowed increase
// of search steps when concatting to find an index. Lower values will
// decrease balancing, but will increase search steps.
var M = 32;
var E = 2;

// An empty array.
var empty = {
	ctor: '_Array',
	height: 0,
	table: []
};


function get(i, array)
{
	if (i < 0 || i >= length(array))
	{
		throw new Error(
			'Index ' + i + ' is out of range. Check the length of ' +
			'your array first or use getMaybe or getWithDefault.');
	}
	return unsafeGet(i, array);
}


function unsafeGet(i, array)
{
	for (var x = array.height; x > 0; x--)
	{
		var slot = i >> (x * 5);
		while (array.lengths[slot] <= i)
		{
			slot++;
		}
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array = array.table[slot];
	}
	return array.table[i];
}


// Sets the value at the index i. Only the nodes leading to i will get
// copied and updated.
function set(i, item, array)
{
	if (i < 0 || length(array) <= i)
	{
		return array;
	}
	return unsafeSet(i, item, array);
}


function unsafeSet(i, item, array)
{
	array = nodeCopy(array);

	if (array.height === 0)
	{
		array.table[i] = item;
	}
	else
	{
		var slot = getSlot(i, array);
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array.table[slot] = unsafeSet(i, item, array.table[slot]);
	}
	return array;
}


function initialize(len, f)
{
	if (len <= 0)
	{
		return empty;
	}
	var h = Math.floor( Math.log(len) / Math.log(M) );
	return initialize_(f, h, 0, len);
}

function initialize_(f, h, from, to)
{
	if (h === 0)
	{
		var table = new Array((to - from) % (M + 1));
		for (var i = 0; i < table.length; i++)
		{
		  table[i] = f(from + i);
		}
		return {
			ctor: '_Array',
			height: 0,
			table: table
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = initialize_(f, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

function fromList(list)
{
	if (list.ctor === '[]')
	{
		return empty;
	}

	// Allocate M sized blocks (table) and write list elements to it.
	var table = new Array(M);
	var nodes = [];
	var i = 0;

	while (list.ctor !== '[]')
	{
		table[i] = list._0;
		list = list._1;
		i++;

		// table is full, so we can push a leaf containing it into the
		// next node.
		if (i === M)
		{
			var leaf = {
				ctor: '_Array',
				height: 0,
				table: table
			};
			fromListPush(leaf, nodes);
			table = new Array(M);
			i = 0;
		}
	}

	// Maybe there is something left on the table.
	if (i > 0)
	{
		var leaf = {
			ctor: '_Array',
			height: 0,
			table: table.splice(0, i)
		};
		fromListPush(leaf, nodes);
	}

	// Go through all of the nodes and eventually push them into higher nodes.
	for (var h = 0; h < nodes.length - 1; h++)
	{
		if (nodes[h].table.length > 0)
		{
			fromListPush(nodes[h], nodes);
		}
	}

	var head = nodes[nodes.length - 1];
	if (head.height > 0 && head.table.length === 1)
	{
		return head.table[0];
	}
	else
	{
		return head;
	}
}

// Push a node into a higher node as a child.
function fromListPush(toPush, nodes)
{
	var h = toPush.height;

	// Maybe the node on this height does not exist.
	if (nodes.length === h)
	{
		var node = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
		nodes.push(node);
	}

	nodes[h].table.push(toPush);
	var len = length(toPush);
	if (nodes[h].lengths.length > 0)
	{
		len += nodes[h].lengths[nodes[h].lengths.length - 1];
	}
	nodes[h].lengths.push(len);

	if (nodes[h].table.length === M)
	{
		fromListPush(nodes[h], nodes);
		nodes[h] = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
	}
}

// Pushes an item via push_ to the bottom right of a tree.
function push(item, a)
{
	var pushed = push_(item, a);
	if (pushed !== null)
	{
		return pushed;
	}

	var newTree = create(item, a.height);
	return siblise(a, newTree);
}

// Recursively tries to push an item to the bottom-right most
// tree possible. If there is no space left for the item,
// null will be returned.
function push_(item, a)
{
	// Handle resursion stop at leaf level.
	if (a.height === 0)
	{
		if (a.table.length < M)
		{
			var newA = {
				ctor: '_Array',
				height: 0,
				table: a.table.slice()
			};
			newA.table.push(item);
			return newA;
		}
		else
		{
		  return null;
		}
	}

	// Recursively push
	var pushed = push_(item, botRight(a));

	// There was space in the bottom right tree, so the slot will
	// be updated.
	if (pushed !== null)
	{
		var newA = nodeCopy(a);
		newA.table[newA.table.length - 1] = pushed;
		newA.lengths[newA.lengths.length - 1]++;
		return newA;
	}

	// When there was no space left, check if there is space left
	// for a new slot with a tree which contains only the item
	// at the bottom.
	if (a.table.length < M)
	{
		var newSlot = create(item, a.height - 1);
		var newA = nodeCopy(a);
		newA.table.push(newSlot);
		newA.lengths.push(newA.lengths[newA.lengths.length - 1] + length(newSlot));
		return newA;
	}
	else
	{
		return null;
	}
}

// Converts an array into a list of elements.
function toList(a)
{
	return toList_(_elm_lang$core$Native_List.Nil, a);
}

function toList_(list, a)
{
	for (var i = a.table.length - 1; i >= 0; i--)
	{
		list =
			a.height === 0
				? _elm_lang$core$Native_List.Cons(a.table[i], list)
				: toList_(list, a.table[i]);
	}
	return list;
}

// Maps a function over the elements of an array.
function map(f, a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? f(a.table[i])
				: map(f, a.table[i]);
	}
	return newA;
}

// Maps a function over the elements with their index as first argument.
function indexedMap(f, a)
{
	return indexedMap_(f, a, 0);
}

function indexedMap_(f, a, from)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? A2(f, from + i, a.table[i])
				: indexedMap_(f, a.table[i], i == 0 ? from : from + a.lengths[i - 1]);
	}
	return newA;
}

function foldl(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = foldl(f, b, a.table[i]);
		}
	}
	return b;
}

function foldr(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = a.table.length; i--; )
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = a.table.length; i--; )
		{
			b = foldr(f, b, a.table[i]);
		}
	}
	return b;
}

// TODO: currently, it slices the right, then the left. This can be
// optimized.
function slice(from, to, a)
{
	if (from < 0)
	{
		from += length(a);
	}
	if (to < 0)
	{
		to += length(a);
	}
	return sliceLeft(from, sliceRight(to, a));
}

function sliceRight(to, a)
{
	if (to === length(a))
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(0, to);
		return newA;
	}

	// Slice the right recursively.
	var right = getSlot(to, a);
	var sliced = sliceRight(to - (right > 0 ? a.lengths[right - 1] : 0), a.table[right]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (right === 0)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(0, right),
		lengths: a.lengths.slice(0, right)
	};
	if (sliced.table.length > 0)
	{
		newA.table[right] = sliced;
		newA.lengths[right] = length(sliced) + (right > 0 ? newA.lengths[right - 1] : 0);
	}
	return newA;
}

function sliceLeft(from, a)
{
	if (from === 0)
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(from, a.table.length + 1);
		return newA;
	}

	// Slice the left recursively.
	var left = getSlot(from, a);
	var sliced = sliceLeft(from - (left > 0 ? a.lengths[left - 1] : 0), a.table[left]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (left === a.table.length - 1)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(left, a.table.length + 1),
		lengths: new Array(a.table.length - left)
	};
	newA.table[0] = sliced;
	var len = 0;
	for (var i = 0; i < newA.table.length; i++)
	{
		len += length(newA.table[i]);
		newA.lengths[i] = len;
	}

	return newA;
}

// Appends two trees.
function append(a,b)
{
	if (a.table.length === 0)
	{
		return b;
	}
	if (b.table.length === 0)
	{
		return a;
	}

	var c = append_(a, b);

	// Check if both nodes can be crunshed together.
	if (c[0].table.length + c[1].table.length <= M)
	{
		if (c[0].table.length === 0)
		{
			return c[1];
		}
		if (c[1].table.length === 0)
		{
			return c[0];
		}

		// Adjust .table and .lengths
		c[0].table = c[0].table.concat(c[1].table);
		if (c[0].height > 0)
		{
			var len = length(c[0]);
			for (var i = 0; i < c[1].lengths.length; i++)
			{
				c[1].lengths[i] += len;
			}
			c[0].lengths = c[0].lengths.concat(c[1].lengths);
		}

		return c[0];
	}

	if (c[0].height > 0)
	{
		var toRemove = calcToRemove(a, b);
		if (toRemove > E)
		{
			c = shuffle(c[0], c[1], toRemove);
		}
	}

	return siblise(c[0], c[1]);
}

// Returns an array of two nodes; right and left. One node _may_ be empty.
function append_(a, b)
{
	if (a.height === 0 && b.height === 0)
	{
		return [a, b];
	}

	if (a.height !== 1 || b.height !== 1)
	{
		if (a.height === b.height)
		{
			a = nodeCopy(a);
			b = nodeCopy(b);
			var appended = append_(botRight(a), botLeft(b));

			insertRight(a, appended[1]);
			insertLeft(b, appended[0]);
		}
		else if (a.height > b.height)
		{
			a = nodeCopy(a);
			var appended = append_(botRight(a), b);

			insertRight(a, appended[0]);
			b = parentise(appended[1], appended[1].height + 1);
		}
		else
		{
			b = nodeCopy(b);
			var appended = append_(a, botLeft(b));

			var left = appended[0].table.length === 0 ? 0 : 1;
			var right = left === 0 ? 1 : 0;
			insertLeft(b, appended[left]);
			a = parentise(appended[right], appended[right].height + 1);
		}
	}

	// Check if balancing is needed and return based on that.
	if (a.table.length === 0 || b.table.length === 0)
	{
		return [a, b];
	}

	var toRemove = calcToRemove(a, b);
	if (toRemove <= E)
	{
		return [a, b];
	}
	return shuffle(a, b, toRemove);
}

// Helperfunctions for append_. Replaces a child node at the side of the parent.
function insertRight(parent, node)
{
	var index = parent.table.length - 1;
	parent.table[index] = node;
	parent.lengths[index] = length(node);
	parent.lengths[index] += index > 0 ? parent.lengths[index - 1] : 0;
}

function insertLeft(parent, node)
{
	if (node.table.length > 0)
	{
		parent.table[0] = node;
		parent.lengths[0] = length(node);

		var len = length(parent.table[0]);
		for (var i = 1; i < parent.lengths.length; i++)
		{
			len += length(parent.table[i]);
			parent.lengths[i] = len;
		}
	}
	else
	{
		parent.table.shift();
		for (var i = 1; i < parent.lengths.length; i++)
		{
			parent.lengths[i] = parent.lengths[i] - parent.lengths[0];
		}
		parent.lengths.shift();
	}
}

// Returns the extra search steps for E. Refer to the paper.
function calcToRemove(a, b)
{
	var subLengths = 0;
	for (var i = 0; i < a.table.length; i++)
	{
		subLengths += a.table[i].table.length;
	}
	for (var i = 0; i < b.table.length; i++)
	{
		subLengths += b.table[i].table.length;
	}

	var toRemove = a.table.length + b.table.length;
	return toRemove - (Math.floor((subLengths - 1) / M) + 1);
}

// get2, set2 and saveSlot are helpers for accessing elements over two arrays.
function get2(a, b, index)
{
	return index < a.length
		? a[index]
		: b[index - a.length];
}

function set2(a, b, index, value)
{
	if (index < a.length)
	{
		a[index] = value;
	}
	else
	{
		b[index - a.length] = value;
	}
}

function saveSlot(a, b, index, slot)
{
	set2(a.table, b.table, index, slot);

	var l = (index === 0 || index === a.lengths.length)
		? 0
		: get2(a.lengths, a.lengths, index - 1);

	set2(a.lengths, b.lengths, index, l + length(slot));
}

// Creates a node or leaf with a given length at their arrays for perfomance.
// Is only used by shuffle.
function createNode(h, length)
{
	if (length < 0)
	{
		length = 0;
	}
	var a = {
		ctor: '_Array',
		height: h,
		table: new Array(length)
	};
	if (h > 0)
	{
		a.lengths = new Array(length);
	}
	return a;
}

// Returns an array of two balanced nodes.
function shuffle(a, b, toRemove)
{
	var newA = createNode(a.height, Math.min(M, a.table.length + b.table.length - toRemove));
	var newB = createNode(a.height, newA.table.length - (a.table.length + b.table.length - toRemove));

	// Skip the slots with size M. More precise: copy the slot references
	// to the new node
	var read = 0;
	while (get2(a.table, b.table, read).table.length % M === 0)
	{
		set2(newA.table, newB.table, read, get2(a.table, b.table, read));
		set2(newA.lengths, newB.lengths, read, get2(a.lengths, b.lengths, read));
		read++;
	}

	// Pulling items from left to right, caching in a slot before writing
	// it into the new nodes.
	var write = read;
	var slot = new createNode(a.height - 1, 0);
	var from = 0;

	// If the current slot is still containing data, then there will be at
	// least one more write, so we do not break this loop yet.
	while (read - write - (slot.table.length > 0 ? 1 : 0) < toRemove)
	{
		// Find out the max possible items for copying.
		var source = get2(a.table, b.table, read);
		var to = Math.min(M - slot.table.length, source.table.length);

		// Copy and adjust size table.
		slot.table = slot.table.concat(source.table.slice(from, to));
		if (slot.height > 0)
		{
			var len = slot.lengths.length;
			for (var i = len; i < len + to - from; i++)
			{
				slot.lengths[i] = length(slot.table[i]);
				slot.lengths[i] += (i > 0 ? slot.lengths[i - 1] : 0);
			}
		}

		from += to;

		// Only proceed to next slots[i] if the current one was
		// fully copied.
		if (source.table.length <= to)
		{
			read++; from = 0;
		}

		// Only create a new slot if the current one is filled up.
		if (slot.table.length === M)
		{
			saveSlot(newA, newB, write, slot);
			slot = createNode(a.height - 1, 0);
			write++;
		}
	}

	// Cleanup after the loop. Copy the last slot into the new nodes.
	if (slot.table.length > 0)
	{
		saveSlot(newA, newB, write, slot);
		write++;
	}

	// Shift the untouched slots to the left
	while (read < a.table.length + b.table.length )
	{
		saveSlot(newA, newB, write, get2(a.table, b.table, read));
		read++;
		write++;
	}

	return [newA, newB];
}

// Navigation functions
function botRight(a)
{
	return a.table[a.table.length - 1];
}
function botLeft(a)
{
	return a.table[0];
}

// Copies a node for updating. Note that you should not use this if
// only updating only one of "table" or "lengths" for performance reasons.
function nodeCopy(a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice()
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths.slice();
	}
	return newA;
}

// Returns how many items are in the tree.
function length(array)
{
	if (array.height === 0)
	{
		return array.table.length;
	}
	else
	{
		return array.lengths[array.lengths.length - 1];
	}
}

// Calculates in which slot of "table" the item probably is, then
// find the exact slot via forward searching in  "lengths". Returns the index.
function getSlot(i, a)
{
	var slot = i >> (5 * a.height);
	while (a.lengths[slot] <= i)
	{
		slot++;
	}
	return slot;
}

// Recursively creates a tree with a given height containing
// only the given item.
function create(item, h)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: [item]
		};
	}
	return {
		ctor: '_Array',
		height: h,
		table: [create(item, h - 1)],
		lengths: [1]
	};
}

// Recursively creates a tree that contains the given tree.
function parentise(tree, h)
{
	if (h === tree.height)
	{
		return tree;
	}

	return {
		ctor: '_Array',
		height: h,
		table: [parentise(tree, h - 1)],
		lengths: [length(tree)]
	};
}

// Emphasizes blood brotherhood beneath two trees.
function siblise(a, b)
{
	return {
		ctor: '_Array',
		height: a.height + 1,
		table: [a, b],
		lengths: [length(a), length(a) + length(b)]
	};
}

function toJSArray(a)
{
	var jsArray = new Array(length(a));
	toJSArray_(jsArray, 0, a);
	return jsArray;
}

function toJSArray_(jsArray, i, a)
{
	for (var t = 0; t < a.table.length; t++)
	{
		if (a.height === 0)
		{
			jsArray[i + t] = a.table[t];
		}
		else
		{
			var inc = t === 0 ? 0 : a.lengths[t - 1];
			toJSArray_(jsArray, i + inc, a.table[t]);
		}
	}
}

function fromJSArray(jsArray)
{
	if (jsArray.length === 0)
	{
		return empty;
	}
	var h = Math.floor(Math.log(jsArray.length) / Math.log(M));
	return fromJSArray_(jsArray, h, 0, jsArray.length);
}

function fromJSArray_(jsArray, h, from, to)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: jsArray.slice(from, to)
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = fromJSArray_(jsArray, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i - 1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

return {
	empty: empty,
	fromList: fromList,
	toList: toList,
	initialize: F2(initialize),
	append: F2(append),
	push: F2(push),
	slice: F3(slice),
	get: F2(get),
	set: F3(set),
	map: F2(map),
	indexedMap: F2(indexedMap),
	foldl: F3(foldl),
	foldr: F3(foldr),
	length: length,

	toJSArray: toJSArray,
	fromJSArray: fromJSArray
};

}();
var _elm_lang$core$Array$append = _elm_lang$core$Native_Array.append;
var _elm_lang$core$Array$length = _elm_lang$core$Native_Array.length;
var _elm_lang$core$Array$isEmpty = function (array) {
	return _elm_lang$core$Native_Utils.eq(
		_elm_lang$core$Array$length(array),
		0);
};
var _elm_lang$core$Array$slice = _elm_lang$core$Native_Array.slice;
var _elm_lang$core$Array$set = _elm_lang$core$Native_Array.set;
var _elm_lang$core$Array$get = F2(
	function (i, array) {
		return ((_elm_lang$core$Native_Utils.cmp(0, i) < 1) && (_elm_lang$core$Native_Utils.cmp(
			i,
			_elm_lang$core$Native_Array.length(array)) < 0)) ? _elm_lang$core$Maybe$Just(
			A2(_elm_lang$core$Native_Array.get, i, array)) : _elm_lang$core$Maybe$Nothing;
	});
var _elm_lang$core$Array$push = _elm_lang$core$Native_Array.push;
var _elm_lang$core$Array$empty = _elm_lang$core$Native_Array.empty;
var _elm_lang$core$Array$filter = F2(
	function (isOkay, arr) {
		var update = F2(
			function (x, xs) {
				return isOkay(x) ? A2(_elm_lang$core$Native_Array.push, x, xs) : xs;
			});
		return A3(_elm_lang$core$Native_Array.foldl, update, _elm_lang$core$Native_Array.empty, arr);
	});
var _elm_lang$core$Array$foldr = _elm_lang$core$Native_Array.foldr;
var _elm_lang$core$Array$foldl = _elm_lang$core$Native_Array.foldl;
var _elm_lang$core$Array$indexedMap = _elm_lang$core$Native_Array.indexedMap;
var _elm_lang$core$Array$map = _elm_lang$core$Native_Array.map;
var _elm_lang$core$Array$toIndexedList = function (array) {
	return A3(
		_elm_lang$core$List$map2,
		F2(
			function (v0, v1) {
				return {ctor: '_Tuple2', _0: v0, _1: v1};
			}),
		A2(
			_elm_lang$core$List$range,
			0,
			_elm_lang$core$Native_Array.length(array) - 1),
		_elm_lang$core$Native_Array.toList(array));
};
var _elm_lang$core$Array$toList = _elm_lang$core$Native_Array.toList;
var _elm_lang$core$Array$fromList = _elm_lang$core$Native_Array.fromList;
var _elm_lang$core$Array$initialize = _elm_lang$core$Native_Array.initialize;
var _elm_lang$core$Array$repeat = F2(
	function (n, e) {
		return A2(
			_elm_lang$core$Array$initialize,
			n,
			_elm_lang$core$Basics$always(e));
	});
var _elm_lang$core$Array$Array = {ctor: 'Array'};

//import Maybe, Native.Array, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_Json = function() {


// CORE DECODERS

function succeed(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'succeed',
		msg: msg
	};
}

function fail(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'fail',
		msg: msg
	};
}

function decodePrimitive(tag)
{
	return {
		ctor: '<decoder>',
		tag: tag
	};
}

function decodeContainer(tag, decoder)
{
	return {
		ctor: '<decoder>',
		tag: tag,
		decoder: decoder
	};
}

function decodeNull(value)
{
	return {
		ctor: '<decoder>',
		tag: 'null',
		value: value
	};
}

function decodeField(field, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'field',
		field: field,
		decoder: decoder
	};
}

function decodeIndex(index, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'index',
		index: index,
		decoder: decoder
	};
}

function decodeKeyValuePairs(decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'key-value',
		decoder: decoder
	};
}

function mapMany(f, decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'map-many',
		func: f,
		decoders: decoders
	};
}

function andThen(callback, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'andThen',
		decoder: decoder,
		callback: callback
	};
}

function oneOf(decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'oneOf',
		decoders: decoders
	};
}


// DECODING OBJECTS

function map1(f, d1)
{
	return mapMany(f, [d1]);
}

function map2(f, d1, d2)
{
	return mapMany(f, [d1, d2]);
}

function map3(f, d1, d2, d3)
{
	return mapMany(f, [d1, d2, d3]);
}

function map4(f, d1, d2, d3, d4)
{
	return mapMany(f, [d1, d2, d3, d4]);
}

function map5(f, d1, d2, d3, d4, d5)
{
	return mapMany(f, [d1, d2, d3, d4, d5]);
}

function map6(f, d1, d2, d3, d4, d5, d6)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6]);
}

function map7(f, d1, d2, d3, d4, d5, d6, d7)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
}

function map8(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
}


// DECODE HELPERS

function ok(value)
{
	return { tag: 'ok', value: value };
}

function badPrimitive(type, value)
{
	return { tag: 'primitive', type: type, value: value };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badField(field, nestedProblems)
{
	return { tag: 'field', field: field, rest: nestedProblems };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badOneOf(problems)
{
	return { tag: 'oneOf', problems: problems };
}

function bad(msg)
{
	return { tag: 'fail', msg: msg };
}

function badToString(problem)
{
	var context = '_';
	while (problem)
	{
		switch (problem.tag)
		{
			case 'primitive':
				return 'Expecting ' + problem.type
					+ (context === '_' ? '' : ' at ' + context)
					+ ' but instead got: ' + jsToString(problem.value);

			case 'index':
				context += '[' + problem.index + ']';
				problem = problem.rest;
				break;

			case 'field':
				context += '.' + problem.field;
				problem = problem.rest;
				break;

			case 'oneOf':
				var problems = problem.problems;
				for (var i = 0; i < problems.length; i++)
				{
					problems[i] = badToString(problems[i]);
				}
				return 'I ran into the following problems'
					+ (context === '_' ? '' : ' at ' + context)
					+ ':\n\n' + problems.join('\n');

			case 'fail':
				return 'I ran into a `fail` decoder'
					+ (context === '_' ? '' : ' at ' + context)
					+ ': ' + problem.msg;
		}
	}
}

function jsToString(value)
{
	return value === undefined
		? 'undefined'
		: JSON.stringify(value);
}


// DECODE

function runOnString(decoder, string)
{
	var json;
	try
	{
		json = JSON.parse(string);
	}
	catch (e)
	{
		return _elm_lang$core$Result$Err('Given an invalid JSON: ' + e.message);
	}
	return run(decoder, json);
}

function run(decoder, value)
{
	var result = runHelp(decoder, value);
	return (result.tag === 'ok')
		? _elm_lang$core$Result$Ok(result.value)
		: _elm_lang$core$Result$Err(badToString(result));
}

function runHelp(decoder, value)
{
	switch (decoder.tag)
	{
		case 'bool':
			return (typeof value === 'boolean')
				? ok(value)
				: badPrimitive('a Bool', value);

		case 'int':
			if (typeof value !== 'number') {
				return badPrimitive('an Int', value);
			}

			if (-2147483647 < value && value < 2147483647 && (value | 0) === value) {
				return ok(value);
			}

			if (isFinite(value) && !(value % 1)) {
				return ok(value);
			}

			return badPrimitive('an Int', value);

		case 'float':
			return (typeof value === 'number')
				? ok(value)
				: badPrimitive('a Float', value);

		case 'string':
			return (typeof value === 'string')
				? ok(value)
				: (value instanceof String)
					? ok(value + '')
					: badPrimitive('a String', value);

		case 'null':
			return (value === null)
				? ok(decoder.value)
				: badPrimitive('null', value);

		case 'value':
			return ok(value);

		case 'list':
			if (!(value instanceof Array))
			{
				return badPrimitive('a List', value);
			}

			var list = _elm_lang$core$Native_List.Nil;
			for (var i = value.length; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result)
				}
				list = _elm_lang$core$Native_List.Cons(result.value, list);
			}
			return ok(list);

		case 'array':
			if (!(value instanceof Array))
			{
				return badPrimitive('an Array', value);
			}

			var len = value.length;
			var array = new Array(len);
			for (var i = len; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result);
				}
				array[i] = result.value;
			}
			return ok(_elm_lang$core$Native_Array.fromJSArray(array));

		case 'maybe':
			var result = runHelp(decoder.decoder, value);
			return (result.tag === 'ok')
				? ok(_elm_lang$core$Maybe$Just(result.value))
				: ok(_elm_lang$core$Maybe$Nothing);

		case 'field':
			var field = decoder.field;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return badPrimitive('an object with a field named `' + field + '`', value);
			}

			var result = runHelp(decoder.decoder, value[field]);
			return (result.tag === 'ok') ? result : badField(field, result);

		case 'index':
			var index = decoder.index;
			if (!(value instanceof Array))
			{
				return badPrimitive('an array', value);
			}
			if (index >= value.length)
			{
				return badPrimitive('a longer array. Need index ' + index + ' but there are only ' + value.length + ' entries', value);
			}

			var result = runHelp(decoder.decoder, value[index]);
			return (result.tag === 'ok') ? result : badIndex(index, result);

		case 'key-value':
			if (typeof value !== 'object' || value === null || value instanceof Array)
			{
				return badPrimitive('an object', value);
			}

			var keyValuePairs = _elm_lang$core$Native_List.Nil;
			for (var key in value)
			{
				var result = runHelp(decoder.decoder, value[key]);
				if (result.tag !== 'ok')
				{
					return badField(key, result);
				}
				var pair = _elm_lang$core$Native_Utils.Tuple2(key, result.value);
				keyValuePairs = _elm_lang$core$Native_List.Cons(pair, keyValuePairs);
			}
			return ok(keyValuePairs);

		case 'map-many':
			var answer = decoder.func;
			var decoders = decoder.decoders;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = runHelp(decoders[i], value);
				if (result.tag !== 'ok')
				{
					return result;
				}
				answer = answer(result.value);
			}
			return ok(answer);

		case 'andThen':
			var result = runHelp(decoder.decoder, value);
			return (result.tag !== 'ok')
				? result
				: runHelp(decoder.callback(result.value), value);

		case 'oneOf':
			var errors = [];
			var temp = decoder.decoders;
			while (temp.ctor !== '[]')
			{
				var result = runHelp(temp._0, value);

				if (result.tag === 'ok')
				{
					return result;
				}

				errors.push(result);

				temp = temp._1;
			}
			return badOneOf(errors);

		case 'fail':
			return bad(decoder.msg);

		case 'succeed':
			return ok(decoder.msg);
	}
}


// EQUALITY

function equality(a, b)
{
	if (a === b)
	{
		return true;
	}

	if (a.tag !== b.tag)
	{
		return false;
	}

	switch (a.tag)
	{
		case 'succeed':
		case 'fail':
			return a.msg === b.msg;

		case 'bool':
		case 'int':
		case 'float':
		case 'string':
		case 'value':
			return true;

		case 'null':
			return a.value === b.value;

		case 'list':
		case 'array':
		case 'maybe':
		case 'key-value':
			return equality(a.decoder, b.decoder);

		case 'field':
			return a.field === b.field && equality(a.decoder, b.decoder);

		case 'index':
			return a.index === b.index && equality(a.decoder, b.decoder);

		case 'map-many':
			if (a.func !== b.func)
			{
				return false;
			}
			return listEquality(a.decoders, b.decoders);

		case 'andThen':
			return a.callback === b.callback && equality(a.decoder, b.decoder);

		case 'oneOf':
			return listEquality(a.decoders, b.decoders);
	}
}

function listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

function encode(indentLevel, value)
{
	return JSON.stringify(value, null, indentLevel);
}

function identity(value)
{
	return value;
}

function encodeObject(keyValuePairs)
{
	var obj = {};
	while (keyValuePairs.ctor !== '[]')
	{
		var pair = keyValuePairs._0;
		obj[pair._0] = pair._1;
		keyValuePairs = keyValuePairs._1;
	}
	return obj;
}

return {
	encode: F2(encode),
	runOnString: F2(runOnString),
	run: F2(run),

	decodeNull: decodeNull,
	decodePrimitive: decodePrimitive,
	decodeContainer: F2(decodeContainer),

	decodeField: F2(decodeField),
	decodeIndex: F2(decodeIndex),

	map1: F2(map1),
	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	map6: F7(map6),
	map7: F8(map7),
	map8: F9(map8),
	decodeKeyValuePairs: decodeKeyValuePairs,

	andThen: F2(andThen),
	fail: fail,
	succeed: succeed,
	oneOf: oneOf,

	identity: identity,
	encodeNull: null,
	encodeArray: _elm_lang$core$Native_Array.toJSArray,
	encodeList: _elm_lang$core$Native_List.toArray,
	encodeObject: encodeObject,

	equality: equality
};

}();

var _elm_lang$core$Json_Encode$list = _elm_lang$core$Native_Json.encodeList;
var _elm_lang$core$Json_Encode$array = _elm_lang$core$Native_Json.encodeArray;
var _elm_lang$core$Json_Encode$object = _elm_lang$core$Native_Json.encodeObject;
var _elm_lang$core$Json_Encode$null = _elm_lang$core$Native_Json.encodeNull;
var _elm_lang$core$Json_Encode$bool = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$float = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$int = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$string = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$encode = _elm_lang$core$Native_Json.encode;
var _elm_lang$core$Json_Encode$Value = {ctor: 'Value'};

var _elm_lang$core$Json_Decode$null = _elm_lang$core$Native_Json.decodeNull;
var _elm_lang$core$Json_Decode$value = _elm_lang$core$Native_Json.decodePrimitive('value');
var _elm_lang$core$Json_Decode$andThen = _elm_lang$core$Native_Json.andThen;
var _elm_lang$core$Json_Decode$fail = _elm_lang$core$Native_Json.fail;
var _elm_lang$core$Json_Decode$succeed = _elm_lang$core$Native_Json.succeed;
var _elm_lang$core$Json_Decode$lazy = function (thunk) {
	return A2(
		_elm_lang$core$Json_Decode$andThen,
		thunk,
		_elm_lang$core$Json_Decode$succeed(
			{ctor: '_Tuple0'}));
};
var _elm_lang$core$Json_Decode$decodeValue = _elm_lang$core$Native_Json.run;
var _elm_lang$core$Json_Decode$decodeString = _elm_lang$core$Native_Json.runOnString;
var _elm_lang$core$Json_Decode$map8 = _elm_lang$core$Native_Json.map8;
var _elm_lang$core$Json_Decode$map7 = _elm_lang$core$Native_Json.map7;
var _elm_lang$core$Json_Decode$map6 = _elm_lang$core$Native_Json.map6;
var _elm_lang$core$Json_Decode$map5 = _elm_lang$core$Native_Json.map5;
var _elm_lang$core$Json_Decode$map4 = _elm_lang$core$Native_Json.map4;
var _elm_lang$core$Json_Decode$map3 = _elm_lang$core$Native_Json.map3;
var _elm_lang$core$Json_Decode$map2 = _elm_lang$core$Native_Json.map2;
var _elm_lang$core$Json_Decode$map = _elm_lang$core$Native_Json.map1;
var _elm_lang$core$Json_Decode$oneOf = _elm_lang$core$Native_Json.oneOf;
var _elm_lang$core$Json_Decode$maybe = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'maybe', decoder);
};
var _elm_lang$core$Json_Decode$index = _elm_lang$core$Native_Json.decodeIndex;
var _elm_lang$core$Json_Decode$field = _elm_lang$core$Native_Json.decodeField;
var _elm_lang$core$Json_Decode$at = F2(
	function (fields, decoder) {
		return A3(_elm_lang$core$List$foldr, _elm_lang$core$Json_Decode$field, decoder, fields);
	});
var _elm_lang$core$Json_Decode$keyValuePairs = _elm_lang$core$Native_Json.decodeKeyValuePairs;
var _elm_lang$core$Json_Decode$dict = function (decoder) {
	return A2(
		_elm_lang$core$Json_Decode$map,
		_elm_lang$core$Dict$fromList,
		_elm_lang$core$Json_Decode$keyValuePairs(decoder));
};
var _elm_lang$core$Json_Decode$array = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'array', decoder);
};
var _elm_lang$core$Json_Decode$list = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'list', decoder);
};
var _elm_lang$core$Json_Decode$nullable = function (decoder) {
	return _elm_lang$core$Json_Decode$oneOf(
		{
			ctor: '::',
			_0: _elm_lang$core$Json_Decode$null(_elm_lang$core$Maybe$Nothing),
			_1: {
				ctor: '::',
				_0: A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Maybe$Just, decoder),
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$core$Json_Decode$float = _elm_lang$core$Native_Json.decodePrimitive('float');
var _elm_lang$core$Json_Decode$int = _elm_lang$core$Native_Json.decodePrimitive('int');
var _elm_lang$core$Json_Decode$bool = _elm_lang$core$Native_Json.decodePrimitive('bool');
var _elm_lang$core$Json_Decode$string = _elm_lang$core$Native_Json.decodePrimitive('string');
var _elm_lang$core$Json_Decode$Decoder = {ctor: 'Decoder'};

var _elm_lang$virtual_dom$VirtualDom_Debug$wrap;
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags;

var _elm_lang$virtual_dom$Native_VirtualDom = function() {

var STYLE_KEY = 'STYLE';
var EVENT_KEY = 'EVENT';
var ATTR_KEY = 'ATTR';
var ATTR_NS_KEY = 'ATTR_NS';

var localDoc = typeof document !== 'undefined' ? document : {};


////////////  VIRTUAL DOM NODES  ////////////


function text(string)
{
	return {
		type: 'text',
		text: string
	};
}


function node(tag)
{
	return F2(function(factList, kidList) {
		return nodeHelp(tag, factList, kidList);
	});
}


function nodeHelp(tag, factList, kidList)
{
	var organized = organizeFacts(factList);
	var namespace = organized.namespace;
	var facts = organized.facts;

	var children = [];
	var descendantsCount = 0;
	while (kidList.ctor !== '[]')
	{
		var kid = kidList._0;
		descendantsCount += (kid.descendantsCount || 0);
		children.push(kid);
		kidList = kidList._1;
	}
	descendantsCount += children.length;

	return {
		type: 'node',
		tag: tag,
		facts: facts,
		children: children,
		namespace: namespace,
		descendantsCount: descendantsCount
	};
}


function keyedNode(tag, factList, kidList)
{
	var organized = organizeFacts(factList);
	var namespace = organized.namespace;
	var facts = organized.facts;

	var children = [];
	var descendantsCount = 0;
	while (kidList.ctor !== '[]')
	{
		var kid = kidList._0;
		descendantsCount += (kid._1.descendantsCount || 0);
		children.push(kid);
		kidList = kidList._1;
	}
	descendantsCount += children.length;

	return {
		type: 'keyed-node',
		tag: tag,
		facts: facts,
		children: children,
		namespace: namespace,
		descendantsCount: descendantsCount
	};
}


function custom(factList, model, impl)
{
	var facts = organizeFacts(factList).facts;

	return {
		type: 'custom',
		facts: facts,
		model: model,
		impl: impl
	};
}


function map(tagger, node)
{
	return {
		type: 'tagger',
		tagger: tagger,
		node: node,
		descendantsCount: 1 + (node.descendantsCount || 0)
	};
}


function thunk(func, args, thunk)
{
	return {
		type: 'thunk',
		func: func,
		args: args,
		thunk: thunk,
		node: undefined
	};
}

function lazy(fn, a)
{
	return thunk(fn, [a], function() {
		return fn(a);
	});
}

function lazy2(fn, a, b)
{
	return thunk(fn, [a,b], function() {
		return A2(fn, a, b);
	});
}

function lazy3(fn, a, b, c)
{
	return thunk(fn, [a,b,c], function() {
		return A3(fn, a, b, c);
	});
}



// FACTS


function organizeFacts(factList)
{
	var namespace, facts = {};

	while (factList.ctor !== '[]')
	{
		var entry = factList._0;
		var key = entry.key;

		if (key === ATTR_KEY || key === ATTR_NS_KEY || key === EVENT_KEY)
		{
			var subFacts = facts[key] || {};
			subFacts[entry.realKey] = entry.value;
			facts[key] = subFacts;
		}
		else if (key === STYLE_KEY)
		{
			var styles = facts[key] || {};
			var styleList = entry.value;
			while (styleList.ctor !== '[]')
			{
				var style = styleList._0;
				styles[style._0] = style._1;
				styleList = styleList._1;
			}
			facts[key] = styles;
		}
		else if (key === 'namespace')
		{
			namespace = entry.value;
		}
		else if (key === 'className')
		{
			var classes = facts[key];
			facts[key] = typeof classes === 'undefined'
				? entry.value
				: classes + ' ' + entry.value;
		}
 		else
		{
			facts[key] = entry.value;
		}
		factList = factList._1;
	}

	return {
		facts: facts,
		namespace: namespace
	};
}



////////////  PROPERTIES AND ATTRIBUTES  ////////////


function style(value)
{
	return {
		key: STYLE_KEY,
		value: value
	};
}


function property(key, value)
{
	return {
		key: key,
		value: value
	};
}


function attribute(key, value)
{
	return {
		key: ATTR_KEY,
		realKey: key,
		value: value
	};
}


function attributeNS(namespace, key, value)
{
	return {
		key: ATTR_NS_KEY,
		realKey: key,
		value: {
			value: value,
			namespace: namespace
		}
	};
}


function on(name, options, decoder)
{
	return {
		key: EVENT_KEY,
		realKey: name,
		value: {
			options: options,
			decoder: decoder
		}
	};
}


function equalEvents(a, b)
{
	if (a.options !== b.options)
	{
		if (a.options.stopPropagation !== b.options.stopPropagation || a.options.preventDefault !== b.options.preventDefault)
		{
			return false;
		}
	}
	return _elm_lang$core$Native_Json.equality(a.decoder, b.decoder);
}


function mapProperty(func, property)
{
	if (property.key !== EVENT_KEY)
	{
		return property;
	}
	return on(
		property.realKey,
		property.value.options,
		A2(_elm_lang$core$Json_Decode$map, func, property.value.decoder)
	);
}


////////////  RENDER  ////////////


function render(vNode, eventNode)
{
	switch (vNode.type)
	{
		case 'thunk':
			if (!vNode.node)
			{
				vNode.node = vNode.thunk();
			}
			return render(vNode.node, eventNode);

		case 'tagger':
			var subNode = vNode.node;
			var tagger = vNode.tagger;

			while (subNode.type === 'tagger')
			{
				typeof tagger !== 'object'
					? tagger = [tagger, subNode.tagger]
					: tagger.push(subNode.tagger);

				subNode = subNode.node;
			}

			var subEventRoot = { tagger: tagger, parent: eventNode };
			var domNode = render(subNode, subEventRoot);
			domNode.elm_event_node_ref = subEventRoot;
			return domNode;

		case 'text':
			return localDoc.createTextNode(vNode.text);

		case 'node':
			var domNode = vNode.namespace
				? localDoc.createElementNS(vNode.namespace, vNode.tag)
				: localDoc.createElement(vNode.tag);

			applyFacts(domNode, eventNode, vNode.facts);

			var children = vNode.children;

			for (var i = 0; i < children.length; i++)
			{
				domNode.appendChild(render(children[i], eventNode));
			}

			return domNode;

		case 'keyed-node':
			var domNode = vNode.namespace
				? localDoc.createElementNS(vNode.namespace, vNode.tag)
				: localDoc.createElement(vNode.tag);

			applyFacts(domNode, eventNode, vNode.facts);

			var children = vNode.children;

			for (var i = 0; i < children.length; i++)
			{
				domNode.appendChild(render(children[i]._1, eventNode));
			}

			return domNode;

		case 'custom':
			var domNode = vNode.impl.render(vNode.model);
			applyFacts(domNode, eventNode, vNode.facts);
			return domNode;
	}
}



////////////  APPLY FACTS  ////////////


function applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		switch (key)
		{
			case STYLE_KEY:
				applyStyles(domNode, value);
				break;

			case EVENT_KEY:
				applyEvents(domNode, eventNode, value);
				break;

			case ATTR_KEY:
				applyAttrs(domNode, value);
				break;

			case ATTR_NS_KEY:
				applyAttrsNS(domNode, value);
				break;

			case 'value':
				if (domNode[key] !== value)
				{
					domNode[key] = value;
				}
				break;

			default:
				domNode[key] = value;
				break;
		}
	}
}

function applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}

function applyEvents(domNode, eventNode, events)
{
	var allHandlers = domNode.elm_handlers || {};

	for (var key in events)
	{
		var handler = allHandlers[key];
		var value = events[key];

		if (typeof value === 'undefined')
		{
			domNode.removeEventListener(key, handler);
			allHandlers[key] = undefined;
		}
		else if (typeof handler === 'undefined')
		{
			var handler = makeEventHandler(eventNode, value);
			domNode.addEventListener(key, handler);
			allHandlers[key] = handler;
		}
		else
		{
			handler.info = value;
		}
	}

	domNode.elm_handlers = allHandlers;
}

function makeEventHandler(eventNode, info)
{
	function eventHandler(event)
	{
		var info = eventHandler.info;

		var value = A2(_elm_lang$core$Native_Json.run, info.decoder, event);

		if (value.ctor === 'Ok')
		{
			var options = info.options;
			if (options.stopPropagation)
			{
				event.stopPropagation();
			}
			if (options.preventDefault)
			{
				event.preventDefault();
			}

			var message = value._0;

			var currentEventNode = eventNode;
			while (currentEventNode)
			{
				var tagger = currentEventNode.tagger;
				if (typeof tagger === 'function')
				{
					message = tagger(message);
				}
				else
				{
					for (var i = tagger.length; i--; )
					{
						message = tagger[i](message);
					}
				}
				currentEventNode = currentEventNode.parent;
			}
		}
	};

	eventHandler.info = info;

	return eventHandler;
}

function applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		if (typeof value === 'undefined')
		{
			domNode.removeAttribute(key);
		}
		else
		{
			domNode.setAttribute(key, value);
		}
	}
}

function applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.namespace;
		var value = pair.value;

		if (typeof value === 'undefined')
		{
			domNode.removeAttributeNS(namespace, key);
		}
		else
		{
			domNode.setAttributeNS(namespace, key, value);
		}
	}
}



////////////  DIFF  ////////////


function diff(a, b)
{
	var patches = [];
	diffHelp(a, b, patches, 0);
	return patches;
}


function makePatch(type, index, data)
{
	return {
		index: index,
		type: type,
		data: data,
		domNode: undefined,
		eventNode: undefined
	};
}


function diffHelp(a, b, patches, index)
{
	if (a === b)
	{
		return;
	}

	var aType = a.type;
	var bType = b.type;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (aType !== bType)
	{
		patches.push(makePatch('p-redraw', index, b));
		return;
	}

	// Now we know that both nodes are the same type.
	switch (bType)
	{
		case 'thunk':
			var aArgs = a.args;
			var bArgs = b.args;
			var i = aArgs.length;
			var same = a.func === b.func && i === bArgs.length;
			while (same && i--)
			{
				same = aArgs[i] === bArgs[i];
			}
			if (same)
			{
				b.node = a.node;
				return;
			}
			b.node = b.thunk();
			var subPatches = [];
			diffHelp(a.node, b.node, subPatches, 0);
			if (subPatches.length > 0)
			{
				patches.push(makePatch('p-thunk', index, subPatches));
			}
			return;

		case 'tagger':
			// gather nested taggers
			var aTaggers = a.tagger;
			var bTaggers = b.tagger;
			var nesting = false;

			var aSubNode = a.node;
			while (aSubNode.type === 'tagger')
			{
				nesting = true;

				typeof aTaggers !== 'object'
					? aTaggers = [aTaggers, aSubNode.tagger]
					: aTaggers.push(aSubNode.tagger);

				aSubNode = aSubNode.node;
			}

			var bSubNode = b.node;
			while (bSubNode.type === 'tagger')
			{
				nesting = true;

				typeof bTaggers !== 'object'
					? bTaggers = [bTaggers, bSubNode.tagger]
					: bTaggers.push(bSubNode.tagger);

				bSubNode = bSubNode.node;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && aTaggers.length !== bTaggers.length)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !pairwiseRefEqual(aTaggers, bTaggers) : aTaggers !== bTaggers)
			{
				patches.push(makePatch('p-tagger', index, bTaggers));
			}

			// diff everything below the taggers
			diffHelp(aSubNode, bSubNode, patches, index + 1);
			return;

		case 'text':
			if (a.text !== b.text)
			{
				patches.push(makePatch('p-text', index, b.text));
				return;
			}

			return;

		case 'node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffChildren(a, b, patches, index);
			return;

		case 'keyed-node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffKeyedChildren(a, b, patches, index);
			return;

		case 'custom':
			if (a.impl !== b.impl)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);
			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			var patch = b.impl.diff(a,b);
			if (patch)
			{
				patches.push(makePatch('p-custom', index, patch));
				return;
			}

			return;
	}
}


// assumes the incoming arrays are the same length
function pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function diffFacts(a, b, category)
{
	var diff;

	// look for changes and removals
	for (var aKey in a)
	{
		if (aKey === STYLE_KEY || aKey === EVENT_KEY || aKey === ATTR_KEY || aKey === ATTR_NS_KEY)
		{
			var subDiff = diffFacts(a[aKey], b[aKey] || {}, aKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[aKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(aKey in b))
		{
			diff = diff || {};
			diff[aKey] =
				(typeof category === 'undefined')
					? (typeof a[aKey] === 'string' ? '' : null)
					:
				(category === STYLE_KEY)
					? ''
					:
				(category === EVENT_KEY || category === ATTR_KEY)
					? undefined
					:
				{ namespace: a[aKey].namespace, value: undefined };

			continue;
		}

		var aValue = a[aKey];
		var bValue = b[aKey];

		// reference equal, so don't worry about it
		if (aValue === bValue && aKey !== 'value'
			|| category === EVENT_KEY && equalEvents(aValue, bValue))
		{
			continue;
		}

		diff = diff || {};
		diff[aKey] = bValue;
	}

	// add new stuff
	for (var bKey in b)
	{
		if (!(bKey in a))
		{
			diff = diff || {};
			diff[bKey] = b[bKey];
		}
	}

	return diff;
}


function diffChildren(aParent, bParent, patches, rootIndex)
{
	var aChildren = aParent.children;
	var bChildren = bParent.children;

	var aLen = aChildren.length;
	var bLen = bChildren.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (aLen > bLen)
	{
		patches.push(makePatch('p-remove-last', rootIndex, aLen - bLen));
	}
	else if (aLen < bLen)
	{
		patches.push(makePatch('p-append', rootIndex, bChildren.slice(aLen)));
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	var index = rootIndex;
	var minLen = aLen < bLen ? aLen : bLen;
	for (var i = 0; i < minLen; i++)
	{
		index++;
		var aChild = aChildren[i];
		diffHelp(aChild, bChildren[i], patches, index);
		index += aChild.descendantsCount || 0;
	}
}



////////////  KEYED DIFF  ////////////


function diffKeyedChildren(aParent, bParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var aChildren = aParent.children;
	var bChildren = bParent.children;
	var aLen = aChildren.length;
	var bLen = bChildren.length;
	var aIndex = 0;
	var bIndex = 0;

	var index = rootIndex;

	while (aIndex < aLen && bIndex < bLen)
	{
		var a = aChildren[aIndex];
		var b = bChildren[bIndex];

		var aKey = a._0;
		var bKey = b._0;
		var aNode = a._1;
		var bNode = b._1;

		// check if keys match

		if (aKey === bKey)
		{
			index++;
			diffHelp(aNode, bNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex++;
			bIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var aLookAhead = aIndex + 1 < aLen;
		var bLookAhead = bIndex + 1 < bLen;

		if (aLookAhead)
		{
			var aNext = aChildren[aIndex + 1];
			var aNextKey = aNext._0;
			var aNextNode = aNext._1;
			var oldMatch = bKey === aNextKey;
		}

		if (bLookAhead)
		{
			var bNext = bChildren[bIndex + 1];
			var bNextKey = bNext._0;
			var bNextNode = bNext._1;
			var newMatch = aKey === bNextKey;
		}


		// swap a and b
		if (aLookAhead && bLookAhead && newMatch && oldMatch)
		{
			index++;
			diffHelp(aNode, bNextNode, localPatches, index);
			insertNode(changes, localPatches, aKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			removeNode(changes, localPatches, aKey, aNextNode, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		// insert b
		if (bLookAhead && newMatch)
		{
			index++;
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			diffHelp(aNode, bNextNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex += 1;
			bIndex += 2;
			continue;
		}

		// remove a
		if (aLookAhead && oldMatch)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 1;
			continue;
		}

		// remove a, insert b
		if (aLookAhead && bLookAhead && aNextKey === bNextKey)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNextNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (aIndex < aLen)
	{
		index++;
		var a = aChildren[aIndex];
		var aNode = a._1;
		removeNode(changes, localPatches, a._0, aNode, index);
		index += aNode.descendantsCount || 0;
		aIndex++;
	}

	var endInserts;
	while (bIndex < bLen)
	{
		endInserts = endInserts || [];
		var b = bChildren[bIndex];
		insertNode(changes, localPatches, b._0, b._1, undefined, endInserts);
		bIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || typeof endInserts !== 'undefined')
	{
		patches.push(makePatch('p-reorder', rootIndex, {
			patches: localPatches,
			inserts: inserts,
			endInserts: endInserts
		}));
	}
}



////////////  CHANGES FROM KEYED DIFF  ////////////


var POSTFIX = '_elmW6BL';


function insertNode(changes, localPatches, key, vnode, bIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		entry = {
			tag: 'insert',
			vnode: vnode,
			index: bIndex,
			data: undefined
		};

		inserts.push({ index: bIndex, entry: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.tag === 'remove')
	{
		inserts.push({ index: bIndex, entry: entry });

		entry.tag = 'move';
		var subPatches = [];
		diffHelp(entry.vnode, vnode, subPatches, entry.index);
		entry.index = bIndex;
		entry.data.data = {
			patches: subPatches,
			entry: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	insertNode(changes, localPatches, key + POSTFIX, vnode, bIndex, inserts);
}


function removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		var patch = makePatch('p-remove', index, undefined);
		localPatches.push(patch);

		changes[key] = {
			tag: 'remove',
			vnode: vnode,
			index: index,
			data: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.tag === 'insert')
	{
		entry.tag = 'move';
		var subPatches = [];
		diffHelp(vnode, entry.vnode, subPatches, index);

		var patch = makePatch('p-remove', index, {
			patches: subPatches,
			entry: entry
		});
		localPatches.push(patch);

		return;
	}

	// this key has already been removed or moved, a duplicate!
	removeNode(changes, localPatches, key + POSTFIX, vnode, index);
}



////////////  ADD DOM NODES  ////////////
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function addDomNodes(domNode, vNode, patches, eventNode)
{
	addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.descendantsCount, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.index;

	while (index === low)
	{
		var patchType = patch.type;

		if (patchType === 'p-thunk')
		{
			addDomNodes(domNode, vNode.node, patch.data, eventNode);
		}
		else if (patchType === 'p-reorder')
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var subPatches = patch.data.patches;
			if (subPatches.length > 0)
			{
				addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 'p-remove')
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var data = patch.data;
			if (typeof data !== 'undefined')
			{
				data.entry.data = domNode;
				var subPatches = data.patches;
				if (subPatches.length > 0)
				{
					addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.index) > high)
		{
			return i;
		}
	}

	switch (vNode.type)
	{
		case 'tagger':
			var subNode = vNode.node;

			while (subNode.type === "tagger")
			{
				subNode = subNode.node;
			}

			return addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);

		case 'node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j];
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'keyed-node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j]._1;
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'text':
		case 'thunk':
			throw new Error('should never traverse `text` or `thunk` nodes like this');
	}
}



////////////  APPLY PATCHES  ////////////


function applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return applyPatchesHelp(rootDomNode, patches);
}

function applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.domNode
		var newNode = applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function applyPatch(domNode, patch)
{
	switch (patch.type)
	{
		case 'p-redraw':
			return applyPatchRedraw(domNode, patch.data, patch.eventNode);

		case 'p-facts':
			applyFacts(domNode, patch.eventNode, patch.data);
			return domNode;

		case 'p-text':
			domNode.replaceData(0, domNode.length, patch.data);
			return domNode;

		case 'p-thunk':
			return applyPatchesHelp(domNode, patch.data);

		case 'p-tagger':
			if (typeof domNode.elm_event_node_ref !== 'undefined')
			{
				domNode.elm_event_node_ref.tagger = patch.data;
			}
			else
			{
				domNode.elm_event_node_ref = { tagger: patch.data, parent: patch.eventNode };
			}
			return domNode;

		case 'p-remove-last':
			var i = patch.data;
			while (i--)
			{
				domNode.removeChild(domNode.lastChild);
			}
			return domNode;

		case 'p-append':
			var newNodes = patch.data;
			for (var i = 0; i < newNodes.length; i++)
			{
				domNode.appendChild(render(newNodes[i], patch.eventNode));
			}
			return domNode;

		case 'p-remove':
			var data = patch.data;
			if (typeof data === 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.entry;
			if (typeof entry.index !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.data = applyPatchesHelp(domNode, data.patches);
			return domNode;

		case 'p-reorder':
			return applyPatchReorder(domNode, patch);

		case 'p-custom':
			var impl = patch.data;
			return impl.applyPatch(domNode, impl.data);

		default:
			throw new Error('Ran into an unknown patch!');
	}
}


function applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = render(vNode, eventNode);

	if (typeof newNode.elm_event_node_ref === 'undefined')
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function applyPatchReorder(domNode, patch)
{
	var data = patch.data;

	// remove end inserts
	var frag = applyPatchReorderEndInsertsHelp(data.endInserts, patch);

	// removals
	domNode = applyPatchesHelp(domNode, data.patches);

	// inserts
	var inserts = data.inserts;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.entry;
		var node = entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode);
		domNode.insertBefore(node, domNode.childNodes[insert.index]);
	}

	// add end inserts
	if (typeof frag !== 'undefined')
	{
		domNode.appendChild(frag);
	}

	return domNode;
}


function applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (typeof endInserts === 'undefined')
	{
		return;
	}

	var frag = localDoc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.entry;
		frag.appendChild(entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode)
		);
	}
	return frag;
}


// PROGRAMS

var program = makeProgram(checkNoFlags);
var programWithFlags = makeProgram(checkYesFlags);

function makeProgram(flagChecker)
{
	return F2(function(debugWrap, impl)
	{
		return function(flagDecoder)
		{
			return function(object, moduleName, debugMetadata)
			{
				var checker = flagChecker(flagDecoder, moduleName);
				if (typeof debugMetadata === 'undefined')
				{
					normalSetup(impl, object, moduleName, checker);
				}
				else
				{
					debugSetup(A2(debugWrap, debugMetadata, impl), object, moduleName, checker);
				}
			};
		};
	});
}

function staticProgram(vNode)
{
	var nothing = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		_elm_lang$core$Platform_Cmd$none
	);
	return A2(program, _elm_lang$virtual_dom$VirtualDom_Debug$wrap, {
		init: nothing,
		view: function() { return vNode; },
		update: F2(function() { return nothing; }),
		subscriptions: function() { return _elm_lang$core$Platform_Sub$none; }
	})();
}


// FLAG CHECKERS

function checkNoFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flags === 'undefined')
		{
			return init;
		}

		var errorMessage =
			'The `' + moduleName + '` module does not need flags.\n'
			+ 'Initialize it with no arguments and you should be all set!';

		crash(errorMessage, domNode);
	};
}

function checkYesFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flagDecoder === 'undefined')
		{
			var errorMessage =
				'Are you trying to sneak a Never value into Elm? Trickster!\n'
				+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
				+ 'Use `program` instead if you do not want flags.'

			crash(errorMessage, domNode);
		}

		var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
		if (result.ctor === 'Ok')
		{
			return init(result._0);
		}

		var errorMessage =
			'Trying to initialize the `' + moduleName + '` module with an unexpected flag.\n'
			+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
			+ result._0;

		crash(errorMessage, domNode);
	};
}

function crash(errorMessage, domNode)
{
	if (domNode)
	{
		domNode.innerHTML =
			'<div style="padding-left:1em;">'
			+ '<h2 style="font-weight:normal;"><b>Oops!</b> Something went wrong when starting your Elm program.</h2>'
			+ '<pre style="padding-left:1em;">' + errorMessage + '</pre>'
			+ '</div>';
	}

	throw new Error(errorMessage);
}


//  NORMAL SETUP

function normalSetup(impl, object, moduleName, flagChecker)
{
	object['embed'] = function embed(node, flags)
	{
		while (node.lastChild)
		{
			node.removeChild(node.lastChild);
		}

		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, node),
			impl.update,
			impl.subscriptions,
			normalRenderer(node, impl.view)
		);
	};

	object['fullscreen'] = function fullscreen(flags)
	{
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update,
			impl.subscriptions,
			normalRenderer(document.body, impl.view)
		);
	};
}

function normalRenderer(parentNode, view)
{
	return function(tagger, initialModel)
	{
		var eventNode = { tagger: tagger, parent: undefined };
		var initialVirtualNode = view(initialModel);
		var domNode = render(initialVirtualNode, eventNode);
		parentNode.appendChild(domNode);
		return makeStepper(domNode, view, initialVirtualNode, eventNode);
	};
}


// STEPPER

var rAF =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { setTimeout(callback, 1000 / 60); };

function makeStepper(domNode, view, initialVirtualNode, eventNode)
{
	var state = 'NO_REQUEST';
	var currNode = initialVirtualNode;
	var nextModel;

	function updateIfNeeded()
	{
		switch (state)
		{
			case 'NO_REQUEST':
				throw new Error(
					'Unexpected draw callback.\n' +
					'Please report this to <https://github.com/elm-lang/virtual-dom/issues>.'
				);

			case 'PENDING_REQUEST':
				rAF(updateIfNeeded);
				state = 'EXTRA_REQUEST';

				var nextNode = view(nextModel);
				var patches = diff(currNode, nextNode);
				domNode = applyPatches(domNode, currNode, patches, eventNode);
				currNode = nextNode;

				return;

			case 'EXTRA_REQUEST':
				state = 'NO_REQUEST';
				return;
		}
	}

	return function stepper(model)
	{
		if (state === 'NO_REQUEST')
		{
			rAF(updateIfNeeded);
		}
		state = 'PENDING_REQUEST';
		nextModel = model;
	};
}


// DEBUG SETUP

function debugSetup(impl, object, moduleName, flagChecker)
{
	object['fullscreen'] = function fullscreen(flags)
	{
		var popoutRef = { doc: undefined };
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update(scrollTask(popoutRef)),
			impl.subscriptions,
			debugRenderer(moduleName, document.body, popoutRef, impl.view, impl.viewIn, impl.viewOut)
		);
	};

	object['embed'] = function fullscreen(node, flags)
	{
		var popoutRef = { doc: undefined };
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, node),
			impl.update(scrollTask(popoutRef)),
			impl.subscriptions,
			debugRenderer(moduleName, node, popoutRef, impl.view, impl.viewIn, impl.viewOut)
		);
	};
}

function scrollTask(popoutRef)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var doc = popoutRef.doc;
		if (doc)
		{
			var msgs = doc.getElementsByClassName('debugger-sidebar-messages')[0];
			if (msgs)
			{
				msgs.scrollTop = msgs.scrollHeight;
			}
		}
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}


function debugRenderer(moduleName, parentNode, popoutRef, view, viewIn, viewOut)
{
	return function(tagger, initialModel)
	{
		var appEventNode = { tagger: tagger, parent: undefined };
		var eventNode = { tagger: tagger, parent: undefined };

		// make normal stepper
		var appVirtualNode = view(initialModel);
		var appNode = render(appVirtualNode, appEventNode);
		parentNode.appendChild(appNode);
		var appStepper = makeStepper(appNode, view, appVirtualNode, appEventNode);

		// make overlay stepper
		var overVirtualNode = viewIn(initialModel)._1;
		var overNode = render(overVirtualNode, eventNode);
		parentNode.appendChild(overNode);
		var wrappedViewIn = wrapViewIn(appEventNode, overNode, viewIn);
		var overStepper = makeStepper(overNode, wrappedViewIn, overVirtualNode, eventNode);

		// make debugger stepper
		var debugStepper = makeDebugStepper(initialModel, viewOut, eventNode, parentNode, moduleName, popoutRef);

		return function stepper(model)
		{
			appStepper(model);
			overStepper(model);
			debugStepper(model);
		}
	};
}

function makeDebugStepper(initialModel, view, eventNode, parentNode, moduleName, popoutRef)
{
	var curr;
	var domNode;

	return function stepper(model)
	{
		if (!model.isDebuggerOpen)
		{
			return;
		}

		if (!popoutRef.doc)
		{
			curr = view(model);
			domNode = openDebugWindow(moduleName, popoutRef, curr, eventNode);
			return;
		}

		// switch to document of popout
		localDoc = popoutRef.doc;

		var next = view(model);
		var patches = diff(curr, next);
		domNode = applyPatches(domNode, curr, patches, eventNode);
		curr = next;

		// switch back to normal document
		localDoc = document;
	};
}

function openDebugWindow(moduleName, popoutRef, virtualNode, eventNode)
{
	var w = 900;
	var h = 360;
	var x = screen.width - w;
	var y = screen.height - h;
	var debugWindow = window.open('', '', 'width=' + w + ',height=' + h + ',left=' + x + ',top=' + y);

	// switch to window document
	localDoc = debugWindow.document;

	popoutRef.doc = localDoc;
	localDoc.title = 'Debugger - ' + moduleName;
	localDoc.body.style.margin = '0';
	localDoc.body.style.padding = '0';
	var domNode = render(virtualNode, eventNode);
	localDoc.body.appendChild(domNode);

	localDoc.addEventListener('keydown', function(event) {
		if (event.metaKey && event.which === 82)
		{
			window.location.reload();
		}
		if (event.which === 38)
		{
			eventNode.tagger({ ctor: 'Up' });
			event.preventDefault();
		}
		if (event.which === 40)
		{
			eventNode.tagger({ ctor: 'Down' });
			event.preventDefault();
		}
	});

	function close()
	{
		popoutRef.doc = undefined;
		debugWindow.close();
	}
	window.addEventListener('unload', close);
	debugWindow.addEventListener('unload', function() {
		popoutRef.doc = undefined;
		window.removeEventListener('unload', close);
		eventNode.tagger({ ctor: 'Close' });
	});

	// switch back to the normal document
	localDoc = document;

	return domNode;
}


// BLOCK EVENTS

function wrapViewIn(appEventNode, overlayNode, viewIn)
{
	var ignorer = makeIgnorer(overlayNode);
	var blocking = 'Normal';
	var overflow;

	var normalTagger = appEventNode.tagger;
	var blockTagger = function() {};

	return function(model)
	{
		var tuple = viewIn(model);
		var newBlocking = tuple._0.ctor;
		appEventNode.tagger = newBlocking === 'Normal' ? normalTagger : blockTagger;
		if (blocking !== newBlocking)
		{
			traverse('removeEventListener', ignorer, blocking);
			traverse('addEventListener', ignorer, newBlocking);

			if (blocking === 'Normal')
			{
				overflow = document.body.style.overflow;
				document.body.style.overflow = 'hidden';
			}

			if (newBlocking === 'Normal')
			{
				document.body.style.overflow = overflow;
			}

			blocking = newBlocking;
		}
		return tuple._1;
	}
}

function traverse(verbEventListener, ignorer, blocking)
{
	switch(blocking)
	{
		case 'Normal':
			return;

		case 'Pause':
			return traverseHelp(verbEventListener, ignorer, mostEvents);

		case 'Message':
			return traverseHelp(verbEventListener, ignorer, allEvents);
	}
}

function traverseHelp(verbEventListener, handler, eventNames)
{
	for (var i = 0; i < eventNames.length; i++)
	{
		document.body[verbEventListener](eventNames[i], handler, true);
	}
}

function makeIgnorer(overlayNode)
{
	return function(event)
	{
		if (event.type === 'keydown' && event.metaKey && event.which === 82)
		{
			return;
		}

		var isScroll = event.type === 'scroll' || event.type === 'wheel';

		var node = event.target;
		while (node !== null)
		{
			if (node.className === 'elm-overlay-message-details' && isScroll)
			{
				return;
			}

			if (node === overlayNode && !isScroll)
			{
				return;
			}
			node = node.parentNode;
		}

		event.stopPropagation();
		event.preventDefault();
	}
}

var mostEvents = [
	'click', 'dblclick', 'mousemove',
	'mouseup', 'mousedown', 'mouseenter', 'mouseleave',
	'touchstart', 'touchend', 'touchcancel', 'touchmove',
	'pointerdown', 'pointerup', 'pointerover', 'pointerout',
	'pointerenter', 'pointerleave', 'pointermove', 'pointercancel',
	'dragstart', 'drag', 'dragend', 'dragenter', 'dragover', 'dragleave', 'drop',
	'keyup', 'keydown', 'keypress',
	'input', 'change',
	'focus', 'blur'
];

var allEvents = mostEvents.concat('wheel', 'scroll');


return {
	node: node,
	text: text,
	custom: custom,
	map: F2(map),

	on: F3(on),
	style: style,
	property: F2(property),
	attribute: F2(attribute),
	attributeNS: F3(attributeNS),
	mapProperty: F2(mapProperty),

	lazy: F2(lazy),
	lazy2: F3(lazy2),
	lazy3: F4(lazy3),
	keyedNode: F3(keyedNode),

	program: program,
	programWithFlags: programWithFlags,
	staticProgram: staticProgram
};

}();

var _elm_lang$core$Debug$crash = _elm_lang$core$Native_Debug.crash;
var _elm_lang$core$Debug$log = _elm_lang$core$Native_Debug.log;

var _elm_lang$core$Tuple$mapSecond = F2(
	function (func, _p0) {
		var _p1 = _p0;
		return {
			ctor: '_Tuple2',
			_0: _p1._0,
			_1: func(_p1._1)
		};
	});
var _elm_lang$core$Tuple$mapFirst = F2(
	function (func, _p2) {
		var _p3 = _p2;
		return {
			ctor: '_Tuple2',
			_0: func(_p3._0),
			_1: _p3._1
		};
	});
var _elm_lang$core$Tuple$second = function (_p4) {
	var _p5 = _p4;
	return _p5._1;
};
var _elm_lang$core$Tuple$first = function (_p6) {
	var _p7 = _p6;
	return _p7._0;
};

//import //

var _elm_lang$core$Native_Platform = function() {


// PROGRAMS

function program(impl)
{
	return function(flagDecoder)
	{
		return function(object, moduleName)
		{
			object['worker'] = function worker(flags)
			{
				if (typeof flags !== 'undefined')
				{
					throw new Error(
						'The `' + moduleName + '` module does not need flags.\n'
						+ 'Call ' + moduleName + '.worker() with no arguments and you should be all set!'
					);
				}

				return initialize(
					impl.init,
					impl.update,
					impl.subscriptions,
					renderer
				);
			};
		};
	};
}

function programWithFlags(impl)
{
	return function(flagDecoder)
	{
		return function(object, moduleName)
		{
			object['worker'] = function worker(flags)
			{
				if (typeof flagDecoder === 'undefined')
				{
					throw new Error(
						'Are you trying to sneak a Never value into Elm? Trickster!\n'
						+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
						+ 'Use `program` instead if you do not want flags.'
					);
				}

				var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
				if (result.ctor === 'Err')
				{
					throw new Error(
						moduleName + '.worker(...) was called with an unexpected argument.\n'
						+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
						+ result._0
					);
				}

				return initialize(
					impl.init(result._0),
					impl.update,
					impl.subscriptions,
					renderer
				);
			};
		};
	};
}

function renderer(enqueue, _)
{
	return function(_) {};
}


// HTML TO PROGRAM

function htmlToProgram(vnode)
{
	var emptyBag = batch(_elm_lang$core$Native_List.Nil);
	var noChange = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		emptyBag
	);

	return _elm_lang$virtual_dom$VirtualDom$program({
		init: noChange,
		view: function(model) { return main; },
		update: F2(function(msg, model) { return noChange; }),
		subscriptions: function (model) { return emptyBag; }
	});
}


// INITIALIZE A PROGRAM

function initialize(init, update, subscriptions, renderer)
{
	// ambient state
	var managers = {};
	var updateView;

	// init and update state in main process
	var initApp = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
		var model = init._0;
		updateView = renderer(enqueue, model);
		var cmds = init._1;
		var subs = subscriptions(model);
		dispatchEffects(managers, cmds, subs);
		callback(_elm_lang$core$Native_Scheduler.succeed(model));
	});

	function onMessage(msg, model)
	{
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
			var results = A2(update, msg, model);
			model = results._0;
			updateView(model);
			var cmds = results._1;
			var subs = subscriptions(model);
			dispatchEffects(managers, cmds, subs);
			callback(_elm_lang$core$Native_Scheduler.succeed(model));
		});
	}

	var mainProcess = spawnLoop(initApp, onMessage);

	function enqueue(msg)
	{
		_elm_lang$core$Native_Scheduler.rawSend(mainProcess, msg);
	}

	var ports = setupEffects(managers, enqueue);

	return ports ? { ports: ports } : {};
}


// EFFECT MANAGERS

var effectManagers = {};

function setupEffects(managers, callback)
{
	var ports;

	// setup all necessary effect managers
	for (var key in effectManagers)
	{
		var manager = effectManagers[key];

		if (manager.isForeign)
		{
			ports = ports || {};
			ports[key] = manager.tag === 'cmd'
				? setupOutgoingPort(key)
				: setupIncomingPort(key, callback);
		}

		managers[key] = makeManager(manager, callback);
	}

	return ports;
}

function makeManager(info, callback)
{
	var router = {
		main: callback,
		self: undefined
	};

	var tag = info.tag;
	var onEffects = info.onEffects;
	var onSelfMsg = info.onSelfMsg;

	function onMessage(msg, state)
	{
		if (msg.ctor === 'self')
		{
			return A3(onSelfMsg, router, msg._0, state);
		}

		var fx = msg._0;
		switch (tag)
		{
			case 'cmd':
				return A3(onEffects, router, fx.cmds, state);

			case 'sub':
				return A3(onEffects, router, fx.subs, state);

			case 'fx':
				return A4(onEffects, router, fx.cmds, fx.subs, state);
		}
	}

	var process = spawnLoop(info.init, onMessage);
	router.self = process;
	return process;
}

function sendToApp(router, msg)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		router.main(msg);
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sendToSelf(router, msg)
{
	return A2(_elm_lang$core$Native_Scheduler.send, router.self, {
		ctor: 'self',
		_0: msg
	});
}


// HELPER for STATEFUL LOOPS

function spawnLoop(init, onMessage)
{
	var andThen = _elm_lang$core$Native_Scheduler.andThen;

	function loop(state)
	{
		var handleMsg = _elm_lang$core$Native_Scheduler.receive(function(msg) {
			return onMessage(msg, state);
		});
		return A2(andThen, loop, handleMsg);
	}

	var task = A2(andThen, loop, init);

	return _elm_lang$core$Native_Scheduler.rawSpawn(task);
}


// BAGS

function leaf(home)
{
	return function(value)
	{
		return {
			type: 'leaf',
			home: home,
			value: value
		};
	};
}

function batch(list)
{
	return {
		type: 'node',
		branches: list
	};
}

function map(tagger, bag)
{
	return {
		type: 'map',
		tagger: tagger,
		tree: bag
	}
}


// PIPE BAGS INTO EFFECT MANAGERS

function dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	gatherEffects(true, cmdBag, effectsDict, null);
	gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		var fx = home in effectsDict
			? effectsDict[home]
			: {
				cmds: _elm_lang$core$Native_List.Nil,
				subs: _elm_lang$core$Native_List.Nil
			};

		_elm_lang$core$Native_Scheduler.rawSend(managers[home], { ctor: 'fx', _0: fx });
	}
}

function gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.type)
	{
		case 'leaf':
			var home = bag.home;
			var effect = toEffect(isCmd, home, taggers, bag.value);
			effectsDict[home] = insert(isCmd, effect, effectsDict[home]);
			return;

		case 'node':
			var list = bag.branches;
			while (list.ctor !== '[]')
			{
				gatherEffects(isCmd, list._0, effectsDict, taggers);
				list = list._1;
			}
			return;

		case 'map':
			gatherEffects(isCmd, bag.tree, effectsDict, {
				tagger: bag.tagger,
				rest: taggers
			});
			return;
	}
}

function toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		var temp = taggers;
		while (temp)
		{
			x = temp.tagger(x);
			temp = temp.rest;
		}
		return x;
	}

	var map = isCmd
		? effectManagers[home].cmdMap
		: effectManagers[home].subMap;

	return A2(map, applyTaggers, value)
}

function insert(isCmd, newEffect, effects)
{
	effects = effects || {
		cmds: _elm_lang$core$Native_List.Nil,
		subs: _elm_lang$core$Native_List.Nil
	};
	if (isCmd)
	{
		effects.cmds = _elm_lang$core$Native_List.Cons(newEffect, effects.cmds);
		return effects;
	}
	effects.subs = _elm_lang$core$Native_List.Cons(newEffect, effects.subs);
	return effects;
}


// PORTS

function checkPortName(name)
{
	if (name in effectManagers)
	{
		throw new Error('There can only be one port named `' + name + '`, but your program has multiple.');
	}
}


// OUTGOING PORTS

function outgoingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'cmd',
		cmdMap: outgoingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var outgoingPortMap = F2(function cmdMap(tagger, value) {
	return value;
});

function setupOutgoingPort(name)
{
	var subs = [];
	var converter = effectManagers[name].converter;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function onEffects(router, cmdList, state)
	{
		while (cmdList.ctor !== '[]')
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = converter(cmdList._0);
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
			cmdList = cmdList._1;
		}
		return init;
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}


// INCOMING PORTS

function incomingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'sub',
		subMap: incomingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var incomingPortMap = F2(function subMap(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});

function setupIncomingPort(name, callback)
{
	var sentBeforeInit = [];
	var subs = _elm_lang$core$Native_List.Nil;
	var converter = effectManagers[name].converter;
	var currentOnEffects = preInitOnEffects;
	var currentSend = preInitSend;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function preInitOnEffects(router, subList, state)
	{
		var postInitResult = postInitOnEffects(router, subList, state);

		for(var i = 0; i < sentBeforeInit.length; i++)
		{
			postInitSend(sentBeforeInit[i]);
		}

		sentBeforeInit = null; // to release objects held in queue
		currentSend = postInitSend;
		currentOnEffects = postInitOnEffects;
		return postInitResult;
	}

	function postInitOnEffects(router, subList, state)
	{
		subs = subList;
		return init;
	}

	function onEffects(router, subList, state)
	{
		return currentOnEffects(router, subList, state);
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function preInitSend(value)
	{
		sentBeforeInit.push(value);
	}

	function postInitSend(value)
	{
		var temp = subs;
		while (temp.ctor !== '[]')
		{
			callback(temp._0(value));
			temp = temp._1;
		}
	}

	function send(incomingValue)
	{
		var result = A2(_elm_lang$core$Json_Decode$decodeValue, converter, incomingValue);
		if (result.ctor === 'Err')
		{
			throw new Error('Trying to send an unexpected type of value through port `' + name + '`:\n' + result._0);
		}

		currentSend(result._0);
	}

	return { send: send };
}

return {
	// routers
	sendToApp: F2(sendToApp),
	sendToSelf: F2(sendToSelf),

	// global setup
	effectManagers: effectManagers,
	outgoingPort: outgoingPort,
	incomingPort: incomingPort,

	htmlToProgram: htmlToProgram,
	program: program,
	programWithFlags: programWithFlags,
	initialize: initialize,

	// effect bags
	leaf: leaf,
	batch: batch,
	map: F2(map)
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Scheduler = function() {

var MAX_STEPS = 10000;


// TASKS

function succeed(value)
{
	return {
		ctor: '_Task_succeed',
		value: value
	};
}

function fail(error)
{
	return {
		ctor: '_Task_fail',
		value: error
	};
}

function nativeBinding(callback)
{
	return {
		ctor: '_Task_nativeBinding',
		callback: callback,
		cancel: null
	};
}

function andThen(callback, task)
{
	return {
		ctor: '_Task_andThen',
		callback: callback,
		task: task
	};
}

function onError(callback, task)
{
	return {
		ctor: '_Task_onError',
		callback: callback,
		task: task
	};
}

function receive(callback)
{
	return {
		ctor: '_Task_receive',
		callback: callback
	};
}


// PROCESSES

function rawSpawn(task)
{
	var process = {
		ctor: '_Process',
		id: _elm_lang$core$Native_Utils.guid(),
		root: task,
		stack: null,
		mailbox: []
	};

	enqueue(process);

	return process;
}

function spawn(task)
{
	return nativeBinding(function(callback) {
		var process = rawSpawn(task);
		callback(succeed(process));
	});
}

function rawSend(process, msg)
{
	process.mailbox.push(msg);
	enqueue(process);
}

function send(process, msg)
{
	return nativeBinding(function(callback) {
		rawSend(process, msg);
		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function kill(process)
{
	return nativeBinding(function(callback) {
		var root = process.root;
		if (root.ctor === '_Task_nativeBinding' && root.cancel)
		{
			root.cancel();
		}

		process.root = null;

		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sleep(time)
{
	return nativeBinding(function(callback) {
		var id = setTimeout(function() {
			callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}


// STEP PROCESSES

function step(numSteps, process)
{
	while (numSteps < MAX_STEPS)
	{
		var ctor = process.root.ctor;

		if (ctor === '_Task_succeed')
		{
			while (process.stack && process.stack.ctor === '_Task_onError')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_fail')
		{
			while (process.stack && process.stack.ctor === '_Task_andThen')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_andThen')
		{
			process.stack = {
				ctor: '_Task_andThen',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_onError')
		{
			process.stack = {
				ctor: '_Task_onError',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_nativeBinding')
		{
			process.root.cancel = process.root.callback(function(newRoot) {
				process.root = newRoot;
				enqueue(process);
			});

			break;
		}

		if (ctor === '_Task_receive')
		{
			var mailbox = process.mailbox;
			if (mailbox.length === 0)
			{
				break;
			}

			process.root = process.root.callback(mailbox.shift());
			++numSteps;
			continue;
		}

		throw new Error(ctor);
	}

	if (numSteps < MAX_STEPS)
	{
		return numSteps + 1;
	}
	enqueue(process);

	return numSteps;
}


// WORK QUEUE

var working = false;
var workQueue = [];

function enqueue(process)
{
	workQueue.push(process);

	if (!working)
	{
		setTimeout(work, 0);
		working = true;
	}
}

function work()
{
	var numSteps = 0;
	var process;
	while (numSteps < MAX_STEPS && (process = workQueue.shift()))
	{
		if (process.root)
		{
			numSteps = step(numSteps, process);
		}
	}
	if (!process)
	{
		working = false;
		return;
	}
	setTimeout(work, 0);
}


return {
	succeed: succeed,
	fail: fail,
	nativeBinding: nativeBinding,
	andThen: F2(andThen),
	onError: F2(onError),
	receive: receive,

	spawn: spawn,
	kill: kill,
	sleep: sleep,
	send: F2(send),

	rawSpawn: rawSpawn,
	rawSend: rawSend
};

}();
var _elm_lang$core$Platform_Cmd$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Cmd$none = _elm_lang$core$Platform_Cmd$batch(
	{ctor: '[]'});
var _elm_lang$core$Platform_Cmd_ops = _elm_lang$core$Platform_Cmd_ops || {};
_elm_lang$core$Platform_Cmd_ops['!'] = F2(
	function (model, commands) {
		return {
			ctor: '_Tuple2',
			_0: model,
			_1: _elm_lang$core$Platform_Cmd$batch(commands)
		};
	});
var _elm_lang$core$Platform_Cmd$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Cmd$Cmd = {ctor: 'Cmd'};

var _elm_lang$core$Platform_Sub$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Sub$none = _elm_lang$core$Platform_Sub$batch(
	{ctor: '[]'});
var _elm_lang$core$Platform_Sub$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Sub$Sub = {ctor: 'Sub'};

var _elm_lang$core$Platform$hack = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Platform$sendToSelf = _elm_lang$core$Native_Platform.sendToSelf;
var _elm_lang$core$Platform$sendToApp = _elm_lang$core$Native_Platform.sendToApp;
var _elm_lang$core$Platform$programWithFlags = _elm_lang$core$Native_Platform.programWithFlags;
var _elm_lang$core$Platform$program = _elm_lang$core$Native_Platform.program;
var _elm_lang$core$Platform$Program = {ctor: 'Program'};
var _elm_lang$core$Platform$Task = {ctor: 'Task'};
var _elm_lang$core$Platform$ProcessId = {ctor: 'ProcessId'};
var _elm_lang$core$Platform$Router = {ctor: 'Router'};

var _elm_lang$virtual_dom$VirtualDom$programWithFlags = function (impl) {
	return A2(_elm_lang$virtual_dom$Native_VirtualDom.programWithFlags, _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags, impl);
};
var _elm_lang$virtual_dom$VirtualDom$program = function (impl) {
	return A2(_elm_lang$virtual_dom$Native_VirtualDom.program, _elm_lang$virtual_dom$VirtualDom_Debug$wrap, impl);
};
var _elm_lang$virtual_dom$VirtualDom$keyedNode = _elm_lang$virtual_dom$Native_VirtualDom.keyedNode;
var _elm_lang$virtual_dom$VirtualDom$lazy3 = _elm_lang$virtual_dom$Native_VirtualDom.lazy3;
var _elm_lang$virtual_dom$VirtualDom$lazy2 = _elm_lang$virtual_dom$Native_VirtualDom.lazy2;
var _elm_lang$virtual_dom$VirtualDom$lazy = _elm_lang$virtual_dom$Native_VirtualDom.lazy;
var _elm_lang$virtual_dom$VirtualDom$defaultOptions = {stopPropagation: false, preventDefault: false};
var _elm_lang$virtual_dom$VirtualDom$onWithOptions = _elm_lang$virtual_dom$Native_VirtualDom.on;
var _elm_lang$virtual_dom$VirtualDom$on = F2(
	function (eventName, decoder) {
		return A3(_elm_lang$virtual_dom$VirtualDom$onWithOptions, eventName, _elm_lang$virtual_dom$VirtualDom$defaultOptions, decoder);
	});
var _elm_lang$virtual_dom$VirtualDom$style = _elm_lang$virtual_dom$Native_VirtualDom.style;
var _elm_lang$virtual_dom$VirtualDom$mapProperty = _elm_lang$virtual_dom$Native_VirtualDom.mapProperty;
var _elm_lang$virtual_dom$VirtualDom$attributeNS = _elm_lang$virtual_dom$Native_VirtualDom.attributeNS;
var _elm_lang$virtual_dom$VirtualDom$attribute = _elm_lang$virtual_dom$Native_VirtualDom.attribute;
var _elm_lang$virtual_dom$VirtualDom$property = _elm_lang$virtual_dom$Native_VirtualDom.property;
var _elm_lang$virtual_dom$VirtualDom$map = _elm_lang$virtual_dom$Native_VirtualDom.map;
var _elm_lang$virtual_dom$VirtualDom$text = _elm_lang$virtual_dom$Native_VirtualDom.text;
var _elm_lang$virtual_dom$VirtualDom$node = _elm_lang$virtual_dom$Native_VirtualDom.node;
var _elm_lang$virtual_dom$VirtualDom$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});
var _elm_lang$virtual_dom$VirtualDom$Node = {ctor: 'Node'};
var _elm_lang$virtual_dom$VirtualDom$Property = {ctor: 'Property'};

var _elm_lang$html$Html$programWithFlags = _elm_lang$virtual_dom$VirtualDom$programWithFlags;
var _elm_lang$html$Html$program = _elm_lang$virtual_dom$VirtualDom$program;
var _elm_lang$html$Html$beginnerProgram = function (_p0) {
	var _p1 = _p0;
	return _elm_lang$html$Html$program(
		{
			init: A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_p1.model,
				{ctor: '[]'}),
			update: F2(
				function (msg, model) {
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						A2(_p1.update, msg, model),
						{ctor: '[]'});
				}),
			view: _p1.view,
			subscriptions: function (_p2) {
				return _elm_lang$core$Platform_Sub$none;
			}
		});
};
var _elm_lang$html$Html$map = _elm_lang$virtual_dom$VirtualDom$map;
var _elm_lang$html$Html$text = _elm_lang$virtual_dom$VirtualDom$text;
var _elm_lang$html$Html$node = _elm_lang$virtual_dom$VirtualDom$node;
var _elm_lang$html$Html$body = _elm_lang$html$Html$node('body');
var _elm_lang$html$Html$section = _elm_lang$html$Html$node('section');
var _elm_lang$html$Html$nav = _elm_lang$html$Html$node('nav');
var _elm_lang$html$Html$article = _elm_lang$html$Html$node('article');
var _elm_lang$html$Html$aside = _elm_lang$html$Html$node('aside');
var _elm_lang$html$Html$h1 = _elm_lang$html$Html$node('h1');
var _elm_lang$html$Html$h2 = _elm_lang$html$Html$node('h2');
var _elm_lang$html$Html$h3 = _elm_lang$html$Html$node('h3');
var _elm_lang$html$Html$h4 = _elm_lang$html$Html$node('h4');
var _elm_lang$html$Html$h5 = _elm_lang$html$Html$node('h5');
var _elm_lang$html$Html$h6 = _elm_lang$html$Html$node('h6');
var _elm_lang$html$Html$header = _elm_lang$html$Html$node('header');
var _elm_lang$html$Html$footer = _elm_lang$html$Html$node('footer');
var _elm_lang$html$Html$address = _elm_lang$html$Html$node('address');
var _elm_lang$html$Html$main_ = _elm_lang$html$Html$node('main');
var _elm_lang$html$Html$p = _elm_lang$html$Html$node('p');
var _elm_lang$html$Html$hr = _elm_lang$html$Html$node('hr');
var _elm_lang$html$Html$pre = _elm_lang$html$Html$node('pre');
var _elm_lang$html$Html$blockquote = _elm_lang$html$Html$node('blockquote');
var _elm_lang$html$Html$ol = _elm_lang$html$Html$node('ol');
var _elm_lang$html$Html$ul = _elm_lang$html$Html$node('ul');
var _elm_lang$html$Html$li = _elm_lang$html$Html$node('li');
var _elm_lang$html$Html$dl = _elm_lang$html$Html$node('dl');
var _elm_lang$html$Html$dt = _elm_lang$html$Html$node('dt');
var _elm_lang$html$Html$dd = _elm_lang$html$Html$node('dd');
var _elm_lang$html$Html$figure = _elm_lang$html$Html$node('figure');
var _elm_lang$html$Html$figcaption = _elm_lang$html$Html$node('figcaption');
var _elm_lang$html$Html$div = _elm_lang$html$Html$node('div');
var _elm_lang$html$Html$a = _elm_lang$html$Html$node('a');
var _elm_lang$html$Html$em = _elm_lang$html$Html$node('em');
var _elm_lang$html$Html$strong = _elm_lang$html$Html$node('strong');
var _elm_lang$html$Html$small = _elm_lang$html$Html$node('small');
var _elm_lang$html$Html$s = _elm_lang$html$Html$node('s');
var _elm_lang$html$Html$cite = _elm_lang$html$Html$node('cite');
var _elm_lang$html$Html$q = _elm_lang$html$Html$node('q');
var _elm_lang$html$Html$dfn = _elm_lang$html$Html$node('dfn');
var _elm_lang$html$Html$abbr = _elm_lang$html$Html$node('abbr');
var _elm_lang$html$Html$time = _elm_lang$html$Html$node('time');
var _elm_lang$html$Html$code = _elm_lang$html$Html$node('code');
var _elm_lang$html$Html$var = _elm_lang$html$Html$node('var');
var _elm_lang$html$Html$samp = _elm_lang$html$Html$node('samp');
var _elm_lang$html$Html$kbd = _elm_lang$html$Html$node('kbd');
var _elm_lang$html$Html$sub = _elm_lang$html$Html$node('sub');
var _elm_lang$html$Html$sup = _elm_lang$html$Html$node('sup');
var _elm_lang$html$Html$i = _elm_lang$html$Html$node('i');
var _elm_lang$html$Html$b = _elm_lang$html$Html$node('b');
var _elm_lang$html$Html$u = _elm_lang$html$Html$node('u');
var _elm_lang$html$Html$mark = _elm_lang$html$Html$node('mark');
var _elm_lang$html$Html$ruby = _elm_lang$html$Html$node('ruby');
var _elm_lang$html$Html$rt = _elm_lang$html$Html$node('rt');
var _elm_lang$html$Html$rp = _elm_lang$html$Html$node('rp');
var _elm_lang$html$Html$bdi = _elm_lang$html$Html$node('bdi');
var _elm_lang$html$Html$bdo = _elm_lang$html$Html$node('bdo');
var _elm_lang$html$Html$span = _elm_lang$html$Html$node('span');
var _elm_lang$html$Html$br = _elm_lang$html$Html$node('br');
var _elm_lang$html$Html$wbr = _elm_lang$html$Html$node('wbr');
var _elm_lang$html$Html$ins = _elm_lang$html$Html$node('ins');
var _elm_lang$html$Html$del = _elm_lang$html$Html$node('del');
var _elm_lang$html$Html$img = _elm_lang$html$Html$node('img');
var _elm_lang$html$Html$iframe = _elm_lang$html$Html$node('iframe');
var _elm_lang$html$Html$embed = _elm_lang$html$Html$node('embed');
var _elm_lang$html$Html$object = _elm_lang$html$Html$node('object');
var _elm_lang$html$Html$param = _elm_lang$html$Html$node('param');
var _elm_lang$html$Html$video = _elm_lang$html$Html$node('video');
var _elm_lang$html$Html$audio = _elm_lang$html$Html$node('audio');
var _elm_lang$html$Html$source = _elm_lang$html$Html$node('source');
var _elm_lang$html$Html$track = _elm_lang$html$Html$node('track');
var _elm_lang$html$Html$canvas = _elm_lang$html$Html$node('canvas');
var _elm_lang$html$Html$math = _elm_lang$html$Html$node('math');
var _elm_lang$html$Html$table = _elm_lang$html$Html$node('table');
var _elm_lang$html$Html$caption = _elm_lang$html$Html$node('caption');
var _elm_lang$html$Html$colgroup = _elm_lang$html$Html$node('colgroup');
var _elm_lang$html$Html$col = _elm_lang$html$Html$node('col');
var _elm_lang$html$Html$tbody = _elm_lang$html$Html$node('tbody');
var _elm_lang$html$Html$thead = _elm_lang$html$Html$node('thead');
var _elm_lang$html$Html$tfoot = _elm_lang$html$Html$node('tfoot');
var _elm_lang$html$Html$tr = _elm_lang$html$Html$node('tr');
var _elm_lang$html$Html$td = _elm_lang$html$Html$node('td');
var _elm_lang$html$Html$th = _elm_lang$html$Html$node('th');
var _elm_lang$html$Html$form = _elm_lang$html$Html$node('form');
var _elm_lang$html$Html$fieldset = _elm_lang$html$Html$node('fieldset');
var _elm_lang$html$Html$legend = _elm_lang$html$Html$node('legend');
var _elm_lang$html$Html$label = _elm_lang$html$Html$node('label');
var _elm_lang$html$Html$input = _elm_lang$html$Html$node('input');
var _elm_lang$html$Html$button = _elm_lang$html$Html$node('button');
var _elm_lang$html$Html$select = _elm_lang$html$Html$node('select');
var _elm_lang$html$Html$datalist = _elm_lang$html$Html$node('datalist');
var _elm_lang$html$Html$optgroup = _elm_lang$html$Html$node('optgroup');
var _elm_lang$html$Html$option = _elm_lang$html$Html$node('option');
var _elm_lang$html$Html$textarea = _elm_lang$html$Html$node('textarea');
var _elm_lang$html$Html$keygen = _elm_lang$html$Html$node('keygen');
var _elm_lang$html$Html$output = _elm_lang$html$Html$node('output');
var _elm_lang$html$Html$progress = _elm_lang$html$Html$node('progress');
var _elm_lang$html$Html$meter = _elm_lang$html$Html$node('meter');
var _elm_lang$html$Html$details = _elm_lang$html$Html$node('details');
var _elm_lang$html$Html$summary = _elm_lang$html$Html$node('summary');
var _elm_lang$html$Html$menuitem = _elm_lang$html$Html$node('menuitem');
var _elm_lang$html$Html$menu = _elm_lang$html$Html$node('menu');

var _elm_lang$html$Html_Attributes$map = _elm_lang$virtual_dom$VirtualDom$mapProperty;
var _elm_lang$html$Html_Attributes$attribute = _elm_lang$virtual_dom$VirtualDom$attribute;
var _elm_lang$html$Html_Attributes$contextmenu = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'contextmenu', value);
};
var _elm_lang$html$Html_Attributes$draggable = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'draggable', value);
};
var _elm_lang$html$Html_Attributes$itemprop = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'itemprop', value);
};
var _elm_lang$html$Html_Attributes$tabindex = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'tabIndex',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$charset = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'charset', value);
};
var _elm_lang$html$Html_Attributes$height = function (value) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'height',
		_elm_lang$core$Basics$toString(value));
};
var _elm_lang$html$Html_Attributes$width = function (value) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'width',
		_elm_lang$core$Basics$toString(value));
};
var _elm_lang$html$Html_Attributes$formaction = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'formAction', value);
};
var _elm_lang$html$Html_Attributes$list = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'list', value);
};
var _elm_lang$html$Html_Attributes$minlength = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'minLength',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$maxlength = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'maxlength',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$size = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'size',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$form = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'form', value);
};
var _elm_lang$html$Html_Attributes$cols = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'cols',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$rows = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'rows',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$challenge = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'challenge', value);
};
var _elm_lang$html$Html_Attributes$media = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'media', value);
};
var _elm_lang$html$Html_Attributes$rel = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'rel', value);
};
var _elm_lang$html$Html_Attributes$datetime = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'datetime', value);
};
var _elm_lang$html$Html_Attributes$pubdate = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'pubdate', value);
};
var _elm_lang$html$Html_Attributes$colspan = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'colspan',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$rowspan = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'rowspan',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$manifest = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'manifest', value);
};
var _elm_lang$html$Html_Attributes$property = _elm_lang$virtual_dom$VirtualDom$property;
var _elm_lang$html$Html_Attributes$stringProperty = F2(
	function (name, string) {
		return A2(
			_elm_lang$html$Html_Attributes$property,
			name,
			_elm_lang$core$Json_Encode$string(string));
	});
var _elm_lang$html$Html_Attributes$class = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'className', name);
};
var _elm_lang$html$Html_Attributes$id = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'id', name);
};
var _elm_lang$html$Html_Attributes$title = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'title', name);
};
var _elm_lang$html$Html_Attributes$accesskey = function ($char) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'accessKey',
		_elm_lang$core$String$fromChar($char));
};
var _elm_lang$html$Html_Attributes$dir = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dir', value);
};
var _elm_lang$html$Html_Attributes$dropzone = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dropzone', value);
};
var _elm_lang$html$Html_Attributes$lang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'lang', value);
};
var _elm_lang$html$Html_Attributes$content = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'content', value);
};
var _elm_lang$html$Html_Attributes$httpEquiv = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'httpEquiv', value);
};
var _elm_lang$html$Html_Attributes$language = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'language', value);
};
var _elm_lang$html$Html_Attributes$src = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'src', value);
};
var _elm_lang$html$Html_Attributes$alt = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'alt', value);
};
var _elm_lang$html$Html_Attributes$preload = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'preload', value);
};
var _elm_lang$html$Html_Attributes$poster = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'poster', value);
};
var _elm_lang$html$Html_Attributes$kind = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'kind', value);
};
var _elm_lang$html$Html_Attributes$srclang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srclang', value);
};
var _elm_lang$html$Html_Attributes$sandbox = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'sandbox', value);
};
var _elm_lang$html$Html_Attributes$srcdoc = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srcdoc', value);
};
var _elm_lang$html$Html_Attributes$type_ = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'type', value);
};
var _elm_lang$html$Html_Attributes$value = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'value', value);
};
var _elm_lang$html$Html_Attributes$defaultValue = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'defaultValue', value);
};
var _elm_lang$html$Html_Attributes$placeholder = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'placeholder', value);
};
var _elm_lang$html$Html_Attributes$accept = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'accept', value);
};
var _elm_lang$html$Html_Attributes$acceptCharset = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'acceptCharset', value);
};
var _elm_lang$html$Html_Attributes$action = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'action', value);
};
var _elm_lang$html$Html_Attributes$autocomplete = function (bool) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'autocomplete',
		bool ? 'on' : 'off');
};
var _elm_lang$html$Html_Attributes$enctype = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'enctype', value);
};
var _elm_lang$html$Html_Attributes$method = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'method', value);
};
var _elm_lang$html$Html_Attributes$name = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'name', value);
};
var _elm_lang$html$Html_Attributes$pattern = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'pattern', value);
};
var _elm_lang$html$Html_Attributes$for = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'htmlFor', value);
};
var _elm_lang$html$Html_Attributes$max = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'max', value);
};
var _elm_lang$html$Html_Attributes$min = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'min', value);
};
var _elm_lang$html$Html_Attributes$step = function (n) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'step', n);
};
var _elm_lang$html$Html_Attributes$wrap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'wrap', value);
};
var _elm_lang$html$Html_Attributes$usemap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'useMap', value);
};
var _elm_lang$html$Html_Attributes$shape = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'shape', value);
};
var _elm_lang$html$Html_Attributes$coords = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'coords', value);
};
var _elm_lang$html$Html_Attributes$keytype = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'keytype', value);
};
var _elm_lang$html$Html_Attributes$align = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'align', value);
};
var _elm_lang$html$Html_Attributes$cite = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'cite', value);
};
var _elm_lang$html$Html_Attributes$href = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'href', value);
};
var _elm_lang$html$Html_Attributes$target = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'target', value);
};
var _elm_lang$html$Html_Attributes$downloadAs = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'download', value);
};
var _elm_lang$html$Html_Attributes$hreflang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'hreflang', value);
};
var _elm_lang$html$Html_Attributes$ping = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'ping', value);
};
var _elm_lang$html$Html_Attributes$start = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'start',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$headers = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'headers', value);
};
var _elm_lang$html$Html_Attributes$scope = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'scope', value);
};
var _elm_lang$html$Html_Attributes$boolProperty = F2(
	function (name, bool) {
		return A2(
			_elm_lang$html$Html_Attributes$property,
			name,
			_elm_lang$core$Json_Encode$bool(bool));
	});
var _elm_lang$html$Html_Attributes$hidden = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'hidden', bool);
};
var _elm_lang$html$Html_Attributes$contenteditable = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'contentEditable', bool);
};
var _elm_lang$html$Html_Attributes$spellcheck = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'spellcheck', bool);
};
var _elm_lang$html$Html_Attributes$async = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'async', bool);
};
var _elm_lang$html$Html_Attributes$defer = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'defer', bool);
};
var _elm_lang$html$Html_Attributes$scoped = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'scoped', bool);
};
var _elm_lang$html$Html_Attributes$autoplay = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autoplay', bool);
};
var _elm_lang$html$Html_Attributes$controls = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'controls', bool);
};
var _elm_lang$html$Html_Attributes$loop = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'loop', bool);
};
var _elm_lang$html$Html_Attributes$default = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'default', bool);
};
var _elm_lang$html$Html_Attributes$seamless = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'seamless', bool);
};
var _elm_lang$html$Html_Attributes$checked = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'checked', bool);
};
var _elm_lang$html$Html_Attributes$selected = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'selected', bool);
};
var _elm_lang$html$Html_Attributes$autofocus = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autofocus', bool);
};
var _elm_lang$html$Html_Attributes$disabled = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'disabled', bool);
};
var _elm_lang$html$Html_Attributes$multiple = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'multiple', bool);
};
var _elm_lang$html$Html_Attributes$novalidate = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'noValidate', bool);
};
var _elm_lang$html$Html_Attributes$readonly = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'readOnly', bool);
};
var _elm_lang$html$Html_Attributes$required = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'required', bool);
};
var _elm_lang$html$Html_Attributes$ismap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'isMap', value);
};
var _elm_lang$html$Html_Attributes$download = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'download', bool);
};
var _elm_lang$html$Html_Attributes$reversed = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'reversed', bool);
};
var _elm_lang$html$Html_Attributes$classList = function (list) {
	return _elm_lang$html$Html_Attributes$class(
		A2(
			_elm_lang$core$String$join,
			' ',
			A2(
				_elm_lang$core$List$map,
				_elm_lang$core$Tuple$first,
				A2(_elm_lang$core$List$filter, _elm_lang$core$Tuple$second, list))));
};
var _elm_lang$html$Html_Attributes$style = _elm_lang$virtual_dom$VirtualDom$style;

var _elm_lang$html$Html_Events$keyCode = A2(_elm_lang$core$Json_Decode$field, 'keyCode', _elm_lang$core$Json_Decode$int);
var _elm_lang$html$Html_Events$targetChecked = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'target',
		_1: {
			ctor: '::',
			_0: 'checked',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$bool);
var _elm_lang$html$Html_Events$targetValue = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'target',
		_1: {
			ctor: '::',
			_0: 'value',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$string);
var _elm_lang$html$Html_Events$defaultOptions = _elm_lang$virtual_dom$VirtualDom$defaultOptions;
var _elm_lang$html$Html_Events$onWithOptions = _elm_lang$virtual_dom$VirtualDom$onWithOptions;
var _elm_lang$html$Html_Events$on = _elm_lang$virtual_dom$VirtualDom$on;
var _elm_lang$html$Html_Events$onFocus = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'focus',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onBlur = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'blur',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onSubmitOptions = _elm_lang$core$Native_Utils.update(
	_elm_lang$html$Html_Events$defaultOptions,
	{preventDefault: true});
var _elm_lang$html$Html_Events$onSubmit = function (msg) {
	return A3(
		_elm_lang$html$Html_Events$onWithOptions,
		'submit',
		_elm_lang$html$Html_Events$onSubmitOptions,
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onCheck = function (tagger) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'change',
		A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetChecked));
};
var _elm_lang$html$Html_Events$onInput = function (tagger) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'input',
		A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetValue));
};
var _elm_lang$html$Html_Events$onMouseOut = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseout',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseOver = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseover',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseLeave = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseleave',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseEnter = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseenter',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseUp = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseup',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseDown = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mousedown',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onDoubleClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'dblclick',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'click',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});

var _rundis$elm_bootstrap$Bootstrap_CDN$fontAwesome = A3(
	_elm_lang$html$Html$node,
	'link',
	{
		ctor: '::',
		_0: _elm_lang$html$Html_Attributes$rel('stylesheet'),
		_1: {
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$href('https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css'),
			_1: {ctor: '[]'}
		}
	},
	{ctor: '[]'});
var _rundis$elm_bootstrap$Bootstrap_CDN$stylesheet = A3(
	_elm_lang$html$Html$node,
	'link',
	{
		ctor: '::',
		_0: _elm_lang$html$Html_Attributes$rel('stylesheet'),
		_1: {
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$href('https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css'),
			_1: {ctor: '[]'}
		}
	},
	{ctor: '[]'});

var _elm_lang$html$Html_Keyed$node = _elm_lang$virtual_dom$VirtualDom$keyedNode;
var _elm_lang$html$Html_Keyed$ol = _elm_lang$html$Html_Keyed$node('ol');
var _elm_lang$html$Html_Keyed$ul = _elm_lang$html$Html_Keyed$node('ul');

var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$horizontalAlignOption = function (align) {
	var _p0 = align;
	switch (_p0.ctor) {
		case 'Left':
			return 'start';
		case 'Center':
			return 'center';
		case 'Right':
			return 'end';
		case 'Around':
			return 'around';
		default:
			return 'between';
	}
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$verticalAlignOption = function (align) {
	var _p1 = align;
	switch (_p1.ctor) {
		case 'Top':
			return 'start';
		case 'Middle':
			return 'center';
		default:
			return 'end';
	}
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$moveCountOption = function (size) {
	var _p2 = size;
	switch (_p2.ctor) {
		case 'Move0':
			return '0';
		case 'Move1':
			return '1';
		case 'Move2':
			return '2';
		case 'Move3':
			return '3';
		case 'Move4':
			return '4';
		case 'Move5':
			return '5';
		case 'Move6':
			return '6';
		case 'Move7':
			return '7';
		case 'Move8':
			return '8';
		case 'Move9':
			return '9';
		case 'Move10':
			return '10';
		case 'Move11':
			return '11';
		default:
			return '12';
	}
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$offsetCountOption = function (size) {
	var _p3 = size;
	switch (_p3.ctor) {
		case 'Offset0':
			return '0';
		case 'Offset1':
			return '1';
		case 'Offset2':
			return '2';
		case 'Offset3':
			return '3';
		case 'Offset4':
			return '4';
		case 'Offset5':
			return '5';
		case 'Offset6':
			return '6';
		case 'Offset7':
			return '7';
		case 'Offset8':
			return '8';
		case 'Offset9':
			return '9';
		case 'Offset10':
			return '10';
		default:
			return '11';
	}
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$columnCountOption = function (size) {
	var _p4 = size;
	switch (_p4.ctor) {
		case 'Col':
			return _elm_lang$core$Maybe$Nothing;
		case 'Col1':
			return _elm_lang$core$Maybe$Just('1');
		case 'Col2':
			return _elm_lang$core$Maybe$Just('2');
		case 'Col3':
			return _elm_lang$core$Maybe$Just('3');
		case 'Col4':
			return _elm_lang$core$Maybe$Just('4');
		case 'Col5':
			return _elm_lang$core$Maybe$Just('5');
		case 'Col6':
			return _elm_lang$core$Maybe$Just('6');
		case 'Col7':
			return _elm_lang$core$Maybe$Just('7');
		case 'Col8':
			return _elm_lang$core$Maybe$Just('8');
		case 'Col9':
			return _elm_lang$core$Maybe$Just('9');
		case 'Col10':
			return _elm_lang$core$Maybe$Just('10');
		case 'Col11':
			return _elm_lang$core$Maybe$Just('11');
		case 'Col12':
			return _elm_lang$core$Maybe$Just('12');
		default:
			return _elm_lang$core$Maybe$Just('auto');
	}
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$screenSizeOption = function (size) {
	var _p5 = size;
	switch (_p5.ctor) {
		case 'XS':
			return _elm_lang$core$Maybe$Nothing;
		case 'SM':
			return _elm_lang$core$Maybe$Just('sm');
		case 'MD':
			return _elm_lang$core$Maybe$Just('md');
		case 'LG':
			return _elm_lang$core$Maybe$Just('lg');
		default:
			return _elm_lang$core$Maybe$Just('xl');
	}
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$screenSizeToPartialString = function (screenSize) {
	var _p6 = _rundis$elm_bootstrap$Bootstrap_Grid_Internal$screenSizeOption(screenSize);
	if (_p6.ctor === 'Just') {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			'-',
			A2(_elm_lang$core$Basics_ops['++'], _p6._0, '-'));
	} else {
		return '-';
	}
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$hAlignClass = function (_p7) {
	var _p8 = _p7;
	return _elm_lang$html$Html_Attributes$class(
		A2(
			_elm_lang$core$Basics_ops['++'],
			'justify-content-',
			A2(
				_elm_lang$core$Basics_ops['++'],
				A2(
					_elm_lang$core$Maybe$withDefault,
					'',
					A2(
						_elm_lang$core$Maybe$map,
						function (v) {
							return A2(_elm_lang$core$Basics_ops['++'], v, '-');
						},
						_rundis$elm_bootstrap$Bootstrap_Grid_Internal$screenSizeOption(_p8.screenSize))),
				_rundis$elm_bootstrap$Bootstrap_Grid_Internal$horizontalAlignOption(_p8.align))));
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$hAlignsToAttributes = function (aligns) {
	var align = function (a) {
		return A2(_elm_lang$core$Maybe$map, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$hAlignClass, a);
	};
	return A2(
		_elm_lang$core$List$filterMap,
		_elm_lang$core$Basics$identity,
		A2(_elm_lang$core$List$map, align, aligns));
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$vAlignClass = F2(
	function (prefix, _p9) {
		var _p10 = _p9;
		return _elm_lang$html$Html_Attributes$class(
			A2(
				_elm_lang$core$Basics_ops['++'],
				prefix,
				A2(
					_elm_lang$core$Basics_ops['++'],
					A2(
						_elm_lang$core$Maybe$withDefault,
						'',
						A2(
							_elm_lang$core$Maybe$map,
							function (v) {
								return A2(_elm_lang$core$Basics_ops['++'], v, '-');
							},
							_rundis$elm_bootstrap$Bootstrap_Grid_Internal$screenSizeOption(_p10.screenSize))),
					_rundis$elm_bootstrap$Bootstrap_Grid_Internal$verticalAlignOption(_p10.align))));
	});
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$vAlignsToAttributes = F2(
	function (prefix, aligns) {
		var align = function (a) {
			return A2(
				_elm_lang$core$Maybe$map,
				_rundis$elm_bootstrap$Bootstrap_Grid_Internal$vAlignClass(prefix),
				a);
		};
		return A2(
			_elm_lang$core$List$filterMap,
			_elm_lang$core$Basics$identity,
			A2(_elm_lang$core$List$map, align, aligns));
	});
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$pushesToAttributes = function (pushes) {
	var push = function (m) {
		var _p11 = m;
		if (_p11.ctor === 'Just') {
			return _elm_lang$core$Maybe$Just(
				_elm_lang$html$Html_Attributes$class(
					A2(
						_elm_lang$core$Basics_ops['++'],
						'push',
						A2(
							_elm_lang$core$Basics_ops['++'],
							_rundis$elm_bootstrap$Bootstrap_Grid_Internal$screenSizeToPartialString(_p11._0.screenSize),
							_rundis$elm_bootstrap$Bootstrap_Grid_Internal$moveCountOption(_p11._0.moveCount)))));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	};
	return A2(
		_elm_lang$core$List$filterMap,
		_elm_lang$core$Basics$identity,
		A2(_elm_lang$core$List$map, push, pushes));
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$pullsToAttributes = function (pulls) {
	var pull = function (m) {
		var _p12 = m;
		if (_p12.ctor === 'Just') {
			return _elm_lang$core$Maybe$Just(
				_elm_lang$html$Html_Attributes$class(
					A2(
						_elm_lang$core$Basics_ops['++'],
						'pull',
						A2(
							_elm_lang$core$Basics_ops['++'],
							_rundis$elm_bootstrap$Bootstrap_Grid_Internal$screenSizeToPartialString(_p12._0.screenSize),
							_rundis$elm_bootstrap$Bootstrap_Grid_Internal$moveCountOption(_p12._0.moveCount)))));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	};
	return A2(
		_elm_lang$core$List$filterMap,
		_elm_lang$core$Basics$identity,
		A2(_elm_lang$core$List$map, pull, pulls));
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$offsetClass = function (_p13) {
	var _p14 = _p13;
	return _elm_lang$html$Html_Attributes$class(
		A2(
			_elm_lang$core$Basics_ops['++'],
			'offset',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_rundis$elm_bootstrap$Bootstrap_Grid_Internal$screenSizeToPartialString(_p14.screenSize),
				_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offsetCountOption(_p14.offsetCount))));
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$offsetsToAttributes = function (offsets) {
	var offset = function (m) {
		return A2(_elm_lang$core$Maybe$map, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$offsetClass, m);
	};
	return A2(
		_elm_lang$core$List$filterMap,
		_elm_lang$core$Basics$identity,
		A2(_elm_lang$core$List$map, offset, offsets));
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$colWidthClass = function (_p15) {
	var _p16 = _p15;
	return _elm_lang$html$Html_Attributes$class(
		A2(
			_elm_lang$core$Basics_ops['++'],
			'col',
			A2(
				_elm_lang$core$Basics_ops['++'],
				A2(
					_elm_lang$core$Maybe$withDefault,
					'',
					A2(
						_elm_lang$core$Maybe$map,
						function (v) {
							return A2(_elm_lang$core$Basics_ops['++'], '-', v);
						},
						_rundis$elm_bootstrap$Bootstrap_Grid_Internal$screenSizeOption(_p16.screenSize))),
				A2(
					_elm_lang$core$Maybe$withDefault,
					'',
					A2(
						_elm_lang$core$Maybe$map,
						function (v) {
							return A2(_elm_lang$core$Basics_ops['++'], '-', v);
						},
						_rundis$elm_bootstrap$Bootstrap_Grid_Internal$columnCountOption(_p16.columnCount))))));
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$colWidthsToAttributes = function (widths) {
	var width = function (w) {
		return A2(_elm_lang$core$Maybe$map, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$colWidthClass, w);
	};
	return A2(
		_elm_lang$core$List$filterMap,
		_elm_lang$core$Basics$identity,
		A2(_elm_lang$core$List$map, width, widths));
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$defaultRowOptions = {
	attributes: {ctor: '[]'},
	vAlignXs: _elm_lang$core$Maybe$Nothing,
	vAlignSm: _elm_lang$core$Maybe$Nothing,
	vAlignMd: _elm_lang$core$Maybe$Nothing,
	vAlignLg: _elm_lang$core$Maybe$Nothing,
	vAlignXl: _elm_lang$core$Maybe$Nothing,
	hAlignXs: _elm_lang$core$Maybe$Nothing,
	hAlignSm: _elm_lang$core$Maybe$Nothing,
	hAlignMd: _elm_lang$core$Maybe$Nothing,
	hAlignLg: _elm_lang$core$Maybe$Nothing,
	hAlignXl: _elm_lang$core$Maybe$Nothing
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$defaultColOptions = {
	attributes: {ctor: '[]'},
	widthXs: _elm_lang$core$Maybe$Nothing,
	widthSm: _elm_lang$core$Maybe$Nothing,
	widthMd: _elm_lang$core$Maybe$Nothing,
	widthLg: _elm_lang$core$Maybe$Nothing,
	widthXl: _elm_lang$core$Maybe$Nothing,
	offsetXs: _elm_lang$core$Maybe$Nothing,
	offsetSm: _elm_lang$core$Maybe$Nothing,
	offsetMd: _elm_lang$core$Maybe$Nothing,
	offsetLg: _elm_lang$core$Maybe$Nothing,
	offsetXl: _elm_lang$core$Maybe$Nothing,
	pullXs: _elm_lang$core$Maybe$Nothing,
	pullSm: _elm_lang$core$Maybe$Nothing,
	pullMd: _elm_lang$core$Maybe$Nothing,
	pullLg: _elm_lang$core$Maybe$Nothing,
	pullXl: _elm_lang$core$Maybe$Nothing,
	pushXs: _elm_lang$core$Maybe$Nothing,
	pushSm: _elm_lang$core$Maybe$Nothing,
	pushMd: _elm_lang$core$Maybe$Nothing,
	pushLg: _elm_lang$core$Maybe$Nothing,
	pushXl: _elm_lang$core$Maybe$Nothing,
	alignXs: _elm_lang$core$Maybe$Nothing,
	alignSm: _elm_lang$core$Maybe$Nothing,
	alignMd: _elm_lang$core$Maybe$Nothing,
	alignLg: _elm_lang$core$Maybe$Nothing,
	alignXl: _elm_lang$core$Maybe$Nothing
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$applyRowHAlign = F2(
	function (align, options) {
		var _p17 = align.screenSize;
		switch (_p17.ctor) {
			case 'XS':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						hAlignXs: _elm_lang$core$Maybe$Just(align)
					});
			case 'SM':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						hAlignSm: _elm_lang$core$Maybe$Just(align)
					});
			case 'MD':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						hAlignMd: _elm_lang$core$Maybe$Just(align)
					});
			case 'LG':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						hAlignLg: _elm_lang$core$Maybe$Just(align)
					});
			default:
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						hAlignXl: _elm_lang$core$Maybe$Just(align)
					});
		}
	});
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$applyRowVAlign = F2(
	function (align, options) {
		var _p18 = align.screenSize;
		switch (_p18.ctor) {
			case 'XS':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						vAlignXs: _elm_lang$core$Maybe$Just(align)
					});
			case 'SM':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						vAlignSm: _elm_lang$core$Maybe$Just(align)
					});
			case 'MD':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						vAlignMd: _elm_lang$core$Maybe$Just(align)
					});
			case 'LG':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						vAlignLg: _elm_lang$core$Maybe$Just(align)
					});
			default:
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						vAlignXl: _elm_lang$core$Maybe$Just(align)
					});
		}
	});
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$applyRowOption = F2(
	function (modifier, options) {
		var _p19 = modifier;
		switch (_p19.ctor) {
			case 'RowAttrs':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						attributes: A2(_elm_lang$core$Basics_ops['++'], options.attributes, _p19._0)
					});
			case 'RowVAlign':
				return A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$applyRowVAlign, _p19._0, options);
			default:
				return A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$applyRowHAlign, _p19._0, options);
		}
	});
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$applyColAlign = F2(
	function (align, options) {
		var _p20 = align.screenSize;
		switch (_p20.ctor) {
			case 'XS':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						alignXs: _elm_lang$core$Maybe$Just(align)
					});
			case 'SM':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						alignSm: _elm_lang$core$Maybe$Just(align)
					});
			case 'MD':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						alignMd: _elm_lang$core$Maybe$Just(align)
					});
			case 'LG':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						alignLg: _elm_lang$core$Maybe$Just(align)
					});
			default:
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						alignXl: _elm_lang$core$Maybe$Just(align)
					});
		}
	});
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$applyColPush = F2(
	function (push, options) {
		var _p21 = push.screenSize;
		switch (_p21.ctor) {
			case 'XS':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						pushXs: _elm_lang$core$Maybe$Just(push)
					});
			case 'SM':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						pushSm: _elm_lang$core$Maybe$Just(push)
					});
			case 'MD':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						pushMd: _elm_lang$core$Maybe$Just(push)
					});
			case 'LG':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						pushLg: _elm_lang$core$Maybe$Just(push)
					});
			default:
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						pushXl: _elm_lang$core$Maybe$Just(push)
					});
		}
	});
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$applyColPull = F2(
	function (pull, options) {
		var _p22 = pull.screenSize;
		switch (_p22.ctor) {
			case 'XS':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						pullXs: _elm_lang$core$Maybe$Just(pull)
					});
			case 'SM':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						pullSm: _elm_lang$core$Maybe$Just(pull)
					});
			case 'MD':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						pullMd: _elm_lang$core$Maybe$Just(pull)
					});
			case 'LG':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						pullLg: _elm_lang$core$Maybe$Just(pull)
					});
			default:
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						pullXl: _elm_lang$core$Maybe$Just(pull)
					});
		}
	});
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$applyColOffset = F2(
	function (offset, options) {
		var _p23 = offset.screenSize;
		switch (_p23.ctor) {
			case 'XS':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						offsetXs: _elm_lang$core$Maybe$Just(offset)
					});
			case 'SM':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						offsetSm: _elm_lang$core$Maybe$Just(offset)
					});
			case 'MD':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						offsetMd: _elm_lang$core$Maybe$Just(offset)
					});
			case 'LG':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						offsetLg: _elm_lang$core$Maybe$Just(offset)
					});
			default:
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						offsetXl: _elm_lang$core$Maybe$Just(offset)
					});
		}
	});
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$applyColWidth = F2(
	function (width, options) {
		var _p24 = width.screenSize;
		switch (_p24.ctor) {
			case 'XS':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						widthXs: _elm_lang$core$Maybe$Just(width)
					});
			case 'SM':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						widthSm: _elm_lang$core$Maybe$Just(width)
					});
			case 'MD':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						widthMd: _elm_lang$core$Maybe$Just(width)
					});
			case 'LG':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						widthLg: _elm_lang$core$Maybe$Just(width)
					});
			default:
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						widthXl: _elm_lang$core$Maybe$Just(width)
					});
		}
	});
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$applyColOption = F2(
	function (modifier, options) {
		var _p25 = modifier;
		switch (_p25.ctor) {
			case 'ColAttrs':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						attributes: A2(_elm_lang$core$Basics_ops['++'], options.attributes, _p25._0)
					});
			case 'ColWidth':
				return A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$applyColWidth, _p25._0, options);
			case 'ColOffset':
				return A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$applyColOffset, _p25._0, options);
			case 'ColPull':
				return A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$applyColPull, _p25._0, options);
			case 'ColPush':
				return A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$applyColPush, _p25._0, options);
			default:
				return A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$applyColAlign, _p25._0, options);
		}
	});
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowAttributes = function (modifiers) {
	var options = A3(_elm_lang$core$List$foldl, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$applyRowOption, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$defaultRowOptions, modifiers);
	return A2(
		_elm_lang$core$Basics_ops['++'],
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('row'),
			_1: {ctor: '[]'}
		},
		A2(
			_elm_lang$core$Basics_ops['++'],
			A2(
				_rundis$elm_bootstrap$Bootstrap_Grid_Internal$vAlignsToAttributes,
				'align-items-',
				{
					ctor: '::',
					_0: options.vAlignXs,
					_1: {
						ctor: '::',
						_0: options.vAlignSm,
						_1: {
							ctor: '::',
							_0: options.vAlignMd,
							_1: {
								ctor: '::',
								_0: options.vAlignLg,
								_1: {
									ctor: '::',
									_0: options.vAlignXl,
									_1: {ctor: '[]'}
								}
							}
						}
					}
				}),
			A2(
				_elm_lang$core$Basics_ops['++'],
				_rundis$elm_bootstrap$Bootstrap_Grid_Internal$hAlignsToAttributes(
					{
						ctor: '::',
						_0: options.hAlignXs,
						_1: {
							ctor: '::',
							_0: options.hAlignSm,
							_1: {
								ctor: '::',
								_0: options.hAlignMd,
								_1: {
									ctor: '::',
									_0: options.hAlignLg,
									_1: {
										ctor: '::',
										_0: options.hAlignXl,
										_1: {ctor: '[]'}
									}
								}
							}
						}
					}),
				options.attributes)));
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Width = F2(
	function (a, b) {
		return {screenSize: a, columnCount: b};
	});
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset = F2(
	function (a, b) {
		return {screenSize: a, offsetCount: b};
	});
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Pull = F2(
	function (a, b) {
		return {screenSize: a, moveCount: b};
	});
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Push = F2(
	function (a, b) {
		return {screenSize: a, moveCount: b};
	});
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$VAlign = F2(
	function (a, b) {
		return {screenSize: a, align: b};
	});
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$HAlign = F2(
	function (a, b) {
		return {screenSize: a, align: b};
	});
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$ColOptions = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return function (k) {
											return function (l) {
												return function (m) {
													return function (n) {
														return function (o) {
															return function (p) {
																return function (q) {
																	return function (r) {
																		return function (s) {
																			return function (t) {
																				return function (u) {
																					return function (v) {
																						return function (w) {
																							return function (x) {
																								return function (y) {
																									return function (z) {
																										return {attributes: a, widthXs: b, widthSm: c, widthMd: d, widthLg: e, widthXl: f, offsetXs: g, offsetSm: h, offsetMd: i, offsetLg: j, offsetXl: k, pullXs: l, pullSm: m, pullMd: n, pullLg: o, pullXl: p, pushXs: q, pushSm: r, pushMd: s, pushLg: t, pushXl: u, alignXs: v, alignSm: w, alignMd: x, alignLg: y, alignXl: z};
																									};
																								};
																							};
																						};
																					};
																				};
																			};
																		};
																	};
																};
															};
														};
													};
												};
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$RowOptions = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return function (k) {
											return {attributes: a, vAlignXs: b, vAlignSm: c, vAlignMd: d, vAlignLg: e, vAlignXl: f, hAlignXs: g, hAlignSm: h, hAlignMd: i, hAlignLg: j, hAlignXl: k};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$ColAttrs = function (a) {
	return {ctor: 'ColAttrs', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$ColAlign = function (a) {
	return {ctor: 'ColAlign', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$colVAlign = F2(
	function (size, align) {
		return _rundis$elm_bootstrap$Bootstrap_Grid_Internal$ColAlign(
			A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$VAlign, size, align));
	});
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$ColPush = function (a) {
	return {ctor: 'ColPush', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$push = F2(
	function (size, count) {
		return _rundis$elm_bootstrap$Bootstrap_Grid_Internal$ColPush(
			A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$Push, size, count));
	});
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$ColPull = function (a) {
	return {ctor: 'ColPull', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull = F2(
	function (size, count) {
		return _rundis$elm_bootstrap$Bootstrap_Grid_Internal$ColPull(
			A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$Pull, size, count));
	});
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$ColOffset = function (a) {
	return {ctor: 'ColOffset', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset = F2(
	function (size, count) {
		return _rundis$elm_bootstrap$Bootstrap_Grid_Internal$ColOffset(
			A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset, size, count));
	});
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$ColWidth = function (a) {
	return {ctor: 'ColWidth', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$width = F2(
	function (size, count) {
		return _rundis$elm_bootstrap$Bootstrap_Grid_Internal$ColWidth(
			A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$Width, size, count));
	});
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$RowAttrs = function (a) {
	return {ctor: 'RowAttrs', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$RowHAlign = function (a) {
	return {ctor: 'RowHAlign', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowHAlign = F2(
	function (size, align) {
		return _rundis$elm_bootstrap$Bootstrap_Grid_Internal$RowHAlign(
			A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$HAlign, size, align));
	});
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$RowVAlign = function (a) {
	return {ctor: 'RowVAlign', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowVAlign = F2(
	function (size, align) {
		return _rundis$elm_bootstrap$Bootstrap_Grid_Internal$RowVAlign(
			A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$VAlign, size, align));
	});
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL = {ctor: 'XL'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG = {ctor: 'LG'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD = {ctor: 'MD'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM = {ctor: 'SM'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS = {ctor: 'XS'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$ColAuto = {ctor: 'ColAuto'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col12 = {ctor: 'Col12'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col11 = {ctor: 'Col11'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col10 = {ctor: 'Col10'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col9 = {ctor: 'Col9'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col8 = {ctor: 'Col8'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col7 = {ctor: 'Col7'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col6 = {ctor: 'Col6'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col5 = {ctor: 'Col5'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col4 = {ctor: 'Col4'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col3 = {ctor: 'Col3'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col2 = {ctor: 'Col2'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col1 = {ctor: 'Col1'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col = {ctor: 'Col'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$colAttributes = function (modifiers) {
	var options = A3(_elm_lang$core$List$foldl, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$applyColOption, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$defaultColOptions, modifiers);
	var shouldAddDefaultXs = _elm_lang$core$Native_Utils.eq(
		_elm_lang$core$List$length(
			A2(
				_elm_lang$core$List$filterMap,
				_elm_lang$core$Basics$identity,
				{
					ctor: '::',
					_0: options.widthXs,
					_1: {
						ctor: '::',
						_0: options.widthSm,
						_1: {
							ctor: '::',
							_0: options.widthMd,
							_1: {
								ctor: '::',
								_0: options.widthLg,
								_1: {
									ctor: '::',
									_0: options.widthXl,
									_1: {ctor: '[]'}
								}
							}
						}
					}
				})),
		0);
	return A2(
		_elm_lang$core$Basics_ops['++'],
		_rundis$elm_bootstrap$Bootstrap_Grid_Internal$colWidthsToAttributes(
			{
				ctor: '::',
				_0: shouldAddDefaultXs ? _elm_lang$core$Maybe$Just(
					A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$Width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col)) : options.widthXs,
				_1: {
					ctor: '::',
					_0: options.widthSm,
					_1: {
						ctor: '::',
						_0: options.widthMd,
						_1: {
							ctor: '::',
							_0: options.widthLg,
							_1: {
								ctor: '::',
								_0: options.widthXl,
								_1: {ctor: '[]'}
							}
						}
					}
				}
			}),
		A2(
			_elm_lang$core$Basics_ops['++'],
			_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offsetsToAttributes(
				{
					ctor: '::',
					_0: options.offsetXs,
					_1: {
						ctor: '::',
						_0: options.offsetSm,
						_1: {
							ctor: '::',
							_0: options.offsetMd,
							_1: {
								ctor: '::',
								_0: options.offsetLg,
								_1: {
									ctor: '::',
									_0: options.offsetXl,
									_1: {ctor: '[]'}
								}
							}
						}
					}
				}),
			A2(
				_elm_lang$core$Basics_ops['++'],
				_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pullsToAttributes(
					{
						ctor: '::',
						_0: options.pullXs,
						_1: {
							ctor: '::',
							_0: options.pullSm,
							_1: {
								ctor: '::',
								_0: options.pullMd,
								_1: {
									ctor: '::',
									_0: options.pullLg,
									_1: {
										ctor: '::',
										_0: options.pullXl,
										_1: {ctor: '[]'}
									}
								}
							}
						}
					}),
				A2(
					_elm_lang$core$Basics_ops['++'],
					_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pushesToAttributes(
						{
							ctor: '::',
							_0: options.pushXs,
							_1: {
								ctor: '::',
								_0: options.pushSm,
								_1: {
									ctor: '::',
									_0: options.pushMd,
									_1: {
										ctor: '::',
										_0: options.pushLg,
										_1: {
											ctor: '::',
											_0: options.pushXl,
											_1: {ctor: '[]'}
										}
									}
								}
							}
						}),
					A2(
						_elm_lang$core$Basics_ops['++'],
						A2(
							_rundis$elm_bootstrap$Bootstrap_Grid_Internal$vAlignsToAttributes,
							'align-self-',
							{
								ctor: '::',
								_0: options.alignXs,
								_1: {
									ctor: '::',
									_0: options.alignSm,
									_1: {
										ctor: '::',
										_0: options.alignMd,
										_1: {
											ctor: '::',
											_0: options.alignLg,
											_1: {
												ctor: '::',
												_0: options.alignXl,
												_1: {ctor: '[]'}
											}
										}
									}
								}
							}),
						options.attributes)))));
};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset11 = {ctor: 'Offset11'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset10 = {ctor: 'Offset10'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset9 = {ctor: 'Offset9'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset8 = {ctor: 'Offset8'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset7 = {ctor: 'Offset7'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset6 = {ctor: 'Offset6'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset5 = {ctor: 'Offset5'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset4 = {ctor: 'Offset4'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset3 = {ctor: 'Offset3'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset2 = {ctor: 'Offset2'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset1 = {ctor: 'Offset1'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset0 = {ctor: 'Offset0'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move12 = {ctor: 'Move12'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move11 = {ctor: 'Move11'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move10 = {ctor: 'Move10'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move9 = {ctor: 'Move9'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move8 = {ctor: 'Move8'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move7 = {ctor: 'Move7'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move6 = {ctor: 'Move6'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move5 = {ctor: 'Move5'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move4 = {ctor: 'Move4'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move3 = {ctor: 'Move3'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move2 = {ctor: 'Move2'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move1 = {ctor: 'Move1'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move0 = {ctor: 'Move0'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Bottom = {ctor: 'Bottom'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Middle = {ctor: 'Middle'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Top = {ctor: 'Top'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Between = {ctor: 'Between'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Around = {ctor: 'Around'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Right = {ctor: 'Right'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Center = {ctor: 'Center'};
var _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Left = {ctor: 'Left'};

var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushXl12 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move12);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushXl11 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move11);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushXl10 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move10);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushXl9 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move9);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushXl8 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move8);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushXl7 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move7);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushXl6 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move6);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushXl5 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move5);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushXl4 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move4);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushXl3 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move3);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushXl2 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move2);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushXl1 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move1);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushXl0 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move0);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushLg12 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move12);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushLg11 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move11);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushLg10 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move10);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushLg9 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move9);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushLg8 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move8);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushLg7 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move7);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushLg6 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move6);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushLg5 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move5);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushLg4 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move4);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushLg3 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move3);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushLg2 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move2);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushLg1 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move1);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushLg0 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move0);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushMd12 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move12);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushMd11 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move11);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushMd10 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move10);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushMd9 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move9);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushMd8 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move8);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushMd7 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move7);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushMd6 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move6);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushMd5 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move5);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushMd4 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move4);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushMd3 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move3);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushMd2 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move2);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushMd1 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move1);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushMd0 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move0);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushSm12 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move12);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushSm11 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move11);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushSm10 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move10);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushSm9 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move9);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushSm8 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move8);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushSm7 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move7);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushSm6 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move6);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushSm5 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move5);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushSm4 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move4);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushSm3 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move3);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushSm2 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move2);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushSm1 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move1);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushSm0 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move0);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushXs12 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move12);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushXs11 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move11);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushXs10 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move10);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushXs9 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move9);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushXs8 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move8);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushXs7 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move7);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushXs6 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move6);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushXs5 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move5);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushXs4 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move4);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushXs3 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move3);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushXs2 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move2);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushXs1 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move1);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pushXs0 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$push, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move0);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullXl12 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move12);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullXl11 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move11);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullXl10 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move10);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullXl9 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move9);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullXl8 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move8);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullXl7 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move7);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullXl6 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move6);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullXl5 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move5);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullXl4 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move4);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullXl3 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move3);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullXl2 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move2);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullXl1 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move1);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullXl0 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move0);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullLg12 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move12);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullLg11 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move11);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullLg10 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move10);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullLg9 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move9);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullLg8 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move8);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullLg7 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move7);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullLg6 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move6);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullLg5 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move5);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullLg4 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move4);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullLg3 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move3);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullLg2 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move2);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullLg1 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move1);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullLg0 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move0);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullMd12 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move12);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullMd11 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move11);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullMd10 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move10);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullMd9 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move9);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullMd8 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move8);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullMd7 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move7);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullMd6 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move6);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullMd5 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move5);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullMd4 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move4);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullMd3 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move3);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullMd2 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move2);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullMd1 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move1);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullMd0 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move0);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullSm12 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move12);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullSm11 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move11);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullSm10 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move10);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullSm9 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move9);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullSm8 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move8);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullSm7 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move7);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullSm6 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move6);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullSm5 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move5);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullSm4 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move4);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullSm3 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move3);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullSm2 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move2);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullSm1 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move1);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullSm0 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move0);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullXs12 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move12);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullXs11 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move11);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullXs10 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move10);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullXs9 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move9);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullXs8 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move8);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullXs7 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move7);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullXs6 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move6);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullXs5 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move5);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullXs4 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move4);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullXs3 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move3);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullXs2 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move2);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullXs1 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move1);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$pullXs0 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$pull, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Move0);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetXl11 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset11);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetXl10 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset10);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetXl9 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset9);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetXl8 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset8);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetXl7 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset7);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetXl6 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset6);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetXl5 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset5);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetXl4 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset4);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetXl3 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset3);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetXl2 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset2);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetXl1 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset1);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetXl0 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset0);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetLg11 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset11);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetLg10 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset10);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetLg9 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset9);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetLg8 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset8);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetLg7 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset7);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetLg6 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset6);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetLg5 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset5);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetLg4 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset4);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetLg3 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset3);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetLg2 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset2);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetLg1 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset1);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetLg0 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset0);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetMd11 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset11);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetMd10 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset10);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetMd9 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset9);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetMd8 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset8);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetMd7 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset7);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetMd6 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset6);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetMd5 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset5);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetMd4 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset4);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetMd3 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset3);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetMd2 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset2);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetMd1 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset1);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetMd0 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset0);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetSm11 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset11);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetSm10 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset10);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetSm9 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset9);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetSm8 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset8);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetSm7 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset7);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetSm6 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset6);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetSm5 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset5);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetSm4 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset4);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetSm3 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset3);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetSm2 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset2);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetSm1 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset1);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetSm0 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset0);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetXs11 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset11);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetXs10 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset10);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetXs9 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset9);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetXs8 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset8);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetXs7 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset7);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetXs6 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset6);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetXs5 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset5);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetXs4 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset4);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetXs3 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset3);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetXs2 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset2);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$offsetXs1 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$offset, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Offset1);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xlAuto = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$ColAuto);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xl12 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col12);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xl11 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col11);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xl10 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col10);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xl9 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col9);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xl8 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col8);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xl7 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col7);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xl6 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col6);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xl5 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col5);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xl4 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col4);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xl3 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col3);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xl2 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col2);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xl1 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col1);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xl = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$lgAuto = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$ColAuto);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$lg12 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col12);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$lg11 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col11);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$lg10 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col10);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$lg9 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col9);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$lg8 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col8);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$lg7 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col7);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$lg6 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col6);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$lg5 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col5);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$lg4 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col4);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$lg3 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col3);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$lg2 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col2);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$lg1 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col1);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$lg = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$mdAuto = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$ColAuto);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$md12 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col12);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$md11 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col11);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$md10 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col10);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$md9 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col9);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$md8 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col8);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$md7 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col7);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$md6 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col6);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$md5 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col5);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$md4 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col4);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$md3 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col3);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$md2 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col2);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$md1 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col1);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$md = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$smAuto = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$ColAuto);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$sm12 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col12);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$sm11 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col11);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$sm10 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col10);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$sm9 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col9);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$sm8 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col8);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$sm7 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col7);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$sm6 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col6);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$sm5 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col5);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$sm4 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col4);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$sm3 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col3);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$sm2 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col2);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$sm1 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col1);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$sm = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xsAuto = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$ColAuto);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xs12 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col12);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xs11 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col11);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xs10 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col10);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xs9 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col9);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xs8 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col8);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xs7 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col7);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xs6 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col6);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xs5 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col5);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xs4 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col4);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xs3 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col3);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xs2 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col2);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xs1 = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col1);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$xs = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$width, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Col);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$bottomXl = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$colVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Bottom);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$bottomLg = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$colVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Bottom);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$bottomMd = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$colVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Bottom);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$bottomSm = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$colVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Bottom);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$bottomXs = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$colVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Bottom);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$middleXl = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$colVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Middle);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$middleLg = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$colVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Middle);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$middleMd = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$colVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Middle);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$middleSm = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$colVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Middle);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$middleXs = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$colVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Middle);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$topXl = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$colVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Top);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$topLg = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$colVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Top);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$topMd = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$colVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Top);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$topSm = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$colVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Top);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$topXs = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$colVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Top);
var _rundis$elm_bootstrap$Bootstrap_Grid_Col$attrs = function (attrs) {
	return _rundis$elm_bootstrap$Bootstrap_Grid_Internal$ColAttrs(attrs);
};

var _rundis$elm_bootstrap$Bootstrap_Grid_Row$betweenXl = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowHAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Between);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$betweenLg = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowHAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Between);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$betweenMd = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowHAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Between);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$betweenSm = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowHAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Between);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$betweenXs = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowHAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Between);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$aroundXl = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowHAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Around);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$aroundLg = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowHAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Around);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$aroundMd = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowHAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Around);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$aroundSm = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowHAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Around);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$aroundXs = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowHAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Around);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$rightXl = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowHAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Right);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$rightLg = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowHAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Right);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$rightMd = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowHAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Right);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$rightSm = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowHAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Right);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$rightXs = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowHAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Right);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$centerXl = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowHAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Center);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$centerLg = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowHAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Center);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$centerMd = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowHAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Center);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$centerSm = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowHAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Center);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$centerXs = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowHAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Center);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$leftXl = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowHAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Left);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$leftLg = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowHAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Left);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$leftMd = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowHAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Left);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$leftSm = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowHAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Left);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$leftXs = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowHAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Left);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$bottomXl = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Bottom);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$bottomLg = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Bottom);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$bottomMd = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Bottom);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$bottomSm = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Bottom);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$bottomXs = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Bottom);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$middleXl = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Middle);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$middleLg = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Middle);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$middleMd = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Middle);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$middleSm = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Middle);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$middleXs = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Middle);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$topXl = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XL, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Top);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$topLg = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Top);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$topMd = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$MD, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Top);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$topSm = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Top);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$topXs = A2(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowVAlign, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$XS, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$Top);
var _rundis$elm_bootstrap$Bootstrap_Grid_Row$attrs = function (attrs) {
	return _rundis$elm_bootstrap$Bootstrap_Grid_Internal$RowAttrs(attrs);
};

var _rundis$elm_bootstrap$Bootstrap_Grid$renderCol = function (column) {
	var _p0 = column;
	switch (_p0.ctor) {
		case 'Column':
			return A2(
				_elm_lang$html$Html$div,
				_rundis$elm_bootstrap$Bootstrap_Grid_Internal$colAttributes(_p0._0.options),
				_p0._0.children);
		case 'ColBreak':
			return _p0._0;
		default:
			return A3(
				_elm_lang$html$Html_Keyed$node,
				'div',
				_rundis$elm_bootstrap$Bootstrap_Grid_Internal$colAttributes(_p0._0.options),
				_p0._0.children);
	}
};
var _rundis$elm_bootstrap$Bootstrap_Grid$keyedRow = F2(
	function (options, keyedCols) {
		return A3(
			_elm_lang$html$Html_Keyed$node,
			'div',
			_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowAttributes(options),
			A2(
				_elm_lang$core$List$map,
				function (_p1) {
					var _p2 = _p1;
					return {
						ctor: '_Tuple2',
						_0: _p2._0,
						_1: _rundis$elm_bootstrap$Bootstrap_Grid$renderCol(_p2._1)
					};
				},
				keyedCols));
	});
var _rundis$elm_bootstrap$Bootstrap_Grid$row = F2(
	function (options, cols) {
		return A2(
			_elm_lang$html$Html$div,
			_rundis$elm_bootstrap$Bootstrap_Grid_Internal$rowAttributes(options),
			A2(_elm_lang$core$List$map, _rundis$elm_bootstrap$Bootstrap_Grid$renderCol, cols));
	});
var _rundis$elm_bootstrap$Bootstrap_Grid$simpleRow = function (cols) {
	return A2(
		_rundis$elm_bootstrap$Bootstrap_Grid$row,
		{ctor: '[]'},
		cols);
};
var _rundis$elm_bootstrap$Bootstrap_Grid$containerFluid = F2(
	function (attributes, children) {
		return A2(
			_elm_lang$html$Html$div,
			A2(
				_elm_lang$core$Basics_ops['++'],
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('container-fluid'),
					_1: {ctor: '[]'}
				},
				attributes),
			children);
	});
var _rundis$elm_bootstrap$Bootstrap_Grid$container = F2(
	function (attributes, children) {
		return A2(
			_elm_lang$html$Html$div,
			A2(
				_elm_lang$core$Basics_ops['++'],
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('container'),
					_1: {ctor: '[]'}
				},
				attributes),
			children);
	});
var _rundis$elm_bootstrap$Bootstrap_Grid$KeyedColumn = function (a) {
	return {ctor: 'KeyedColumn', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Grid$keyedCol = F2(
	function (options, children) {
		return _rundis$elm_bootstrap$Bootstrap_Grid$KeyedColumn(
			{options: options, children: children});
	});
var _rundis$elm_bootstrap$Bootstrap_Grid$ColBreak = function (a) {
	return {ctor: 'ColBreak', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Grid$colBreak = function (attributes) {
	return _rundis$elm_bootstrap$Bootstrap_Grid$ColBreak(
		A2(
			_elm_lang$html$Html$div,
			A2(
				_elm_lang$core$Basics_ops['++'],
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('w-100'),
					_1: {ctor: '[]'}
				},
				attributes),
			{ctor: '[]'}));
};
var _rundis$elm_bootstrap$Bootstrap_Grid$Column = function (a) {
	return {ctor: 'Column', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Grid$col = F2(
	function (options, children) {
		return _rundis$elm_bootstrap$Bootstrap_Grid$Column(
			{options: options, children: children});
	});

var _rundis$elm_bootstrap$Bootstrap_Internal_Button$roleClass = function (role) {
	var _p0 = role;
	switch (_p0.ctor) {
		case 'Primary':
			return 'primary';
		case 'Secondary':
			return 'secondary';
		case 'Success':
			return 'success';
		case 'Info':
			return 'info';
		case 'Warning':
			return 'warning';
		case 'Danger':
			return 'danger';
		case 'Dark':
			return 'dark';
		case 'Light':
			return 'light';
		default:
			return 'link';
	}
};
var _rundis$elm_bootstrap$Bootstrap_Internal_Button$applyModifier = F2(
	function (modifier, options) {
		var _p1 = modifier;
		switch (_p1.ctor) {
			case 'Size':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						size: _elm_lang$core$Maybe$Just(_p1._0)
					});
			case 'Coloring':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						coloring: _elm_lang$core$Maybe$Just(_p1._0)
					});
			case 'Block':
				return _elm_lang$core$Native_Utils.update(
					options,
					{block: true});
			case 'Disabled':
				return _elm_lang$core$Native_Utils.update(
					options,
					{disabled: _p1._0});
			default:
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						attributes: A2(_elm_lang$core$Basics_ops['++'], options.attributes, _p1._0)
					});
		}
	});
var _rundis$elm_bootstrap$Bootstrap_Internal_Button$defaultOptions = {
	coloring: _elm_lang$core$Maybe$Nothing,
	block: false,
	disabled: false,
	size: _elm_lang$core$Maybe$Nothing,
	attributes: {ctor: '[]'}
};
var _rundis$elm_bootstrap$Bootstrap_Internal_Button$buttonAttributes = function (modifiers) {
	var options = A3(_elm_lang$core$List$foldl, _rundis$elm_bootstrap$Bootstrap_Internal_Button$applyModifier, _rundis$elm_bootstrap$Bootstrap_Internal_Button$defaultOptions, modifiers);
	return A2(
		_elm_lang$core$Basics_ops['++'],
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$classList(
				{
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'btn', _1: true},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'btn-block', _1: options.block},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'disabled', _1: options.disabled},
							_1: {ctor: '[]'}
						}
					}
				}),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$disabled(options.disabled),
				_1: {ctor: '[]'}
			}
		},
		A2(
			_elm_lang$core$Basics_ops['++'],
			function () {
				var _p2 = A2(_elm_lang$core$Maybe$andThen, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$screenSizeOption, options.size);
				if (_p2.ctor === 'Just') {
					return {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class(
							A2(_elm_lang$core$Basics_ops['++'], 'btn-', _p2._0)),
						_1: {ctor: '[]'}
					};
				} else {
					return {ctor: '[]'};
				}
			}(),
			A2(
				_elm_lang$core$Basics_ops['++'],
				function () {
					var _p3 = options.coloring;
					if (_p3.ctor === 'Just') {
						if (_p3._0.ctor === 'Roled') {
							return {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class(
									A2(
										_elm_lang$core$Basics_ops['++'],
										'btn-',
										_rundis$elm_bootstrap$Bootstrap_Internal_Button$roleClass(_p3._0._0))),
								_1: {ctor: '[]'}
							};
						} else {
							return {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class(
									A2(
										_elm_lang$core$Basics_ops['++'],
										'btn-outline-',
										_rundis$elm_bootstrap$Bootstrap_Internal_Button$roleClass(_p3._0._0))),
								_1: {ctor: '[]'}
							};
						}
					} else {
						return {ctor: '[]'};
					}
				}(),
				options.attributes)));
};
var _rundis$elm_bootstrap$Bootstrap_Internal_Button$Options = F5(
	function (a, b, c, d, e) {
		return {coloring: a, block: b, disabled: c, size: d, attributes: e};
	});
var _rundis$elm_bootstrap$Bootstrap_Internal_Button$Attrs = function (a) {
	return {ctor: 'Attrs', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Internal_Button$Disabled = function (a) {
	return {ctor: 'Disabled', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Internal_Button$Block = {ctor: 'Block'};
var _rundis$elm_bootstrap$Bootstrap_Internal_Button$Coloring = function (a) {
	return {ctor: 'Coloring', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Internal_Button$Size = function (a) {
	return {ctor: 'Size', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Internal_Button$Outlined = function (a) {
	return {ctor: 'Outlined', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Internal_Button$Roled = function (a) {
	return {ctor: 'Roled', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Internal_Button$Link = {ctor: 'Link'};
var _rundis$elm_bootstrap$Bootstrap_Internal_Button$Light = {ctor: 'Light'};
var _rundis$elm_bootstrap$Bootstrap_Internal_Button$Dark = {ctor: 'Dark'};
var _rundis$elm_bootstrap$Bootstrap_Internal_Button$Danger = {ctor: 'Danger'};
var _rundis$elm_bootstrap$Bootstrap_Internal_Button$Warning = {ctor: 'Warning'};
var _rundis$elm_bootstrap$Bootstrap_Internal_Button$Info = {ctor: 'Info'};
var _rundis$elm_bootstrap$Bootstrap_Internal_Button$Success = {ctor: 'Success'};
var _rundis$elm_bootstrap$Bootstrap_Internal_Button$Secondary = {ctor: 'Secondary'};
var _rundis$elm_bootstrap$Bootstrap_Internal_Button$Primary = {ctor: 'Primary'};

var _rundis$elm_bootstrap$Bootstrap_Button$disabled = function (disabled) {
	return _rundis$elm_bootstrap$Bootstrap_Internal_Button$Disabled(disabled);
};
var _rundis$elm_bootstrap$Bootstrap_Button$block = _rundis$elm_bootstrap$Bootstrap_Internal_Button$Block;
var _rundis$elm_bootstrap$Bootstrap_Button$outlineDark = _rundis$elm_bootstrap$Bootstrap_Internal_Button$Coloring(
	_rundis$elm_bootstrap$Bootstrap_Internal_Button$Outlined(_rundis$elm_bootstrap$Bootstrap_Internal_Button$Dark));
var _rundis$elm_bootstrap$Bootstrap_Button$outlineLight = _rundis$elm_bootstrap$Bootstrap_Internal_Button$Coloring(
	_rundis$elm_bootstrap$Bootstrap_Internal_Button$Outlined(_rundis$elm_bootstrap$Bootstrap_Internal_Button$Light));
var _rundis$elm_bootstrap$Bootstrap_Button$outlineDanger = _rundis$elm_bootstrap$Bootstrap_Internal_Button$Coloring(
	_rundis$elm_bootstrap$Bootstrap_Internal_Button$Outlined(_rundis$elm_bootstrap$Bootstrap_Internal_Button$Danger));
var _rundis$elm_bootstrap$Bootstrap_Button$outlineWarning = _rundis$elm_bootstrap$Bootstrap_Internal_Button$Coloring(
	_rundis$elm_bootstrap$Bootstrap_Internal_Button$Outlined(_rundis$elm_bootstrap$Bootstrap_Internal_Button$Warning));
var _rundis$elm_bootstrap$Bootstrap_Button$outlineInfo = _rundis$elm_bootstrap$Bootstrap_Internal_Button$Coloring(
	_rundis$elm_bootstrap$Bootstrap_Internal_Button$Outlined(_rundis$elm_bootstrap$Bootstrap_Internal_Button$Info));
var _rundis$elm_bootstrap$Bootstrap_Button$outlineSuccess = _rundis$elm_bootstrap$Bootstrap_Internal_Button$Coloring(
	_rundis$elm_bootstrap$Bootstrap_Internal_Button$Outlined(_rundis$elm_bootstrap$Bootstrap_Internal_Button$Success));
var _rundis$elm_bootstrap$Bootstrap_Button$outlineSecondary = _rundis$elm_bootstrap$Bootstrap_Internal_Button$Coloring(
	_rundis$elm_bootstrap$Bootstrap_Internal_Button$Outlined(_rundis$elm_bootstrap$Bootstrap_Internal_Button$Secondary));
var _rundis$elm_bootstrap$Bootstrap_Button$outlinePrimary = _rundis$elm_bootstrap$Bootstrap_Internal_Button$Coloring(
	_rundis$elm_bootstrap$Bootstrap_Internal_Button$Outlined(_rundis$elm_bootstrap$Bootstrap_Internal_Button$Primary));
var _rundis$elm_bootstrap$Bootstrap_Button$roleLink = _rundis$elm_bootstrap$Bootstrap_Internal_Button$Coloring(
	_rundis$elm_bootstrap$Bootstrap_Internal_Button$Roled(_rundis$elm_bootstrap$Bootstrap_Internal_Button$Link));
var _rundis$elm_bootstrap$Bootstrap_Button$dark = _rundis$elm_bootstrap$Bootstrap_Internal_Button$Coloring(
	_rundis$elm_bootstrap$Bootstrap_Internal_Button$Roled(_rundis$elm_bootstrap$Bootstrap_Internal_Button$Dark));
var _rundis$elm_bootstrap$Bootstrap_Button$light = _rundis$elm_bootstrap$Bootstrap_Internal_Button$Coloring(
	_rundis$elm_bootstrap$Bootstrap_Internal_Button$Roled(_rundis$elm_bootstrap$Bootstrap_Internal_Button$Light));
var _rundis$elm_bootstrap$Bootstrap_Button$danger = _rundis$elm_bootstrap$Bootstrap_Internal_Button$Coloring(
	_rundis$elm_bootstrap$Bootstrap_Internal_Button$Roled(_rundis$elm_bootstrap$Bootstrap_Internal_Button$Danger));
var _rundis$elm_bootstrap$Bootstrap_Button$warning = _rundis$elm_bootstrap$Bootstrap_Internal_Button$Coloring(
	_rundis$elm_bootstrap$Bootstrap_Internal_Button$Roled(_rundis$elm_bootstrap$Bootstrap_Internal_Button$Warning));
var _rundis$elm_bootstrap$Bootstrap_Button$info = _rundis$elm_bootstrap$Bootstrap_Internal_Button$Coloring(
	_rundis$elm_bootstrap$Bootstrap_Internal_Button$Roled(_rundis$elm_bootstrap$Bootstrap_Internal_Button$Info));
var _rundis$elm_bootstrap$Bootstrap_Button$success = _rundis$elm_bootstrap$Bootstrap_Internal_Button$Coloring(
	_rundis$elm_bootstrap$Bootstrap_Internal_Button$Roled(_rundis$elm_bootstrap$Bootstrap_Internal_Button$Success));
var _rundis$elm_bootstrap$Bootstrap_Button$secondary = _rundis$elm_bootstrap$Bootstrap_Internal_Button$Coloring(
	_rundis$elm_bootstrap$Bootstrap_Internal_Button$Roled(_rundis$elm_bootstrap$Bootstrap_Internal_Button$Secondary));
var _rundis$elm_bootstrap$Bootstrap_Button$primary = _rundis$elm_bootstrap$Bootstrap_Internal_Button$Coloring(
	_rundis$elm_bootstrap$Bootstrap_Internal_Button$Roled(_rundis$elm_bootstrap$Bootstrap_Internal_Button$Primary));
var _rundis$elm_bootstrap$Bootstrap_Button$large = _rundis$elm_bootstrap$Bootstrap_Internal_Button$Size(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG);
var _rundis$elm_bootstrap$Bootstrap_Button$small = _rundis$elm_bootstrap$Bootstrap_Internal_Button$Size(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM);
var _rundis$elm_bootstrap$Bootstrap_Button$attrs = function (attrs) {
	return _rundis$elm_bootstrap$Bootstrap_Internal_Button$Attrs(attrs);
};
var _rundis$elm_bootstrap$Bootstrap_Button$onClick = function (message) {
	var defaultOptions = _elm_lang$html$Html_Events$defaultOptions;
	return _rundis$elm_bootstrap$Bootstrap_Button$attrs(
		{
			ctor: '::',
			_0: A3(
				_elm_lang$html$Html_Events$onWithOptions,
				'click',
				_elm_lang$core$Native_Utils.update(
					defaultOptions,
					{preventDefault: true}),
				_elm_lang$core$Json_Decode$succeed(message)),
			_1: {ctor: '[]'}
		});
};
var _rundis$elm_bootstrap$Bootstrap_Button$checkboxButton = F3(
	function (checked, options, children) {
		return A2(
			_elm_lang$html$Html$label,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$classList(
					{
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'active', _1: checked},
						_1: {ctor: '[]'}
					}),
				_1: _rundis$elm_bootstrap$Bootstrap_Internal_Button$buttonAttributes(options)
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$input,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$type_('checkbox'),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$checked(checked),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$autocomplete(false),
								_1: {ctor: '[]'}
							}
						}
					},
					{ctor: '[]'}),
				_1: children
			});
	});
var _rundis$elm_bootstrap$Bootstrap_Button$radioButton = F3(
	function (checked, options, children) {
		var hideRadio = A2(_elm_lang$html$Html_Attributes$attribute, 'data-toggle', 'button');
		return A2(
			_elm_lang$html$Html$label,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$classList(
					{
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'active', _1: checked},
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: hideRadio,
					_1: _rundis$elm_bootstrap$Bootstrap_Internal_Button$buttonAttributes(options)
				}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$input,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$type_('radio'),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$checked(checked),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$autocomplete(false),
								_1: {ctor: '[]'}
							}
						}
					},
					{ctor: '[]'}),
				_1: children
			});
	});
var _rundis$elm_bootstrap$Bootstrap_Button$linkButton = F2(
	function (options, children) {
		return A2(
			_elm_lang$html$Html$a,
			{
				ctor: '::',
				_0: A2(_elm_lang$html$Html_Attributes$attribute, 'role', 'button'),
				_1: _rundis$elm_bootstrap$Bootstrap_Internal_Button$buttonAttributes(options)
			},
			children);
	});
var _rundis$elm_bootstrap$Bootstrap_Button$button = F2(
	function (options, children) {
		return A2(
			_elm_lang$html$Html$button,
			_rundis$elm_bootstrap$Bootstrap_Internal_Button$buttonAttributes(options),
			children);
	});

var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$defaultOptions = {
	size: _elm_lang$core$Maybe$Nothing,
	vertical: false,
	attributes: {ctor: '[]'}
};
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$applyModifier = F2(
	function (modifier, options) {
		var _p0 = modifier;
		switch (_p0.ctor) {
			case 'Size':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						size: _elm_lang$core$Maybe$Just(_p0._0)
					});
			case 'Vertical':
				return _elm_lang$core$Native_Utils.update(
					options,
					{vertical: true});
			default:
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						attributes: A2(_elm_lang$core$Basics_ops['++'], options.attributes, _p0._0)
					});
		}
	});
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$groupAttributes = F2(
	function (toggle, modifiers) {
		var options = A3(_elm_lang$core$List$foldl, _rundis$elm_bootstrap$Bootstrap_ButtonGroup$applyModifier, _rundis$elm_bootstrap$Bootstrap_ButtonGroup$defaultOptions, modifiers);
		return A2(
			_elm_lang$core$Basics_ops['++'],
			{
				ctor: '::',
				_0: A2(_elm_lang$html$Html_Attributes$attribute, 'role', 'group'),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$classList(
						{
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'btn-group', _1: true},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'btn-group-toggle', _1: toggle},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'btn-group-vertical', _1: options.vertical},
									_1: {ctor: '[]'}
								}
							}
						}),
					_1: {
						ctor: '::',
						_0: A2(_elm_lang$html$Html_Attributes$attribute, 'data-toggle', 'buttons'),
						_1: {ctor: '[]'}
					}
				}
			},
			A2(
				_elm_lang$core$Basics_ops['++'],
				function () {
					var _p1 = A2(_elm_lang$core$Maybe$andThen, _rundis$elm_bootstrap$Bootstrap_Grid_Internal$screenSizeOption, options.size);
					if (_p1.ctor === 'Just') {
						return {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class(
								A2(_elm_lang$core$Basics_ops['++'], 'btn-group-', _p1._0)),
							_1: {ctor: '[]'}
						};
					} else {
						return {ctor: '[]'};
					}
				}(),
				options.attributes));
	});
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$renderGroup = function (_p2) {
	var _p3 = _p2;
	return _p3._0;
};
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$toolbar = F2(
	function (attributes, items) {
		return A2(
			_elm_lang$html$Html$div,
			A2(
				_elm_lang$core$Basics_ops['++'],
				{
					ctor: '::',
					_0: A2(_elm_lang$html$Html_Attributes$attribute, 'role', 'toolbar'),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('btn-toolbar'),
						_1: {ctor: '[]'}
					}
				},
				attributes),
			A2(_elm_lang$core$List$map, _rundis$elm_bootstrap$Bootstrap_ButtonGroup$renderGroup, items));
	});
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$Options = F3(
	function (a, b, c) {
		return {size: a, vertical: b, attributes: c};
	});
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$Attrs = function (a) {
	return {ctor: 'Attrs', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$attrs = function (attrs) {
	return _rundis$elm_bootstrap$Bootstrap_ButtonGroup$Attrs(attrs);
};
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$Vertical = {ctor: 'Vertical'};
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$vertical = _rundis$elm_bootstrap$Bootstrap_ButtonGroup$Vertical;
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$Size = function (a) {
	return {ctor: 'Size', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$small = _rundis$elm_bootstrap$Bootstrap_ButtonGroup$Size(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM);
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$large = _rundis$elm_bootstrap$Bootstrap_ButtonGroup$Size(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG);
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$GroupItem = function (a) {
	return {ctor: 'GroupItem', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$buttonGroupItem = F2(
	function (options, items) {
		return _rundis$elm_bootstrap$Bootstrap_ButtonGroup$GroupItem(
			A2(
				_elm_lang$html$Html$div,
				A2(_rundis$elm_bootstrap$Bootstrap_ButtonGroup$groupAttributes, false, options),
				A2(
					_elm_lang$core$List$map,
					function (_p4) {
						var _p5 = _p4;
						return _p5._0;
					},
					items)));
	});
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$buttonGroup = F2(
	function (options, items) {
		return _rundis$elm_bootstrap$Bootstrap_ButtonGroup$renderGroup(
			A2(_rundis$elm_bootstrap$Bootstrap_ButtonGroup$buttonGroupItem, options, items));
	});
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$linkButtonGroupItem = F2(
	function (options, items) {
		return _rundis$elm_bootstrap$Bootstrap_ButtonGroup$GroupItem(
			A2(
				_elm_lang$html$Html$div,
				A2(_rundis$elm_bootstrap$Bootstrap_ButtonGroup$groupAttributes, false, options),
				A2(
					_elm_lang$core$List$map,
					function (_p6) {
						var _p7 = _p6;
						return _p7._0;
					},
					items)));
	});
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$linkButtonGroup = F2(
	function (options, items) {
		return _rundis$elm_bootstrap$Bootstrap_ButtonGroup$renderGroup(
			A2(_rundis$elm_bootstrap$Bootstrap_ButtonGroup$linkButtonGroupItem, options, items));
	});
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$radioButtonGroupItem = F2(
	function (options, items) {
		return _rundis$elm_bootstrap$Bootstrap_ButtonGroup$GroupItem(
			A2(
				_elm_lang$html$Html$div,
				A2(_rundis$elm_bootstrap$Bootstrap_ButtonGroup$groupAttributes, true, options),
				A2(
					_elm_lang$core$List$map,
					function (_p8) {
						var _p9 = _p8;
						return _p9._0;
					},
					items)));
	});
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$radioButtonGroup = F2(
	function (options, items) {
		return _rundis$elm_bootstrap$Bootstrap_ButtonGroup$renderGroup(
			A2(_rundis$elm_bootstrap$Bootstrap_ButtonGroup$radioButtonGroupItem, options, items));
	});
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$checkboxButtonGroupItem = F2(
	function (options, items) {
		return _rundis$elm_bootstrap$Bootstrap_ButtonGroup$GroupItem(
			A2(
				_elm_lang$html$Html$div,
				A2(_rundis$elm_bootstrap$Bootstrap_ButtonGroup$groupAttributes, true, options),
				A2(
					_elm_lang$core$List$map,
					function (_p10) {
						var _p11 = _p10;
						return _p11._0;
					},
					items)));
	});
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$checkboxButtonGroup = F2(
	function (options, items) {
		return _rundis$elm_bootstrap$Bootstrap_ButtonGroup$renderGroup(
			A2(_rundis$elm_bootstrap$Bootstrap_ButtonGroup$checkboxButtonGroupItem, options, items));
	});
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$ButtonItem = function (a) {
	return {ctor: 'ButtonItem', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$button = F2(
	function (options, children) {
		return _rundis$elm_bootstrap$Bootstrap_ButtonGroup$ButtonItem(
			A2(_rundis$elm_bootstrap$Bootstrap_Button$button, options, children));
	});
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$LinkButtonItem = function (a) {
	return {ctor: 'LinkButtonItem', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$linkButton = F2(
	function (options, children) {
		return _rundis$elm_bootstrap$Bootstrap_ButtonGroup$LinkButtonItem(
			A2(_rundis$elm_bootstrap$Bootstrap_Button$linkButton, options, children));
	});
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$RadioButtonItem = function (a) {
	return {ctor: 'RadioButtonItem', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$radioButton = F3(
	function (checked, options, children) {
		return _rundis$elm_bootstrap$Bootstrap_ButtonGroup$RadioButtonItem(
			A3(_rundis$elm_bootstrap$Bootstrap_Button$radioButton, checked, options, children));
	});
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$CheckboxButtonItem = function (a) {
	return {ctor: 'CheckboxButtonItem', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_ButtonGroup$checkboxButton = F3(
	function (checked, options, children) {
		return _rundis$elm_bootstrap$Bootstrap_ButtonGroup$CheckboxButtonItem(
			A3(_rundis$elm_bootstrap$Bootstrap_Button$checkboxButton, checked, options, children));
	});

var _rundis$elm_bootstrap$Bootstrap_Internal_Role$toClass = F2(
	function (prefix, role) {
		return _elm_lang$html$Html_Attributes$class(
			A2(
				_elm_lang$core$Basics_ops['++'],
				prefix,
				A2(
					_elm_lang$core$Basics_ops['++'],
					'-',
					function () {
						var _p0 = role;
						switch (_p0.ctor) {
							case 'Primary':
								return 'primary';
							case 'Secondary':
								return 'secondary';
							case 'Success':
								return 'success';
							case 'Info':
								return 'info';
							case 'Warning':
								return 'warning';
							case 'Danger':
								return 'danger';
							case 'Light':
								return 'light';
							default:
								return 'dark';
						}
					}())));
	});
var _rundis$elm_bootstrap$Bootstrap_Internal_Role$Dark = {ctor: 'Dark'};
var _rundis$elm_bootstrap$Bootstrap_Internal_Role$Light = {ctor: 'Light'};
var _rundis$elm_bootstrap$Bootstrap_Internal_Role$Danger = {ctor: 'Danger'};
var _rundis$elm_bootstrap$Bootstrap_Internal_Role$Warning = {ctor: 'Warning'};
var _rundis$elm_bootstrap$Bootstrap_Internal_Role$Info = {ctor: 'Info'};
var _rundis$elm_bootstrap$Bootstrap_Internal_Role$Success = {ctor: 'Success'};
var _rundis$elm_bootstrap$Bootstrap_Internal_Role$Secondary = {ctor: 'Secondary'};
var _rundis$elm_bootstrap$Bootstrap_Internal_Role$Primary = {ctor: 'Primary'};

var _rundis$elm_bootstrap$Bootstrap_Utilities_Border$circle = _elm_lang$html$Html_Attributes$class('rounded-circle');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Border$roundedNone = _elm_lang$html$Html_Attributes$class('rounded-0');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Border$roundedRight = _elm_lang$html$Html_Attributes$class('rounded-right');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Border$roundedLeft = _elm_lang$html$Html_Attributes$class('rounded-left');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Border$roundedBottom = _elm_lang$html$Html_Attributes$class('rounded-bottom');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Border$roundedTop = _elm_lang$html$Html_Attributes$class('rounded-top');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Border$rounded = _elm_lang$html$Html_Attributes$class('rounded');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Border$light = A2(_rundis$elm_bootstrap$Bootstrap_Internal_Role$toClass, 'border', _rundis$elm_bootstrap$Bootstrap_Internal_Role$Light);
var _rundis$elm_bootstrap$Bootstrap_Utilities_Border$dark = A2(_rundis$elm_bootstrap$Bootstrap_Internal_Role$toClass, 'border', _rundis$elm_bootstrap$Bootstrap_Internal_Role$Dark);
var _rundis$elm_bootstrap$Bootstrap_Utilities_Border$warning = A2(_rundis$elm_bootstrap$Bootstrap_Internal_Role$toClass, 'border', _rundis$elm_bootstrap$Bootstrap_Internal_Role$Warning);
var _rundis$elm_bootstrap$Bootstrap_Utilities_Border$danger = A2(_rundis$elm_bootstrap$Bootstrap_Internal_Role$toClass, 'border', _rundis$elm_bootstrap$Bootstrap_Internal_Role$Danger);
var _rundis$elm_bootstrap$Bootstrap_Utilities_Border$info = A2(_rundis$elm_bootstrap$Bootstrap_Internal_Role$toClass, 'border', _rundis$elm_bootstrap$Bootstrap_Internal_Role$Info);
var _rundis$elm_bootstrap$Bootstrap_Utilities_Border$success = A2(_rundis$elm_bootstrap$Bootstrap_Internal_Role$toClass, 'border', _rundis$elm_bootstrap$Bootstrap_Internal_Role$Success);
var _rundis$elm_bootstrap$Bootstrap_Utilities_Border$secondary = A2(_rundis$elm_bootstrap$Bootstrap_Internal_Role$toClass, 'border', _rundis$elm_bootstrap$Bootstrap_Internal_Role$Secondary);
var _rundis$elm_bootstrap$Bootstrap_Utilities_Border$primary = A2(_rundis$elm_bootstrap$Bootstrap_Internal_Role$toClass, 'border', _rundis$elm_bootstrap$Bootstrap_Internal_Role$Primary);
var _rundis$elm_bootstrap$Bootstrap_Utilities_Border$rightNone = _elm_lang$html$Html_Attributes$class('border-right-0');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Border$leftNone = _elm_lang$html$Html_Attributes$class('border-left-0');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Border$bottomNone = _elm_lang$html$Html_Attributes$class('border-botton-0');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Border$topNone = _elm_lang$html$Html_Attributes$class('border-top-0');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Border$none = _elm_lang$html$Html_Attributes$class('border-0');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Border$right = _elm_lang$html$Html_Attributes$class('border-right');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Border$left = _elm_lang$html$Html_Attributes$class('border-left');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Border$bottom = _elm_lang$html$Html_Attributes$class('border-bottom');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Border$top = _elm_lang$html$Html_Attributes$class('border-top');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Border$all = _elm_lang$html$Html_Attributes$class('border');

var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$wrapReverseXl = _elm_lang$html$Html_Attributes$class('flex-xl-wrap-reverse');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$wrapReverseLg = _elm_lang$html$Html_Attributes$class('flex-lg-wrap-reverse');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$wrapReverseMd = _elm_lang$html$Html_Attributes$class('flex-md-wrap-reverse');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$wrapReverseSm = _elm_lang$html$Html_Attributes$class('flex-sm-wrap-reverse');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$wrapReverse = _elm_lang$html$Html_Attributes$class('flex-wrap-reverse');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$nowrapXl = _elm_lang$html$Html_Attributes$class('flex-xl-nowrap');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$nowrapLg = _elm_lang$html$Html_Attributes$class('flex-lg-nowrap');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$nowrapMd = _elm_lang$html$Html_Attributes$class('flex-md-nowrap');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$nowrapSm = _elm_lang$html$Html_Attributes$class('flex-sm-nowrap');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$nowrap = _elm_lang$html$Html_Attributes$class('flex-nowrap');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$wrapXl = _elm_lang$html$Html_Attributes$class('flex-xl-wrap');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$wrapLg = _elm_lang$html$Html_Attributes$class('flex-lg-wrap');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$wrapMd = _elm_lang$html$Html_Attributes$class('flex-md-wrap');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$wrapSm = _elm_lang$html$Html_Attributes$class('flex-sm-wrap');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$wrap = _elm_lang$html$Html_Attributes$class('flex-wrap');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignSelfStretchXl = _elm_lang$html$Html_Attributes$class('align-self-xl-stretch');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignSelfStretchLg = _elm_lang$html$Html_Attributes$class('align-self-lg-stretch');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignSelfStretchMd = _elm_lang$html$Html_Attributes$class('align-self-md-stretch');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignSelfStretchSm = _elm_lang$html$Html_Attributes$class('align-self-sm-stretch');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignSelfStretch = _elm_lang$html$Html_Attributes$class('align-self-stretch');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignSelfBaselineXl = _elm_lang$html$Html_Attributes$class('align-self-xl-baseline');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignSelfBaselineLg = _elm_lang$html$Html_Attributes$class('align-self-lg-baseline');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignSelfBaselineMd = _elm_lang$html$Html_Attributes$class('align-self-md-baseline');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignSelfBaselineSm = _elm_lang$html$Html_Attributes$class('align-self-sm-baseline');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignSelfBaseline = _elm_lang$html$Html_Attributes$class('align-self-baseline');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignSelfCenterXl = _elm_lang$html$Html_Attributes$class('align-self-xl-center');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignSelfCenterLg = _elm_lang$html$Html_Attributes$class('align-self-lg-center');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignSelfCenterMd = _elm_lang$html$Html_Attributes$class('align-self-md-center');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignSelfCenterSm = _elm_lang$html$Html_Attributes$class('align-self-sm-center');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignSelfCenter = _elm_lang$html$Html_Attributes$class('align-self-center');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignSelfEndXl = _elm_lang$html$Html_Attributes$class('align-self-xl-end');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignSelfEndLg = _elm_lang$html$Html_Attributes$class('align-self-lg-end');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignSelfEndMd = _elm_lang$html$Html_Attributes$class('align-self-md-end');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignSelfEndSm = _elm_lang$html$Html_Attributes$class('align-self-sm-end');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignSelfEnd = _elm_lang$html$Html_Attributes$class('align-self-end');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignSelfStartXl = _elm_lang$html$Html_Attributes$class('align-self-xl-start');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignSelfStartLg = _elm_lang$html$Html_Attributes$class('align-self-lg-start');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignSelfStartMd = _elm_lang$html$Html_Attributes$class('align-self-md-start');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignSelfStartSm = _elm_lang$html$Html_Attributes$class('align-self-sm-start');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignSelfStart = _elm_lang$html$Html_Attributes$class('align-self-start');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignItemsStretchXl = _elm_lang$html$Html_Attributes$class('align-items-xl-stretched');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignItemsStretchLg = _elm_lang$html$Html_Attributes$class('align-items-lg-stretched');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignItemsStretchMd = _elm_lang$html$Html_Attributes$class('align-items-md-stretched');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignItemsStretchSm = _elm_lang$html$Html_Attributes$class('align-items-sm-stretched');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignItemsStretch = _elm_lang$html$Html_Attributes$class('align-items-stretched');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignItemsBaselineXl = _elm_lang$html$Html_Attributes$class('align-items-xl-baseline');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignItemsBaselineLg = _elm_lang$html$Html_Attributes$class('align-items-lg-baseline');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignItemsBaselineMd = _elm_lang$html$Html_Attributes$class('align-items-md-baseline');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignItemsBaselineSm = _elm_lang$html$Html_Attributes$class('align-items-sm-baseline');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignItemsBaseline = _elm_lang$html$Html_Attributes$class('align-items-baseline');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignItemsCenterXl = _elm_lang$html$Html_Attributes$class('align-items-xl-center');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignItemsCenterLg = _elm_lang$html$Html_Attributes$class('align-items-lg-center');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignItemsCenterMd = _elm_lang$html$Html_Attributes$class('align-items-md-center');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignItemsCenterSm = _elm_lang$html$Html_Attributes$class('align-items-sm-center');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignItemsCenter = _elm_lang$html$Html_Attributes$class('align-items-center');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignItemsEndXl = _elm_lang$html$Html_Attributes$class('align-items-xl-end');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignItemsEndLg = _elm_lang$html$Html_Attributes$class('align-items-lg-end');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignItemsEndMd = _elm_lang$html$Html_Attributes$class('align-items-md-end');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignItemsEndSm = _elm_lang$html$Html_Attributes$class('align-items-sm-end');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignItemsEnd = _elm_lang$html$Html_Attributes$class('align-items-end');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignItemsStartXl = _elm_lang$html$Html_Attributes$class('align-items-xl-start');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignItemsStartLg = _elm_lang$html$Html_Attributes$class('align-items-lg-start');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignItemsStartMd = _elm_lang$html$Html_Attributes$class('align-items-md-start');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignItemsStartSm = _elm_lang$html$Html_Attributes$class('align-items-sm-start');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$alignItemsStart = _elm_lang$html$Html_Attributes$class('align-items-start');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$justifyAroundXl = _elm_lang$html$Html_Attributes$class('justify-content-xl-around');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$justifyAroundLg = _elm_lang$html$Html_Attributes$class('justify-content-lg-around');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$justifyAroundMd = _elm_lang$html$Html_Attributes$class('justify-content-md-around');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$justifyAroundSm = _elm_lang$html$Html_Attributes$class('justify-content-sm-around');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$justifyAround = _elm_lang$html$Html_Attributes$class('justify-content-around');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$justifyBetweenXl = _elm_lang$html$Html_Attributes$class('justify-content-xl-between');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$justifyBetweenLg = _elm_lang$html$Html_Attributes$class('justify-content-lg-between');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$justifyBetweenMd = _elm_lang$html$Html_Attributes$class('justify-content-md-between');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$justifyBetweenSm = _elm_lang$html$Html_Attributes$class('justify-content-sm-between');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$justifyBetween = _elm_lang$html$Html_Attributes$class('justify-content-between');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$justifyCenterXl = _elm_lang$html$Html_Attributes$class('justify-content-xl-center');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$justifyCenterLg = _elm_lang$html$Html_Attributes$class('justify-content-lg-center');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$justifyCenterMd = _elm_lang$html$Html_Attributes$class('justify-content-md-center');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$justifyCenterSm = _elm_lang$html$Html_Attributes$class('justify-content-sm-center');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$justifyCenter = _elm_lang$html$Html_Attributes$class('justify-content-center');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$justifyEndXl = _elm_lang$html$Html_Attributes$class('justify-content-xl-end');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$justifyEndLg = _elm_lang$html$Html_Attributes$class('justify-content-lg-end');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$justifyEndMd = _elm_lang$html$Html_Attributes$class('justify-content-md-end');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$justifyEndSm = _elm_lang$html$Html_Attributes$class('justify-content-sm-end');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$justifyEnd = _elm_lang$html$Html_Attributes$class('justify-content-end');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$justifyStartXl = _elm_lang$html$Html_Attributes$class('justify-content-xl-start');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$justifyStartLg = _elm_lang$html$Html_Attributes$class('justify-content-lg-start');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$justifyStartMd = _elm_lang$html$Html_Attributes$class('justify-content-md-start');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$justifyStartSm = _elm_lang$html$Html_Attributes$class('justify-content-sm-start');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$justifyStart = _elm_lang$html$Html_Attributes$class('justify-content-start');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$colReverseXl = _elm_lang$html$Html_Attributes$class('flex-xl-column-reverse');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$colReverseLg = _elm_lang$html$Html_Attributes$class('flex-lg-column-reverse');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$colReverseMd = _elm_lang$html$Html_Attributes$class('flex-md-column-reverse');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$colReverseSm = _elm_lang$html$Html_Attributes$class('flex-sm-column-reverse');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$colReverse = _elm_lang$html$Html_Attributes$class('flex-column-reverse');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$colXl = _elm_lang$html$Html_Attributes$class('flex-xl-column');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$colLg = _elm_lang$html$Html_Attributes$class('flex-lg-column');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$colMd = _elm_lang$html$Html_Attributes$class('flex-md-column');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$colSm = _elm_lang$html$Html_Attributes$class('flex-sm-column');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$col = _elm_lang$html$Html_Attributes$class('flex-column');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$rowReverseXl = _elm_lang$html$Html_Attributes$class('flex-xl-row-reverse');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$rowReverseLg = _elm_lang$html$Html_Attributes$class('flex-lg-row-reverse');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$rowReverseMd = _elm_lang$html$Html_Attributes$class('flex-md-row-reverse');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$rowReverseSm = _elm_lang$html$Html_Attributes$class('flex-sm-row-reverse');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$rowReverse = _elm_lang$html$Html_Attributes$class('flex-row-reverse');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$rowXl = _elm_lang$html$Html_Attributes$class('flex-xl-row');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$rowLg = _elm_lang$html$Html_Attributes$class('flex-lg-row');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$rowMd = _elm_lang$html$Html_Attributes$class('flex-md-row');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$rowSm = _elm_lang$html$Html_Attributes$class('flex-sm-row');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$row = _elm_lang$html$Html_Attributes$class('flex-row');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$inlineXl = _elm_lang$html$Html_Attributes$class('d-xl-inline-flex');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$inlineLg = _elm_lang$html$Html_Attributes$class('d-lg-inline-flex');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$inlineMd = _elm_lang$html$Html_Attributes$class('d-md-inline-flex');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$inlineSm = _elm_lang$html$Html_Attributes$class('d-sm-inline-flex');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$inline = _elm_lang$html$Html_Attributes$class('d-inline-flex');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$blockXl = _elm_lang$html$Html_Attributes$class('d-xl-flex');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$blockLg = _elm_lang$html$Html_Attributes$class('d-lg-flex');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$blockMd = _elm_lang$html$Html_Attributes$class('d-md-flex');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$blockSm = _elm_lang$html$Html_Attributes$class('d-sm-flex');
var _rundis$elm_bootstrap$Bootstrap_Utilities_Flex$block = _elm_lang$html$Html_Attributes$class('d-flex');

var _rundis$elm_bootstrap$Bootstrap_Form_FormInternal$validationToString = function (validation) {
	var _p0 = validation;
	if (_p0.ctor === 'Success') {
		return 'is-valid';
	} else {
		return 'is-invalid';
	}
};
var _rundis$elm_bootstrap$Bootstrap_Form_FormInternal$validationWrapperAttribute = function (validation) {
	return _elm_lang$html$Html_Attributes$class(
		A2(
			_elm_lang$core$Basics_ops['++'],
			'has-',
			_rundis$elm_bootstrap$Bootstrap_Form_FormInternal$validationToString(validation)));
};
var _rundis$elm_bootstrap$Bootstrap_Form_FormInternal$Danger = {ctor: 'Danger'};
var _rundis$elm_bootstrap$Bootstrap_Form_FormInternal$Success = {ctor: 'Success'};

var _rundis$elm_bootstrap$Bootstrap_Form_Input$validationAttribute = function (validation) {
	return _elm_lang$html$Html_Attributes$class(
		_rundis$elm_bootstrap$Bootstrap_Form_FormInternal$validationToString(validation));
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$typeAttribute = function (inputType) {
	return _elm_lang$html$Html_Attributes$type_(
		function () {
			var _p0 = inputType;
			switch (_p0.ctor) {
				case 'Text':
					return 'text';
				case 'Password':
					return 'password';
				case 'DatetimeLocal':
					return 'datetime-local';
				case 'Date':
					return 'date';
				case 'Month':
					return 'month';
				case 'Time':
					return 'time';
				case 'Week':
					return 'week';
				case 'Number':
					return 'number';
				case 'Email':
					return 'email';
				case 'Url':
					return 'url';
				case 'Search':
					return 'search';
				case 'Tel':
					return 'tel';
				default:
					return 'color';
			}
		}());
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$sizeAttribute = function (size) {
	return A2(
		_elm_lang$core$Maybe$map,
		function (s) {
			return _elm_lang$html$Html_Attributes$class(
				A2(_elm_lang$core$Basics_ops['++'], 'form-control-', s));
		},
		_rundis$elm_bootstrap$Bootstrap_Grid_Internal$screenSizeOption(size));
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$applyModifier = F2(
	function (modifier, options) {
		var _p1 = modifier;
		switch (_p1.ctor) {
			case 'Size':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						size: _elm_lang$core$Maybe$Just(_p1._0)
					});
			case 'Id':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						id: _elm_lang$core$Maybe$Just(_p1._0)
					});
			case 'Type':
				return _elm_lang$core$Native_Utils.update(
					options,
					{tipe: _p1._0});
			case 'Disabled':
				return _elm_lang$core$Native_Utils.update(
					options,
					{disabled: _p1._0});
			case 'Value':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						value: _elm_lang$core$Maybe$Just(_p1._0)
					});
			case 'DefaultValue':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						defaultValue: _elm_lang$core$Maybe$Just(_p1._0)
					});
			case 'Placeholder':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						placeholder: _elm_lang$core$Maybe$Just(_p1._0)
					});
			case 'OnInput':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						onInput: _elm_lang$core$Maybe$Just(_p1._0)
					});
			case 'Validation':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						validation: _elm_lang$core$Maybe$Just(_p1._0)
					});
			case 'Readonly':
				return _elm_lang$core$Native_Utils.update(
					options,
					{readonly: _p1._0});
			default:
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						attributes: A2(_elm_lang$core$Basics_ops['++'], options.attributes, _p1._0)
					});
		}
	});
var _rundis$elm_bootstrap$Bootstrap_Form_Input$Options = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return function (k) {
											return {tipe: a, id: b, size: c, disabled: d, value: e, defaultValue: f, placeholder: g, onInput: h, validation: i, readonly: j, attributes: k};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$Input = function (a) {
	return {ctor: 'Input', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$Attrs = function (a) {
	return {ctor: 'Attrs', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$attrs = function (attrs) {
	return _rundis$elm_bootstrap$Bootstrap_Form_Input$Attrs(attrs);
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$Readonly = function (a) {
	return {ctor: 'Readonly', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$readonly = function (readonly) {
	return _rundis$elm_bootstrap$Bootstrap_Form_Input$Readonly(readonly);
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$Placeholder = function (a) {
	return {ctor: 'Placeholder', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$placeholder = function (value) {
	return _rundis$elm_bootstrap$Bootstrap_Form_Input$Placeholder(value);
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$Validation = function (a) {
	return {ctor: 'Validation', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$success = _rundis$elm_bootstrap$Bootstrap_Form_Input$Validation(_rundis$elm_bootstrap$Bootstrap_Form_FormInternal$Success);
var _rundis$elm_bootstrap$Bootstrap_Form_Input$danger = _rundis$elm_bootstrap$Bootstrap_Form_Input$Validation(_rundis$elm_bootstrap$Bootstrap_Form_FormInternal$Danger);
var _rundis$elm_bootstrap$Bootstrap_Form_Input$OnInput = function (a) {
	return {ctor: 'OnInput', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$onInput = function (toMsg) {
	return _rundis$elm_bootstrap$Bootstrap_Form_Input$OnInput(toMsg);
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$DefaultValue = function (a) {
	return {ctor: 'DefaultValue', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$defaultValue = function (value) {
	return _rundis$elm_bootstrap$Bootstrap_Form_Input$DefaultValue(value);
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$Value = function (a) {
	return {ctor: 'Value', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$value = function (value) {
	return _rundis$elm_bootstrap$Bootstrap_Form_Input$Value(value);
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$Disabled = function (a) {
	return {ctor: 'Disabled', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$disabled = function (disabled) {
	return _rundis$elm_bootstrap$Bootstrap_Form_Input$Disabled(disabled);
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$Type = function (a) {
	return {ctor: 'Type', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$create = F2(
	function (tipe, options) {
		return _rundis$elm_bootstrap$Bootstrap_Form_Input$Input(
			{
				options: {
					ctor: '::',
					_0: _rundis$elm_bootstrap$Bootstrap_Form_Input$Type(tipe),
					_1: options
				}
			});
	});
var _rundis$elm_bootstrap$Bootstrap_Form_Input$Id = function (a) {
	return {ctor: 'Id', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$id = function (id) {
	return _rundis$elm_bootstrap$Bootstrap_Form_Input$Id(id);
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$Size = function (a) {
	return {ctor: 'Size', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$small = _rundis$elm_bootstrap$Bootstrap_Form_Input$Size(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM);
var _rundis$elm_bootstrap$Bootstrap_Form_Input$large = _rundis$elm_bootstrap$Bootstrap_Form_Input$Size(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG);
var _rundis$elm_bootstrap$Bootstrap_Form_Input$Color = {ctor: 'Color'};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$Tel = {ctor: 'Tel'};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$Search = {ctor: 'Search'};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$Url = {ctor: 'Url'};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$Email = {ctor: 'Email'};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$Number = {ctor: 'Number'};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$Week = {ctor: 'Week'};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$Time = {ctor: 'Time'};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$Month = {ctor: 'Month'};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$Date = {ctor: 'Date'};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$DatetimeLocal = {ctor: 'DatetimeLocal'};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$Password = {ctor: 'Password'};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$Text = {ctor: 'Text'};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$defaultOptions = {
	tipe: _rundis$elm_bootstrap$Bootstrap_Form_Input$Text,
	id: _elm_lang$core$Maybe$Nothing,
	size: _elm_lang$core$Maybe$Nothing,
	disabled: false,
	value: _elm_lang$core$Maybe$Nothing,
	defaultValue: _elm_lang$core$Maybe$Nothing,
	placeholder: _elm_lang$core$Maybe$Nothing,
	onInput: _elm_lang$core$Maybe$Nothing,
	validation: _elm_lang$core$Maybe$Nothing,
	readonly: false,
	attributes: {ctor: '[]'}
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$toAttributes = function (modifiers) {
	var options = A3(_elm_lang$core$List$foldl, _rundis$elm_bootstrap$Bootstrap_Form_Input$applyModifier, _rundis$elm_bootstrap$Bootstrap_Form_Input$defaultOptions, modifiers);
	return A2(
		_elm_lang$core$Basics_ops['++'],
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('form-control'),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$disabled(options.disabled),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$readonly(options.readonly),
					_1: {
						ctor: '::',
						_0: _rundis$elm_bootstrap$Bootstrap_Form_Input$typeAttribute(options.tipe),
						_1: {ctor: '[]'}
					}
				}
			}
		},
		A2(
			_elm_lang$core$Basics_ops['++'],
			A2(
				_elm_lang$core$List$filterMap,
				_elm_lang$core$Basics$identity,
				{
					ctor: '::',
					_0: A2(_elm_lang$core$Maybe$map, _elm_lang$html$Html_Attributes$id, options.id),
					_1: {
						ctor: '::',
						_0: A2(_elm_lang$core$Maybe$andThen, _rundis$elm_bootstrap$Bootstrap_Form_Input$sizeAttribute, options.size),
						_1: {
							ctor: '::',
							_0: A2(_elm_lang$core$Maybe$map, _elm_lang$html$Html_Attributes$value, options.value),
							_1: {
								ctor: '::',
								_0: A2(_elm_lang$core$Maybe$map, _elm_lang$html$Html_Attributes$defaultValue, options.defaultValue),
								_1: {
									ctor: '::',
									_0: A2(_elm_lang$core$Maybe$map, _elm_lang$html$Html_Attributes$placeholder, options.placeholder),
									_1: {
										ctor: '::',
										_0: A2(_elm_lang$core$Maybe$map, _elm_lang$html$Html_Events$onInput, options.onInput),
										_1: {
											ctor: '::',
											_0: A2(_elm_lang$core$Maybe$map, _rundis$elm_bootstrap$Bootstrap_Form_Input$validationAttribute, options.validation),
											_1: {ctor: '[]'}
										}
									}
								}
							}
						}
					}
				}),
			options.attributes));
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$view = function (_p2) {
	var _p3 = _p2;
	return A2(
		_elm_lang$html$Html$input,
		_rundis$elm_bootstrap$Bootstrap_Form_Input$toAttributes(_p3._0.options),
		{ctor: '[]'});
};
var _rundis$elm_bootstrap$Bootstrap_Form_Input$input = F2(
	function (tipe, options) {
		return _rundis$elm_bootstrap$Bootstrap_Form_Input$view(
			A2(_rundis$elm_bootstrap$Bootstrap_Form_Input$create, tipe, options));
	});
var _rundis$elm_bootstrap$Bootstrap_Form_Input$text = _rundis$elm_bootstrap$Bootstrap_Form_Input$input(_rundis$elm_bootstrap$Bootstrap_Form_Input$Text);
var _rundis$elm_bootstrap$Bootstrap_Form_Input$password = _rundis$elm_bootstrap$Bootstrap_Form_Input$input(_rundis$elm_bootstrap$Bootstrap_Form_Input$Password);
var _rundis$elm_bootstrap$Bootstrap_Form_Input$datetimeLocal = _rundis$elm_bootstrap$Bootstrap_Form_Input$input(_rundis$elm_bootstrap$Bootstrap_Form_Input$DatetimeLocal);
var _rundis$elm_bootstrap$Bootstrap_Form_Input$date = _rundis$elm_bootstrap$Bootstrap_Form_Input$input(_rundis$elm_bootstrap$Bootstrap_Form_Input$Date);
var _rundis$elm_bootstrap$Bootstrap_Form_Input$month = _rundis$elm_bootstrap$Bootstrap_Form_Input$input(_rundis$elm_bootstrap$Bootstrap_Form_Input$Month);
var _rundis$elm_bootstrap$Bootstrap_Form_Input$time = _rundis$elm_bootstrap$Bootstrap_Form_Input$input(_rundis$elm_bootstrap$Bootstrap_Form_Input$Time);
var _rundis$elm_bootstrap$Bootstrap_Form_Input$week = _rundis$elm_bootstrap$Bootstrap_Form_Input$input(_rundis$elm_bootstrap$Bootstrap_Form_Input$Week);
var _rundis$elm_bootstrap$Bootstrap_Form_Input$number = _rundis$elm_bootstrap$Bootstrap_Form_Input$input(_rundis$elm_bootstrap$Bootstrap_Form_Input$Number);
var _rundis$elm_bootstrap$Bootstrap_Form_Input$email = _rundis$elm_bootstrap$Bootstrap_Form_Input$input(_rundis$elm_bootstrap$Bootstrap_Form_Input$Email);
var _rundis$elm_bootstrap$Bootstrap_Form_Input$url = _rundis$elm_bootstrap$Bootstrap_Form_Input$input(_rundis$elm_bootstrap$Bootstrap_Form_Input$Url);
var _rundis$elm_bootstrap$Bootstrap_Form_Input$search = _rundis$elm_bootstrap$Bootstrap_Form_Input$input(_rundis$elm_bootstrap$Bootstrap_Form_Input$Search);
var _rundis$elm_bootstrap$Bootstrap_Form_Input$tel = _rundis$elm_bootstrap$Bootstrap_Form_Input$input(_rundis$elm_bootstrap$Bootstrap_Form_Input$Tel);
var _rundis$elm_bootstrap$Bootstrap_Form_Input$color = _rundis$elm_bootstrap$Bootstrap_Form_Input$input(_rundis$elm_bootstrap$Bootstrap_Form_Input$Color);

var _elm_lang$dom$Native_Dom = function() {

var fakeNode = {
	addEventListener: function() {},
	removeEventListener: function() {}
};

var onDocument = on(typeof document !== 'undefined' ? document : fakeNode);
var onWindow = on(typeof window !== 'undefined' ? window : fakeNode);

function on(node)
{
	return function(eventName, decoder, toTask)
	{
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {

			function performTask(event)
			{
				var result = A2(_elm_lang$core$Json_Decode$decodeValue, decoder, event);
				if (result.ctor === 'Ok')
				{
					_elm_lang$core$Native_Scheduler.rawSpawn(toTask(result._0));
				}
			}

			node.addEventListener(eventName, performTask);

			return function()
			{
				node.removeEventListener(eventName, performTask);
			};
		});
	};
}

var rAF = typeof requestAnimationFrame !== 'undefined'
	? requestAnimationFrame
	: function(callback) { callback(); };

function withNode(id, doStuff)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		rAF(function()
		{
			var node = document.getElementById(id);
			if (node === null)
			{
				callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'NotFound', _0: id }));
				return;
			}
			callback(_elm_lang$core$Native_Scheduler.succeed(doStuff(node)));
		});
	});
}


// FOCUS

function focus(id)
{
	return withNode(id, function(node) {
		node.focus();
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function blur(id)
{
	return withNode(id, function(node) {
		node.blur();
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}


// SCROLLING

function getScrollTop(id)
{
	return withNode(id, function(node) {
		return node.scrollTop;
	});
}

function setScrollTop(id, desiredScrollTop)
{
	return withNode(id, function(node) {
		node.scrollTop = desiredScrollTop;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function toBottom(id)
{
	return withNode(id, function(node) {
		node.scrollTop = node.scrollHeight;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function getScrollLeft(id)
{
	return withNode(id, function(node) {
		return node.scrollLeft;
	});
}

function setScrollLeft(id, desiredScrollLeft)
{
	return withNode(id, function(node) {
		node.scrollLeft = desiredScrollLeft;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function toRight(id)
{
	return withNode(id, function(node) {
		node.scrollLeft = node.scrollWidth;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}


// SIZE

function width(options, id)
{
	return withNode(id, function(node) {
		switch (options.ctor)
		{
			case 'Content':
				return node.scrollWidth;
			case 'VisibleContent':
				return node.clientWidth;
			case 'VisibleContentWithBorders':
				return node.offsetWidth;
			case 'VisibleContentWithBordersAndMargins':
				var rect = node.getBoundingClientRect();
				return rect.right - rect.left;
		}
	});
}

function height(options, id)
{
	return withNode(id, function(node) {
		switch (options.ctor)
		{
			case 'Content':
				return node.scrollHeight;
			case 'VisibleContent':
				return node.clientHeight;
			case 'VisibleContentWithBorders':
				return node.offsetHeight;
			case 'VisibleContentWithBordersAndMargins':
				var rect = node.getBoundingClientRect();
				return rect.bottom - rect.top;
		}
	});
}

return {
	onDocument: F3(onDocument),
	onWindow: F3(onWindow),

	focus: focus,
	blur: blur,

	getScrollTop: getScrollTop,
	setScrollTop: F2(setScrollTop),
	getScrollLeft: getScrollLeft,
	setScrollLeft: F2(setScrollLeft),
	toBottom: toBottom,
	toRight: toRight,

	height: F2(height),
	width: F2(width)
};

}();

var _elm_lang$core$Task$onError = _elm_lang$core$Native_Scheduler.onError;
var _elm_lang$core$Task$andThen = _elm_lang$core$Native_Scheduler.andThen;
var _elm_lang$core$Task$spawnCmd = F2(
	function (router, _p0) {
		var _p1 = _p0;
		return _elm_lang$core$Native_Scheduler.spawn(
			A2(
				_elm_lang$core$Task$andThen,
				_elm_lang$core$Platform$sendToApp(router),
				_p1._0));
	});
var _elm_lang$core$Task$fail = _elm_lang$core$Native_Scheduler.fail;
var _elm_lang$core$Task$mapError = F2(
	function (convert, task) {
		return A2(
			_elm_lang$core$Task$onError,
			function (_p2) {
				return _elm_lang$core$Task$fail(
					convert(_p2));
			},
			task);
	});
var _elm_lang$core$Task$succeed = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return _elm_lang$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var _elm_lang$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return _elm_lang$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map3 = F4(
	function (func, taskA, taskB, taskC) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return _elm_lang$core$Task$succeed(
									A3(func, a, b, c));
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map4 = F5(
	function (func, taskA, taskB, taskC, taskD) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Task$andThen,
									function (d) {
										return _elm_lang$core$Task$succeed(
											A4(func, a, b, c, d));
									},
									taskD);
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map5 = F6(
	function (func, taskA, taskB, taskC, taskD, taskE) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Task$andThen,
									function (d) {
										return A2(
											_elm_lang$core$Task$andThen,
											function (e) {
												return _elm_lang$core$Task$succeed(
													A5(func, a, b, c, d, e));
											},
											taskE);
									},
									taskD);
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$sequence = function (tasks) {
	var _p3 = tasks;
	if (_p3.ctor === '[]') {
		return _elm_lang$core$Task$succeed(
			{ctor: '[]'});
	} else {
		return A3(
			_elm_lang$core$Task$map2,
			F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				}),
			_p3._0,
			_elm_lang$core$Task$sequence(_p3._1));
	}
};
var _elm_lang$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			_elm_lang$core$Task$map,
			function (_p4) {
				return {ctor: '_Tuple0'};
			},
			_elm_lang$core$Task$sequence(
				A2(
					_elm_lang$core$List$map,
					_elm_lang$core$Task$spawnCmd(router),
					commands)));
	});
var _elm_lang$core$Task$init = _elm_lang$core$Task$succeed(
	{ctor: '_Tuple0'});
var _elm_lang$core$Task$onSelfMsg = F3(
	function (_p7, _p6, _p5) {
		return _elm_lang$core$Task$succeed(
			{ctor: '_Tuple0'});
	});
var _elm_lang$core$Task$command = _elm_lang$core$Native_Platform.leaf('Task');
var _elm_lang$core$Task$Perform = function (a) {
	return {ctor: 'Perform', _0: a};
};
var _elm_lang$core$Task$perform = F2(
	function (toMessage, task) {
		return _elm_lang$core$Task$command(
			_elm_lang$core$Task$Perform(
				A2(_elm_lang$core$Task$map, toMessage, task)));
	});
var _elm_lang$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return _elm_lang$core$Task$command(
			_elm_lang$core$Task$Perform(
				A2(
					_elm_lang$core$Task$onError,
					function (_p8) {
						return _elm_lang$core$Task$succeed(
							resultToMessage(
								_elm_lang$core$Result$Err(_p8)));
					},
					A2(
						_elm_lang$core$Task$andThen,
						function (_p9) {
							return _elm_lang$core$Task$succeed(
								resultToMessage(
									_elm_lang$core$Result$Ok(_p9)));
						},
						task))));
	});
var _elm_lang$core$Task$cmdMap = F2(
	function (tagger, _p10) {
		var _p11 = _p10;
		return _elm_lang$core$Task$Perform(
			A2(_elm_lang$core$Task$map, tagger, _p11._0));
	});
_elm_lang$core$Native_Platform.effectManagers['Task'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Task$init, onEffects: _elm_lang$core$Task$onEffects, onSelfMsg: _elm_lang$core$Task$onSelfMsg, tag: 'cmd', cmdMap: _elm_lang$core$Task$cmdMap};

var _elm_lang$dom$Dom_LowLevel$onWindow = _elm_lang$dom$Native_Dom.onWindow;
var _elm_lang$dom$Dom_LowLevel$onDocument = _elm_lang$dom$Native_Dom.onDocument;

//import Native.Scheduler //

var _elm_lang$core$Native_Time = function() {

var now = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
{
	callback(_elm_lang$core$Native_Scheduler.succeed(Date.now()));
});

function setInterval_(interval, task)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var id = setInterval(function() {
			_elm_lang$core$Native_Scheduler.rawSpawn(task);
		}, interval);

		return function() { clearInterval(id); };
	});
}

return {
	now: now,
	setInterval_: F2(setInterval_)
};

}();
var _elm_lang$core$Time$setInterval = _elm_lang$core$Native_Time.setInterval_;
var _elm_lang$core$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		var _p0 = intervals;
		if (_p0.ctor === '[]') {
			return _elm_lang$core$Task$succeed(processes);
		} else {
			var _p1 = _p0._0;
			var spawnRest = function (id) {
				return A3(
					_elm_lang$core$Time$spawnHelp,
					router,
					_p0._1,
					A3(_elm_lang$core$Dict$insert, _p1, id, processes));
			};
			var spawnTimer = _elm_lang$core$Native_Scheduler.spawn(
				A2(
					_elm_lang$core$Time$setInterval,
					_p1,
					A2(_elm_lang$core$Platform$sendToSelf, router, _p1)));
			return A2(_elm_lang$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var _elm_lang$core$Time$addMySub = F2(
	function (_p2, state) {
		var _p3 = _p2;
		var _p6 = _p3._1;
		var _p5 = _p3._0;
		var _p4 = A2(_elm_lang$core$Dict$get, _p5, state);
		if (_p4.ctor === 'Nothing') {
			return A3(
				_elm_lang$core$Dict$insert,
				_p5,
				{
					ctor: '::',
					_0: _p6,
					_1: {ctor: '[]'}
				},
				state);
		} else {
			return A3(
				_elm_lang$core$Dict$insert,
				_p5,
				{ctor: '::', _0: _p6, _1: _p4._0},
				state);
		}
	});
var _elm_lang$core$Time$inMilliseconds = function (t) {
	return t;
};
var _elm_lang$core$Time$millisecond = 1;
var _elm_lang$core$Time$second = 1000 * _elm_lang$core$Time$millisecond;
var _elm_lang$core$Time$minute = 60 * _elm_lang$core$Time$second;
var _elm_lang$core$Time$hour = 60 * _elm_lang$core$Time$minute;
var _elm_lang$core$Time$inHours = function (t) {
	return t / _elm_lang$core$Time$hour;
};
var _elm_lang$core$Time$inMinutes = function (t) {
	return t / _elm_lang$core$Time$minute;
};
var _elm_lang$core$Time$inSeconds = function (t) {
	return t / _elm_lang$core$Time$second;
};
var _elm_lang$core$Time$now = _elm_lang$core$Native_Time.now;
var _elm_lang$core$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _p7 = A2(_elm_lang$core$Dict$get, interval, state.taggers);
		if (_p7.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var tellTaggers = function (time) {
				return _elm_lang$core$Task$sequence(
					A2(
						_elm_lang$core$List$map,
						function (tagger) {
							return A2(
								_elm_lang$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						_p7._0));
			};
			return A2(
				_elm_lang$core$Task$andThen,
				function (_p8) {
					return _elm_lang$core$Task$succeed(state);
				},
				A2(_elm_lang$core$Task$andThen, tellTaggers, _elm_lang$core$Time$now));
		}
	});
var _elm_lang$core$Time$subscription = _elm_lang$core$Native_Platform.leaf('Time');
var _elm_lang$core$Time$State = F2(
	function (a, b) {
		return {taggers: a, processes: b};
	});
var _elm_lang$core$Time$init = _elm_lang$core$Task$succeed(
	A2(_elm_lang$core$Time$State, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty));
var _elm_lang$core$Time$onEffects = F3(
	function (router, subs, _p9) {
		var _p10 = _p9;
		var rightStep = F3(
			function (_p12, id, _p11) {
				var _p13 = _p11;
				return {
					ctor: '_Tuple3',
					_0: _p13._0,
					_1: _p13._1,
					_2: A2(
						_elm_lang$core$Task$andThen,
						function (_p14) {
							return _p13._2;
						},
						_elm_lang$core$Native_Scheduler.kill(id))
				};
			});
		var bothStep = F4(
			function (interval, taggers, id, _p15) {
				var _p16 = _p15;
				return {
					ctor: '_Tuple3',
					_0: _p16._0,
					_1: A3(_elm_lang$core$Dict$insert, interval, id, _p16._1),
					_2: _p16._2
				};
			});
		var leftStep = F3(
			function (interval, taggers, _p17) {
				var _p18 = _p17;
				return {
					ctor: '_Tuple3',
					_0: {ctor: '::', _0: interval, _1: _p18._0},
					_1: _p18._1,
					_2: _p18._2
				};
			});
		var newTaggers = A3(_elm_lang$core$List$foldl, _elm_lang$core$Time$addMySub, _elm_lang$core$Dict$empty, subs);
		var _p19 = A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			_p10.processes,
			{
				ctor: '_Tuple3',
				_0: {ctor: '[]'},
				_1: _elm_lang$core$Dict$empty,
				_2: _elm_lang$core$Task$succeed(
					{ctor: '_Tuple0'})
			});
		var spawnList = _p19._0;
		var existingDict = _p19._1;
		var killTask = _p19._2;
		return A2(
			_elm_lang$core$Task$andThen,
			function (newProcesses) {
				return _elm_lang$core$Task$succeed(
					A2(_elm_lang$core$Time$State, newTaggers, newProcesses));
			},
			A2(
				_elm_lang$core$Task$andThen,
				function (_p20) {
					return A3(_elm_lang$core$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var _elm_lang$core$Time$Every = F2(
	function (a, b) {
		return {ctor: 'Every', _0: a, _1: b};
	});
var _elm_lang$core$Time$every = F2(
	function (interval, tagger) {
		return _elm_lang$core$Time$subscription(
			A2(_elm_lang$core$Time$Every, interval, tagger));
	});
var _elm_lang$core$Time$subMap = F2(
	function (f, _p21) {
		var _p22 = _p21;
		return A2(
			_elm_lang$core$Time$Every,
			_p22._0,
			function (_p23) {
				return f(
					_p22._1(_p23));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Time'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Time$init, onEffects: _elm_lang$core$Time$onEffects, onSelfMsg: _elm_lang$core$Time$onSelfMsg, tag: 'sub', subMap: _elm_lang$core$Time$subMap};

var _elm_lang$core$Process$kill = _elm_lang$core$Native_Scheduler.kill;
var _elm_lang$core$Process$sleep = _elm_lang$core$Native_Scheduler.sleep;
var _elm_lang$core$Process$spawn = _elm_lang$core$Native_Scheduler.spawn;

var _elm_lang$mouse$Mouse_ops = _elm_lang$mouse$Mouse_ops || {};
_elm_lang$mouse$Mouse_ops['&>'] = F2(
	function (t1, t2) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (_p0) {
				return t2;
			},
			t1);
	});
var _elm_lang$mouse$Mouse$onSelfMsg = F3(
	function (router, _p1, state) {
		var _p2 = _p1;
		var _p3 = A2(_elm_lang$core$Dict$get, _p2.category, state);
		if (_p3.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var send = function (tagger) {
				return A2(
					_elm_lang$core$Platform$sendToApp,
					router,
					tagger(_p2.position));
			};
			return A2(
				_elm_lang$mouse$Mouse_ops['&>'],
				_elm_lang$core$Task$sequence(
					A2(_elm_lang$core$List$map, send, _p3._0.taggers)),
				_elm_lang$core$Task$succeed(state));
		}
	});
var _elm_lang$mouse$Mouse$init = _elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty);
var _elm_lang$mouse$Mouse$categorizeHelpHelp = F2(
	function (value, maybeValues) {
		var _p4 = maybeValues;
		if (_p4.ctor === 'Nothing') {
			return _elm_lang$core$Maybe$Just(
				{
					ctor: '::',
					_0: value,
					_1: {ctor: '[]'}
				});
		} else {
			return _elm_lang$core$Maybe$Just(
				{ctor: '::', _0: value, _1: _p4._0});
		}
	});
var _elm_lang$mouse$Mouse$categorizeHelp = F2(
	function (subs, subDict) {
		categorizeHelp:
		while (true) {
			var _p5 = subs;
			if (_p5.ctor === '[]') {
				return subDict;
			} else {
				var _v4 = _p5._1,
					_v5 = A3(
					_elm_lang$core$Dict$update,
					_p5._0._0,
					_elm_lang$mouse$Mouse$categorizeHelpHelp(_p5._0._1),
					subDict);
				subs = _v4;
				subDict = _v5;
				continue categorizeHelp;
			}
		}
	});
var _elm_lang$mouse$Mouse$categorize = function (subs) {
	return A2(_elm_lang$mouse$Mouse$categorizeHelp, subs, _elm_lang$core$Dict$empty);
};
var _elm_lang$mouse$Mouse$subscription = _elm_lang$core$Native_Platform.leaf('Mouse');
var _elm_lang$mouse$Mouse$Position = F2(
	function (a, b) {
		return {x: a, y: b};
	});
var _elm_lang$mouse$Mouse$position = A3(
	_elm_lang$core$Json_Decode$map2,
	_elm_lang$mouse$Mouse$Position,
	A2(_elm_lang$core$Json_Decode$field, 'pageX', _elm_lang$core$Json_Decode$int),
	A2(_elm_lang$core$Json_Decode$field, 'pageY', _elm_lang$core$Json_Decode$int));
var _elm_lang$mouse$Mouse$Watcher = F2(
	function (a, b) {
		return {taggers: a, pid: b};
	});
var _elm_lang$mouse$Mouse$Msg = F2(
	function (a, b) {
		return {category: a, position: b};
	});
var _elm_lang$mouse$Mouse$onEffects = F3(
	function (router, newSubs, oldState) {
		var rightStep = F3(
			function (category, taggers, task) {
				var tracker = A3(
					_elm_lang$dom$Dom_LowLevel$onDocument,
					category,
					_elm_lang$mouse$Mouse$position,
					function (_p6) {
						return A2(
							_elm_lang$core$Platform$sendToSelf,
							router,
							A2(_elm_lang$mouse$Mouse$Msg, category, _p6));
					});
				return A2(
					_elm_lang$core$Task$andThen,
					function (state) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (pid) {
								return _elm_lang$core$Task$succeed(
									A3(
										_elm_lang$core$Dict$insert,
										category,
										A2(_elm_lang$mouse$Mouse$Watcher, taggers, pid),
										state));
							},
							_elm_lang$core$Process$spawn(tracker));
					},
					task);
			});
		var bothStep = F4(
			function (category, _p7, taggers, task) {
				var _p8 = _p7;
				return A2(
					_elm_lang$core$Task$andThen,
					function (state) {
						return _elm_lang$core$Task$succeed(
							A3(
								_elm_lang$core$Dict$insert,
								category,
								A2(_elm_lang$mouse$Mouse$Watcher, taggers, _p8.pid),
								state));
					},
					task);
			});
		var leftStep = F3(
			function (category, _p9, task) {
				var _p10 = _p9;
				return A2(
					_elm_lang$mouse$Mouse_ops['&>'],
					_elm_lang$core$Process$kill(_p10.pid),
					task);
			});
		return A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			oldState,
			_elm_lang$mouse$Mouse$categorize(newSubs),
			_elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty));
	});
var _elm_lang$mouse$Mouse$MySub = F2(
	function (a, b) {
		return {ctor: 'MySub', _0: a, _1: b};
	});
var _elm_lang$mouse$Mouse$clicks = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'click', tagger));
};
var _elm_lang$mouse$Mouse$moves = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mousemove', tagger));
};
var _elm_lang$mouse$Mouse$downs = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mousedown', tagger));
};
var _elm_lang$mouse$Mouse$ups = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mouseup', tagger));
};
var _elm_lang$mouse$Mouse$subMap = F2(
	function (func, _p11) {
		var _p12 = _p11;
		return A2(
			_elm_lang$mouse$Mouse$MySub,
			_p12._0,
			function (_p13) {
				return func(
					_p12._1(_p13));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Mouse'] = {pkg: 'elm-lang/mouse', init: _elm_lang$mouse$Mouse$init, onEffects: _elm_lang$mouse$Mouse$onEffects, onSelfMsg: _elm_lang$mouse$Mouse$onSelfMsg, tag: 'sub', subMap: _elm_lang$mouse$Mouse$subMap};

var _elm_lang$animation_frame$Native_AnimationFrame = function()
{

function create()
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var id = requestAnimationFrame(function() {
			callback(_elm_lang$core$Native_Scheduler.succeed(Date.now()));
		});

		return function() {
			cancelAnimationFrame(id);
		};
	});
}

return {
	create: create
};

}();

var _elm_lang$animation_frame$AnimationFrame$rAF = _elm_lang$animation_frame$Native_AnimationFrame.create(
	{ctor: '_Tuple0'});
var _elm_lang$animation_frame$AnimationFrame$subscription = _elm_lang$core$Native_Platform.leaf('AnimationFrame');
var _elm_lang$animation_frame$AnimationFrame$State = F3(
	function (a, b, c) {
		return {subs: a, request: b, oldTime: c};
	});
var _elm_lang$animation_frame$AnimationFrame$init = _elm_lang$core$Task$succeed(
	A3(
		_elm_lang$animation_frame$AnimationFrame$State,
		{ctor: '[]'},
		_elm_lang$core$Maybe$Nothing,
		0));
var _elm_lang$animation_frame$AnimationFrame$onEffects = F3(
	function (router, subs, _p0) {
		var _p1 = _p0;
		var _p5 = _p1.request;
		var _p4 = _p1.oldTime;
		var _p2 = {ctor: '_Tuple2', _0: _p5, _1: subs};
		if (_p2._0.ctor === 'Nothing') {
			if (_p2._1.ctor === '[]') {
				return _elm_lang$core$Task$succeed(
					A3(
						_elm_lang$animation_frame$AnimationFrame$State,
						{ctor: '[]'},
						_elm_lang$core$Maybe$Nothing,
						_p4));
			} else {
				return A2(
					_elm_lang$core$Task$andThen,
					function (pid) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (time) {
								return _elm_lang$core$Task$succeed(
									A3(
										_elm_lang$animation_frame$AnimationFrame$State,
										subs,
										_elm_lang$core$Maybe$Just(pid),
										time));
							},
							_elm_lang$core$Time$now);
					},
					_elm_lang$core$Process$spawn(
						A2(
							_elm_lang$core$Task$andThen,
							_elm_lang$core$Platform$sendToSelf(router),
							_elm_lang$animation_frame$AnimationFrame$rAF)));
			}
		} else {
			if (_p2._1.ctor === '[]') {
				return A2(
					_elm_lang$core$Task$andThen,
					function (_p3) {
						return _elm_lang$core$Task$succeed(
							A3(
								_elm_lang$animation_frame$AnimationFrame$State,
								{ctor: '[]'},
								_elm_lang$core$Maybe$Nothing,
								_p4));
					},
					_elm_lang$core$Process$kill(_p2._0._0));
			} else {
				return _elm_lang$core$Task$succeed(
					A3(_elm_lang$animation_frame$AnimationFrame$State, subs, _p5, _p4));
			}
		}
	});
var _elm_lang$animation_frame$AnimationFrame$onSelfMsg = F3(
	function (router, newTime, _p6) {
		var _p7 = _p6;
		var _p10 = _p7.subs;
		var diff = newTime - _p7.oldTime;
		var send = function (sub) {
			var _p8 = sub;
			if (_p8.ctor === 'Time') {
				return A2(
					_elm_lang$core$Platform$sendToApp,
					router,
					_p8._0(newTime));
			} else {
				return A2(
					_elm_lang$core$Platform$sendToApp,
					router,
					_p8._0(diff));
			}
		};
		return A2(
			_elm_lang$core$Task$andThen,
			function (pid) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (_p9) {
						return _elm_lang$core$Task$succeed(
							A3(
								_elm_lang$animation_frame$AnimationFrame$State,
								_p10,
								_elm_lang$core$Maybe$Just(pid),
								newTime));
					},
					_elm_lang$core$Task$sequence(
						A2(_elm_lang$core$List$map, send, _p10)));
			},
			_elm_lang$core$Process$spawn(
				A2(
					_elm_lang$core$Task$andThen,
					_elm_lang$core$Platform$sendToSelf(router),
					_elm_lang$animation_frame$AnimationFrame$rAF)));
	});
var _elm_lang$animation_frame$AnimationFrame$Diff = function (a) {
	return {ctor: 'Diff', _0: a};
};
var _elm_lang$animation_frame$AnimationFrame$diffs = function (tagger) {
	return _elm_lang$animation_frame$AnimationFrame$subscription(
		_elm_lang$animation_frame$AnimationFrame$Diff(tagger));
};
var _elm_lang$animation_frame$AnimationFrame$Time = function (a) {
	return {ctor: 'Time', _0: a};
};
var _elm_lang$animation_frame$AnimationFrame$times = function (tagger) {
	return _elm_lang$animation_frame$AnimationFrame$subscription(
		_elm_lang$animation_frame$AnimationFrame$Time(tagger));
};
var _elm_lang$animation_frame$AnimationFrame$subMap = F2(
	function (func, sub) {
		var _p11 = sub;
		if (_p11.ctor === 'Time') {
			return _elm_lang$animation_frame$AnimationFrame$Time(
				function (_p12) {
					return func(
						_p11._0(_p12));
				});
		} else {
			return _elm_lang$animation_frame$AnimationFrame$Diff(
				function (_p13) {
					return func(
						_p11._0(_p13));
				});
		}
	});
_elm_lang$core$Native_Platform.effectManagers['AnimationFrame'] = {pkg: 'elm-lang/animation-frame', init: _elm_lang$animation_frame$AnimationFrame$init, onEffects: _elm_lang$animation_frame$AnimationFrame$onEffects, onSelfMsg: _elm_lang$animation_frame$AnimationFrame$onSelfMsg, tag: 'sub', subMap: _elm_lang$animation_frame$AnimationFrame$subMap};

var _debois$elm_dom$DOM$className = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'className',
		_1: {ctor: '[]'}
	},
	_elm_lang$core$Json_Decode$string);
var _debois$elm_dom$DOM$scrollTop = A2(_elm_lang$core$Json_Decode$field, 'scrollTop', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$scrollLeft = A2(_elm_lang$core$Json_Decode$field, 'scrollLeft', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$offsetTop = A2(_elm_lang$core$Json_Decode$field, 'offsetTop', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$offsetLeft = A2(_elm_lang$core$Json_Decode$field, 'offsetLeft', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$offsetHeight = A2(_elm_lang$core$Json_Decode$field, 'offsetHeight', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$offsetWidth = A2(_elm_lang$core$Json_Decode$field, 'offsetWidth', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$childNodes = function (decoder) {
	var loop = F2(
		function (idx, xs) {
			return A2(
				_elm_lang$core$Json_Decode$andThen,
				function (_p0) {
					return A2(
						_elm_lang$core$Maybe$withDefault,
						_elm_lang$core$Json_Decode$succeed(xs),
						A2(
							_elm_lang$core$Maybe$map,
							function (x) {
								return A2(
									loop,
									idx + 1,
									{ctor: '::', _0: x, _1: xs});
							},
							_p0));
				},
				_elm_lang$core$Json_Decode$maybe(
					A2(
						_elm_lang$core$Json_Decode$field,
						_elm_lang$core$Basics$toString(idx),
						decoder)));
		});
	return A2(
		_elm_lang$core$Json_Decode$map,
		_elm_lang$core$List$reverse,
		A2(
			_elm_lang$core$Json_Decode$field,
			'childNodes',
			A2(
				loop,
				0,
				{ctor: '[]'})));
};
var _debois$elm_dom$DOM$childNode = function (idx) {
	return _elm_lang$core$Json_Decode$at(
		{
			ctor: '::',
			_0: 'childNodes',
			_1: {
				ctor: '::',
				_0: _elm_lang$core$Basics$toString(idx),
				_1: {ctor: '[]'}
			}
		});
};
var _debois$elm_dom$DOM$parentElement = function (decoder) {
	return A2(_elm_lang$core$Json_Decode$field, 'parentElement', decoder);
};
var _debois$elm_dom$DOM$previousSibling = function (decoder) {
	return A2(_elm_lang$core$Json_Decode$field, 'previousSibling', decoder);
};
var _debois$elm_dom$DOM$nextSibling = function (decoder) {
	return A2(_elm_lang$core$Json_Decode$field, 'nextSibling', decoder);
};
var _debois$elm_dom$DOM$offsetParent = F2(
	function (x, decoder) {
		return _elm_lang$core$Json_Decode$oneOf(
			{
				ctor: '::',
				_0: A2(
					_elm_lang$core$Json_Decode$field,
					'offsetParent',
					_elm_lang$core$Json_Decode$null(x)),
				_1: {
					ctor: '::',
					_0: A2(_elm_lang$core$Json_Decode$field, 'offsetParent', decoder),
					_1: {ctor: '[]'}
				}
			});
	});
var _debois$elm_dom$DOM$position = F2(
	function (x, y) {
		return A2(
			_elm_lang$core$Json_Decode$andThen,
			function (_p1) {
				var _p2 = _p1;
				var _p4 = _p2._1;
				var _p3 = _p2._0;
				return A2(
					_debois$elm_dom$DOM$offsetParent,
					{ctor: '_Tuple2', _0: _p3, _1: _p4},
					A2(_debois$elm_dom$DOM$position, _p3, _p4));
			},
			A5(
				_elm_lang$core$Json_Decode$map4,
				F4(
					function (scrollLeft, scrollTop, offsetLeft, offsetTop) {
						return {ctor: '_Tuple2', _0: (x + offsetLeft) - scrollLeft, _1: (y + offsetTop) - scrollTop};
					}),
				_debois$elm_dom$DOM$scrollLeft,
				_debois$elm_dom$DOM$scrollTop,
				_debois$elm_dom$DOM$offsetLeft,
				_debois$elm_dom$DOM$offsetTop));
	});
var _debois$elm_dom$DOM$boundingClientRect = A4(
	_elm_lang$core$Json_Decode$map3,
	F3(
		function (_p5, width, height) {
			var _p6 = _p5;
			return {top: _p6._1, left: _p6._0, width: width, height: height};
		}),
	A2(_debois$elm_dom$DOM$position, 0, 0),
	_debois$elm_dom$DOM$offsetWidth,
	_debois$elm_dom$DOM$offsetHeight);
var _debois$elm_dom$DOM$target = function (decoder) {
	return A2(_elm_lang$core$Json_Decode$field, 'target', decoder);
};
var _debois$elm_dom$DOM$Rectangle = F4(
	function (a, b, c, d) {
		return {top: a, left: b, width: c, height: d};
	});

var _rundis$elm_bootstrap$Bootstrap_Dropdown$sizeDecoder = A3(
	_elm_lang$core$Json_Decode$map2,
	F2(
		function (v0, v1) {
			return {ctor: '_Tuple2', _0: v0, _1: v1};
		}),
	_debois$elm_dom$DOM$target(_debois$elm_dom$DOM$boundingClientRect),
	_debois$elm_dom$DOM$target(
		_debois$elm_dom$DOM$nextSibling(
			A2(_debois$elm_dom$DOM$childNode, 0, _debois$elm_dom$DOM$boundingClientRect))));
var _rundis$elm_bootstrap$Bootstrap_Dropdown$applyModifier = F2(
	function (option, options) {
		var _p0 = option;
		switch (_p0.ctor) {
			case 'AlignMenuRight':
				return _elm_lang$core$Native_Utils.update(
					options,
					{hasMenuRight: true});
			case 'Dropup':
				return _elm_lang$core$Native_Utils.update(
					options,
					{isDropUp: true});
			case 'Attrs':
				return _elm_lang$core$Native_Utils.update(
					options,
					{attributes: _p0._0});
			case 'DropToDir':
				return _elm_lang$core$Native_Utils.update(
					options,
					{
						dropDirection: _elm_lang$core$Maybe$Just(_p0._0)
					});
			default:
				return _elm_lang$core$Native_Utils.update(
					options,
					{menuAttrs: _p0._0});
		}
	});
var _rundis$elm_bootstrap$Bootstrap_Dropdown$defaultOptions = {
	hasMenuRight: false,
	isDropUp: false,
	attributes: {ctor: '[]'},
	dropDirection: _elm_lang$core$Maybe$Nothing,
	menuAttrs: {ctor: '[]'}
};
var _rundis$elm_bootstrap$Bootstrap_Dropdown$toConfig = function (options) {
	return A3(_elm_lang$core$List$foldl, _rundis$elm_bootstrap$Bootstrap_Dropdown$applyModifier, _rundis$elm_bootstrap$Bootstrap_Dropdown$defaultOptions, options);
};
var _rundis$elm_bootstrap$Bootstrap_Dropdown$menuStyle = F2(
	function (_p1, config) {
		var _p2 = _p1;
		var _p5 = _p2._0.toggleSize;
		var _p4 = _p2._0.menuSize;
		var px = function (n) {
			return A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Basics$toString(n),
				'px');
		};
		var translate = F3(
			function (x, y, z) {
				return A2(
					_elm_lang$core$Basics_ops['++'],
					'translate3d(',
					A2(
						_elm_lang$core$Basics_ops['++'],
						px(x),
						A2(
							_elm_lang$core$Basics_ops['++'],
							',',
							A2(
								_elm_lang$core$Basics_ops['++'],
								px(y),
								A2(
									_elm_lang$core$Basics_ops['++'],
									',',
									A2(
										_elm_lang$core$Basics_ops['++'],
										px(z),
										')'))))));
			});
		var $default = {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: 'top', _1: '0'},
			_1: {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'left', _1: '0'},
				_1: {ctor: '[]'}
			}
		};
		return _elm_lang$html$Html_Attributes$style(
			function () {
				var _p3 = {ctor: '_Tuple2', _0: config.isDropUp, _1: config.dropDirection};
				_v2_3:
				do {
					_v2_0:
					do {
						if (_p3.ctor === '_Tuple2') {
							if (_p3._1.ctor === 'Just') {
								if (_p3._1._0.ctor === 'Dropright') {
									if (_p3._0 === true) {
										break _v2_0;
									} else {
										return $default;
									}
								} else {
									if (_p3._0 === true) {
										break _v2_0;
									} else {
										return A2(
											_elm_lang$core$Basics_ops['++'],
											$default,
											{
												ctor: '::',
												_0: {
													ctor: '_Tuple2',
													_0: 'transform',
													_1: A3(translate, (0 - _p5.width) - _p4.width, 0, 0)
												},
												_1: {ctor: '[]'}
											});
									}
								}
							} else {
								if (_p3._0 === true) {
									break _v2_0;
								} else {
									break _v2_3;
								}
							}
						} else {
							break _v2_3;
						}
					} while(false);
					return A2(
						_elm_lang$core$Basics_ops['++'],
						$default,
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'transform',
								_1: A3(translate, 0 - _p5.width, 0 - _p4.height, 0)
							},
							_1: {ctor: '[]'}
						});
				} while(false);
				return A2(
					_elm_lang$core$Basics_ops['++'],
					$default,
					{
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'transform',
							_1: A3(translate, 0 - _p5.width, _p5.height, 0)
						},
						_1: {ctor: '[]'}
					});
			}());
	});
var _rundis$elm_bootstrap$Bootstrap_Dropdown$dropDir = function (maybeDir) {
	var toAttrs = function (dir) {
		return {
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class(
				A2(
					_elm_lang$core$Basics_ops['++'],
					'drop',
					function () {
						var _p6 = dir;
						if (_p6.ctor === 'Dropleft') {
							return 'left';
						} else {
							return 'right';
						}
					}())),
			_1: {ctor: '[]'}
		};
	};
	return A2(
		_elm_lang$core$Maybe$withDefault,
		{ctor: '[]'},
		A2(_elm_lang$core$Maybe$map, toAttrs, maybeDir));
};
var _rundis$elm_bootstrap$Bootstrap_Dropdown$StateRec = F3(
	function (a, b, c) {
		return {status: a, toggleSize: b, menuSize: c};
	});
var _rundis$elm_bootstrap$Bootstrap_Dropdown$SplitToggleConfig = F3(
	function (a, b, c) {
		return {options: a, togglerOptions: b, children: c};
	});
var _rundis$elm_bootstrap$Bootstrap_Dropdown$Options = F5(
	function (a, b, c, d, e) {
		return {isDropUp: a, hasMenuRight: b, dropDirection: c, attributes: d, menuAttrs: e};
	});
var _rundis$elm_bootstrap$Bootstrap_Dropdown$State = function (a) {
	return {ctor: 'State', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Dropdown$updateStatus = F2(
	function (status, _p7) {
		var _p8 = _p7;
		return _rundis$elm_bootstrap$Bootstrap_Dropdown$State(
			_elm_lang$core$Native_Utils.update(
				_p8._0,
				{status: status}));
	});
var _rundis$elm_bootstrap$Bootstrap_Dropdown$Closed = {ctor: 'Closed'};
var _rundis$elm_bootstrap$Bootstrap_Dropdown$initialState = _rundis$elm_bootstrap$Bootstrap_Dropdown$State(
	{
		status: _rundis$elm_bootstrap$Bootstrap_Dropdown$Closed,
		toggleSize: A4(_debois$elm_dom$DOM$Rectangle, 0, 0, 0, 0),
		menuSize: A4(_debois$elm_dom$DOM$Rectangle, 0, 0, 0, 0)
	});
var _rundis$elm_bootstrap$Bootstrap_Dropdown$dropdownAttributes = F2(
	function (status, config) {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$classList(
					{
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'btn-group', _1: true},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'show',
								_1: !_elm_lang$core$Native_Utils.eq(status, _rundis$elm_bootstrap$Bootstrap_Dropdown$Closed)
							},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'dropup', _1: config.isDropUp},
								_1: {ctor: '[]'}
							}
						}
					}),
				_1: {ctor: '[]'}
			},
			A2(
				_elm_lang$core$Basics_ops['++'],
				_rundis$elm_bootstrap$Bootstrap_Dropdown$dropDir(config.dropDirection),
				config.attributes));
	});
var _rundis$elm_bootstrap$Bootstrap_Dropdown$dropdownMenu = F3(
	function (_p9, config, items) {
		var _p10 = _p9;
		var wrapperStyle = _elm_lang$core$Native_Utils.eq(_p10._0.status, _rundis$elm_bootstrap$Bootstrap_Dropdown$Closed) ? {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: 'height', _1: '0'},
			_1: {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'overflow', _1: 'hidden'},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'position', _1: 'relative'},
					_1: {ctor: '[]'}
				}
			}
		} : {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: 'position', _1: 'relative'},
			_1: {ctor: '[]'}
		};
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$style(wrapperStyle),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					A2(
						_elm_lang$core$Basics_ops['++'],
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$classList(
								{
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'dropdown-menu', _1: true},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'dropdown-menu-right', _1: config.hasMenuRight},
										_1: {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'show', _1: true},
											_1: {ctor: '[]'}
										}
									}
								}),
							_1: {
								ctor: '::',
								_0: A2(_rundis$elm_bootstrap$Bootstrap_Dropdown$menuStyle, _p10, config),
								_1: {ctor: '[]'}
							}
						},
						config.menuAttrs),
					A2(
						_elm_lang$core$List$map,
						function (_p11) {
							var _p12 = _p11;
							return _p12._0;
						},
						items)),
				_1: {ctor: '[]'}
			});
	});
var _rundis$elm_bootstrap$Bootstrap_Dropdown$dropdown = F2(
	function (_p14, _p13) {
		var _p15 = _p14;
		var _p18 = _p15;
		var _p16 = _p13;
		var config = _rundis$elm_bootstrap$Bootstrap_Dropdown$toConfig(_p16.options);
		var _p17 = _p16.toggleButton;
		var buttonFn = _p17._0;
		return A2(
			_elm_lang$html$Html$div,
			A2(_rundis$elm_bootstrap$Bootstrap_Dropdown$dropdownAttributes, _p15._0.status, config),
			{
				ctor: '::',
				_0: A2(buttonFn, _p16.toggleMsg, _p18),
				_1: {
					ctor: '::',
					_0: A3(_rundis$elm_bootstrap$Bootstrap_Dropdown$dropdownMenu, _p18, config, _p16.items),
					_1: {ctor: '[]'}
				}
			});
	});
var _rundis$elm_bootstrap$Bootstrap_Dropdown$splitDropdown = F2(
	function (_p20, _p19) {
		var _p21 = _p20;
		var _p24 = _p21;
		var _p22 = _p19;
		var config = _rundis$elm_bootstrap$Bootstrap_Dropdown$toConfig(_p22.options);
		var _p23 = _p22.toggleButton;
		var buttonsFn = _p23._0;
		return A2(
			_elm_lang$html$Html$div,
			A2(_rundis$elm_bootstrap$Bootstrap_Dropdown$dropdownAttributes, _p21._0.status, config),
			A2(
				_elm_lang$core$Basics_ops['++'],
				A2(buttonsFn, _p22.toggleMsg, _p24),
				{
					ctor: '::',
					_0: A3(_rundis$elm_bootstrap$Bootstrap_Dropdown$dropdownMenu, _p24, config, _p22.items),
					_1: {ctor: '[]'}
				}));
	});
var _rundis$elm_bootstrap$Bootstrap_Dropdown$ListenClicks = {ctor: 'ListenClicks'};
var _rundis$elm_bootstrap$Bootstrap_Dropdown$subscriptions = F2(
	function (_p25, toMsg) {
		var _p26 = _p25;
		var _p30 = _p26;
		var _p27 = _p26._0.status;
		switch (_p27.ctor) {
			case 'Open':
				return _elm_lang$animation_frame$AnimationFrame$times(
					function (_p28) {
						return toMsg(
							A2(_rundis$elm_bootstrap$Bootstrap_Dropdown$updateStatus, _rundis$elm_bootstrap$Bootstrap_Dropdown$ListenClicks, _p30));
					});
			case 'ListenClicks':
				return _elm_lang$mouse$Mouse$clicks(
					function (_p29) {
						return toMsg(
							A2(_rundis$elm_bootstrap$Bootstrap_Dropdown$updateStatus, _rundis$elm_bootstrap$Bootstrap_Dropdown$Closed, _p30));
					});
			default:
				return _elm_lang$core$Platform_Sub$none;
		}
	});
var _rundis$elm_bootstrap$Bootstrap_Dropdown$Open = {ctor: 'Open'};
var _rundis$elm_bootstrap$Bootstrap_Dropdown$nextStatus = function (status) {
	var _p31 = status;
	switch (_p31.ctor) {
		case 'Open':
			return _rundis$elm_bootstrap$Bootstrap_Dropdown$Closed;
		case 'ListenClicks':
			return _rundis$elm_bootstrap$Bootstrap_Dropdown$Closed;
		default:
			return _rundis$elm_bootstrap$Bootstrap_Dropdown$Open;
	}
};
var _rundis$elm_bootstrap$Bootstrap_Dropdown$clickHandler = F2(
	function (toMsg, _p32) {
		var _p33 = _p32;
		return A2(
			_elm_lang$core$Json_Decode$andThen,
			function (_p34) {
				var _p35 = _p34;
				return _elm_lang$core$Json_Decode$succeed(
					toMsg(
						_rundis$elm_bootstrap$Bootstrap_Dropdown$State(
							{
								status: _rundis$elm_bootstrap$Bootstrap_Dropdown$nextStatus(_p33._0.status),
								toggleSize: _p35._0,
								menuSize: _p35._1
							})));
			},
			_rundis$elm_bootstrap$Bootstrap_Dropdown$sizeDecoder);
	});
var _rundis$elm_bootstrap$Bootstrap_Dropdown$togglePrivate = F4(
	function (buttonOptions, children, toggleMsg, state) {
		return A2(
			_elm_lang$html$Html$button,
			A2(
				_elm_lang$core$Basics_ops['++'],
				_rundis$elm_bootstrap$Bootstrap_Internal_Button$buttonAttributes(buttonOptions),
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('dropdown-toggle'),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$type_('button'),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html_Events$on,
								'click',
								A2(_rundis$elm_bootstrap$Bootstrap_Dropdown$clickHandler, toggleMsg, state)),
							_1: {ctor: '[]'}
						}
					}
				}),
			children);
	});
var _rundis$elm_bootstrap$Bootstrap_Dropdown$splitToggleButtonPrivate = F3(
	function (_p36, toggleMsg, state) {
		var _p37 = _p36;
		return {
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$button,
				_rundis$elm_bootstrap$Bootstrap_Internal_Button$buttonAttributes(_p37.options),
				_p37.children),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$button,
					A2(
						_elm_lang$core$Basics_ops['++'],
						_rundis$elm_bootstrap$Bootstrap_Internal_Button$buttonAttributes(_p37.togglerOptions),
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('dropdown-toggle'),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('dropdown-toggle-split'),
								_1: {
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$type_('button'),
									_1: {
										ctor: '::',
										_0: A2(
											_elm_lang$html$Html_Events$on,
											'click',
											A2(_rundis$elm_bootstrap$Bootstrap_Dropdown$clickHandler, toggleMsg, state)),
										_1: {ctor: '[]'}
									}
								}
							}
						}),
					{ctor: '[]'}),
				_1: {ctor: '[]'}
			}
		};
	});
var _rundis$elm_bootstrap$Bootstrap_Dropdown$toggleOpen = F2(
	function (toMsg, _p38) {
		var _p39 = _p38;
		return toMsg(
			A2(
				_rundis$elm_bootstrap$Bootstrap_Dropdown$updateStatus,
				_rundis$elm_bootstrap$Bootstrap_Dropdown$nextStatus(_p39._0.status),
				_p39));
	});
var _rundis$elm_bootstrap$Bootstrap_Dropdown$Attrs = function (a) {
	return {ctor: 'Attrs', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Dropdown$attrs = function (attrs) {
	return _rundis$elm_bootstrap$Bootstrap_Dropdown$Attrs(attrs);
};
var _rundis$elm_bootstrap$Bootstrap_Dropdown$MenuAttrs = function (a) {
	return {ctor: 'MenuAttrs', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Dropdown$menuAttrs = function (attrs) {
	return _rundis$elm_bootstrap$Bootstrap_Dropdown$MenuAttrs(attrs);
};
var _rundis$elm_bootstrap$Bootstrap_Dropdown$DropToDir = function (a) {
	return {ctor: 'DropToDir', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Dropdown$AlignMenuRight = {ctor: 'AlignMenuRight'};
var _rundis$elm_bootstrap$Bootstrap_Dropdown$alignMenuRight = _rundis$elm_bootstrap$Bootstrap_Dropdown$AlignMenuRight;
var _rundis$elm_bootstrap$Bootstrap_Dropdown$Dropup = {ctor: 'Dropup'};
var _rundis$elm_bootstrap$Bootstrap_Dropdown$dropUp = _rundis$elm_bootstrap$Bootstrap_Dropdown$Dropup;
var _rundis$elm_bootstrap$Bootstrap_Dropdown$Dropright = {ctor: 'Dropright'};
var _rundis$elm_bootstrap$Bootstrap_Dropdown$dropRight = _rundis$elm_bootstrap$Bootstrap_Dropdown$DropToDir(_rundis$elm_bootstrap$Bootstrap_Dropdown$Dropright);
var _rundis$elm_bootstrap$Bootstrap_Dropdown$Dropleft = {ctor: 'Dropleft'};
var _rundis$elm_bootstrap$Bootstrap_Dropdown$dropLeft = _rundis$elm_bootstrap$Bootstrap_Dropdown$DropToDir(_rundis$elm_bootstrap$Bootstrap_Dropdown$Dropleft);
var _rundis$elm_bootstrap$Bootstrap_Dropdown$DropdownItem = function (a) {
	return {ctor: 'DropdownItem', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Dropdown$anchorItem = F2(
	function (attributes, children) {
		return _rundis$elm_bootstrap$Bootstrap_Dropdown$DropdownItem(
			A2(
				_elm_lang$html$Html$a,
				A2(
					_elm_lang$core$Basics_ops['++'],
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('dropdown-item'),
						_1: {ctor: '[]'}
					},
					attributes),
				children));
	});
var _rundis$elm_bootstrap$Bootstrap_Dropdown$buttonItem = F2(
	function (attributes, children) {
		return _rundis$elm_bootstrap$Bootstrap_Dropdown$DropdownItem(
			A2(
				_elm_lang$html$Html$button,
				A2(
					_elm_lang$core$Basics_ops['++'],
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$type_('button'),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('dropdown-item'),
							_1: {ctor: '[]'}
						}
					},
					attributes),
				children));
	});
var _rundis$elm_bootstrap$Bootstrap_Dropdown$divider = _rundis$elm_bootstrap$Bootstrap_Dropdown$DropdownItem(
	A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('dropdown-divider'),
			_1: {ctor: '[]'}
		},
		{ctor: '[]'}));
var _rundis$elm_bootstrap$Bootstrap_Dropdown$header = function (children) {
	return _rundis$elm_bootstrap$Bootstrap_Dropdown$DropdownItem(
		A2(
			_elm_lang$html$Html$h6,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('dropdown-header'),
				_1: {ctor: '[]'}
			},
			children));
};
var _rundis$elm_bootstrap$Bootstrap_Dropdown$DropdownToggle = function (a) {
	return {ctor: 'DropdownToggle', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Dropdown$toggle = F2(
	function (buttonOptions, children) {
		return _rundis$elm_bootstrap$Bootstrap_Dropdown$DropdownToggle(
			A2(_rundis$elm_bootstrap$Bootstrap_Dropdown$togglePrivate, buttonOptions, children));
	});
var _rundis$elm_bootstrap$Bootstrap_Dropdown$SplitDropdownToggle = function (a) {
	return {ctor: 'SplitDropdownToggle', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Dropdown$splitToggle = function (config) {
	return _rundis$elm_bootstrap$Bootstrap_Dropdown$SplitDropdownToggle(
		_rundis$elm_bootstrap$Bootstrap_Dropdown$splitToggleButtonPrivate(config));
};

var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$sizeAttribute = function (size) {
	return A2(
		_elm_lang$core$Maybe$map,
		function (s) {
			return _elm_lang$html$Html_Attributes$class(
				A2(_elm_lang$core$Basics_ops['++'], 'input-group-', s));
		},
		_rundis$elm_bootstrap$Bootstrap_Grid_Internal$screenSizeOption(size));
};
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$view = function (_p0) {
	var _p1 = _p0;
	var _p7 = _p1._0;
	var _p2 = _p7.input;
	var input = _p2._0;
	return A2(
		_elm_lang$html$Html$div,
		A2(
			_elm_lang$core$Basics_ops['++'],
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('input-group'),
				_1: {ctor: '[]'}
			},
			A2(
				_elm_lang$core$Basics_ops['++'],
				A2(
					_elm_lang$core$List$filterMap,
					_elm_lang$core$Basics$identity,
					{
						ctor: '::',
						_0: A2(_elm_lang$core$Maybe$andThen, _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$sizeAttribute, _p7.size),
						_1: {ctor: '[]'}
					}),
				_p7.attributes)),
		A2(
			_elm_lang$core$Basics_ops['++'],
			A2(
				_elm_lang$core$List$map,
				function (_p3) {
					var _p4 = _p3;
					return A2(
						_elm_lang$html$Html$div,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('input-group-prepend'),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _p4._0,
							_1: {ctor: '[]'}
						});
				},
				_p7.predecessors),
			A2(
				_elm_lang$core$Basics_ops['++'],
				{
					ctor: '::',
					_0: input,
					_1: {ctor: '[]'}
				},
				A2(
					_elm_lang$core$List$map,
					function (_p5) {
						var _p6 = _p5;
						return A2(
							_elm_lang$html$Html$div,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('input-group-prepend'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: _p6._0,
								_1: {ctor: '[]'}
							});
					},
					_p7.successors))));
};
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$Config = function (a) {
	return {ctor: 'Config', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$config = function (input) {
	return _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$Config(
		{
			input: input,
			predecessors: {ctor: '[]'},
			successors: {ctor: '[]'},
			size: _elm_lang$core$Maybe$Nothing,
			attributes: {ctor: '[]'}
		});
};
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$predecessors = F2(
	function (addons, _p8) {
		var _p9 = _p8;
		return _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$Config(
			_elm_lang$core$Native_Utils.update(
				_p9._0,
				{predecessors: addons}));
	});
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$successors = F2(
	function (addons, _p10) {
		var _p11 = _p10;
		return _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$Config(
			_elm_lang$core$Native_Utils.update(
				_p11._0,
				{successors: addons}));
	});
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$large = function (_p12) {
	var _p13 = _p12;
	return _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$Config(
		_elm_lang$core$Native_Utils.update(
			_p13._0,
			{
				size: _elm_lang$core$Maybe$Just(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$LG)
			}));
};
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$small = function (_p14) {
	var _p15 = _p14;
	return _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$Config(
		_elm_lang$core$Native_Utils.update(
			_p15._0,
			{
				size: _elm_lang$core$Maybe$Just(_rundis$elm_bootstrap$Bootstrap_Grid_Internal$SM)
			}));
};
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$attrs = F2(
	function (attributes, _p16) {
		var _p17 = _p16;
		return _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$Config(
			_elm_lang$core$Native_Utils.update(
				_p17._0,
				{attributes: attributes}));
	});
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$Input = function (a) {
	return {ctor: 'Input', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$input = F2(
	function (inputFn, options) {
		return _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$Input(
			inputFn(options));
	});
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$text = _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$input(_rundis$elm_bootstrap$Bootstrap_Form_Input$text);
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$password = _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$input(_rundis$elm_bootstrap$Bootstrap_Form_Input$password);
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$datetimeLocal = _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$input(_rundis$elm_bootstrap$Bootstrap_Form_Input$datetimeLocal);
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$date = _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$input(_rundis$elm_bootstrap$Bootstrap_Form_Input$date);
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$month = _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$input(_rundis$elm_bootstrap$Bootstrap_Form_Input$month);
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$time = _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$input(_rundis$elm_bootstrap$Bootstrap_Form_Input$time);
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$week = _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$input(_rundis$elm_bootstrap$Bootstrap_Form_Input$week);
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$number = _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$input(_rundis$elm_bootstrap$Bootstrap_Form_Input$number);
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$email = _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$input(_rundis$elm_bootstrap$Bootstrap_Form_Input$email);
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$url = _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$input(_rundis$elm_bootstrap$Bootstrap_Form_Input$url);
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$search = _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$input(_rundis$elm_bootstrap$Bootstrap_Form_Input$search);
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$tel = _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$input(_rundis$elm_bootstrap$Bootstrap_Form_Input$tel);
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$Addon = function (a) {
	return {ctor: 'Addon', _0: a};
};
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$span = F2(
	function (attributes, children) {
		return _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$Addon(
			A2(
				_elm_lang$html$Html$span,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('input-group-text'),
					_1: attributes
				},
				children));
	});
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$button = F2(
	function (options, children) {
		return _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$Addon(
			A2(_rundis$elm_bootstrap$Bootstrap_Button$button, options, children));
	});
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$dropdown = F2(
	function (state, config) {
		return _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$Addon(
			A2(_rundis$elm_bootstrap$Bootstrap_Dropdown$dropdown, state, config));
	});
var _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$splitDropdown = F2(
	function (state, config) {
		return _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$Addon(
			A2(_rundis$elm_bootstrap$Bootstrap_Dropdown$splitDropdown, state, config));
	});

var _gicentre$elm_vega$VegaLite$windowSortFieldSpec = function (wsf) {
	var _p0 = wsf;
	if (_p0.ctor === 'WAscending') {
		return _elm_lang$core$Json_Encode$object(
			{
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'field',
					_1: _elm_lang$core$Json_Encode$string(_p0._0)
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'order',
						_1: _elm_lang$core$Json_Encode$string('ascending')
					},
					_1: {ctor: '[]'}
				}
			});
	} else {
		return _elm_lang$core$Json_Encode$object(
			{
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'field',
					_1: _elm_lang$core$Json_Encode$string(_p0._0)
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'order',
						_1: _elm_lang$core$Json_Encode$string('descending')
					},
					_1: {ctor: '[]'}
				}
			});
	}
};
var _gicentre$elm_vega$VegaLite$windowPropertySpec = F2(
	function (wpName, wps) {
		var wpSpec = function (wp) {
			var _p1 = wpName;
			switch (_p1) {
				case 'frame':
					var _p2 = wp;
					if (_p2.ctor === 'WFrame') {
						if (_p2._0.ctor === 'Just') {
							if (_p2._1.ctor === 'Just') {
								return _elm_lang$core$Json_Encode$list(
									{
										ctor: '::',
										_0: _elm_lang$core$Json_Encode$int(_p2._0._0),
										_1: {
											ctor: '::',
											_0: _elm_lang$core$Json_Encode$int(_p2._1._0),
											_1: {ctor: '[]'}
										}
									});
							} else {
								return _elm_lang$core$Json_Encode$list(
									{
										ctor: '::',
										_0: _elm_lang$core$Json_Encode$int(_p2._0._0),
										_1: {
											ctor: '::',
											_0: _elm_lang$core$Json_Encode$null,
											_1: {ctor: '[]'}
										}
									});
							}
						} else {
							if (_p2._1.ctor === 'Just') {
								return _elm_lang$core$Json_Encode$list(
									{
										ctor: '::',
										_0: _elm_lang$core$Json_Encode$null,
										_1: {
											ctor: '::',
											_0: _elm_lang$core$Json_Encode$int(_p2._1._0),
											_1: {ctor: '[]'}
										}
									});
							} else {
								return _elm_lang$core$Json_Encode$list(
									{
										ctor: '::',
										_0: _elm_lang$core$Json_Encode$null,
										_1: {
											ctor: '::',
											_0: _elm_lang$core$Json_Encode$null,
											_1: {ctor: '[]'}
										}
									});
							}
						}
					} else {
						return _elm_lang$core$Json_Encode$null;
					}
				case 'ignorePeers':
					var _p3 = wp;
					if (_p3.ctor === 'WIgnorePeers') {
						return _elm_lang$core$Json_Encode$bool(_p3._0);
					} else {
						return _elm_lang$core$Json_Encode$null;
					}
				case 'groupby':
					var _p4 = wp;
					if (_p4.ctor === 'WGroupBy') {
						return _elm_lang$core$Json_Encode$list(
							A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p4._0));
					} else {
						return _elm_lang$core$Json_Encode$null;
					}
				case 'sort':
					var _p5 = wp;
					if (_p5.ctor === 'WSort') {
						return _elm_lang$core$Json_Encode$list(
							A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$windowSortFieldSpec, _p5._0));
					} else {
						return _elm_lang$core$Json_Encode$null;
					}
				default:
					return A2(
						_elm_lang$core$Debug$log,
						A2(
							_elm_lang$core$Basics_ops['++'],
							'Unexpected window property name ',
							_elm_lang$core$Basics$toString(wpName)),
						_elm_lang$core$Json_Encode$null);
			}
		};
		var specList = A2(
			_elm_lang$core$List$filter,
			function (x) {
				return !_elm_lang$core$Native_Utils.eq(x, _elm_lang$core$Json_Encode$null);
			},
			A2(_elm_lang$core$List$map, wpSpec, wps));
		var _p6 = specList;
		if ((_p6.ctor === '::') && (_p6._1.ctor === '[]')) {
			return _p6._0;
		} else {
			return _elm_lang$core$Json_Encode$null;
		}
	});
var _gicentre$elm_vega$VegaLite$wOperationLabel = function (op) {
	var _p7 = op;
	switch (_p7.ctor) {
		case 'RowNumber':
			return 'row_number';
		case 'Rank':
			return 'rank';
		case 'DenseRank':
			return 'dense_rank';
		case 'PercentRank':
			return 'percent_rank';
		case 'CumeDist':
			return 'cume_dist';
		case 'Ntile':
			return 'ntile';
		case 'Lag':
			return 'lag';
		case 'Lead':
			return 'lead';
		case 'FirstValue':
			return 'first_value';
		case 'LastValue':
			return 'last_value';
		default:
			return 'nth_value';
	}
};
var _gicentre$elm_vega$VegaLite$vlPropertyLabel = function (spec) {
	var _p8 = spec;
	switch (_p8.ctor) {
		case 'VLName':
			return 'name';
		case 'VLDescription':
			return 'description';
		case 'VLTitle':
			return 'title';
		case 'VLWidth':
			return 'width';
		case 'VLHeight':
			return 'height';
		case 'VLPadding':
			return 'padding';
		case 'VLAutosize':
			return 'autosize';
		case 'VLBackground':
			return 'background';
		case 'VLData':
			return 'data';
		case 'VLDatasets':
			return 'datasets';
		case 'VLProjection':
			return 'projection';
		case 'VLMark':
			return 'mark';
		case 'VLTransform':
			return 'transform';
		case 'VLEncoding':
			return 'encoding';
		case 'VLConfig':
			return 'config';
		case 'VLSelection':
			return 'selection';
		case 'VLHConcat':
			return 'hconcat';
		case 'VLVConcat':
			return 'vconcat';
		case 'VLLayer':
			return 'layer';
		case 'VLRepeat':
			return 'repeat';
		case 'VLFacet':
			return 'facet';
		case 'VLSpec':
			return 'spec';
		default:
			return 'resolve';
	}
};
var _gicentre$elm_vega$VegaLite$viewConfigProperty = function (viewCfg) {
	var _p9 = viewCfg;
	switch (_p9.ctor) {
		case 'ViewWidth':
			return {
				ctor: '_Tuple2',
				_0: 'width',
				_1: _elm_lang$core$Json_Encode$float(_p9._0)
			};
		case 'ViewHeight':
			return {
				ctor: '_Tuple2',
				_0: 'height',
				_1: _elm_lang$core$Json_Encode$float(_p9._0)
			};
		case 'Clip':
			return {
				ctor: '_Tuple2',
				_0: 'clip',
				_1: _elm_lang$core$Json_Encode$bool(_p9._0)
			};
		case 'Fill':
			var _p10 = _p9._0;
			if (_p10.ctor === 'Just') {
				return {
					ctor: '_Tuple2',
					_0: 'fill',
					_1: _elm_lang$core$Json_Encode$string(_p10._0)
				};
			} else {
				return {
					ctor: '_Tuple2',
					_0: 'fill',
					_1: _elm_lang$core$Json_Encode$string('')
				};
			}
		case 'FillOpacity':
			return {
				ctor: '_Tuple2',
				_0: 'fillOpacity',
				_1: _elm_lang$core$Json_Encode$float(_p9._0)
			};
		case 'Stroke':
			var _p11 = _p9._0;
			if (_p11.ctor === 'Just') {
				return {
					ctor: '_Tuple2',
					_0: 'stroke',
					_1: _elm_lang$core$Json_Encode$string(_p11._0)
				};
			} else {
				return {
					ctor: '_Tuple2',
					_0: 'stroke',
					_1: _elm_lang$core$Json_Encode$string('')
				};
			}
		case 'StrokeOpacity':
			return {
				ctor: '_Tuple2',
				_0: 'strokeOpacity',
				_1: _elm_lang$core$Json_Encode$float(_p9._0)
			};
		case 'StrokeWidth':
			return {
				ctor: '_Tuple2',
				_0: 'strokeWidth',
				_1: _elm_lang$core$Json_Encode$float(_p9._0)
			};
		case 'StrokeDash':
			return {
				ctor: '_Tuple2',
				_0: 'strokeDash',
				_1: _elm_lang$core$Json_Encode$list(
					A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$float, _p9._0))
			};
		default:
			return {
				ctor: '_Tuple2',
				_0: 'strokeDashOffset',
				_1: _elm_lang$core$Json_Encode$float(_p9._0)
			};
	}
};
var _gicentre$elm_vega$VegaLite$vAlignLabel = function (align) {
	var _p12 = align;
	switch (_p12.ctor) {
		case 'AlignTop':
			return 'top';
		case 'AlignMiddle':
			return 'middle';
		default:
			return 'bottom';
	}
};
var _gicentre$elm_vega$VegaLite$timeUnitLabel = function (tu) {
	var _p13 = tu;
	switch (_p13.ctor) {
		case 'Year':
			return 'year';
		case 'YearQuarter':
			return 'yearquarter';
		case 'YearQuarterMonth':
			return 'yearquartermonth';
		case 'YearMonth':
			return 'yearmonth';
		case 'YearMonthDate':
			return 'yearmonthdate';
		case 'YearMonthDateHours':
			return 'yearmonthdatehours';
		case 'YearMonthDateHoursMinutes':
			return 'yearmonthdatehoursminutes';
		case 'YearMonthDateHoursMinutesSeconds':
			return 'yearmonthdatehoursminutesseconds';
		case 'Quarter':
			return 'quarter';
		case 'QuarterMonth':
			return 'quartermonth';
		case 'Month':
			return 'month';
		case 'MonthDate':
			return 'monthdate';
		case 'Date':
			return 'date';
		case 'Day':
			return 'day';
		case 'Hours':
			return 'hours';
		case 'HoursMinutes':
			return 'hoursminutes';
		case 'HoursMinutesSeconds':
			return 'hoursminutesseconds';
		case 'Minutes':
			return 'minutes';
		case 'MinutesSeconds':
			return 'minutesseconds';
		case 'Seconds':
			return 'seconds';
		case 'SecondsMilliseconds':
			return 'secondsmilliseconds';
		case 'Milliseconds':
			return 'milliseconds';
		default:
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'utc',
				_gicentre$elm_vega$VegaLite$timeUnitLabel(_p13._0));
	}
};
var _gicentre$elm_vega$VegaLite$symbolLabel = function (sym) {
	var _p14 = sym;
	switch (_p14.ctor) {
		case 'SymCircle':
			return 'circle';
		case 'SymSquare':
			return 'square';
		case 'Cross':
			return 'cross';
		case 'Diamond':
			return 'diamond';
		case 'TriangleUp':
			return 'triangle-up';
		case 'TriangleDown':
			return 'triangle-down';
		default:
			return _p14._0;
	}
};
var _gicentre$elm_vega$VegaLite$stackProperty = function (sp) {
	var _p15 = sp;
	switch (_p15.ctor) {
		case 'StZero':
			return {
				ctor: '_Tuple2',
				_0: 'stack',
				_1: _elm_lang$core$Json_Encode$string('zero')
			};
		case 'StNormalize':
			return {
				ctor: '_Tuple2',
				_0: 'stack',
				_1: _elm_lang$core$Json_Encode$string('normalize')
			};
		case 'StCenter':
			return {
				ctor: '_Tuple2',
				_0: 'stack',
				_1: _elm_lang$core$Json_Encode$string('center')
			};
		default:
			return {ctor: '_Tuple2', _0: 'stack', _1: _elm_lang$core$Json_Encode$null};
	}
};
var _gicentre$elm_vega$VegaLite$sideLabel = function (side) {
	var _p16 = side;
	switch (_p16.ctor) {
		case 'STop':
			return 'top';
		case 'SBottom':
			return 'bottom';
		case 'SLeft':
			return 'left';
		default:
			return 'right';
	}
};
var _gicentre$elm_vega$VegaLite$selectionResolutionLabel = function (res) {
	var _p17 = res;
	switch (_p17.ctor) {
		case 'Global':
			return 'global';
		case 'Union':
			return 'union';
		default:
			return 'intersect';
	}
};
var _gicentre$elm_vega$VegaLite$selectionMarkProperty = function (markProp) {
	var _p18 = markProp;
	switch (_p18.ctor) {
		case 'SMFill':
			return {
				ctor: '_Tuple2',
				_0: 'fill',
				_1: _elm_lang$core$Json_Encode$string(_p18._0)
			};
		case 'SMFillOpacity':
			return {
				ctor: '_Tuple2',
				_0: 'fillOpacity',
				_1: _elm_lang$core$Json_Encode$float(_p18._0)
			};
		case 'SMStroke':
			return {
				ctor: '_Tuple2',
				_0: 'stroke',
				_1: _elm_lang$core$Json_Encode$string(_p18._0)
			};
		case 'SMStrokeOpacity':
			return {
				ctor: '_Tuple2',
				_0: 'strokeOpacity',
				_1: _elm_lang$core$Json_Encode$float(_p18._0)
			};
		case 'SMStrokeWidth':
			return {
				ctor: '_Tuple2',
				_0: 'strokeWidth',
				_1: _elm_lang$core$Json_Encode$float(_p18._0)
			};
		case 'SMStrokeDash':
			return {
				ctor: '_Tuple2',
				_0: 'strokeDash',
				_1: _elm_lang$core$Json_Encode$list(
					A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$float, _p18._0))
			};
		default:
			return {
				ctor: '_Tuple2',
				_0: 'strokeDashOffset',
				_1: _elm_lang$core$Json_Encode$float(_p18._0)
			};
	}
};
var _gicentre$elm_vega$VegaLite$selectionLabel = function (seType) {
	var _p19 = seType;
	switch (_p19.ctor) {
		case 'Single':
			return 'single';
		case 'Multi':
			return 'multi';
		default:
			return 'interval';
	}
};
var _gicentre$elm_vega$VegaLite$schemeProperty = F2(
	function (name, extent) {
		var _p20 = extent;
		if (((_p20.ctor === '::') && (_p20._1.ctor === '::')) && (_p20._1._1.ctor === '[]')) {
			return {
				ctor: '_Tuple2',
				_0: 'scheme',
				_1: _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'name',
							_1: _elm_lang$core$Json_Encode$string(name)
						},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'extent',
								_1: _elm_lang$core$Json_Encode$list(
									{
										ctor: '::',
										_0: _elm_lang$core$Json_Encode$float(_p20._0),
										_1: {
											ctor: '::',
											_0: _elm_lang$core$Json_Encode$float(_p20._1._0),
											_1: {ctor: '[]'}
										}
									})
							},
							_1: {ctor: '[]'}
						}
					})
			};
		} else {
			return {
				ctor: '_Tuple2',
				_0: 'scheme',
				_1: _elm_lang$core$Json_Encode$string(name)
			};
		}
	});
var _gicentre$elm_vega$VegaLite$scaleNiceSpec = function (ni) {
	var _p21 = ni;
	switch (_p21.ctor) {
		case 'NMillisecond':
			return _elm_lang$core$Json_Encode$string('millisecond');
		case 'NSecond':
			return _elm_lang$core$Json_Encode$string('second');
		case 'NMinute':
			return _elm_lang$core$Json_Encode$string('minute');
		case 'NHour':
			return _elm_lang$core$Json_Encode$string('hour');
		case 'NDay':
			return _elm_lang$core$Json_Encode$string('day');
		case 'NWeek':
			return _elm_lang$core$Json_Encode$string('week');
		case 'NMonth':
			return _elm_lang$core$Json_Encode$string('month');
		case 'NYear':
			return _elm_lang$core$Json_Encode$string('year');
		case 'NInterval':
			return _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'interval',
						_1: _elm_lang$core$Json_Encode$string(
							_gicentre$elm_vega$VegaLite$timeUnitLabel(_p21._0))
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'step',
							_1: _elm_lang$core$Json_Encode$int(_p21._1)
						},
						_1: {ctor: '[]'}
					}
				});
		case 'IsNice':
			return _elm_lang$core$Json_Encode$bool(_p21._0);
		default:
			return _elm_lang$core$Json_Encode$int(_p21._0);
	}
};
var _gicentre$elm_vega$VegaLite$scaleLabel = function (scType) {
	var _p22 = scType;
	switch (_p22.ctor) {
		case 'ScLinear':
			return 'linear';
		case 'ScPow':
			return 'pow';
		case 'ScSqrt':
			return 'sqrt';
		case 'ScLog':
			return 'log';
		case 'ScTime':
			return 'time';
		case 'ScUtc':
			return 'utc';
		case 'ScSequential':
			return 'sequential';
		case 'ScOrdinal':
			return 'ordinal';
		case 'ScBand':
			return 'band';
		case 'ScPoint':
			return 'point';
		case 'ScBinLinear':
			return 'bin-linear';
		default:
			return 'bin-ordinal';
	}
};
var _gicentre$elm_vega$VegaLite$scaleConfigProperty = function (scaleCfg) {
	var _p23 = scaleCfg;
	switch (_p23.ctor) {
		case 'SCBandPaddingInner':
			return {
				ctor: '_Tuple2',
				_0: 'bandPaddingInner',
				_1: _elm_lang$core$Json_Encode$float(_p23._0)
			};
		case 'SCBandPaddingOuter':
			return {
				ctor: '_Tuple2',
				_0: 'bandPaddingOuter',
				_1: _elm_lang$core$Json_Encode$float(_p23._0)
			};
		case 'SCClamp':
			return {
				ctor: '_Tuple2',
				_0: 'clamp',
				_1: _elm_lang$core$Json_Encode$bool(_p23._0)
			};
		case 'SCMaxBandSize':
			return {
				ctor: '_Tuple2',
				_0: 'maxBandSize',
				_1: _elm_lang$core$Json_Encode$float(_p23._0)
			};
		case 'SCMinBandSize':
			return {
				ctor: '_Tuple2',
				_0: 'minBandSize',
				_1: _elm_lang$core$Json_Encode$float(_p23._0)
			};
		case 'SCMaxFontSize':
			return {
				ctor: '_Tuple2',
				_0: 'maxFontSize',
				_1: _elm_lang$core$Json_Encode$float(_p23._0)
			};
		case 'SCMinFontSize':
			return {
				ctor: '_Tuple2',
				_0: 'minFontSize',
				_1: _elm_lang$core$Json_Encode$float(_p23._0)
			};
		case 'SCMaxOpacity':
			return {
				ctor: '_Tuple2',
				_0: 'maxOpacity',
				_1: _elm_lang$core$Json_Encode$float(_p23._0)
			};
		case 'SCMinOpacity':
			return {
				ctor: '_Tuple2',
				_0: 'minOpacity',
				_1: _elm_lang$core$Json_Encode$float(_p23._0)
			};
		case 'SCMaxSize':
			return {
				ctor: '_Tuple2',
				_0: 'maxSize',
				_1: _elm_lang$core$Json_Encode$float(_p23._0)
			};
		case 'SCMinSize':
			return {
				ctor: '_Tuple2',
				_0: 'minSize',
				_1: _elm_lang$core$Json_Encode$float(_p23._0)
			};
		case 'SCMaxStrokeWidth':
			return {
				ctor: '_Tuple2',
				_0: 'maxStrokeWidth',
				_1: _elm_lang$core$Json_Encode$float(_p23._0)
			};
		case 'SCMinStrokeWidth':
			return {
				ctor: '_Tuple2',
				_0: 'minStrokeWidth',
				_1: _elm_lang$core$Json_Encode$float(_p23._0)
			};
		case 'SCPointPadding':
			return {
				ctor: '_Tuple2',
				_0: 'pointPadding',
				_1: _elm_lang$core$Json_Encode$float(_p23._0)
			};
		case 'SCRangeStep':
			var _p24 = _p23._0;
			if (_p24.ctor === 'Just') {
				return {
					ctor: '_Tuple2',
					_0: 'rangeStep',
					_1: _elm_lang$core$Json_Encode$float(_p24._0)
				};
			} else {
				return {ctor: '_Tuple2', _0: 'rangeStep', _1: _elm_lang$core$Json_Encode$null};
			}
		case 'SCRound':
			return {
				ctor: '_Tuple2',
				_0: 'round',
				_1: _elm_lang$core$Json_Encode$bool(_p23._0)
			};
		case 'SCTextXRangeStep':
			return {
				ctor: '_Tuple2',
				_0: 'textXRangeStep',
				_1: _elm_lang$core$Json_Encode$float(_p23._0)
			};
		default:
			return {
				ctor: '_Tuple2',
				_0: 'useUnaggregatedDomain',
				_1: _elm_lang$core$Json_Encode$bool(_p23._0)
			};
	}
};
var _gicentre$elm_vega$VegaLite$resolutionLabel = function (res) {
	var _p25 = res;
	if (_p25.ctor === 'Shared') {
		return 'shared';
	} else {
		return 'independent';
	}
};
var _gicentre$elm_vega$VegaLite$repeatFieldsProperty = function (fields) {
	var _p26 = fields;
	if (_p26.ctor === 'RowFields') {
		return {
			ctor: '_Tuple2',
			_0: 'row',
			_1: _elm_lang$core$Json_Encode$list(
				A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p26._0))
		};
	} else {
		return {
			ctor: '_Tuple2',
			_0: 'column',
			_1: _elm_lang$core$Json_Encode$list(
				A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p26._0))
		};
	}
};
var _gicentre$elm_vega$VegaLite$rangeConfigProperty = function (rangeCfg) {
	var _p27 = rangeCfg;
	switch (_p27.ctor) {
		case 'RCategory':
			return {
				ctor: '_Tuple2',
				_0: 'category',
				_1: _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: A2(
							_gicentre$elm_vega$VegaLite$schemeProperty,
							_p27._0,
							{ctor: '[]'}),
						_1: {ctor: '[]'}
					})
			};
		case 'RDiverging':
			return {
				ctor: '_Tuple2',
				_0: 'diverging',
				_1: _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: A2(
							_gicentre$elm_vega$VegaLite$schemeProperty,
							_p27._0,
							{ctor: '[]'}),
						_1: {ctor: '[]'}
					})
			};
		case 'RHeatmap':
			return {
				ctor: '_Tuple2',
				_0: 'heatmap',
				_1: _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: A2(
							_gicentre$elm_vega$VegaLite$schemeProperty,
							_p27._0,
							{ctor: '[]'}),
						_1: {ctor: '[]'}
					})
			};
		case 'ROrdinal':
			return {
				ctor: '_Tuple2',
				_0: 'ordinal',
				_1: _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: A2(
							_gicentre$elm_vega$VegaLite$schemeProperty,
							_p27._0,
							{ctor: '[]'}),
						_1: {ctor: '[]'}
					})
			};
		case 'RRamp':
			return {
				ctor: '_Tuple2',
				_0: 'ramp',
				_1: _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: A2(
							_gicentre$elm_vega$VegaLite$schemeProperty,
							_p27._0,
							{ctor: '[]'}),
						_1: {ctor: '[]'}
					})
			};
		default:
			return {
				ctor: '_Tuple2',
				_0: 'symbol',
				_1: _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: A2(
							_gicentre$elm_vega$VegaLite$schemeProperty,
							_p27._0,
							{ctor: '[]'}),
						_1: {ctor: '[]'}
					})
			};
	}
};
var _gicentre$elm_vega$VegaLite$positionLabel = function (pChannel) {
	var _p28 = pChannel;
	switch (_p28.ctor) {
		case 'X':
			return 'x';
		case 'Y':
			return 'y';
		case 'X2':
			return 'x2';
		case 'Y2':
			return 'y2';
		case 'Longitude':
			return 'longitude';
		case 'Latitude':
			return 'latitude';
		case 'Longitude2':
			return 'longitude2';
		default:
			return 'latitude2';
	}
};
var _gicentre$elm_vega$VegaLite$projectionLabel = function (proj) {
	var _p29 = proj;
	switch (_p29.ctor) {
		case 'Albers':
			return 'albers';
		case 'AlbersUsa':
			return 'albersUsa';
		case 'AzimuthalEqualArea':
			return 'azimuthalEqualArea';
		case 'AzimuthalEquidistant':
			return 'azimuthalEquidistant';
		case 'ConicConformal':
			return 'conicConformal';
		case 'ConicEqualArea':
			return 'conicEqualarea';
		case 'ConicEquidistant':
			return 'conicEquidistant';
		case 'Custom':
			return _p29._0;
		case 'Equirectangular':
			return 'equirectangular';
		case 'Gnomonic':
			return 'gnomonic';
		case 'Mercator':
			return 'mercator';
		case 'Orthographic':
			return 'orthographic';
		case 'Stereographic':
			return 'stereographic';
		default:
			return 'transverseMercator';
	}
};
var _gicentre$elm_vega$VegaLite$projectionProperty = function (pp) {
	var _p30 = pp;
	switch (_p30.ctor) {
		case 'PType':
			return {
				ctor: '_Tuple2',
				_0: 'type',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$projectionLabel(_p30._0))
			};
		case 'PClipAngle':
			var _p31 = _p30._0;
			if (_p31.ctor === 'Just') {
				return {
					ctor: '_Tuple2',
					_0: 'clipAngle',
					_1: _elm_lang$core$Json_Encode$float(_p31._0)
				};
			} else {
				return {ctor: '_Tuple2', _0: 'clipAngle', _1: _elm_lang$core$Json_Encode$null};
			}
		case 'PClipExtent':
			var _p32 = _p30._0;
			if (_p32.ctor === 'NoClip') {
				return {ctor: '_Tuple2', _0: 'clipExtent', _1: _elm_lang$core$Json_Encode$null};
			} else {
				return {
					ctor: '_Tuple2',
					_0: 'clipExtent',
					_1: _elm_lang$core$Json_Encode$list(
						A2(
							_elm_lang$core$List$map,
							_elm_lang$core$Json_Encode$float,
							{
								ctor: '::',
								_0: _p32._0,
								_1: {
									ctor: '::',
									_0: _p32._1,
									_1: {
										ctor: '::',
										_0: _p32._2,
										_1: {
											ctor: '::',
											_0: _p32._3,
											_1: {ctor: '[]'}
										}
									}
								}
							}))
				};
			}
		case 'PCenter':
			return {
				ctor: '_Tuple2',
				_0: 'center',
				_1: _elm_lang$core$Json_Encode$list(
					{
						ctor: '::',
						_0: _elm_lang$core$Json_Encode$float(_p30._0),
						_1: {
							ctor: '::',
							_0: _elm_lang$core$Json_Encode$float(_p30._1),
							_1: {ctor: '[]'}
						}
					})
			};
		case 'PRotate':
			return {
				ctor: '_Tuple2',
				_0: 'rotate',
				_1: _elm_lang$core$Json_Encode$list(
					A2(
						_elm_lang$core$List$map,
						_elm_lang$core$Json_Encode$float,
						{
							ctor: '::',
							_0: _p30._0,
							_1: {
								ctor: '::',
								_0: _p30._1,
								_1: {
									ctor: '::',
									_0: _p30._2,
									_1: {ctor: '[]'}
								}
							}
						}))
			};
		case 'PPrecision':
			return {
				ctor: '_Tuple2',
				_0: 'precision',
				_1: _elm_lang$core$Json_Encode$float(_p30._0)
			};
		case 'PCoefficient':
			return {
				ctor: '_Tuple2',
				_0: 'coefficient',
				_1: _elm_lang$core$Json_Encode$float(_p30._0)
			};
		case 'PDistance':
			return {
				ctor: '_Tuple2',
				_0: 'distance',
				_1: _elm_lang$core$Json_Encode$float(_p30._0)
			};
		case 'PFraction':
			return {
				ctor: '_Tuple2',
				_0: 'fraction',
				_1: _elm_lang$core$Json_Encode$float(_p30._0)
			};
		case 'PLobes':
			return {
				ctor: '_Tuple2',
				_0: 'lobes',
				_1: _elm_lang$core$Json_Encode$int(_p30._0)
			};
		case 'PParallel':
			return {
				ctor: '_Tuple2',
				_0: 'parallel',
				_1: _elm_lang$core$Json_Encode$float(_p30._0)
			};
		case 'PRadius':
			return {
				ctor: '_Tuple2',
				_0: 'radius',
				_1: _elm_lang$core$Json_Encode$float(_p30._0)
			};
		case 'PRatio':
			return {
				ctor: '_Tuple2',
				_0: 'ratio',
				_1: _elm_lang$core$Json_Encode$float(_p30._0)
			};
		case 'PSpacing':
			return {
				ctor: '_Tuple2',
				_0: 'spacing',
				_1: _elm_lang$core$Json_Encode$float(_p30._0)
			};
		default:
			return {
				ctor: '_Tuple2',
				_0: 'tilt',
				_1: _elm_lang$core$Json_Encode$float(_p30._0)
			};
	}
};
var _gicentre$elm_vega$VegaLite$paddingSpec = function (pad) {
	var _p33 = pad;
	if (_p33.ctor === 'PSize') {
		return _elm_lang$core$Json_Encode$float(_p33._0);
	} else {
		return _elm_lang$core$Json_Encode$object(
			{
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'left',
					_1: _elm_lang$core$Json_Encode$float(_p33._0)
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'top',
						_1: _elm_lang$core$Json_Encode$float(_p33._1)
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'right',
							_1: _elm_lang$core$Json_Encode$float(_p33._2)
						},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'bottom',
								_1: _elm_lang$core$Json_Encode$float(_p33._3)
							},
							_1: {ctor: '[]'}
						}
					}
				}
			});
	}
};
var _gicentre$elm_vega$VegaLite$overlapStrategyLabel = function (strat) {
	var _p34 = strat;
	switch (_p34.ctor) {
		case 'ONone':
			return 'false';
		case 'OParity':
			return 'parity';
		default:
			return 'greedy';
	}
};
var _gicentre$elm_vega$VegaLite$operationLabel = function (op) {
	var _p35 = op;
	switch (_p35.ctor) {
		case 'ArgMax':
			return 'argmax';
		case 'ArgMin':
			return 'argmin';
		case 'Average':
			return 'average';
		case 'Count':
			return 'count';
		case 'CI0':
			return 'ci0';
		case 'CI1':
			return 'ci1';
		case 'Distinct':
			return 'distinct';
		case 'Max':
			return 'max';
		case 'Mean':
			return 'mean';
		case 'Median':
			return 'median';
		case 'Min':
			return 'min';
		case 'Missing':
			return 'missing';
		case 'Q1':
			return 'q1';
		case 'Q3':
			return 'q3';
		case 'Stdev':
			return 'stdev';
		case 'StdevP':
			return 'stdevp';
		case 'Sum':
			return 'sum';
		case 'Stderr':
			return 'stderr';
		case 'Valid':
			return 'valid';
		case 'Variance':
			return 'variance';
		default:
			return 'variancep';
	}
};
var _gicentre$elm_vega$VegaLite$windowAsProperty = function (w) {
	var _p36 = w;
	switch (_p36.ctor) {
		case 'WAggregateOp':
			return {
				ctor: '_Tuple2',
				_0: 'op',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$operationLabel(_p36._0))
			};
		case 'WOp':
			return {
				ctor: '_Tuple2',
				_0: 'op',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$wOperationLabel(_p36._0))
			};
		case 'WParam':
			return {
				ctor: '_Tuple2',
				_0: 'param',
				_1: _elm_lang$core$Json_Encode$int(_p36._0)
			};
		default:
			return {
				ctor: '_Tuple2',
				_0: 'field',
				_1: _elm_lang$core$Json_Encode$string(_p36._0)
			};
	}
};
var _gicentre$elm_vega$VegaLite$monthNameLabel = function (mon) {
	var _p37 = mon;
	switch (_p37.ctor) {
		case 'Jan':
			return 'Jan';
		case 'Feb':
			return 'Feb';
		case 'Mar':
			return 'Mar';
		case 'Apr':
			return 'Apr';
		case 'May':
			return 'May';
		case 'Jun':
			return 'Jun';
		case 'Jul':
			return 'Jul';
		case 'Aug':
			return 'Aug';
		case 'Sep':
			return 'Sep';
		case 'Oct':
			return 'Oct';
		case 'Nov':
			return 'Nov';
		default:
			return 'Dec';
	}
};
var _gicentre$elm_vega$VegaLite$measurementLabel = function (mType) {
	var _p38 = mType;
	switch (_p38.ctor) {
		case 'Nominal':
			return 'nominal';
		case 'Ordinal':
			return 'ordinal';
		case 'Quantitative':
			return 'quantitative';
		case 'Temporal':
			return 'temporal';
		default:
			return 'geojson';
	}
};
var _gicentre$elm_vega$VegaLite$markOrientationLabel = function (orient) {
	var _p39 = orient;
	if (_p39.ctor === 'Horizontal') {
		return 'horizontal';
	} else {
		return 'vertical';
	}
};
var _gicentre$elm_vega$VegaLite$markLabel = function (mark) {
	var _p40 = mark;
	switch (_p40.ctor) {
		case 'Area':
			return 'area';
		case 'Bar':
			return 'bar';
		case 'Circle':
			return 'circle';
		case 'Line':
			return 'line';
		case 'Geoshape':
			return 'geoshape';
		case 'Point':
			return 'point';
		case 'Rect':
			return 'rect';
		case 'Rule':
			return 'rule';
		case 'Square':
			return 'square';
		case 'Text':
			return 'text';
		case 'Tick':
			return 'tick';
		default:
			return 'trail';
	}
};
var _gicentre$elm_vega$VegaLite$markInterpolationLabel = function (interp) {
	var _p41 = interp;
	switch (_p41.ctor) {
		case 'Linear':
			return 'linear';
		case 'LinearClosed':
			return 'linear-closed';
		case 'Stepwise':
			return 'step';
		case 'StepBefore':
			return 'step-before';
		case 'StepAfter':
			return 'step-after';
		case 'Basis':
			return 'basis';
		case 'BasisOpen':
			return 'basis-open';
		case 'BasisClosed':
			return 'basis-closed';
		case 'Cardinal':
			return 'cardinal';
		case 'CardinalOpen':
			return 'cardinal-open';
		case 'CardinalClosed':
			return 'cardinal-closed';
		case 'Bundle':
			return 'bundle';
		default:
			return 'monotone';
	}
};
var _gicentre$elm_vega$VegaLite$legendOrientLabel = function (orient) {
	var _p42 = orient;
	switch (_p42.ctor) {
		case 'Left':
			return 'left';
		case 'BottomLeft':
			return 'bottom-left';
		case 'BottomRight':
			return 'bottom-right';
		case 'Right':
			return 'right';
		case 'TopLeft':
			return 'top-left';
		case 'TopRight':
			return 'top-right';
		default:
			return 'none';
	}
};
var _gicentre$elm_vega$VegaLite$inputProperty = function (prop) {
	var _p43 = prop;
	switch (_p43.ctor) {
		case 'InMin':
			return {
				ctor: '_Tuple2',
				_0: 'min',
				_1: _elm_lang$core$Json_Encode$float(_p43._0)
			};
		case 'InMax':
			return {
				ctor: '_Tuple2',
				_0: 'max',
				_1: _elm_lang$core$Json_Encode$float(_p43._0)
			};
		case 'InStep':
			return {
				ctor: '_Tuple2',
				_0: 'step',
				_1: _elm_lang$core$Json_Encode$float(_p43._0)
			};
		case 'Debounce':
			return {
				ctor: '_Tuple2',
				_0: 'debounce',
				_1: _elm_lang$core$Json_Encode$float(_p43._0)
			};
		case 'InName':
			return {
				ctor: '_Tuple2',
				_0: 'name',
				_1: _elm_lang$core$Json_Encode$string(_p43._0)
			};
		case 'InOptions':
			return {
				ctor: '_Tuple2',
				_0: 'options',
				_1: _elm_lang$core$Json_Encode$list(
					A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p43._0))
			};
		case 'InPlaceholder':
			return {
				ctor: '_Tuple2',
				_0: 'placeholder',
				_1: _elm_lang$core$Json_Encode$string(_p43._0)
			};
		default:
			return {
				ctor: '_Tuple2',
				_0: 'element',
				_1: _elm_lang$core$Json_Encode$string(_p43._0)
			};
	}
};
var _gicentre$elm_vega$VegaLite$headerProperty = function (hProp) {
	var _p44 = hProp;
	if (_p44.ctor === 'HFormat') {
		return {
			ctor: '_Tuple2',
			_0: 'format',
			_1: _elm_lang$core$Json_Encode$string(_p44._0)
		};
	} else {
		return {
			ctor: '_Tuple2',
			_0: 'title',
			_1: _elm_lang$core$Json_Encode$string(_p44._0)
		};
	}
};
var _gicentre$elm_vega$VegaLite$hAlignLabel = function (align) {
	var _p45 = align;
	switch (_p45.ctor) {
		case 'AlignLeft':
			return 'left';
		case 'AlignCenter':
			return 'center';
		default:
			return 'right';
	}
};
var _gicentre$elm_vega$VegaLite$geometryTypeSpec = function (gType) {
	var toCoords = function (pairs) {
		return _elm_lang$core$Json_Encode$list(
			A2(
				_elm_lang$core$List$map,
				function (_p46) {
					var _p47 = _p46;
					return _elm_lang$core$Json_Encode$list(
						{
							ctor: '::',
							_0: _elm_lang$core$Json_Encode$float(_p47._0),
							_1: {
								ctor: '::',
								_0: _elm_lang$core$Json_Encode$float(_p47._1),
								_1: {ctor: '[]'}
							}
						});
				},
				pairs));
	};
	var _p48 = gType;
	switch (_p48.ctor) {
		case 'GeoPoint':
			return _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'type',
						_1: _elm_lang$core$Json_Encode$string('Point')
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'coordinates',
							_1: _elm_lang$core$Json_Encode$list(
								{
									ctor: '::',
									_0: _elm_lang$core$Json_Encode$float(_p48._0),
									_1: {
										ctor: '::',
										_0: _elm_lang$core$Json_Encode$float(_p48._1),
										_1: {ctor: '[]'}
									}
								})
						},
						_1: {ctor: '[]'}
					}
				});
		case 'GeoPoints':
			return _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'type',
						_1: _elm_lang$core$Json_Encode$string('MultiPoint')
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'coordinates',
							_1: toCoords(_p48._0)
						},
						_1: {ctor: '[]'}
					}
				});
		case 'GeoLine':
			return _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'type',
						_1: _elm_lang$core$Json_Encode$string('LineString')
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'coordinates',
							_1: toCoords(_p48._0)
						},
						_1: {ctor: '[]'}
					}
				});
		case 'GeoLines':
			return _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'type',
						_1: _elm_lang$core$Json_Encode$string('MultiLineString')
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'coordinates',
							_1: _elm_lang$core$Json_Encode$list(
								A2(_elm_lang$core$List$map, toCoords, _p48._0))
						},
						_1: {ctor: '[]'}
					}
				});
		case 'GeoPolygon':
			return _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'type',
						_1: _elm_lang$core$Json_Encode$string('Polygon')
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'coordinates',
							_1: _elm_lang$core$Json_Encode$list(
								A2(_elm_lang$core$List$map, toCoords, _p48._0))
						},
						_1: {ctor: '[]'}
					}
				});
		default:
			return _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'type',
						_1: _elm_lang$core$Json_Encode$string('MultiPolygon')
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'coordinates',
							_1: _elm_lang$core$Json_Encode$list(
								A2(
									_elm_lang$core$List$map,
									function (cs) {
										return _elm_lang$core$Json_Encode$list(
											A2(_elm_lang$core$List$map, toCoords, cs));
									},
									_p48._0))
						},
						_1: {ctor: '[]'}
					}
				});
	}
};
var _gicentre$elm_vega$VegaLite$fontWeightSpec = function (w) {
	var _p49 = w;
	switch (_p49.ctor) {
		case 'Normal':
			return _elm_lang$core$Json_Encode$string('normal');
		case 'Bold':
			return _elm_lang$core$Json_Encode$string('bold');
		case 'Bolder':
			return _elm_lang$core$Json_Encode$string('bolder');
		case 'Lighter':
			return _elm_lang$core$Json_Encode$string('lighter');
		case 'W100':
			return _elm_lang$core$Json_Encode$float(100);
		case 'W200':
			return _elm_lang$core$Json_Encode$float(200);
		case 'W300':
			return _elm_lang$core$Json_Encode$float(300);
		case 'W400':
			return _elm_lang$core$Json_Encode$float(400);
		case 'W500':
			return _elm_lang$core$Json_Encode$float(500);
		case 'W600':
			return _elm_lang$core$Json_Encode$float(600);
		case 'W700':
			return _elm_lang$core$Json_Encode$float(700);
		case 'W800':
			return _elm_lang$core$Json_Encode$float(800);
		default:
			return _elm_lang$core$Json_Encode$float(900);
	}
};
var _gicentre$elm_vega$VegaLite$legendConfigProperty = function (legendConfig) {
	var _p50 = legendConfig;
	switch (_p50.ctor) {
		case 'CornerRadius':
			return {
				ctor: '_Tuple2',
				_0: 'cornerRadius',
				_1: _elm_lang$core$Json_Encode$float(_p50._0)
			};
		case 'FillColor':
			return {
				ctor: '_Tuple2',
				_0: 'fillColor',
				_1: _elm_lang$core$Json_Encode$string(_p50._0)
			};
		case 'Orient':
			return {
				ctor: '_Tuple2',
				_0: 'orient',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$legendOrientLabel(_p50._0))
			};
		case 'Offset':
			return {
				ctor: '_Tuple2',
				_0: 'offset',
				_1: _elm_lang$core$Json_Encode$float(_p50._0)
			};
		case 'StrokeColor':
			return {
				ctor: '_Tuple2',
				_0: 'strokeColor',
				_1: _elm_lang$core$Json_Encode$string(_p50._0)
			};
		case 'LeStrokeDash':
			return {
				ctor: '_Tuple2',
				_0: 'strokeDash',
				_1: _elm_lang$core$Json_Encode$list(
					A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$float, _p50._0))
			};
		case 'LeStrokeWidth':
			return {
				ctor: '_Tuple2',
				_0: 'strokeWidth',
				_1: _elm_lang$core$Json_Encode$float(_p50._0)
			};
		case 'LePadding':
			return {
				ctor: '_Tuple2',
				_0: 'padding',
				_1: _elm_lang$core$Json_Encode$float(_p50._0)
			};
		case 'GradientLabelBaseline':
			return {
				ctor: '_Tuple2',
				_0: 'gradientLabelBaseline',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$vAlignLabel(_p50._0))
			};
		case 'GradientLabelLimit':
			return {
				ctor: '_Tuple2',
				_0: 'gradientLabelLimit',
				_1: _elm_lang$core$Json_Encode$float(_p50._0)
			};
		case 'GradientLabelOffset':
			return {
				ctor: '_Tuple2',
				_0: 'gradientLabelOffset',
				_1: _elm_lang$core$Json_Encode$float(_p50._0)
			};
		case 'GradientStrokeColor':
			return {
				ctor: '_Tuple2',
				_0: 'gradientStrokeColor',
				_1: _elm_lang$core$Json_Encode$string(_p50._0)
			};
		case 'GradientStrokeWidth':
			return {
				ctor: '_Tuple2',
				_0: 'gradientStrokeWidth',
				_1: _elm_lang$core$Json_Encode$float(_p50._0)
			};
		case 'GradientHeight':
			return {
				ctor: '_Tuple2',
				_0: 'gradientHeight',
				_1: _elm_lang$core$Json_Encode$float(_p50._0)
			};
		case 'GradientWidth':
			return {
				ctor: '_Tuple2',
				_0: 'gradientWidth',
				_1: _elm_lang$core$Json_Encode$float(_p50._0)
			};
		case 'LeLabelAlign':
			return {
				ctor: '_Tuple2',
				_0: 'labelAlign',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$hAlignLabel(_p50._0))
			};
		case 'LeLabelBaseline':
			return {
				ctor: '_Tuple2',
				_0: 'labelBaseline',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$vAlignLabel(_p50._0))
			};
		case 'LeLabelColor':
			return {
				ctor: '_Tuple2',
				_0: 'labelColor',
				_1: _elm_lang$core$Json_Encode$string(_p50._0)
			};
		case 'LeLabelFont':
			return {
				ctor: '_Tuple2',
				_0: 'labelFont',
				_1: _elm_lang$core$Json_Encode$string(_p50._0)
			};
		case 'LeLabelFontSize':
			return {
				ctor: '_Tuple2',
				_0: 'labelFontSize',
				_1: _elm_lang$core$Json_Encode$float(_p50._0)
			};
		case 'LeLabelLimit':
			return {
				ctor: '_Tuple2',
				_0: 'labelLimit',
				_1: _elm_lang$core$Json_Encode$float(_p50._0)
			};
		case 'LeLabelOffset':
			return {
				ctor: '_Tuple2',
				_0: 'labelOffset',
				_1: _elm_lang$core$Json_Encode$float(_p50._0)
			};
		case 'LeShortTimeLabels':
			return {
				ctor: '_Tuple2',
				_0: 'shortTimeLabels',
				_1: _elm_lang$core$Json_Encode$bool(_p50._0)
			};
		case 'EntryPadding':
			return {
				ctor: '_Tuple2',
				_0: 'entryPadding',
				_1: _elm_lang$core$Json_Encode$float(_p50._0)
			};
		case 'SymbolColor':
			return {
				ctor: '_Tuple2',
				_0: 'symbolColor',
				_1: _elm_lang$core$Json_Encode$string(_p50._0)
			};
		case 'SymbolType':
			return {
				ctor: '_Tuple2',
				_0: 'symbolType',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$symbolLabel(_p50._0))
			};
		case 'SymbolSize':
			return {
				ctor: '_Tuple2',
				_0: 'symbolSize',
				_1: _elm_lang$core$Json_Encode$float(_p50._0)
			};
		case 'SymbolStrokeWidth':
			return {
				ctor: '_Tuple2',
				_0: 'symbolStrokeWidth',
				_1: _elm_lang$core$Json_Encode$float(_p50._0)
			};
		case 'LeTitleAlign':
			return {
				ctor: '_Tuple2',
				_0: 'titleAlign',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$hAlignLabel(_p50._0))
			};
		case 'LeTitleBaseline':
			return {
				ctor: '_Tuple2',
				_0: 'titleBaseline',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$vAlignLabel(_p50._0))
			};
		case 'LeTitleColor':
			return {
				ctor: '_Tuple2',
				_0: 'titleColor',
				_1: _elm_lang$core$Json_Encode$string(_p50._0)
			};
		case 'LeTitleFont':
			return {
				ctor: '_Tuple2',
				_0: 'titleFont',
				_1: _elm_lang$core$Json_Encode$string(_p50._0)
			};
		case 'LeTitleFontSize':
			return {
				ctor: '_Tuple2',
				_0: 'titleFontSize',
				_1: _elm_lang$core$Json_Encode$float(_p50._0)
			};
		case 'LeTitleFontWeight':
			return {
				ctor: '_Tuple2',
				_0: 'titleFontWeight',
				_1: _gicentre$elm_vega$VegaLite$fontWeightSpec(_p50._0)
			};
		case 'LeTitleLimit':
			return {
				ctor: '_Tuple2',
				_0: 'titleLimit',
				_1: _elm_lang$core$Json_Encode$float(_p50._0)
			};
		default:
			return {
				ctor: '_Tuple2',
				_0: 'titlePadding',
				_1: _elm_lang$core$Json_Encode$float(_p50._0)
			};
	}
};
var _gicentre$elm_vega$VegaLite$fieldTitleLabel = function (ftp) {
	var _p51 = ftp;
	switch (_p51.ctor) {
		case 'Verbal':
			return 'verbal';
		case 'Function':
			return 'function';
		default:
			return 'plain';
	}
};
var _gicentre$elm_vega$VegaLite$dayLabel = function (day) {
	var _p52 = day;
	switch (_p52.ctor) {
		case 'Mon':
			return 'Mon';
		case 'Tue':
			return 'Tue';
		case 'Wed':
			return 'Wed';
		case 'Thu':
			return 'Thu';
		case 'Fri':
			return 'Fri';
		case 'Sat':
			return 'Sat';
		default:
			return 'Sun';
	}
};
var _gicentre$elm_vega$VegaLite$dateTimeProperty = function (dt) {
	var _p53 = dt;
	switch (_p53.ctor) {
		case 'DTYear':
			return {
				ctor: '_Tuple2',
				_0: 'year',
				_1: _elm_lang$core$Json_Encode$int(_p53._0)
			};
		case 'DTQuarter':
			return {
				ctor: '_Tuple2',
				_0: 'quarter',
				_1: _elm_lang$core$Json_Encode$int(_p53._0)
			};
		case 'DTMonth':
			return {
				ctor: '_Tuple2',
				_0: 'month',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$monthNameLabel(_p53._0))
			};
		case 'DTDate':
			return {
				ctor: '_Tuple2',
				_0: 'date',
				_1: _elm_lang$core$Json_Encode$int(_p53._0)
			};
		case 'DTDay':
			return {
				ctor: '_Tuple2',
				_0: 'day',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$dayLabel(_p53._0))
			};
		case 'DTHours':
			return {
				ctor: '_Tuple2',
				_0: 'hours',
				_1: _elm_lang$core$Json_Encode$int(_p53._0)
			};
		case 'DTMinutes':
			return {
				ctor: '_Tuple2',
				_0: 'minutes',
				_1: _elm_lang$core$Json_Encode$int(_p53._0)
			};
		case 'DTSeconds':
			return {
				ctor: '_Tuple2',
				_0: 'seconds',
				_1: _elm_lang$core$Json_Encode$int(_p53._0)
			};
		default:
			return {
				ctor: '_Tuple2',
				_0: 'milliseconds',
				_1: _elm_lang$core$Json_Encode$int(_p53._0)
			};
	}
};
var _gicentre$elm_vega$VegaLite$legendProperty = function (legendProp) {
	var _p54 = legendProp;
	switch (_p54.ctor) {
		case 'LType':
			var _p55 = _p54._0;
			if (_p55.ctor === 'Gradient') {
				return {
					ctor: '_Tuple2',
					_0: 'type',
					_1: _elm_lang$core$Json_Encode$string('gradient')
				};
			} else {
				return {
					ctor: '_Tuple2',
					_0: 'type',
					_1: _elm_lang$core$Json_Encode$string('symbol')
				};
			}
		case 'LEntryPadding':
			return {
				ctor: '_Tuple2',
				_0: 'entryPadding',
				_1: _elm_lang$core$Json_Encode$float(_p54._0)
			};
		case 'LFormat':
			return {
				ctor: '_Tuple2',
				_0: 'format',
				_1: _elm_lang$core$Json_Encode$string(_p54._0)
			};
		case 'LOffset':
			return {
				ctor: '_Tuple2',
				_0: 'offset',
				_1: _elm_lang$core$Json_Encode$float(_p54._0)
			};
		case 'LOrient':
			return {
				ctor: '_Tuple2',
				_0: 'orient',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$legendOrientLabel(_p54._0))
			};
		case 'LPadding':
			return {
				ctor: '_Tuple2',
				_0: 'padding',
				_1: _elm_lang$core$Json_Encode$float(_p54._0)
			};
		case 'LTickCount':
			return {
				ctor: '_Tuple2',
				_0: 'tickCount',
				_1: _elm_lang$core$Json_Encode$float(_p54._0)
			};
		case 'LTitle':
			var _p56 = _p54._0;
			return _elm_lang$core$Native_Utils.eq(_p56, '') ? {ctor: '_Tuple2', _0: 'title', _1: _elm_lang$core$Json_Encode$null} : {
				ctor: '_Tuple2',
				_0: 'title',
				_1: _elm_lang$core$Json_Encode$string(_p56)
			};
		case 'LValues':
			var list = function () {
				var _p57 = _p54._0;
				switch (_p57.ctor) {
					case 'LNumbers':
						return _elm_lang$core$Json_Encode$list(
							A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$float, _p57._0));
					case 'LDateTimes':
						return _elm_lang$core$Json_Encode$list(
							A2(
								_elm_lang$core$List$map,
								function (dt) {
									return _elm_lang$core$Json_Encode$object(
										A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$dateTimeProperty, dt));
								},
								_p57._0));
					default:
						return _elm_lang$core$Json_Encode$list(
							A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p57._0));
				}
			}();
			return {ctor: '_Tuple2', _0: 'values', _1: list};
		default:
			return {
				ctor: '_Tuple2',
				_0: 'zindex',
				_1: _elm_lang$core$Json_Encode$int(_p54._0)
			};
	}
};
var _gicentre$elm_vega$VegaLite$scaleDomainSpec = function (sdType) {
	var _p58 = sdType;
	switch (_p58.ctor) {
		case 'DNumbers':
			return _elm_lang$core$Json_Encode$list(
				A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$float, _p58._0));
		case 'DDateTimes':
			return _elm_lang$core$Json_Encode$list(
				A2(
					_elm_lang$core$List$map,
					function (dt) {
						return _elm_lang$core$Json_Encode$object(
							A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$dateTimeProperty, dt));
					},
					_p58._0));
		case 'DStrings':
			return _elm_lang$core$Json_Encode$list(
				A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p58._0));
		case 'DSelection':
			return _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'selection',
						_1: _elm_lang$core$Json_Encode$string(_p58._0)
					},
					_1: {ctor: '[]'}
				});
		default:
			return _elm_lang$core$Json_Encode$string('unaggregated');
	}
};
var _gicentre$elm_vega$VegaLite$dataValuesSpecs = function (dvs) {
	var _p59 = dvs;
	switch (_p59.ctor) {
		case 'Numbers':
			return A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$float, _p59._0);
		case 'Strings':
			return A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p59._0);
		case 'DateTimes':
			return A2(
				_elm_lang$core$List$map,
				function (dts) {
					return _elm_lang$core$Json_Encode$object(
						A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$dateTimeProperty, dts));
				},
				_p59._0);
		default:
			return A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$bool, _p59._0);
	}
};
var _gicentre$elm_vega$VegaLite$dataValueSpec = function (val) {
	var _p60 = val;
	switch (_p60.ctor) {
		case 'Number':
			return _elm_lang$core$Json_Encode$float(_p60._0);
		case 'Str':
			return _elm_lang$core$Json_Encode$string(_p60._0);
		case 'Boolean':
			return _elm_lang$core$Json_Encode$bool(_p60._0);
		default:
			return _elm_lang$core$Json_Encode$object(
				A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$dateTimeProperty, _p60._0));
	}
};
var _gicentre$elm_vega$VegaLite$dataTypeSpec = function (dType) {
	var _p61 = dType;
	switch (_p61.ctor) {
		case 'FoNumber':
			return _elm_lang$core$Json_Encode$string('number');
		case 'FoBoolean':
			return _elm_lang$core$Json_Encode$string('boolean');
		case 'FoDate':
			var _p62 = _p61._0;
			return _elm_lang$core$Native_Utils.eq(_p62, '') ? _elm_lang$core$Json_Encode$string('date') : _elm_lang$core$Json_Encode$string(
				A2(
					_elm_lang$core$Basics_ops['++'],
					'date:\'',
					A2(_elm_lang$core$Basics_ops['++'], _p62, '\'')));
		default:
			var _p63 = _p61._0;
			return _elm_lang$core$Native_Utils.eq(_p63, '') ? _elm_lang$core$Json_Encode$string('utc') : _elm_lang$core$Json_Encode$string(
				A2(
					_elm_lang$core$Basics_ops['++'],
					'utc:\'',
					A2(_elm_lang$core$Basics_ops['++'], _p63, '\'')));
	}
};
var _gicentre$elm_vega$VegaLite$formatProperty = function (fmt) {
	var _p64 = fmt;
	switch (_p64.ctor) {
		case 'JSON':
			var _p65 = _p64._0;
			return _elm_lang$core$Native_Utils.eq(
				_elm_lang$core$String$trim(_p65),
				'') ? {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'type',
					_1: _elm_lang$core$Json_Encode$string('json')
				},
				_1: {ctor: '[]'}
			} : {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'type',
					_1: _elm_lang$core$Json_Encode$string('json')
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'property',
						_1: _elm_lang$core$Json_Encode$string(_p65)
					},
					_1: {ctor: '[]'}
				}
			};
		case 'CSV':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'type',
					_1: _elm_lang$core$Json_Encode$string('csv')
				},
				_1: {ctor: '[]'}
			};
		case 'TSV':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'type',
					_1: _elm_lang$core$Json_Encode$string('tsv')
				},
				_1: {ctor: '[]'}
			};
		case 'TopojsonFeature':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'type',
					_1: _elm_lang$core$Json_Encode$string('topojson')
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'feature',
						_1: _elm_lang$core$Json_Encode$string(_p64._0)
					},
					_1: {ctor: '[]'}
				}
			};
		case 'TopojsonMesh':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'type',
					_1: _elm_lang$core$Json_Encode$string('topojson')
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'mesh',
						_1: _elm_lang$core$Json_Encode$string(_p64._0)
					},
					_1: {ctor: '[]'}
				}
			};
		default:
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'parse',
					_1: _elm_lang$core$Json_Encode$object(
						A2(
							_elm_lang$core$List$map,
							function (_p66) {
								var _p67 = _p66;
								return {
									ctor: '_Tuple2',
									_0: _p67._0,
									_1: _gicentre$elm_vega$VegaLite$dataTypeSpec(_p67._1)
								};
							},
							_p64._0))
				},
				_1: {ctor: '[]'}
			};
	}
};
var _gicentre$elm_vega$VegaLite$cursorLabel = function (cur) {
	var _p68 = cur;
	switch (_p68.ctor) {
		case 'CAuto':
			return 'auto';
		case 'CDefault':
			return 'default';
		case 'CNone':
			return 'none';
		case 'CContextMenu':
			return 'context-menu';
		case 'CHelp':
			return 'help';
		case 'CPointer':
			return 'pointer';
		case 'CProgress':
			return 'progress';
		case 'CWait':
			return 'wait';
		case 'CCell':
			return 'cell';
		case 'CCrosshair':
			return 'crosshair';
		case 'CText':
			return 'text';
		case 'CVerticalText':
			return 'vertical-text';
		case 'CAlias':
			return 'alias';
		case 'CCopy':
			return 'copy';
		case 'CMove':
			return 'move';
		case 'CNoDrop':
			return 'no-drop';
		case 'CNotAllowed':
			return 'not-allowed';
		case 'CAllScroll':
			return 'all-scroll';
		case 'CColResize':
			return 'col-resize';
		case 'CRowResize':
			return 'row-resize';
		case 'CNResize':
			return 'n-resize';
		case 'CEResize':
			return 'e-resize';
		case 'CSResize':
			return 's-resize';
		case 'CWResize':
			return 'w-resize';
		case 'CNEResize':
			return 'ne-resize';
		case 'CNWResize':
			return 'nw-resize';
		case 'CSEResize':
			return 'se-resize';
		case 'CSWResize':
			return 'sw-resize';
		case 'CEWResize':
			return 'ew-resize';
		case 'CNSResize':
			return 'ns-resize';
		case 'CNESWResize':
			return 'nesw-resize';
		case 'CNWSEResize':
			return 'nwse-resize';
		case 'CZoomIn':
			return 'zoom-in';
		case 'CZoomOut':
			return 'zoom-out';
		case 'CGrab':
			return 'grab';
		default:
			return 'grabbing';
	}
};
var _gicentre$elm_vega$VegaLite$markProperty = function (mProp) {
	var _p69 = mProp;
	switch (_p69.ctor) {
		case 'MFilled':
			return {
				ctor: '_Tuple2',
				_0: 'filled',
				_1: _elm_lang$core$Json_Encode$bool(_p69._0)
			};
		case 'MClip':
			return {
				ctor: '_Tuple2',
				_0: 'clip',
				_1: _elm_lang$core$Json_Encode$bool(_p69._0)
			};
		case 'MColor':
			return {
				ctor: '_Tuple2',
				_0: 'color',
				_1: _elm_lang$core$Json_Encode$string(_p69._0)
			};
		case 'MCursor':
			return {
				ctor: '_Tuple2',
				_0: 'cursor',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$cursorLabel(_p69._0))
			};
		case 'MFill':
			return {
				ctor: '_Tuple2',
				_0: 'fill',
				_1: _elm_lang$core$Json_Encode$string(_p69._0)
			};
		case 'MStroke':
			return {
				ctor: '_Tuple2',
				_0: 'stroke',
				_1: _elm_lang$core$Json_Encode$string(_p69._0)
			};
		case 'MOpacity':
			return {
				ctor: '_Tuple2',
				_0: 'opacity',
				_1: _elm_lang$core$Json_Encode$float(_p69._0)
			};
		case 'MFillOpacity':
			return {
				ctor: '_Tuple2',
				_0: 'fillOpacity',
				_1: _elm_lang$core$Json_Encode$float(_p69._0)
			};
		case 'MStrokeOpacity':
			return {
				ctor: '_Tuple2',
				_0: 'strokeOpacity',
				_1: _elm_lang$core$Json_Encode$float(_p69._0)
			};
		case 'MStrokeWidth':
			return {
				ctor: '_Tuple2',
				_0: 'strokeWidth',
				_1: _elm_lang$core$Json_Encode$float(_p69._0)
			};
		case 'MStrokeDash':
			return {
				ctor: '_Tuple2',
				_0: 'strokeDash',
				_1: _elm_lang$core$Json_Encode$list(
					A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$float, _p69._0))
			};
		case 'MStrokeDashOffset':
			return {
				ctor: '_Tuple2',
				_0: 'strokeDashOffset',
				_1: _elm_lang$core$Json_Encode$float(_p69._0)
			};
		case 'MStyle':
			return {
				ctor: '_Tuple2',
				_0: 'style',
				_1: _elm_lang$core$Json_Encode$list(
					A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p69._0))
			};
		case 'MInterpolate':
			return {
				ctor: '_Tuple2',
				_0: 'interpolate',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$markInterpolationLabel(_p69._0))
			};
		case 'MTension':
			return {
				ctor: '_Tuple2',
				_0: 'tension',
				_1: _elm_lang$core$Json_Encode$float(_p69._0)
			};
		case 'MOrient':
			return {
				ctor: '_Tuple2',
				_0: 'orient',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$markOrientationLabel(_p69._0))
			};
		case 'MShape':
			return {
				ctor: '_Tuple2',
				_0: 'shape',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$symbolLabel(_p69._0))
			};
		case 'MSize':
			return {
				ctor: '_Tuple2',
				_0: 'size',
				_1: _elm_lang$core$Json_Encode$float(_p69._0)
			};
		case 'MAngle':
			return {
				ctor: '_Tuple2',
				_0: 'angle',
				_1: _elm_lang$core$Json_Encode$float(_p69._0)
			};
		case 'MAlign':
			return {
				ctor: '_Tuple2',
				_0: 'align',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$hAlignLabel(_p69._0))
			};
		case 'MBaseline':
			return {
				ctor: '_Tuple2',
				_0: 'baseline',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$vAlignLabel(_p69._0))
			};
		case 'MdX':
			return {
				ctor: '_Tuple2',
				_0: 'dx',
				_1: _elm_lang$core$Json_Encode$float(_p69._0)
			};
		case 'MdY':
			return {
				ctor: '_Tuple2',
				_0: 'dy',
				_1: _elm_lang$core$Json_Encode$float(_p69._0)
			};
		case 'MFont':
			return {
				ctor: '_Tuple2',
				_0: 'font',
				_1: _elm_lang$core$Json_Encode$string(_p69._0)
			};
		case 'MFontSize':
			return {
				ctor: '_Tuple2',
				_0: 'fontSize',
				_1: _elm_lang$core$Json_Encode$float(_p69._0)
			};
		case 'MFontStyle':
			return {
				ctor: '_Tuple2',
				_0: 'fontStyle',
				_1: _elm_lang$core$Json_Encode$string(_p69._0)
			};
		case 'MFontWeight':
			return {
				ctor: '_Tuple2',
				_0: 'fontWeight',
				_1: _gicentre$elm_vega$VegaLite$fontWeightSpec(_p69._0)
			};
		case 'MRadius':
			return {
				ctor: '_Tuple2',
				_0: 'radius',
				_1: _elm_lang$core$Json_Encode$float(_p69._0)
			};
		case 'MText':
			return {
				ctor: '_Tuple2',
				_0: 'text',
				_1: _elm_lang$core$Json_Encode$string(_p69._0)
			};
		case 'MTheta':
			return {
				ctor: '_Tuple2',
				_0: 'theta',
				_1: _elm_lang$core$Json_Encode$float(_p69._0)
			};
		case 'MBinSpacing':
			return {
				ctor: '_Tuple2',
				_0: 'binSpacing',
				_1: _elm_lang$core$Json_Encode$float(_p69._0)
			};
		case 'MContinuousBandSize':
			return {
				ctor: '_Tuple2',
				_0: 'continuousBandSize',
				_1: _elm_lang$core$Json_Encode$float(_p69._0)
			};
		case 'MDiscreteBandSize':
			return {
				ctor: '_Tuple2',
				_0: 'discreteBandSize',
				_1: _elm_lang$core$Json_Encode$float(_p69._0)
			};
		case 'MShortTimeLabels':
			return {
				ctor: '_Tuple2',
				_0: 'shortTimeLabels',
				_1: _elm_lang$core$Json_Encode$bool(_p69._0)
			};
		case 'MBandSize':
			return {
				ctor: '_Tuple2',
				_0: 'bandSize',
				_1: _elm_lang$core$Json_Encode$float(_p69._0)
			};
		case 'MThickness':
			return {
				ctor: '_Tuple2',
				_0: 'thickness',
				_1: _elm_lang$core$Json_Encode$float(_p69._0)
			};
		default:
			return {
				ctor: '_Tuple2',
				_0: 'point',
				_1: _gicentre$elm_vega$VegaLite$pointMarkerSpec(_p69._0)
			};
	}
};
var _gicentre$elm_vega$VegaLite$pointMarkerSpec = function (pm) {
	var _p70 = pm;
	switch (_p70.ctor) {
		case 'PMTransparent':
			return _elm_lang$core$Json_Encode$string('transparent');
		case 'PMNone':
			return _elm_lang$core$Json_Encode$bool(false);
		default:
			return _elm_lang$core$Json_Encode$object(
				A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$markProperty, _p70._0));
	}
};
var _gicentre$elm_vega$VegaLite$cInterpolateSpec = function (iType) {
	var _p71 = iType;
	switch (_p71.ctor) {
		case 'Rgb':
			return _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'type',
						_1: _elm_lang$core$Json_Encode$string('rgb')
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'gamma',
							_1: _elm_lang$core$Json_Encode$float(_p71._0)
						},
						_1: {ctor: '[]'}
					}
				});
		case 'Hsl':
			return _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'type',
						_1: _elm_lang$core$Json_Encode$string('hsl')
					},
					_1: {ctor: '[]'}
				});
		case 'HslLong':
			return _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'type',
						_1: _elm_lang$core$Json_Encode$string('hsl-long')
					},
					_1: {ctor: '[]'}
				});
		case 'Lab':
			return _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'type',
						_1: _elm_lang$core$Json_Encode$string('lab')
					},
					_1: {ctor: '[]'}
				});
		case 'Hcl':
			return _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'type',
						_1: _elm_lang$core$Json_Encode$string('hcl')
					},
					_1: {ctor: '[]'}
				});
		case 'HclLong':
			return _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'type',
						_1: _elm_lang$core$Json_Encode$string('hcl-long')
					},
					_1: {ctor: '[]'}
				});
		case 'CubeHelix':
			return _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'type',
						_1: _elm_lang$core$Json_Encode$string('cubehelix')
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'gamma',
							_1: _elm_lang$core$Json_Encode$float(_p71._0)
						},
						_1: {ctor: '[]'}
					}
				});
		default:
			return _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'type',
						_1: _elm_lang$core$Json_Encode$string('cubehelix-long')
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'gamma',
							_1: _elm_lang$core$Json_Encode$float(_p71._0)
						},
						_1: {ctor: '[]'}
					}
				});
	}
};
var _gicentre$elm_vega$VegaLite$scaleProperty = function (scaleProp) {
	var _p72 = scaleProp;
	switch (_p72.ctor) {
		case 'SType':
			return {
				ctor: '_Tuple2',
				_0: 'type',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$scaleLabel(_p72._0))
			};
		case 'SDomain':
			return {
				ctor: '_Tuple2',
				_0: 'domain',
				_1: _gicentre$elm_vega$VegaLite$scaleDomainSpec(_p72._0)
			};
		case 'SRange':
			var _p73 = _p72._0;
			switch (_p73.ctor) {
				case 'RNumbers':
					return {
						ctor: '_Tuple2',
						_0: 'range',
						_1: _elm_lang$core$Json_Encode$list(
							A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$float, _p73._0))
					};
				case 'RStrings':
					return {
						ctor: '_Tuple2',
						_0: 'range',
						_1: _elm_lang$core$Json_Encode$list(
							A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p73._0))
					};
				default:
					return {
						ctor: '_Tuple2',
						_0: 'range',
						_1: _elm_lang$core$Json_Encode$string(_p73._0)
					};
			}
		case 'SScheme':
			return A2(_gicentre$elm_vega$VegaLite$schemeProperty, _p72._0, _p72._1);
		case 'SPadding':
			return {
				ctor: '_Tuple2',
				_0: 'padding',
				_1: _elm_lang$core$Json_Encode$float(_p72._0)
			};
		case 'SPaddingInner':
			return {
				ctor: '_Tuple2',
				_0: 'paddingInner',
				_1: _elm_lang$core$Json_Encode$float(_p72._0)
			};
		case 'SPaddingOuter':
			return {
				ctor: '_Tuple2',
				_0: 'paddingOuter',
				_1: _elm_lang$core$Json_Encode$float(_p72._0)
			};
		case 'SRangeStep':
			var _p74 = _p72._0;
			if (_p74.ctor === 'Just') {
				return {
					ctor: '_Tuple2',
					_0: 'rangeStep',
					_1: _elm_lang$core$Json_Encode$float(_p74._0)
				};
			} else {
				return {ctor: '_Tuple2', _0: 'rangeStep', _1: _elm_lang$core$Json_Encode$null};
			}
		case 'SRound':
			return {
				ctor: '_Tuple2',
				_0: 'round',
				_1: _elm_lang$core$Json_Encode$bool(_p72._0)
			};
		case 'SClamp':
			return {
				ctor: '_Tuple2',
				_0: 'clamp',
				_1: _elm_lang$core$Json_Encode$bool(_p72._0)
			};
		case 'SInterpolate':
			return {
				ctor: '_Tuple2',
				_0: 'interpolate',
				_1: _gicentre$elm_vega$VegaLite$cInterpolateSpec(_p72._0)
			};
		case 'SNice':
			return {
				ctor: '_Tuple2',
				_0: 'nice',
				_1: _gicentre$elm_vega$VegaLite$scaleNiceSpec(_p72._0)
			};
		case 'SZero':
			return {
				ctor: '_Tuple2',
				_0: 'zero',
				_1: _elm_lang$core$Json_Encode$bool(_p72._0)
			};
		default:
			return {
				ctor: '_Tuple2',
				_0: 'reverse',
				_1: _elm_lang$core$Json_Encode$bool(_p72._0)
			};
	}
};
var _gicentre$elm_vega$VegaLite$channelLabel = function (ch) {
	var _p75 = ch;
	switch (_p75.ctor) {
		case 'ChX':
			return 'x';
		case 'ChY':
			return 'y';
		case 'ChX2':
			return 'x2';
		case 'ChY2':
			return 'y2';
		case 'ChColor':
			return 'color';
		case 'ChOpacity':
			return 'opacity';
		case 'ChShape':
			return 'shape';
		default:
			return 'size';
	}
};
var _gicentre$elm_vega$VegaLite$resolveProperty = function (res) {
	var _p76 = res;
	switch (_p76.ctor) {
		case 'RAxis':
			return {
				ctor: '_Tuple2',
				_0: 'axis',
				_1: _elm_lang$core$Json_Encode$object(
					A2(
						_elm_lang$core$List$map,
						function (_p77) {
							var _p78 = _p77;
							return {
								ctor: '_Tuple2',
								_0: _gicentre$elm_vega$VegaLite$channelLabel(_p78._0),
								_1: _elm_lang$core$Json_Encode$string(
									_gicentre$elm_vega$VegaLite$resolutionLabel(_p78._1))
							};
						},
						_p76._0))
			};
		case 'RLegend':
			return {
				ctor: '_Tuple2',
				_0: 'legend',
				_1: _elm_lang$core$Json_Encode$object(
					A2(
						_elm_lang$core$List$map,
						function (_p79) {
							var _p80 = _p79;
							return {
								ctor: '_Tuple2',
								_0: _gicentre$elm_vega$VegaLite$channelLabel(_p80._0),
								_1: _elm_lang$core$Json_Encode$string(
									_gicentre$elm_vega$VegaLite$resolutionLabel(_p80._1))
							};
						},
						_p76._0))
			};
		default:
			return {
				ctor: '_Tuple2',
				_0: 'scale',
				_1: _elm_lang$core$Json_Encode$object(
					A2(
						_elm_lang$core$List$map,
						function (_p81) {
							var _p82 = _p81;
							return {
								ctor: '_Tuple2',
								_0: _gicentre$elm_vega$VegaLite$channelLabel(_p82._0),
								_1: _elm_lang$core$Json_Encode$string(
									_gicentre$elm_vega$VegaLite$resolutionLabel(_p82._1))
							};
						},
						_p76._0))
			};
	}
};
var _gicentre$elm_vega$VegaLite$booleanOpSpec = function (bo) {
	var _p83 = bo;
	switch (_p83.ctor) {
		case 'Expr':
			return _elm_lang$core$Json_Encode$string(_p83._0);
		case 'SelectionName':
			return _elm_lang$core$Json_Encode$string(_p83._0);
		case 'Selection':
			return _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'selection',
						_1: _elm_lang$core$Json_Encode$string(_p83._0)
					},
					_1: {ctor: '[]'}
				});
		case 'And':
			return _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'and',
						_1: _elm_lang$core$Json_Encode$list(
							{
								ctor: '::',
								_0: _gicentre$elm_vega$VegaLite$booleanOpSpec(_p83._0),
								_1: {
									ctor: '::',
									_0: _gicentre$elm_vega$VegaLite$booleanOpSpec(_p83._1),
									_1: {ctor: '[]'}
								}
							})
					},
					_1: {ctor: '[]'}
				});
		case 'Or':
			return _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'or',
						_1: _elm_lang$core$Json_Encode$list(
							{
								ctor: '::',
								_0: _gicentre$elm_vega$VegaLite$booleanOpSpec(_p83._0),
								_1: {
									ctor: '::',
									_0: _gicentre$elm_vega$VegaLite$booleanOpSpec(_p83._1),
									_1: {ctor: '[]'}
								}
							})
					},
					_1: {ctor: '[]'}
				});
		default:
			return _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'not',
						_1: _gicentre$elm_vega$VegaLite$booleanOpSpec(_p83._0)
					},
					_1: {ctor: '[]'}
				});
	}
};
var _gicentre$elm_vega$VegaLite$binProperty = function (binProp) {
	var _p84 = binProp;
	switch (_p84.ctor) {
		case 'MaxBins':
			return {
				ctor: '_Tuple2',
				_0: 'maxbins',
				_1: _elm_lang$core$Json_Encode$int(_p84._0)
			};
		case 'Base':
			return {
				ctor: '_Tuple2',
				_0: 'base',
				_1: _elm_lang$core$Json_Encode$float(_p84._0)
			};
		case 'Step':
			return {
				ctor: '_Tuple2',
				_0: 'step',
				_1: _elm_lang$core$Json_Encode$float(_p84._0)
			};
		case 'Steps':
			return {
				ctor: '_Tuple2',
				_0: 'steps',
				_1: _elm_lang$core$Json_Encode$list(
					A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$float, _p84._0))
			};
		case 'MinStep':
			return {
				ctor: '_Tuple2',
				_0: 'minstep',
				_1: _elm_lang$core$Json_Encode$float(_p84._0)
			};
		case 'Divide':
			return {
				ctor: '_Tuple2',
				_0: 'divide',
				_1: _elm_lang$core$Json_Encode$list(
					{
						ctor: '::',
						_0: _elm_lang$core$Json_Encode$float(_p84._0),
						_1: {
							ctor: '::',
							_0: _elm_lang$core$Json_Encode$float(_p84._1),
							_1: {ctor: '[]'}
						}
					})
			};
		case 'Divides':
			return {
				ctor: '_Tuple2',
				_0: 'divide',
				_1: _elm_lang$core$Json_Encode$list(
					A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$float, _p84._0))
			};
		case 'Extent':
			return {
				ctor: '_Tuple2',
				_0: 'extent',
				_1: _elm_lang$core$Json_Encode$list(
					{
						ctor: '::',
						_0: _elm_lang$core$Json_Encode$float(_p84._0),
						_1: {
							ctor: '::',
							_0: _elm_lang$core$Json_Encode$float(_p84._1),
							_1: {ctor: '[]'}
						}
					})
			};
		default:
			return {
				ctor: '_Tuple2',
				_0: 'nice',
				_1: _elm_lang$core$Json_Encode$bool(_p84._0)
			};
	}
};
var _gicentre$elm_vega$VegaLite$bindingSpec = function (bnd) {
	var _p85 = bnd;
	switch (_p85.ctor) {
		case 'IRange':
			return {
				ctor: '_Tuple2',
				_0: _p85._0,
				_1: _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'input',
							_1: _elm_lang$core$Json_Encode$string('range')
						},
						_1: A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$inputProperty, _p85._1)
					})
			};
		case 'ICheckbox':
			return {
				ctor: '_Tuple2',
				_0: _p85._0,
				_1: _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'input',
							_1: _elm_lang$core$Json_Encode$string('checkbox')
						},
						_1: A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$inputProperty, _p85._1)
					})
			};
		case 'IRadio':
			return {
				ctor: '_Tuple2',
				_0: _p85._0,
				_1: _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'input',
							_1: _elm_lang$core$Json_Encode$string('radio')
						},
						_1: A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$inputProperty, _p85._1)
					})
			};
		case 'ISelect':
			return {
				ctor: '_Tuple2',
				_0: _p85._0,
				_1: _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'input',
							_1: _elm_lang$core$Json_Encode$string('select')
						},
						_1: A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$inputProperty, _p85._1)
					})
			};
		case 'IText':
			return {
				ctor: '_Tuple2',
				_0: _p85._0,
				_1: _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'input',
							_1: _elm_lang$core$Json_Encode$string('text')
						},
						_1: A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$inputProperty, _p85._1)
					})
			};
		case 'INumber':
			return {
				ctor: '_Tuple2',
				_0: _p85._0,
				_1: _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'input',
							_1: _elm_lang$core$Json_Encode$string('number')
						},
						_1: A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$inputProperty, _p85._1)
					})
			};
		case 'IDate':
			return {
				ctor: '_Tuple2',
				_0: _p85._0,
				_1: _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'input',
							_1: _elm_lang$core$Json_Encode$string('date')
						},
						_1: A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$inputProperty, _p85._1)
					})
			};
		case 'ITime':
			return {
				ctor: '_Tuple2',
				_0: _p85._0,
				_1: _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'input',
							_1: _elm_lang$core$Json_Encode$string('time')
						},
						_1: A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$inputProperty, _p85._1)
					})
			};
		case 'IMonth':
			return {
				ctor: '_Tuple2',
				_0: _p85._0,
				_1: _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'input',
							_1: _elm_lang$core$Json_Encode$string('month')
						},
						_1: A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$inputProperty, _p85._1)
					})
			};
		case 'IWeek':
			return {
				ctor: '_Tuple2',
				_0: _p85._0,
				_1: _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'input',
							_1: _elm_lang$core$Json_Encode$string('week')
						},
						_1: A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$inputProperty, _p85._1)
					})
			};
		case 'IDateTimeLocal':
			return {
				ctor: '_Tuple2',
				_0: _p85._0,
				_1: _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'input',
							_1: _elm_lang$core$Json_Encode$string('datetimelocal')
						},
						_1: A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$inputProperty, _p85._1)
					})
			};
		case 'ITel':
			return {
				ctor: '_Tuple2',
				_0: _p85._0,
				_1: _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'input',
							_1: _elm_lang$core$Json_Encode$string('tel')
						},
						_1: A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$inputProperty, _p85._1)
					})
			};
		default:
			return {
				ctor: '_Tuple2',
				_0: _p85._0,
				_1: _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'input',
							_1: _elm_lang$core$Json_Encode$string('color')
						},
						_1: A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$inputProperty, _p85._1)
					})
			};
	}
};
var _gicentre$elm_vega$VegaLite$selectionProperty = function (selProp) {
	var _p86 = selProp;
	switch (_p86.ctor) {
		case 'Fields':
			return {
				ctor: '_Tuple2',
				_0: 'fields',
				_1: _elm_lang$core$Json_Encode$list(
					A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p86._0))
			};
		case 'Encodings':
			return {
				ctor: '_Tuple2',
				_0: 'encodings',
				_1: _elm_lang$core$Json_Encode$list(
					A2(
						_elm_lang$core$List$map,
						function (_p87) {
							return _elm_lang$core$Json_Encode$string(
								_gicentre$elm_vega$VegaLite$channelLabel(_p87));
						},
						_p86._0))
			};
		case 'On':
			return {
				ctor: '_Tuple2',
				_0: 'on',
				_1: _elm_lang$core$Json_Encode$string(_p86._0)
			};
		case 'Empty':
			return {
				ctor: '_Tuple2',
				_0: 'empty',
				_1: _elm_lang$core$Json_Encode$string('none')
			};
		case 'ResolveSelections':
			return {
				ctor: '_Tuple2',
				_0: 'resolve',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$selectionResolutionLabel(_p86._0))
			};
		case 'SelectionMark':
			return {
				ctor: '_Tuple2',
				_0: 'mark',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$selectionMarkProperty, _p86._0))
			};
		case 'BindScales':
			return {
				ctor: '_Tuple2',
				_0: 'bind',
				_1: _elm_lang$core$Json_Encode$string('scales')
			};
		case 'Bind':
			return {
				ctor: '_Tuple2',
				_0: 'bind',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$bindingSpec, _p86._0))
			};
		case 'Nearest':
			return {
				ctor: '_Tuple2',
				_0: 'nearest',
				_1: _elm_lang$core$Json_Encode$bool(_p86._0)
			};
		case 'Toggle':
			return {
				ctor: '_Tuple2',
				_0: 'toggle',
				_1: _elm_lang$core$Json_Encode$string(_p86._0)
			};
		case 'Translate':
			var _p88 = _p86._0;
			return _elm_lang$core$Native_Utils.eq(_p88, '') ? {
				ctor: '_Tuple2',
				_0: 'translate',
				_1: _elm_lang$core$Json_Encode$bool(false)
			} : {
				ctor: '_Tuple2',
				_0: 'translate',
				_1: _elm_lang$core$Json_Encode$string(_p88)
			};
		default:
			var _p89 = _p86._0;
			return _elm_lang$core$Native_Utils.eq(_p89, '') ? {
				ctor: '_Tuple2',
				_0: 'zoom',
				_1: _elm_lang$core$Json_Encode$bool(false)
			} : {
				ctor: '_Tuple2',
				_0: 'zoom',
				_1: _elm_lang$core$Json_Encode$string(_p89)
			};
	}
};
var _gicentre$elm_vega$VegaLite$axisProperty = function (axisProp) {
	var _p90 = axisProp;
	switch (_p90.ctor) {
		case 'AxFormat':
			return {
				ctor: '_Tuple2',
				_0: 'format',
				_1: _elm_lang$core$Json_Encode$string(_p90._0)
			};
		case 'AxLabels':
			return {
				ctor: '_Tuple2',
				_0: 'labels',
				_1: _elm_lang$core$Json_Encode$bool(_p90._0)
			};
		case 'AxLabelAngle':
			return {
				ctor: '_Tuple2',
				_0: 'labelAngle',
				_1: _elm_lang$core$Json_Encode$float(_p90._0)
			};
		case 'AxLabelOverlap':
			return {
				ctor: '_Tuple2',
				_0: 'labelOverlap',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$overlapStrategyLabel(_p90._0))
			};
		case 'AxLabelPadding':
			return {
				ctor: '_Tuple2',
				_0: 'labelPadding',
				_1: _elm_lang$core$Json_Encode$float(_p90._0)
			};
		case 'AxDomain':
			return {
				ctor: '_Tuple2',
				_0: 'domain',
				_1: _elm_lang$core$Json_Encode$bool(_p90._0)
			};
		case 'AxGrid':
			return {
				ctor: '_Tuple2',
				_0: 'grid',
				_1: _elm_lang$core$Json_Encode$bool(_p90._0)
			};
		case 'AxMaxExtent':
			return {
				ctor: '_Tuple2',
				_0: 'maxExtent',
				_1: _elm_lang$core$Json_Encode$float(_p90._0)
			};
		case 'AxMinExtent':
			return {
				ctor: '_Tuple2',
				_0: 'minExtent',
				_1: _elm_lang$core$Json_Encode$float(_p90._0)
			};
		case 'AxOrient':
			return {
				ctor: '_Tuple2',
				_0: 'orient',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$sideLabel(_p90._0))
			};
		case 'AxOffset':
			return {
				ctor: '_Tuple2',
				_0: 'offset',
				_1: _elm_lang$core$Json_Encode$float(_p90._0)
			};
		case 'AxPosition':
			return {
				ctor: '_Tuple2',
				_0: 'position',
				_1: _elm_lang$core$Json_Encode$float(_p90._0)
			};
		case 'AxZIndex':
			return {
				ctor: '_Tuple2',
				_0: 'zindex',
				_1: _elm_lang$core$Json_Encode$int(_p90._0)
			};
		case 'AxTicks':
			return {
				ctor: '_Tuple2',
				_0: 'ticks',
				_1: _elm_lang$core$Json_Encode$bool(_p90._0)
			};
		case 'AxTickCount':
			return {
				ctor: '_Tuple2',
				_0: 'tickCount',
				_1: _elm_lang$core$Json_Encode$int(_p90._0)
			};
		case 'AxTickSize':
			return {
				ctor: '_Tuple2',
				_0: 'tickSize',
				_1: _elm_lang$core$Json_Encode$float(_p90._0)
			};
		case 'AxValues':
			return {
				ctor: '_Tuple2',
				_0: 'values',
				_1: _elm_lang$core$Json_Encode$list(
					A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$float, _p90._0))
			};
		case 'AxDates':
			return {
				ctor: '_Tuple2',
				_0: 'values',
				_1: _elm_lang$core$Json_Encode$list(
					A2(
						_elm_lang$core$List$map,
						function (dts) {
							return _elm_lang$core$Json_Encode$object(
								A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$dateTimeProperty, dts));
						},
						_p90._0))
			};
		case 'AxTitle':
			return {
				ctor: '_Tuple2',
				_0: 'title',
				_1: _elm_lang$core$Json_Encode$string(_p90._0)
			};
		case 'AxTitleAlign':
			return {
				ctor: '_Tuple2',
				_0: 'titleAlign',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$hAlignLabel(_p90._0))
			};
		case 'AxTitleAngle':
			return {
				ctor: '_Tuple2',
				_0: 'titleAngle',
				_1: _elm_lang$core$Json_Encode$float(_p90._0)
			};
		case 'AxTitleMaxLength':
			return {
				ctor: '_Tuple2',
				_0: 'titleMaxLength',
				_1: _elm_lang$core$Json_Encode$float(_p90._0)
			};
		default:
			return {
				ctor: '_Tuple2',
				_0: 'titlePadding',
				_1: _elm_lang$core$Json_Encode$float(_p90._0)
			};
	}
};
var _gicentre$elm_vega$VegaLite$axisConfigProperty = function (axisCfg) {
	var _p91 = axisCfg;
	switch (_p91.ctor) {
		case 'BandPosition':
			return {
				ctor: '_Tuple2',
				_0: 'bandPosition',
				_1: _elm_lang$core$Json_Encode$float(_p91._0)
			};
		case 'Domain':
			return {
				ctor: '_Tuple2',
				_0: 'domain',
				_1: _elm_lang$core$Json_Encode$bool(_p91._0)
			};
		case 'DomainColor':
			return {
				ctor: '_Tuple2',
				_0: 'domainColor',
				_1: _elm_lang$core$Json_Encode$string(_p91._0)
			};
		case 'DomainWidth':
			return {
				ctor: '_Tuple2',
				_0: 'domainWidth',
				_1: _elm_lang$core$Json_Encode$float(_p91._0)
			};
		case 'MaxExtent':
			return {
				ctor: '_Tuple2',
				_0: 'maxExtent',
				_1: _elm_lang$core$Json_Encode$float(_p91._0)
			};
		case 'MinExtent':
			return {
				ctor: '_Tuple2',
				_0: 'minExtent',
				_1: _elm_lang$core$Json_Encode$float(_p91._0)
			};
		case 'Grid':
			return {
				ctor: '_Tuple2',
				_0: 'grid',
				_1: _elm_lang$core$Json_Encode$bool(_p91._0)
			};
		case 'GridColor':
			return {
				ctor: '_Tuple2',
				_0: 'gridColor',
				_1: _elm_lang$core$Json_Encode$string(_p91._0)
			};
		case 'GridDash':
			return {
				ctor: '_Tuple2',
				_0: 'gridDash',
				_1: _elm_lang$core$Json_Encode$list(
					A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$float, _p91._0))
			};
		case 'GridOpacity':
			return {
				ctor: '_Tuple2',
				_0: 'gridOpacity',
				_1: _elm_lang$core$Json_Encode$float(_p91._0)
			};
		case 'GridWidth':
			return {
				ctor: '_Tuple2',
				_0: 'gridWidth',
				_1: _elm_lang$core$Json_Encode$float(_p91._0)
			};
		case 'Labels':
			return {
				ctor: '_Tuple2',
				_0: 'labels',
				_1: _elm_lang$core$Json_Encode$bool(_p91._0)
			};
		case 'LabelAngle':
			return {
				ctor: '_Tuple2',
				_0: 'labelAngle',
				_1: _elm_lang$core$Json_Encode$float(_p91._0)
			};
		case 'LabelColor':
			return {
				ctor: '_Tuple2',
				_0: 'labelColor',
				_1: _elm_lang$core$Json_Encode$string(_p91._0)
			};
		case 'LabelFont':
			return {
				ctor: '_Tuple2',
				_0: 'labelFont',
				_1: _elm_lang$core$Json_Encode$string(_p91._0)
			};
		case 'LabelFontSize':
			return {
				ctor: '_Tuple2',
				_0: 'labelFontSize',
				_1: _elm_lang$core$Json_Encode$float(_p91._0)
			};
		case 'LabelLimit':
			return {
				ctor: '_Tuple2',
				_0: 'labelLimit',
				_1: _elm_lang$core$Json_Encode$float(_p91._0)
			};
		case 'LabelOverlap':
			return {
				ctor: '_Tuple2',
				_0: 'labelOverlap',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$overlapStrategyLabel(_p91._0))
			};
		case 'LabelPadding':
			return {
				ctor: '_Tuple2',
				_0: 'labelPadding',
				_1: _elm_lang$core$Json_Encode$float(_p91._0)
			};
		case 'ShortTimeLabels':
			return {
				ctor: '_Tuple2',
				_0: 'shortTimeLabels',
				_1: _elm_lang$core$Json_Encode$bool(_p91._0)
			};
		case 'Ticks':
			return {
				ctor: '_Tuple2',
				_0: 'ticks',
				_1: _elm_lang$core$Json_Encode$bool(_p91._0)
			};
		case 'TickColor':
			return {
				ctor: '_Tuple2',
				_0: 'tickColor',
				_1: _elm_lang$core$Json_Encode$string(_p91._0)
			};
		case 'TickRound':
			return {
				ctor: '_Tuple2',
				_0: 'tickRound',
				_1: _elm_lang$core$Json_Encode$bool(_p91._0)
			};
		case 'TickSize':
			return {
				ctor: '_Tuple2',
				_0: 'tickSize',
				_1: _elm_lang$core$Json_Encode$float(_p91._0)
			};
		case 'TickWidth':
			return {
				ctor: '_Tuple2',
				_0: 'tickWidth',
				_1: _elm_lang$core$Json_Encode$float(_p91._0)
			};
		case 'TitleAlign':
			return {
				ctor: '_Tuple2',
				_0: 'titleAlign',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$hAlignLabel(_p91._0))
			};
		case 'TitleAngle':
			return {
				ctor: '_Tuple2',
				_0: 'titleAngle',
				_1: _elm_lang$core$Json_Encode$float(_p91._0)
			};
		case 'TitleBaseline':
			return {
				ctor: '_Tuple2',
				_0: 'titleBaseline',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$vAlignLabel(_p91._0))
			};
		case 'TitleColor':
			return {
				ctor: '_Tuple2',
				_0: 'titleColor',
				_1: _elm_lang$core$Json_Encode$string(_p91._0)
			};
		case 'TitleFont':
			return {
				ctor: '_Tuple2',
				_0: 'titleFont',
				_1: _elm_lang$core$Json_Encode$string(_p91._0)
			};
		case 'TitleFontWeight':
			return {
				ctor: '_Tuple2',
				_0: 'titleFontWeight',
				_1: _gicentre$elm_vega$VegaLite$fontWeightSpec(_p91._0)
			};
		case 'TitleFontSize':
			return {
				ctor: '_Tuple2',
				_0: 'titleFontSize',
				_1: _elm_lang$core$Json_Encode$float(_p91._0)
			};
		case 'TitleLimit':
			return {
				ctor: '_Tuple2',
				_0: 'titleLimit',
				_1: _elm_lang$core$Json_Encode$float(_p91._0)
			};
		case 'TitleMaxLength':
			return {
				ctor: '_Tuple2',
				_0: 'titleMaxLength',
				_1: _elm_lang$core$Json_Encode$float(_p91._0)
			};
		case 'TitlePadding':
			return {
				ctor: '_Tuple2',
				_0: 'titlePadding',
				_1: _elm_lang$core$Json_Encode$float(_p91._0)
			};
		case 'TitleX':
			return {
				ctor: '_Tuple2',
				_0: 'titleX',
				_1: _elm_lang$core$Json_Encode$float(_p91._0)
			};
		default:
			return {
				ctor: '_Tuple2',
				_0: 'titleY',
				_1: _elm_lang$core$Json_Encode$float(_p91._0)
			};
	}
};
var _gicentre$elm_vega$VegaLite$autosizeProperty = function (asCfg) {
	var _p92 = asCfg;
	switch (_p92.ctor) {
		case 'APad':
			return {
				ctor: '_Tuple2',
				_0: 'type',
				_1: _elm_lang$core$Json_Encode$string('pad')
			};
		case 'AFit':
			return {
				ctor: '_Tuple2',
				_0: 'type',
				_1: _elm_lang$core$Json_Encode$string('fit')
			};
		case 'ANone':
			return {
				ctor: '_Tuple2',
				_0: 'type',
				_1: _elm_lang$core$Json_Encode$string('none')
			};
		case 'AResize':
			return {
				ctor: '_Tuple2',
				_0: 'resize',
				_1: _elm_lang$core$Json_Encode$bool(true)
			};
		case 'AContent':
			return {
				ctor: '_Tuple2',
				_0: 'contains',
				_1: _elm_lang$core$Json_Encode$string('content')
			};
		default:
			return {
				ctor: '_Tuple2',
				_0: 'contains',
				_1: _elm_lang$core$Json_Encode$string('padding')
			};
	}
};
var _gicentre$elm_vega$VegaLite$arrangementLabel = function (arrng) {
	var _p93 = arrng;
	if (_p93.ctor === 'Row') {
		return 'row';
	} else {
		return 'column';
	}
};
var _gicentre$elm_vega$VegaLite$sortProperty = function (sp) {
	var _p94 = sp;
	switch (_p94.ctor) {
		case 'Ascending':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'order',
					_1: _elm_lang$core$Json_Encode$string('ascending')
				},
				_1: {ctor: '[]'}
			};
		case 'Descending':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'order',
					_1: _elm_lang$core$Json_Encode$string('descending')
				},
				_1: {ctor: '[]'}
			};
		case 'Op':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'op',
					_1: _elm_lang$core$Json_Encode$string(
						_gicentre$elm_vega$VegaLite$operationLabel(_p94._0))
				},
				_1: {ctor: '[]'}
			};
		case 'ByField':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'field',
					_1: _elm_lang$core$Json_Encode$string(_p94._0)
				},
				_1: {ctor: '[]'}
			};
		case 'ByFieldOp':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'field',
					_1: _elm_lang$core$Json_Encode$string(_p94._0)
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'op',
						_1: _elm_lang$core$Json_Encode$string(
							_gicentre$elm_vega$VegaLite$operationLabel(_p94._1))
					},
					_1: {ctor: '[]'}
				}
			};
		case 'ByRepeat':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'field',
					_1: _elm_lang$core$Json_Encode$object(
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'repeat',
								_1: _elm_lang$core$Json_Encode$string(
									_gicentre$elm_vega$VegaLite$arrangementLabel(_p94._0))
							},
							_1: {ctor: '[]'}
						})
				},
				_1: {ctor: '[]'}
			};
		case 'ByRepeatOp':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'field',
					_1: _elm_lang$core$Json_Encode$object(
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'repeat',
								_1: _elm_lang$core$Json_Encode$string(
									_gicentre$elm_vega$VegaLite$arrangementLabel(_p94._0))
							},
							_1: {ctor: '[]'}
						})
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'op',
						_1: _elm_lang$core$Json_Encode$string(
							_gicentre$elm_vega$VegaLite$operationLabel(_p94._1))
					},
					_1: {ctor: '[]'}
				}
			};
		default:
			return A2(
				_elm_lang$core$Debug$log,
				'Warning: Unexpected custom sorting provided to sortProperty',
				{ctor: '[]'});
	}
};
var _gicentre$elm_vega$VegaLite$anchorLabel = function (an) {
	var _p95 = an;
	switch (_p95.ctor) {
		case 'AStart':
			return 'start';
		case 'AMiddle':
			return 'middle';
		default:
			return 'end';
	}
};
var _gicentre$elm_vega$VegaLite$titleConfigSpec = function (titleCfg) {
	var _p96 = titleCfg;
	switch (_p96.ctor) {
		case 'TAnchor':
			return {
				ctor: '_Tuple2',
				_0: 'anchor',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$anchorLabel(_p96._0))
			};
		case 'TAngle':
			return {
				ctor: '_Tuple2',
				_0: 'angle',
				_1: _elm_lang$core$Json_Encode$float(_p96._0)
			};
		case 'TBaseline':
			return {
				ctor: '_Tuple2',
				_0: 'baseline',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$vAlignLabel(_p96._0))
			};
		case 'TColor':
			return {
				ctor: '_Tuple2',
				_0: 'color',
				_1: _elm_lang$core$Json_Encode$string(_p96._0)
			};
		case 'TFont':
			return {
				ctor: '_Tuple2',
				_0: 'font',
				_1: _elm_lang$core$Json_Encode$string(_p96._0)
			};
		case 'TFontSize':
			return {
				ctor: '_Tuple2',
				_0: 'fontSize',
				_1: _elm_lang$core$Json_Encode$float(_p96._0)
			};
		case 'TFontWeight':
			return {
				ctor: '_Tuple2',
				_0: 'fontWeight',
				_1: _gicentre$elm_vega$VegaLite$fontWeightSpec(_p96._0)
			};
		case 'TLimit':
			return {
				ctor: '_Tuple2',
				_0: 'limit',
				_1: _elm_lang$core$Json_Encode$float(_p96._0)
			};
		case 'TOffset':
			return {
				ctor: '_Tuple2',
				_0: 'offset',
				_1: _elm_lang$core$Json_Encode$float(_p96._0)
			};
		default:
			return {
				ctor: '_Tuple2',
				_0: 'orient',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$sideLabel(_p96._0))
			};
	}
};
var _gicentre$elm_vega$VegaLite$configProperty = function (configProp) {
	var _p97 = configProp;
	switch (_p97.ctor) {
		case 'Autosize':
			return {
				ctor: '_Tuple2',
				_0: 'autosize',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$autosizeProperty, _p97._0))
			};
		case 'Background':
			return {
				ctor: '_Tuple2',
				_0: 'background',
				_1: _elm_lang$core$Json_Encode$string(_p97._0)
			};
		case 'CountTitle':
			return {
				ctor: '_Tuple2',
				_0: 'countTitle',
				_1: _elm_lang$core$Json_Encode$string(_p97._0)
			};
		case 'FieldTitle':
			return {
				ctor: '_Tuple2',
				_0: 'fieldTitle',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$fieldTitleLabel(_p97._0))
			};
		case 'RemoveInvalid':
			return _p97._0 ? {
				ctor: '_Tuple2',
				_0: 'invalidValues',
				_1: _elm_lang$core$Json_Encode$string('filter')
			} : {ctor: '_Tuple2', _0: 'invalidValues', _1: _elm_lang$core$Json_Encode$null};
		case 'NumberFormat':
			return {
				ctor: '_Tuple2',
				_0: 'numberFormat',
				_1: _elm_lang$core$Json_Encode$string(_p97._0)
			};
		case 'Padding':
			return {
				ctor: '_Tuple2',
				_0: 'padding',
				_1: _gicentre$elm_vega$VegaLite$paddingSpec(_p97._0)
			};
		case 'TimeFormat':
			return {
				ctor: '_Tuple2',
				_0: 'timeFormat',
				_1: _elm_lang$core$Json_Encode$string(_p97._0)
			};
		case 'Axis':
			return {
				ctor: '_Tuple2',
				_0: 'axis',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$axisConfigProperty, _p97._0))
			};
		case 'AxisX':
			return {
				ctor: '_Tuple2',
				_0: 'axisX',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$axisConfigProperty, _p97._0))
			};
		case 'AxisY':
			return {
				ctor: '_Tuple2',
				_0: 'axisY',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$axisConfigProperty, _p97._0))
			};
		case 'AxisLeft':
			return {
				ctor: '_Tuple2',
				_0: 'axisLeft',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$axisConfigProperty, _p97._0))
			};
		case 'AxisRight':
			return {
				ctor: '_Tuple2',
				_0: 'axisRight',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$axisConfigProperty, _p97._0))
			};
		case 'AxisTop':
			return {
				ctor: '_Tuple2',
				_0: 'axisTop',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$axisConfigProperty, _p97._0))
			};
		case 'AxisBottom':
			return {
				ctor: '_Tuple2',
				_0: 'axisBottom',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$axisConfigProperty, _p97._0))
			};
		case 'AxisBand':
			return {
				ctor: '_Tuple2',
				_0: 'axisBand',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$axisConfigProperty, _p97._0))
			};
		case 'Legend':
			return {
				ctor: '_Tuple2',
				_0: 'legend',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$legendConfigProperty, _p97._0))
			};
		case 'MarkStyle':
			return {
				ctor: '_Tuple2',
				_0: 'mark',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$markProperty, _p97._0))
			};
		case 'Projection':
			return {
				ctor: '_Tuple2',
				_0: 'projection',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$projectionProperty, _p97._0))
			};
		case 'AreaStyle':
			return {
				ctor: '_Tuple2',
				_0: 'area',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$markProperty, _p97._0))
			};
		case 'BarStyle':
			return {
				ctor: '_Tuple2',
				_0: 'bar',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$markProperty, _p97._0))
			};
		case 'CircleStyle':
			return {
				ctor: '_Tuple2',
				_0: 'circle',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$markProperty, _p97._0))
			};
		case 'GeoshapeStyle':
			return {
				ctor: '_Tuple2',
				_0: 'geoshape',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$markProperty, _p97._0))
			};
		case 'LineStyle':
			return {
				ctor: '_Tuple2',
				_0: 'line',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$markProperty, _p97._0))
			};
		case 'PointStyle':
			return {
				ctor: '_Tuple2',
				_0: 'point',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$markProperty, _p97._0))
			};
		case 'RectStyle':
			return {
				ctor: '_Tuple2',
				_0: 'rect',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$markProperty, _p97._0))
			};
		case 'RuleStyle':
			return {
				ctor: '_Tuple2',
				_0: 'rule',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$markProperty, _p97._0))
			};
		case 'SquareStyle':
			return {
				ctor: '_Tuple2',
				_0: 'square',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$markProperty, _p97._0))
			};
		case 'TextStyle':
			return {
				ctor: '_Tuple2',
				_0: 'text',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$markProperty, _p97._0))
			};
		case 'TickStyle':
			return {
				ctor: '_Tuple2',
				_0: 'tick',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$markProperty, _p97._0))
			};
		case 'TitleStyle':
			return {
				ctor: '_Tuple2',
				_0: 'title',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$titleConfigSpec, _p97._0))
			};
		case 'NamedStyle':
			return {
				ctor: '_Tuple2',
				_0: 'style',
				_1: _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: _p97._0,
							_1: _elm_lang$core$Json_Encode$object(
								A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$markProperty, _p97._1))
						},
						_1: {ctor: '[]'}
					})
			};
		case 'Scale':
			return {
				ctor: '_Tuple2',
				_0: 'scale',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$scaleConfigProperty, _p97._0))
			};
		case 'Stack':
			return _gicentre$elm_vega$VegaLite$stackProperty(_p97._0);
		case 'Range':
			return {
				ctor: '_Tuple2',
				_0: 'range',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$rangeConfigProperty, _p97._0))
			};
		case 'SelectionStyle':
			var selProp = function (_p98) {
				var _p99 = _p98;
				return {
					ctor: '_Tuple2',
					_0: _gicentre$elm_vega$VegaLite$selectionLabel(_p99._0),
					_1: _elm_lang$core$Json_Encode$object(
						A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$selectionProperty, _p99._1))
				};
			};
			return {
				ctor: '_Tuple2',
				_0: 'selection',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, selProp, _p97._0))
			};
		case 'View':
			return {
				ctor: '_Tuple2',
				_0: 'view',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$viewConfigProperty, _p97._0))
			};
		default:
			return {
				ctor: '_Tuple2',
				_0: 'trail',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$markProperty, _p97._0))
			};
	}
};
var _gicentre$elm_vega$VegaLite$transpose = function (ll) {
	transpose:
	while (true) {
		var _p100 = ll;
		if (_p100.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			if (_p100._0.ctor === '[]') {
				var _v88 = _p100._1;
				ll = _v88;
				continue transpose;
			} else {
				var _p101 = _p100._1;
				var tails = A2(_elm_lang$core$List$filterMap, _elm_lang$core$List$tail, _p101);
				var heads = A2(_elm_lang$core$List$filterMap, _elm_lang$core$List$head, _p101);
				return {
					ctor: '::',
					_0: {ctor: '::', _0: _p100._0._0, _1: heads},
					_1: _gicentre$elm_vega$VegaLite$transpose(
						{ctor: '::', _0: _p100._0._1, _1: tails})
				};
			}
		}
	}
};
var _gicentre$elm_vega$VegaLite$windowAs = F3(
	function (fName, ws, wProps) {
		return F2(
			function (x, y) {
				return {ctor: '::', _0: x, _1: y};
			})(
			{
				ctor: '_Tuple2',
				_0: 'windowAs',
				_1: _elm_lang$core$Json_Encode$list(
					{
						ctor: '::',
						_0: _elm_lang$core$Json_Encode$object(
							{
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: 'as',
									_1: _elm_lang$core$Json_Encode$string(fName)
								},
								_1: A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$windowAsProperty, ws)
							}),
						_1: {
							ctor: '::',
							_0: A2(_gicentre$elm_vega$VegaLite$windowPropertySpec, 'frame', wProps),
							_1: {
								ctor: '::',
								_0: A2(_gicentre$elm_vega$VegaLite$windowPropertySpec, 'ignorePeers', wProps),
								_1: {
									ctor: '::',
									_0: A2(_gicentre$elm_vega$VegaLite$windowPropertySpec, 'groupby', wProps),
									_1: {
										ctor: '::',
										_0: A2(_gicentre$elm_vega$VegaLite$windowPropertySpec, 'sort', wProps),
										_1: {ctor: '[]'}
									}
								}
							}
						}
					})
			});
	});
var _gicentre$elm_vega$VegaLite$toVegaLite = function (spec) {
	return _elm_lang$core$Json_Encode$object(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: '$schema',
				_1: _elm_lang$core$Json_Encode$string('https://vega.github.io/schema/vega-lite/v2.json')
			},
			_1: A2(
				_elm_lang$core$List$map,
				function (_p102) {
					var _p103 = _p102;
					return {
						ctor: '_Tuple2',
						_0: _gicentre$elm_vega$VegaLite$vlPropertyLabel(_p103._0),
						_1: _p103._1
					};
				},
				spec)
		});
};
var _gicentre$elm_vega$VegaLite$timeUnitAs = F3(
	function (tu, field, label) {
		return F2(
			function (x, y) {
				return {ctor: '::', _0: x, _1: y};
			})(
			{
				ctor: '_Tuple2',
				_0: 'timeUnit',
				_1: _elm_lang$core$Json_Encode$list(
					{
						ctor: '::',
						_0: _elm_lang$core$Json_Encode$string(
							_gicentre$elm_vega$VegaLite$timeUnitLabel(tu)),
						_1: {
							ctor: '::',
							_0: _elm_lang$core$Json_Encode$string(field),
							_1: {
								ctor: '::',
								_0: _elm_lang$core$Json_Encode$string(label),
								_1: {ctor: '[]'}
							}
						}
					})
			});
	});
var _gicentre$elm_vega$VegaLite$select = F3(
	function (name, sType, options) {
		var selProps = {
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'type',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$selectionLabel(sType))
			},
			_1: A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$selectionProperty, options)
		};
		return F2(
			function (x, y) {
				return {ctor: '::', _0: x, _1: y};
			})(
			{
				ctor: '_Tuple2',
				_0: name,
				_1: _elm_lang$core$Json_Encode$object(selProps)
			});
	});
var _gicentre$elm_vega$VegaLite$resolution = function (res) {
	return F2(
		function (x, y) {
			return {ctor: '::', _0: x, _1: y};
		})(
		_gicentre$elm_vega$VegaLite$resolveProperty(res));
};
var _gicentre$elm_vega$VegaLite$opAs = F3(
	function (op, field, label) {
		return _elm_lang$core$Json_Encode$object(
			{
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'op',
					_1: _elm_lang$core$Json_Encode$string(
						_gicentre$elm_vega$VegaLite$operationLabel(op))
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'field',
						_1: _elm_lang$core$Json_Encode$string(field)
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'as',
							_1: _elm_lang$core$Json_Encode$string(label)
						},
						_1: {ctor: '[]'}
					}
				}
			});
	});
var _gicentre$elm_vega$VegaLite$lookupAs = F4(
	function (key1, _p104, key2, asName) {
		var _p105 = _p104;
		return F2(
			function (x, y) {
				return {ctor: '::', _0: x, _1: y};
			})(
			{
				ctor: '_Tuple2',
				_0: 'lookupAs',
				_1: _elm_lang$core$Json_Encode$list(
					{
						ctor: '::',
						_0: _elm_lang$core$Json_Encode$string(key1),
						_1: {
							ctor: '::',
							_0: _p105._1,
							_1: {
								ctor: '::',
								_0: _elm_lang$core$Json_Encode$string(key2),
								_1: {
									ctor: '::',
									_0: _elm_lang$core$Json_Encode$string(asName),
									_1: {ctor: '[]'}
								}
							}
						}
					})
			});
	});
var _gicentre$elm_vega$VegaLite$lookup = F4(
	function (key1, _p106, key2, fields) {
		var _p107 = _p106;
		return F2(
			function (x, y) {
				return {ctor: '::', _0: x, _1: y};
			})(
			{
				ctor: '_Tuple2',
				_0: 'lookup',
				_1: _elm_lang$core$Json_Encode$list(
					{
						ctor: '::',
						_0: _elm_lang$core$Json_Encode$string(key1),
						_1: {
							ctor: '::',
							_0: _p107._1,
							_1: {
								ctor: '::',
								_0: _elm_lang$core$Json_Encode$string(key2),
								_1: {
									ctor: '::',
									_0: _elm_lang$core$Json_Encode$list(
										A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, fields)),
									_1: {ctor: '[]'}
								}
							}
						}
					})
			});
	});
var _gicentre$elm_vega$VegaLite$geometry = F2(
	function (gType, properties) {
		return _elm_lang$core$Native_Utils.eq(
			properties,
			{ctor: '[]'}) ? _elm_lang$core$Json_Encode$object(
			{
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'type',
					_1: _elm_lang$core$Json_Encode$string('Feature')
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'geometry',
						_1: _gicentre$elm_vega$VegaLite$geometryTypeSpec(gType)
					},
					_1: {ctor: '[]'}
				}
			}) : _elm_lang$core$Json_Encode$object(
			{
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'type',
					_1: _elm_lang$core$Json_Encode$string('Feature')
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'geometry',
						_1: _gicentre$elm_vega$VegaLite$geometryTypeSpec(gType)
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'properties',
							_1: _elm_lang$core$Json_Encode$object(
								A2(
									_elm_lang$core$List$map,
									function (_p108) {
										var _p109 = _p108;
										return {
											ctor: '_Tuple2',
											_0: _p109._0,
											_1: _gicentre$elm_vega$VegaLite$dataValueSpec(_p109._1)
										};
									},
									properties))
						},
						_1: {ctor: '[]'}
					}
				}
			});
	});
var _gicentre$elm_vega$VegaLite$geometryCollection = function (geoms) {
	return _elm_lang$core$Json_Encode$object(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'type',
				_1: _elm_lang$core$Json_Encode$string('GeometryCollection')
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'geometries',
					_1: _elm_lang$core$Json_Encode$list(geoms)
				},
				_1: {ctor: '[]'}
			}
		});
};
var _gicentre$elm_vega$VegaLite$geoFeatureCollection = function (geoms) {
	return _elm_lang$core$Json_Encode$object(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'type',
				_1: _elm_lang$core$Json_Encode$string('FeatureCollection')
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'features',
					_1: _elm_lang$core$Json_Encode$list(geoms)
				},
				_1: {ctor: '[]'}
			}
		});
};
var _gicentre$elm_vega$VegaLite$filter = function (f) {
	var _p110 = f;
	switch (_p110.ctor) {
		case 'FExpr':
			return F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				})(
				{
					ctor: '_Tuple2',
					_0: 'filter',
					_1: _elm_lang$core$Json_Encode$string(_p110._0)
				});
		case 'FCompose':
			return F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				})(
				{
					ctor: '_Tuple2',
					_0: 'filter',
					_1: _gicentre$elm_vega$VegaLite$booleanOpSpec(_p110._0)
				});
		case 'FEqual':
			return F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				})(
				{
					ctor: '_Tuple2',
					_0: 'filter',
					_1: _elm_lang$core$Json_Encode$object(
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'field',
								_1: _elm_lang$core$Json_Encode$string(_p110._0)
							},
							_1: {
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: 'equal',
									_1: _gicentre$elm_vega$VegaLite$dataValueSpec(_p110._1)
								},
								_1: {ctor: '[]'}
							}
						})
				});
		case 'FSelection':
			return F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				})(
				{
					ctor: '_Tuple2',
					_0: 'filter',
					_1: _elm_lang$core$Json_Encode$object(
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'selection',
								_1: _elm_lang$core$Json_Encode$string(_p110._0)
							},
							_1: {ctor: '[]'}
						})
				});
		case 'FRange':
			var values = function () {
				var _p111 = _p110._1;
				if (_p111.ctor === 'NumberRange') {
					return _elm_lang$core$Json_Encode$list(
						{
							ctor: '::',
							_0: _elm_lang$core$Json_Encode$float(_p111._0),
							_1: {
								ctor: '::',
								_0: _elm_lang$core$Json_Encode$float(_p111._1),
								_1: {ctor: '[]'}
							}
						});
				} else {
					if (_p111._0.ctor === '[]') {
						if (_p111._1.ctor === '[]') {
							return _elm_lang$core$Json_Encode$list(
								{
									ctor: '::',
									_0: _elm_lang$core$Json_Encode$null,
									_1: {
										ctor: '::',
										_0: _elm_lang$core$Json_Encode$null,
										_1: {ctor: '[]'}
									}
								});
						} else {
							return _elm_lang$core$Json_Encode$list(
								{
									ctor: '::',
									_0: _elm_lang$core$Json_Encode$null,
									_1: {
										ctor: '::',
										_0: _elm_lang$core$Json_Encode$object(
											A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$dateTimeProperty, _p111._1)),
										_1: {ctor: '[]'}
									}
								});
						}
					} else {
						if (_p111._1.ctor === '[]') {
							return _elm_lang$core$Json_Encode$list(
								{
									ctor: '::',
									_0: _elm_lang$core$Json_Encode$object(
										A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$dateTimeProperty, _p111._0)),
									_1: {
										ctor: '::',
										_0: _elm_lang$core$Json_Encode$null,
										_1: {ctor: '[]'}
									}
								});
						} else {
							return _elm_lang$core$Json_Encode$list(
								{
									ctor: '::',
									_0: _elm_lang$core$Json_Encode$object(
										A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$dateTimeProperty, _p111._0)),
									_1: {
										ctor: '::',
										_0: _elm_lang$core$Json_Encode$object(
											A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$dateTimeProperty, _p111._1)),
										_1: {ctor: '[]'}
									}
								});
						}
					}
				}
			}();
			return F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				})(
				{
					ctor: '_Tuple2',
					_0: 'filter',
					_1: _elm_lang$core$Json_Encode$object(
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'field',
								_1: _elm_lang$core$Json_Encode$string(_p110._0)
							},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'range', _1: values},
								_1: {ctor: '[]'}
							}
						})
				});
		default:
			var values = function () {
				var _p112 = _p110._1;
				switch (_p112.ctor) {
					case 'Numbers':
						return _elm_lang$core$Json_Encode$list(
							A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$float, _p112._0));
					case 'DateTimes':
						return _elm_lang$core$Json_Encode$list(
							A2(
								_elm_lang$core$List$map,
								function (dt) {
									return _elm_lang$core$Json_Encode$object(
										A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$dateTimeProperty, dt));
								},
								_p112._0));
					case 'Strings':
						return _elm_lang$core$Json_Encode$list(
							A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p112._0));
					default:
						return _elm_lang$core$Json_Encode$list(
							A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$bool, _p112._0));
				}
			}();
			return F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				})(
				{
					ctor: '_Tuple2',
					_0: 'filter',
					_1: _elm_lang$core$Json_Encode$object(
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'field',
								_1: _elm_lang$core$Json_Encode$string(_p110._0)
							},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'oneOf', _1: values},
								_1: {ctor: '[]'}
							}
						})
				});
	}
};
var _gicentre$elm_vega$VegaLite$dataRow = function (row) {
	return F2(
		function (x, y) {
			return {ctor: '::', _0: x, _1: y};
		})(
		_elm_lang$core$Json_Encode$object(
			A2(
				_elm_lang$core$List$map,
				function (_p113) {
					var _p114 = _p113;
					return {
						ctor: '_Tuple2',
						_0: _p114._0,
						_1: _gicentre$elm_vega$VegaLite$dataValueSpec(_p114._1)
					};
				},
				row)));
};
var _gicentre$elm_vega$VegaLite$dataColumn = F2(
	function (colName, data) {
		var _p115 = data;
		switch (_p115.ctor) {
			case 'Numbers':
				return F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					})(
					A2(
						_elm_lang$core$List$map,
						function (x) {
							return {
								ctor: '_Tuple2',
								_0: colName,
								_1: _elm_lang$core$Json_Encode$float(x)
							};
						},
						_p115._0));
			case 'Strings':
				return F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					})(
					A2(
						_elm_lang$core$List$map,
						function (s) {
							return {
								ctor: '_Tuple2',
								_0: colName,
								_1: _elm_lang$core$Json_Encode$string(s)
							};
						},
						_p115._0));
			case 'DateTimes':
				return F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					})(
					A2(
						_elm_lang$core$List$map,
						function (dts) {
							return {
								ctor: '_Tuple2',
								_0: colName,
								_1: _elm_lang$core$Json_Encode$object(
									A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$dateTimeProperty, dts))
							};
						},
						_p115._0));
			default:
				return F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					})(
					A2(
						_elm_lang$core$List$map,
						function (b) {
							return {
								ctor: '_Tuple2',
								_0: colName,
								_1: _elm_lang$core$Json_Encode$bool(b)
							};
						},
						_p115._0));
		}
	});
var _gicentre$elm_vega$VegaLite$configuration = function (cfg) {
	return F2(
		function (x, y) {
			return {ctor: '::', _0: x, _1: y};
		})(
		_gicentre$elm_vega$VegaLite$configProperty(cfg));
};
var _gicentre$elm_vega$VegaLite$combineSpecs = function (specs) {
	return _elm_lang$core$Json_Encode$object(specs);
};
var _gicentre$elm_vega$VegaLite$calculateAs = F2(
	function (expr, label) {
		return F2(
			function (x, y) {
				return {ctor: '::', _0: x, _1: y};
			})(
			{
				ctor: '_Tuple2',
				_0: 'calculate',
				_1: _elm_lang$core$Json_Encode$list(
					{
						ctor: '::',
						_0: _elm_lang$core$Json_Encode$string(expr),
						_1: {
							ctor: '::',
							_0: _elm_lang$core$Json_Encode$string(label),
							_1: {ctor: '[]'}
						}
					})
			});
	});
var _gicentre$elm_vega$VegaLite$binAs = F3(
	function (bProps, field, label) {
		return _elm_lang$core$Native_Utils.eq(
			bProps,
			{ctor: '[]'}) ? F2(
			function (x, y) {
				return {ctor: '::', _0: x, _1: y};
			})(
			{
				ctor: '_Tuple2',
				_0: 'bin',
				_1: _elm_lang$core$Json_Encode$list(
					{
						ctor: '::',
						_0: _elm_lang$core$Json_Encode$bool(true),
						_1: {
							ctor: '::',
							_0: _elm_lang$core$Json_Encode$string(field),
							_1: {
								ctor: '::',
								_0: _elm_lang$core$Json_Encode$string(label),
								_1: {ctor: '[]'}
							}
						}
					})
			}) : F2(
			function (x, y) {
				return {ctor: '::', _0: x, _1: y};
			})(
			{
				ctor: '_Tuple2',
				_0: 'bin',
				_1: _elm_lang$core$Json_Encode$list(
					{
						ctor: '::',
						_0: _elm_lang$core$Json_Encode$object(
							A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$binProperty, bProps)),
						_1: {
							ctor: '::',
							_0: _elm_lang$core$Json_Encode$string(field),
							_1: {
								ctor: '::',
								_0: _elm_lang$core$Json_Encode$string(label),
								_1: {ctor: '[]'}
							}
						}
					})
			});
	});
var _gicentre$elm_vega$VegaLite$bin = function (bProps) {
	return _elm_lang$core$Native_Utils.eq(
		bProps,
		{ctor: '[]'}) ? {
		ctor: '_Tuple2',
		_0: 'bin',
		_1: _elm_lang$core$Json_Encode$bool(true)
	} : {
		ctor: '_Tuple2',
		_0: 'bin',
		_1: _elm_lang$core$Json_Encode$object(
			A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$binProperty, bProps))
	};
};
var _gicentre$elm_vega$VegaLite$detailChannelProperty = function (field) {
	var _p116 = field;
	switch (_p116.ctor) {
		case 'DName':
			return {
				ctor: '_Tuple2',
				_0: 'field',
				_1: _elm_lang$core$Json_Encode$string(_p116._0)
			};
		case 'DmType':
			return {
				ctor: '_Tuple2',
				_0: 'type',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$measurementLabel(_p116._0))
			};
		case 'DBin':
			return _gicentre$elm_vega$VegaLite$bin(_p116._0);
		case 'DTimeUnit':
			return {
				ctor: '_Tuple2',
				_0: 'timeUnit',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$timeUnitLabel(_p116._0))
			};
		default:
			return {
				ctor: '_Tuple2',
				_0: 'aggregate',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$operationLabel(_p116._0))
			};
	}
};
var _gicentre$elm_vega$VegaLite$detail = function (detailProps) {
	return F2(
		function (x, y) {
			return {ctor: '::', _0: x, _1: y};
		})(
		{
			ctor: '_Tuple2',
			_0: 'detail',
			_1: _elm_lang$core$Json_Encode$object(
				A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$detailChannelProperty, detailProps))
		});
};
var _gicentre$elm_vega$VegaLite$facetChannelProperty = function (fMap) {
	var _p117 = fMap;
	switch (_p117.ctor) {
		case 'FName':
			return {
				ctor: '_Tuple2',
				_0: 'field',
				_1: _elm_lang$core$Json_Encode$string(_p117._0)
			};
		case 'FmType':
			return {
				ctor: '_Tuple2',
				_0: 'type',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$measurementLabel(_p117._0))
			};
		case 'FBin':
			return _gicentre$elm_vega$VegaLite$bin(_p117._0);
		case 'FAggregate':
			return {
				ctor: '_Tuple2',
				_0: 'aggregate',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$operationLabel(_p117._0))
			};
		case 'FTimeUnit':
			return {
				ctor: '_Tuple2',
				_0: 'timeUnit',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$timeUnitLabel(_p117._0))
			};
		default:
			return {
				ctor: '_Tuple2',
				_0: 'header',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$headerProperty, _p117._0))
			};
	}
};
var _gicentre$elm_vega$VegaLite$column = function (fFields) {
	return F2(
		function (x, y) {
			return {ctor: '::', _0: x, _1: y};
		})(
		{
			ctor: '_Tuple2',
			_0: 'column',
			_1: _elm_lang$core$Json_Encode$object(
				A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$facetChannelProperty, fFields))
		});
};
var _gicentre$elm_vega$VegaLite$row = function (fFields) {
	return F2(
		function (x, y) {
			return {ctor: '::', _0: x, _1: y};
		})(
		{
			ctor: '_Tuple2',
			_0: 'row',
			_1: _elm_lang$core$Json_Encode$object(
				A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$facetChannelProperty, fFields))
		});
};
var _gicentre$elm_vega$VegaLite$facetMappingProperty = function (fMap) {
	var _p118 = fMap;
	if (_p118.ctor === 'RowBy') {
		return {
			ctor: '_Tuple2',
			_0: 'row',
			_1: _elm_lang$core$Json_Encode$object(
				A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$facetChannelProperty, _p118._0))
		};
	} else {
		return {
			ctor: '_Tuple2',
			_0: 'column',
			_1: _elm_lang$core$Json_Encode$object(
				A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$facetChannelProperty, _p118._0))
		};
	}
};
var _gicentre$elm_vega$VegaLite$hyperlinkChannelProperty = function (field) {
	var _p119 = field;
	switch (_p119.ctor) {
		case 'HName':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'field',
					_1: _elm_lang$core$Json_Encode$string(_p119._0)
				},
				_1: {ctor: '[]'}
			};
		case 'HRepeat':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'field',
					_1: _elm_lang$core$Json_Encode$object(
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'repeat',
								_1: _elm_lang$core$Json_Encode$string(
									_gicentre$elm_vega$VegaLite$arrangementLabel(_p119._0))
							},
							_1: {ctor: '[]'}
						})
				},
				_1: {ctor: '[]'}
			};
		case 'HmType':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'type',
					_1: _elm_lang$core$Json_Encode$string(
						_gicentre$elm_vega$VegaLite$measurementLabel(_p119._0))
				},
				_1: {ctor: '[]'}
			};
		case 'HBin':
			return {
				ctor: '::',
				_0: _gicentre$elm_vega$VegaLite$bin(_p119._0),
				_1: {ctor: '[]'}
			};
		case 'HSelectionCondition':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'condition',
					_1: _elm_lang$core$Json_Encode$object(
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'selection',
								_1: _gicentre$elm_vega$VegaLite$booleanOpSpec(_p119._0)
							},
							_1: A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$hyperlinkChannelProperty, _p119._1)
						})
				},
				_1: A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$hyperlinkChannelProperty, _p119._2)
			};
		case 'HDataCondition':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'condition',
					_1: _elm_lang$core$Json_Encode$object(
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'test',
								_1: _gicentre$elm_vega$VegaLite$booleanOpSpec(_p119._0)
							},
							_1: A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$hyperlinkChannelProperty, _p119._1)
						})
				},
				_1: A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$hyperlinkChannelProperty, _p119._2)
			};
		case 'HTimeUnit':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'timeUnit',
					_1: _elm_lang$core$Json_Encode$string(
						_gicentre$elm_vega$VegaLite$timeUnitLabel(_p119._0))
				},
				_1: {ctor: '[]'}
			};
		case 'HAggregate':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'aggregate',
					_1: _elm_lang$core$Json_Encode$string(
						_gicentre$elm_vega$VegaLite$operationLabel(_p119._0))
				},
				_1: {ctor: '[]'}
			};
		default:
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'value',
					_1: _elm_lang$core$Json_Encode$string(_p119._0)
				},
				_1: {ctor: '[]'}
			};
	}
};
var _gicentre$elm_vega$VegaLite$hyperlink = function (hyperProps) {
	return F2(
		function (x, y) {
			return {ctor: '::', _0: x, _1: y};
		})(
		{
			ctor: '_Tuple2',
			_0: 'href',
			_1: _elm_lang$core$Json_Encode$object(
				A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$hyperlinkChannelProperty, hyperProps))
		});
};
var _gicentre$elm_vega$VegaLite$markChannelProperty = function (field) {
	var _p120 = field;
	switch (_p120.ctor) {
		case 'MName':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'field',
					_1: _elm_lang$core$Json_Encode$string(_p120._0)
				},
				_1: {ctor: '[]'}
			};
		case 'MRepeat':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'field',
					_1: _elm_lang$core$Json_Encode$object(
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'repeat',
								_1: _elm_lang$core$Json_Encode$string(
									_gicentre$elm_vega$VegaLite$arrangementLabel(_p120._0))
							},
							_1: {ctor: '[]'}
						})
				},
				_1: {ctor: '[]'}
			};
		case 'MmType':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'type',
					_1: _elm_lang$core$Json_Encode$string(
						_gicentre$elm_vega$VegaLite$measurementLabel(_p120._0))
				},
				_1: {ctor: '[]'}
			};
		case 'MScale':
			var _p121 = _p120._0;
			return _elm_lang$core$Native_Utils.eq(
				_p121,
				{ctor: '[]'}) ? {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'scale', _1: _elm_lang$core$Json_Encode$null},
				_1: {ctor: '[]'}
			} : {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'scale',
					_1: _elm_lang$core$Json_Encode$object(
						A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$scaleProperty, _p121))
				},
				_1: {ctor: '[]'}
			};
		case 'MLegend':
			var _p122 = _p120._0;
			return _elm_lang$core$Native_Utils.eq(
				_p122,
				{ctor: '[]'}) ? {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'legend', _1: _elm_lang$core$Json_Encode$null},
				_1: {ctor: '[]'}
			} : {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'legend',
					_1: _elm_lang$core$Json_Encode$object(
						A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$legendProperty, _p122))
				},
				_1: {ctor: '[]'}
			};
		case 'MBin':
			return {
				ctor: '::',
				_0: _gicentre$elm_vega$VegaLite$bin(_p120._0),
				_1: {ctor: '[]'}
			};
		case 'MSelectionCondition':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'condition',
					_1: _elm_lang$core$Json_Encode$object(
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'selection',
								_1: _gicentre$elm_vega$VegaLite$booleanOpSpec(_p120._0)
							},
							_1: A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$markChannelProperty, _p120._1)
						})
				},
				_1: A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$markChannelProperty, _p120._2)
			};
		case 'MDataCondition':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'condition',
					_1: _elm_lang$core$Json_Encode$object(
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'test',
								_1: _gicentre$elm_vega$VegaLite$booleanOpSpec(_p120._0)
							},
							_1: A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$markChannelProperty, _p120._1)
						})
				},
				_1: A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$markChannelProperty, _p120._2)
			};
		case 'MTimeUnit':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'timeUnit',
					_1: _elm_lang$core$Json_Encode$string(
						_gicentre$elm_vega$VegaLite$timeUnitLabel(_p120._0))
				},
				_1: {ctor: '[]'}
			};
		case 'MAggregate':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'aggregate',
					_1: _elm_lang$core$Json_Encode$string(
						_gicentre$elm_vega$VegaLite$operationLabel(_p120._0))
				},
				_1: {ctor: '[]'}
			};
		case 'MPath':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'value',
					_1: _elm_lang$core$Json_Encode$string(_p120._0)
				},
				_1: {ctor: '[]'}
			};
		case 'MNumber':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'value',
					_1: _elm_lang$core$Json_Encode$float(_p120._0)
				},
				_1: {ctor: '[]'}
			};
		case 'MString':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'value',
					_1: _elm_lang$core$Json_Encode$string(_p120._0)
				},
				_1: {ctor: '[]'}
			};
		default:
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'value',
					_1: _elm_lang$core$Json_Encode$bool(_p120._0)
				},
				_1: {ctor: '[]'}
			};
	}
};
var _gicentre$elm_vega$VegaLite$color = function (markProps) {
	return F2(
		function (x, y) {
			return {ctor: '::', _0: x, _1: y};
		})(
		{
			ctor: '_Tuple2',
			_0: 'color',
			_1: _elm_lang$core$Json_Encode$object(
				A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$markChannelProperty, markProps))
		});
};
var _gicentre$elm_vega$VegaLite$fill = function (markProps) {
	return F2(
		function (x, y) {
			return {ctor: '::', _0: x, _1: y};
		})(
		{
			ctor: '_Tuple2',
			_0: 'fill',
			_1: _elm_lang$core$Json_Encode$object(
				A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$markChannelProperty, markProps))
		});
};
var _gicentre$elm_vega$VegaLite$opacity = function (markProps) {
	return F2(
		function (x, y) {
			return {ctor: '::', _0: x, _1: y};
		})(
		{
			ctor: '_Tuple2',
			_0: 'opacity',
			_1: _elm_lang$core$Json_Encode$object(
				A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$markChannelProperty, markProps))
		});
};
var _gicentre$elm_vega$VegaLite$shape = function (markProps) {
	return F2(
		function (x, y) {
			return {ctor: '::', _0: x, _1: y};
		})(
		{
			ctor: '_Tuple2',
			_0: 'shape',
			_1: _elm_lang$core$Json_Encode$object(
				A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$markChannelProperty, markProps))
		});
};
var _gicentre$elm_vega$VegaLite$size = function (markProps) {
	return F2(
		function (x, y) {
			return {ctor: '::', _0: x, _1: y};
		})(
		{
			ctor: '_Tuple2',
			_0: 'size',
			_1: _elm_lang$core$Json_Encode$object(
				A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$markChannelProperty, markProps))
		});
};
var _gicentre$elm_vega$VegaLite$stroke = function (markProps) {
	return F2(
		function (x, y) {
			return {ctor: '::', _0: x, _1: y};
		})(
		{
			ctor: '_Tuple2',
			_0: 'stroke',
			_1: _elm_lang$core$Json_Encode$object(
				A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$markChannelProperty, markProps))
		});
};
var _gicentre$elm_vega$VegaLite$orderChannelProperty = function (oDef) {
	var _p123 = oDef;
	switch (_p123.ctor) {
		case 'OName':
			return {
				ctor: '_Tuple2',
				_0: 'field',
				_1: _elm_lang$core$Json_Encode$string(_p123._0)
			};
		case 'ORepeat':
			return {
				ctor: '_Tuple2',
				_0: 'field',
				_1: _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'repeat',
							_1: _elm_lang$core$Json_Encode$string(
								_gicentre$elm_vega$VegaLite$arrangementLabel(_p123._0))
						},
						_1: {ctor: '[]'}
					})
			};
		case 'OmType':
			return {
				ctor: '_Tuple2',
				_0: 'type',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$measurementLabel(_p123._0))
			};
		case 'OBin':
			return _gicentre$elm_vega$VegaLite$bin(_p123._0);
		case 'OAggregate':
			return {
				ctor: '_Tuple2',
				_0: 'aggregate',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$operationLabel(_p123._0))
			};
		case 'OTimeUnit':
			return {
				ctor: '_Tuple2',
				_0: 'timeUnit',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$timeUnitLabel(_p123._0))
			};
		default:
			var _p125 = _p123._0;
			var _p124 = _p125;
			_v104_4:
			do {
				if (_p124.ctor === '[]') {
					return {ctor: '_Tuple2', _0: 'sort', _1: _elm_lang$core$Json_Encode$null};
				} else {
					if (_p124._1.ctor === '[]') {
						switch (_p124._0.ctor) {
							case 'Ascending':
								return {
									ctor: '_Tuple2',
									_0: 'sort',
									_1: _elm_lang$core$Json_Encode$string('ascending')
								};
							case 'Descending':
								return {
									ctor: '_Tuple2',
									_0: 'sort',
									_1: _elm_lang$core$Json_Encode$string('descending')
								};
							case 'CustomSort':
								return {
									ctor: '_Tuple2',
									_0: 'sort',
									_1: _elm_lang$core$Json_Encode$list(
										_gicentre$elm_vega$VegaLite$dataValuesSpecs(_p124._0._0))
								};
							default:
								break _v104_4;
						}
					} else {
						break _v104_4;
					}
				}
			} while(false);
			return {
				ctor: '_Tuple2',
				_0: 'sort',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$sortProperty, _p125))
			};
	}
};
var _gicentre$elm_vega$VegaLite$order = function (oDefs) {
	return F2(
		function (x, y) {
			return {ctor: '::', _0: x, _1: y};
		})(
		{
			ctor: '_Tuple2',
			_0: 'order',
			_1: _elm_lang$core$Json_Encode$object(
				A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$orderChannelProperty, oDefs))
		});
};
var _gicentre$elm_vega$VegaLite$positionChannelProperty = function (pDef) {
	var _p126 = pDef;
	switch (_p126.ctor) {
		case 'PName':
			return {
				ctor: '_Tuple2',
				_0: 'field',
				_1: _elm_lang$core$Json_Encode$string(_p126._0)
			};
		case 'PmType':
			return {
				ctor: '_Tuple2',
				_0: 'type',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$measurementLabel(_p126._0))
			};
		case 'PBin':
			return _gicentre$elm_vega$VegaLite$bin(_p126._0);
		case 'PAggregate':
			return {
				ctor: '_Tuple2',
				_0: 'aggregate',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$operationLabel(_p126._0))
			};
		case 'PTimeUnit':
			return {
				ctor: '_Tuple2',
				_0: 'timeUnit',
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$timeUnitLabel(_p126._0))
			};
		case 'PSort':
			var _p128 = _p126._0;
			var _p127 = _p128;
			_v106_4:
			do {
				if (_p127.ctor === '[]') {
					return {ctor: '_Tuple2', _0: 'sort', _1: _elm_lang$core$Json_Encode$null};
				} else {
					if (_p127._1.ctor === '[]') {
						switch (_p127._0.ctor) {
							case 'Ascending':
								return {
									ctor: '_Tuple2',
									_0: 'sort',
									_1: _elm_lang$core$Json_Encode$string('ascending')
								};
							case 'Descending':
								return {
									ctor: '_Tuple2',
									_0: 'sort',
									_1: _elm_lang$core$Json_Encode$string('descending')
								};
							case 'CustomSort':
								return {
									ctor: '_Tuple2',
									_0: 'sort',
									_1: _elm_lang$core$Json_Encode$list(
										_gicentre$elm_vega$VegaLite$dataValuesSpecs(_p127._0._0))
								};
							default:
								break _v106_4;
						}
					} else {
						break _v106_4;
					}
				}
			} while(false);
			return {
				ctor: '_Tuple2',
				_0: 'sort',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$sortProperty, _p128))
			};
		case 'PScale':
			var _p129 = _p126._0;
			return _elm_lang$core$Native_Utils.eq(
				_p129,
				{ctor: '[]'}) ? {ctor: '_Tuple2', _0: 'scale', _1: _elm_lang$core$Json_Encode$null} : {
				ctor: '_Tuple2',
				_0: 'scale',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$scaleProperty, _p129))
			};
		case 'PAxis':
			var _p130 = _p126._0;
			return _elm_lang$core$Native_Utils.eq(
				_p130,
				{ctor: '[]'}) ? {ctor: '_Tuple2', _0: 'axis', _1: _elm_lang$core$Json_Encode$null} : {
				ctor: '_Tuple2',
				_0: 'axis',
				_1: _elm_lang$core$Json_Encode$object(
					A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$axisProperty, _p130))
			};
		case 'PStack':
			return _gicentre$elm_vega$VegaLite$stackProperty(_p126._0);
		default:
			return {
				ctor: '_Tuple2',
				_0: 'field',
				_1: _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'repeat',
							_1: _elm_lang$core$Json_Encode$string(
								_gicentre$elm_vega$VegaLite$arrangementLabel(_p126._0))
						},
						_1: {ctor: '[]'}
					})
			};
	}
};
var _gicentre$elm_vega$VegaLite$textChannelProperty = function (tDef) {
	var _p131 = tDef;
	switch (_p131.ctor) {
		case 'TName':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'field',
					_1: _elm_lang$core$Json_Encode$string(_p131._0)
				},
				_1: {ctor: '[]'}
			};
		case 'TRepeat':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'field',
					_1: _elm_lang$core$Json_Encode$object(
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'repeat',
								_1: _elm_lang$core$Json_Encode$string(
									_gicentre$elm_vega$VegaLite$arrangementLabel(_p131._0))
							},
							_1: {ctor: '[]'}
						})
				},
				_1: {ctor: '[]'}
			};
		case 'TmType':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'type',
					_1: _elm_lang$core$Json_Encode$string(
						_gicentre$elm_vega$VegaLite$measurementLabel(_p131._0))
				},
				_1: {ctor: '[]'}
			};
		case 'TBin':
			return {
				ctor: '::',
				_0: _gicentre$elm_vega$VegaLite$bin(_p131._0),
				_1: {ctor: '[]'}
			};
		case 'TAggregate':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'aggregate',
					_1: _elm_lang$core$Json_Encode$string(
						_gicentre$elm_vega$VegaLite$operationLabel(_p131._0))
				},
				_1: {ctor: '[]'}
			};
		case 'TTimeUnit':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'timeUnit',
					_1: _elm_lang$core$Json_Encode$string(
						_gicentre$elm_vega$VegaLite$timeUnitLabel(_p131._0))
				},
				_1: {ctor: '[]'}
			};
		case 'TFormat':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'format',
					_1: _elm_lang$core$Json_Encode$string(_p131._0)
				},
				_1: {ctor: '[]'}
			};
		case 'TSelectionCondition':
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'condition',
					_1: _elm_lang$core$Json_Encode$object(
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'selection',
								_1: _gicentre$elm_vega$VegaLite$booleanOpSpec(_p131._0)
							},
							_1: A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$textChannelProperty, _p131._1)
						})
				},
				_1: A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$textChannelProperty, _p131._2)
			};
		default:
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'condition',
					_1: _elm_lang$core$Json_Encode$object(
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'test',
								_1: _gicentre$elm_vega$VegaLite$booleanOpSpec(_p131._0)
							},
							_1: A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$textChannelProperty, _p131._1)
						})
				},
				_1: A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$textChannelProperty, _p131._2)
			};
	}
};
var _gicentre$elm_vega$VegaLite$text = function (tDefs) {
	return F2(
		function (x, y) {
			return {ctor: '::', _0: x, _1: y};
		})(
		{
			ctor: '_Tuple2',
			_0: 'text',
			_1: _elm_lang$core$Json_Encode$object(
				A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$textChannelProperty, tDefs))
		});
};
var _gicentre$elm_vega$VegaLite$tooltip = function (tDefs) {
	return F2(
		function (x, y) {
			return {ctor: '::', _0: x, _1: y};
		})(
		{
			ctor: '_Tuple2',
			_0: 'tooltip',
			_1: _elm_lang$core$Json_Encode$object(
				A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$textChannelProperty, tDefs))
		});
};
var _gicentre$elm_vega$VegaLite$tooltips = function (tDefss) {
	return F2(
		function (x, y) {
			return {ctor: '::', _0: x, _1: y};
		})(
		{
			ctor: '_Tuple2',
			_0: 'tooltip',
			_1: _elm_lang$core$Json_Encode$list(
				A2(
					_elm_lang$core$List$map,
					function (tDefs) {
						return _elm_lang$core$Json_Encode$object(
							A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$textChannelProperty, tDefs));
					},
					tDefss))
		});
};
var _gicentre$elm_vega$VegaLite$asSpec = function (specs) {
	return _elm_lang$core$Json_Encode$object(
		A2(
			_elm_lang$core$List$map,
			function (_p132) {
				var _p133 = _p132;
				return {
					ctor: '_Tuple2',
					_0: _gicentre$elm_vega$VegaLite$vlPropertyLabel(_p133._0),
					_1: _p133._1
				};
			},
			specs));
};
var _gicentre$elm_vega$VegaLite$aggregate = F2(
	function (ops, groups) {
		return F2(
			function (x, y) {
				return {ctor: '::', _0: x, _1: y};
			})(
			{
				ctor: '_Tuple2',
				_0: 'aggregate',
				_1: _elm_lang$core$Json_Encode$list(
					{
						ctor: '::',
						_0: _elm_lang$core$Json_Encode$list(ops),
						_1: {
							ctor: '::',
							_0: _elm_lang$core$Json_Encode$list(
								A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, groups)),
							_1: {ctor: '[]'}
						}
					})
			});
	});
var _gicentre$elm_vega$VegaLite$AEnd = {ctor: 'AEnd'};
var _gicentre$elm_vega$VegaLite$AMiddle = {ctor: 'AMiddle'};
var _gicentre$elm_vega$VegaLite$AStart = {ctor: 'AStart'};
var _gicentre$elm_vega$VegaLite$Row = {ctor: 'Row'};
var _gicentre$elm_vega$VegaLite$Column = {ctor: 'Column'};
var _gicentre$elm_vega$VegaLite$AResize = {ctor: 'AResize'};
var _gicentre$elm_vega$VegaLite$APadding = {ctor: 'APadding'};
var _gicentre$elm_vega$VegaLite$APad = {ctor: 'APad'};
var _gicentre$elm_vega$VegaLite$ANone = {ctor: 'ANone'};
var _gicentre$elm_vega$VegaLite$AFit = {ctor: 'AFit'};
var _gicentre$elm_vega$VegaLite$AContent = {ctor: 'AContent'};
var _gicentre$elm_vega$VegaLite$TitleY = function (a) {
	return {ctor: 'TitleY', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoTitleY = _gicentre$elm_vega$VegaLite$TitleY;
var _gicentre$elm_vega$VegaLite$TitleX = function (a) {
	return {ctor: 'TitleX', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoTitleX = _gicentre$elm_vega$VegaLite$TitleX;
var _gicentre$elm_vega$VegaLite$TitlePadding = function (a) {
	return {ctor: 'TitlePadding', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoTitlePadding = _gicentre$elm_vega$VegaLite$TitlePadding;
var _gicentre$elm_vega$VegaLite$TitleMaxLength = function (a) {
	return {ctor: 'TitleMaxLength', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoTitleMaxLength = _gicentre$elm_vega$VegaLite$TitleMaxLength;
var _gicentre$elm_vega$VegaLite$TitleLimit = function (a) {
	return {ctor: 'TitleLimit', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoTitleLimit = _gicentre$elm_vega$VegaLite$TitleLimit;
var _gicentre$elm_vega$VegaLite$TitleFontSize = function (a) {
	return {ctor: 'TitleFontSize', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoTitleFontSize = _gicentre$elm_vega$VegaLite$TitleFontSize;
var _gicentre$elm_vega$VegaLite$TitleFontWeight = function (a) {
	return {ctor: 'TitleFontWeight', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoTitleFontWeight = _gicentre$elm_vega$VegaLite$TitleFontWeight;
var _gicentre$elm_vega$VegaLite$TitleFont = function (a) {
	return {ctor: 'TitleFont', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoTitleFont = _gicentre$elm_vega$VegaLite$TitleFont;
var _gicentre$elm_vega$VegaLite$TitleColor = function (a) {
	return {ctor: 'TitleColor', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoTitleColor = _gicentre$elm_vega$VegaLite$TitleColor;
var _gicentre$elm_vega$VegaLite$TitleBaseline = function (a) {
	return {ctor: 'TitleBaseline', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoTitleBaseline = _gicentre$elm_vega$VegaLite$TitleBaseline;
var _gicentre$elm_vega$VegaLite$TitleAngle = function (a) {
	return {ctor: 'TitleAngle', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoTitleAngle = _gicentre$elm_vega$VegaLite$TitleAngle;
var _gicentre$elm_vega$VegaLite$TitleAlign = function (a) {
	return {ctor: 'TitleAlign', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoTitleAlign = _gicentre$elm_vega$VegaLite$TitleAlign;
var _gicentre$elm_vega$VegaLite$TickWidth = function (a) {
	return {ctor: 'TickWidth', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoTickWidth = _gicentre$elm_vega$VegaLite$TickWidth;
var _gicentre$elm_vega$VegaLite$TickSize = function (a) {
	return {ctor: 'TickSize', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoTickSize = _gicentre$elm_vega$VegaLite$TickSize;
var _gicentre$elm_vega$VegaLite$TickRound = function (a) {
	return {ctor: 'TickRound', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoTickRound = _gicentre$elm_vega$VegaLite$TickRound;
var _gicentre$elm_vega$VegaLite$TickColor = function (a) {
	return {ctor: 'TickColor', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoTickColor = _gicentre$elm_vega$VegaLite$TickColor;
var _gicentre$elm_vega$VegaLite$Ticks = function (a) {
	return {ctor: 'Ticks', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoTicks = _gicentre$elm_vega$VegaLite$Ticks;
var _gicentre$elm_vega$VegaLite$ShortTimeLabels = function (a) {
	return {ctor: 'ShortTimeLabels', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoShortTimeLabels = _gicentre$elm_vega$VegaLite$ShortTimeLabels;
var _gicentre$elm_vega$VegaLite$LabelPadding = function (a) {
	return {ctor: 'LabelPadding', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoLabelPadding = _gicentre$elm_vega$VegaLite$LabelPadding;
var _gicentre$elm_vega$VegaLite$LabelOverlap = function (a) {
	return {ctor: 'LabelOverlap', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoLabelOverlap = _gicentre$elm_vega$VegaLite$LabelOverlap;
var _gicentre$elm_vega$VegaLite$LabelLimit = function (a) {
	return {ctor: 'LabelLimit', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoLabelLimit = _gicentre$elm_vega$VegaLite$LabelLimit;
var _gicentre$elm_vega$VegaLite$LabelFontSize = function (a) {
	return {ctor: 'LabelFontSize', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoLabelFontSize = _gicentre$elm_vega$VegaLite$LabelFontSize;
var _gicentre$elm_vega$VegaLite$LabelFont = function (a) {
	return {ctor: 'LabelFont', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoLabelFont = _gicentre$elm_vega$VegaLite$LabelFont;
var _gicentre$elm_vega$VegaLite$LabelColor = function (a) {
	return {ctor: 'LabelColor', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoLabelColor = _gicentre$elm_vega$VegaLite$LabelColor;
var _gicentre$elm_vega$VegaLite$LabelAngle = function (a) {
	return {ctor: 'LabelAngle', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoLabelAngle = _gicentre$elm_vega$VegaLite$LabelAngle;
var _gicentre$elm_vega$VegaLite$Labels = function (a) {
	return {ctor: 'Labels', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoLabels = _gicentre$elm_vega$VegaLite$Labels;
var _gicentre$elm_vega$VegaLite$GridWidth = function (a) {
	return {ctor: 'GridWidth', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoGridWidth = _gicentre$elm_vega$VegaLite$GridWidth;
var _gicentre$elm_vega$VegaLite$GridOpacity = function (a) {
	return {ctor: 'GridOpacity', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoGridOpacity = _gicentre$elm_vega$VegaLite$GridOpacity;
var _gicentre$elm_vega$VegaLite$GridDash = function (a) {
	return {ctor: 'GridDash', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoGridDash = _gicentre$elm_vega$VegaLite$GridDash;
var _gicentre$elm_vega$VegaLite$GridColor = function (a) {
	return {ctor: 'GridColor', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoGridColor = _gicentre$elm_vega$VegaLite$GridColor;
var _gicentre$elm_vega$VegaLite$Grid = function (a) {
	return {ctor: 'Grid', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoGrid = _gicentre$elm_vega$VegaLite$Grid;
var _gicentre$elm_vega$VegaLite$MinExtent = function (a) {
	return {ctor: 'MinExtent', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoMinExtent = _gicentre$elm_vega$VegaLite$MinExtent;
var _gicentre$elm_vega$VegaLite$MaxExtent = function (a) {
	return {ctor: 'MaxExtent', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoMaxExtent = _gicentre$elm_vega$VegaLite$MaxExtent;
var _gicentre$elm_vega$VegaLite$DomainWidth = function (a) {
	return {ctor: 'DomainWidth', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoDomainWidth = _gicentre$elm_vega$VegaLite$DomainWidth;
var _gicentre$elm_vega$VegaLite$DomainColor = function (a) {
	return {ctor: 'DomainColor', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoDomainColor = _gicentre$elm_vega$VegaLite$DomainColor;
var _gicentre$elm_vega$VegaLite$Domain = function (a) {
	return {ctor: 'Domain', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoDomain = _gicentre$elm_vega$VegaLite$Domain;
var _gicentre$elm_vega$VegaLite$BandPosition = function (a) {
	return {ctor: 'BandPosition', _0: a};
};
var _gicentre$elm_vega$VegaLite$axcoBandPosition = _gicentre$elm_vega$VegaLite$BandPosition;
var _gicentre$elm_vega$VegaLite$AxZIndex = function (a) {
	return {ctor: 'AxZIndex', _0: a};
};
var _gicentre$elm_vega$VegaLite$axZIndex = _gicentre$elm_vega$VegaLite$AxZIndex;
var _gicentre$elm_vega$VegaLite$AxDates = function (a) {
	return {ctor: 'AxDates', _0: a};
};
var _gicentre$elm_vega$VegaLite$axDates = _gicentre$elm_vega$VegaLite$AxDates;
var _gicentre$elm_vega$VegaLite$AxValues = function (a) {
	return {ctor: 'AxValues', _0: a};
};
var _gicentre$elm_vega$VegaLite$axValues = _gicentre$elm_vega$VegaLite$AxValues;
var _gicentre$elm_vega$VegaLite$AxTitlePadding = function (a) {
	return {ctor: 'AxTitlePadding', _0: a};
};
var _gicentre$elm_vega$VegaLite$axTitlePadding = _gicentre$elm_vega$VegaLite$AxTitlePadding;
var _gicentre$elm_vega$VegaLite$AxTitleMaxLength = function (a) {
	return {ctor: 'AxTitleMaxLength', _0: a};
};
var _gicentre$elm_vega$VegaLite$axTitleMaxLength = _gicentre$elm_vega$VegaLite$AxTitleMaxLength;
var _gicentre$elm_vega$VegaLite$AxTitleAngle = function (a) {
	return {ctor: 'AxTitleAngle', _0: a};
};
var _gicentre$elm_vega$VegaLite$axTitleAngle = _gicentre$elm_vega$VegaLite$AxTitleAngle;
var _gicentre$elm_vega$VegaLite$AxTitleAlign = function (a) {
	return {ctor: 'AxTitleAlign', _0: a};
};
var _gicentre$elm_vega$VegaLite$axTitleAlign = _gicentre$elm_vega$VegaLite$AxTitleAlign;
var _gicentre$elm_vega$VegaLite$AxTitle = function (a) {
	return {ctor: 'AxTitle', _0: a};
};
var _gicentre$elm_vega$VegaLite$axTitle = _gicentre$elm_vega$VegaLite$AxTitle;
var _gicentre$elm_vega$VegaLite$AxTickSize = function (a) {
	return {ctor: 'AxTickSize', _0: a};
};
var _gicentre$elm_vega$VegaLite$axTickSize = _gicentre$elm_vega$VegaLite$AxTickSize;
var _gicentre$elm_vega$VegaLite$AxTickCount = function (a) {
	return {ctor: 'AxTickCount', _0: a};
};
var _gicentre$elm_vega$VegaLite$axTickCount = _gicentre$elm_vega$VegaLite$AxTickCount;
var _gicentre$elm_vega$VegaLite$AxTicks = function (a) {
	return {ctor: 'AxTicks', _0: a};
};
var _gicentre$elm_vega$VegaLite$axTicks = _gicentre$elm_vega$VegaLite$AxTicks;
var _gicentre$elm_vega$VegaLite$AxPosition = function (a) {
	return {ctor: 'AxPosition', _0: a};
};
var _gicentre$elm_vega$VegaLite$axPosition = _gicentre$elm_vega$VegaLite$AxPosition;
var _gicentre$elm_vega$VegaLite$AxOrient = function (a) {
	return {ctor: 'AxOrient', _0: a};
};
var _gicentre$elm_vega$VegaLite$axOrient = _gicentre$elm_vega$VegaLite$AxOrient;
var _gicentre$elm_vega$VegaLite$AxOffset = function (a) {
	return {ctor: 'AxOffset', _0: a};
};
var _gicentre$elm_vega$VegaLite$axOffset = _gicentre$elm_vega$VegaLite$AxOffset;
var _gicentre$elm_vega$VegaLite$AxMinExtent = function (a) {
	return {ctor: 'AxMinExtent', _0: a};
};
var _gicentre$elm_vega$VegaLite$axMinExtent = _gicentre$elm_vega$VegaLite$AxMinExtent;
var _gicentre$elm_vega$VegaLite$AxMaxExtent = function (a) {
	return {ctor: 'AxMaxExtent', _0: a};
};
var _gicentre$elm_vega$VegaLite$axMaxExtent = _gicentre$elm_vega$VegaLite$AxMaxExtent;
var _gicentre$elm_vega$VegaLite$AxLabels = function (a) {
	return {ctor: 'AxLabels', _0: a};
};
var _gicentre$elm_vega$VegaLite$axLabels = _gicentre$elm_vega$VegaLite$AxLabels;
var _gicentre$elm_vega$VegaLite$AxLabelPadding = function (a) {
	return {ctor: 'AxLabelPadding', _0: a};
};
var _gicentre$elm_vega$VegaLite$axLabelPadding = _gicentre$elm_vega$VegaLite$AxLabelPadding;
var _gicentre$elm_vega$VegaLite$AxLabelOverlap = function (a) {
	return {ctor: 'AxLabelOverlap', _0: a};
};
var _gicentre$elm_vega$VegaLite$axLabelOverlap = _gicentre$elm_vega$VegaLite$AxLabelOverlap;
var _gicentre$elm_vega$VegaLite$AxLabelAngle = function (a) {
	return {ctor: 'AxLabelAngle', _0: a};
};
var _gicentre$elm_vega$VegaLite$axLabelAngle = _gicentre$elm_vega$VegaLite$AxLabelAngle;
var _gicentre$elm_vega$VegaLite$AxGrid = function (a) {
	return {ctor: 'AxGrid', _0: a};
};
var _gicentre$elm_vega$VegaLite$axGrid = _gicentre$elm_vega$VegaLite$AxGrid;
var _gicentre$elm_vega$VegaLite$AxFormat = function (a) {
	return {ctor: 'AxFormat', _0: a};
};
var _gicentre$elm_vega$VegaLite$axFormat = _gicentre$elm_vega$VegaLite$AxFormat;
var _gicentre$elm_vega$VegaLite$AxDomain = function (a) {
	return {ctor: 'AxDomain', _0: a};
};
var _gicentre$elm_vega$VegaLite$axDomain = _gicentre$elm_vega$VegaLite$AxDomain;
var _gicentre$elm_vega$VegaLite$IColor = F2(
	function (a, b) {
		return {ctor: 'IColor', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$iColor = function (f) {
	return _gicentre$elm_vega$VegaLite$IColor(f);
};
var _gicentre$elm_vega$VegaLite$ITel = F2(
	function (a, b) {
		return {ctor: 'ITel', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$iTel = function (f) {
	return _gicentre$elm_vega$VegaLite$ITel(f);
};
var _gicentre$elm_vega$VegaLite$IDateTimeLocal = F2(
	function (a, b) {
		return {ctor: 'IDateTimeLocal', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$iDateTimeLocal = function (f) {
	return _gicentre$elm_vega$VegaLite$IDateTimeLocal(f);
};
var _gicentre$elm_vega$VegaLite$IWeek = F2(
	function (a, b) {
		return {ctor: 'IWeek', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$iWeek = function (f) {
	return _gicentre$elm_vega$VegaLite$IWeek(f);
};
var _gicentre$elm_vega$VegaLite$IMonth = F2(
	function (a, b) {
		return {ctor: 'IMonth', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$iMonth = function (f) {
	return _gicentre$elm_vega$VegaLite$IMonth(f);
};
var _gicentre$elm_vega$VegaLite$ITime = F2(
	function (a, b) {
		return {ctor: 'ITime', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$iTime = function (f) {
	return _gicentre$elm_vega$VegaLite$ITime(f);
};
var _gicentre$elm_vega$VegaLite$IDate = F2(
	function (a, b) {
		return {ctor: 'IDate', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$iDate = function (f) {
	return _gicentre$elm_vega$VegaLite$IDate(f);
};
var _gicentre$elm_vega$VegaLite$INumber = F2(
	function (a, b) {
		return {ctor: 'INumber', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$iNumber = function (f) {
	return _gicentre$elm_vega$VegaLite$INumber(f);
};
var _gicentre$elm_vega$VegaLite$IText = F2(
	function (a, b) {
		return {ctor: 'IText', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$iText = function (f) {
	return _gicentre$elm_vega$VegaLite$IText(f);
};
var _gicentre$elm_vega$VegaLite$ISelect = F2(
	function (a, b) {
		return {ctor: 'ISelect', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$iSelect = function (f) {
	return _gicentre$elm_vega$VegaLite$ISelect(f);
};
var _gicentre$elm_vega$VegaLite$IRadio = F2(
	function (a, b) {
		return {ctor: 'IRadio', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$iRadio = function (f) {
	return _gicentre$elm_vega$VegaLite$IRadio(f);
};
var _gicentre$elm_vega$VegaLite$ICheckbox = F2(
	function (a, b) {
		return {ctor: 'ICheckbox', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$iCheckbox = function (f) {
	return _gicentre$elm_vega$VegaLite$ICheckbox(f);
};
var _gicentre$elm_vega$VegaLite$IRange = F2(
	function (a, b) {
		return {ctor: 'IRange', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$iRange = function (f) {
	return _gicentre$elm_vega$VegaLite$IRange(f);
};
var _gicentre$elm_vega$VegaLite$Steps = function (a) {
	return {ctor: 'Steps', _0: a};
};
var _gicentre$elm_vega$VegaLite$biSteps = _gicentre$elm_vega$VegaLite$Steps;
var _gicentre$elm_vega$VegaLite$Step = function (a) {
	return {ctor: 'Step', _0: a};
};
var _gicentre$elm_vega$VegaLite$biStep = _gicentre$elm_vega$VegaLite$Step;
var _gicentre$elm_vega$VegaLite$Nice = function (a) {
	return {ctor: 'Nice', _0: a};
};
var _gicentre$elm_vega$VegaLite$biNice = _gicentre$elm_vega$VegaLite$Nice;
var _gicentre$elm_vega$VegaLite$MinStep = function (a) {
	return {ctor: 'MinStep', _0: a};
};
var _gicentre$elm_vega$VegaLite$biMinStep = _gicentre$elm_vega$VegaLite$MinStep;
var _gicentre$elm_vega$VegaLite$MaxBins = function (a) {
	return {ctor: 'MaxBins', _0: a};
};
var _gicentre$elm_vega$VegaLite$biMaxBins = _gicentre$elm_vega$VegaLite$MaxBins;
var _gicentre$elm_vega$VegaLite$Extent = F2(
	function (a, b) {
		return {ctor: 'Extent', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$biExtent = _gicentre$elm_vega$VegaLite$Extent;
var _gicentre$elm_vega$VegaLite$Divides = function (a) {
	return {ctor: 'Divides', _0: a};
};
var _gicentre$elm_vega$VegaLite$biDivide = _gicentre$elm_vega$VegaLite$Divides;
var _gicentre$elm_vega$VegaLite$Divide = F2(
	function (a, b) {
		return {ctor: 'Divide', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$Base = function (a) {
	return {ctor: 'Base', _0: a};
};
var _gicentre$elm_vega$VegaLite$biBase = _gicentre$elm_vega$VegaLite$Base;
var _gicentre$elm_vega$VegaLite$Not = function (a) {
	return {ctor: 'Not', _0: a};
};
var _gicentre$elm_vega$VegaLite$not = _gicentre$elm_vega$VegaLite$Not;
var _gicentre$elm_vega$VegaLite$Or = F2(
	function (a, b) {
		return {ctor: 'Or', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$or = F2(
	function (op1, op2) {
		return A2(_gicentre$elm_vega$VegaLite$Or, op1, op2);
	});
var _gicentre$elm_vega$VegaLite$And = F2(
	function (a, b) {
		return {ctor: 'And', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$and = F2(
	function (op1, op2) {
		return A2(_gicentre$elm_vega$VegaLite$And, op1, op2);
	});
var _gicentre$elm_vega$VegaLite$SelectionName = function (a) {
	return {ctor: 'SelectionName', _0: a};
};
var _gicentre$elm_vega$VegaLite$selectionName = _gicentre$elm_vega$VegaLite$SelectionName;
var _gicentre$elm_vega$VegaLite$Selection = function (a) {
	return {ctor: 'Selection', _0: a};
};
var _gicentre$elm_vega$VegaLite$selected = _gicentre$elm_vega$VegaLite$Selection;
var _gicentre$elm_vega$VegaLite$Expr = function (a) {
	return {ctor: 'Expr', _0: a};
};
var _gicentre$elm_vega$VegaLite$expr = _gicentre$elm_vega$VegaLite$Expr;
var _gicentre$elm_vega$VegaLite$ChSize = {ctor: 'ChSize'};
var _gicentre$elm_vega$VegaLite$ChShape = {ctor: 'ChShape'};
var _gicentre$elm_vega$VegaLite$ChOpacity = {ctor: 'ChOpacity'};
var _gicentre$elm_vega$VegaLite$ChColor = {ctor: 'ChColor'};
var _gicentre$elm_vega$VegaLite$ChY2 = {ctor: 'ChY2'};
var _gicentre$elm_vega$VegaLite$ChX2 = {ctor: 'ChX2'};
var _gicentre$elm_vega$VegaLite$ChY = {ctor: 'ChY'};
var _gicentre$elm_vega$VegaLite$ChX = {ctor: 'ChX'};
var _gicentre$elm_vega$VegaLite$Rgb = function (a) {
	return {ctor: 'Rgb', _0: a};
};
var _gicentre$elm_vega$VegaLite$rgb = _gicentre$elm_vega$VegaLite$Rgb;
var _gicentre$elm_vega$VegaLite$Lab = {ctor: 'Lab'};
var _gicentre$elm_vega$VegaLite$HslLong = {ctor: 'HslLong'};
var _gicentre$elm_vega$VegaLite$Hsl = {ctor: 'Hsl'};
var _gicentre$elm_vega$VegaLite$HclLong = {ctor: 'HclLong'};
var _gicentre$elm_vega$VegaLite$Hcl = {ctor: 'Hcl'};
var _gicentre$elm_vega$VegaLite$CubeHelixLong = function (a) {
	return {ctor: 'CubeHelixLong', _0: a};
};
var _gicentre$elm_vega$VegaLite$cubeHelixLong = _gicentre$elm_vega$VegaLite$CubeHelixLong;
var _gicentre$elm_vega$VegaLite$CubeHelix = function (a) {
	return {ctor: 'CubeHelix', _0: a};
};
var _gicentre$elm_vega$VegaLite$cubeHelix = _gicentre$elm_vega$VegaLite$CubeHelix;
var _gicentre$elm_vega$VegaLite$LTRB = F4(
	function (a, b, c, d) {
		return {ctor: 'LTRB', _0: a, _1: b, _2: c, _3: d};
	});
var _gicentre$elm_vega$VegaLite$clipRect = F4(
	function (l, t, r, b) {
		return A4(_gicentre$elm_vega$VegaLite$LTRB, l, t, r, b);
	});
var _gicentre$elm_vega$VegaLite$NoClip = {ctor: 'NoClip'};
var _gicentre$elm_vega$VegaLite$View = function (a) {
	return {ctor: 'View', _0: a};
};
var _gicentre$elm_vega$VegaLite$coView = _gicentre$elm_vega$VegaLite$View;
var _gicentre$elm_vega$VegaLite$TrailStyle = function (a) {
	return {ctor: 'TrailStyle', _0: a};
};
var _gicentre$elm_vega$VegaLite$coTrail = function (mps) {
	return F2(
		function (x, y) {
			return {ctor: '::', _0: x, _1: y};
		})(
		_gicentre$elm_vega$VegaLite$configProperty(
			_gicentre$elm_vega$VegaLite$TrailStyle(mps)));
};
var _gicentre$elm_vega$VegaLite$TimeFormat = function (a) {
	return {ctor: 'TimeFormat', _0: a};
};
var _gicentre$elm_vega$VegaLite$coTimeFormat = _gicentre$elm_vega$VegaLite$TimeFormat;
var _gicentre$elm_vega$VegaLite$TitleStyle = function (a) {
	return {ctor: 'TitleStyle', _0: a};
};
var _gicentre$elm_vega$VegaLite$coTitle = _gicentre$elm_vega$VegaLite$TitleStyle;
var _gicentre$elm_vega$VegaLite$TickStyle = function (a) {
	return {ctor: 'TickStyle', _0: a};
};
var _gicentre$elm_vega$VegaLite$coTick = _gicentre$elm_vega$VegaLite$TickStyle;
var _gicentre$elm_vega$VegaLite$TextStyle = function (a) {
	return {ctor: 'TextStyle', _0: a};
};
var _gicentre$elm_vega$VegaLite$coText = _gicentre$elm_vega$VegaLite$TextStyle;
var _gicentre$elm_vega$VegaLite$Stack = function (a) {
	return {ctor: 'Stack', _0: a};
};
var _gicentre$elm_vega$VegaLite$coStack = _gicentre$elm_vega$VegaLite$Stack;
var _gicentre$elm_vega$VegaLite$SquareStyle = function (a) {
	return {ctor: 'SquareStyle', _0: a};
};
var _gicentre$elm_vega$VegaLite$coSquare = _gicentre$elm_vega$VegaLite$SquareStyle;
var _gicentre$elm_vega$VegaLite$SelectionStyle = function (a) {
	return {ctor: 'SelectionStyle', _0: a};
};
var _gicentre$elm_vega$VegaLite$coSelection = _gicentre$elm_vega$VegaLite$SelectionStyle;
var _gicentre$elm_vega$VegaLite$Scale = function (a) {
	return {ctor: 'Scale', _0: a};
};
var _gicentre$elm_vega$VegaLite$coScale = _gicentre$elm_vega$VegaLite$Scale;
var _gicentre$elm_vega$VegaLite$RuleStyle = function (a) {
	return {ctor: 'RuleStyle', _0: a};
};
var _gicentre$elm_vega$VegaLite$coRule = _gicentre$elm_vega$VegaLite$RuleStyle;
var _gicentre$elm_vega$VegaLite$RemoveInvalid = function (a) {
	return {ctor: 'RemoveInvalid', _0: a};
};
var _gicentre$elm_vega$VegaLite$coRemoveInvalid = _gicentre$elm_vega$VegaLite$RemoveInvalid;
var _gicentre$elm_vega$VegaLite$RectStyle = function (a) {
	return {ctor: 'RectStyle', _0: a};
};
var _gicentre$elm_vega$VegaLite$coRect = _gicentre$elm_vega$VegaLite$RectStyle;
var _gicentre$elm_vega$VegaLite$Range = function (a) {
	return {ctor: 'Range', _0: a};
};
var _gicentre$elm_vega$VegaLite$coRange = _gicentre$elm_vega$VegaLite$Range;
var _gicentre$elm_vega$VegaLite$Projection = function (a) {
	return {ctor: 'Projection', _0: a};
};
var _gicentre$elm_vega$VegaLite$coProjection = _gicentre$elm_vega$VegaLite$Projection;
var _gicentre$elm_vega$VegaLite$PointStyle = function (a) {
	return {ctor: 'PointStyle', _0: a};
};
var _gicentre$elm_vega$VegaLite$coPoint = _gicentre$elm_vega$VegaLite$PointStyle;
var _gicentre$elm_vega$VegaLite$Padding = function (a) {
	return {ctor: 'Padding', _0: a};
};
var _gicentre$elm_vega$VegaLite$coPadding = _gicentre$elm_vega$VegaLite$Padding;
var _gicentre$elm_vega$VegaLite$NumberFormat = function (a) {
	return {ctor: 'NumberFormat', _0: a};
};
var _gicentre$elm_vega$VegaLite$coNumberFormat = _gicentre$elm_vega$VegaLite$NumberFormat;
var _gicentre$elm_vega$VegaLite$NamedStyle = F2(
	function (a, b) {
		return {ctor: 'NamedStyle', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$coNamedStyle = _gicentre$elm_vega$VegaLite$NamedStyle;
var _gicentre$elm_vega$VegaLite$MarkStyle = function (a) {
	return {ctor: 'MarkStyle', _0: a};
};
var _gicentre$elm_vega$VegaLite$coMark = _gicentre$elm_vega$VegaLite$MarkStyle;
var _gicentre$elm_vega$VegaLite$LineStyle = function (a) {
	return {ctor: 'LineStyle', _0: a};
};
var _gicentre$elm_vega$VegaLite$coLine = _gicentre$elm_vega$VegaLite$LineStyle;
var _gicentre$elm_vega$VegaLite$Legend = function (a) {
	return {ctor: 'Legend', _0: a};
};
var _gicentre$elm_vega$VegaLite$coLegend = _gicentre$elm_vega$VegaLite$Legend;
var _gicentre$elm_vega$VegaLite$GeoshapeStyle = function (a) {
	return {ctor: 'GeoshapeStyle', _0: a};
};
var _gicentre$elm_vega$VegaLite$coGeoshape = _gicentre$elm_vega$VegaLite$GeoshapeStyle;
var _gicentre$elm_vega$VegaLite$FieldTitle = function (a) {
	return {ctor: 'FieldTitle', _0: a};
};
var _gicentre$elm_vega$VegaLite$coFieldTitle = _gicentre$elm_vega$VegaLite$FieldTitle;
var _gicentre$elm_vega$VegaLite$CountTitle = function (a) {
	return {ctor: 'CountTitle', _0: a};
};
var _gicentre$elm_vega$VegaLite$coCountTitle = _gicentre$elm_vega$VegaLite$CountTitle;
var _gicentre$elm_vega$VegaLite$CircleStyle = function (a) {
	return {ctor: 'CircleStyle', _0: a};
};
var _gicentre$elm_vega$VegaLite$coCircle = _gicentre$elm_vega$VegaLite$CircleStyle;
var _gicentre$elm_vega$VegaLite$BarStyle = function (a) {
	return {ctor: 'BarStyle', _0: a};
};
var _gicentre$elm_vega$VegaLite$coBar = _gicentre$elm_vega$VegaLite$BarStyle;
var _gicentre$elm_vega$VegaLite$Background = function (a) {
	return {ctor: 'Background', _0: a};
};
var _gicentre$elm_vega$VegaLite$coBackground = _gicentre$elm_vega$VegaLite$Background;
var _gicentre$elm_vega$VegaLite$AxisBand = function (a) {
	return {ctor: 'AxisBand', _0: a};
};
var _gicentre$elm_vega$VegaLite$coAxisBand = _gicentre$elm_vega$VegaLite$AxisBand;
var _gicentre$elm_vega$VegaLite$AxisBottom = function (a) {
	return {ctor: 'AxisBottom', _0: a};
};
var _gicentre$elm_vega$VegaLite$coAxisBottom = _gicentre$elm_vega$VegaLite$AxisBottom;
var _gicentre$elm_vega$VegaLite$AxisTop = function (a) {
	return {ctor: 'AxisTop', _0: a};
};
var _gicentre$elm_vega$VegaLite$coAxisTop = _gicentre$elm_vega$VegaLite$AxisTop;
var _gicentre$elm_vega$VegaLite$AxisRight = function (a) {
	return {ctor: 'AxisRight', _0: a};
};
var _gicentre$elm_vega$VegaLite$coAxisRight = _gicentre$elm_vega$VegaLite$AxisRight;
var _gicentre$elm_vega$VegaLite$AxisLeft = function (a) {
	return {ctor: 'AxisLeft', _0: a};
};
var _gicentre$elm_vega$VegaLite$coAxisLeft = _gicentre$elm_vega$VegaLite$AxisLeft;
var _gicentre$elm_vega$VegaLite$AxisY = function (a) {
	return {ctor: 'AxisY', _0: a};
};
var _gicentre$elm_vega$VegaLite$coAxisY = _gicentre$elm_vega$VegaLite$AxisY;
var _gicentre$elm_vega$VegaLite$AxisX = function (a) {
	return {ctor: 'AxisX', _0: a};
};
var _gicentre$elm_vega$VegaLite$coAxisX = _gicentre$elm_vega$VegaLite$AxisX;
var _gicentre$elm_vega$VegaLite$Axis = function (a) {
	return {ctor: 'Axis', _0: a};
};
var _gicentre$elm_vega$VegaLite$coAxis = _gicentre$elm_vega$VegaLite$Axis;
var _gicentre$elm_vega$VegaLite$Autosize = function (a) {
	return {ctor: 'Autosize', _0: a};
};
var _gicentre$elm_vega$VegaLite$coAutosize = _gicentre$elm_vega$VegaLite$Autosize;
var _gicentre$elm_vega$VegaLite$AreaStyle = function (a) {
	return {ctor: 'AreaStyle', _0: a};
};
var _gicentre$elm_vega$VegaLite$coArea = _gicentre$elm_vega$VegaLite$AreaStyle;
var _gicentre$elm_vega$VegaLite$CGrabbing = {ctor: 'CGrabbing'};
var _gicentre$elm_vega$VegaLite$CGrab = {ctor: 'CGrab'};
var _gicentre$elm_vega$VegaLite$CZoomOut = {ctor: 'CZoomOut'};
var _gicentre$elm_vega$VegaLite$CZoomIn = {ctor: 'CZoomIn'};
var _gicentre$elm_vega$VegaLite$CNWSEResize = {ctor: 'CNWSEResize'};
var _gicentre$elm_vega$VegaLite$CNESWResize = {ctor: 'CNESWResize'};
var _gicentre$elm_vega$VegaLite$CNSResize = {ctor: 'CNSResize'};
var _gicentre$elm_vega$VegaLite$CEWResize = {ctor: 'CEWResize'};
var _gicentre$elm_vega$VegaLite$CSWResize = {ctor: 'CSWResize'};
var _gicentre$elm_vega$VegaLite$CSEResize = {ctor: 'CSEResize'};
var _gicentre$elm_vega$VegaLite$CNWResize = {ctor: 'CNWResize'};
var _gicentre$elm_vega$VegaLite$CNEResize = {ctor: 'CNEResize'};
var _gicentre$elm_vega$VegaLite$CWResize = {ctor: 'CWResize'};
var _gicentre$elm_vega$VegaLite$CSResize = {ctor: 'CSResize'};
var _gicentre$elm_vega$VegaLite$CEResize = {ctor: 'CEResize'};
var _gicentre$elm_vega$VegaLite$CNResize = {ctor: 'CNResize'};
var _gicentre$elm_vega$VegaLite$CRowResize = {ctor: 'CRowResize'};
var _gicentre$elm_vega$VegaLite$CColResize = {ctor: 'CColResize'};
var _gicentre$elm_vega$VegaLite$CAllScroll = {ctor: 'CAllScroll'};
var _gicentre$elm_vega$VegaLite$CNotAllowed = {ctor: 'CNotAllowed'};
var _gicentre$elm_vega$VegaLite$CNoDrop = {ctor: 'CNoDrop'};
var _gicentre$elm_vega$VegaLite$CMove = {ctor: 'CMove'};
var _gicentre$elm_vega$VegaLite$CCopy = {ctor: 'CCopy'};
var _gicentre$elm_vega$VegaLite$CAlias = {ctor: 'CAlias'};
var _gicentre$elm_vega$VegaLite$CVerticalText = {ctor: 'CVerticalText'};
var _gicentre$elm_vega$VegaLite$CText = {ctor: 'CText'};
var _gicentre$elm_vega$VegaLite$CCrosshair = {ctor: 'CCrosshair'};
var _gicentre$elm_vega$VegaLite$CCell = {ctor: 'CCell'};
var _gicentre$elm_vega$VegaLite$CWait = {ctor: 'CWait'};
var _gicentre$elm_vega$VegaLite$CProgress = {ctor: 'CProgress'};
var _gicentre$elm_vega$VegaLite$CPointer = {ctor: 'CPointer'};
var _gicentre$elm_vega$VegaLite$CHelp = {ctor: 'CHelp'};
var _gicentre$elm_vega$VegaLite$CContextMenu = {ctor: 'CContextMenu'};
var _gicentre$elm_vega$VegaLite$CNone = {ctor: 'CNone'};
var _gicentre$elm_vega$VegaLite$CDefault = {ctor: 'CDefault'};
var _gicentre$elm_vega$VegaLite$CAuto = {ctor: 'CAuto'};
var _gicentre$elm_vega$VegaLite$FoUtc = function (a) {
	return {ctor: 'FoUtc', _0: a};
};
var _gicentre$elm_vega$VegaLite$foUtc = _gicentre$elm_vega$VegaLite$FoUtc;
var _gicentre$elm_vega$VegaLite$FoDate = function (a) {
	return {ctor: 'FoDate', _0: a};
};
var _gicentre$elm_vega$VegaLite$foDate = _gicentre$elm_vega$VegaLite$FoDate;
var _gicentre$elm_vega$VegaLite$FoBoolean = {ctor: 'FoBoolean'};
var _gicentre$elm_vega$VegaLite$FoNumber = {ctor: 'FoNumber'};
var _gicentre$elm_vega$VegaLite$Str = function (a) {
	return {ctor: 'Str', _0: a};
};
var _gicentre$elm_vega$VegaLite$str = _gicentre$elm_vega$VegaLite$Str;
var _gicentre$elm_vega$VegaLite$Number = function (a) {
	return {ctor: 'Number', _0: a};
};
var _gicentre$elm_vega$VegaLite$num = _gicentre$elm_vega$VegaLite$Number;
var _gicentre$elm_vega$VegaLite$DateTime = function (a) {
	return {ctor: 'DateTime', _0: a};
};
var _gicentre$elm_vega$VegaLite$dt = _gicentre$elm_vega$VegaLite$DateTime;
var _gicentre$elm_vega$VegaLite$Boolean = function (a) {
	return {ctor: 'Boolean', _0: a};
};
var _gicentre$elm_vega$VegaLite$boo = _gicentre$elm_vega$VegaLite$Boolean;
var _gicentre$elm_vega$VegaLite$Strings = function (a) {
	return {ctor: 'Strings', _0: a};
};
var _gicentre$elm_vega$VegaLite$strs = _gicentre$elm_vega$VegaLite$Strings;
var _gicentre$elm_vega$VegaLite$Numbers = function (a) {
	return {ctor: 'Numbers', _0: a};
};
var _gicentre$elm_vega$VegaLite$nums = _gicentre$elm_vega$VegaLite$Numbers;
var _gicentre$elm_vega$VegaLite$DateTimes = function (a) {
	return {ctor: 'DateTimes', _0: a};
};
var _gicentre$elm_vega$VegaLite$dts = _gicentre$elm_vega$VegaLite$DateTimes;
var _gicentre$elm_vega$VegaLite$Booleans = function (a) {
	return {ctor: 'Booleans', _0: a};
};
var _gicentre$elm_vega$VegaLite$boos = _gicentre$elm_vega$VegaLite$Booleans;
var _gicentre$elm_vega$VegaLite$DTMilliseconds = function (a) {
	return {ctor: 'DTMilliseconds', _0: a};
};
var _gicentre$elm_vega$VegaLite$dtMillisecond = _gicentre$elm_vega$VegaLite$DTMilliseconds;
var _gicentre$elm_vega$VegaLite$DTSeconds = function (a) {
	return {ctor: 'DTSeconds', _0: a};
};
var _gicentre$elm_vega$VegaLite$dtSecond = _gicentre$elm_vega$VegaLite$DTSeconds;
var _gicentre$elm_vega$VegaLite$DTMinutes = function (a) {
	return {ctor: 'DTMinutes', _0: a};
};
var _gicentre$elm_vega$VegaLite$dtMinute = _gicentre$elm_vega$VegaLite$DTMinutes;
var _gicentre$elm_vega$VegaLite$DTHours = function (a) {
	return {ctor: 'DTHours', _0: a};
};
var _gicentre$elm_vega$VegaLite$dtHour = _gicentre$elm_vega$VegaLite$DTHours;
var _gicentre$elm_vega$VegaLite$DTDay = function (a) {
	return {ctor: 'DTDay', _0: a};
};
var _gicentre$elm_vega$VegaLite$dtDay = _gicentre$elm_vega$VegaLite$DTDay;
var _gicentre$elm_vega$VegaLite$DTDate = function (a) {
	return {ctor: 'DTDate', _0: a};
};
var _gicentre$elm_vega$VegaLite$dtDate = _gicentre$elm_vega$VegaLite$DTDate;
var _gicentre$elm_vega$VegaLite$DTMonth = function (a) {
	return {ctor: 'DTMonth', _0: a};
};
var _gicentre$elm_vega$VegaLite$dtMonth = _gicentre$elm_vega$VegaLite$DTMonth;
var _gicentre$elm_vega$VegaLite$DTQuarter = function (a) {
	return {ctor: 'DTQuarter', _0: a};
};
var _gicentre$elm_vega$VegaLite$dtQuarter = _gicentre$elm_vega$VegaLite$DTQuarter;
var _gicentre$elm_vega$VegaLite$DTYear = function (a) {
	return {ctor: 'DTYear', _0: a};
};
var _gicentre$elm_vega$VegaLite$dtYear = _gicentre$elm_vega$VegaLite$DTYear;
var _gicentre$elm_vega$VegaLite$Sun = {ctor: 'Sun'};
var _gicentre$elm_vega$VegaLite$Sat = {ctor: 'Sat'};
var _gicentre$elm_vega$VegaLite$Fri = {ctor: 'Fri'};
var _gicentre$elm_vega$VegaLite$Thu = {ctor: 'Thu'};
var _gicentre$elm_vega$VegaLite$Wed = {ctor: 'Wed'};
var _gicentre$elm_vega$VegaLite$Tue = {ctor: 'Tue'};
var _gicentre$elm_vega$VegaLite$Mon = {ctor: 'Mon'};
var _gicentre$elm_vega$VegaLite$DAggregate = function (a) {
	return {ctor: 'DAggregate', _0: a};
};
var _gicentre$elm_vega$VegaLite$dAggregate = _gicentre$elm_vega$VegaLite$DAggregate;
var _gicentre$elm_vega$VegaLite$DTimeUnit = function (a) {
	return {ctor: 'DTimeUnit', _0: a};
};
var _gicentre$elm_vega$VegaLite$dTimeUnit = _gicentre$elm_vega$VegaLite$DTimeUnit;
var _gicentre$elm_vega$VegaLite$DBin = function (a) {
	return {ctor: 'DBin', _0: a};
};
var _gicentre$elm_vega$VegaLite$dBin = _gicentre$elm_vega$VegaLite$DBin;
var _gicentre$elm_vega$VegaLite$DmType = function (a) {
	return {ctor: 'DmType', _0: a};
};
var _gicentre$elm_vega$VegaLite$dMType = _gicentre$elm_vega$VegaLite$DmType;
var _gicentre$elm_vega$VegaLite$DName = function (a) {
	return {ctor: 'DName', _0: a};
};
var _gicentre$elm_vega$VegaLite$dName = _gicentre$elm_vega$VegaLite$DName;
var _gicentre$elm_vega$VegaLite$FHeader = function (a) {
	return {ctor: 'FHeader', _0: a};
};
var _gicentre$elm_vega$VegaLite$fHeader = _gicentre$elm_vega$VegaLite$FHeader;
var _gicentre$elm_vega$VegaLite$FTimeUnit = function (a) {
	return {ctor: 'FTimeUnit', _0: a};
};
var _gicentre$elm_vega$VegaLite$fTimeUnit = _gicentre$elm_vega$VegaLite$FTimeUnit;
var _gicentre$elm_vega$VegaLite$FAggregate = function (a) {
	return {ctor: 'FAggregate', _0: a};
};
var _gicentre$elm_vega$VegaLite$fAggregate = _gicentre$elm_vega$VegaLite$FAggregate;
var _gicentre$elm_vega$VegaLite$FBin = function (a) {
	return {ctor: 'FBin', _0: a};
};
var _gicentre$elm_vega$VegaLite$fBin = _gicentre$elm_vega$VegaLite$FBin;
var _gicentre$elm_vega$VegaLite$FmType = function (a) {
	return {ctor: 'FmType', _0: a};
};
var _gicentre$elm_vega$VegaLite$fMType = _gicentre$elm_vega$VegaLite$FmType;
var _gicentre$elm_vega$VegaLite$FName = function (a) {
	return {ctor: 'FName', _0: a};
};
var _gicentre$elm_vega$VegaLite$fName = _gicentre$elm_vega$VegaLite$FName;
var _gicentre$elm_vega$VegaLite$RowBy = function (a) {
	return {ctor: 'RowBy', _0: a};
};
var _gicentre$elm_vega$VegaLite$rowBy = _gicentre$elm_vega$VegaLite$RowBy;
var _gicentre$elm_vega$VegaLite$ColumnBy = function (a) {
	return {ctor: 'ColumnBy', _0: a};
};
var _gicentre$elm_vega$VegaLite$columnBy = _gicentre$elm_vega$VegaLite$ColumnBy;
var _gicentre$elm_vega$VegaLite$Plain = {ctor: 'Plain'};
var _gicentre$elm_vega$VegaLite$Function = {ctor: 'Function'};
var _gicentre$elm_vega$VegaLite$Verbal = {ctor: 'Verbal'};
var _gicentre$elm_vega$VegaLite$FRange = F2(
	function (a, b) {
		return {ctor: 'FRange', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$fiRange = _gicentre$elm_vega$VegaLite$FRange;
var _gicentre$elm_vega$VegaLite$FOneOf = F2(
	function (a, b) {
		return {ctor: 'FOneOf', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$fiOneOf = _gicentre$elm_vega$VegaLite$FOneOf;
var _gicentre$elm_vega$VegaLite$FSelection = function (a) {
	return {ctor: 'FSelection', _0: a};
};
var _gicentre$elm_vega$VegaLite$fiSelection = _gicentre$elm_vega$VegaLite$FSelection;
var _gicentre$elm_vega$VegaLite$FCompose = function (a) {
	return {ctor: 'FCompose', _0: a};
};
var _gicentre$elm_vega$VegaLite$fiCompose = _gicentre$elm_vega$VegaLite$FCompose;
var _gicentre$elm_vega$VegaLite$FExpr = function (a) {
	return {ctor: 'FExpr', _0: a};
};
var _gicentre$elm_vega$VegaLite$fiExpr = _gicentre$elm_vega$VegaLite$FExpr;
var _gicentre$elm_vega$VegaLite$FEqual = F2(
	function (a, b) {
		return {ctor: 'FEqual', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$fiEqual = _gicentre$elm_vega$VegaLite$FEqual;
var _gicentre$elm_vega$VegaLite$DateRange = F2(
	function (a, b) {
		return {ctor: 'DateRange', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$dtRange = _gicentre$elm_vega$VegaLite$DateRange;
var _gicentre$elm_vega$VegaLite$NumberRange = F2(
	function (a, b) {
		return {ctor: 'NumberRange', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$numRange = _gicentre$elm_vega$VegaLite$NumberRange;
var _gicentre$elm_vega$VegaLite$W900 = {ctor: 'W900'};
var _gicentre$elm_vega$VegaLite$W800 = {ctor: 'W800'};
var _gicentre$elm_vega$VegaLite$W700 = {ctor: 'W700'};
var _gicentre$elm_vega$VegaLite$W600 = {ctor: 'W600'};
var _gicentre$elm_vega$VegaLite$W500 = {ctor: 'W500'};
var _gicentre$elm_vega$VegaLite$W400 = {ctor: 'W400'};
var _gicentre$elm_vega$VegaLite$W300 = {ctor: 'W300'};
var _gicentre$elm_vega$VegaLite$W200 = {ctor: 'W200'};
var _gicentre$elm_vega$VegaLite$W100 = {ctor: 'W100'};
var _gicentre$elm_vega$VegaLite$Normal = {ctor: 'Normal'};
var _gicentre$elm_vega$VegaLite$Lighter = {ctor: 'Lighter'};
var _gicentre$elm_vega$VegaLite$Bolder = {ctor: 'Bolder'};
var _gicentre$elm_vega$VegaLite$Bold = {ctor: 'Bold'};
var _gicentre$elm_vega$VegaLite$Parse = function (a) {
	return {ctor: 'Parse', _0: a};
};
var _gicentre$elm_vega$VegaLite$parse = _gicentre$elm_vega$VegaLite$Parse;
var _gicentre$elm_vega$VegaLite$TopojsonMesh = function (a) {
	return {ctor: 'TopojsonMesh', _0: a};
};
var _gicentre$elm_vega$VegaLite$topojsonMesh = _gicentre$elm_vega$VegaLite$TopojsonMesh;
var _gicentre$elm_vega$VegaLite$TopojsonFeature = function (a) {
	return {ctor: 'TopojsonFeature', _0: a};
};
var _gicentre$elm_vega$VegaLite$topojsonFeature = _gicentre$elm_vega$VegaLite$TopojsonFeature;
var _gicentre$elm_vega$VegaLite$TSV = {ctor: 'TSV'};
var _gicentre$elm_vega$VegaLite$CSV = {ctor: 'CSV'};
var _gicentre$elm_vega$VegaLite$JSON = function (a) {
	return {ctor: 'JSON', _0: a};
};
var _gicentre$elm_vega$VegaLite$jsonProperty = _gicentre$elm_vega$VegaLite$JSON;
var _gicentre$elm_vega$VegaLite$GeoPolygons = function (a) {
	return {ctor: 'GeoPolygons', _0: a};
};
var _gicentre$elm_vega$VegaLite$geoPolygons = _gicentre$elm_vega$VegaLite$GeoPolygons;
var _gicentre$elm_vega$VegaLite$GeoPolygon = function (a) {
	return {ctor: 'GeoPolygon', _0: a};
};
var _gicentre$elm_vega$VegaLite$geoPolygon = _gicentre$elm_vega$VegaLite$GeoPolygon;
var _gicentre$elm_vega$VegaLite$GeoLines = function (a) {
	return {ctor: 'GeoLines', _0: a};
};
var _gicentre$elm_vega$VegaLite$geoLines = _gicentre$elm_vega$VegaLite$GeoLines;
var _gicentre$elm_vega$VegaLite$GeoLine = function (a) {
	return {ctor: 'GeoLine', _0: a};
};
var _gicentre$elm_vega$VegaLite$geoLine = _gicentre$elm_vega$VegaLite$GeoLine;
var _gicentre$elm_vega$VegaLite$GeoPoints = function (a) {
	return {ctor: 'GeoPoints', _0: a};
};
var _gicentre$elm_vega$VegaLite$geoPoints = _gicentre$elm_vega$VegaLite$GeoPoints;
var _gicentre$elm_vega$VegaLite$GeoPoint = F2(
	function (a, b) {
		return {ctor: 'GeoPoint', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$geoPoint = _gicentre$elm_vega$VegaLite$GeoPoint;
var _gicentre$elm_vega$VegaLite$AlignRight = {ctor: 'AlignRight'};
var _gicentre$elm_vega$VegaLite$AlignLeft = {ctor: 'AlignLeft'};
var _gicentre$elm_vega$VegaLite$AlignCenter = {ctor: 'AlignCenter'};
var _gicentre$elm_vega$VegaLite$HTitle = function (a) {
	return {ctor: 'HTitle', _0: a};
};
var _gicentre$elm_vega$VegaLite$hdTitle = _gicentre$elm_vega$VegaLite$HTitle;
var _gicentre$elm_vega$VegaLite$HFormat = function (a) {
	return {ctor: 'HFormat', _0: a};
};
var _gicentre$elm_vega$VegaLite$hdFormat = _gicentre$elm_vega$VegaLite$HFormat;
var _gicentre$elm_vega$VegaLite$HString = function (a) {
	return {ctor: 'HString', _0: a};
};
var _gicentre$elm_vega$VegaLite$hStr = _gicentre$elm_vega$VegaLite$HString;
var _gicentre$elm_vega$VegaLite$HDataCondition = F3(
	function (a, b, c) {
		return {ctor: 'HDataCondition', _0: a, _1: b, _2: c};
	});
var _gicentre$elm_vega$VegaLite$hDataCondition = F3(
	function (op, tCh, fCh) {
		return A3(_gicentre$elm_vega$VegaLite$HDataCondition, op, tCh, fCh);
	});
var _gicentre$elm_vega$VegaLite$HSelectionCondition = F3(
	function (a, b, c) {
		return {ctor: 'HSelectionCondition', _0: a, _1: b, _2: c};
	});
var _gicentre$elm_vega$VegaLite$hSelectionCondition = F3(
	function (op, tCh, fCh) {
		return A3(_gicentre$elm_vega$VegaLite$HSelectionCondition, op, tCh, fCh);
	});
var _gicentre$elm_vega$VegaLite$HTimeUnit = function (a) {
	return {ctor: 'HTimeUnit', _0: a};
};
var _gicentre$elm_vega$VegaLite$hTimeUnit = _gicentre$elm_vega$VegaLite$HTimeUnit;
var _gicentre$elm_vega$VegaLite$HAggregate = function (a) {
	return {ctor: 'HAggregate', _0: a};
};
var _gicentre$elm_vega$VegaLite$hAggregate = _gicentre$elm_vega$VegaLite$HAggregate;
var _gicentre$elm_vega$VegaLite$HBin = function (a) {
	return {ctor: 'HBin', _0: a};
};
var _gicentre$elm_vega$VegaLite$hBin = _gicentre$elm_vega$VegaLite$HBin;
var _gicentre$elm_vega$VegaLite$HmType = function (a) {
	return {ctor: 'HmType', _0: a};
};
var _gicentre$elm_vega$VegaLite$hMType = _gicentre$elm_vega$VegaLite$HmType;
var _gicentre$elm_vega$VegaLite$HRepeat = function (a) {
	return {ctor: 'HRepeat', _0: a};
};
var _gicentre$elm_vega$VegaLite$hRepeat = _gicentre$elm_vega$VegaLite$HRepeat;
var _gicentre$elm_vega$VegaLite$HName = function (a) {
	return {ctor: 'HName', _0: a};
};
var _gicentre$elm_vega$VegaLite$hName = _gicentre$elm_vega$VegaLite$HName;
var _gicentre$elm_vega$VegaLite$InPlaceholder = function (a) {
	return {ctor: 'InPlaceholder', _0: a};
};
var _gicentre$elm_vega$VegaLite$inPlaceholder = _gicentre$elm_vega$VegaLite$InPlaceholder;
var _gicentre$elm_vega$VegaLite$InStep = function (a) {
	return {ctor: 'InStep', _0: a};
};
var _gicentre$elm_vega$VegaLite$inStep = _gicentre$elm_vega$VegaLite$InStep;
var _gicentre$elm_vega$VegaLite$InName = function (a) {
	return {ctor: 'InName', _0: a};
};
var _gicentre$elm_vega$VegaLite$inName = _gicentre$elm_vega$VegaLite$InName;
var _gicentre$elm_vega$VegaLite$InMax = function (a) {
	return {ctor: 'InMax', _0: a};
};
var _gicentre$elm_vega$VegaLite$inMax = _gicentre$elm_vega$VegaLite$InMax;
var _gicentre$elm_vega$VegaLite$InMin = function (a) {
	return {ctor: 'InMin', _0: a};
};
var _gicentre$elm_vega$VegaLite$inMin = _gicentre$elm_vega$VegaLite$InMin;
var _gicentre$elm_vega$VegaLite$InOptions = function (a) {
	return {ctor: 'InOptions', _0: a};
};
var _gicentre$elm_vega$VegaLite$inOptions = _gicentre$elm_vega$VegaLite$InOptions;
var _gicentre$elm_vega$VegaLite$Element = function (a) {
	return {ctor: 'Element', _0: a};
};
var _gicentre$elm_vega$VegaLite$inElement = _gicentre$elm_vega$VegaLite$Element;
var _gicentre$elm_vega$VegaLite$Debounce = function (a) {
	return {ctor: 'Debounce', _0: a};
};
var _gicentre$elm_vega$VegaLite$inDebounce = _gicentre$elm_vega$VegaLite$Debounce;
var _gicentre$elm_vega$VegaLite$Symbol = {ctor: 'Symbol'};
var _gicentre$elm_vega$VegaLite$Gradient = {ctor: 'Gradient'};
var _gicentre$elm_vega$VegaLite$LeTitlePadding = function (a) {
	return {ctor: 'LeTitlePadding', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoTitlePadding = _gicentre$elm_vega$VegaLite$LeTitlePadding;
var _gicentre$elm_vega$VegaLite$LeTitleLimit = function (a) {
	return {ctor: 'LeTitleLimit', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoTitleLimit = _gicentre$elm_vega$VegaLite$LeTitleLimit;
var _gicentre$elm_vega$VegaLite$LeTitleFontWeight = function (a) {
	return {ctor: 'LeTitleFontWeight', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoTitleFontWeight = _gicentre$elm_vega$VegaLite$LeTitleFontWeight;
var _gicentre$elm_vega$VegaLite$LeTitleFontSize = function (a) {
	return {ctor: 'LeTitleFontSize', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoTitleFontSize = _gicentre$elm_vega$VegaLite$LeTitleFontSize;
var _gicentre$elm_vega$VegaLite$LeTitleFont = function (a) {
	return {ctor: 'LeTitleFont', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoTitleFont = _gicentre$elm_vega$VegaLite$LeTitleFont;
var _gicentre$elm_vega$VegaLite$LeTitleColor = function (a) {
	return {ctor: 'LeTitleColor', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoTitleColor = _gicentre$elm_vega$VegaLite$LeTitleColor;
var _gicentre$elm_vega$VegaLite$LeTitleBaseline = function (a) {
	return {ctor: 'LeTitleBaseline', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoTitleBaseline = _gicentre$elm_vega$VegaLite$LeTitleBaseline;
var _gicentre$elm_vega$VegaLite$LeTitleAlign = function (a) {
	return {ctor: 'LeTitleAlign', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoTitleAlign = _gicentre$elm_vega$VegaLite$LeTitleAlign;
var _gicentre$elm_vega$VegaLite$SymbolStrokeWidth = function (a) {
	return {ctor: 'SymbolStrokeWidth', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoSymbolStrokeWidth = _gicentre$elm_vega$VegaLite$SymbolStrokeWidth;
var _gicentre$elm_vega$VegaLite$SymbolSize = function (a) {
	return {ctor: 'SymbolSize', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoSymbolSize = _gicentre$elm_vega$VegaLite$SymbolSize;
var _gicentre$elm_vega$VegaLite$SymbolType = function (a) {
	return {ctor: 'SymbolType', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoSymbolType = _gicentre$elm_vega$VegaLite$SymbolType;
var _gicentre$elm_vega$VegaLite$SymbolColor = function (a) {
	return {ctor: 'SymbolColor', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoSymbolColor = _gicentre$elm_vega$VegaLite$SymbolColor;
var _gicentre$elm_vega$VegaLite$EntryPadding = function (a) {
	return {ctor: 'EntryPadding', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoEntryPadding = _gicentre$elm_vega$VegaLite$EntryPadding;
var _gicentre$elm_vega$VegaLite$LeShortTimeLabels = function (a) {
	return {ctor: 'LeShortTimeLabels', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoShortTimeLabels = _gicentre$elm_vega$VegaLite$LeShortTimeLabels;
var _gicentre$elm_vega$VegaLite$LeLabelOffset = function (a) {
	return {ctor: 'LeLabelOffset', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoLabelOffset = _gicentre$elm_vega$VegaLite$LeLabelOffset;
var _gicentre$elm_vega$VegaLite$LeLabelLimit = function (a) {
	return {ctor: 'LeLabelLimit', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoLabelLimit = _gicentre$elm_vega$VegaLite$LeLabelLimit;
var _gicentre$elm_vega$VegaLite$LeLabelFontSize = function (a) {
	return {ctor: 'LeLabelFontSize', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoLabelFontSize = _gicentre$elm_vega$VegaLite$LeLabelFontSize;
var _gicentre$elm_vega$VegaLite$LeLabelFont = function (a) {
	return {ctor: 'LeLabelFont', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoLabelFont = _gicentre$elm_vega$VegaLite$LeLabelFont;
var _gicentre$elm_vega$VegaLite$LeLabelColor = function (a) {
	return {ctor: 'LeLabelColor', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoLabelColor = _gicentre$elm_vega$VegaLite$LeLabelColor;
var _gicentre$elm_vega$VegaLite$LeLabelBaseline = function (a) {
	return {ctor: 'LeLabelBaseline', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoLabelBaseline = _gicentre$elm_vega$VegaLite$LeLabelBaseline;
var _gicentre$elm_vega$VegaLite$LeLabelAlign = function (a) {
	return {ctor: 'LeLabelAlign', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoLabelAlign = _gicentre$elm_vega$VegaLite$LeLabelAlign;
var _gicentre$elm_vega$VegaLite$GradientWidth = function (a) {
	return {ctor: 'GradientWidth', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoGradientWidth = _gicentre$elm_vega$VegaLite$GradientWidth;
var _gicentre$elm_vega$VegaLite$GradientHeight = function (a) {
	return {ctor: 'GradientHeight', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoGradientHeight = _gicentre$elm_vega$VegaLite$GradientHeight;
var _gicentre$elm_vega$VegaLite$GradientStrokeWidth = function (a) {
	return {ctor: 'GradientStrokeWidth', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoGradientStrokeWidth = _gicentre$elm_vega$VegaLite$GradientStrokeWidth;
var _gicentre$elm_vega$VegaLite$GradientStrokeColor = function (a) {
	return {ctor: 'GradientStrokeColor', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoGradientStrokeColor = _gicentre$elm_vega$VegaLite$GradientStrokeColor;
var _gicentre$elm_vega$VegaLite$GradientLabelOffset = function (a) {
	return {ctor: 'GradientLabelOffset', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoGradientLabelOffset = _gicentre$elm_vega$VegaLite$GradientLabelOffset;
var _gicentre$elm_vega$VegaLite$GradientLabelLimit = function (a) {
	return {ctor: 'GradientLabelLimit', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoGradientLabelLimit = _gicentre$elm_vega$VegaLite$GradientLabelLimit;
var _gicentre$elm_vega$VegaLite$GradientLabelBaseline = function (a) {
	return {ctor: 'GradientLabelBaseline', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoGradientLabelBaseline = _gicentre$elm_vega$VegaLite$GradientLabelBaseline;
var _gicentre$elm_vega$VegaLite$LePadding = function (a) {
	return {ctor: 'LePadding', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoPadding = _gicentre$elm_vega$VegaLite$LePadding;
var _gicentre$elm_vega$VegaLite$LeStrokeWidth = function (a) {
	return {ctor: 'LeStrokeWidth', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoStrokeWidth = _gicentre$elm_vega$VegaLite$LeStrokeWidth;
var _gicentre$elm_vega$VegaLite$LeStrokeDash = function (a) {
	return {ctor: 'LeStrokeDash', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoStrokeDash = _gicentre$elm_vega$VegaLite$LeStrokeDash;
var _gicentre$elm_vega$VegaLite$StrokeColor = function (a) {
	return {ctor: 'StrokeColor', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoStrokeColor = _gicentre$elm_vega$VegaLite$StrokeColor;
var _gicentre$elm_vega$VegaLite$Offset = function (a) {
	return {ctor: 'Offset', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoOffset = _gicentre$elm_vega$VegaLite$Offset;
var _gicentre$elm_vega$VegaLite$Orient = function (a) {
	return {ctor: 'Orient', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoOrient = _gicentre$elm_vega$VegaLite$Orient;
var _gicentre$elm_vega$VegaLite$FillColor = function (a) {
	return {ctor: 'FillColor', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoFillColor = _gicentre$elm_vega$VegaLite$FillColor;
var _gicentre$elm_vega$VegaLite$CornerRadius = function (a) {
	return {ctor: 'CornerRadius', _0: a};
};
var _gicentre$elm_vega$VegaLite$lecoCornerRadius = _gicentre$elm_vega$VegaLite$CornerRadius;
var _gicentre$elm_vega$VegaLite$TopRight = {ctor: 'TopRight'};
var _gicentre$elm_vega$VegaLite$TopLeft = {ctor: 'TopLeft'};
var _gicentre$elm_vega$VegaLite$Right = {ctor: 'Right'};
var _gicentre$elm_vega$VegaLite$None = {ctor: 'None'};
var _gicentre$elm_vega$VegaLite$Left = {ctor: 'Left'};
var _gicentre$elm_vega$VegaLite$BottomRight = {ctor: 'BottomRight'};
var _gicentre$elm_vega$VegaLite$BottomLeft = {ctor: 'BottomLeft'};
var _gicentre$elm_vega$VegaLite$LZIndex = function (a) {
	return {ctor: 'LZIndex', _0: a};
};
var _gicentre$elm_vega$VegaLite$leZIndex = _gicentre$elm_vega$VegaLite$LZIndex;
var _gicentre$elm_vega$VegaLite$LValues = function (a) {
	return {ctor: 'LValues', _0: a};
};
var _gicentre$elm_vega$VegaLite$leValues = _gicentre$elm_vega$VegaLite$LValues;
var _gicentre$elm_vega$VegaLite$LType = function (a) {
	return {ctor: 'LType', _0: a};
};
var _gicentre$elm_vega$VegaLite$leType = _gicentre$elm_vega$VegaLite$LType;
var _gicentre$elm_vega$VegaLite$LTitle = function (a) {
	return {ctor: 'LTitle', _0: a};
};
var _gicentre$elm_vega$VegaLite$leTitle = _gicentre$elm_vega$VegaLite$LTitle;
var _gicentre$elm_vega$VegaLite$LTickCount = function (a) {
	return {ctor: 'LTickCount', _0: a};
};
var _gicentre$elm_vega$VegaLite$leTickCount = _gicentre$elm_vega$VegaLite$LTickCount;
var _gicentre$elm_vega$VegaLite$LPadding = function (a) {
	return {ctor: 'LPadding', _0: a};
};
var _gicentre$elm_vega$VegaLite$lePadding = _gicentre$elm_vega$VegaLite$LPadding;
var _gicentre$elm_vega$VegaLite$LOrient = function (a) {
	return {ctor: 'LOrient', _0: a};
};
var _gicentre$elm_vega$VegaLite$leOrient = _gicentre$elm_vega$VegaLite$LOrient;
var _gicentre$elm_vega$VegaLite$LOffset = function (a) {
	return {ctor: 'LOffset', _0: a};
};
var _gicentre$elm_vega$VegaLite$leOffset = _gicentre$elm_vega$VegaLite$LOffset;
var _gicentre$elm_vega$VegaLite$LFormat = function (a) {
	return {ctor: 'LFormat', _0: a};
};
var _gicentre$elm_vega$VegaLite$leFormat = _gicentre$elm_vega$VegaLite$LFormat;
var _gicentre$elm_vega$VegaLite$LEntryPadding = function (a) {
	return {ctor: 'LEntryPadding', _0: a};
};
var _gicentre$elm_vega$VegaLite$leEntryPadding = _gicentre$elm_vega$VegaLite$LEntryPadding;
var _gicentre$elm_vega$VegaLite$LStrings = function (a) {
	return {ctor: 'LStrings', _0: a};
};
var _gicentre$elm_vega$VegaLite$leStrs = _gicentre$elm_vega$VegaLite$LStrings;
var _gicentre$elm_vega$VegaLite$LNumbers = function (a) {
	return {ctor: 'LNumbers', _0: a};
};
var _gicentre$elm_vega$VegaLite$leNums = _gicentre$elm_vega$VegaLite$LNumbers;
var _gicentre$elm_vega$VegaLite$LDateTimes = function (a) {
	return {ctor: 'LDateTimes', _0: a};
};
var _gicentre$elm_vega$VegaLite$leDts = _gicentre$elm_vega$VegaLite$LDateTimes;
var _gicentre$elm_vega$VegaLite$Trail = {ctor: 'Trail'};
var _gicentre$elm_vega$VegaLite$Tick = {ctor: 'Tick'};
var _gicentre$elm_vega$VegaLite$Text = {ctor: 'Text'};
var _gicentre$elm_vega$VegaLite$Square = {ctor: 'Square'};
var _gicentre$elm_vega$VegaLite$Rule = {ctor: 'Rule'};
var _gicentre$elm_vega$VegaLite$Rect = {ctor: 'Rect'};
var _gicentre$elm_vega$VegaLite$Point = {ctor: 'Point'};
var _gicentre$elm_vega$VegaLite$Line = {ctor: 'Line'};
var _gicentre$elm_vega$VegaLite$Geoshape = {ctor: 'Geoshape'};
var _gicentre$elm_vega$VegaLite$Circle = {ctor: 'Circle'};
var _gicentre$elm_vega$VegaLite$Bar = {ctor: 'Bar'};
var _gicentre$elm_vega$VegaLite$Area = {ctor: 'Area'};
var _gicentre$elm_vega$VegaLite$MBoolean = function (a) {
	return {ctor: 'MBoolean', _0: a};
};
var _gicentre$elm_vega$VegaLite$mBoo = _gicentre$elm_vega$VegaLite$MBoolean;
var _gicentre$elm_vega$VegaLite$MString = function (a) {
	return {ctor: 'MString', _0: a};
};
var _gicentre$elm_vega$VegaLite$mStr = _gicentre$elm_vega$VegaLite$MString;
var _gicentre$elm_vega$VegaLite$MNumber = function (a) {
	return {ctor: 'MNumber', _0: a};
};
var _gicentre$elm_vega$VegaLite$mNum = _gicentre$elm_vega$VegaLite$MNumber;
var _gicentre$elm_vega$VegaLite$MPath = function (a) {
	return {ctor: 'MPath', _0: a};
};
var _gicentre$elm_vega$VegaLite$mPath = _gicentre$elm_vega$VegaLite$MPath;
var _gicentre$elm_vega$VegaLite$MDataCondition = F3(
	function (a, b, c) {
		return {ctor: 'MDataCondition', _0: a, _1: b, _2: c};
	});
var _gicentre$elm_vega$VegaLite$mDataCondition = F3(
	function (op, tMks, fMks) {
		return A3(_gicentre$elm_vega$VegaLite$MDataCondition, op, tMks, fMks);
	});
var _gicentre$elm_vega$VegaLite$MSelectionCondition = F3(
	function (a, b, c) {
		return {ctor: 'MSelectionCondition', _0: a, _1: b, _2: c};
	});
var _gicentre$elm_vega$VegaLite$mSelectionCondition = F3(
	function (op, tMks, fMks) {
		return A3(_gicentre$elm_vega$VegaLite$MSelectionCondition, op, tMks, fMks);
	});
var _gicentre$elm_vega$VegaLite$MLegend = function (a) {
	return {ctor: 'MLegend', _0: a};
};
var _gicentre$elm_vega$VegaLite$mLegend = _gicentre$elm_vega$VegaLite$MLegend;
var _gicentre$elm_vega$VegaLite$MAggregate = function (a) {
	return {ctor: 'MAggregate', _0: a};
};
var _gicentre$elm_vega$VegaLite$mAggregate = _gicentre$elm_vega$VegaLite$MAggregate;
var _gicentre$elm_vega$VegaLite$MTimeUnit = function (a) {
	return {ctor: 'MTimeUnit', _0: a};
};
var _gicentre$elm_vega$VegaLite$mTimeUnit = _gicentre$elm_vega$VegaLite$MTimeUnit;
var _gicentre$elm_vega$VegaLite$MBin = function (a) {
	return {ctor: 'MBin', _0: a};
};
var _gicentre$elm_vega$VegaLite$mBin = _gicentre$elm_vega$VegaLite$MBin;
var _gicentre$elm_vega$VegaLite$MScale = function (a) {
	return {ctor: 'MScale', _0: a};
};
var _gicentre$elm_vega$VegaLite$mScale = _gicentre$elm_vega$VegaLite$MScale;
var _gicentre$elm_vega$VegaLite$MmType = function (a) {
	return {ctor: 'MmType', _0: a};
};
var _gicentre$elm_vega$VegaLite$mMType = _gicentre$elm_vega$VegaLite$MmType;
var _gicentre$elm_vega$VegaLite$MRepeat = function (a) {
	return {ctor: 'MRepeat', _0: a};
};
var _gicentre$elm_vega$VegaLite$mRepeat = _gicentre$elm_vega$VegaLite$MRepeat;
var _gicentre$elm_vega$VegaLite$MName = function (a) {
	return {ctor: 'MName', _0: a};
};
var _gicentre$elm_vega$VegaLite$mName = _gicentre$elm_vega$VegaLite$MName;
var _gicentre$elm_vega$VegaLite$Stepwise = {ctor: 'Stepwise'};
var _gicentre$elm_vega$VegaLite$StepBefore = {ctor: 'StepBefore'};
var _gicentre$elm_vega$VegaLite$StepAfter = {ctor: 'StepAfter'};
var _gicentre$elm_vega$VegaLite$Monotone = {ctor: 'Monotone'};
var _gicentre$elm_vega$VegaLite$LinearClosed = {ctor: 'LinearClosed'};
var _gicentre$elm_vega$VegaLite$Linear = {ctor: 'Linear'};
var _gicentre$elm_vega$VegaLite$CardinalOpen = {ctor: 'CardinalOpen'};
var _gicentre$elm_vega$VegaLite$CardinalClosed = {ctor: 'CardinalClosed'};
var _gicentre$elm_vega$VegaLite$Cardinal = {ctor: 'Cardinal'};
var _gicentre$elm_vega$VegaLite$Bundle = {ctor: 'Bundle'};
var _gicentre$elm_vega$VegaLite$BasisOpen = {ctor: 'BasisOpen'};
var _gicentre$elm_vega$VegaLite$BasisClosed = {ctor: 'BasisClosed'};
var _gicentre$elm_vega$VegaLite$Basis = {ctor: 'Basis'};
var _gicentre$elm_vega$VegaLite$Vertical = {ctor: 'Vertical'};
var _gicentre$elm_vega$VegaLite$Horizontal = {ctor: 'Horizontal'};
var _gicentre$elm_vega$VegaLite$MThickness = function (a) {
	return {ctor: 'MThickness', _0: a};
};
var _gicentre$elm_vega$VegaLite$maThickness = _gicentre$elm_vega$VegaLite$MThickness;
var _gicentre$elm_vega$VegaLite$MTheta = function (a) {
	return {ctor: 'MTheta', _0: a};
};
var _gicentre$elm_vega$VegaLite$maTheta = _gicentre$elm_vega$VegaLite$MTheta;
var _gicentre$elm_vega$VegaLite$MText = function (a) {
	return {ctor: 'MText', _0: a};
};
var _gicentre$elm_vega$VegaLite$maText = _gicentre$elm_vega$VegaLite$MText;
var _gicentre$elm_vega$VegaLite$MTension = function (a) {
	return {ctor: 'MTension', _0: a};
};
var _gicentre$elm_vega$VegaLite$maTension = _gicentre$elm_vega$VegaLite$MTension;
var _gicentre$elm_vega$VegaLite$MStyle = function (a) {
	return {ctor: 'MStyle', _0: a};
};
var _gicentre$elm_vega$VegaLite$maStyle = _gicentre$elm_vega$VegaLite$MStyle;
var _gicentre$elm_vega$VegaLite$MStrokeWidth = function (a) {
	return {ctor: 'MStrokeWidth', _0: a};
};
var _gicentre$elm_vega$VegaLite$maStrokeWidth = _gicentre$elm_vega$VegaLite$MStrokeWidth;
var _gicentre$elm_vega$VegaLite$MStrokeOpacity = function (a) {
	return {ctor: 'MStrokeOpacity', _0: a};
};
var _gicentre$elm_vega$VegaLite$maStrokeOpacity = _gicentre$elm_vega$VegaLite$MStrokeOpacity;
var _gicentre$elm_vega$VegaLite$MStrokeDashOffset = function (a) {
	return {ctor: 'MStrokeDashOffset', _0: a};
};
var _gicentre$elm_vega$VegaLite$maStrokeDashOffset = _gicentre$elm_vega$VegaLite$MStrokeDashOffset;
var _gicentre$elm_vega$VegaLite$MStrokeDash = function (a) {
	return {ctor: 'MStrokeDash', _0: a};
};
var _gicentre$elm_vega$VegaLite$maStrokeDash = _gicentre$elm_vega$VegaLite$MStrokeDash;
var _gicentre$elm_vega$VegaLite$MStroke = function (a) {
	return {ctor: 'MStroke', _0: a};
};
var _gicentre$elm_vega$VegaLite$maStroke = _gicentre$elm_vega$VegaLite$MStroke;
var _gicentre$elm_vega$VegaLite$MSize = function (a) {
	return {ctor: 'MSize', _0: a};
};
var _gicentre$elm_vega$VegaLite$maSize = _gicentre$elm_vega$VegaLite$MSize;
var _gicentre$elm_vega$VegaLite$MShortTimeLabels = function (a) {
	return {ctor: 'MShortTimeLabels', _0: a};
};
var _gicentre$elm_vega$VegaLite$maShortTimeLabels = _gicentre$elm_vega$VegaLite$MShortTimeLabels;
var _gicentre$elm_vega$VegaLite$MShape = function (a) {
	return {ctor: 'MShape', _0: a};
};
var _gicentre$elm_vega$VegaLite$maShape = _gicentre$elm_vega$VegaLite$MShape;
var _gicentre$elm_vega$VegaLite$MRadius = function (a) {
	return {ctor: 'MRadius', _0: a};
};
var _gicentre$elm_vega$VegaLite$maRadius = _gicentre$elm_vega$VegaLite$MRadius;
var _gicentre$elm_vega$VegaLite$MPoint = function (a) {
	return {ctor: 'MPoint', _0: a};
};
var _gicentre$elm_vega$VegaLite$maPoint = _gicentre$elm_vega$VegaLite$MPoint;
var _gicentre$elm_vega$VegaLite$MOrient = function (a) {
	return {ctor: 'MOrient', _0: a};
};
var _gicentre$elm_vega$VegaLite$maOrient = _gicentre$elm_vega$VegaLite$MOrient;
var _gicentre$elm_vega$VegaLite$MOpacity = function (a) {
	return {ctor: 'MOpacity', _0: a};
};
var _gicentre$elm_vega$VegaLite$maOpacity = _gicentre$elm_vega$VegaLite$MOpacity;
var _gicentre$elm_vega$VegaLite$MInterpolate = function (a) {
	return {ctor: 'MInterpolate', _0: a};
};
var _gicentre$elm_vega$VegaLite$maInterpolate = _gicentre$elm_vega$VegaLite$MInterpolate;
var _gicentre$elm_vega$VegaLite$MFontWeight = function (a) {
	return {ctor: 'MFontWeight', _0: a};
};
var _gicentre$elm_vega$VegaLite$maFontWeight = _gicentre$elm_vega$VegaLite$MFontWeight;
var _gicentre$elm_vega$VegaLite$MFontStyle = function (a) {
	return {ctor: 'MFontStyle', _0: a};
};
var _gicentre$elm_vega$VegaLite$maFontStyle = _gicentre$elm_vega$VegaLite$MFontStyle;
var _gicentre$elm_vega$VegaLite$MFontSize = function (a) {
	return {ctor: 'MFontSize', _0: a};
};
var _gicentre$elm_vega$VegaLite$maFontSize = _gicentre$elm_vega$VegaLite$MFontSize;
var _gicentre$elm_vega$VegaLite$MFont = function (a) {
	return {ctor: 'MFont', _0: a};
};
var _gicentre$elm_vega$VegaLite$maFont = _gicentre$elm_vega$VegaLite$MFont;
var _gicentre$elm_vega$VegaLite$MFillOpacity = function (a) {
	return {ctor: 'MFillOpacity', _0: a};
};
var _gicentre$elm_vega$VegaLite$maFillOpacity = _gicentre$elm_vega$VegaLite$MFillOpacity;
var _gicentre$elm_vega$VegaLite$MFilled = function (a) {
	return {ctor: 'MFilled', _0: a};
};
var _gicentre$elm_vega$VegaLite$maFilled = _gicentre$elm_vega$VegaLite$MFilled;
var _gicentre$elm_vega$VegaLite$MFill = function (a) {
	return {ctor: 'MFill', _0: a};
};
var _gicentre$elm_vega$VegaLite$maFill = _gicentre$elm_vega$VegaLite$MFill;
var _gicentre$elm_vega$VegaLite$MdY = function (a) {
	return {ctor: 'MdY', _0: a};
};
var _gicentre$elm_vega$VegaLite$maDy = _gicentre$elm_vega$VegaLite$MdY;
var _gicentre$elm_vega$VegaLite$MdX = function (a) {
	return {ctor: 'MdX', _0: a};
};
var _gicentre$elm_vega$VegaLite$maDx = _gicentre$elm_vega$VegaLite$MdX;
var _gicentre$elm_vega$VegaLite$MDiscreteBandSize = function (a) {
	return {ctor: 'MDiscreteBandSize', _0: a};
};
var _gicentre$elm_vega$VegaLite$maDiscreteBandSize = _gicentre$elm_vega$VegaLite$MDiscreteBandSize;
var _gicentre$elm_vega$VegaLite$MContinuousBandSize = function (a) {
	return {ctor: 'MContinuousBandSize', _0: a};
};
var _gicentre$elm_vega$VegaLite$maContinuousBandSize = _gicentre$elm_vega$VegaLite$MContinuousBandSize;
var _gicentre$elm_vega$VegaLite$MCursor = function (a) {
	return {ctor: 'MCursor', _0: a};
};
var _gicentre$elm_vega$VegaLite$maCursor = _gicentre$elm_vega$VegaLite$MCursor;
var _gicentre$elm_vega$VegaLite$MColor = function (a) {
	return {ctor: 'MColor', _0: a};
};
var _gicentre$elm_vega$VegaLite$maColor = _gicentre$elm_vega$VegaLite$MColor;
var _gicentre$elm_vega$VegaLite$MClip = function (a) {
	return {ctor: 'MClip', _0: a};
};
var _gicentre$elm_vega$VegaLite$maClip = _gicentre$elm_vega$VegaLite$MClip;
var _gicentre$elm_vega$VegaLite$MBinSpacing = function (a) {
	return {ctor: 'MBinSpacing', _0: a};
};
var _gicentre$elm_vega$VegaLite$maBinSpacing = _gicentre$elm_vega$VegaLite$MBinSpacing;
var _gicentre$elm_vega$VegaLite$MBaseline = function (a) {
	return {ctor: 'MBaseline', _0: a};
};
var _gicentre$elm_vega$VegaLite$maBaseline = _gicentre$elm_vega$VegaLite$MBaseline;
var _gicentre$elm_vega$VegaLite$MBandSize = function (a) {
	return {ctor: 'MBandSize', _0: a};
};
var _gicentre$elm_vega$VegaLite$maBandSize = _gicentre$elm_vega$VegaLite$MBandSize;
var _gicentre$elm_vega$VegaLite$MAngle = function (a) {
	return {ctor: 'MAngle', _0: a};
};
var _gicentre$elm_vega$VegaLite$maAngle = _gicentre$elm_vega$VegaLite$MAngle;
var _gicentre$elm_vega$VegaLite$MAlign = function (a) {
	return {ctor: 'MAlign', _0: a};
};
var _gicentre$elm_vega$VegaLite$maAlign = _gicentre$elm_vega$VegaLite$MAlign;
var _gicentre$elm_vega$VegaLite$GeoFeature = {ctor: 'GeoFeature'};
var _gicentre$elm_vega$VegaLite$Temporal = {ctor: 'Temporal'};
var _gicentre$elm_vega$VegaLite$Quantitative = {ctor: 'Quantitative'};
var _gicentre$elm_vega$VegaLite$Ordinal = {ctor: 'Ordinal'};
var _gicentre$elm_vega$VegaLite$Nominal = {ctor: 'Nominal'};
var _gicentre$elm_vega$VegaLite$Dec = {ctor: 'Dec'};
var _gicentre$elm_vega$VegaLite$Nov = {ctor: 'Nov'};
var _gicentre$elm_vega$VegaLite$Oct = {ctor: 'Oct'};
var _gicentre$elm_vega$VegaLite$Sep = {ctor: 'Sep'};
var _gicentre$elm_vega$VegaLite$Aug = {ctor: 'Aug'};
var _gicentre$elm_vega$VegaLite$Jul = {ctor: 'Jul'};
var _gicentre$elm_vega$VegaLite$Jun = {ctor: 'Jun'};
var _gicentre$elm_vega$VegaLite$May = {ctor: 'May'};
var _gicentre$elm_vega$VegaLite$Apr = {ctor: 'Apr'};
var _gicentre$elm_vega$VegaLite$Mar = {ctor: 'Mar'};
var _gicentre$elm_vega$VegaLite$Feb = {ctor: 'Feb'};
var _gicentre$elm_vega$VegaLite$Jan = {ctor: 'Jan'};
var _gicentre$elm_vega$VegaLite$VarianceP = {ctor: 'VarianceP'};
var _gicentre$elm_vega$VegaLite$Variance = {ctor: 'Variance'};
var _gicentre$elm_vega$VegaLite$Valid = {ctor: 'Valid'};
var _gicentre$elm_vega$VegaLite$Sum = {ctor: 'Sum'};
var _gicentre$elm_vega$VegaLite$StdevP = {ctor: 'StdevP'};
var _gicentre$elm_vega$VegaLite$Stdev = {ctor: 'Stdev'};
var _gicentre$elm_vega$VegaLite$Stderr = {ctor: 'Stderr'};
var _gicentre$elm_vega$VegaLite$Q3 = {ctor: 'Q3'};
var _gicentre$elm_vega$VegaLite$Q1 = {ctor: 'Q1'};
var _gicentre$elm_vega$VegaLite$Missing = {ctor: 'Missing'};
var _gicentre$elm_vega$VegaLite$Min = {ctor: 'Min'};
var _gicentre$elm_vega$VegaLite$Median = {ctor: 'Median'};
var _gicentre$elm_vega$VegaLite$Mean = {ctor: 'Mean'};
var _gicentre$elm_vega$VegaLite$Max = {ctor: 'Max'};
var _gicentre$elm_vega$VegaLite$Distinct = {ctor: 'Distinct'};
var _gicentre$elm_vega$VegaLite$Count = {ctor: 'Count'};
var _gicentre$elm_vega$VegaLite$CI1 = {ctor: 'CI1'};
var _gicentre$elm_vega$VegaLite$CI0 = {ctor: 'CI0'};
var _gicentre$elm_vega$VegaLite$Average = {ctor: 'Average'};
var _gicentre$elm_vega$VegaLite$ArgMin = {ctor: 'ArgMin'};
var _gicentre$elm_vega$VegaLite$ArgMax = {ctor: 'ArgMax'};
var _gicentre$elm_vega$VegaLite$OSort = function (a) {
	return {ctor: 'OSort', _0: a};
};
var _gicentre$elm_vega$VegaLite$oSort = _gicentre$elm_vega$VegaLite$OSort;
var _gicentre$elm_vega$VegaLite$OTimeUnit = function (a) {
	return {ctor: 'OTimeUnit', _0: a};
};
var _gicentre$elm_vega$VegaLite$oTimeUnit = _gicentre$elm_vega$VegaLite$OTimeUnit;
var _gicentre$elm_vega$VegaLite$OAggregate = function (a) {
	return {ctor: 'OAggregate', _0: a};
};
var _gicentre$elm_vega$VegaLite$oAggregate = _gicentre$elm_vega$VegaLite$OAggregate;
var _gicentre$elm_vega$VegaLite$OBin = function (a) {
	return {ctor: 'OBin', _0: a};
};
var _gicentre$elm_vega$VegaLite$oBin = _gicentre$elm_vega$VegaLite$OBin;
var _gicentre$elm_vega$VegaLite$OmType = function (a) {
	return {ctor: 'OmType', _0: a};
};
var _gicentre$elm_vega$VegaLite$oMType = _gicentre$elm_vega$VegaLite$OmType;
var _gicentre$elm_vega$VegaLite$ORepeat = function (a) {
	return {ctor: 'ORepeat', _0: a};
};
var _gicentre$elm_vega$VegaLite$oRepeat = _gicentre$elm_vega$VegaLite$ORepeat;
var _gicentre$elm_vega$VegaLite$OName = function (a) {
	return {ctor: 'OName', _0: a};
};
var _gicentre$elm_vega$VegaLite$oName = _gicentre$elm_vega$VegaLite$OName;
var _gicentre$elm_vega$VegaLite$OGreedy = {ctor: 'OGreedy'};
var _gicentre$elm_vega$VegaLite$OParity = {ctor: 'OParity'};
var _gicentre$elm_vega$VegaLite$ONone = {ctor: 'ONone'};
var _gicentre$elm_vega$VegaLite$PEdges = F4(
	function (a, b, c, d) {
		return {ctor: 'PEdges', _0: a, _1: b, _2: c, _3: d};
	});
var _gicentre$elm_vega$VegaLite$paEdges = _gicentre$elm_vega$VegaLite$PEdges;
var _gicentre$elm_vega$VegaLite$PSize = function (a) {
	return {ctor: 'PSize', _0: a};
};
var _gicentre$elm_vega$VegaLite$paSize = _gicentre$elm_vega$VegaLite$PSize;
var _gicentre$elm_vega$VegaLite$PMMarker = function (a) {
	return {ctor: 'PMMarker', _0: a};
};
var _gicentre$elm_vega$VegaLite$pmMarker = _gicentre$elm_vega$VegaLite$PMMarker;
var _gicentre$elm_vega$VegaLite$PMNone = {ctor: 'PMNone'};
var _gicentre$elm_vega$VegaLite$PMTransparent = {ctor: 'PMTransparent'};
var _gicentre$elm_vega$VegaLite$Latitude2 = {ctor: 'Latitude2'};
var _gicentre$elm_vega$VegaLite$Longitude2 = {ctor: 'Longitude2'};
var _gicentre$elm_vega$VegaLite$Latitude = {ctor: 'Latitude'};
var _gicentre$elm_vega$VegaLite$Longitude = {ctor: 'Longitude'};
var _gicentre$elm_vega$VegaLite$Y2 = {ctor: 'Y2'};
var _gicentre$elm_vega$VegaLite$X2 = {ctor: 'X2'};
var _gicentre$elm_vega$VegaLite$Y = {ctor: 'Y'};
var _gicentre$elm_vega$VegaLite$X = {ctor: 'X'};
var _gicentre$elm_vega$VegaLite$position = F2(
	function (pos, pDefs) {
		var isNotPmType = function (pp) {
			var _p134 = pp;
			if (_p134.ctor === 'PmType') {
				return false;
			} else {
				return true;
			}
		};
		var _p135 = pos;
		switch (_p135.ctor) {
			case 'X':
				return F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					})(
					{
						ctor: '_Tuple2',
						_0: _gicentre$elm_vega$VegaLite$positionLabel(_gicentre$elm_vega$VegaLite$X),
						_1: _elm_lang$core$Json_Encode$object(
							A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$positionChannelProperty, pDefs))
					});
			case 'Y':
				return F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					})(
					{
						ctor: '_Tuple2',
						_0: _gicentre$elm_vega$VegaLite$positionLabel(_gicentre$elm_vega$VegaLite$Y),
						_1: _elm_lang$core$Json_Encode$object(
							A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$positionChannelProperty, pDefs))
					});
			case 'X2':
				return F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					})(
					{
						ctor: '_Tuple2',
						_0: _gicentre$elm_vega$VegaLite$positionLabel(_gicentre$elm_vega$VegaLite$X2),
						_1: _elm_lang$core$Json_Encode$object(
							A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$positionChannelProperty, pDefs))
					});
			case 'Y2':
				return F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					})(
					{
						ctor: '_Tuple2',
						_0: _gicentre$elm_vega$VegaLite$positionLabel(_gicentre$elm_vega$VegaLite$Y2),
						_1: _elm_lang$core$Json_Encode$object(
							A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$positionChannelProperty, pDefs))
					});
			case 'Longitude':
				return F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					})(
					{
						ctor: '_Tuple2',
						_0: _gicentre$elm_vega$VegaLite$positionLabel(_gicentre$elm_vega$VegaLite$Longitude),
						_1: _elm_lang$core$Json_Encode$object(
							A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$positionChannelProperty, pDefs))
					});
			case 'Latitude':
				return F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					})(
					{
						ctor: '_Tuple2',
						_0: _gicentre$elm_vega$VegaLite$positionLabel(_gicentre$elm_vega$VegaLite$Latitude),
						_1: _elm_lang$core$Json_Encode$object(
							A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$positionChannelProperty, pDefs))
					});
			case 'Longitude2':
				return F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					})(
					{
						ctor: '_Tuple2',
						_0: _gicentre$elm_vega$VegaLite$positionLabel(_gicentre$elm_vega$VegaLite$Longitude2),
						_1: _elm_lang$core$Json_Encode$object(
							A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$positionChannelProperty, pDefs))
					});
			default:
				return F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					})(
					{
						ctor: '_Tuple2',
						_0: _gicentre$elm_vega$VegaLite$positionLabel(_gicentre$elm_vega$VegaLite$Latitude2),
						_1: _elm_lang$core$Json_Encode$object(
							A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$positionChannelProperty, pDefs))
					});
		}
	});
var _gicentre$elm_vega$VegaLite$PStack = function (a) {
	return {ctor: 'PStack', _0: a};
};
var _gicentre$elm_vega$VegaLite$pStack = _gicentre$elm_vega$VegaLite$PStack;
var _gicentre$elm_vega$VegaLite$PSort = function (a) {
	return {ctor: 'PSort', _0: a};
};
var _gicentre$elm_vega$VegaLite$pSort = _gicentre$elm_vega$VegaLite$PSort;
var _gicentre$elm_vega$VegaLite$PAxis = function (a) {
	return {ctor: 'PAxis', _0: a};
};
var _gicentre$elm_vega$VegaLite$pAxis = _gicentre$elm_vega$VegaLite$PAxis;
var _gicentre$elm_vega$VegaLite$PScale = function (a) {
	return {ctor: 'PScale', _0: a};
};
var _gicentre$elm_vega$VegaLite$pScale = _gicentre$elm_vega$VegaLite$PScale;
var _gicentre$elm_vega$VegaLite$PAggregate = function (a) {
	return {ctor: 'PAggregate', _0: a};
};
var _gicentre$elm_vega$VegaLite$pAggregate = _gicentre$elm_vega$VegaLite$PAggregate;
var _gicentre$elm_vega$VegaLite$PTimeUnit = function (a) {
	return {ctor: 'PTimeUnit', _0: a};
};
var _gicentre$elm_vega$VegaLite$pTimeUnit = _gicentre$elm_vega$VegaLite$PTimeUnit;
var _gicentre$elm_vega$VegaLite$PBin = function (a) {
	return {ctor: 'PBin', _0: a};
};
var _gicentre$elm_vega$VegaLite$pBin = _gicentre$elm_vega$VegaLite$PBin;
var _gicentre$elm_vega$VegaLite$PmType = function (a) {
	return {ctor: 'PmType', _0: a};
};
var _gicentre$elm_vega$VegaLite$pMType = _gicentre$elm_vega$VegaLite$PmType;
var _gicentre$elm_vega$VegaLite$PRepeat = function (a) {
	return {ctor: 'PRepeat', _0: a};
};
var _gicentre$elm_vega$VegaLite$pRepeat = _gicentre$elm_vega$VegaLite$PRepeat;
var _gicentre$elm_vega$VegaLite$PName = function (a) {
	return {ctor: 'PName', _0: a};
};
var _gicentre$elm_vega$VegaLite$pName = _gicentre$elm_vega$VegaLite$PName;
var _gicentre$elm_vega$VegaLite$TransverseMercator = {ctor: 'TransverseMercator'};
var _gicentre$elm_vega$VegaLite$Stereographic = {ctor: 'Stereographic'};
var _gicentre$elm_vega$VegaLite$Orthographic = {ctor: 'Orthographic'};
var _gicentre$elm_vega$VegaLite$Mercator = {ctor: 'Mercator'};
var _gicentre$elm_vega$VegaLite$Gnomonic = {ctor: 'Gnomonic'};
var _gicentre$elm_vega$VegaLite$Equirectangular = {ctor: 'Equirectangular'};
var _gicentre$elm_vega$VegaLite$Custom = function (a) {
	return {ctor: 'Custom', _0: a};
};
var _gicentre$elm_vega$VegaLite$customProjection = _gicentre$elm_vega$VegaLite$Custom;
var _gicentre$elm_vega$VegaLite$ConicEquidistant = {ctor: 'ConicEquidistant'};
var _gicentre$elm_vega$VegaLite$ConicEqualArea = {ctor: 'ConicEqualArea'};
var _gicentre$elm_vega$VegaLite$ConicConformal = {ctor: 'ConicConformal'};
var _gicentre$elm_vega$VegaLite$AzimuthalEquidistant = {ctor: 'AzimuthalEquidistant'};
var _gicentre$elm_vega$VegaLite$AzimuthalEqualArea = {ctor: 'AzimuthalEqualArea'};
var _gicentre$elm_vega$VegaLite$AlbersUsa = {ctor: 'AlbersUsa'};
var _gicentre$elm_vega$VegaLite$Albers = {ctor: 'Albers'};
var _gicentre$elm_vega$VegaLite$PTilt = function (a) {
	return {ctor: 'PTilt', _0: a};
};
var _gicentre$elm_vega$VegaLite$prTilt = _gicentre$elm_vega$VegaLite$PTilt;
var _gicentre$elm_vega$VegaLite$PSpacing = function (a) {
	return {ctor: 'PSpacing', _0: a};
};
var _gicentre$elm_vega$VegaLite$prSpacing = _gicentre$elm_vega$VegaLite$PSpacing;
var _gicentre$elm_vega$VegaLite$PRatio = function (a) {
	return {ctor: 'PRatio', _0: a};
};
var _gicentre$elm_vega$VegaLite$prRatio = _gicentre$elm_vega$VegaLite$PRatio;
var _gicentre$elm_vega$VegaLite$PRadius = function (a) {
	return {ctor: 'PRadius', _0: a};
};
var _gicentre$elm_vega$VegaLite$prRadius = _gicentre$elm_vega$VegaLite$PRadius;
var _gicentre$elm_vega$VegaLite$PParallel = function (a) {
	return {ctor: 'PParallel', _0: a};
};
var _gicentre$elm_vega$VegaLite$prParallel = _gicentre$elm_vega$VegaLite$PParallel;
var _gicentre$elm_vega$VegaLite$PLobes = function (a) {
	return {ctor: 'PLobes', _0: a};
};
var _gicentre$elm_vega$VegaLite$prLobes = _gicentre$elm_vega$VegaLite$PLobes;
var _gicentre$elm_vega$VegaLite$PFraction = function (a) {
	return {ctor: 'PFraction', _0: a};
};
var _gicentre$elm_vega$VegaLite$prFraction = _gicentre$elm_vega$VegaLite$PFraction;
var _gicentre$elm_vega$VegaLite$PDistance = function (a) {
	return {ctor: 'PDistance', _0: a};
};
var _gicentre$elm_vega$VegaLite$prDistance = _gicentre$elm_vega$VegaLite$PDistance;
var _gicentre$elm_vega$VegaLite$PCoefficient = function (a) {
	return {ctor: 'PCoefficient', _0: a};
};
var _gicentre$elm_vega$VegaLite$prCoefficient = _gicentre$elm_vega$VegaLite$PCoefficient;
var _gicentre$elm_vega$VegaLite$PPrecision = function (a) {
	return {ctor: 'PPrecision', _0: a};
};
var _gicentre$elm_vega$VegaLite$prPrecision = _gicentre$elm_vega$VegaLite$PPrecision;
var _gicentre$elm_vega$VegaLite$PRotate = F3(
	function (a, b, c) {
		return {ctor: 'PRotate', _0: a, _1: b, _2: c};
	});
var _gicentre$elm_vega$VegaLite$prRotate = _gicentre$elm_vega$VegaLite$PRotate;
var _gicentre$elm_vega$VegaLite$PCenter = F2(
	function (a, b) {
		return {ctor: 'PCenter', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$prCenter = _gicentre$elm_vega$VegaLite$PCenter;
var _gicentre$elm_vega$VegaLite$PClipExtent = function (a) {
	return {ctor: 'PClipExtent', _0: a};
};
var _gicentre$elm_vega$VegaLite$prClipExtent = _gicentre$elm_vega$VegaLite$PClipExtent;
var _gicentre$elm_vega$VegaLite$PClipAngle = function (a) {
	return {ctor: 'PClipAngle', _0: a};
};
var _gicentre$elm_vega$VegaLite$prClipAngle = _gicentre$elm_vega$VegaLite$PClipAngle;
var _gicentre$elm_vega$VegaLite$PType = function (a) {
	return {ctor: 'PType', _0: a};
};
var _gicentre$elm_vega$VegaLite$prType = _gicentre$elm_vega$VegaLite$PType;
var _gicentre$elm_vega$VegaLite$RSymbol = function (a) {
	return {ctor: 'RSymbol', _0: a};
};
var _gicentre$elm_vega$VegaLite$racoSymbol = _gicentre$elm_vega$VegaLite$RSymbol;
var _gicentre$elm_vega$VegaLite$RRamp = function (a) {
	return {ctor: 'RRamp', _0: a};
};
var _gicentre$elm_vega$VegaLite$racoRamp = _gicentre$elm_vega$VegaLite$RRamp;
var _gicentre$elm_vega$VegaLite$ROrdinal = function (a) {
	return {ctor: 'ROrdinal', _0: a};
};
var _gicentre$elm_vega$VegaLite$racoOrdinal = _gicentre$elm_vega$VegaLite$ROrdinal;
var _gicentre$elm_vega$VegaLite$RHeatmap = function (a) {
	return {ctor: 'RHeatmap', _0: a};
};
var _gicentre$elm_vega$VegaLite$racoHeatmap = _gicentre$elm_vega$VegaLite$RHeatmap;
var _gicentre$elm_vega$VegaLite$RDiverging = function (a) {
	return {ctor: 'RDiverging', _0: a};
};
var _gicentre$elm_vega$VegaLite$racoDiverging = _gicentre$elm_vega$VegaLite$RDiverging;
var _gicentre$elm_vega$VegaLite$RCategory = function (a) {
	return {ctor: 'RCategory', _0: a};
};
var _gicentre$elm_vega$VegaLite$racoCategory = _gicentre$elm_vega$VegaLite$RCategory;
var _gicentre$elm_vega$VegaLite$ColumnFields = function (a) {
	return {ctor: 'ColumnFields', _0: a};
};
var _gicentre$elm_vega$VegaLite$columnFields = _gicentre$elm_vega$VegaLite$ColumnFields;
var _gicentre$elm_vega$VegaLite$RowFields = function (a) {
	return {ctor: 'RowFields', _0: a};
};
var _gicentre$elm_vega$VegaLite$rowFields = _gicentre$elm_vega$VegaLite$RowFields;
var _gicentre$elm_vega$VegaLite$Independent = {ctor: 'Independent'};
var _gicentre$elm_vega$VegaLite$Shared = {ctor: 'Shared'};
var _gicentre$elm_vega$VegaLite$RScale = function (a) {
	return {ctor: 'RScale', _0: a};
};
var _gicentre$elm_vega$VegaLite$reScale = _gicentre$elm_vega$VegaLite$RScale;
var _gicentre$elm_vega$VegaLite$RLegend = function (a) {
	return {ctor: 'RLegend', _0: a};
};
var _gicentre$elm_vega$VegaLite$reLegend = _gicentre$elm_vega$VegaLite$RLegend;
var _gicentre$elm_vega$VegaLite$RAxis = function (a) {
	return {ctor: 'RAxis', _0: a};
};
var _gicentre$elm_vega$VegaLite$reAxis = _gicentre$elm_vega$VegaLite$RAxis;
var _gicentre$elm_vega$VegaLite$ScBinOrdinal = {ctor: 'ScBinOrdinal'};
var _gicentre$elm_vega$VegaLite$ScBinLinear = {ctor: 'ScBinLinear'};
var _gicentre$elm_vega$VegaLite$ScPoint = {ctor: 'ScPoint'};
var _gicentre$elm_vega$VegaLite$ScBand = {ctor: 'ScBand'};
var _gicentre$elm_vega$VegaLite$ScOrdinal = {ctor: 'ScOrdinal'};
var _gicentre$elm_vega$VegaLite$ScSequential = {ctor: 'ScSequential'};
var _gicentre$elm_vega$VegaLite$ScUtc = {ctor: 'ScUtc'};
var _gicentre$elm_vega$VegaLite$ScTime = {ctor: 'ScTime'};
var _gicentre$elm_vega$VegaLite$ScLog = {ctor: 'ScLog'};
var _gicentre$elm_vega$VegaLite$ScSqrt = {ctor: 'ScSqrt'};
var _gicentre$elm_vega$VegaLite$ScPow = {ctor: 'ScPow'};
var _gicentre$elm_vega$VegaLite$ScLinear = {ctor: 'ScLinear'};
var _gicentre$elm_vega$VegaLite$SCUseUnaggregatedDomain = function (a) {
	return {ctor: 'SCUseUnaggregatedDomain', _0: a};
};
var _gicentre$elm_vega$VegaLite$sacoUseUnaggregatedDomain = _gicentre$elm_vega$VegaLite$SCUseUnaggregatedDomain;
var _gicentre$elm_vega$VegaLite$SCTextXRangeStep = function (a) {
	return {ctor: 'SCTextXRangeStep', _0: a};
};
var _gicentre$elm_vega$VegaLite$sacoTextXRangeStep = _gicentre$elm_vega$VegaLite$SCTextXRangeStep;
var _gicentre$elm_vega$VegaLite$SCRound = function (a) {
	return {ctor: 'SCRound', _0: a};
};
var _gicentre$elm_vega$VegaLite$sacoRound = _gicentre$elm_vega$VegaLite$SCRound;
var _gicentre$elm_vega$VegaLite$SCRangeStep = function (a) {
	return {ctor: 'SCRangeStep', _0: a};
};
var _gicentre$elm_vega$VegaLite$sacoRangeStep = _gicentre$elm_vega$VegaLite$SCRangeStep;
var _gicentre$elm_vega$VegaLite$SCPointPadding = function (a) {
	return {ctor: 'SCPointPadding', _0: a};
};
var _gicentre$elm_vega$VegaLite$sacoPointPadding = _gicentre$elm_vega$VegaLite$SCPointPadding;
var _gicentre$elm_vega$VegaLite$SCMinStrokeWidth = function (a) {
	return {ctor: 'SCMinStrokeWidth', _0: a};
};
var _gicentre$elm_vega$VegaLite$sacoMinStrokeWidth = _gicentre$elm_vega$VegaLite$SCMinStrokeWidth;
var _gicentre$elm_vega$VegaLite$SCMaxStrokeWidth = function (a) {
	return {ctor: 'SCMaxStrokeWidth', _0: a};
};
var _gicentre$elm_vega$VegaLite$sacoMaxStrokeWidth = _gicentre$elm_vega$VegaLite$SCMaxStrokeWidth;
var _gicentre$elm_vega$VegaLite$SCMinSize = function (a) {
	return {ctor: 'SCMinSize', _0: a};
};
var _gicentre$elm_vega$VegaLite$sacoMinSize = _gicentre$elm_vega$VegaLite$SCMinSize;
var _gicentre$elm_vega$VegaLite$SCMaxSize = function (a) {
	return {ctor: 'SCMaxSize', _0: a};
};
var _gicentre$elm_vega$VegaLite$sacoMaxSize = _gicentre$elm_vega$VegaLite$SCMaxSize;
var _gicentre$elm_vega$VegaLite$SCMinOpacity = function (a) {
	return {ctor: 'SCMinOpacity', _0: a};
};
var _gicentre$elm_vega$VegaLite$sacoMinOpacity = _gicentre$elm_vega$VegaLite$SCMinOpacity;
var _gicentre$elm_vega$VegaLite$SCMaxOpacity = function (a) {
	return {ctor: 'SCMaxOpacity', _0: a};
};
var _gicentre$elm_vega$VegaLite$sacoMaxOpacity = _gicentre$elm_vega$VegaLite$SCMaxOpacity;
var _gicentre$elm_vega$VegaLite$SCMinFontSize = function (a) {
	return {ctor: 'SCMinFontSize', _0: a};
};
var _gicentre$elm_vega$VegaLite$sacoMinFontSize = _gicentre$elm_vega$VegaLite$SCMinFontSize;
var _gicentre$elm_vega$VegaLite$SCMaxFontSize = function (a) {
	return {ctor: 'SCMaxFontSize', _0: a};
};
var _gicentre$elm_vega$VegaLite$sacoMaxFontSize = _gicentre$elm_vega$VegaLite$SCMaxFontSize;
var _gicentre$elm_vega$VegaLite$SCMinBandSize = function (a) {
	return {ctor: 'SCMinBandSize', _0: a};
};
var _gicentre$elm_vega$VegaLite$sacoMinBandSize = _gicentre$elm_vega$VegaLite$SCMinBandSize;
var _gicentre$elm_vega$VegaLite$SCMaxBandSize = function (a) {
	return {ctor: 'SCMaxBandSize', _0: a};
};
var _gicentre$elm_vega$VegaLite$sacoMaxBandSize = _gicentre$elm_vega$VegaLite$SCMaxBandSize;
var _gicentre$elm_vega$VegaLite$SCClamp = function (a) {
	return {ctor: 'SCClamp', _0: a};
};
var _gicentre$elm_vega$VegaLite$sacoClamp = _gicentre$elm_vega$VegaLite$SCClamp;
var _gicentre$elm_vega$VegaLite$SCBandPaddingOuter = function (a) {
	return {ctor: 'SCBandPaddingOuter', _0: a};
};
var _gicentre$elm_vega$VegaLite$sacoBandPaddingOuter = _gicentre$elm_vega$VegaLite$SCBandPaddingOuter;
var _gicentre$elm_vega$VegaLite$SCBandPaddingInner = function (a) {
	return {ctor: 'SCBandPaddingInner', _0: a};
};
var _gicentre$elm_vega$VegaLite$sacoBandPaddingInner = _gicentre$elm_vega$VegaLite$SCBandPaddingInner;
var _gicentre$elm_vega$VegaLite$Unaggregated = {ctor: 'Unaggregated'};
var _gicentre$elm_vega$VegaLite$DSelection = function (a) {
	return {ctor: 'DSelection', _0: a};
};
var _gicentre$elm_vega$VegaLite$doSelection = _gicentre$elm_vega$VegaLite$DSelection;
var _gicentre$elm_vega$VegaLite$DDateTimes = function (a) {
	return {ctor: 'DDateTimes', _0: a};
};
var _gicentre$elm_vega$VegaLite$doDts = _gicentre$elm_vega$VegaLite$DDateTimes;
var _gicentre$elm_vega$VegaLite$DStrings = function (a) {
	return {ctor: 'DStrings', _0: a};
};
var _gicentre$elm_vega$VegaLite$doStrs = _gicentre$elm_vega$VegaLite$DStrings;
var _gicentre$elm_vega$VegaLite$DNumbers = function (a) {
	return {ctor: 'DNumbers', _0: a};
};
var _gicentre$elm_vega$VegaLite$doNums = _gicentre$elm_vega$VegaLite$DNumbers;
var _gicentre$elm_vega$VegaLite$NTickCount = function (a) {
	return {ctor: 'NTickCount', _0: a};
};
var _gicentre$elm_vega$VegaLite$scNiceTickCount = _gicentre$elm_vega$VegaLite$NTickCount;
var _gicentre$elm_vega$VegaLite$IsNice = function (a) {
	return {ctor: 'IsNice', _0: a};
};
var _gicentre$elm_vega$VegaLite$scIsNice = _gicentre$elm_vega$VegaLite$IsNice;
var _gicentre$elm_vega$VegaLite$NInterval = F2(
	function (a, b) {
		return {ctor: 'NInterval', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$scNiceInterval = _gicentre$elm_vega$VegaLite$NInterval;
var _gicentre$elm_vega$VegaLite$NYear = {ctor: 'NYear'};
var _gicentre$elm_vega$VegaLite$NMonth = {ctor: 'NMonth'};
var _gicentre$elm_vega$VegaLite$NWeek = {ctor: 'NWeek'};
var _gicentre$elm_vega$VegaLite$NDay = {ctor: 'NDay'};
var _gicentre$elm_vega$VegaLite$NHour = {ctor: 'NHour'};
var _gicentre$elm_vega$VegaLite$NMinute = {ctor: 'NMinute'};
var _gicentre$elm_vega$VegaLite$NSecond = {ctor: 'NSecond'};
var _gicentre$elm_vega$VegaLite$NMillisecond = {ctor: 'NMillisecond'};
var _gicentre$elm_vega$VegaLite$SReverse = function (a) {
	return {ctor: 'SReverse', _0: a};
};
var _gicentre$elm_vega$VegaLite$scReverse = _gicentre$elm_vega$VegaLite$SReverse;
var _gicentre$elm_vega$VegaLite$SZero = function (a) {
	return {ctor: 'SZero', _0: a};
};
var _gicentre$elm_vega$VegaLite$scZero = _gicentre$elm_vega$VegaLite$SZero;
var _gicentre$elm_vega$VegaLite$SNice = function (a) {
	return {ctor: 'SNice', _0: a};
};
var _gicentre$elm_vega$VegaLite$scNice = _gicentre$elm_vega$VegaLite$SNice;
var _gicentre$elm_vega$VegaLite$SInterpolate = function (a) {
	return {ctor: 'SInterpolate', _0: a};
};
var _gicentre$elm_vega$VegaLite$scInterpolate = _gicentre$elm_vega$VegaLite$SInterpolate;
var _gicentre$elm_vega$VegaLite$SClamp = function (a) {
	return {ctor: 'SClamp', _0: a};
};
var _gicentre$elm_vega$VegaLite$scClamp = _gicentre$elm_vega$VegaLite$SClamp;
var _gicentre$elm_vega$VegaLite$SRound = function (a) {
	return {ctor: 'SRound', _0: a};
};
var _gicentre$elm_vega$VegaLite$scRound = _gicentre$elm_vega$VegaLite$SRound;
var _gicentre$elm_vega$VegaLite$SRangeStep = function (a) {
	return {ctor: 'SRangeStep', _0: a};
};
var _gicentre$elm_vega$VegaLite$scRangeStep = _gicentre$elm_vega$VegaLite$SRangeStep;
var _gicentre$elm_vega$VegaLite$SPaddingOuter = function (a) {
	return {ctor: 'SPaddingOuter', _0: a};
};
var _gicentre$elm_vega$VegaLite$scPaddingOuter = _gicentre$elm_vega$VegaLite$SPaddingOuter;
var _gicentre$elm_vega$VegaLite$SPaddingInner = function (a) {
	return {ctor: 'SPaddingInner', _0: a};
};
var _gicentre$elm_vega$VegaLite$scPaddingInner = _gicentre$elm_vega$VegaLite$SPaddingInner;
var _gicentre$elm_vega$VegaLite$SPadding = function (a) {
	return {ctor: 'SPadding', _0: a};
};
var _gicentre$elm_vega$VegaLite$scPadding = _gicentre$elm_vega$VegaLite$SPadding;
var _gicentre$elm_vega$VegaLite$SScheme = F2(
	function (a, b) {
		return {ctor: 'SScheme', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$scScheme = function (name) {
	return _gicentre$elm_vega$VegaLite$SScheme(name);
};
var _gicentre$elm_vega$VegaLite$SRange = function (a) {
	return {ctor: 'SRange', _0: a};
};
var _gicentre$elm_vega$VegaLite$scRange = _gicentre$elm_vega$VegaLite$SRange;
var _gicentre$elm_vega$VegaLite$SDomain = function (a) {
	return {ctor: 'SDomain', _0: a};
};
var _gicentre$elm_vega$VegaLite$scDomain = _gicentre$elm_vega$VegaLite$SDomain;
var _gicentre$elm_vega$VegaLite$SType = function (a) {
	return {ctor: 'SType', _0: a};
};
var _gicentre$elm_vega$VegaLite$scType = _gicentre$elm_vega$VegaLite$SType;
var _gicentre$elm_vega$VegaLite$RName = function (a) {
	return {ctor: 'RName', _0: a};
};
var _gicentre$elm_vega$VegaLite$raName = _gicentre$elm_vega$VegaLite$RName;
var _gicentre$elm_vega$VegaLite$RStrings = function (a) {
	return {ctor: 'RStrings', _0: a};
};
var _gicentre$elm_vega$VegaLite$categoricalDomainMap = function (scaleDomainPairs) {
	var _p136 = _elm_lang$core$List$unzip(scaleDomainPairs);
	var domain = _p136._0;
	var range = _p136._1;
	return {
		ctor: '::',
		_0: _gicentre$elm_vega$VegaLite$SDomain(
			_gicentre$elm_vega$VegaLite$DStrings(domain)),
		_1: {
			ctor: '::',
			_0: _gicentre$elm_vega$VegaLite$SRange(
				_gicentre$elm_vega$VegaLite$RStrings(range)),
			_1: {ctor: '[]'}
		}
	};
};
var _gicentre$elm_vega$VegaLite$domainRangeMap = F2(
	function (lowerMap, upperMap) {
		var _p137 = _elm_lang$core$List$unzip(
			{
				ctor: '::',
				_0: lowerMap,
				_1: {
					ctor: '::',
					_0: upperMap,
					_1: {ctor: '[]'}
				}
			});
		var domain = _p137._0;
		var range = _p137._1;
		return {
			ctor: '::',
			_0: _gicentre$elm_vega$VegaLite$SDomain(
				_gicentre$elm_vega$VegaLite$DNumbers(domain)),
			_1: {
				ctor: '::',
				_0: _gicentre$elm_vega$VegaLite$SRange(
					_gicentre$elm_vega$VegaLite$RStrings(range)),
				_1: {ctor: '[]'}
			}
		};
	});
var _gicentre$elm_vega$VegaLite$raStrs = _gicentre$elm_vega$VegaLite$RStrings;
var _gicentre$elm_vega$VegaLite$RNumbers = function (a) {
	return {ctor: 'RNumbers', _0: a};
};
var _gicentre$elm_vega$VegaLite$raNums = _gicentre$elm_vega$VegaLite$RNumbers;
var _gicentre$elm_vega$VegaLite$Interval = {ctor: 'Interval'};
var _gicentre$elm_vega$VegaLite$Multi = {ctor: 'Multi'};
var _gicentre$elm_vega$VegaLite$Single = {ctor: 'Single'};
var _gicentre$elm_vega$VegaLite$SMStrokeDashOffset = function (a) {
	return {ctor: 'SMStrokeDashOffset', _0: a};
};
var _gicentre$elm_vega$VegaLite$smStrokeDashOffset = _gicentre$elm_vega$VegaLite$SMStrokeDashOffset;
var _gicentre$elm_vega$VegaLite$SMStrokeDash = function (a) {
	return {ctor: 'SMStrokeDash', _0: a};
};
var _gicentre$elm_vega$VegaLite$smStrokeDash = _gicentre$elm_vega$VegaLite$SMStrokeDash;
var _gicentre$elm_vega$VegaLite$SMStrokeWidth = function (a) {
	return {ctor: 'SMStrokeWidth', _0: a};
};
var _gicentre$elm_vega$VegaLite$smStrokeWidth = _gicentre$elm_vega$VegaLite$SMStrokeWidth;
var _gicentre$elm_vega$VegaLite$SMStrokeOpacity = function (a) {
	return {ctor: 'SMStrokeOpacity', _0: a};
};
var _gicentre$elm_vega$VegaLite$smStrokeOpacity = _gicentre$elm_vega$VegaLite$SMStrokeOpacity;
var _gicentre$elm_vega$VegaLite$SMStroke = function (a) {
	return {ctor: 'SMStroke', _0: a};
};
var _gicentre$elm_vega$VegaLite$smStroke = _gicentre$elm_vega$VegaLite$SMStroke;
var _gicentre$elm_vega$VegaLite$SMFillOpacity = function (a) {
	return {ctor: 'SMFillOpacity', _0: a};
};
var _gicentre$elm_vega$VegaLite$smFillOpacity = _gicentre$elm_vega$VegaLite$SMFillOpacity;
var _gicentre$elm_vega$VegaLite$SMFill = function (a) {
	return {ctor: 'SMFill', _0: a};
};
var _gicentre$elm_vega$VegaLite$smFill = _gicentre$elm_vega$VegaLite$SMFill;
var _gicentre$elm_vega$VegaLite$Toggle = function (a) {
	return {ctor: 'Toggle', _0: a};
};
var _gicentre$elm_vega$VegaLite$seToggle = _gicentre$elm_vega$VegaLite$Toggle;
var _gicentre$elm_vega$VegaLite$Nearest = function (a) {
	return {ctor: 'Nearest', _0: a};
};
var _gicentre$elm_vega$VegaLite$seNearest = _gicentre$elm_vega$VegaLite$Nearest;
var _gicentre$elm_vega$VegaLite$Bind = function (a) {
	return {ctor: 'Bind', _0: a};
};
var _gicentre$elm_vega$VegaLite$seBind = _gicentre$elm_vega$VegaLite$Bind;
var _gicentre$elm_vega$VegaLite$SelectionMark = function (a) {
	return {ctor: 'SelectionMark', _0: a};
};
var _gicentre$elm_vega$VegaLite$seSelectionMark = _gicentre$elm_vega$VegaLite$SelectionMark;
var _gicentre$elm_vega$VegaLite$ResolveSelections = function (a) {
	return {ctor: 'ResolveSelections', _0: a};
};
var _gicentre$elm_vega$VegaLite$seResolve = _gicentre$elm_vega$VegaLite$ResolveSelections;
var _gicentre$elm_vega$VegaLite$Encodings = function (a) {
	return {ctor: 'Encodings', _0: a};
};
var _gicentre$elm_vega$VegaLite$seEncodings = _gicentre$elm_vega$VegaLite$Encodings;
var _gicentre$elm_vega$VegaLite$Fields = function (a) {
	return {ctor: 'Fields', _0: a};
};
var _gicentre$elm_vega$VegaLite$seFields = _gicentre$elm_vega$VegaLite$Fields;
var _gicentre$elm_vega$VegaLite$Zoom = function (a) {
	return {ctor: 'Zoom', _0: a};
};
var _gicentre$elm_vega$VegaLite$seZoom = _gicentre$elm_vega$VegaLite$Zoom;
var _gicentre$elm_vega$VegaLite$Translate = function (a) {
	return {ctor: 'Translate', _0: a};
};
var _gicentre$elm_vega$VegaLite$seTranslate = _gicentre$elm_vega$VegaLite$Translate;
var _gicentre$elm_vega$VegaLite$On = function (a) {
	return {ctor: 'On', _0: a};
};
var _gicentre$elm_vega$VegaLite$seOn = _gicentre$elm_vega$VegaLite$On;
var _gicentre$elm_vega$VegaLite$BindScales = {ctor: 'BindScales'};
var _gicentre$elm_vega$VegaLite$Empty = {ctor: 'Empty'};
var _gicentre$elm_vega$VegaLite$Intersection = {ctor: 'Intersection'};
var _gicentre$elm_vega$VegaLite$Union = {ctor: 'Union'};
var _gicentre$elm_vega$VegaLite$Global = {ctor: 'Global'};
var _gicentre$elm_vega$VegaLite$SRight = {ctor: 'SRight'};
var _gicentre$elm_vega$VegaLite$SLeft = {ctor: 'SLeft'};
var _gicentre$elm_vega$VegaLite$SBottom = {ctor: 'SBottom'};
var _gicentre$elm_vega$VegaLite$STop = {ctor: 'STop'};
var _gicentre$elm_vega$VegaLite$ByFieldOp = F2(
	function (a, b) {
		return {ctor: 'ByFieldOp', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$soByField = _gicentre$elm_vega$VegaLite$ByFieldOp;
var _gicentre$elm_vega$VegaLite$ByRepeatOp = F2(
	function (a, b) {
		return {ctor: 'ByRepeatOp', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$soByRepeat = _gicentre$elm_vega$VegaLite$ByRepeatOp;
var _gicentre$elm_vega$VegaLite$CustomSort = function (a) {
	return {ctor: 'CustomSort', _0: a};
};
var _gicentre$elm_vega$VegaLite$soCustom = _gicentre$elm_vega$VegaLite$CustomSort;
var _gicentre$elm_vega$VegaLite$Op = function (a) {
	return {ctor: 'Op', _0: a};
};
var _gicentre$elm_vega$VegaLite$ByRepeat = function (a) {
	return {ctor: 'ByRepeat', _0: a};
};
var _gicentre$elm_vega$VegaLite$ByField = function (a) {
	return {ctor: 'ByField', _0: a};
};
var _gicentre$elm_vega$VegaLite$Descending = {ctor: 'Descending'};
var _gicentre$elm_vega$VegaLite$Ascending = {ctor: 'Ascending'};
var _gicentre$elm_vega$VegaLite$NoStack = {ctor: 'NoStack'};
var _gicentre$elm_vega$VegaLite$StCenter = {ctor: 'StCenter'};
var _gicentre$elm_vega$VegaLite$StNormalize = {ctor: 'StNormalize'};
var _gicentre$elm_vega$VegaLite$StZero = {ctor: 'StZero'};
var _gicentre$elm_vega$VegaLite$Path = function (a) {
	return {ctor: 'Path', _0: a};
};
var _gicentre$elm_vega$VegaLite$symbolPath = _gicentre$elm_vega$VegaLite$Path;
var _gicentre$elm_vega$VegaLite$TriangleDown = {ctor: 'TriangleDown'};
var _gicentre$elm_vega$VegaLite$TriangleUp = {ctor: 'TriangleUp'};
var _gicentre$elm_vega$VegaLite$Diamond = {ctor: 'Diamond'};
var _gicentre$elm_vega$VegaLite$Cross = {ctor: 'Cross'};
var _gicentre$elm_vega$VegaLite$SymSquare = {ctor: 'SymSquare'};
var _gicentre$elm_vega$VegaLite$SymCircle = {ctor: 'SymCircle'};
var _gicentre$elm_vega$VegaLite$TFormat = function (a) {
	return {ctor: 'TFormat', _0: a};
};
var _gicentre$elm_vega$VegaLite$tFormat = _gicentre$elm_vega$VegaLite$TFormat;
var _gicentre$elm_vega$VegaLite$TDataCondition = F3(
	function (a, b, c) {
		return {ctor: 'TDataCondition', _0: a, _1: b, _2: c};
	});
var _gicentre$elm_vega$VegaLite$tDataCondition = F3(
	function (op, tCh, fCh) {
		return A3(_gicentre$elm_vega$VegaLite$TDataCondition, op, tCh, fCh);
	});
var _gicentre$elm_vega$VegaLite$TSelectionCondition = F3(
	function (a, b, c) {
		return {ctor: 'TSelectionCondition', _0: a, _1: b, _2: c};
	});
var _gicentre$elm_vega$VegaLite$tSelectionCondition = F3(
	function (op, tCh, fCh) {
		return A3(_gicentre$elm_vega$VegaLite$TSelectionCondition, op, tCh, fCh);
	});
var _gicentre$elm_vega$VegaLite$TTimeUnit = function (a) {
	return {ctor: 'TTimeUnit', _0: a};
};
var _gicentre$elm_vega$VegaLite$tTimeUnit = _gicentre$elm_vega$VegaLite$TTimeUnit;
var _gicentre$elm_vega$VegaLite$TAggregate = function (a) {
	return {ctor: 'TAggregate', _0: a};
};
var _gicentre$elm_vega$VegaLite$tAggregate = _gicentre$elm_vega$VegaLite$TAggregate;
var _gicentre$elm_vega$VegaLite$TBin = function (a) {
	return {ctor: 'TBin', _0: a};
};
var _gicentre$elm_vega$VegaLite$tBin = _gicentre$elm_vega$VegaLite$TBin;
var _gicentre$elm_vega$VegaLite$TmType = function (a) {
	return {ctor: 'TmType', _0: a};
};
var _gicentre$elm_vega$VegaLite$tMType = _gicentre$elm_vega$VegaLite$TmType;
var _gicentre$elm_vega$VegaLite$TRepeat = function (a) {
	return {ctor: 'TRepeat', _0: a};
};
var _gicentre$elm_vega$VegaLite$tRepeat = _gicentre$elm_vega$VegaLite$TRepeat;
var _gicentre$elm_vega$VegaLite$TName = function (a) {
	return {ctor: 'TName', _0: a};
};
var _gicentre$elm_vega$VegaLite$tName = _gicentre$elm_vega$VegaLite$TName;
var _gicentre$elm_vega$VegaLite$Utc = function (a) {
	return {ctor: 'Utc', _0: a};
};
var _gicentre$elm_vega$VegaLite$utc = function (tu) {
	return _gicentre$elm_vega$VegaLite$Utc(tu);
};
var _gicentre$elm_vega$VegaLite$Milliseconds = {ctor: 'Milliseconds'};
var _gicentre$elm_vega$VegaLite$SecondsMilliseconds = {ctor: 'SecondsMilliseconds'};
var _gicentre$elm_vega$VegaLite$Seconds = {ctor: 'Seconds'};
var _gicentre$elm_vega$VegaLite$MinutesSeconds = {ctor: 'MinutesSeconds'};
var _gicentre$elm_vega$VegaLite$Minutes = {ctor: 'Minutes'};
var _gicentre$elm_vega$VegaLite$HoursMinutesSeconds = {ctor: 'HoursMinutesSeconds'};
var _gicentre$elm_vega$VegaLite$HoursMinutes = {ctor: 'HoursMinutes'};
var _gicentre$elm_vega$VegaLite$Hours = {ctor: 'Hours'};
var _gicentre$elm_vega$VegaLite$Day = {ctor: 'Day'};
var _gicentre$elm_vega$VegaLite$Date = {ctor: 'Date'};
var _gicentre$elm_vega$VegaLite$MonthDate = {ctor: 'MonthDate'};
var _gicentre$elm_vega$VegaLite$Month = {ctor: 'Month'};
var _gicentre$elm_vega$VegaLite$QuarterMonth = {ctor: 'QuarterMonth'};
var _gicentre$elm_vega$VegaLite$Quarter = {ctor: 'Quarter'};
var _gicentre$elm_vega$VegaLite$YearMonthDateHoursMinutesSeconds = {ctor: 'YearMonthDateHoursMinutesSeconds'};
var _gicentre$elm_vega$VegaLite$YearMonthDateHoursMinutes = {ctor: 'YearMonthDateHoursMinutes'};
var _gicentre$elm_vega$VegaLite$YearMonthDateHours = {ctor: 'YearMonthDateHours'};
var _gicentre$elm_vega$VegaLite$YearMonthDate = {ctor: 'YearMonthDate'};
var _gicentre$elm_vega$VegaLite$YearMonth = {ctor: 'YearMonth'};
var _gicentre$elm_vega$VegaLite$YearQuarterMonth = {ctor: 'YearQuarterMonth'};
var _gicentre$elm_vega$VegaLite$YearQuarter = {ctor: 'YearQuarter'};
var _gicentre$elm_vega$VegaLite$Year = {ctor: 'Year'};
var _gicentre$elm_vega$VegaLite$TOrient = function (a) {
	return {ctor: 'TOrient', _0: a};
};
var _gicentre$elm_vega$VegaLite$ticoOrient = _gicentre$elm_vega$VegaLite$TOrient;
var _gicentre$elm_vega$VegaLite$TOffset = function (a) {
	return {ctor: 'TOffset', _0: a};
};
var _gicentre$elm_vega$VegaLite$ticoOffset = _gicentre$elm_vega$VegaLite$TOffset;
var _gicentre$elm_vega$VegaLite$TLimit = function (a) {
	return {ctor: 'TLimit', _0: a};
};
var _gicentre$elm_vega$VegaLite$ticoLimit = _gicentre$elm_vega$VegaLite$TLimit;
var _gicentre$elm_vega$VegaLite$TFontWeight = function (a) {
	return {ctor: 'TFontWeight', _0: a};
};
var _gicentre$elm_vega$VegaLite$ticoFontWeight = _gicentre$elm_vega$VegaLite$TFontWeight;
var _gicentre$elm_vega$VegaLite$TFontSize = function (a) {
	return {ctor: 'TFontSize', _0: a};
};
var _gicentre$elm_vega$VegaLite$ticoFontSize = _gicentre$elm_vega$VegaLite$TFontSize;
var _gicentre$elm_vega$VegaLite$TFont = function (a) {
	return {ctor: 'TFont', _0: a};
};
var _gicentre$elm_vega$VegaLite$ticoFont = _gicentre$elm_vega$VegaLite$TFont;
var _gicentre$elm_vega$VegaLite$TColor = function (a) {
	return {ctor: 'TColor', _0: a};
};
var _gicentre$elm_vega$VegaLite$ticoColor = _gicentre$elm_vega$VegaLite$TColor;
var _gicentre$elm_vega$VegaLite$TBaseline = function (a) {
	return {ctor: 'TBaseline', _0: a};
};
var _gicentre$elm_vega$VegaLite$ticoBaseline = _gicentre$elm_vega$VegaLite$TBaseline;
var _gicentre$elm_vega$VegaLite$TAngle = function (a) {
	return {ctor: 'TAngle', _0: a};
};
var _gicentre$elm_vega$VegaLite$ticoAngle = _gicentre$elm_vega$VegaLite$TAngle;
var _gicentre$elm_vega$VegaLite$TAnchor = function (a) {
	return {ctor: 'TAnchor', _0: a};
};
var _gicentre$elm_vega$VegaLite$ticoAnchor = _gicentre$elm_vega$VegaLite$TAnchor;
var _gicentre$elm_vega$VegaLite$AlignBottom = {ctor: 'AlignBottom'};
var _gicentre$elm_vega$VegaLite$AlignMiddle = {ctor: 'AlignMiddle'};
var _gicentre$elm_vega$VegaLite$AlignTop = {ctor: 'AlignTop'};
var _gicentre$elm_vega$VegaLite$StrokeDashOffset = function (a) {
	return {ctor: 'StrokeDashOffset', _0: a};
};
var _gicentre$elm_vega$VegaLite$vicoStrokeDashOffset = _gicentre$elm_vega$VegaLite$StrokeDashOffset;
var _gicentre$elm_vega$VegaLite$StrokeDash = function (a) {
	return {ctor: 'StrokeDash', _0: a};
};
var _gicentre$elm_vega$VegaLite$vicoStrokeDash = _gicentre$elm_vega$VegaLite$StrokeDash;
var _gicentre$elm_vega$VegaLite$StrokeWidth = function (a) {
	return {ctor: 'StrokeWidth', _0: a};
};
var _gicentre$elm_vega$VegaLite$vicoStrokeWidth = _gicentre$elm_vega$VegaLite$StrokeWidth;
var _gicentre$elm_vega$VegaLite$StrokeOpacity = function (a) {
	return {ctor: 'StrokeOpacity', _0: a};
};
var _gicentre$elm_vega$VegaLite$vicoStrokeOpacity = _gicentre$elm_vega$VegaLite$StrokeOpacity;
var _gicentre$elm_vega$VegaLite$Stroke = function (a) {
	return {ctor: 'Stroke', _0: a};
};
var _gicentre$elm_vega$VegaLite$vicoStroke = _gicentre$elm_vega$VegaLite$Stroke;
var _gicentre$elm_vega$VegaLite$FillOpacity = function (a) {
	return {ctor: 'FillOpacity', _0: a};
};
var _gicentre$elm_vega$VegaLite$vicoFillOpacity = _gicentre$elm_vega$VegaLite$FillOpacity;
var _gicentre$elm_vega$VegaLite$Fill = function (a) {
	return {ctor: 'Fill', _0: a};
};
var _gicentre$elm_vega$VegaLite$vicoFill = _gicentre$elm_vega$VegaLite$Fill;
var _gicentre$elm_vega$VegaLite$Clip = function (a) {
	return {ctor: 'Clip', _0: a};
};
var _gicentre$elm_vega$VegaLite$vicoClip = _gicentre$elm_vega$VegaLite$Clip;
var _gicentre$elm_vega$VegaLite$ViewHeight = function (a) {
	return {ctor: 'ViewHeight', _0: a};
};
var _gicentre$elm_vega$VegaLite$vicoHeight = _gicentre$elm_vega$VegaLite$ViewHeight;
var _gicentre$elm_vega$VegaLite$ViewWidth = function (a) {
	return {ctor: 'ViewWidth', _0: a};
};
var _gicentre$elm_vega$VegaLite$vicoWidth = _gicentre$elm_vega$VegaLite$ViewWidth;
var _gicentre$elm_vega$VegaLite$VLSelection = {ctor: 'VLSelection'};
var _gicentre$elm_vega$VegaLite$selection = function (sels) {
	return {
		ctor: '_Tuple2',
		_0: _gicentre$elm_vega$VegaLite$VLSelection,
		_1: _elm_lang$core$Json_Encode$object(sels)
	};
};
var _gicentre$elm_vega$VegaLite$VLConfig = {ctor: 'VLConfig'};
var _gicentre$elm_vega$VegaLite$configure = function (configs) {
	return {
		ctor: '_Tuple2',
		_0: _gicentre$elm_vega$VegaLite$VLConfig,
		_1: _elm_lang$core$Json_Encode$object(configs)
	};
};
var _gicentre$elm_vega$VegaLite$VLResolve = {ctor: 'VLResolve'};
var _gicentre$elm_vega$VegaLite$resolve = function (res) {
	return {
		ctor: '_Tuple2',
		_0: _gicentre$elm_vega$VegaLite$VLResolve,
		_1: _elm_lang$core$Json_Encode$object(res)
	};
};
var _gicentre$elm_vega$VegaLite$VLSpec = {ctor: 'VLSpec'};
var _gicentre$elm_vega$VegaLite$specification = function (spec) {
	return {ctor: '_Tuple2', _0: _gicentre$elm_vega$VegaLite$VLSpec, _1: spec};
};
var _gicentre$elm_vega$VegaLite$VLFacet = {ctor: 'VLFacet'};
var _gicentre$elm_vega$VegaLite$facet = function (fMaps) {
	return {
		ctor: '_Tuple2',
		_0: _gicentre$elm_vega$VegaLite$VLFacet,
		_1: _elm_lang$core$Json_Encode$object(
			A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$facetMappingProperty, fMaps))
	};
};
var _gicentre$elm_vega$VegaLite$VLRepeat = {ctor: 'VLRepeat'};
var _gicentre$elm_vega$VegaLite$repeat = function (fields) {
	return {
		ctor: '_Tuple2',
		_0: _gicentre$elm_vega$VegaLite$VLRepeat,
		_1: _elm_lang$core$Json_Encode$object(
			A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$repeatFieldsProperty, fields))
	};
};
var _gicentre$elm_vega$VegaLite$VLVConcat = {ctor: 'VLVConcat'};
var _gicentre$elm_vega$VegaLite$vConcat = function (specs) {
	return {
		ctor: '_Tuple2',
		_0: _gicentre$elm_vega$VegaLite$VLVConcat,
		_1: _elm_lang$core$Json_Encode$list(specs)
	};
};
var _gicentre$elm_vega$VegaLite$VLHConcat = {ctor: 'VLHConcat'};
var _gicentre$elm_vega$VegaLite$hConcat = function (specs) {
	return {
		ctor: '_Tuple2',
		_0: _gicentre$elm_vega$VegaLite$VLHConcat,
		_1: _elm_lang$core$Json_Encode$list(specs)
	};
};
var _gicentre$elm_vega$VegaLite$VLLayer = {ctor: 'VLLayer'};
var _gicentre$elm_vega$VegaLite$layer = function (specs) {
	return {
		ctor: '_Tuple2',
		_0: _gicentre$elm_vega$VegaLite$VLLayer,
		_1: _elm_lang$core$Json_Encode$list(specs)
	};
};
var _gicentre$elm_vega$VegaLite$VLEncoding = {ctor: 'VLEncoding'};
var _gicentre$elm_vega$VegaLite$encoding = function (channels) {
	return {
		ctor: '_Tuple2',
		_0: _gicentre$elm_vega$VegaLite$VLEncoding,
		_1: _elm_lang$core$Json_Encode$object(channels)
	};
};
var _gicentre$elm_vega$VegaLite$VLProjection = {ctor: 'VLProjection'};
var _gicentre$elm_vega$VegaLite$projection = function (pProps) {
	return {
		ctor: '_Tuple2',
		_0: _gicentre$elm_vega$VegaLite$VLProjection,
		_1: _elm_lang$core$Json_Encode$object(
			A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$projectionProperty, pProps))
	};
};
var _gicentre$elm_vega$VegaLite$VLTransform = {ctor: 'VLTransform'};
var _gicentre$elm_vega$VegaLite$transform = function (transforms) {
	var assemble = function (_p138) {
		var _p139 = _p138;
		var _p153 = _p139._1;
		var _p152 = _p139._0;
		var _p140 = _p152;
		switch (_p140) {
			case 'aggregate':
				var _p141 = A2(
					_elm_lang$core$Json_Decode$decodeString,
					_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$value),
					A2(_elm_lang$core$Json_Encode$encode, 0, _p153));
				if ((((_p141.ctor === 'Ok') && (_p141._0.ctor === '::')) && (_p141._0._1.ctor === '::')) && (_p141._0._1._1.ctor === '[]')) {
					return _elm_lang$core$Json_Encode$object(
						{
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'aggregate', _1: _p141._0._0},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'groupby', _1: _p141._0._1._0},
								_1: {ctor: '[]'}
							}
						});
				} else {
					return _elm_lang$core$Json_Encode$null;
				}
			case 'bin':
				var _p142 = A2(
					_elm_lang$core$Json_Decode$decodeString,
					_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$value),
					A2(_elm_lang$core$Json_Encode$encode, 0, _p153));
				if (((((_p142.ctor === 'Ok') && (_p142._0.ctor === '::')) && (_p142._0._1.ctor === '::')) && (_p142._0._1._1.ctor === '::')) && (_p142._0._1._1._1.ctor === '[]')) {
					return _elm_lang$core$Json_Encode$object(
						{
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'bin', _1: _p142._0._0},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'field', _1: _p142._0._1._0},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'as', _1: _p142._0._1._1._0},
									_1: {ctor: '[]'}
								}
							}
						});
				} else {
					return _elm_lang$core$Json_Encode$null;
				}
			case 'calculate':
				var _p143 = A2(
					_elm_lang$core$Json_Decode$decodeString,
					_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$value),
					A2(_elm_lang$core$Json_Encode$encode, 0, _p153));
				if ((((_p143.ctor === 'Ok') && (_p143._0.ctor === '::')) && (_p143._0._1.ctor === '::')) && (_p143._0._1._1.ctor === '[]')) {
					return _elm_lang$core$Json_Encode$object(
						{
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'calculate', _1: _p143._0._0},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'as', _1: _p143._0._1._0},
								_1: {ctor: '[]'}
							}
						});
				} else {
					return _elm_lang$core$Json_Encode$null;
				}
			case 'lookup':
				var _p144 = A2(
					_elm_lang$core$Json_Decode$decodeString,
					_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$value),
					A2(_elm_lang$core$Json_Encode$encode, 0, _p153));
				if ((((((_p144.ctor === 'Ok') && (_p144._0.ctor === '::')) && (_p144._0._1.ctor === '::')) && (_p144._0._1._1.ctor === '::')) && (_p144._0._1._1._1.ctor === '::')) && (_p144._0._1._1._1._1.ctor === '[]')) {
					return _elm_lang$core$Json_Encode$object(
						{
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'lookup', _1: _p144._0._0},
							_1: {
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: 'from',
									_1: _elm_lang$core$Json_Encode$object(
										{
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'data', _1: _p144._0._1._0},
											_1: {
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: 'key', _1: _p144._0._1._1._0},
												_1: {
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: 'fields', _1: _p144._0._1._1._1._0},
													_1: {ctor: '[]'}
												}
											}
										})
								},
								_1: {ctor: '[]'}
							}
						});
				} else {
					return _elm_lang$core$Json_Encode$null;
				}
			case 'lookupAs':
				var _p145 = A2(
					_elm_lang$core$Json_Decode$decodeString,
					_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$value),
					A2(_elm_lang$core$Json_Encode$encode, 0, _p153));
				if ((((((_p145.ctor === 'Ok') && (_p145._0.ctor === '::')) && (_p145._0._1.ctor === '::')) && (_p145._0._1._1.ctor === '::')) && (_p145._0._1._1._1.ctor === '::')) && (_p145._0._1._1._1._1.ctor === '[]')) {
					return _elm_lang$core$Json_Encode$object(
						{
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'lookup', _1: _p145._0._0},
							_1: {
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: 'from',
									_1: _elm_lang$core$Json_Encode$object(
										{
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'data', _1: _p145._0._1._0},
											_1: {
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: 'key', _1: _p145._0._1._1._0},
												_1: {ctor: '[]'}
											}
										})
								},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'as', _1: _p145._0._1._1._1._0},
									_1: {ctor: '[]'}
								}
							}
						});
				} else {
					return _elm_lang$core$Json_Encode$null;
				}
			case 'timeUnit':
				var _p146 = A2(
					_elm_lang$core$Json_Decode$decodeString,
					_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$value),
					A2(_elm_lang$core$Json_Encode$encode, 0, _p153));
				if (((((_p146.ctor === 'Ok') && (_p146._0.ctor === '::')) && (_p146._0._1.ctor === '::')) && (_p146._0._1._1.ctor === '::')) && (_p146._0._1._1._1.ctor === '[]')) {
					return _elm_lang$core$Json_Encode$object(
						{
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'timeUnit', _1: _p146._0._0},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'field', _1: _p146._0._1._0},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'as', _1: _p146._0._1._1._0},
									_1: {ctor: '[]'}
								}
							}
						});
				} else {
					return _elm_lang$core$Json_Encode$null;
				}
			case 'windowAs':
				var _p147 = A2(
					_elm_lang$core$Json_Decode$decodeString,
					_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$value),
					A2(_elm_lang$core$Json_Encode$encode, 0, _p153));
				if (((((((_p147.ctor === 'Ok') && (_p147._0.ctor === '::')) && (_p147._0._1.ctor === '::')) && (_p147._0._1._1.ctor === '::')) && (_p147._0._1._1._1.ctor === '::')) && (_p147._0._1._1._1._1.ctor === '::')) && (_p147._0._1._1._1._1._1.ctor === '[]')) {
					var _p151 = _p147._0._1._1._1._1._0;
					var _p150 = _p147._0._1._1._0;
					var _p149 = _p147._0._1._1._1._0;
					var _p148 = _p147._0._1._0;
					return _elm_lang$core$Json_Encode$object(
						A2(
							_elm_lang$core$Basics_ops['++'],
							{
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: 'window',
									_1: _elm_lang$core$Json_Encode$list(
										{
											ctor: '::',
											_0: _p147._0._0,
											_1: {ctor: '[]'}
										})
								},
								_1: {ctor: '[]'}
							},
							A2(
								_elm_lang$core$Basics_ops['++'],
								_elm_lang$core$Native_Utils.eq(_p148, _elm_lang$core$Json_Encode$null) ? {ctor: '[]'} : {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'frame', _1: _p148},
									_1: {ctor: '[]'}
								},
								A2(
									_elm_lang$core$Basics_ops['++'],
									_elm_lang$core$Native_Utils.eq(_p150, _elm_lang$core$Json_Encode$null) ? {ctor: '[]'} : {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'ignorePeers', _1: _p150},
										_1: {ctor: '[]'}
									},
									A2(
										_elm_lang$core$Basics_ops['++'],
										_elm_lang$core$Native_Utils.eq(_p149, _elm_lang$core$Json_Encode$null) ? {ctor: '[]'} : {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'groupby', _1: _p149},
											_1: {ctor: '[]'}
										},
										_elm_lang$core$Native_Utils.eq(_p151, _elm_lang$core$Json_Encode$null) ? {ctor: '[]'} : {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'sort', _1: _p151},
											_1: {ctor: '[]'}
										})))));
				} else {
					return _elm_lang$core$Json_Encode$null;
				}
			default:
				return _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: _p152, _1: _p153},
						_1: {ctor: '[]'}
					});
		}
	};
	return _elm_lang$core$List$isEmpty(transforms) ? {ctor: '_Tuple2', _0: _gicentre$elm_vega$VegaLite$VLTransform, _1: _elm_lang$core$Json_Encode$null} : {
		ctor: '_Tuple2',
		_0: _gicentre$elm_vega$VegaLite$VLTransform,
		_1: _elm_lang$core$Json_Encode$list(
			A2(_elm_lang$core$List$map, assemble, transforms))
	};
};
var _gicentre$elm_vega$VegaLite$VLMark = {ctor: 'VLMark'};
var _gicentre$elm_vega$VegaLite$mark = F2(
	function (mark, mProps) {
		var _p154 = mProps;
		if (_p154.ctor === '[]') {
			return {
				ctor: '_Tuple2',
				_0: _gicentre$elm_vega$VegaLite$VLMark,
				_1: _elm_lang$core$Json_Encode$string(
					_gicentre$elm_vega$VegaLite$markLabel(mark))
			};
		} else {
			return {
				ctor: '_Tuple2',
				_0: _gicentre$elm_vega$VegaLite$VLMark,
				_1: _elm_lang$core$Json_Encode$object(
					{
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'type',
							_1: _elm_lang$core$Json_Encode$string(
								_gicentre$elm_vega$VegaLite$markLabel(mark))
						},
						_1: A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$markProperty, mProps)
					})
			};
		}
	});
var _gicentre$elm_vega$VegaLite$area = _gicentre$elm_vega$VegaLite$mark(_gicentre$elm_vega$VegaLite$Area);
var _gicentre$elm_vega$VegaLite$bar = _gicentre$elm_vega$VegaLite$mark(_gicentre$elm_vega$VegaLite$Bar);
var _gicentre$elm_vega$VegaLite$circle = _gicentre$elm_vega$VegaLite$mark(_gicentre$elm_vega$VegaLite$Circle);
var _gicentre$elm_vega$VegaLite$geoshape = _gicentre$elm_vega$VegaLite$mark(_gicentre$elm_vega$VegaLite$Geoshape);
var _gicentre$elm_vega$VegaLite$line = _gicentre$elm_vega$VegaLite$mark(_gicentre$elm_vega$VegaLite$Line);
var _gicentre$elm_vega$VegaLite$point = _gicentre$elm_vega$VegaLite$mark(_gicentre$elm_vega$VegaLite$Point);
var _gicentre$elm_vega$VegaLite$rect = _gicentre$elm_vega$VegaLite$mark(_gicentre$elm_vega$VegaLite$Rect);
var _gicentre$elm_vega$VegaLite$rule = _gicentre$elm_vega$VegaLite$mark(_gicentre$elm_vega$VegaLite$Rule);
var _gicentre$elm_vega$VegaLite$square = _gicentre$elm_vega$VegaLite$mark(_gicentre$elm_vega$VegaLite$Square);
var _gicentre$elm_vega$VegaLite$textMark = _gicentre$elm_vega$VegaLite$mark(_gicentre$elm_vega$VegaLite$Text);
var _gicentre$elm_vega$VegaLite$tick = _gicentre$elm_vega$VegaLite$mark(_gicentre$elm_vega$VegaLite$Tick);
var _gicentre$elm_vega$VegaLite$trail = _gicentre$elm_vega$VegaLite$mark(_gicentre$elm_vega$VegaLite$Trail);
var _gicentre$elm_vega$VegaLite$VLDatasets = {ctor: 'VLDatasets'};
var _gicentre$elm_vega$VegaLite$datasets = function (namedData) {
	var extract = function (data) {
		var _p155 = A2(
			_elm_lang$core$Json_Decode$decodeString,
			_elm_lang$core$Json_Decode$keyValuePairs(_elm_lang$core$Json_Decode$value),
			A2(_elm_lang$core$Json_Encode$encode, 0, data));
		if ((((_p155.ctor === 'Ok') && (_p155._0.ctor === '::')) && (_p155._0._0.ctor === '_Tuple2')) && (_p155._0._1.ctor === '[]')) {
			return _p155._0._0._1;
		} else {
			return data;
		}
	};
	var specs = A2(
		_elm_lang$core$List$map,
		function (_p156) {
			var _p157 = _p156;
			return {
				ctor: '_Tuple2',
				_0: _p157._0,
				_1: function (_p158) {
					var _p159 = _p158;
					return extract(_p159._1);
				}(_p157._1)
			};
		},
		namedData);
	return {
		ctor: '_Tuple2',
		_0: _gicentre$elm_vega$VegaLite$VLDatasets,
		_1: _elm_lang$core$Json_Encode$object(specs)
	};
};
var _gicentre$elm_vega$VegaLite$VLData = {ctor: 'VLData'};
var _gicentre$elm_vega$VegaLite$dataFromColumns = F2(
	function (fmts, cols) {
		var dataArray = _elm_lang$core$Json_Encode$list(
			A2(
				_elm_lang$core$List$map,
				_elm_lang$core$Json_Encode$object,
				_gicentre$elm_vega$VegaLite$transpose(cols)));
		return _elm_lang$core$Native_Utils.eq(
			fmts,
			{ctor: '[]'}) ? {
			ctor: '_Tuple2',
			_0: _gicentre$elm_vega$VegaLite$VLData,
			_1: _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'values', _1: dataArray},
					_1: {ctor: '[]'}
				})
		} : {
			ctor: '_Tuple2',
			_0: _gicentre$elm_vega$VegaLite$VLData,
			_1: _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'values', _1: dataArray},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'format',
							_1: _elm_lang$core$Json_Encode$object(
								A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$formatProperty, fmts))
						},
						_1: {ctor: '[]'}
					}
				})
		};
	});
var _gicentre$elm_vega$VegaLite$dataFromJson = F2(
	function (json, fmts) {
		return _elm_lang$core$Native_Utils.eq(
			fmts,
			{ctor: '[]'}) ? {
			ctor: '_Tuple2',
			_0: _gicentre$elm_vega$VegaLite$VLData,
			_1: _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'values', _1: json},
					_1: {ctor: '[]'}
				})
		} : {
			ctor: '_Tuple2',
			_0: _gicentre$elm_vega$VegaLite$VLData,
			_1: _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'values', _1: json},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'format',
							_1: _elm_lang$core$Json_Encode$object(
								A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$formatProperty, fmts))
						},
						_1: {ctor: '[]'}
					}
				})
		};
	});
var _gicentre$elm_vega$VegaLite$dataFromRows = F2(
	function (fmts, rows) {
		return _elm_lang$core$Native_Utils.eq(
			fmts,
			{ctor: '[]'}) ? {
			ctor: '_Tuple2',
			_0: _gicentre$elm_vega$VegaLite$VLData,
			_1: _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'values',
						_1: _elm_lang$core$Json_Encode$list(rows)
					},
					_1: {ctor: '[]'}
				})
		} : {
			ctor: '_Tuple2',
			_0: _gicentre$elm_vega$VegaLite$VLData,
			_1: _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'values',
						_1: _elm_lang$core$Json_Encode$list(rows)
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'format',
							_1: _elm_lang$core$Json_Encode$object(
								A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$formatProperty, fmts))
						},
						_1: {ctor: '[]'}
					}
				})
		};
	});
var _gicentre$elm_vega$VegaLite$dataFromSource = F2(
	function (sourceName, fmts) {
		return _elm_lang$core$Native_Utils.eq(
			fmts,
			{ctor: '[]'}) ? {
			ctor: '_Tuple2',
			_0: _gicentre$elm_vega$VegaLite$VLData,
			_1: _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'name',
						_1: _elm_lang$core$Json_Encode$string(sourceName)
					},
					_1: {ctor: '[]'}
				})
		} : {
			ctor: '_Tuple2',
			_0: _gicentre$elm_vega$VegaLite$VLData,
			_1: _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'name',
						_1: _elm_lang$core$Json_Encode$string(sourceName)
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'format',
							_1: _elm_lang$core$Json_Encode$object(
								A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$formatProperty, fmts))
						},
						_1: {ctor: '[]'}
					}
				})
		};
	});
var _gicentre$elm_vega$VegaLite$dataFromUrl = F2(
	function (url, fmts) {
		return _elm_lang$core$Native_Utils.eq(
			fmts,
			{ctor: '[]'}) ? {
			ctor: '_Tuple2',
			_0: _gicentre$elm_vega$VegaLite$VLData,
			_1: _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'url',
						_1: _elm_lang$core$Json_Encode$string(url)
					},
					_1: {ctor: '[]'}
				})
		} : {
			ctor: '_Tuple2',
			_0: _gicentre$elm_vega$VegaLite$VLData,
			_1: _elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'url',
						_1: _elm_lang$core$Json_Encode$string(url)
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'format',
							_1: _elm_lang$core$Json_Encode$object(
								A2(_elm_lang$core$List$concatMap, _gicentre$elm_vega$VegaLite$formatProperty, fmts))
						},
						_1: {ctor: '[]'}
					}
				})
		};
	});
var _gicentre$elm_vega$VegaLite$VLBackground = {ctor: 'VLBackground'};
var _gicentre$elm_vega$VegaLite$background = function (colour) {
	return {
		ctor: '_Tuple2',
		_0: _gicentre$elm_vega$VegaLite$VLBackground,
		_1: _elm_lang$core$Json_Encode$string(colour)
	};
};
var _gicentre$elm_vega$VegaLite$VLPadding = {ctor: 'VLPadding'};
var _gicentre$elm_vega$VegaLite$padding = function (pad) {
	return {
		ctor: '_Tuple2',
		_0: _gicentre$elm_vega$VegaLite$VLPadding,
		_1: _gicentre$elm_vega$VegaLite$paddingSpec(pad)
	};
};
var _gicentre$elm_vega$VegaLite$VLAutosize = {ctor: 'VLAutosize'};
var _gicentre$elm_vega$VegaLite$autosize = function (aus) {
	return {
		ctor: '_Tuple2',
		_0: _gicentre$elm_vega$VegaLite$VLAutosize,
		_1: _elm_lang$core$Json_Encode$object(
			A2(_elm_lang$core$List$map, _gicentre$elm_vega$VegaLite$autosizeProperty, aus))
	};
};
var _gicentre$elm_vega$VegaLite$VLHeight = {ctor: 'VLHeight'};
var _gicentre$elm_vega$VegaLite$height = function (h) {
	return {
		ctor: '_Tuple2',
		_0: _gicentre$elm_vega$VegaLite$VLHeight,
		_1: _elm_lang$core$Json_Encode$float(h)
	};
};
var _gicentre$elm_vega$VegaLite$VLWidth = {ctor: 'VLWidth'};
var _gicentre$elm_vega$VegaLite$width = function (w) {
	return {
		ctor: '_Tuple2',
		_0: _gicentre$elm_vega$VegaLite$VLWidth,
		_1: _elm_lang$core$Json_Encode$float(w)
	};
};
var _gicentre$elm_vega$VegaLite$VLTitle = {ctor: 'VLTitle'};
var _gicentre$elm_vega$VegaLite$title = function (s) {
	return {
		ctor: '_Tuple2',
		_0: _gicentre$elm_vega$VegaLite$VLTitle,
		_1: _elm_lang$core$Json_Encode$string(s)
	};
};
var _gicentre$elm_vega$VegaLite$VLDescription = {ctor: 'VLDescription'};
var _gicentre$elm_vega$VegaLite$description = function (s) {
	return {
		ctor: '_Tuple2',
		_0: _gicentre$elm_vega$VegaLite$VLDescription,
		_1: _elm_lang$core$Json_Encode$string(s)
	};
};
var _gicentre$elm_vega$VegaLite$VLName = {ctor: 'VLName'};
var _gicentre$elm_vega$VegaLite$name = function (s) {
	return {
		ctor: '_Tuple2',
		_0: _gicentre$elm_vega$VegaLite$VLName,
		_1: _elm_lang$core$Json_Encode$string(s)
	};
};
var _gicentre$elm_vega$VegaLite$WField = function (a) {
	return {ctor: 'WField', _0: a};
};
var _gicentre$elm_vega$VegaLite$wiField = _gicentre$elm_vega$VegaLite$WField;
var _gicentre$elm_vega$VegaLite$WParam = function (a) {
	return {ctor: 'WParam', _0: a};
};
var _gicentre$elm_vega$VegaLite$wiParam = _gicentre$elm_vega$VegaLite$WParam;
var _gicentre$elm_vega$VegaLite$WOp = function (a) {
	return {ctor: 'WOp', _0: a};
};
var _gicentre$elm_vega$VegaLite$wiOp = _gicentre$elm_vega$VegaLite$WOp;
var _gicentre$elm_vega$VegaLite$WAggregateOp = function (a) {
	return {ctor: 'WAggregateOp', _0: a};
};
var _gicentre$elm_vega$VegaLite$wiAggregateOp = _gicentre$elm_vega$VegaLite$WAggregateOp;
var _gicentre$elm_vega$VegaLite$NthValue = {ctor: 'NthValue'};
var _gicentre$elm_vega$VegaLite$LastValue = {ctor: 'LastValue'};
var _gicentre$elm_vega$VegaLite$FirstValue = {ctor: 'FirstValue'};
var _gicentre$elm_vega$VegaLite$Lead = {ctor: 'Lead'};
var _gicentre$elm_vega$VegaLite$Lag = {ctor: 'Lag'};
var _gicentre$elm_vega$VegaLite$Ntile = {ctor: 'Ntile'};
var _gicentre$elm_vega$VegaLite$CumeDist = {ctor: 'CumeDist'};
var _gicentre$elm_vega$VegaLite$PercentRank = {ctor: 'PercentRank'};
var _gicentre$elm_vega$VegaLite$DenseRank = {ctor: 'DenseRank'};
var _gicentre$elm_vega$VegaLite$Rank = {ctor: 'Rank'};
var _gicentre$elm_vega$VegaLite$RowNumber = {ctor: 'RowNumber'};
var _gicentre$elm_vega$VegaLite$WSort = function (a) {
	return {ctor: 'WSort', _0: a};
};
var _gicentre$elm_vega$VegaLite$wiSort = _gicentre$elm_vega$VegaLite$WSort;
var _gicentre$elm_vega$VegaLite$WGroupBy = function (a) {
	return {ctor: 'WGroupBy', _0: a};
};
var _gicentre$elm_vega$VegaLite$wiGroupBy = _gicentre$elm_vega$VegaLite$WGroupBy;
var _gicentre$elm_vega$VegaLite$WIgnorePeers = function (a) {
	return {ctor: 'WIgnorePeers', _0: a};
};
var _gicentre$elm_vega$VegaLite$wiIgnorePeers = _gicentre$elm_vega$VegaLite$WIgnorePeers;
var _gicentre$elm_vega$VegaLite$WFrame = F2(
	function (a, b) {
		return {ctor: 'WFrame', _0: a, _1: b};
	});
var _gicentre$elm_vega$VegaLite$wiFrame = _gicentre$elm_vega$VegaLite$WFrame;
var _gicentre$elm_vega$VegaLite$WDescending = function (a) {
	return {ctor: 'WDescending', _0: a};
};
var _gicentre$elm_vega$VegaLite$wiDescending = _gicentre$elm_vega$VegaLite$WDescending;
var _gicentre$elm_vega$VegaLite$WAscending = function (a) {
	return {ctor: 'WAscending', _0: a};
};
var _gicentre$elm_vega$VegaLite$wiAscending = _gicentre$elm_vega$VegaLite$WAscending;

/**
 * much of this is copied from https://github.com/evancz/elm-markdown
 */
var _bsouthga$elm_katex$Native_KaTeX = function() {


// VIRTUAL-DOM WIDGET IMPLEMENTATION

var implementation = {
  render: _render,
  diff: diff
};


// VIRTUAL-DOM WIDGETS

function renderElement(options, expression) {
  options.throwOnError = false;

  var model = {
    options: options,
    expression: expression
  };

  var factList = {ctor: '[]'};

  return _elm_lang$virtual_dom$Native_VirtualDom
    .custom(factList, model, implementation);
}


function tryRenderKatex(expression, element, options) {
  try {
    katex.render(expression, element, options);
    return element;
  } catch (err) {
    return element;
  }
}


function _render(model) {
  var div = document.createElement('div');

  return tryRenderKatex(model.expression, div, model.options);
}


function diff(a, b) {
  if (a.model.expression === b.model.expression &&
    a.model.options === b.model.options) {
    return null;
  }

  return {
    applyPatch: applyPatch,
    data: b.model
  };
}


function applyPatch(domNode, data) {
  var options = data.options;
  var expression = data.expression;

  options.throwOnError = false;

  return tryRenderKatex(
    expression,
    domNode,
    options
  );
}


function renderToString(options, expression) {
  options.throwOnError = false;
  try {
    return katex.renderToString(expression, options);
  } catch (err) {
    return '';
  }
}


var katex = function() {
  // catch the KaTeX export object regardless of the outer environment.
  // (ex. a CommonJS module compatible environment.)
  // note that this depends on KaTeX's implementation of environment detection.
  var module = {};
  var exports = module.exports = {};

  /**
   * KaTeX (version 0.6.0) - KaTeX is a fast, easy-to-use JavaScript library for TeX math rendering on the web.
   *
   * Copyright (c) 2015 Khan Academy. (MIT Licensed)
   * https://github.com/Khan/KaTeX
   */
  (function(e){if(typeof exports==="object"&&typeof module!=="undefined"){module.exports=e()}else if(typeof define==="function"&&define.amd){define([],e)}else{var t;if(typeof window!=="undefined"){t=window}else if(typeof global!=="undefined"){t=global}else if(typeof self!=="undefined"){t=self}else{t=this}t.katex=e()}})(function(){var e,t,r;return function a(e,t,r){function i(s,l){if(!t[s]){if(!e[s]){var o=typeof require=="function"&&require;if(!l&&o)return o(s,!0);if(n)return n(s,!0);var u=new Error("Cannot find module '"+s+"'");throw u.code="MODULE_NOT_FOUND",u}var p=t[s]={exports:{}};e[s][0].call(p.exports,function(t){var r=e[s][1][t];return i(r?r:t)},p,p.exports,a,e,t,r)}return t[s].exports}var n=typeof require=="function"&&require;for(var s=0;s<r.length;s++)i(r[s]);return i}({1:[function(e,t,r){var a=e("./src/ParseError");var i=e("./src/Settings");var n=e("./src/buildTree");var s=e("./src/parseTree");var l=e("./src/utils");var o=function(e,t,r){l.clearNode(t);var a=new i(r);var o=s(e,a);var u=n(o,e,a).toNode();t.appendChild(u)};if(typeof document!=="undefined"){if(document.compatMode!=="CSS1Compat"){typeof console!=="undefined"&&console.warn("Warning: KaTeX doesn't work in quirks mode. Make sure your "+"website has a suitable doctype.");o=function(){throw new a("KaTeX doesn't work in quirks mode.")}}}var u=function(e,t){var r=new i(t);var a=s(e,r);return n(a,e,r).toMarkup()};var p=function(e,t){var r=new i(t);return s(e,r)};t.exports={render:o,renderToString:u,__parse:p,ParseError:a}},{"./src/ParseError":5,"./src/Settings":7,"./src/buildTree":12,"./src/parseTree":21,"./src/utils":23}],2:[function(e,t,r){"use strict";function a(e){if(!e.__matchAtRelocatable){var t=e.source+"|()";var r="g"+(e.ignoreCase?"i":"")+(e.multiline?"m":"")+(e.unicode?"u":"");e.__matchAtRelocatable=new RegExp(t,r)}return e.__matchAtRelocatable}function i(e,t,r){if(e.global||e.sticky){throw new Error("matchAt(...): Only non-global regexes are supported")}var i=a(e);i.lastIndex=r;var n=i.exec(t);if(n[n.length-1]==null){n.length=n.length-1;return n}else{return null}}t.exports=i},{}],3:[function(e,t,r){var a=e("match-at");var i=e("./ParseError");function n(e){this._input=e}function s(e,t,r){this.text=e;this.data=t;this.position=r}var l=new RegExp("([ \r\n	]+)|("+"---?"+"|[!-\\[\\]-\u2027\u202a-\ud7ff\uf900-\uffff]"+"|[\ud800-\udbff][\udc00-\udfff]"+"|\\\\(?:[a-zA-Z]+|[^\ud800-\udfff])"+")");var o=/\s*/;n.prototype._innerLex=function(e,t){var r=this._input;if(e===r.length){return new s("EOF",null,e)}var n=a(l,r,e);if(n===null){throw new i("Unexpected character: '"+r[e]+"'",this,e)}else if(n[2]){return new s(n[2],null,e+n[2].length)}else if(t){return this._innerLex(e+n[1].length,true)}else{return new s(" ",null,e+n[1].length)}};var u=/#[a-z0-9]+|[a-z]+/i;n.prototype._innerLexColor=function(e){var t=this._input;var r=a(o,t,e)[0];e+=r.length;var n;if(n=a(u,t,e)){return new s(n[0],null,e+n[0].length)}else{throw new i("Invalid color",this,e)}};var p=/(-?)\s*(\d+(?:\.\d*)?|\.\d+)\s*([a-z]{2})/;n.prototype._innerLexSize=function(e){var t=this._input;var r=a(o,t,e)[0];e+=r.length;var n;if(n=a(p,t,e)){var l=n[3];if(l!=="em"&&l!=="ex"){throw new i("Invalid unit: '"+l+"'",this,e)}return new s(n[0],{number:+(n[1]+n[2]),unit:l},e+n[0].length)}throw new i("Invalid size",this,e)};n.prototype._innerLexWhitespace=function(e){var t=this._input;var r=a(o,t,e)[0];e+=r.length;return new s(r[0],null,e)};n.prototype.lex=function(e,t){if(t==="math"){return this._innerLex(e,true)}else if(t==="text"){return this._innerLex(e,false)}else if(t==="color"){return this._innerLexColor(e)}else if(t==="size"){return this._innerLexSize(e)}else if(t==="whitespace"){return this._innerLexWhitespace(e)}};t.exports=n},{"./ParseError":5,"match-at":2}],4:[function(e,t,r){function a(e){this.style=e.style;this.color=e.color;this.size=e.size;this.phantom=e.phantom;this.font=e.font;if(e.parentStyle===undefined){this.parentStyle=e.style}else{this.parentStyle=e.parentStyle}if(e.parentSize===undefined){this.parentSize=e.size}else{this.parentSize=e.parentSize}}a.prototype.extend=function(e){var t={style:this.style,size:this.size,color:this.color,parentStyle:this.style,parentSize:this.size,phantom:this.phantom,font:this.font};for(var r in e){if(e.hasOwnProperty(r)){t[r]=e[r]}}return new a(t)};a.prototype.withStyle=function(e){return this.extend({style:e})};a.prototype.withSize=function(e){return this.extend({size:e})};a.prototype.withColor=function(e){return this.extend({color:e})};a.prototype.withPhantom=function(){return this.extend({phantom:true})};a.prototype.withFont=function(e){return this.extend({font:e})};a.prototype.reset=function(){return this.extend({})};var i={"katex-blue":"#6495ed","katex-orange":"#ffa500","katex-pink":"#ff00af","katex-red":"#df0030","katex-green":"#28ae7b","katex-gray":"gray","katex-purple":"#9d38bd","katex-blueA":"#c7e9f1","katex-blueB":"#9cdceb","katex-blueC":"#58c4dd","katex-blueD":"#29abca","katex-blueE":"#1c758a","katex-tealA":"#acead7","katex-tealB":"#76ddc0","katex-tealC":"#5cd0b3","katex-tealD":"#55c1a7","katex-tealE":"#49a88f","katex-greenA":"#c9e2ae","katex-greenB":"#a6cf8c","katex-greenC":"#83c167","katex-greenD":"#77b05d","katex-greenE":"#699c52","katex-goldA":"#f7c797","katex-goldB":"#f9b775","katex-goldC":"#f0ac5f","katex-goldD":"#e1a158","katex-goldE":"#c78d46","katex-redA":"#f7a1a3","katex-redB":"#ff8080","katex-redC":"#fc6255","katex-redD":"#e65a4c","katex-redE":"#cf5044","katex-maroonA":"#ecabc1","katex-maroonB":"#ec92ab","katex-maroonC":"#c55f73","katex-maroonD":"#a24d61","katex-maroonE":"#94424f","katex-purpleA":"#caa3e8","katex-purpleB":"#b189c6","katex-purpleC":"#9a72ac","katex-purpleD":"#715582","katex-purpleE":"#644172","katex-mintA":"#f5f9e8","katex-mintB":"#edf2df","katex-mintC":"#e0e5cc","katex-grayA":"#fdfdfd","katex-grayB":"#f7f7f7","katex-grayC":"#eeeeee","katex-grayD":"#dddddd","katex-grayE":"#cccccc","katex-grayF":"#aaaaaa","katex-grayG":"#999999","katex-grayH":"#555555","katex-grayI":"#333333","katex-kaBlue":"#314453","katex-kaGreen":"#639b24"};a.prototype.getColor=function(){if(this.phantom){return"transparent"}else{return i[this.color]||this.color}};t.exports=a},{}],5:[function(e,t,r){function a(e,t,r){var i="KaTeX parse error: "+e;if(t!==undefined&&r!==undefined){i+=" at position "+r+": ";var n=t._input;n=n.slice(0,r)+"\u0332"+n.slice(r);var s=Math.max(0,r-15);var l=r+15;i+=n.slice(s,l)}var o=new Error(i);o.name="ParseError";o.__proto__=a.prototype;o.position=r;return o}a.prototype.__proto__=Error.prototype;t.exports=a},{}],6:[function(e,t,r){var a=e("./functions");var i=e("./environments");var n=e("./Lexer");var s=e("./symbols");var l=e("./utils");var o=e("./parseData");var u=e("./ParseError");function p(e,t){this.lexer=new n(e);this.settings=t}var h=o.ParseNode;function c(e,t){this.result=e;this.isFunction=t}p.prototype.expect=function(e,t){if(this.nextToken.text!==e){throw new u("Expected '"+e+"', got '"+this.nextToken.text+"'",this.lexer,this.nextToken.position)}if(t!==false){this.consume()}};p.prototype.consume=function(){this.pos=this.nextToken.position;this.nextToken=this.lexer.lex(this.pos,this.mode)};p.prototype.parse=function(){this.mode="math";this.pos=0;this.nextToken=this.lexer.lex(this.pos,this.mode);var e=this.parseInput();return e};p.prototype.parseInput=function(){var e=this.parseExpression(false);this.expect("EOF",false);return e};var v=["}","\\end","\\right","&","\\\\","\\cr"];p.prototype.parseExpression=function(e,t){var r=[];while(true){var a=this.nextToken;var i=this.pos;if(v.indexOf(a.text)!==-1){break}if(t&&a.text===t){break}var n=this.parseAtom();if(!n){if(!this.settings.throwOnError&&a.text[0]==="\\"){var s=this.handleUnsupportedCmd();r.push(s);i=a.position;continue}break}if(e&&n.type==="infix"){this.pos=i;this.nextToken=a;break}r.push(n)}return this.handleInfixNodes(r)};p.prototype.handleInfixNodes=function(e){var t=-1;var r;for(var a=0;a<e.length;a++){var i=e[a];if(i.type==="infix"){if(t!==-1){throw new u("only one infix operator per group",this.lexer,-1)}t=a;r=i.value.replaceWith}}if(t!==-1){var n;var s;var l=e.slice(0,t);var o=e.slice(t+1);if(l.length===1&&l[0].type==="ordgroup"){n=l[0]}else{n=new h("ordgroup",l,this.mode)}if(o.length===1&&o[0].type==="ordgroup"){s=o[0]}else{s=new h("ordgroup",o,this.mode)}var p=this.callFunction(r,[n,s],null);return[new h(p.type,p,this.mode)]}else{return e}};var m=1;p.prototype.handleSupSubscript=function(e){var t=this.nextToken.text;var r=this.pos;this.consume();var i=this.parseGroup();if(!i){if(!this.settings.throwOnError&&this.nextToken.text[0]==="\\"){return this.handleUnsupportedCmd()}else{throw new u("Expected group after '"+t+"'",this.lexer,r+1)}}else if(i.isFunction){var n=a[i.result].greediness;if(n>m){return this.parseFunction(i)}else{throw new u("Got function '"+i.result+"' with no arguments "+"as "+e,this.lexer,r+1)}}else{return i.result}};p.prototype.handleUnsupportedCmd=function(){var e=this.nextToken.text;var t=[];for(var r=0;r<e.length;r++){t.push(new h("textord",e[r],"text"))}var a=new h("text",{body:t,type:"text"},this.mode);var i=new h("color",{color:this.settings.errorColor,value:[a],type:"color"},this.mode);this.consume();return i};p.prototype.parseAtom=function(){var e=this.parseImplicitGroup();if(this.mode==="text"){return e}var t;var r;while(true){var a=this.nextToken;if(a.text==="\\limits"||a.text==="\\nolimits"){if(!e||e.type!=="op"){throw new u("Limit controls must follow a math operator",this.lexer,this.pos)}else{var i=a.text==="\\limits";e.value.limits=i;e.value.alwaysHandleSupSub=true}this.consume()}else if(a.text==="^"){if(t){throw new u("Double superscript",this.lexer,this.pos)}t=this.handleSupSubscript("superscript")}else if(a.text==="_"){if(r){throw new u("Double subscript",this.lexer,this.pos)}r=this.handleSupSubscript("subscript")}else if(a.text==="'"){var n=new h("textord","\\prime",this.mode);var s=[n];this.consume();while(this.nextToken.text==="'"){s.push(n);this.consume()}t=new h("ordgroup",s,this.mode)}else{break}}if(t||r){return new h("supsub",{base:e,sup:t,sub:r},this.mode)}else{return e}};var f=["\\tiny","\\scriptsize","\\footnotesize","\\small","\\normalsize","\\large","\\Large","\\LARGE","\\huge","\\Huge"];var d=["\\displaystyle","\\textstyle","\\scriptstyle","\\scriptscriptstyle"];p.prototype.parseImplicitGroup=function(){var e=this.parseSymbol();if(e==null){return this.parseFunction()}var t=e.result;var r;if(t==="\\left"){var a=this.parseFunction(e);r=this.parseExpression(false);this.expect("\\right",false);var n=this.parseFunction();return new h("leftright",{body:r,left:a.value.value,right:n.value.value},this.mode)}else if(t==="\\begin"){var s=this.parseFunction(e);var o=s.value.name;if(!i.hasOwnProperty(o)){throw new u("No such environment: "+o,this.lexer,s.value.namepos)}var p=i[o];var c=this.parseArguments("\\begin{"+o+"}",p);var v={mode:this.mode,envName:o,parser:this,lexer:this.lexer,positions:c.pop()};var m=p.handler(v,c);this.expect("\\end",false);var g=this.parseFunction();if(g.value.name!==o){throw new u("Mismatch: \\begin{"+o+"} matched "+"by \\end{"+g.value.name+"}",this.lexer)}m.position=g.position;return m}else if(l.contains(f,t)){r=this.parseExpression(false);return new h("sizing",{size:"size"+(l.indexOf(f,t)+1),value:r},this.mode)}else if(l.contains(d,t)){r=this.parseExpression(true);return new h("styling",{style:t.slice(1,t.length-5),value:r},this.mode)}else{return this.parseFunction(e)}};p.prototype.parseFunction=function(e){if(!e){e=this.parseGroup()}if(e){if(e.isFunction){var t=e.result;var r=a[t];if(this.mode==="text"&&!r.allowedInText){throw new u("Can't use function '"+t+"' in text mode",this.lexer,e.position)}var i=this.parseArguments(t,r);var n=this.callFunction(t,i,i.pop());return new h(n.type,n,this.mode)}else{return e.result}}else{return null}};p.prototype.callFunction=function(e,t,r){var i={funcName:e,parser:this,lexer:this.lexer,positions:r};return a[e].handler(i,t)};p.prototype.parseArguments=function(e,t){var r=t.numArgs+t.numOptionalArgs;if(r===0){return[[this.pos]]}var i=t.greediness;var n=[this.pos];var s=[];for(var l=0;l<r;l++){var o=t.argTypes&&t.argTypes[l];var p;if(l<t.numOptionalArgs){if(o){p=this.parseSpecialGroup(o,true)}else{p=this.parseOptionalGroup()}if(!p){s.push(null);n.push(this.pos);continue}}else{if(o){p=this.parseSpecialGroup(o)}else{p=this.parseGroup()}if(!p){if(!this.settings.throwOnError&&this.nextToken.text[0]==="\\"){p=new c(this.handleUnsupportedCmd(this.nextToken.text),false)}else{throw new u("Expected group after '"+e+"'",this.lexer,this.pos)}}}var h;if(p.isFunction){var v=a[p.result].greediness;if(v>i){h=this.parseFunction(p)}else{throw new u("Got function '"+p.result+"' as "+"argument to '"+e+"'",this.lexer,this.pos-1)}}else{h=p.result}s.push(h);n.push(this.pos)}s.push(n);return s};p.prototype.parseSpecialGroup=function(e,t){var r=this.mode;if(e==="original"){e=r}if(e==="color"||e==="size"){var a=this.nextToken;if(t&&a.text!=="["){return null}this.mode=e;this.expect(t?"[":"{");var i=this.nextToken;this.mode=r;var n;if(e==="color"){n=i.text}else{n=i.data}this.consume();this.expect(t?"]":"}");return new c(new h(e,n,r),false)}else if(e==="text"){var s=this.lexer.lex(this.pos,"whitespace");this.pos=s.position}this.mode=e;this.nextToken=this.lexer.lex(this.pos,e);var l;if(t){l=this.parseOptionalGroup()}else{l=this.parseGroup()}this.mode=r;this.nextToken=this.lexer.lex(this.pos,r);return l};p.prototype.parseGroup=function(){if(this.nextToken.text==="{"){this.consume();var e=this.parseExpression(false);this.expect("}");return new c(new h("ordgroup",e,this.mode),false)}else{return this.parseSymbol()}};p.prototype.parseOptionalGroup=function(){if(this.nextToken.text==="["){this.consume();var e=this.parseExpression(false,"]");this.expect("]");return new c(new h("ordgroup",e,this.mode),false)}else{return null}};p.prototype.parseSymbol=function(){var e=this.nextToken;if(a[e.text]){this.consume();return new c(e.text,true)}else if(s[this.mode][e.text]){this.consume();return new c(new h(s[this.mode][e.text].group,e.text,this.mode),false)}else{return null}};p.prototype.ParseNode=h;t.exports=p},{"./Lexer":3,"./ParseError":5,"./environments":15,"./functions":18,"./parseData":20,"./symbols":22,"./utils":23}],7:[function(e,t,r){function a(e,t){return e===undefined?t:e}function i(e){e=e||{};this.displayMode=a(e.displayMode,false);this.throwOnError=a(e.throwOnError,true);this.errorColor=a(e.errorColor,"#cc0000")}t.exports=i},{}],8:[function(e,t,r){function a(e,t,r,a){this.id=e;this.size=t;this.cramped=a;this.sizeMultiplier=r}a.prototype.sup=function(){return m[f[this.id]]};a.prototype.sub=function(){return m[d[this.id]]};a.prototype.fracNum=function(){return m[g[this.id]]};a.prototype.fracDen=function(){return m[y[this.id]]};a.prototype.cramp=function(){return m[b[this.id]]};a.prototype.cls=function(){return c[this.size]+(this.cramped?" cramped":" uncramped")};a.prototype.reset=function(){return v[this.size]};var i=0;var n=1;var s=2;var l=3;var o=4;var u=5;var p=6;var h=7;var c=["displaystyle textstyle","textstyle","scriptstyle","scriptscriptstyle"];var v=["reset-textstyle","reset-textstyle","reset-scriptstyle","reset-scriptscriptstyle"];var m=[new a(i,0,1,false),new a(n,0,1,true),new a(s,1,1,false),new a(l,1,1,true),new a(o,2,.7,false),new a(u,2,.7,true),new a(p,3,.5,false),new a(h,3,.5,true)];var f=[o,u,o,u,p,h,p,h];var d=[u,u,u,u,h,h,h,h];var g=[s,l,o,u,p,h,p,h];var y=[l,l,u,u,h,h,h,h];var b=[n,n,l,l,u,u,h,h];t.exports={DISPLAY:m[i],TEXT:m[s],SCRIPT:m[o],SCRIPTSCRIPT:m[p]}},{}],9:[function(e,t,r){var a=e("./domTree");var i=e("./fontMetrics");var n=e("./symbols");var s=e("./utils");var l=["\\Gamma","\\Delta","\\Theta","\\Lambda","\\Xi","\\Pi","\\Sigma","\\Upsilon","\\Phi","\\Psi","\\Omega"];var o=["\u0131","\u0237"];var u=function(e,t,r,s,l){if(n[r][e]&&n[r][e].replace){e=n[r][e].replace}var o=i.getCharacterMetrics(e,t);var u;if(o){u=new a.symbolNode(e,o.height,o.depth,o.italic,o.skew,l)}else{typeof console!=="undefined"&&console.warn("No character metrics for '"+e+"' in style '"+t+"'");u=new a.symbolNode(e,0,0,0,0,l)}if(s){u.style.color=s}return u};var p=function(e,t,r,a){if(e==="\\"||n[t][e].font==="main"){return u(e,"Main-Regular",t,r,a)}else{return u(e,"AMS-Regular",t,r,a.concat(["amsrm"]))}};var h=function(e,t,r,a,i){if(i==="mathord"){return c(e,t,r,a)}else if(i==="textord"){return u(e,"Main-Regular",t,r,a.concat(["mathrm"]))}else{throw new Error("unexpected type: "+i+" in mathDefault")}};var c=function(e,t,r,a){if(/[0-9]/.test(e.charAt(0))||s.contains(o,e)||s.contains(l,e)){return u(e,"Main-Italic",t,r,a.concat(["mainit"]))}else{return u(e,"Math-Italic",t,r,a.concat(["mathit"]))}};var v=function(e,t,r){var a=e.mode;var l=e.value;if(n[a][l]&&n[a][l].replace){l=n[a][l].replace}var p=["mord"];var v=t.getColor();var m=t.font;if(m){if(m==="mathit"||s.contains(o,l)){return c(l,a,v,p)}else{var f=w[m].fontName;if(i.getCharacterMetrics(l,f)){return u(l,f,a,v,p.concat([m]))}else{return h(l,a,v,p,r)}}}else{return h(l,a,v,p,r)}};var m=function(e){var t=0;var r=0;var a=0;if(e.children){for(var i=0;i<e.children.length;i++){if(e.children[i].height>t){t=e.children[i].height}if(e.children[i].depth>r){r=e.children[i].depth}if(e.children[i].maxFontSize>a){a=e.children[i].maxFontSize}}}e.height=t;e.depth=r;e.maxFontSize=a};var f=function(e,t,r){var i=new a.span(e,t);m(i);if(r){i.style.color=r}return i};var d=function(e){var t=new a.documentFragment(e);m(t);return t};var g=function(e,t){var r=f([],[new a.symbolNode("\u200b")]);r.style.fontSize=t/e.style.sizeMultiplier+"em";var i=f(["fontsize-ensurer","reset-"+e.size,"size5"],[r]);return i};var y=function(e,t,r,i){var n;var s;var l;if(t==="individualShift"){var o=e;e=[o[0]];n=-o[0].shift-o[0].elem.depth;s=n;for(l=1;l<o.length;l++){var u=-o[l].shift-s-o[l].elem.depth;var p=u-(o[l-1].elem.height+o[l-1].elem.depth);s=s+u;e.push({type:"kern",size:p});e.push(o[l])}}else if(t==="top"){var h=r;for(l=0;l<e.length;l++){if(e[l].type==="kern"){h-=e[l].size}else{h-=e[l].elem.height+e[l].elem.depth}}n=h}else if(t==="bottom"){n=-r}else if(t==="shift"){n=-e[0].elem.depth-r}else if(t==="firstBaseline"){n=-e[0].elem.depth}else{n=0}var c=0;for(l=0;l<e.length;l++){if(e[l].type==="elem"){c=Math.max(c,e[l].elem.maxFontSize)}}var v=g(i,c);var m=[];s=n;for(l=0;l<e.length;l++){if(e[l].type==="kern"){s+=e[l].size}else{var d=e[l].elem;var y=-d.depth-s;s+=d.height+d.depth;var b=f([],[v,d]);b.height-=y;b.depth+=y;b.style.top=y+"em";m.push(b)}}var x=f(["baseline-fix"],[v,new a.symbolNode("\u200b")]);m.push(x);var w=f(["vlist"],m);w.height=Math.max(s,w.height);w.depth=Math.max(-n,w.depth);return w};var b={size1:.5,size2:.7,size3:.8,size4:.9,size5:1,size6:1.2,size7:1.44,size8:1.73,size9:2.07,size10:2.49};var x={"\\qquad":{size:"2em",className:"qquad"},"\\quad":{size:"1em",className:"quad"},"\\enspace":{size:"0.5em",className:"enspace"},"\\;":{size:"0.277778em",className:"thickspace"},"\\:":{size:"0.22222em",className:"mediumspace"},"\\,":{size:"0.16667em",className:"thinspace"},"\\!":{size:"-0.16667em",className:"negativethinspace"}};var w={mathbf:{variant:"bold",fontName:"Main-Bold"},mathrm:{variant:"normal",fontName:"Main-Regular"},mathbb:{variant:"double-struck",fontName:"AMS-Regular"},mathcal:{variant:"script",fontName:"Caligraphic-Regular"},mathfrak:{variant:"fraktur",fontName:"Fraktur-Regular"},mathscr:{variant:"script",fontName:"Script-Regular"},mathsf:{variant:"sans-serif",fontName:"SansSerif-Regular"},mathtt:{variant:"monospace",fontName:"Typewriter-Regular"}};t.exports={fontMap:w,makeSymbol:u,mathsym:p,makeSpan:f,makeFragment:d,makeVList:y,makeOrd:v,sizingMultiplier:b,spacingFunctions:x}},{"./domTree":14,"./fontMetrics":16,"./symbols":22,"./utils":23}],10:[function(e,t,r){var a=e("./ParseError");var i=e("./Style");var n=e("./buildCommon");var s=e("./delimiter");var l=e("./domTree");var o=e("./fontMetrics");var u=e("./utils");var p=n.makeSpan;var h=function(e,t,r){var a=[];for(var i=0;i<e.length;i++){var n=e[i];a.push(b(n,t,r));r=n}return a};var c={mathord:"mord",textord:"mord",bin:"mbin",rel:"mrel",text:"mord",open:"mopen",close:"mclose",inner:"minner",genfrac:"mord",array:"mord",spacing:"mord",punct:"mpunct",ordgroup:"mord",op:"mop",katex:"mord",overline:"mord",underline:"mord",rule:"mord",leftright:"minner",sqrt:"mord",accent:"mord"};var v=function(e){if(e==null){return c.mathord}else if(e.type==="supsub"){return v(e.value.base)}else if(e.type==="llap"||e.type==="rlap"){return v(e.value)}else if(e.type==="color"){return v(e.value.value)}else if(e.type==="sizing"){return v(e.value.value)}else if(e.type==="styling"){return v(e.value.value)}else if(e.type==="delimsizing"){return c[e.value.delimType]}else{return c[e.type]}};var m=function(e,t){if(!e){return false}else if(e.type==="op"){return e.value.limits&&(t.style.size===i.DISPLAY.size||e.value.alwaysHandleSupSub)}else if(e.type==="accent"){return d(e.value.base)}else{return null}};var f=function(e){if(!e){return false}else if(e.type==="ordgroup"){if(e.value.length===1){return f(e.value[0])}else{return e}}else if(e.type==="color"){if(e.value.value.length===1){return f(e.value.value[0])}else{return e}}else{return e}};var d=function(e){var t=f(e);return t.type==="mathord"||t.type==="textord"||t.type==="bin"||t.type==="rel"||t.type==="inner"||t.type==="open"||t.type==="close"||t.type==="punct"};var g=function(e){return p(["sizing","reset-"+e.size,"size5",e.style.reset(),i.TEXT.cls(),"nulldelimiter"])};var y={};y.mathord=function(e,t,r){return n.makeOrd(e,t,"mathord")};y.textord=function(e,t,r){return n.makeOrd(e,t,"textord")};y.bin=function(e,t,r){var a="mbin";var i=r;while(i&&i.type==="color"){var s=i.value.value;i=s[s.length-1]}if(!r||u.contains(["mbin","mopen","mrel","mop","mpunct"],v(i))){e.type="textord";a="mord"}return n.mathsym(e.value,e.mode,t.getColor(),[a])};y.rel=function(e,t,r){return n.mathsym(e.value,e.mode,t.getColor(),["mrel"])};y.open=function(e,t,r){return n.mathsym(e.value,e.mode,t.getColor(),["mopen"])};y.close=function(e,t,r){return n.mathsym(e.value,e.mode,t.getColor(),["mclose"])};y.inner=function(e,t,r){return n.mathsym(e.value,e.mode,t.getColor(),["minner"])};y.punct=function(e,t,r){return n.mathsym(e.value,e.mode,t.getColor(),["mpunct"])};y.ordgroup=function(e,t,r){return p(["mord",t.style.cls()],h(e.value,t.reset()))};y.text=function(e,t,r){return p(["text","mord",t.style.cls()],h(e.value.body,t.reset()))};y.color=function(e,t,r){var a=h(e.value.value,t.withColor(e.value.color),r);return new n.makeFragment(a)};y.supsub=function(e,t,r){if(m(e.value.base,t)){return y[e.value.base.type](e,t,r)}var a=b(e.value.base,t.reset());var s;var u;var h;var c;if(e.value.sup){h=b(e.value.sup,t.withStyle(t.style.sup()));s=p([t.style.reset(),t.style.sup().cls()],[h])}if(e.value.sub){c=b(e.value.sub,t.withStyle(t.style.sub()));u=p([t.style.reset(),t.style.sub().cls()],[c])}var f;var g;if(d(e.value.base)){f=0;g=0}else{f=a.height-o.metrics.supDrop;g=a.depth+o.metrics.subDrop}var x;if(t.style===i.DISPLAY){x=o.metrics.sup1}else if(t.style.cramped){x=o.metrics.sup3}else{x=o.metrics.sup2}var w=i.TEXT.sizeMultiplier*t.style.sizeMultiplier;var k=.5/o.metrics.ptPerEm/w+"em";var z;if(!e.value.sup){g=Math.max(g,o.metrics.sub1,c.height-.8*o.metrics.xHeight);z=n.makeVList([{type:"elem",elem:u}],"shift",g,t);z.children[0].style.marginRight=k;if(a instanceof l.symbolNode){z.children[0].style.marginLeft=-a.italic+"em"}}else if(!e.value.sub){f=Math.max(f,x,h.depth+.25*o.metrics.xHeight);z=n.makeVList([{type:"elem",elem:s}],"shift",-f,t);z.children[0].style.marginRight=k}else{f=Math.max(f,x,h.depth+.25*o.metrics.xHeight);g=Math.max(g,o.metrics.sub2);var S=o.metrics.defaultRuleThickness;if(f-h.depth-(c.height-g)<4*S){g=4*S-(f-h.depth)+c.height;var M=.8*o.metrics.xHeight-(f-h.depth);if(M>0){f+=M;g-=M}}z=n.makeVList([{type:"elem",elem:u,shift:g},{type:"elem",elem:s,shift:-f}],"individualShift",null,t);if(a instanceof l.symbolNode){z.children[0].style.marginLeft=-a.italic+"em"}z.children[0].style.marginRight=k;z.children[1].style.marginRight=k}return p([v(e.value.base)],[a,z])};y.genfrac=function(e,t,r){var a=t.style;if(e.value.size==="display"){a=i.DISPLAY}else if(e.value.size==="text"){a=i.TEXT}var l=a.fracNum();var u=a.fracDen();var h=b(e.value.numer,t.withStyle(l));var c=p([a.reset(),l.cls()],[h]);var v=b(e.value.denom,t.withStyle(u));var m=p([a.reset(),u.cls()],[v]);var f;if(e.value.hasBarLine){f=o.metrics.defaultRuleThickness/t.style.sizeMultiplier}else{f=0}var d;var y;var x;if(a.size===i.DISPLAY.size){d=o.metrics.num1;if(f>0){y=3*f}else{y=7*o.metrics.defaultRuleThickness}x=o.metrics.denom1}else{if(f>0){d=o.metrics.num2;y=f}else{d=o.metrics.num3;y=3*o.metrics.defaultRuleThickness}x=o.metrics.denom2}var w;if(f===0){var k=d-h.depth-(v.height-x);if(k<y){d+=.5*(y-k);x+=.5*(y-k)}w=n.makeVList([{type:"elem",elem:m,shift:x},{type:"elem",elem:c,shift:-d}],"individualShift",null,t)}else{var z=o.metrics.axisHeight;if(d-h.depth-(z+.5*f)<y){d+=y-(d-h.depth-(z+.5*f))}if(z-.5*f-(v.height-x)<y){x+=y-(z-.5*f-(v.height-x))}var S=p([t.style.reset(),i.TEXT.cls(),"frac-line"]);S.height=f;var M=-(z-.5*f);w=n.makeVList([{type:"elem",elem:m,shift:x},{type:"elem",elem:S,shift:M},{type:"elem",elem:c,shift:-d}],"individualShift",null,t)}w.height*=a.sizeMultiplier/t.style.sizeMultiplier;w.depth*=a.sizeMultiplier/t.style.sizeMultiplier;var T;if(a.size===i.DISPLAY.size){T=o.metrics.delim1}else{T=o.metrics.getDelim2(a)}var N;var q;if(e.value.leftDelim==null){N=g(t)}else{N=s.customSizedDelim(e.value.leftDelim,T,true,t.withStyle(a),e.mode)}if(e.value.rightDelim==null){q=g(t)}else{q=s.customSizedDelim(e.value.rightDelim,T,true,t.withStyle(a),e.mode)}return p(["mord",t.style.reset(),a.cls()],[N,p(["mfrac"],[w]),q],t.getColor())};y.array=function(e,t,r){var i;var s;var l=e.value.body.length;var h=0;var c=new Array(l);var v=1/o.metrics.ptPerEm;var m=5*v;var f=12*v;var d=u.deflt(e.value.arraystretch,1);var g=d*f;var y=.7*g;var x=.3*g;var w=0;for(i=0;i<e.value.body.length;++i){var k=e.value.body[i];var z=y;var S=x;if(h<k.length){h=k.length}var M=new Array(k.length);for(s=0;s<k.length;++s){var T=b(k[s],t);if(S<T.depth){S=T.depth}if(z<T.height){z=T.height}M[s]=T}var N=0;if(e.value.rowGaps[i]){N=e.value.rowGaps[i].value;switch(N.unit){case"em":N=N.number;break;case"ex":N=N.number*o.metrics.emPerEx;break;default:console.error("Can't handle unit "+N.unit);N=0}if(N>0){N+=x;if(S<N){S=N}N=0}}M.height=z;M.depth=S;w+=z;M.pos=w;w+=S+N;c[i]=M}var q=w/2+o.metrics.axisHeight;var A=e.value.cols||[];var C=[];var R;var E;for(s=0,E=0;s<h||E<A.length;++s,++E){var P=A[E]||{};var D=true;while(P.type==="separator"){if(!D){R=p(["arraycolsep"],[]);R.style.width=o.metrics.doubleRuleSep+"em";C.push(R)}if(P.separator==="|"){var L=p(["vertical-separator"],[]);L.style.height=w+"em";L.style.verticalAlign=-(w-q)+"em";C.push(L)}else{throw new a("Invalid separator type: "+P.separator)}E++;P=A[E]||{};D=false}if(s>=h){continue}var O;if(s>0||e.value.hskipBeforeAndAfter){O=u.deflt(P.pregap,m);if(O!==0){R=p(["arraycolsep"],[]);R.style.width=O+"em";C.push(R)}}var I=[];for(i=0;i<l;++i){var B=c[i];var F=B[s];if(!F){continue}var _=B.pos-q;F.depth=B.depth;F.height=B.height;I.push({type:"elem",elem:F,shift:_})}I=n.makeVList(I,"individualShift",null,t);I=p(["col-align-"+(P.align||"c")],[I]);C.push(I);if(s<h-1||e.value.hskipBeforeAndAfter){O=u.deflt(P.postgap,m);if(O!==0){R=p(["arraycolsep"],[]);R.style.width=O+"em";C.push(R)}}}c=p(["mtable"],C);return p(["mord"],[c],t.getColor())};y.spacing=function(e,t,r){if(e.value==="\\ "||e.value==="\\space"||e.value===" "||e.value==="~"){return p(["mord","mspace"],[n.mathsym(e.value,e.mode)])}else{return p(["mord","mspace",n.spacingFunctions[e.value].className])}};y.llap=function(e,t,r){var a=p(["inner"],[b(e.value.body,t.reset())]);var i=p(["fix"],[]);return p(["llap",t.style.cls()],[a,i])};y.rlap=function(e,t,r){var a=p(["inner"],[b(e.value.body,t.reset())]);var i=p(["fix"],[]);return p(["rlap",t.style.cls()],[a,i])};y.op=function(e,t,r){var a;var s;var l=false;if(e.type==="supsub"){a=e.value.sup;s=e.value.sub;e=e.value.base;l=true}var h=["\\smallint"];var c=false;if(t.style.size===i.DISPLAY.size&&e.value.symbol&&!u.contains(h,e.value.body)){c=true}var v;var m=0;var f=0;if(e.value.symbol){var d=c?"Size2-Regular":"Size1-Regular";v=n.makeSymbol(e.value.body,d,"math",t.getColor(),["op-symbol",c?"large-op":"small-op","mop"]);m=(v.height-v.depth)/2-o.metrics.axisHeight*t.style.sizeMultiplier;f=v.italic}else{var g=[];for(var y=1;y<e.value.body.length;y++){g.push(n.mathsym(e.value.body[y],e.mode))}v=p(["mop"],g,t.getColor())}if(l){v=p([],[v]);var x;var w;var k;var z;if(a){var S=b(a,t.withStyle(t.style.sup()));x=p([t.style.reset(),t.style.sup().cls()],[S]);w=Math.max(o.metrics.bigOpSpacing1,o.metrics.bigOpSpacing3-S.depth)}if(s){var M=b(s,t.withStyle(t.style.sub()));k=p([t.style.reset(),t.style.sub().cls()],[M]);z=Math.max(o.metrics.bigOpSpacing2,o.metrics.bigOpSpacing4-M.height)}var T;var N;var q;if(!a){N=v.height-m;T=n.makeVList([{type:"kern",size:o.metrics.bigOpSpacing5},{type:"elem",elem:k},{type:"kern",size:z},{type:"elem",elem:v}],"top",N,t);T.children[0].style.marginLeft=-f+"em"}else if(!s){q=v.depth+m;T=n.makeVList([{type:"elem",elem:v},{type:"kern",size:w},{type:"elem",elem:x},{type:"kern",size:o.metrics.bigOpSpacing5}],"bottom",q,t);T.children[1].style.marginLeft=f+"em"}else if(!a&&!s){return v}else{q=o.metrics.bigOpSpacing5+k.height+k.depth+z+v.depth+m;T=n.makeVList([{type:"kern",size:o.metrics.bigOpSpacing5},{type:"elem",elem:k},{type:"kern",size:z},{type:"elem",elem:v},{type:"kern",size:w},{type:"elem",elem:x},{type:"kern",size:o.metrics.bigOpSpacing5}],"bottom",q,t);T.children[0].style.marginLeft=-f+"em";T.children[2].style.marginLeft=f+"em"}return p(["mop","op-limits"],[T])}else{if(e.value.symbol){v.style.top=m+"em"}return v}};y.katex=function(e,t,r){var a=p(["k"],[n.mathsym("K",e.mode)]);var i=p(["a"],[n.mathsym("A",e.mode)]);i.height=(i.height+.2)*.75;i.depth=(i.height-.2)*.75;var s=p(["t"],[n.mathsym("T",e.mode)]);var l=p(["e"],[n.mathsym("E",e.mode)]);l.height=l.height-.2155;l.depth=l.depth+.2155;var o=p(["x"],[n.mathsym("X",e.mode)]);return p(["katex-logo","mord"],[a,i,s,l,o],t.getColor())};y.overline=function(e,t,r){var a=b(e.value.body,t.withStyle(t.style.cramp()));var s=o.metrics.defaultRuleThickness/t.style.sizeMultiplier;var l=p([t.style.reset(),i.TEXT.cls(),"overline-line"]);l.height=s;l.maxFontSize=1;var u=n.makeVList([{type:"elem",elem:a},{type:"kern",size:3*s},{type:"elem",elem:l},{type:"kern",size:s}],"firstBaseline",null,t);return p(["overline","mord"],[u],t.getColor())};y.underline=function(e,t,r){var a=b(e.value.body,t);var s=o.metrics.defaultRuleThickness/t.style.sizeMultiplier;var l=p([t.style.reset(),i.TEXT.cls(),"underline-line"]);l.height=s;l.maxFontSize=1;var u=n.makeVList([{type:"kern",size:s},{type:"elem",elem:l},{type:"kern",size:3*s},{type:"elem",elem:a}],"top",a.height,t);return p(["underline","mord"],[u],t.getColor())};y.sqrt=function(e,t,r){var a=b(e.value.body,t.withStyle(t.style.cramp()));var l=o.metrics.defaultRuleThickness/t.style.sizeMultiplier;var u=p([t.style.reset(),i.TEXT.cls(),"sqrt-line"],[],t.getColor());u.height=l;u.maxFontSize=1;var h=l;if(t.style.id<i.TEXT.id){h=o.metrics.xHeight}var c=l+h/4;var v=(a.height+a.depth)*t.style.sizeMultiplier;var m=v+c+l;var f=p(["sqrt-sign"],[s.customSizedDelim("\\surd",m,false,t,e.mode)],t.getColor());var d=f.height+f.depth-l;if(d>a.height+a.depth+c){c=(c+d-a.height-a.depth)/2}var g=-(a.height+c+l)+f.height;f.style.top=g+"em";f.height-=g;f.depth+=g;var y;if(a.height===0&&a.depth===0){y=p()}else{y=n.makeVList([{type:"elem",elem:a},{type:"kern",size:c},{type:"elem",elem:u},{type:"kern",size:l}],"firstBaseline",null,t)}if(!e.value.index){return p(["sqrt","mord"],[f,y])}else{var x=b(e.value.index,t.withStyle(i.SCRIPTSCRIPT));var w=p([t.style.reset(),i.SCRIPTSCRIPT.cls()],[x]);var k=Math.max(f.height,y.height);var z=Math.max(f.depth,y.depth);var S=.6*(k-z);var M=n.makeVList([{type:"elem",elem:w}],"shift",-S,t);var T=p(["root"],[M]);return p(["sqrt","mord"],[T,f,y]);
  }};y.sizing=function(e,t,r){var a=h(e.value.value,t.withSize(e.value.size),r);var i=p(["mord"],[p(["sizing","reset-"+t.size,e.value.size,t.style.cls()],a)]);var s=n.sizingMultiplier[e.value.size];i.maxFontSize=s*t.style.sizeMultiplier;return i};y.styling=function(e,t,r){var a={display:i.DISPLAY,text:i.TEXT,script:i.SCRIPT,scriptscript:i.SCRIPTSCRIPT};var n=a[e.value.style];var s=h(e.value.value,t.withStyle(n),r);return p([t.style.reset(),n.cls()],s)};y.font=function(e,t,r){var a=e.value.font;return b(e.value.body,t.withFont(a),r)};y.delimsizing=function(e,t,r){var a=e.value.value;if(a==="."){return p([c[e.value.delimType]])}return p([c[e.value.delimType]],[s.sizedDelim(a,e.value.size,t,e.mode)])};y.leftright=function(e,t,r){var a=h(e.value.body,t.reset());var i=0;var n=0;for(var l=0;l<a.length;l++){i=Math.max(a[l].height,i);n=Math.max(a[l].depth,n)}i*=t.style.sizeMultiplier;n*=t.style.sizeMultiplier;var o;if(e.value.left==="."){o=g(t)}else{o=s.leftRightDelim(e.value.left,i,n,t,e.mode)}a.unshift(o);var u;if(e.value.right==="."){u=g(t)}else{u=s.leftRightDelim(e.value.right,i,n,t,e.mode)}a.push(u);return p(["minner",t.style.cls()],a,t.getColor())};y.rule=function(e,t,r){var a=p(["mord","rule"],[],t.getColor());var i=0;if(e.value.shift){i=e.value.shift.number;if(e.value.shift.unit==="ex"){i*=o.metrics.xHeight}}var n=e.value.width.number;if(e.value.width.unit==="ex"){n*=o.metrics.xHeight}var s=e.value.height.number;if(e.value.height.unit==="ex"){s*=o.metrics.xHeight}i/=t.style.sizeMultiplier;n/=t.style.sizeMultiplier;s/=t.style.sizeMultiplier;a.style.borderRightWidth=n+"em";a.style.borderTopWidth=s+"em";a.style.bottom=i+"em";a.width=n;a.height=s+i;a.depth=-i;return a};y.accent=function(e,t,r){var a=e.value.base;var i;if(e.type==="supsub"){var s=e;e=s.value.base;a=e.value.base;s.value.base=a;i=b(s,t.reset(),r)}var l=b(a,t.withStyle(t.style.cramp()));var u;if(d(a)){var h=f(a);var c=b(h,t.withStyle(t.style.cramp()));u=c.skew}else{u=0}var v=Math.min(l.height,o.metrics.xHeight);var m=n.makeSymbol(e.value.accent,"Main-Regular","math",t.getColor());m.italic=0;var g=e.value.accent==="\\vec"?"accent-vec":null;var y=p(["accent-body",g],[p([],[m])]);y=n.makeVList([{type:"elem",elem:l},{type:"kern",size:-v},{type:"elem",elem:y}],"firstBaseline",null,t);y.children[1].style.marginLeft=2*u+"em";var x=p(["mord","accent"],[y]);if(i){i.children[0]=x;i.height=Math.max(x.height,i.height);i.classes[0]="mord";return i}else{return x}};y.phantom=function(e,t,r){var a=h(e.value.value,t.withPhantom(),r);return new n.makeFragment(a)};var b=function(e,t,r){if(!e){return p()}if(y[e.type]){var i=y[e.type](e,t,r);var s;if(t.style!==t.parentStyle){s=t.style.sizeMultiplier/t.parentStyle.sizeMultiplier;i.height*=s;i.depth*=s}if(t.size!==t.parentSize){s=n.sizingMultiplier[t.size]/n.sizingMultiplier[t.parentSize];i.height*=s;i.depth*=s}return i}else{throw new a("Got group of unknown type: '"+e.type+"'")}};var x=function(e,t){e=JSON.parse(JSON.stringify(e));var r=h(e,t);var a=p(["base",t.style.cls()],r);var i=p(["strut"]);var n=p(["strut","bottom"]);i.style.height=a.height+"em";n.style.height=a.height+a.depth+"em";n.style.verticalAlign=-a.depth+"em";var s=p(["katex-html"],[i,n,a]);s.setAttribute("aria-hidden","true");return s};t.exports=x},{"./ParseError":5,"./Style":8,"./buildCommon":9,"./delimiter":13,"./domTree":14,"./fontMetrics":16,"./utils":23}],11:[function(e,t,r){var a=e("./buildCommon");var i=e("./fontMetrics");var n=e("./mathMLTree");var s=e("./ParseError");var l=e("./symbols");var o=e("./utils");var u=a.makeSpan;var p=a.fontMap;var h=function(e,t){if(l[t][e]&&l[t][e].replace){e=l[t][e].replace}return new n.TextNode(e)};var c=function(e,t){var r=t.font;if(!r){return null}var a=e.mode;if(r==="mathit"){return"italic"}var n=e.value;if(o.contains(["\\imath","\\jmath"],n)){return null}if(l[a][n]&&l[a][n].replace){n=l[a][n].replace}var s=p[r].fontName;if(i.getCharacterMetrics(n,s)){return p[t.font].variant}return null};var v={};v.mathord=function(e,t){var r=new n.MathNode("mi",[h(e.value,e.mode)]);var a=c(e,t);if(a){r.setAttribute("mathvariant",a)}return r};v.textord=function(e,t){var r=h(e.value,e.mode);var a=c(e,t)||"normal";var i;if(/[0-9]/.test(e.value)){i=new n.MathNode("mn",[r]);if(t.font){i.setAttribute("mathvariant",a)}}else{i=new n.MathNode("mi",[r]);i.setAttribute("mathvariant",a)}return i};v.bin=function(e){var t=new n.MathNode("mo",[h(e.value,e.mode)]);return t};v.rel=function(e){var t=new n.MathNode("mo",[h(e.value,e.mode)]);return t};v.open=function(e){var t=new n.MathNode("mo",[h(e.value,e.mode)]);return t};v.close=function(e){var t=new n.MathNode("mo",[h(e.value,e.mode)]);return t};v.inner=function(e){var t=new n.MathNode("mo",[h(e.value,e.mode)]);return t};v.punct=function(e){var t=new n.MathNode("mo",[h(e.value,e.mode)]);t.setAttribute("separator","true");return t};v.ordgroup=function(e,t){var r=m(e.value,t);var a=new n.MathNode("mrow",r);return a};v.text=function(e,t){var r=m(e.value.body,t);var a=new n.MathNode("mtext",r);return a};v.color=function(e,t){var r=m(e.value.value,t);var a=new n.MathNode("mstyle",r);a.setAttribute("mathcolor",e.value.color);return a};v.supsub=function(e,t){var r=[f(e.value.base,t)];if(e.value.sub){r.push(f(e.value.sub,t))}if(e.value.sup){r.push(f(e.value.sup,t))}var a;if(!e.value.sub){a="msup"}else if(!e.value.sup){a="msub"}else{a="msubsup"}var i=new n.MathNode(a,r);return i};v.genfrac=function(e,t){var r=new n.MathNode("mfrac",[f(e.value.numer,t),f(e.value.denom,t)]);if(!e.value.hasBarLine){r.setAttribute("linethickness","0px")}if(e.value.leftDelim!=null||e.value.rightDelim!=null){var a=[];if(e.value.leftDelim!=null){var i=new n.MathNode("mo",[new n.TextNode(e.value.leftDelim)]);i.setAttribute("fence","true");a.push(i)}a.push(r);if(e.value.rightDelim!=null){var s=new n.MathNode("mo",[new n.TextNode(e.value.rightDelim)]);s.setAttribute("fence","true");a.push(s)}var l=new n.MathNode("mrow",a);return l}return r};v.array=function(e,t){return new n.MathNode("mtable",e.value.body.map(function(e){return new n.MathNode("mtr",e.map(function(e){return new n.MathNode("mtd",[f(e,t)])}))}))};v.sqrt=function(e,t){var r;if(e.value.index){r=new n.MathNode("mroot",[f(e.value.body,t),f(e.value.index,t)])}else{r=new n.MathNode("msqrt",[f(e.value.body,t)])}return r};v.leftright=function(e,t){var r=m(e.value.body,t);if(e.value.left!=="."){var a=new n.MathNode("mo",[h(e.value.left,e.mode)]);a.setAttribute("fence","true");r.unshift(a)}if(e.value.right!=="."){var i=new n.MathNode("mo",[h(e.value.right,e.mode)]);i.setAttribute("fence","true");r.push(i)}var s=new n.MathNode("mrow",r);return s};v.accent=function(e,t){var r=new n.MathNode("mo",[h(e.value.accent,e.mode)]);var a=new n.MathNode("mover",[f(e.value.base,t),r]);a.setAttribute("accent","true");return a};v.spacing=function(e){var t;if(e.value==="\\ "||e.value==="\\space"||e.value===" "||e.value==="~"){t=new n.MathNode("mtext",[new n.TextNode("\xa0")])}else{t=new n.MathNode("mspace");t.setAttribute("width",a.spacingFunctions[e.value].size)}return t};v.op=function(e){var t;if(e.value.symbol){t=new n.MathNode("mo",[h(e.value.body,e.mode)])}else{t=new n.MathNode("mi",[new n.TextNode(e.value.body.slice(1))])}return t};v.katex=function(e){var t=new n.MathNode("mtext",[new n.TextNode("KaTeX")]);return t};v.font=function(e,t){var r=e.value.font;return f(e.value.body,t.withFont(r))};v.delimsizing=function(e){var t=[];if(e.value.value!=="."){t.push(h(e.value.value,e.mode))}var r=new n.MathNode("mo",t);if(e.value.delimType==="open"||e.value.delimType==="close"){r.setAttribute("fence","true")}else{r.setAttribute("fence","false")}return r};v.styling=function(e,t){var r=m(e.value.value,t);var a=new n.MathNode("mstyle",r);var i={display:["0","true"],text:["0","false"],script:["1","false"],scriptscript:["2","false"]};var s=i[e.value.style];a.setAttribute("scriptlevel",s[0]);a.setAttribute("displaystyle",s[1]);return a};v.sizing=function(e,t){var r=m(e.value.value,t);var i=new n.MathNode("mstyle",r);i.setAttribute("mathsize",a.sizingMultiplier[e.value.size]+"em");return i};v.overline=function(e,t){var r=new n.MathNode("mo",[new n.TextNode("\u203e")]);r.setAttribute("stretchy","true");var a=new n.MathNode("mover",[f(e.value.body,t),r]);a.setAttribute("accent","true");return a};v.underline=function(e,t){var r=new n.MathNode("mo",[new n.TextNode("\u203e")]);r.setAttribute("stretchy","true");var a=new n.MathNode("munder",[f(e.value.body,t),r]);a.setAttribute("accentunder","true");return a};v.rule=function(e){var t=new n.MathNode("mrow");return t};v.llap=function(e,t){var r=new n.MathNode("mpadded",[f(e.value.body,t)]);r.setAttribute("lspace","-1width");r.setAttribute("width","0px");return r};v.rlap=function(e,t){var r=new n.MathNode("mpadded",[f(e.value.body,t)]);r.setAttribute("width","0px");return r};v.phantom=function(e,t,r){var a=m(e.value.value,t);return new n.MathNode("mphantom",a)};var m=function(e,t){var r=[];for(var a=0;a<e.length;a++){var i=e[a];r.push(f(i,t))}return r};var f=function(e,t){if(!e){return new n.MathNode("mrow")}if(v[e.type]){return v[e.type](e,t)}else{throw new s("Got group of unknown type: '"+e.type+"'")}};var d=function(e,t,r){var a=m(e,r);var i=new n.MathNode("mrow",a);var s=new n.MathNode("annotation",[new n.TextNode(t)]);s.setAttribute("encoding","application/x-tex");var l=new n.MathNode("semantics",[i,s]);var o=new n.MathNode("math",[l]);return u(["katex-mathml"],[o])};t.exports=d},{"./ParseError":5,"./buildCommon":9,"./fontMetrics":16,"./mathMLTree":19,"./symbols":22,"./utils":23}],12:[function(e,t,r){var a=e("./buildHTML");var i=e("./buildMathML");var n=e("./buildCommon");var s=e("./Options");var l=e("./Settings");var o=e("./Style");var u=n.makeSpan;var p=function(e,t,r){r=r||new l({});var n=o.TEXT;if(r.displayMode){n=o.DISPLAY}var p=new s({style:n,size:"size5"});var h=i(e,t,p);var c=a(e,p);var v=u(["katex"],[h,c]);if(r.displayMode){return u(["katex-display"],[v])}else{return v}};t.exports=p},{"./Options":4,"./Settings":7,"./Style":8,"./buildCommon":9,"./buildHTML":10,"./buildMathML":11}],13:[function(e,t,r){var a=e("./ParseError");var i=e("./Style");var n=e("./buildCommon");var s=e("./fontMetrics");var l=e("./symbols");var o=e("./utils");var u=n.makeSpan;var p=function(e,t){if(l.math[e]&&l.math[e].replace){return s.getCharacterMetrics(l.math[e].replace,t)}else{return s.getCharacterMetrics(e,t)}};var h=function(e,t,r){return n.makeSymbol(e,"Size"+t+"-Regular",r)};var c=function(e,t,r){var a=u(["style-wrap",r.style.reset(),t.cls()],[e]);var i=t.sizeMultiplier/r.style.sizeMultiplier;a.height*=i;a.depth*=i;a.maxFontSize=t.sizeMultiplier;return a};var v=function(e,t,r,a,i){var l=n.makeSymbol(e,"Main-Regular",i);var o=c(l,t,a);if(r){var u=(1-a.style.sizeMultiplier/t.sizeMultiplier)*s.metrics.axisHeight;o.style.top=u+"em";o.height-=u;o.depth+=u}return o};var m=function(e,t,r,a,n){var l=h(e,t,n);var o=c(u(["delimsizing","size"+t],[l],a.getColor()),i.TEXT,a);if(r){var p=(1-a.style.sizeMultiplier)*s.metrics.axisHeight;o.style.top=p+"em";o.height-=p;o.depth+=p}return o};var f=function(e,t,r){var a;if(t==="Size1-Regular"){a="delim-size1"}else if(t==="Size4-Regular"){a="delim-size4"}var i=u(["delimsizinginner",a],[u([],[n.makeSymbol(e,t,r)])]);return{type:"elem",elem:i}};var d=function(e,t,r,a,l){var o;var h;var v;var m;o=v=m=e;h=null;var d="Size1-Regular";if(e==="\\uparrow"){v=m="\u23d0"}else if(e==="\\Uparrow"){v=m="\u2016"}else if(e==="\\downarrow"){o=v="\u23d0"}else if(e==="\\Downarrow"){o=v="\u2016"}else if(e==="\\updownarrow"){o="\\uparrow";v="\u23d0";m="\\downarrow"}else if(e==="\\Updownarrow"){o="\\Uparrow";v="\u2016";m="\\Downarrow"}else if(e==="["||e==="\\lbrack"){o="\u23a1";v="\u23a2";m="\u23a3";d="Size4-Regular"}else if(e==="]"||e==="\\rbrack"){o="\u23a4";v="\u23a5";m="\u23a6";d="Size4-Regular"}else if(e==="\\lfloor"){v=o="\u23a2";m="\u23a3";d="Size4-Regular"}else if(e==="\\lceil"){o="\u23a1";v=m="\u23a2";d="Size4-Regular"}else if(e==="\\rfloor"){v=o="\u23a5";m="\u23a6";d="Size4-Regular"}else if(e==="\\rceil"){o="\u23a4";v=m="\u23a5";d="Size4-Regular"}else if(e==="("){o="\u239b";v="\u239c";m="\u239d";d="Size4-Regular"}else if(e===")"){o="\u239e";v="\u239f";m="\u23a0";d="Size4-Regular"}else if(e==="\\{"||e==="\\lbrace"){o="\u23a7";h="\u23a8";m="\u23a9";v="\u23aa";d="Size4-Regular"}else if(e==="\\}"||e==="\\rbrace"){o="\u23ab";h="\u23ac";m="\u23ad";v="\u23aa";d="Size4-Regular"}else if(e==="\\lgroup"){o="\u23a7";m="\u23a9";v="\u23aa";d="Size4-Regular"}else if(e==="\\rgroup"){o="\u23ab";m="\u23ad";v="\u23aa";d="Size4-Regular"}else if(e==="\\lmoustache"){o="\u23a7";m="\u23ad";v="\u23aa";d="Size4-Regular"}else if(e==="\\rmoustache"){o="\u23ab";m="\u23a9";v="\u23aa";d="Size4-Regular"}else if(e==="\\surd"){o="\ue001";m="\u23b7";v="\ue000";d="Size4-Regular"}var g=p(o,d);var y=g.height+g.depth;var b=p(v,d);var x=b.height+b.depth;var w=p(m,d);var k=w.height+w.depth;var z=0;var S=1;if(h!==null){var M=p(h,d);z=M.height+M.depth;S=2}var T=y+k+z;var N=Math.ceil((t-T)/(S*x));var q=T+N*S*x;var A=s.metrics.axisHeight;if(r){A*=a.style.sizeMultiplier}var C=q/2-A;var R=[];R.push(f(m,d,l));var E;if(h===null){for(E=0;E<N;E++){R.push(f(v,d,l))}}else{for(E=0;E<N;E++){R.push(f(v,d,l))}R.push(f(h,d,l));for(E=0;E<N;E++){R.push(f(v,d,l))}}R.push(f(o,d,l));var P=n.makeVList(R,"bottom",C,a);return c(u(["delimsizing","mult"],[P],a.getColor()),i.TEXT,a)};var g=["(",")","[","\\lbrack","]","\\rbrack","\\{","\\lbrace","\\}","\\rbrace","\\lfloor","\\rfloor","\\lceil","\\rceil","\\surd"];var y=["\\uparrow","\\downarrow","\\updownarrow","\\Uparrow","\\Downarrow","\\Updownarrow","|","\\|","\\vert","\\Vert","\\lvert","\\rvert","\\lVert","\\rVert","\\lgroup","\\rgroup","\\lmoustache","\\rmoustache"];var b=["<",">","\\langle","\\rangle","/","\\backslash","\\lt","\\gt"];var x=[0,1.2,1.8,2.4,3];var w=function(e,t,r,i){if(e==="<"||e==="\\lt"){e="\\langle"}else if(e===">"||e==="\\gt"){e="\\rangle"}if(o.contains(g,e)||o.contains(b,e)){return m(e,t,false,r,i)}else if(o.contains(y,e)){return d(e,x[t],false,r,i)}else{throw new a("Illegal delimiter: '"+e+"'")}};var k=[{type:"small",style:i.SCRIPTSCRIPT},{type:"small",style:i.SCRIPT},{type:"small",style:i.TEXT},{type:"large",size:1},{type:"large",size:2},{type:"large",size:3},{type:"large",size:4}];var z=[{type:"small",style:i.SCRIPTSCRIPT},{type:"small",style:i.SCRIPT},{type:"small",style:i.TEXT},{type:"stack"}];var S=[{type:"small",style:i.SCRIPTSCRIPT},{type:"small",style:i.SCRIPT},{type:"small",style:i.TEXT},{type:"large",size:1},{type:"large",size:2},{type:"large",size:3},{type:"large",size:4},{type:"stack"}];var M=function(e){if(e.type==="small"){return"Main-Regular"}else if(e.type==="large"){return"Size"+e.size+"-Regular"}else if(e.type==="stack"){return"Size4-Regular"}};var T=function(e,t,r,a){var i=Math.min(2,3-a.style.size);for(var n=i;n<r.length;n++){if(r[n].type==="stack"){break}var s=p(e,M(r[n]));var l=s.height+s.depth;if(r[n].type==="small"){l*=r[n].style.sizeMultiplier}if(l>t){return r[n]}}return r[r.length-1]};var N=function(e,t,r,a,i){if(e==="<"||e==="\\lt"){e="\\langle"}else if(e===">"||e==="\\gt"){e="\\rangle"}var n;if(o.contains(b,e)){n=k}else if(o.contains(g,e)){n=S}else{n=z}var s=T(e,t,n,a);if(s.type==="small"){return v(e,s.style,r,a,i)}else if(s.type==="large"){return m(e,s.size,r,a,i)}else if(s.type==="stack"){return d(e,t,r,a,i)}};var q=function(e,t,r,a,i){var n=s.metrics.axisHeight*a.style.sizeMultiplier;var l=901;var o=5/s.metrics.ptPerEm;var u=Math.max(t-n,r+n);var p=Math.max(u/500*l,2*u-o);return N(e,p,true,a,i)};t.exports={sizedDelim:w,customSizedDelim:N,leftRightDelim:q}},{"./ParseError":5,"./Style":8,"./buildCommon":9,"./fontMetrics":16,"./symbols":22,"./utils":23}],14:[function(e,t,r){var a=e("./utils");var i=function(e){e=e.slice();for(var t=e.length-1;t>=0;t--){if(!e[t]){e.splice(t,1)}}return e.join(" ")};function n(e,t,r,a,i,n){this.classes=e||[];this.children=t||[];this.height=r||0;this.depth=a||0;this.maxFontSize=i||0;this.style=n||{};this.attributes={}}n.prototype.setAttribute=function(e,t){this.attributes[e]=t};n.prototype.toNode=function(){var e=document.createElement("span");e.className=i(this.classes);for(var t in this.style){if(Object.prototype.hasOwnProperty.call(this.style,t)){e.style[t]=this.style[t]}}for(var r in this.attributes){if(Object.prototype.hasOwnProperty.call(this.attributes,r)){e.setAttribute(r,this.attributes[r])}}for(var a=0;a<this.children.length;a++){e.appendChild(this.children[a].toNode())}return e};n.prototype.toMarkup=function(){var e="<span";if(this.classes.length){e+=' class="';e+=a.escape(i(this.classes));e+='"'}var t="";for(var r in this.style){if(this.style.hasOwnProperty(r)){t+=a.hyphenate(r)+":"+this.style[r]+";"}}if(t){e+=' style="'+a.escape(t)+'"'}for(var n in this.attributes){if(Object.prototype.hasOwnProperty.call(this.attributes,n)){e+=" "+n+'="';e+=a.escape(this.attributes[n]);e+='"'}}e+=">";for(var s=0;s<this.children.length;s++){e+=this.children[s].toMarkup()}e+="</span>";return e};function s(e,t,r,a){this.children=e||[];this.height=t||0;this.depth=r||0;this.maxFontSize=a||0}s.prototype.toNode=function(){var e=document.createDocumentFragment();for(var t=0;t<this.children.length;t++){e.appendChild(this.children[t].toNode())}return e};s.prototype.toMarkup=function(){var e="";for(var t=0;t<this.children.length;t++){e+=this.children[t].toMarkup()}return e};function l(e,t,r,a,i,n,s){this.value=e||"";this.height=t||0;this.depth=r||0;this.italic=a||0;this.skew=i||0;this.classes=n||[];this.style=s||{};this.maxFontSize=0}l.prototype.toNode=function(){var e=document.createTextNode(this.value);var t=null;if(this.italic>0){t=document.createElement("span");t.style.marginRight=this.italic+"em"}if(this.classes.length>0){t=t||document.createElement("span");t.className=i(this.classes)}for(var r in this.style){if(this.style.hasOwnProperty(r)){t=t||document.createElement("span");t.style[r]=this.style[r]}}if(t){t.appendChild(e);return t}else{return e}};l.prototype.toMarkup=function(){var e=false;var t="<span";if(this.classes.length){e=true;t+=' class="';t+=a.escape(i(this.classes));t+='"'}var r="";if(this.italic>0){r+="margin-right:"+this.italic+"em;"}for(var n in this.style){if(this.style.hasOwnProperty(n)){r+=a.hyphenate(n)+":"+this.style[n]+";"}}if(r){e=true;t+=' style="'+a.escape(r)+'"'}var s=a.escape(this.value);if(e){t+=">";t+=s;t+="</span>";return t}else{return s}};t.exports={span:n,documentFragment:s,symbolNode:l}},{"./utils":23}],15:[function(e,t,r){var a=e("./fontMetrics");var i=e("./parseData");var n=e("./ParseError");var s=i.ParseNode;function l(e,t){var r=[];var a=[r];var i=[];while(true){var l=e.parseExpression(false,null);r.push(new s("ordgroup",l,e.mode));var o=e.nextToken.text;if(o==="&"){e.consume()}else if(o==="\\end"){break}else if(o==="\\\\"||o==="\\cr"){var u=e.parseFunction();i.push(u.value.size);r=[];a.push(r)}else{var p=Math.min(e.pos+1,e.lexer._input.length);throw new n("Expected & or \\\\ or \\end",e.lexer,p)}}t.body=a;t.rowGaps=i;return new s(t.type,t,e.mode)}function o(e,r,a){if(typeof e==="string"){e=[e]}if(typeof r==="number"){r={numArgs:r}}var i={numArgs:r.numArgs||0,argTypes:r.argTypes,greediness:1,allowedInText:!!r.allowedInText,numOptionalArgs:r.numOptionalArgs||0,handler:a};for(var n=0;n<e.length;++n){t.exports[e[n]]=i}}o("array",{numArgs:1},function(e,t){var r=t[0];r=r.value.map?r.value:[r];var a=r.map(function(t){var r=t.value;if("lcr".indexOf(r)!==-1){return{type:"align",align:r}}else if(r==="|"){return{type:"separator",separator:"|"}}throw new n("Unknown column alignment: "+t.value,e.lexer,e.positions[1])});var i={type:"array",cols:a,hskipBeforeAndAfter:true};i=l(e.parser,i);return i});o(["matrix","pmatrix","bmatrix","Bmatrix","vmatrix","Vmatrix"],{},function(e){var t={matrix:null,pmatrix:["(",")"],bmatrix:["[","]"],Bmatrix:["\\{","\\}"],vmatrix:["|","|"],Vmatrix:["\\Vert","\\Vert"]}[e.envName];var r={type:"array",hskipBeforeAndAfter:false};r=l(e.parser,r);if(t){r=new s("leftright",{body:[r],left:t[0],right:t[1]},e.mode)}return r});o("cases",{},function(e){var t={type:"array",arraystretch:1.2,cols:[{type:"align",align:"l",pregap:0,postgap:a.metrics.quad},{type:"align",align:"l",pregap:0,postgap:0}]};t=l(e.parser,t);t=new s("leftright",{body:[t],left:"\\{",right:"."},e.mode);return t});o("aligned",{},function(e){var t={type:"array",cols:[]};t=l(e.parser,t);var r=new s("ordgroup",[],e.mode);var a=0;t.value.body.forEach(function(e){var t;for(t=1;t<e.length;t+=2){e[t].value.unshift(r)}if(a<e.length){a=e.length}});for(var i=0;i<a;++i){var n="r";var o=0;if(i%2===1){n="l"}else if(i>0){o=2}t.value.cols[i]={type:"align",align:n,pregap:o,postgap:0}}return t})},{"./ParseError":5,"./fontMetrics":16,"./parseData":20}],16:[function(e,t,r){var a=e("./Style");var i=.025;var n=0;var s=0;var l=0;var o=.431;var u=1;var p=0;var h=.677;var c=.394;var v=.444;var m=.686;var f=.345;var d=.413;var g=.363;var y=.289;var b=.15;var x=.247;var w=.386;var k=.05;var z=2.39;var S=1.01;var M=.81;var T=.71;var N=.25;var q=0;var A=0;var C=0;var R=0;var E=.431;var P=1;var D=0;var L=.04;var O=.111;var I=.166;var B=.2;var F=.6;var _=.1;var V=10;var G=2/V;var H={xHeight:o,quad:u,num1:h,num2:c,num3:v,denom1:m,denom2:f,sup1:d,sup2:g,sup3:y,sub1:b,sub2:x,supDrop:w,subDrop:k,axisHeight:N,defaultRuleThickness:L,bigOpSpacing1:O,bigOpSpacing2:I,bigOpSpacing3:B,bigOpSpacing4:F,bigOpSpacing5:_,ptPerEm:V,emPerEx:o/u,doubleRuleSep:G,delim1:z,getDelim2:function(e){if(e.size===a.TEXT.size){return S}else if(e.size===a.SCRIPT.size){return M}else if(e.size===a.SCRIPTSCRIPT.size){return T}throw new Error("Unexpected style size: "+e.size)}};var X=e("./fontMetricsData");var U=function(e,t){var r=X[t][e.charCodeAt(0)];if(r){return{depth:r[0],height:r[1],italic:r[2],skew:r[3],width:r[4]}}};t.exports={metrics:H,getCharacterMetrics:U}},{"./Style":8,"./fontMetricsData":17}],17:[function(e,t,r){t.exports={"AMS-Regular":{65:[0,.68889,0,0],66:[0,.68889,0,0],67:[0,.68889,0,0],68:[0,.68889,0,0],69:[0,.68889,0,0],70:[0,.68889,0,0],71:[0,.68889,0,0],72:[0,.68889,0,0],73:[0,.68889,0,0],74:[.16667,.68889,0,0],75:[0,.68889,0,0],76:[0,.68889,0,0],77:[0,.68889,0,0],78:[0,.68889,0,0],79:[.16667,.68889,0,0],80:[0,.68889,0,0],81:[.16667,.68889,0,0],82:[0,.68889,0,0],83:[0,.68889,0,0],84:[0,.68889,0,0],85:[0,.68889,0,0],86:[0,.68889,0,0],87:[0,.68889,0,0],88:[0,.68889,0,0],89:[0,.68889,0,0],90:[0,.68889,0,0],107:[0,.68889,0,0],165:[0,.675,.025,0],174:[.15559,.69224,0,0],240:[0,.68889,0,0],295:[0,.68889,0,0],710:[0,.825,0,0],732:[0,.9,0,0],770:[0,.825,0,0],771:[0,.9,0,0],989:[.08167,.58167,0,0],1008:[0,.43056,.04028,0],8245:[0,.54986,0,0],8463:[0,.68889,0,0],8487:[0,.68889,0,0],8498:[0,.68889,0,0],8502:[0,.68889,0,0],8503:[0,.68889,0,0],8504:[0,.68889,0,0],8513:[0,.68889,0,0],8592:[-.03598,.46402,0,0],8594:[-.03598,.46402,0,0],8602:[-.13313,.36687,0,0],8603:[-.13313,.36687,0,0],8606:[.01354,.52239,0,0],8608:[.01354,.52239,0,0],8610:[.01354,.52239,0,0],8611:[.01354,.52239,0,0],8619:[0,.54986,0,0],8620:[0,.54986,0,0],8621:[-.13313,.37788,0,0],8622:[-.13313,.36687,0,0],8624:[0,.69224,0,0],8625:[0,.69224,0,0],8630:[0,.43056,0,0],8631:[0,.43056,0,0],8634:[.08198,.58198,0,0],8635:[.08198,.58198,0,0],8638:[.19444,.69224,0,0],8639:[.19444,.69224,0,0],8642:[.19444,.69224,0,0],8643:[.19444,.69224,0,0],8644:[.1808,.675,0,0],8646:[.1808,.675,0,0],8647:[.1808,.675,0,0],8648:[.19444,.69224,0,0],8649:[.1808,.675,0,0],8650:[.19444,.69224,0,0],8651:[.01354,.52239,0,0],8652:[.01354,.52239,0,0],8653:[-.13313,.36687,0,0],8654:[-.13313,.36687,0,0],8655:[-.13313,.36687,0,0],8666:[.13667,.63667,0,0],8667:[.13667,.63667,0,0],8669:[-.13313,.37788,0,0],8672:[-.064,.437,0,0],8674:[-.064,.437,0,0],8705:[0,.825,0,0],8708:[0,.68889,0,0],8709:[.08167,.58167,0,0],8717:[0,.43056,0,0],8722:[-.03598,.46402,0,0],8724:[.08198,.69224,0,0],8726:[.08167,.58167,0,0],8733:[0,.69224,0,0],8736:[0,.69224,0,0],8737:[0,.69224,0,0],8738:[.03517,.52239,0,0],8739:[.08167,.58167,0,0],8740:[.25142,.74111,0,0],8741:[.08167,.58167,0,0],8742:[.25142,.74111,0,0],8756:[0,.69224,0,0],8757:[0,.69224,0,0],8764:[-.13313,.36687,0,0],8765:[-.13313,.37788,0,0],8769:[-.13313,.36687,0,0],8770:[-.03625,.46375,0,0],8774:[.30274,.79383,0,0],8776:[-.01688,.48312,0,0],8778:[.08167,.58167,0,0],8782:[.06062,.54986,0,0],8783:[.06062,.54986,0,0],8785:[.08198,.58198,0,0],8786:[.08198,.58198,0,0],8787:[.08198,.58198,0,0],8790:[0,.69224,0,0],8791:[.22958,.72958,0,0],8796:[.08198,.91667,0,0],8806:[.25583,.75583,0,0],8807:[.25583,.75583,0,0],8808:[.25142,.75726,0,0],8809:[.25142,.75726,0,0],8812:[.25583,.75583,0,0],8814:[.20576,.70576,0,0],8815:[.20576,.70576,0,0],8816:[.30274,.79383,0,0],8817:[.30274,.79383,0,0],8818:[.22958,.72958,0,0],8819:[.22958,.72958,0,0],8822:[.1808,.675,0,0],8823:[.1808,.675,0,0],8828:[.13667,.63667,0,0],8829:[.13667,.63667,0,0],8830:[.22958,.72958,0,0],8831:[.22958,.72958,0,0],8832:[.20576,.70576,0,0],8833:[.20576,.70576,0,0],8840:[.30274,.79383,0,0],8841:[.30274,.79383,0,0],8842:[.13597,.63597,0,0],8843:[.13597,.63597,0,0],8847:[.03517,.54986,0,0],8848:[.03517,.54986,0,0],8858:[.08198,.58198,0,0],8859:[.08198,.58198,0,0],8861:[.08198,.58198,0,0],8862:[0,.675,0,0],8863:[0,.675,0,0],8864:[0,.675,0,0],8865:[0,.675,0,0],8872:[0,.69224,0,0],8873:[0,.69224,0,0],8874:[0,.69224,0,0],8876:[0,.68889,0,0],8877:[0,.68889,0,0],8878:[0,.68889,0,0],8879:[0,.68889,0,0],8882:[.03517,.54986,0,0],8883:[.03517,.54986,0,0],8884:[.13667,.63667,0,0],8885:[.13667,.63667,0,0],8888:[0,.54986,0,0],8890:[.19444,.43056,0,0],8891:[.19444,.69224,0,0],8892:[.19444,.69224,0,0],8901:[0,.54986,0,0],8903:[.08167,.58167,0,0],8905:[.08167,.58167,0,0],8906:[.08167,.58167,0,0],8907:[0,.69224,0,0],8908:[0,.69224,0,0],8909:[-.03598,.46402,0,0],8910:[0,.54986,0,0],8911:[0,.54986,0,0],8912:[.03517,.54986,0,0],8913:[.03517,.54986,0,0],8914:[0,.54986,0,0],8915:[0,.54986,0,0],8916:[0,.69224,0,0],8918:[.0391,.5391,0,0],8919:[.0391,.5391,0,0],8920:[.03517,.54986,0,0],8921:[.03517,.54986,0,0],8922:[.38569,.88569,0,0],8923:[.38569,.88569,0,0],8926:[.13667,.63667,0,0],8927:[.13667,.63667,0,0],8928:[.30274,.79383,0,0],8929:[.30274,.79383,0,0],8934:[.23222,.74111,0,0],8935:[.23222,.74111,0,0],8936:[.23222,.74111,0,0],8937:[.23222,.74111,0,0],8938:[.20576,.70576,0,0],8939:[.20576,.70576,0,0],8940:[.30274,.79383,0,0],8941:[.30274,.79383,0,0],8994:[.19444,.69224,0,0],8995:[.19444,.69224,0,0],9416:[.15559,.69224,0,0],9484:[0,.69224,0,0],9488:[0,.69224,0,0],9492:[0,.37788,0,0],9496:[0,.37788,0,0],9585:[.19444,.68889,0,0],9586:[.19444,.74111,0,0],9632:[0,.675,0,0],9633:[0,.675,0,0],9650:[0,.54986,0,0],9651:[0,.54986,0,0],9654:[.03517,.54986,0,0],9660:[0,.54986,0,0],9661:[0,.54986,0,0],9664:[.03517,.54986,0,0],9674:[.11111,.69224,0,0],9733:[.19444,.69224,0,0],10003:[0,.69224,0,0],10016:[0,.69224,0,0],10731:[.11111,.69224,0,0],10846:[.19444,.75583,0,0],10877:[.13667,.63667,0,0],10878:[.13667,.63667,0,0],10885:[.25583,.75583,0,0],10886:[.25583,.75583,0,0],10887:[.13597,.63597,0,0],10888:[.13597,.63597,0,0],10889:[.26167,.75726,0,0],10890:[.26167,.75726,0,0],10891:[.48256,.98256,0,0],10892:[.48256,.98256,0,0],10901:[.13667,.63667,0,0],10902:[.13667,.63667,0,0],10933:[.25142,.75726,0,0],10934:[.25142,.75726,0,0],10935:[.26167,.75726,0,0],10936:[.26167,.75726,0,0],10937:[.26167,.75726,0,0],10938:[.26167,.75726,0,0],10949:[.25583,.75583,0,0],10950:[.25583,.75583,0,0],10955:[.28481,.79383,0,0],10956:[.28481,.79383,0,0],57350:[.08167,.58167,0,0],57351:[.08167,.58167,0,0],57352:[.08167,.58167,0,0],57353:[0,.43056,.04028,0],57356:[.25142,.75726,0,0],57357:[.25142,.75726,0,0],57358:[.41951,.91951,0,0],57359:[.30274,.79383,0,0],57360:[.30274,.79383,0,0],57361:[.41951,.91951,0,0],57366:[.25142,.75726,0,0],57367:[.25142,.75726,0,0],57368:[.25142,.75726,0,0],57369:[.25142,.75726,0,0],57370:[.13597,.63597,0,0],57371:[.13597,.63597,0,0]},"Caligraphic-Regular":{48:[0,.43056,0,0],49:[0,.43056,0,0],50:[0,.43056,0,0],51:[.19444,.43056,0,0],52:[.19444,.43056,0,0],53:[.19444,.43056,0,0],54:[0,.64444,0,0],55:[.19444,.43056,0,0],56:[0,.64444,0,0],57:[.19444,.43056,0,0],65:[0,.68333,0,.19445],66:[0,.68333,.03041,.13889],67:[0,.68333,.05834,.13889],68:[0,.68333,.02778,.08334],69:[0,.68333,.08944,.11111],70:[0,.68333,.09931,.11111],71:[.09722,.68333,.0593,.11111],72:[0,.68333,.00965,.11111],73:[0,.68333,.07382,0],74:[.09722,.68333,.18472,.16667],75:[0,.68333,.01445,.05556],76:[0,.68333,0,.13889],77:[0,.68333,0,.13889],78:[0,.68333,.14736,.08334],79:[0,.68333,.02778,.11111],80:[0,.68333,.08222,.08334],81:[.09722,.68333,0,.11111],82:[0,.68333,0,.08334],83:[0,.68333,.075,.13889],84:[0,.68333,.25417,0],85:[0,.68333,.09931,.08334],86:[0,.68333,.08222,0],87:[0,.68333,.08222,.08334],88:[0,.68333,.14643,.13889],89:[.09722,.68333,.08222,.08334],90:[0,.68333,.07944,.13889]},"Fraktur-Regular":{33:[0,.69141,0,0],34:[0,.69141,0,0],38:[0,.69141,0,0],39:[0,.69141,0,0],40:[.24982,.74947,0,0],41:[.24982,.74947,0,0],42:[0,.62119,0,0],43:[.08319,.58283,0,0],44:[0,.10803,0,0],45:[.08319,.58283,0,0],46:[0,.10803,0,0],47:[.24982,.74947,0,0],48:[0,.47534,0,0],49:[0,.47534,0,0],50:[0,.47534,0,0],51:[.18906,.47534,0,0],52:[.18906,.47534,0,0],53:[.18906,.47534,0,0],54:[0,.69141,0,0],55:[.18906,.47534,0,0],56:[0,.69141,0,0],57:[.18906,.47534,0,0],58:[0,.47534,0,0],59:[.12604,.47534,0,0],61:[-.13099,.36866,0,0],63:[0,.69141,0,0],65:[0,.69141,0,0],66:[0,.69141,0,0],67:[0,.69141,0,0],68:[0,.69141,0,0],69:[0,.69141,0,0],70:[.12604,.69141,0,0],71:[0,.69141,0,0],72:[.06302,.69141,0,0],73:[0,.69141,0,0],74:[.12604,.69141,0,0],75:[0,.69141,0,0],76:[0,.69141,0,0],77:[0,.69141,0,0],78:[0,.69141,0,0],79:[0,.69141,0,0],80:[.18906,.69141,0,0],81:[.03781,.69141,0,0],82:[0,.69141,0,0],83:[0,.69141,0,0],84:[0,.69141,0,0],85:[0,.69141,0,0],86:[0,.69141,0,0],87:[0,.69141,0,0],88:[0,.69141,0,0],89:[.18906,.69141,0,0],90:[.12604,.69141,0,0],91:[.24982,.74947,0,0],93:[.24982,.74947,0,0],94:[0,.69141,0,0],97:[0,.47534,0,0],98:[0,.69141,0,0],99:[0,.47534,0,0],100:[0,.62119,0,0],101:[0,.47534,0,0],102:[.18906,.69141,0,0],103:[.18906,.47534,0,0],104:[.18906,.69141,0,0],105:[0,.69141,0,0],106:[0,.69141,0,0],107:[0,.69141,0,0],108:[0,.69141,0,0],109:[0,.47534,0,0],110:[0,.47534,0,0],111:[0,.47534,0,0],112:[.18906,.52396,0,0],113:[.18906,.47534,0,0],114:[0,.47534,0,0],115:[0,.47534,0,0],116:[0,.62119,0,0],117:[0,.47534,0,0],118:[0,.52396,0,0],119:[0,.52396,0,0],120:[.18906,.47534,0,0],121:[.18906,.47534,0,0],122:[.18906,.47534,0,0],8216:[0,.69141,0,0],8217:[0,.69141,0,0],58112:[0,.62119,0,0],58113:[0,.62119,0,0],58114:[.18906,.69141,0,0],58115:[.18906,.69141,0,0],58116:[.18906,.47534,0,0],58117:[0,.69141,0,0],58118:[0,.62119,0,0],58119:[0,.47534,0,0]},"Main-Bold":{33:[0,.69444,0,0],34:[0,.69444,0,0],35:[.19444,.69444,0,0],36:[.05556,.75,0,0],37:[.05556,.75,0,0],38:[0,.69444,0,0],39:[0,.69444,0,0],40:[.25,.75,0,0],41:[.25,.75,0,0],42:[0,.75,0,0],43:[.13333,.63333,0,0],44:[.19444,.15556,0,0],45:[0,.44444,0,0],46:[0,.15556,0,0],47:[.25,.75,0,0],48:[0,.64444,0,0],49:[0,.64444,0,0],50:[0,.64444,0,0],51:[0,.64444,0,0],52:[0,.64444,0,0],53:[0,.64444,0,0],54:[0,.64444,0,0],55:[0,.64444,0,0],56:[0,.64444,0,0],57:[0,.64444,0,0],58:[0,.44444,0,0],59:[.19444,.44444,0,0],60:[.08556,.58556,0,0],61:[-.10889,.39111,0,0],62:[.08556,.58556,0,0],63:[0,.69444,0,0],64:[0,.69444,0,0],65:[0,.68611,0,0],66:[0,.68611,0,0],67:[0,.68611,0,0],68:[0,.68611,0,0],69:[0,.68611,0,0],70:[0,.68611,0,0],71:[0,.68611,0,0],72:[0,.68611,0,0],73:[0,.68611,0,0],74:[0,.68611,0,0],75:[0,.68611,0,0],76:[0,.68611,0,0],77:[0,.68611,0,0],78:[0,.68611,0,0],79:[0,.68611,0,0],80:[0,.68611,0,0],81:[.19444,.68611,0,0],82:[0,.68611,0,0],83:[0,.68611,0,0],84:[0,.68611,0,0],85:[0,.68611,0,0],86:[0,.68611,.01597,0],87:[0,.68611,.01597,0],88:[0,.68611,0,0],89:[0,.68611,.02875,0],90:[0,.68611,0,0],91:[.25,.75,0,0],92:[.25,.75,0,0],93:[.25,.75,0,0],94:[0,.69444,0,0],95:[.31,.13444,.03194,0],96:[0,.69444,0,0],97:[0,.44444,0,0],98:[0,.69444,0,0],99:[0,.44444,0,0],100:[0,.69444,0,0],101:[0,.44444,0,0],102:[0,.69444,.10903,0],103:[.19444,.44444,.01597,0],104:[0,.69444,0,0],105:[0,.69444,0,0],106:[.19444,.69444,0,0],107:[0,.69444,0,0],108:[0,.69444,0,0],109:[0,.44444,0,0],
  110:[0,.44444,0,0],111:[0,.44444,0,0],112:[.19444,.44444,0,0],113:[.19444,.44444,0,0],114:[0,.44444,0,0],115:[0,.44444,0,0],116:[0,.63492,0,0],117:[0,.44444,0,0],118:[0,.44444,.01597,0],119:[0,.44444,.01597,0],120:[0,.44444,0,0],121:[.19444,.44444,.01597,0],122:[0,.44444,0,0],123:[.25,.75,0,0],124:[.25,.75,0,0],125:[.25,.75,0,0],126:[.35,.34444,0,0],168:[0,.69444,0,0],172:[0,.44444,0,0],175:[0,.59611,0,0],176:[0,.69444,0,0],177:[.13333,.63333,0,0],180:[0,.69444,0,0],215:[.13333,.63333,0,0],247:[.13333,.63333,0,0],305:[0,.44444,0,0],567:[.19444,.44444,0,0],710:[0,.69444,0,0],711:[0,.63194,0,0],713:[0,.59611,0,0],714:[0,.69444,0,0],715:[0,.69444,0,0],728:[0,.69444,0,0],729:[0,.69444,0,0],730:[0,.69444,0,0],732:[0,.69444,0,0],768:[0,.69444,0,0],769:[0,.69444,0,0],770:[0,.69444,0,0],771:[0,.69444,0,0],772:[0,.59611,0,0],774:[0,.69444,0,0],775:[0,.69444,0,0],776:[0,.69444,0,0],778:[0,.69444,0,0],779:[0,.69444,0,0],780:[0,.63194,0,0],824:[.19444,.69444,0,0],915:[0,.68611,0,0],916:[0,.68611,0,0],920:[0,.68611,0,0],923:[0,.68611,0,0],926:[0,.68611,0,0],928:[0,.68611,0,0],931:[0,.68611,0,0],933:[0,.68611,0,0],934:[0,.68611,0,0],936:[0,.68611,0,0],937:[0,.68611,0,0],8211:[0,.44444,.03194,0],8212:[0,.44444,.03194,0],8216:[0,.69444,0,0],8217:[0,.69444,0,0],8220:[0,.69444,0,0],8221:[0,.69444,0,0],8224:[.19444,.69444,0,0],8225:[.19444,.69444,0,0],8242:[0,.55556,0,0],8407:[0,.72444,.15486,0],8463:[0,.69444,0,0],8465:[0,.69444,0,0],8467:[0,.69444,0,0],8472:[.19444,.44444,0,0],8476:[0,.69444,0,0],8501:[0,.69444,0,0],8592:[-.10889,.39111,0,0],8593:[.19444,.69444,0,0],8594:[-.10889,.39111,0,0],8595:[.19444,.69444,0,0],8596:[-.10889,.39111,0,0],8597:[.25,.75,0,0],8598:[.19444,.69444,0,0],8599:[.19444,.69444,0,0],8600:[.19444,.69444,0,0],8601:[.19444,.69444,0,0],8636:[-.10889,.39111,0,0],8637:[-.10889,.39111,0,0],8640:[-.10889,.39111,0,0],8641:[-.10889,.39111,0,0],8656:[-.10889,.39111,0,0],8657:[.19444,.69444,0,0],8658:[-.10889,.39111,0,0],8659:[.19444,.69444,0,0],8660:[-.10889,.39111,0,0],8661:[.25,.75,0,0],8704:[0,.69444,0,0],8706:[0,.69444,.06389,0],8707:[0,.69444,0,0],8709:[.05556,.75,0,0],8711:[0,.68611,0,0],8712:[.08556,.58556,0,0],8715:[.08556,.58556,0,0],8722:[.13333,.63333,0,0],8723:[.13333,.63333,0,0],8725:[.25,.75,0,0],8726:[.25,.75,0,0],8727:[-.02778,.47222,0,0],8728:[-.02639,.47361,0,0],8729:[-.02639,.47361,0,0],8730:[.18,.82,0,0],8733:[0,.44444,0,0],8734:[0,.44444,0,0],8736:[0,.69224,0,0],8739:[.25,.75,0,0],8741:[.25,.75,0,0],8743:[0,.55556,0,0],8744:[0,.55556,0,0],8745:[0,.55556,0,0],8746:[0,.55556,0,0],8747:[.19444,.69444,.12778,0],8764:[-.10889,.39111,0,0],8768:[.19444,.69444,0,0],8771:[.00222,.50222,0,0],8776:[.02444,.52444,0,0],8781:[.00222,.50222,0,0],8801:[.00222,.50222,0,0],8804:[.19667,.69667,0,0],8805:[.19667,.69667,0,0],8810:[.08556,.58556,0,0],8811:[.08556,.58556,0,0],8826:[.08556,.58556,0,0],8827:[.08556,.58556,0,0],8834:[.08556,.58556,0,0],8835:[.08556,.58556,0,0],8838:[.19667,.69667,0,0],8839:[.19667,.69667,0,0],8846:[0,.55556,0,0],8849:[.19667,.69667,0,0],8850:[.19667,.69667,0,0],8851:[0,.55556,0,0],8852:[0,.55556,0,0],8853:[.13333,.63333,0,0],8854:[.13333,.63333,0,0],8855:[.13333,.63333,0,0],8856:[.13333,.63333,0,0],8857:[.13333,.63333,0,0],8866:[0,.69444,0,0],8867:[0,.69444,0,0],8868:[0,.69444,0,0],8869:[0,.69444,0,0],8900:[-.02639,.47361,0,0],8901:[-.02639,.47361,0,0],8902:[-.02778,.47222,0,0],8968:[.25,.75,0,0],8969:[.25,.75,0,0],8970:[.25,.75,0,0],8971:[.25,.75,0,0],8994:[-.13889,.36111,0,0],8995:[-.13889,.36111,0,0],9651:[.19444,.69444,0,0],9657:[-.02778,.47222,0,0],9661:[.19444,.69444,0,0],9667:[-.02778,.47222,0,0],9711:[.19444,.69444,0,0],9824:[.12963,.69444,0,0],9825:[.12963,.69444,0,0],9826:[.12963,.69444,0,0],9827:[.12963,.69444,0,0],9837:[0,.75,0,0],9838:[.19444,.69444,0,0],9839:[.19444,.69444,0,0],10216:[.25,.75,0,0],10217:[.25,.75,0,0],10815:[0,.68611,0,0],10927:[.19667,.69667,0,0],10928:[.19667,.69667,0,0]},"Main-Italic":{33:[0,.69444,.12417,0],34:[0,.69444,.06961,0],35:[.19444,.69444,.06616,0],37:[.05556,.75,.13639,0],38:[0,.69444,.09694,0],39:[0,.69444,.12417,0],40:[.25,.75,.16194,0],41:[.25,.75,.03694,0],42:[0,.75,.14917,0],43:[.05667,.56167,.03694,0],44:[.19444,.10556,0,0],45:[0,.43056,.02826,0],46:[0,.10556,0,0],47:[.25,.75,.16194,0],48:[0,.64444,.13556,0],49:[0,.64444,.13556,0],50:[0,.64444,.13556,0],51:[0,.64444,.13556,0],52:[.19444,.64444,.13556,0],53:[0,.64444,.13556,0],54:[0,.64444,.13556,0],55:[.19444,.64444,.13556,0],56:[0,.64444,.13556,0],57:[0,.64444,.13556,0],58:[0,.43056,.0582,0],59:[.19444,.43056,.0582,0],61:[-.13313,.36687,.06616,0],63:[0,.69444,.1225,0],64:[0,.69444,.09597,0],65:[0,.68333,0,0],66:[0,.68333,.10257,0],67:[0,.68333,.14528,0],68:[0,.68333,.09403,0],69:[0,.68333,.12028,0],70:[0,.68333,.13305,0],71:[0,.68333,.08722,0],72:[0,.68333,.16389,0],73:[0,.68333,.15806,0],74:[0,.68333,.14028,0],75:[0,.68333,.14528,0],76:[0,.68333,0,0],77:[0,.68333,.16389,0],78:[0,.68333,.16389,0],79:[0,.68333,.09403,0],80:[0,.68333,.10257,0],81:[.19444,.68333,.09403,0],82:[0,.68333,.03868,0],83:[0,.68333,.11972,0],84:[0,.68333,.13305,0],85:[0,.68333,.16389,0],86:[0,.68333,.18361,0],87:[0,.68333,.18361,0],88:[0,.68333,.15806,0],89:[0,.68333,.19383,0],90:[0,.68333,.14528,0],91:[.25,.75,.1875,0],93:[.25,.75,.10528,0],94:[0,.69444,.06646,0],95:[.31,.12056,.09208,0],97:[0,.43056,.07671,0],98:[0,.69444,.06312,0],99:[0,.43056,.05653,0],100:[0,.69444,.10333,0],101:[0,.43056,.07514,0],102:[.19444,.69444,.21194,0],103:[.19444,.43056,.08847,0],104:[0,.69444,.07671,0],105:[0,.65536,.1019,0],106:[.19444,.65536,.14467,0],107:[0,.69444,.10764,0],108:[0,.69444,.10333,0],109:[0,.43056,.07671,0],110:[0,.43056,.07671,0],111:[0,.43056,.06312,0],112:[.19444,.43056,.06312,0],113:[.19444,.43056,.08847,0],114:[0,.43056,.10764,0],115:[0,.43056,.08208,0],116:[0,.61508,.09486,0],117:[0,.43056,.07671,0],118:[0,.43056,.10764,0],119:[0,.43056,.10764,0],120:[0,.43056,.12042,0],121:[.19444,.43056,.08847,0],122:[0,.43056,.12292,0],126:[.35,.31786,.11585,0],163:[0,.69444,0,0],305:[0,.43056,0,.02778],567:[.19444,.43056,0,.08334],768:[0,.69444,0,0],769:[0,.69444,.09694,0],770:[0,.69444,.06646,0],771:[0,.66786,.11585,0],772:[0,.56167,.10333,0],774:[0,.69444,.10806,0],775:[0,.66786,.11752,0],776:[0,.66786,.10474,0],778:[0,.69444,0,0],779:[0,.69444,.1225,0],780:[0,.62847,.08295,0],915:[0,.68333,.13305,0],916:[0,.68333,0,0],920:[0,.68333,.09403,0],923:[0,.68333,0,0],926:[0,.68333,.15294,0],928:[0,.68333,.16389,0],931:[0,.68333,.12028,0],933:[0,.68333,.11111,0],934:[0,.68333,.05986,0],936:[0,.68333,.11111,0],937:[0,.68333,.10257,0],8211:[0,.43056,.09208,0],8212:[0,.43056,.09208,0],8216:[0,.69444,.12417,0],8217:[0,.69444,.12417,0],8220:[0,.69444,.1685,0],8221:[0,.69444,.06961,0],8463:[0,.68889,0,0]},"Main-Regular":{32:[0,0,0,0],33:[0,.69444,0,0],34:[0,.69444,0,0],35:[.19444,.69444,0,0],36:[.05556,.75,0,0],37:[.05556,.75,0,0],38:[0,.69444,0,0],39:[0,.69444,0,0],40:[.25,.75,0,0],41:[.25,.75,0,0],42:[0,.75,0,0],43:[.08333,.58333,0,0],44:[.19444,.10556,0,0],45:[0,.43056,0,0],46:[0,.10556,0,0],47:[.25,.75,0,0],48:[0,.64444,0,0],49:[0,.64444,0,0],50:[0,.64444,0,0],51:[0,.64444,0,0],52:[0,.64444,0,0],53:[0,.64444,0,0],54:[0,.64444,0,0],55:[0,.64444,0,0],56:[0,.64444,0,0],57:[0,.64444,0,0],58:[0,.43056,0,0],59:[.19444,.43056,0,0],60:[.0391,.5391,0,0],61:[-.13313,.36687,0,0],62:[.0391,.5391,0,0],63:[0,.69444,0,0],64:[0,.69444,0,0],65:[0,.68333,0,0],66:[0,.68333,0,0],67:[0,.68333,0,0],68:[0,.68333,0,0],69:[0,.68333,0,0],70:[0,.68333,0,0],71:[0,.68333,0,0],72:[0,.68333,0,0],73:[0,.68333,0,0],74:[0,.68333,0,0],75:[0,.68333,0,0],76:[0,.68333,0,0],77:[0,.68333,0,0],78:[0,.68333,0,0],79:[0,.68333,0,0],80:[0,.68333,0,0],81:[.19444,.68333,0,0],82:[0,.68333,0,0],83:[0,.68333,0,0],84:[0,.68333,0,0],85:[0,.68333,0,0],86:[0,.68333,.01389,0],87:[0,.68333,.01389,0],88:[0,.68333,0,0],89:[0,.68333,.025,0],90:[0,.68333,0,0],91:[.25,.75,0,0],92:[.25,.75,0,0],93:[.25,.75,0,0],94:[0,.69444,0,0],95:[.31,.12056,.02778,0],96:[0,.69444,0,0],97:[0,.43056,0,0],98:[0,.69444,0,0],99:[0,.43056,0,0],100:[0,.69444,0,0],101:[0,.43056,0,0],102:[0,.69444,.07778,0],103:[.19444,.43056,.01389,0],104:[0,.69444,0,0],105:[0,.66786,0,0],106:[.19444,.66786,0,0],107:[0,.69444,0,0],108:[0,.69444,0,0],109:[0,.43056,0,0],110:[0,.43056,0,0],111:[0,.43056,0,0],112:[.19444,.43056,0,0],113:[.19444,.43056,0,0],114:[0,.43056,0,0],115:[0,.43056,0,0],116:[0,.61508,0,0],117:[0,.43056,0,0],118:[0,.43056,.01389,0],119:[0,.43056,.01389,0],120:[0,.43056,0,0],121:[.19444,.43056,.01389,0],122:[0,.43056,0,0],123:[.25,.75,0,0],124:[.25,.75,0,0],125:[.25,.75,0,0],126:[.35,.31786,0,0],160:[0,0,0,0],168:[0,.66786,0,0],172:[0,.43056,0,0],175:[0,.56778,0,0],176:[0,.69444,0,0],177:[.08333,.58333,0,0],180:[0,.69444,0,0],215:[.08333,.58333,0,0],247:[.08333,.58333,0,0],305:[0,.43056,0,0],567:[.19444,.43056,0,0],710:[0,.69444,0,0],711:[0,.62847,0,0],713:[0,.56778,0,0],714:[0,.69444,0,0],715:[0,.69444,0,0],728:[0,.69444,0,0],729:[0,.66786,0,0],730:[0,.69444,0,0],732:[0,.66786,0,0],768:[0,.69444,0,0],769:[0,.69444,0,0],770:[0,.69444,0,0],771:[0,.66786,0,0],772:[0,.56778,0,0],774:[0,.69444,0,0],775:[0,.66786,0,0],776:[0,.66786,0,0],778:[0,.69444,0,0],779:[0,.69444,0,0],780:[0,.62847,0,0],824:[.19444,.69444,0,0],915:[0,.68333,0,0],916:[0,.68333,0,0],920:[0,.68333,0,0],923:[0,.68333,0,0],926:[0,.68333,0,0],928:[0,.68333,0,0],931:[0,.68333,0,0],933:[0,.68333,0,0],934:[0,.68333,0,0],936:[0,.68333,0,0],937:[0,.68333,0,0],8211:[0,.43056,.02778,0],8212:[0,.43056,.02778,0],8216:[0,.69444,0,0],8217:[0,.69444,0,0],8220:[0,.69444,0,0],8221:[0,.69444,0,0],8224:[.19444,.69444,0,0],8225:[.19444,.69444,0,0],8230:[0,.12,0,0],8242:[0,.55556,0,0],8407:[0,.71444,.15382,0],8463:[0,.68889,0,0],8465:[0,.69444,0,0],8467:[0,.69444,0,.11111],8472:[.19444,.43056,0,.11111],8476:[0,.69444,0,0],8501:[0,.69444,0,0],8592:[-.13313,.36687,0,0],8593:[.19444,.69444,0,0],8594:[-.13313,.36687,0,0],8595:[.19444,.69444,0,0],8596:[-.13313,.36687,0,0],8597:[.25,.75,0,0],8598:[.19444,.69444,0,0],8599:[.19444,.69444,0,0],8600:[.19444,.69444,0,0],8601:[.19444,.69444,0,0],8614:[.011,.511,0,0],8617:[.011,.511,0,0],8618:[.011,.511,0,0],8636:[-.13313,.36687,0,0],8637:[-.13313,.36687,0,0],8640:[-.13313,.36687,0,0],8641:[-.13313,.36687,0,0],8652:[.011,.671,0,0],8656:[-.13313,.36687,0,0],8657:[.19444,.69444,0,0],8658:[-.13313,.36687,0,0],8659:[.19444,.69444,0,0],8660:[-.13313,.36687,0,0],8661:[.25,.75,0,0],8704:[0,.69444,0,0],8706:[0,.69444,.05556,.08334],8707:[0,.69444,0,0],8709:[.05556,.75,0,0],8711:[0,.68333,0,0],8712:[.0391,.5391,0,0],8715:[.0391,.5391,0,0],8722:[.08333,.58333,0,0],8723:[.08333,.58333,0,0],8725:[.25,.75,0,0],8726:[.25,.75,0,0],8727:[-.03472,.46528,0,0],8728:[-.05555,.44445,0,0],8729:[-.05555,.44445,0,0],8730:[.2,.8,0,0],8733:[0,.43056,0,0],8734:[0,.43056,0,0],8736:[0,.69224,0,0],8739:[.25,.75,0,0],8741:[.25,.75,0,0],8743:[0,.55556,0,0],8744:[0,.55556,0,0],8745:[0,.55556,0,0],8746:[0,.55556,0,0],8747:[.19444,.69444,.11111,0],8764:[-.13313,.36687,0,0],8768:[.19444,.69444,0,0],8771:[-.03625,.46375,0,0],8773:[-.022,.589,0,0],8776:[-.01688,.48312,0,0],8781:[-.03625,.46375,0,0],8784:[-.133,.67,0,0],8800:[.215,.716,0,0],8801:[-.03625,.46375,0,0],8804:[.13597,.63597,0,0],8805:[.13597,.63597,0,0],8810:[.0391,.5391,0,0],8811:[.0391,.5391,0,0],8826:[.0391,.5391,0,0],8827:[.0391,.5391,0,0],8834:[.0391,.5391,0,0],8835:[.0391,.5391,0,0],8838:[.13597,.63597,0,0],8839:[.13597,.63597,0,0],8846:[0,.55556,0,0],8849:[.13597,.63597,0,0],8850:[.13597,.63597,0,0],8851:[0,.55556,0,0],8852:[0,.55556,0,0],8853:[.08333,.58333,0,0],8854:[.08333,.58333,0,0],8855:[.08333,.58333,0,0],8856:[.08333,.58333,0,0],8857:[.08333,.58333,0,0],8866:[0,.69444,0,0],8867:[0,.69444,0,0],8868:[0,.69444,0,0],8869:[0,.69444,0,0],8872:[.249,.75,0,0],8900:[-.05555,.44445,0,0],8901:[-.05555,.44445,0,0],8902:[-.03472,.46528,0,0],8904:[.005,.505,0,0],8942:[.03,.9,0,0],8943:[-.19,.31,0,0],8945:[-.1,.82,0,0],8968:[.25,.75,0,0],8969:[.25,.75,0,0],8970:[.25,.75,0,0],8971:[.25,.75,0,0],8994:[-.14236,.35764,0,0],8995:[-.14236,.35764,0,0],9136:[.244,.744,0,0],9137:[.244,.744,0,0],9651:[.19444,.69444,0,0],9657:[-.03472,.46528,0,0],9661:[.19444,.69444,0,0],9667:[-.03472,.46528,0,0],9711:[.19444,.69444,0,0],9824:[.12963,.69444,0,0],9825:[.12963,.69444,0,0],9826:[.12963,.69444,0,0],9827:[.12963,.69444,0,0],9837:[0,.75,0,0],9838:[.19444,.69444,0,0],9839:[.19444,.69444,0,0],10216:[.25,.75,0,0],10217:[.25,.75,0,0],10222:[.244,.744,0,0],10223:[.244,.744,0,0],10229:[.011,.511,0,0],10230:[.011,.511,0,0],10231:[.011,.511,0,0],10232:[.024,.525,0,0],10233:[.024,.525,0,0],10234:[.024,.525,0,0],10236:[.011,.511,0,0],10815:[0,.68333,0,0],10927:[.13597,.63597,0,0],10928:[.13597,.63597,0,0]},"Math-BoldItalic":{47:[.19444,.69444,0,0],65:[0,.68611,0,0],66:[0,.68611,.04835,0],67:[0,.68611,.06979,0],68:[0,.68611,.03194,0],69:[0,.68611,.05451,0],70:[0,.68611,.15972,0],71:[0,.68611,0,0],72:[0,.68611,.08229,0],73:[0,.68611,.07778,0],74:[0,.68611,.10069,0],75:[0,.68611,.06979,0],76:[0,.68611,0,0],77:[0,.68611,.11424,0],78:[0,.68611,.11424,0],79:[0,.68611,.03194,0],80:[0,.68611,.15972,0],81:[.19444,.68611,0,0],82:[0,.68611,.00421,0],83:[0,.68611,.05382,0],84:[0,.68611,.15972,0],85:[0,.68611,.11424,0],86:[0,.68611,.25555,0],87:[0,.68611,.15972,0],88:[0,.68611,.07778,0],89:[0,.68611,.25555,0],90:[0,.68611,.06979,0],97:[0,.44444,0,0],98:[0,.69444,0,0],99:[0,.44444,0,0],100:[0,.69444,0,0],101:[0,.44444,0,0],102:[.19444,.69444,.11042,0],103:[.19444,.44444,.03704,0],104:[0,.69444,0,0],105:[0,.69326,0,0],106:[.19444,.69326,.0622,0],107:[0,.69444,.01852,0],108:[0,.69444,.0088,0],109:[0,.44444,0,0],110:[0,.44444,0,0],111:[0,.44444,0,0],112:[.19444,.44444,0,0],113:[.19444,.44444,.03704,0],114:[0,.44444,.03194,0],115:[0,.44444,0,0],116:[0,.63492,0,0],117:[0,.44444,0,0],118:[0,.44444,.03704,0],119:[0,.44444,.02778,0],120:[0,.44444,0,0],121:[.19444,.44444,.03704,0],122:[0,.44444,.04213,0],915:[0,.68611,.15972,0],916:[0,.68611,0,0],920:[0,.68611,.03194,0],923:[0,.68611,0,0],926:[0,.68611,.07458,0],928:[0,.68611,.08229,0],931:[0,.68611,.05451,0],933:[0,.68611,.15972,0],934:[0,.68611,0,0],936:[0,.68611,.11653,0],937:[0,.68611,.04835,0],945:[0,.44444,0,0],946:[.19444,.69444,.03403,0],947:[.19444,.44444,.06389,0],948:[0,.69444,.03819,0],949:[0,.44444,0,0],950:[.19444,.69444,.06215,0],951:[.19444,.44444,.03704,0],952:[0,.69444,.03194,0],953:[0,.44444,0,0],954:[0,.44444,0,0],955:[0,.69444,0,0],956:[.19444,.44444,0,0],957:[0,.44444,.06898,0],958:[.19444,.69444,.03021,0],959:[0,.44444,0,0],960:[0,.44444,.03704,0],961:[.19444,.44444,0,0],962:[.09722,.44444,.07917,0],963:[0,.44444,.03704,0],964:[0,.44444,.13472,0],965:[0,.44444,.03704,0],966:[.19444,.44444,0,0],967:[.19444,.44444,0,0],968:[.19444,.69444,.03704,0],969:[0,.44444,.03704,0],977:[0,.69444,0,0],981:[.19444,.69444,0,0],982:[0,.44444,.03194,0],1009:[.19444,.44444,0,0],1013:[0,.44444,0,0]},"Math-Italic":{47:[.19444,.69444,0,0],65:[0,.68333,0,.13889],66:[0,.68333,.05017,.08334],67:[0,.68333,.07153,.08334],68:[0,.68333,.02778,.05556],69:[0,.68333,.05764,.08334],70:[0,.68333,.13889,.08334],71:[0,.68333,0,.08334],72:[0,.68333,.08125,.05556],73:[0,.68333,.07847,.11111],74:[0,.68333,.09618,.16667],75:[0,.68333,.07153,.05556],76:[0,.68333,0,.02778],77:[0,.68333,.10903,.08334],78:[0,.68333,.10903,.08334],79:[0,.68333,.02778,.08334],80:[0,.68333,.13889,.08334],81:[.19444,.68333,0,.08334],82:[0,.68333,.00773,.08334],83:[0,.68333,.05764,.08334],84:[0,.68333,.13889,.08334],85:[0,.68333,.10903,.02778],86:[0,.68333,.22222,0],87:[0,.68333,.13889,0],88:[0,.68333,.07847,.08334],89:[0,.68333,.22222,0],90:[0,.68333,.07153,.08334],97:[0,.43056,0,0],98:[0,.69444,0,0],99:[0,.43056,0,.05556],100:[0,.69444,0,.16667],101:[0,.43056,0,.05556],102:[.19444,.69444,.10764,.16667],103:[.19444,.43056,.03588,.02778],104:[0,.69444,0,0],105:[0,.65952,0,0],106:[.19444,.65952,.05724,0],107:[0,.69444,.03148,0],108:[0,.69444,.01968,.08334],109:[0,.43056,0,0],110:[0,.43056,0,0],111:[0,.43056,0,.05556],112:[.19444,.43056,0,.08334],113:[.19444,.43056,.03588,.08334],114:[0,.43056,.02778,.05556],115:[0,.43056,0,.05556],116:[0,.61508,0,.08334],117:[0,.43056,0,.02778],118:[0,.43056,.03588,.02778],119:[0,.43056,.02691,.08334],120:[0,.43056,0,.02778],121:[.19444,.43056,.03588,.05556],122:[0,.43056,.04398,.05556],915:[0,.68333,.13889,.08334],916:[0,.68333,0,.16667],920:[0,.68333,.02778,.08334],923:[0,.68333,0,.16667],926:[0,.68333,.07569,.08334],928:[0,.68333,.08125,.05556],931:[0,.68333,.05764,.08334],933:[0,.68333,.13889,.05556],934:[0,.68333,0,.08334],936:[0,.68333,.11,.05556],937:[0,.68333,.05017,.08334],945:[0,.43056,.0037,.02778],946:[.19444,.69444,.05278,.08334],947:[.19444,.43056,.05556,0],948:[0,.69444,.03785,.05556],949:[0,.43056,0,.08334],950:[.19444,.69444,.07378,.08334],951:[.19444,.43056,.03588,.05556],952:[0,.69444,.02778,.08334],953:[0,.43056,0,.05556],954:[0,.43056,0,0],955:[0,.69444,0,0],956:[.19444,.43056,0,.02778],957:[0,.43056,.06366,.02778],958:[.19444,.69444,.04601,.11111],959:[0,.43056,0,.05556],960:[0,.43056,.03588,0],961:[.19444,.43056,0,.08334],962:[.09722,.43056,.07986,.08334],963:[0,.43056,.03588,0],964:[0,.43056,.1132,.02778],965:[0,.43056,.03588,.02778],966:[.19444,.43056,0,.08334],967:[.19444,.43056,0,.05556],968:[.19444,.69444,.03588,.11111],969:[0,.43056,.03588,0],977:[0,.69444,0,.08334],981:[.19444,.69444,0,.08334],982:[0,.43056,.02778,0],1009:[.19444,.43056,0,.08334],1013:[0,.43056,0,.05556]},"Math-Regular":{65:[0,.68333,0,.13889],66:[0,.68333,.05017,.08334],67:[0,.68333,.07153,.08334],68:[0,.68333,.02778,.05556],69:[0,.68333,.05764,.08334],70:[0,.68333,.13889,.08334],71:[0,.68333,0,.08334],72:[0,.68333,.08125,.05556],73:[0,.68333,.07847,.11111],74:[0,.68333,.09618,.16667],75:[0,.68333,.07153,.05556],76:[0,.68333,0,.02778],77:[0,.68333,.10903,.08334],78:[0,.68333,.10903,.08334],79:[0,.68333,.02778,.08334],80:[0,.68333,.13889,.08334],81:[.19444,.68333,0,.08334],82:[0,.68333,.00773,.08334],83:[0,.68333,.05764,.08334],84:[0,.68333,.13889,.08334],85:[0,.68333,.10903,.02778],86:[0,.68333,.22222,0],87:[0,.68333,.13889,0],88:[0,.68333,.07847,.08334],89:[0,.68333,.22222,0],90:[0,.68333,.07153,.08334],97:[0,.43056,0,0],98:[0,.69444,0,0],99:[0,.43056,0,.05556],100:[0,.69444,0,.16667],101:[0,.43056,0,.05556],102:[.19444,.69444,.10764,.16667],103:[.19444,.43056,.03588,.02778],104:[0,.69444,0,0],105:[0,.65952,0,0],106:[.19444,.65952,.05724,0],107:[0,.69444,.03148,0],108:[0,.69444,.01968,.08334],109:[0,.43056,0,0],110:[0,.43056,0,0],111:[0,.43056,0,.05556],112:[.19444,.43056,0,.08334],113:[.19444,.43056,.03588,.08334],114:[0,.43056,.02778,.05556],115:[0,.43056,0,.05556],116:[0,.61508,0,.08334],117:[0,.43056,0,.02778],118:[0,.43056,.03588,.02778],119:[0,.43056,.02691,.08334],120:[0,.43056,0,.02778],121:[.19444,.43056,.03588,.05556],122:[0,.43056,.04398,.05556],915:[0,.68333,.13889,.08334],916:[0,.68333,0,.16667],920:[0,.68333,.02778,.08334],923:[0,.68333,0,.16667],926:[0,.68333,.07569,.08334],928:[0,.68333,.08125,.05556],931:[0,.68333,.05764,.08334],933:[0,.68333,.13889,.05556],934:[0,.68333,0,.08334],936:[0,.68333,.11,.05556],937:[0,.68333,.05017,.08334],945:[0,.43056,.0037,.02778],946:[.19444,.69444,.05278,.08334],947:[.19444,.43056,.05556,0],948:[0,.69444,.03785,.05556],949:[0,.43056,0,.08334],950:[.19444,.69444,.07378,.08334],951:[.19444,.43056,.03588,.05556],952:[0,.69444,.02778,.08334],953:[0,.43056,0,.05556],954:[0,.43056,0,0],955:[0,.69444,0,0],956:[.19444,.43056,0,.02778],957:[0,.43056,.06366,.02778],958:[.19444,.69444,.04601,.11111],959:[0,.43056,0,.05556],960:[0,.43056,.03588,0],961:[.19444,.43056,0,.08334],962:[.09722,.43056,.07986,.08334],963:[0,.43056,.03588,0],964:[0,.43056,.1132,.02778],965:[0,.43056,.03588,.02778],966:[.19444,.43056,0,.08334],967:[.19444,.43056,0,.05556],968:[.19444,.69444,.03588,.11111],969:[0,.43056,.03588,0],977:[0,.69444,0,.08334],981:[.19444,.69444,0,.08334],982:[0,.43056,.02778,0],1009:[.19444,.43056,0,.08334],1013:[0,.43056,0,.05556]},"SansSerif-Regular":{33:[0,.69444,0,0],34:[0,.69444,0,0],35:[.19444,.69444,0,0],36:[.05556,.75,0,0],37:[.05556,.75,0,0],38:[0,.69444,0,0],39:[0,.69444,0,0],40:[.25,.75,0,0],41:[.25,.75,0,0],42:[0,.75,0,0],43:[.08333,.58333,0,0],44:[.125,.08333,0,0],45:[0,.44444,0,0],46:[0,.08333,0,0],47:[.25,.75,0,0],48:[0,.65556,0,0],49:[0,.65556,0,0],50:[0,.65556,0,0],51:[0,.65556,0,0],52:[0,.65556,0,0],53:[0,.65556,0,0],54:[0,.65556,0,0],55:[0,.65556,0,0],56:[0,.65556,0,0],57:[0,.65556,0,0],58:[0,.44444,0,0],59:[.125,.44444,0,0],61:[-.13,.37,0,0],63:[0,.69444,0,0],64:[0,.69444,0,0],65:[0,.69444,0,0],66:[0,.69444,0,0],67:[0,.69444,0,0],68:[0,.69444,0,0],69:[0,.69444,0,0],70:[0,.69444,0,0],71:[0,.69444,0,0],72:[0,.69444,0,0],73:[0,.69444,0,0],74:[0,.69444,0,0],75:[0,.69444,0,0],76:[0,.69444,0,0],77:[0,.69444,0,0],78:[0,.69444,0,0],79:[0,.69444,0,0],80:[0,.69444,0,0],81:[.125,.69444,0,0],82:[0,.69444,0,0],83:[0,.69444,0,0],84:[0,.69444,0,0],85:[0,.69444,0,0],86:[0,.69444,.01389,0],87:[0,.69444,.01389,0],88:[0,.69444,0,0],89:[0,.69444,.025,0],90:[0,.69444,0,0],91:[.25,.75,0,0],93:[.25,.75,0,0],94:[0,.69444,0,0],95:[.35,.09444,.02778,0],97:[0,.44444,0,0],98:[0,.69444,0,0],99:[0,.44444,0,0],100:[0,.69444,0,0],101:[0,.44444,0,0],102:[0,.69444,.06944,0],103:[.19444,.44444,.01389,0],104:[0,.69444,0,0],105:[0,.67937,0,0],106:[.19444,.67937,0,0],107:[0,.69444,0,0],108:[0,.69444,0,0],109:[0,.44444,0,0],110:[0,.44444,0,0],111:[0,.44444,0,0],112:[.19444,.44444,0,0],113:[.19444,.44444,0,0],114:[0,.44444,.01389,0],115:[0,.44444,0,0],116:[0,.57143,0,0],117:[0,.44444,0,0],118:[0,.44444,.01389,0],119:[0,.44444,.01389,0],120:[0,.44444,0,0],121:[.19444,.44444,.01389,0],122:[0,.44444,0,0],126:[.35,.32659,0,0],305:[0,.44444,0,0],567:[.19444,.44444,0,0],768:[0,.69444,0,0],769:[0,.69444,0,0],770:[0,.69444,0,0],771:[0,.67659,0,0],772:[0,.60889,0,0],774:[0,.69444,0,0],775:[0,.67937,0,0],776:[0,.67937,0,0],778:[0,.69444,0,0],779:[0,.69444,0,0],780:[0,.63194,0,0],915:[0,.69444,0,0],916:[0,.69444,0,0],920:[0,.69444,0,0],923:[0,.69444,0,0],926:[0,.69444,0,0],928:[0,.69444,0,0],931:[0,.69444,0,0],933:[0,.69444,0,0],934:[0,.69444,0,0],936:[0,.69444,0,0],937:[0,.69444,0,0],8211:[0,.44444,.02778,0],8212:[0,.44444,.02778,0],8216:[0,.69444,0,0],8217:[0,.69444,0,0],8220:[0,.69444,0,0],8221:[0,.69444,0,0]},"Script-Regular":{65:[0,.7,.22925,0],66:[0,.7,.04087,0],67:[0,.7,.1689,0],68:[0,.7,.09371,0],69:[0,.7,.18583,0],70:[0,.7,.13634,0],71:[0,.7,.17322,0],72:[0,.7,.29694,0],73:[0,.7,.19189,0],74:[.27778,.7,.19189,0],75:[0,.7,.31259,0],76:[0,.7,.19189,0],77:[0,.7,.15981,0],78:[0,.7,.3525,0],79:[0,.7,.08078,0],80:[0,.7,.08078,0],81:[0,.7,.03305,0],82:[0,.7,.06259,0],83:[0,.7,.19189,0],84:[0,.7,.29087,0],85:[0,.7,.25815,0],86:[0,.7,.27523,0],87:[0,.7,.27523,0],88:[0,.7,.26006,0],89:[0,.7,.2939,0],90:[0,.7,.24037,0]},"Size1-Regular":{40:[.35001,.85,0,0],41:[.35001,.85,0,0],47:[.35001,.85,0,0],91:[.35001,.85,0,0],92:[.35001,.85,0,0],93:[.35001,.85,0,0],123:[.35001,.85,0,0],125:[.35001,.85,0,0],710:[0,.72222,0,0],732:[0,.72222,0,0],770:[0,.72222,0,0],771:[0,.72222,0,0],8214:[-99e-5,.601,0,0],8593:[1e-5,.6,0,0],8595:[1e-5,.6,0,0],8657:[1e-5,.6,0,0],8659:[1e-5,.6,0,0],8719:[.25001,.75,0,0],8720:[.25001,.75,0,0],8721:[.25001,.75,0,0],8730:[.35001,.85,0,0],8739:[-.00599,.606,0,0],8741:[-.00599,.606,0,0],8747:[.30612,.805,.19445,0],8748:[.306,.805,.19445,0],8749:[.306,.805,.19445,0],8750:[.30612,.805,.19445,0],8896:[.25001,.75,0,0],8897:[.25001,.75,0,0],8898:[.25001,.75,0,0],8899:[.25001,.75,0,0],8968:[.35001,.85,0,0],8969:[.35001,.85,0,0],8970:[.35001,.85,0,0],8971:[.35001,.85,0,0],9168:[-99e-5,.601,0,0],10216:[.35001,.85,0,0],10217:[.35001,.85,0,0],10752:[.25001,.75,0,0],10753:[.25001,.75,0,0],10754:[.25001,.75,0,0],10756:[.25001,.75,0,0],10758:[.25001,.75,0,0]},"Size2-Regular":{40:[.65002,1.15,0,0],41:[.65002,1.15,0,0],47:[.65002,1.15,0,0],91:[.65002,1.15,0,0],92:[.65002,1.15,0,0],93:[.65002,1.15,0,0],123:[.65002,1.15,0,0],125:[.65002,1.15,0,0],710:[0,.75,0,0],732:[0,.75,0,0],770:[0,.75,0,0],771:[0,.75,0,0],8719:[.55001,1.05,0,0],8720:[.55001,1.05,0,0],8721:[.55001,1.05,0,0],8730:[.65002,1.15,0,0],8747:[.86225,1.36,.44445,0],8748:[.862,1.36,.44445,0],8749:[.862,1.36,.44445,0],8750:[.86225,1.36,.44445,0],8896:[.55001,1.05,0,0],8897:[.55001,1.05,0,0],8898:[.55001,1.05,0,0],8899:[.55001,1.05,0,0],8968:[.65002,1.15,0,0],8969:[.65002,1.15,0,0],8970:[.65002,1.15,0,0],8971:[.65002,1.15,0,0],10216:[.65002,1.15,0,0],10217:[.65002,1.15,0,0],10752:[.55001,1.05,0,0],10753:[.55001,1.05,0,0],10754:[.55001,1.05,0,0],10756:[.55001,1.05,0,0],10758:[.55001,1.05,0,0]},"Size3-Regular":{40:[.95003,1.45,0,0],41:[.95003,1.45,0,0],47:[.95003,1.45,0,0],91:[.95003,1.45,0,0],92:[.95003,1.45,0,0],93:[.95003,1.45,0,0],123:[.95003,1.45,0,0],125:[.95003,1.45,0,0],710:[0,.75,0,0],732:[0,.75,0,0],770:[0,.75,0,0],771:[0,.75,0,0],8730:[.95003,1.45,0,0],8968:[.95003,1.45,0,0],8969:[.95003,1.45,0,0],8970:[.95003,1.45,0,0],8971:[.95003,1.45,0,0],10216:[.95003,1.45,0,0],10217:[.95003,1.45,0,0]},"Size4-Regular":{40:[1.25003,1.75,0,0],41:[1.25003,1.75,0,0],47:[1.25003,1.75,0,0],91:[1.25003,1.75,0,0],92:[1.25003,1.75,0,0],93:[1.25003,1.75,0,0],123:[1.25003,1.75,0,0],125:[1.25003,1.75,0,0],710:[0,.825,0,0],732:[0,.825,0,0],770:[0,.825,0,0],771:[0,.825,0,0],8730:[1.25003,1.75,0,0],8968:[1.25003,1.75,0,0],8969:[1.25003,1.75,0,0],8970:[1.25003,1.75,0,0],8971:[1.25003,1.75,0,0],9115:[.64502,1.155,0,0],9116:[1e-5,.6,0,0],9117:[.64502,1.155,0,0],9118:[.64502,1.155,0,0],9119:[1e-5,.6,0,0],9120:[.64502,1.155,0,0],9121:[.64502,1.155,0,0],9122:[-99e-5,.601,0,0],9123:[.64502,1.155,0,0],9124:[.64502,1.155,0,0],9125:[-99e-5,.601,0,0],9126:[.64502,1.155,0,0],9127:[1e-5,.9,0,0],9128:[.65002,1.15,0,0],9129:[.90001,0,0,0],9130:[0,.3,0,0],9131:[1e-5,.9,0,0],9132:[.65002,1.15,0,0],9133:[.90001,0,0,0],9143:[.88502,.915,0,0],10216:[1.25003,1.75,0,0],10217:[1.25003,1.75,0,0],57344:[-.00499,.605,0,0],57345:[-.00499,.605,0,0],57680:[0,.12,0,0],57681:[0,.12,0,0],57682:[0,.12,0,0],57683:[0,.12,0,0]},"Typewriter-Regular":{33:[0,.61111,0,0],34:[0,.61111,0,0],35:[0,.61111,0,0],36:[.08333,.69444,0,0],37:[.08333,.69444,0,0],38:[0,.61111,0,0],39:[0,.61111,0,0],40:[.08333,.69444,0,0],41:[.08333,.69444,0,0],42:[0,.52083,0,0],43:[-.08056,.53055,0,0],44:[.13889,.125,0,0],45:[-.08056,.53055,0,0],46:[0,.125,0,0],47:[.08333,.69444,0,0],48:[0,.61111,0,0],49:[0,.61111,0,0],50:[0,.61111,0,0],51:[0,.61111,0,0],52:[0,.61111,0,0],53:[0,.61111,0,0],54:[0,.61111,0,0],55:[0,.61111,0,0],56:[0,.61111,0,0],57:[0,.61111,0,0],58:[0,.43056,0,0],59:[.13889,.43056,0,0],60:[-.05556,.55556,0,0],61:[-.19549,.41562,0,0],62:[-.05556,.55556,0,0],63:[0,.61111,0,0],64:[0,.61111,0,0],65:[0,.61111,0,0],66:[0,.61111,0,0],67:[0,.61111,0,0],68:[0,.61111,0,0],69:[0,.61111,0,0],70:[0,.61111,0,0],71:[0,.61111,0,0],72:[0,.61111,0,0],73:[0,.61111,0,0],74:[0,.61111,0,0],75:[0,.61111,0,0],76:[0,.61111,0,0],77:[0,.61111,0,0],78:[0,.61111,0,0],79:[0,.61111,0,0],80:[0,.61111,0,0],81:[.13889,.61111,0,0],82:[0,.61111,0,0],83:[0,.61111,0,0],84:[0,.61111,0,0],85:[0,.61111,0,0],86:[0,.61111,0,0],87:[0,.61111,0,0],88:[0,.61111,0,0],89:[0,.61111,0,0],90:[0,.61111,0,0],91:[.08333,.69444,0,0],92:[.08333,.69444,0,0],93:[.08333,.69444,0,0],94:[0,.61111,0,0],95:[.09514,0,0,0],96:[0,.61111,0,0],97:[0,.43056,0,0],98:[0,.61111,0,0],99:[0,.43056,0,0],100:[0,.61111,0,0],101:[0,.43056,0,0],102:[0,.61111,0,0],103:[.22222,.43056,0,0],104:[0,.61111,0,0],105:[0,.61111,0,0],106:[.22222,.61111,0,0],107:[0,.61111,0,0],108:[0,.61111,0,0],109:[0,.43056,0,0],110:[0,.43056,0,0],111:[0,.43056,0,0],112:[.22222,.43056,0,0],113:[.22222,.43056,0,0],114:[0,.43056,0,0],115:[0,.43056,0,0],116:[0,.55358,0,0],117:[0,.43056,0,0],118:[0,.43056,0,0],119:[0,.43056,0,0],120:[0,.43056,0,0],121:[.22222,.43056,0,0],122:[0,.43056,0,0],123:[.08333,.69444,0,0],124:[.08333,.69444,0,0],125:[.08333,.69444,0,0],126:[0,.61111,0,0],127:[0,.61111,0,0],305:[0,.43056,0,0],567:[.22222,.43056,0,0],768:[0,.61111,0,0],769:[0,.61111,0,0],770:[0,.61111,0,0],771:[0,.61111,0,0],772:[0,.56555,0,0],774:[0,.61111,0,0],776:[0,.61111,0,0],778:[0,.61111,0,0],780:[0,.56597,0,0],915:[0,.61111,0,0],916:[0,.61111,0,0],920:[0,.61111,0,0],923:[0,.61111,0,0],926:[0,.61111,0,0],928:[0,.61111,0,0],931:[0,.61111,0,0],933:[0,.61111,0,0],934:[0,.61111,0,0],936:[0,.61111,0,0],937:[0,.61111,0,0],2018:[0,.61111,0,0],2019:[0,.61111,0,0],8242:[0,.61111,0,0]}}},{}],18:[function(e,t,r){var a=e("./utils");var i=e("./ParseError");function n(e,r,a){if(typeof e==="string"){e=[e]}if(typeof r==="number"){r={numArgs:r}}var i={numArgs:r.numArgs,argTypes:r.argTypes,greediness:r.greediness===undefined?1:r.greediness,allowedInText:!!r.allowedInText,numOptionalArgs:r.numOptionalArgs||0,handler:a};for(var n=0;n<e.length;++n){t.exports[e[n]]=i}}n("\\sqrt",{numArgs:1,numOptionalArgs:1},function(e,t){var r=t[0];var a=t[1];return{type:"sqrt",body:a,index:r}});n("\\text",{numArgs:1,argTypes:["text"],greediness:2},function(e,t){var r=t[0];var a;if(r.type==="ordgroup"){a=r.value}else{a=[r]}return{type:"text",body:a}});n("\\color",{numArgs:2,allowedInText:true,greediness:3,argTypes:["color","original"]},function(e,t){var r=t[0];var a=t[1];var i;if(a.type==="ordgroup"){i=a.value}else{i=[a]}return{type:"color",color:r.value,value:i}});n("\\overline",{numArgs:1},function(e,t){var r=t[0];return{type:"overline",body:r}});n("\\underline",{numArgs:1},function(e,t){var r=t[0];return{type:"underline",body:r}});n("\\rule",{numArgs:2,numOptionalArgs:1,argTypes:["size","size","size"]},function(e,t){var r=t[0];var a=t[1];var i=t[2];return{type:"rule",shift:r&&r.value,width:a.value,height:i.value}});n("\\KaTeX",{numArgs:0},function(e){return{type:"katex"}});n("\\phantom",{numArgs:1},function(e,t){var r=t[0];var a;if(r.type==="ordgroup"){a=r.value}else{a=[r]}return{type:"phantom",value:a}});var s={"\\bigl":{type:"open",size:1},"\\Bigl":{type:"open",size:2},"\\biggl":{type:"open",size:3},"\\Biggl":{type:"open",size:4},"\\bigr":{type:"close",size:1},"\\Bigr":{type:"close",size:2},"\\biggr":{type:"close",size:3},"\\Biggr":{type:"close",size:4},"\\bigm":{type:"rel",size:1},"\\Bigm":{type:"rel",size:2},"\\biggm":{type:"rel",size:3},"\\Biggm":{type:"rel",size:4},"\\big":{type:"textord",size:1},"\\Big":{type:"textord",size:2},"\\bigg":{type:"textord",size:3},"\\Bigg":{type:"textord",size:4}};var l=["(",")","[","\\lbrack","]","\\rbrack","\\{","\\lbrace","\\}","\\rbrace","\\lfloor","\\rfloor","\\lceil","\\rceil","<",">","\\langle","\\rangle","\\lt","\\gt","\\lvert","\\rvert","\\lVert","\\rVert","\\lgroup","\\rgroup","\\lmoustache","\\rmoustache","/","\\backslash","|","\\vert","\\|","\\Vert","\\uparrow","\\Uparrow","\\downarrow","\\Downarrow","\\updownarrow","\\Updownarrow","."];var o={"\\Bbb":"\\mathbb","\\bold":"\\mathbf","\\frak":"\\mathfrak"};n(["\\blue","\\orange","\\pink","\\red","\\green","\\gray","\\purple","\\blueA","\\blueB","\\blueC","\\blueD","\\blueE","\\tealA","\\tealB","\\tealC","\\tealD","\\tealE","\\greenA","\\greenB","\\greenC","\\greenD","\\greenE","\\goldA","\\goldB","\\goldC","\\goldD","\\goldE","\\redA","\\redB","\\redC","\\redD","\\redE","\\maroonA","\\maroonB","\\maroonC","\\maroonD","\\maroonE","\\purpleA","\\purpleB","\\purpleC","\\purpleD","\\purpleE","\\mintA","\\mintB","\\mintC","\\grayA","\\grayB","\\grayC","\\grayD","\\grayE","\\grayF","\\grayG","\\grayH","\\grayI","\\kaBlue","\\kaGreen"],{numArgs:1,allowedInText:true,greediness:3},function(e,t){var r=t[0];var a;if(r.type==="ordgroup"){a=r.value}else{a=[r]}return{type:"color",color:"katex-"+e.funcName.slice(1),value:a}});n(["\\arcsin","\\arccos","\\arctan","\\arg","\\cos","\\cosh","\\cot","\\coth","\\csc","\\deg","\\dim","\\exp","\\hom","\\ker","\\lg","\\ln","\\log","\\sec","\\sin","\\sinh","\\tan","\\tanh"],{numArgs:0},function(e){return{type:"op",limits:false,symbol:false,body:e.funcName}});n(["\\det","\\gcd","\\inf","\\lim","\\liminf","\\limsup","\\max","\\min","\\Pr","\\sup"],{numArgs:0},function(e){return{type:"op",limits:true,symbol:false,body:e.funcName}});n(["\\int","\\iint","\\iiint","\\oint"],{numArgs:0},function(e){return{type:"op",limits:false,symbol:true,body:e.funcName}});n(["\\coprod","\\bigvee","\\bigwedge","\\biguplus","\\bigcap","\\bigcup","\\intop","\\prod","\\sum","\\bigotimes","\\bigoplus","\\bigodot","\\bigsqcup","\\smallint"],{
  numArgs:0},function(e){return{type:"op",limits:true,symbol:true,body:e.funcName}});n(["\\dfrac","\\frac","\\tfrac","\\dbinom","\\binom","\\tbinom"],{numArgs:2,greediness:2},function(e,t){var r=t[0];var a=t[1];var i;var n=null;var s=null;var l="auto";switch(e.funcName){case"\\dfrac":case"\\frac":case"\\tfrac":i=true;break;case"\\dbinom":case"\\binom":case"\\tbinom":i=false;n="(";s=")";break;default:throw new Error("Unrecognized genfrac command")}switch(e.funcName){case"\\dfrac":case"\\dbinom":l="display";break;case"\\tfrac":case"\\tbinom":l="text";break}return{type:"genfrac",numer:r,denom:a,hasBarLine:i,leftDelim:n,rightDelim:s,size:l}});n(["\\llap","\\rlap"],{numArgs:1,allowedInText:true},function(e,t){var r=t[0];return{type:e.funcName.slice(1),body:r}});n(["\\bigl","\\Bigl","\\biggl","\\Biggl","\\bigr","\\Bigr","\\biggr","\\Biggr","\\bigm","\\Bigm","\\biggm","\\Biggm","\\big","\\Big","\\bigg","\\Bigg","\\left","\\right"],{numArgs:1},function(e,t){var r=t[0];if(!a.contains(l,r.value)){throw new i("Invalid delimiter: '"+r.value+"' after '"+e.funcName+"'",e.lexer,e.positions[1])}if(e.funcName==="\\left"||e.funcName==="\\right"){return{type:"leftright",value:r.value}}else{return{type:"delimsizing",size:s[e.funcName].size,delimType:s[e.funcName].type,value:r.value}}});n(["\\tiny","\\scriptsize","\\footnotesize","\\small","\\normalsize","\\large","\\Large","\\LARGE","\\huge","\\Huge"],0,null);n(["\\displaystyle","\\textstyle","\\scriptstyle","\\scriptscriptstyle"],0,null);n(["\\mathrm","\\mathit","\\mathbf","\\mathbb","\\mathcal","\\mathfrak","\\mathscr","\\mathsf","\\mathtt","\\Bbb","\\bold","\\frak"],{numArgs:1,greediness:2},function(e,t){var r=t[0];var a=e.funcName;if(a in o){a=o[a]}return{type:"font",font:a.slice(1),body:r}});n(["\\acute","\\grave","\\ddot","\\tilde","\\bar","\\breve","\\check","\\hat","\\vec","\\dot"],{numArgs:1},function(e,t){var r=t[0];return{type:"accent",accent:e.funcName,base:r}});n(["\\over","\\choose"],{numArgs:0},function(e){var t;switch(e.funcName){case"\\over":t="\\frac";break;case"\\choose":t="\\binom";break;default:throw new Error("Unrecognized infix genfrac command")}return{type:"infix",replaceWith:t}});n(["\\\\","\\cr"],{numArgs:0,numOptionalArgs:1,argTypes:["size"]},function(e,t){var r=t[0];return{type:"cr",size:r}});n(["\\begin","\\end"],{numArgs:1,argTypes:["text"]},function(e,t){var r=t[0];if(r.type!=="ordgroup"){throw new i("Invalid environment name",e.lexer,e.positions[1])}var a="";for(var n=0;n<r.value.length;++n){a+=r.value[n].value}return{type:"environment",name:a,namepos:e.positions[1]}})},{"./ParseError":5,"./utils":23}],19:[function(e,t,r){var a=e("./utils");function i(e,t){this.type=e;this.attributes={};this.children=t||[]}i.prototype.setAttribute=function(e,t){this.attributes[e]=t};i.prototype.toNode=function(){var e=document.createElementNS("http://www.w3.org/1998/Math/MathML",this.type);for(var t in this.attributes){if(Object.prototype.hasOwnProperty.call(this.attributes,t)){e.setAttribute(t,this.attributes[t])}}for(var r=0;r<this.children.length;r++){e.appendChild(this.children[r].toNode())}return e};i.prototype.toMarkup=function(){var e="<"+this.type;for(var t in this.attributes){if(Object.prototype.hasOwnProperty.call(this.attributes,t)){e+=" "+t+'="';e+=a.escape(this.attributes[t]);e+='"'}}e+=">";for(var r=0;r<this.children.length;r++){e+=this.children[r].toMarkup()}e+="</"+this.type+">";return e};function n(e){this.text=e}n.prototype.toNode=function(){return document.createTextNode(this.text)};n.prototype.toMarkup=function(){return a.escape(this.text)};t.exports={MathNode:i,TextNode:n}},{"./utils":23}],20:[function(e,t,r){function a(e,t,r){this.type=e;this.value=t;this.mode=r}t.exports={ParseNode:a}},{}],21:[function(e,t,r){var a=e("./Parser");var i=function(e,t){var r=new a(e,t);return r.parse()};t.exports=i},{"./Parser":6}],22:[function(e,t,r){t.exports={math:{},text:{}};function a(e,r,a,i,n){t.exports[e][n]={font:r,group:a,replace:i}}var i="math";var n="text";var s="main";var l="ams";var o="accent";var u="bin";var p="close";var h="inner";var c="mathord";var v="op";var m="open";var f="punct";var d="rel";var g="spacing";var y="textord";a(i,s,d,"\u2261","\\equiv");a(i,s,d,"\u227a","\\prec");a(i,s,d,"\u227b","\\succ");a(i,s,d,"\u223c","\\sim");a(i,s,d,"\u22a5","\\perp");a(i,s,d,"\u2aaf","\\preceq");a(i,s,d,"\u2ab0","\\succeq");a(i,s,d,"\u2243","\\simeq");a(i,s,d,"\u2223","\\mid");a(i,s,d,"\u226a","\\ll");a(i,s,d,"\u226b","\\gg");a(i,s,d,"\u224d","\\asymp");a(i,s,d,"\u2225","\\parallel");a(i,s,d,"\u22c8","\\bowtie");a(i,s,d,"\u2323","\\smile");a(i,s,d,"\u2291","\\sqsubseteq");a(i,s,d,"\u2292","\\sqsupseteq");a(i,s,d,"\u2250","\\doteq");a(i,s,d,"\u2322","\\frown");a(i,s,d,"\u220b","\\ni");a(i,s,d,"\u221d","\\propto");a(i,s,d,"\u22a2","\\vdash");a(i,s,d,"\u22a3","\\dashv");a(i,s,d,"\u220b","\\owns");a(i,s,f,".","\\ldotp");a(i,s,f,"\u22c5","\\cdotp");a(i,s,y,"#","\\#");a(i,s,y,"&","\\&");a(i,s,y,"\u2135","\\aleph");a(i,s,y,"\u2200","\\forall");a(i,s,y,"\u210f","\\hbar");a(i,s,y,"\u2203","\\exists");a(i,s,y,"\u2207","\\nabla");a(i,s,y,"\u266d","\\flat");a(i,s,y,"\u2113","\\ell");a(i,s,y,"\u266e","\\natural");a(i,s,y,"\u2663","\\clubsuit");a(i,s,y,"\u2118","\\wp");a(i,s,y,"\u266f","\\sharp");a(i,s,y,"\u2662","\\diamondsuit");a(i,s,y,"\u211c","\\Re");a(i,s,y,"\u2661","\\heartsuit");a(i,s,y,"\u2111","\\Im");a(i,s,y,"\u2660","\\spadesuit");a(i,s,y,"\u2020","\\dag");a(i,s,y,"\u2021","\\ddag");a(i,s,p,"\u23b1","\\rmoustache");a(i,s,m,"\u23b0","\\lmoustache");a(i,s,p,"\u27ef","\\rgroup");a(i,s,m,"\u27ee","\\lgroup");a(i,s,u,"\u2213","\\mp");a(i,s,u,"\u2296","\\ominus");a(i,s,u,"\u228e","\\uplus");a(i,s,u,"\u2293","\\sqcap");a(i,s,u,"\u2217","\\ast");a(i,s,u,"\u2294","\\sqcup");a(i,s,u,"\u25ef","\\bigcirc");a(i,s,u,"\u2219","\\bullet");a(i,s,u,"\u2021","\\ddagger");a(i,s,u,"\u2240","\\wr");a(i,s,u,"\u2a3f","\\amalg");a(i,s,d,"\u27f5","\\longleftarrow");a(i,s,d,"\u21d0","\\Leftarrow");a(i,s,d,"\u27f8","\\Longleftarrow");a(i,s,d,"\u27f6","\\longrightarrow");a(i,s,d,"\u21d2","\\Rightarrow");a(i,s,d,"\u27f9","\\Longrightarrow");a(i,s,d,"\u2194","\\leftrightarrow");a(i,s,d,"\u27f7","\\longleftrightarrow");a(i,s,d,"\u21d4","\\Leftrightarrow");a(i,s,d,"\u27fa","\\Longleftrightarrow");a(i,s,d,"\u21a6","\\mapsto");a(i,s,d,"\u27fc","\\longmapsto");a(i,s,d,"\u2197","\\nearrow");a(i,s,d,"\u21a9","\\hookleftarrow");a(i,s,d,"\u21aa","\\hookrightarrow");a(i,s,d,"\u2198","\\searrow");a(i,s,d,"\u21bc","\\leftharpoonup");a(i,s,d,"\u21c0","\\rightharpoonup");a(i,s,d,"\u2199","\\swarrow");a(i,s,d,"\u21bd","\\leftharpoondown");a(i,s,d,"\u21c1","\\rightharpoondown");a(i,s,d,"\u2196","\\nwarrow");a(i,s,d,"\u21cc","\\rightleftharpoons");a(i,l,d,"\u226e","\\nless");a(i,l,d,"\ue010","\\nleqslant");a(i,l,d,"\ue011","\\nleqq");a(i,l,d,"\u2a87","\\lneq");a(i,l,d,"\u2268","\\lneqq");a(i,l,d,"\ue00c","\\lvertneqq");a(i,l,d,"\u22e6","\\lnsim");a(i,l,d,"\u2a89","\\lnapprox");a(i,l,d,"\u2280","\\nprec");a(i,l,d,"\u22e0","\\npreceq");a(i,l,d,"\u22e8","\\precnsim");a(i,l,d,"\u2ab9","\\precnapprox");a(i,l,d,"\u2241","\\nsim");a(i,l,d,"\ue006","\\nshortmid");a(i,l,d,"\u2224","\\nmid");a(i,l,d,"\u22ac","\\nvdash");a(i,l,d,"\u22ad","\\nvDash");a(i,l,d,"\u22ea","\\ntriangleleft");a(i,l,d,"\u22ec","\\ntrianglelefteq");a(i,l,d,"\u228a","\\subsetneq");a(i,l,d,"\ue01a","\\varsubsetneq");a(i,l,d,"\u2acb","\\subsetneqq");a(i,l,d,"\ue017","\\varsubsetneqq");a(i,l,d,"\u226f","\\ngtr");a(i,l,d,"\ue00f","\\ngeqslant");a(i,l,d,"\ue00e","\\ngeqq");a(i,l,d,"\u2a88","\\gneq");a(i,l,d,"\u2269","\\gneqq");a(i,l,d,"\ue00d","\\gvertneqq");a(i,l,d,"\u22e7","\\gnsim");a(i,l,d,"\u2a8a","\\gnapprox");a(i,l,d,"\u2281","\\nsucc");a(i,l,d,"\u22e1","\\nsucceq");a(i,l,d,"\u22e9","\\succnsim");a(i,l,d,"\u2aba","\\succnapprox");a(i,l,d,"\u2246","\\ncong");a(i,l,d,"\ue007","\\nshortparallel");a(i,l,d,"\u2226","\\nparallel");a(i,l,d,"\u22af","\\nVDash");a(i,l,d,"\u22eb","\\ntriangleright");a(i,l,d,"\u22ed","\\ntrianglerighteq");a(i,l,d,"\ue018","\\nsupseteqq");a(i,l,d,"\u228b","\\supsetneq");a(i,l,d,"\ue01b","\\varsupsetneq");a(i,l,d,"\u2acc","\\supsetneqq");a(i,l,d,"\ue019","\\varsupsetneqq");a(i,l,d,"\u22ae","\\nVdash");a(i,l,d,"\u2ab5","\\precneqq");a(i,l,d,"\u2ab6","\\succneqq");a(i,l,d,"\ue016","\\nsubseteqq");a(i,l,u,"\u22b4","\\unlhd");a(i,l,u,"\u22b5","\\unrhd");a(i,l,d,"\u219a","\\nleftarrow");a(i,l,d,"\u219b","\\nrightarrow");a(i,l,d,"\u21cd","\\nLeftarrow");a(i,l,d,"\u21cf","\\nRightarrow");a(i,l,d,"\u21ae","\\nleftrightarrow");a(i,l,d,"\u21ce","\\nLeftrightarrow");a(i,l,d,"\u25b3","\\vartriangle");a(i,l,y,"\u210f","\\hslash");a(i,l,y,"\u25bd","\\triangledown");a(i,l,y,"\u25ca","\\lozenge");a(i,l,y,"\u24c8","\\circledS");a(i,l,y,"\xae","\\circledR");a(i,l,y,"\u2221","\\measuredangle");a(i,l,y,"\u2204","\\nexists");a(i,l,y,"\u2127","\\mho");a(i,l,y,"\u2132","\\Finv");a(i,l,y,"\u2141","\\Game");a(i,l,y,"k","\\Bbbk");a(i,l,y,"\u2035","\\backprime");a(i,l,y,"\u25b2","\\blacktriangle");a(i,l,y,"\u25bc","\\blacktriangledown");a(i,l,y,"\u25a0","\\blacksquare");a(i,l,y,"\u29eb","\\blacklozenge");a(i,l,y,"\u2605","\\bigstar");a(i,l,y,"\u2222","\\sphericalangle");a(i,l,y,"\u2201","\\complement");a(i,l,y,"\xf0","\\eth");a(i,l,y,"\u2571","\\diagup");a(i,l,y,"\u2572","\\diagdown");a(i,l,y,"\u25a1","\\square");a(i,l,y,"\u25a1","\\Box");a(i,l,y,"\u25ca","\\Diamond");a(i,l,y,"\xa5","\\yen");a(i,l,y,"\u2713","\\checkmark");a(i,l,y,"\u2136","\\beth");a(i,l,y,"\u2138","\\daleth");a(i,l,y,"\u2137","\\gimel");a(i,l,y,"\u03dd","\\digamma");a(i,l,y,"\u03f0","\\varkappa");a(i,l,m,"\u250c","\\ulcorner");a(i,l,p,"\u2510","\\urcorner");a(i,l,m,"\u2514","\\llcorner");a(i,l,p,"\u2518","\\lrcorner");a(i,l,d,"\u2266","\\leqq");a(i,l,d,"\u2a7d","\\leqslant");a(i,l,d,"\u2a95","\\eqslantless");a(i,l,d,"\u2272","\\lesssim");a(i,l,d,"\u2a85","\\lessapprox");a(i,l,d,"\u224a","\\approxeq");a(i,l,u,"\u22d6","\\lessdot");a(i,l,d,"\u22d8","\\lll");a(i,l,d,"\u2276","\\lessgtr");a(i,l,d,"\u22da","\\lesseqgtr");a(i,l,d,"\u2a8b","\\lesseqqgtr");a(i,l,d,"\u2251","\\doteqdot");a(i,l,d,"\u2253","\\risingdotseq");a(i,l,d,"\u2252","\\fallingdotseq");a(i,l,d,"\u223d","\\backsim");a(i,l,d,"\u22cd","\\backsimeq");a(i,l,d,"\u2ac5","\\subseteqq");a(i,l,d,"\u22d0","\\Subset");a(i,l,d,"\u228f","\\sqsubset");a(i,l,d,"\u227c","\\preccurlyeq");a(i,l,d,"\u22de","\\curlyeqprec");a(i,l,d,"\u227e","\\precsim");a(i,l,d,"\u2ab7","\\precapprox");a(i,l,d,"\u22b2","\\vartriangleleft");a(i,l,d,"\u22b4","\\trianglelefteq");a(i,l,d,"\u22a8","\\vDash");a(i,l,d,"\u22aa","\\Vvdash");a(i,l,d,"\u2323","\\smallsmile");a(i,l,d,"\u2322","\\smallfrown");a(i,l,d,"\u224f","\\bumpeq");a(i,l,d,"\u224e","\\Bumpeq");a(i,l,d,"\u2267","\\geqq");a(i,l,d,"\u2a7e","\\geqslant");a(i,l,d,"\u2a96","\\eqslantgtr");a(i,l,d,"\u2273","\\gtrsim");a(i,l,d,"\u2a86","\\gtrapprox");a(i,l,u,"\u22d7","\\gtrdot");a(i,l,d,"\u22d9","\\ggg");a(i,l,d,"\u2277","\\gtrless");a(i,l,d,"\u22db","\\gtreqless");a(i,l,d,"\u2a8c","\\gtreqqless");a(i,l,d,"\u2256","\\eqcirc");a(i,l,d,"\u2257","\\circeq");a(i,l,d,"\u225c","\\triangleq");a(i,l,d,"\u223c","\\thicksim");a(i,l,d,"\u2248","\\thickapprox");a(i,l,d,"\u2ac6","\\supseteqq");a(i,l,d,"\u22d1","\\Supset");a(i,l,d,"\u2290","\\sqsupset");a(i,l,d,"\u227d","\\succcurlyeq");a(i,l,d,"\u22df","\\curlyeqsucc");a(i,l,d,"\u227f","\\succsim");a(i,l,d,"\u2ab8","\\succapprox");a(i,l,d,"\u22b3","\\vartriangleright");a(i,l,d,"\u22b5","\\trianglerighteq");a(i,l,d,"\u22a9","\\Vdash");a(i,l,d,"\u2223","\\shortmid");a(i,l,d,"\u2225","\\shortparallel");a(i,l,d,"\u226c","\\between");a(i,l,d,"\u22d4","\\pitchfork");a(i,l,d,"\u221d","\\varpropto");a(i,l,d,"\u25c0","\\blacktriangleleft");a(i,l,d,"\u2234","\\therefore");a(i,l,d,"\u220d","\\backepsilon");a(i,l,d,"\u25b6","\\blacktriangleright");a(i,l,d,"\u2235","\\because");a(i,l,d,"\u22d8","\\llless");a(i,l,d,"\u22d9","\\gggtr");a(i,l,u,"\u22b2","\\lhd");a(i,l,u,"\u22b3","\\rhd");a(i,l,d,"\u2242","\\eqsim");a(i,s,d,"\u22c8","\\Join");a(i,l,d,"\u2251","\\Doteq");a(i,l,u,"\u2214","\\dotplus");a(i,l,u,"\u2216","\\smallsetminus");a(i,l,u,"\u22d2","\\Cap");a(i,l,u,"\u22d3","\\Cup");a(i,l,u,"\u2a5e","\\doublebarwedge");a(i,l,u,"\u229f","\\boxminus");a(i,l,u,"\u229e","\\boxplus");a(i,l,u,"\u22c7","\\divideontimes");a(i,l,u,"\u22c9","\\ltimes");a(i,l,u,"\u22ca","\\rtimes");a(i,l,u,"\u22cb","\\leftthreetimes");a(i,l,u,"\u22cc","\\rightthreetimes");a(i,l,u,"\u22cf","\\curlywedge");a(i,l,u,"\u22ce","\\curlyvee");a(i,l,u,"\u229d","\\circleddash");a(i,l,u,"\u229b","\\circledast");a(i,l,u,"\u22c5","\\centerdot");a(i,l,u,"\u22ba","\\intercal");a(i,l,u,"\u22d2","\\doublecap");a(i,l,u,"\u22d3","\\doublecup");a(i,l,u,"\u22a0","\\boxtimes");a(i,l,d,"\u21e2","\\dashrightarrow");a(i,l,d,"\u21e0","\\dashleftarrow");a(i,l,d,"\u21c7","\\leftleftarrows");a(i,l,d,"\u21c6","\\leftrightarrows");a(i,l,d,"\u21da","\\Lleftarrow");a(i,l,d,"\u219e","\\twoheadleftarrow");a(i,l,d,"\u21a2","\\leftarrowtail");a(i,l,d,"\u21ab","\\looparrowleft");a(i,l,d,"\u21cb","\\leftrightharpoons");a(i,l,d,"\u21b6","\\curvearrowleft");a(i,l,d,"\u21ba","\\circlearrowleft");a(i,l,d,"\u21b0","\\Lsh");a(i,l,d,"\u21c8","\\upuparrows");a(i,l,d,"\u21bf","\\upharpoonleft");a(i,l,d,"\u21c3","\\downharpoonleft");a(i,l,d,"\u22b8","\\multimap");a(i,l,d,"\u21ad","\\leftrightsquigarrow");a(i,l,d,"\u21c9","\\rightrightarrows");a(i,l,d,"\u21c4","\\rightleftarrows");a(i,l,d,"\u21a0","\\twoheadrightarrow");a(i,l,d,"\u21a3","\\rightarrowtail");a(i,l,d,"\u21ac","\\looparrowright");a(i,l,d,"\u21b7","\\curvearrowright");a(i,l,d,"\u21bb","\\circlearrowright");a(i,l,d,"\u21b1","\\Rsh");a(i,l,d,"\u21ca","\\downdownarrows");a(i,l,d,"\u21be","\\upharpoonright");a(i,l,d,"\u21c2","\\downharpoonright");a(i,l,d,"\u21dd","\\rightsquigarrow");a(i,l,d,"\u21dd","\\leadsto");a(i,l,d,"\u21db","\\Rrightarrow");a(i,l,d,"\u21be","\\restriction");a(i,s,y,"\u2018","`");a(i,s,y,"$","\\$");a(i,s,y,"%","\\%");a(i,s,y,"_","\\_");a(i,s,y,"\u2220","\\angle");a(i,s,y,"\u221e","\\infty");a(i,s,y,"\u2032","\\prime");a(i,s,y,"\u25b3","\\triangle");a(i,s,y,"\u0393","\\Gamma");a(i,s,y,"\u0394","\\Delta");a(i,s,y,"\u0398","\\Theta");a(i,s,y,"\u039b","\\Lambda");a(i,s,y,"\u039e","\\Xi");a(i,s,y,"\u03a0","\\Pi");a(i,s,y,"\u03a3","\\Sigma");a(i,s,y,"\u03a5","\\Upsilon");a(i,s,y,"\u03a6","\\Phi");a(i,s,y,"\u03a8","\\Psi");a(i,s,y,"\u03a9","\\Omega");a(i,s,y,"\xac","\\neg");a(i,s,y,"\xac","\\lnot");a(i,s,y,"\u22a4","\\top");a(i,s,y,"\u22a5","\\bot");a(i,s,y,"\u2205","\\emptyset");a(i,l,y,"\u2205","\\varnothing");a(i,s,c,"\u03b1","\\alpha");a(i,s,c,"\u03b2","\\beta");a(i,s,c,"\u03b3","\\gamma");a(i,s,c,"\u03b4","\\delta");a(i,s,c,"\u03f5","\\epsilon");a(i,s,c,"\u03b6","\\zeta");a(i,s,c,"\u03b7","\\eta");a(i,s,c,"\u03b8","\\theta");a(i,s,c,"\u03b9","\\iota");a(i,s,c,"\u03ba","\\kappa");a(i,s,c,"\u03bb","\\lambda");a(i,s,c,"\u03bc","\\mu");a(i,s,c,"\u03bd","\\nu");a(i,s,c,"\u03be","\\xi");a(i,s,c,"o","\\omicron");a(i,s,c,"\u03c0","\\pi");a(i,s,c,"\u03c1","\\rho");a(i,s,c,"\u03c3","\\sigma");a(i,s,c,"\u03c4","\\tau");a(i,s,c,"\u03c5","\\upsilon");a(i,s,c,"\u03d5","\\phi");a(i,s,c,"\u03c7","\\chi");a(i,s,c,"\u03c8","\\psi");a(i,s,c,"\u03c9","\\omega");a(i,s,c,"\u03b5","\\varepsilon");a(i,s,c,"\u03d1","\\vartheta");a(i,s,c,"\u03d6","\\varpi");a(i,s,c,"\u03f1","\\varrho");a(i,s,c,"\u03c2","\\varsigma");a(i,s,c,"\u03c6","\\varphi");a(i,s,u,"\u2217","*");a(i,s,u,"+","+");a(i,s,u,"\u2212","-");a(i,s,u,"\u22c5","\\cdot");a(i,s,u,"\u2218","\\circ");a(i,s,u,"\xf7","\\div");a(i,s,u,"\xb1","\\pm");a(i,s,u,"\xd7","\\times");a(i,s,u,"\u2229","\\cap");a(i,s,u,"\u222a","\\cup");a(i,s,u,"\u2216","\\setminus");a(i,s,u,"\u2227","\\land");a(i,s,u,"\u2228","\\lor");a(i,s,u,"\u2227","\\wedge");a(i,s,u,"\u2228","\\vee");a(i,s,y,"\u221a","\\surd");a(i,s,m,"(","(");a(i,s,m,"[","[");a(i,s,m,"\u27e8","\\langle");a(i,s,m,"\u2223","\\lvert");a(i,s,m,"\u2225","\\lVert");a(i,s,p,")",")");a(i,s,p,"]","]");a(i,s,p,"?","?");a(i,s,p,"!","!");a(i,s,p,"\u27e9","\\rangle");a(i,s,p,"\u2223","\\rvert");a(i,s,p,"\u2225","\\rVert");a(i,s,d,"=","=");a(i,s,d,"<","<");a(i,s,d,">",">");a(i,s,d,":",":");a(i,s,d,"\u2248","\\approx");a(i,s,d,"\u2245","\\cong");a(i,s,d,"\u2265","\\ge");a(i,s,d,"\u2265","\\geq");a(i,s,d,"\u2190","\\gets");a(i,s,d,">","\\gt");a(i,s,d,"\u2208","\\in");a(i,s,d,"\u2209","\\notin");a(i,s,d,"\u2282","\\subset");a(i,s,d,"\u2283","\\supset");a(i,s,d,"\u2286","\\subseteq");a(i,s,d,"\u2287","\\supseteq");a(i,l,d,"\u2288","\\nsubseteq");a(i,l,d,"\u2289","\\nsupseteq");a(i,s,d,"\u22a8","\\models");a(i,s,d,"\u2190","\\leftarrow");a(i,s,d,"\u2264","\\le");a(i,s,d,"\u2264","\\leq");a(i,s,d,"<","\\lt");a(i,s,d,"\u2260","\\ne");a(i,s,d,"\u2260","\\neq");a(i,s,d,"\u2192","\\rightarrow");a(i,s,d,"\u2192","\\to");a(i,l,d,"\u2271","\\ngeq");a(i,l,d,"\u2270","\\nleq");a(i,s,g,null,"\\!");a(i,s,g,"\xa0","\\ ");a(i,s,g,"\xa0","~");a(i,s,g,null,"\\,");a(i,s,g,null,"\\:");a(i,s,g,null,"\\;");a(i,s,g,null,"\\enspace");a(i,s,g,null,"\\qquad");a(i,s,g,null,"\\quad");a(i,s,g,"\xa0","\\space");a(i,s,f,",",",");a(i,s,f,";",";");a(i,s,f,":","\\colon");a(i,l,u,"\u22bc","\\barwedge");a(i,l,u,"\u22bb","\\veebar");a(i,s,u,"\u2299","\\odot");a(i,s,u,"\u2295","\\oplus");a(i,s,u,"\u2297","\\otimes");a(i,s,y,"\u2202","\\partial");a(i,s,u,"\u2298","\\oslash");a(i,l,u,"\u229a","\\circledcirc");a(i,l,u,"\u22a1","\\boxdot");a(i,s,u,"\u25b3","\\bigtriangleup");a(i,s,u,"\u25bd","\\bigtriangledown");a(i,s,u,"\u2020","\\dagger");a(i,s,u,"\u22c4","\\diamond");a(i,s,u,"\u22c6","\\star");a(i,s,u,"\u25c3","\\triangleleft");a(i,s,u,"\u25b9","\\triangleright");a(i,s,m,"{","\\{");a(i,s,p,"}","\\}");a(i,s,m,"{","\\lbrace");a(i,s,p,"}","\\rbrace");a(i,s,m,"[","\\lbrack");a(i,s,p,"]","\\rbrack");a(i,s,m,"\u230a","\\lfloor");a(i,s,p,"\u230b","\\rfloor");a(i,s,m,"\u2308","\\lceil");a(i,s,p,"\u2309","\\rceil");a(i,s,y,"\\","\\backslash");a(i,s,y,"\u2223","|");a(i,s,y,"\u2223","\\vert");a(i,s,y,"\u2225","\\|");a(i,s,y,"\u2225","\\Vert");a(i,s,d,"\u2191","\\uparrow");a(i,s,d,"\u21d1","\\Uparrow");a(i,s,d,"\u2193","\\downarrow");a(i,s,d,"\u21d3","\\Downarrow");a(i,s,d,"\u2195","\\updownarrow");a(i,s,d,"\u21d5","\\Updownarrow");a(i,i,v,"\u2210","\\coprod");a(i,i,v,"\u22c1","\\bigvee");a(i,i,v,"\u22c0","\\bigwedge");a(i,i,v,"\u2a04","\\biguplus");a(i,i,v,"\u22c2","\\bigcap");a(i,i,v,"\u22c3","\\bigcup");a(i,i,v,"\u222b","\\int");a(i,i,v,"\u222b","\\intop");a(i,i,v,"\u222c","\\iint");a(i,i,v,"\u222d","\\iiint");a(i,i,v,"\u220f","\\prod");a(i,i,v,"\u2211","\\sum");a(i,i,v,"\u2a02","\\bigotimes");a(i,i,v,"\u2a01","\\bigoplus");a(i,i,v,"\u2a00","\\bigodot");a(i,i,v,"\u222e","\\oint");a(i,i,v,"\u2a06","\\bigsqcup");a(i,i,v,"\u222b","\\smallint");a(i,s,h,"\u2026","\\ldots");a(i,s,h,"\u22ef","\\cdots");a(i,s,h,"\u22f1","\\ddots");a(i,s,y,"\u22ee","\\vdots");a(i,s,o,"\xb4","\\acute");a(i,s,o,"`","\\grave");a(i,s,o,"\xa8","\\ddot");a(i,s,o,"~","\\tilde");a(i,s,o,"\xaf","\\bar");a(i,s,o,"\u02d8","\\breve");a(i,s,o,"\u02c7","\\check");a(i,s,o,"^","\\hat");a(i,s,o,"\u20d7","\\vec");a(i,s,o,"\u02d9","\\dot");a(i,s,c,"\u0131","\\imath");a(i,s,c,"\u0237","\\jmath");a(n,s,g,"\xa0","\\ ");a(n,s,g,"\xa0"," ");a(n,s,g,"\xa0","~");var b;var x;var w='0123456789/@."';for(b=0;b<w.length;b++){x=w.charAt(b);a(i,s,y,x,x)}var k="0123456789`!@*()-=+[]'\";:?/.,";for(b=0;b<k.length;b++){x=k.charAt(b);a(n,s,y,x,x)}var z="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";for(b=0;b<z.length;b++){x=z.charAt(b);a(i,s,c,x,x);a(n,s,y,x,x)}},{}],23:[function(e,t,r){var a=Array.prototype.indexOf;var i=function(e,t){if(e==null){return-1}if(a&&e.indexOf===a){return e.indexOf(t)}var r=0;var i=e.length;for(;r<i;r++){if(e[r]===t){return r}}return-1};var n=function(e,t){return i(e,t)!==-1};var s=function(e,t){return e===undefined?t:e};var l=/([A-Z])/g;var o=function(e){return e.replace(l,"-$1").toLowerCase()};var u={"&":"&amp;",">":"&gt;","<":"&lt;",'"':"&quot;","'":"&#x27;"};var p=/[&><"']/g;function h(e){return u[e]}function c(e){return(""+e).replace(p,h)}var v;if(typeof document!=="undefined"){var m=document.createElement("span");if("textContent"in m){v=function(e,t){e.textContent=t}}else{v=function(e,t){e.innerText=t}}}function f(e){v(e,"")}t.exports={contains:n,deflt:s,escape:c,hyphenate:o,indexOf:i,setTextContent:v,clearNode:f}},{}]},{},[1])(1)});

  return module.exports;
}();


// EXPORTS

return {
  render: F2(renderElement),
  renderToString: F2(renderToString)
};

}();
var _bsouthga$elm_katex$KaTeX$renderToStringWithOptions = F2(
	function (options, string) {
		return A2(_bsouthga$elm_katex$Native_KaTeX.renderToString, options, string);
	});
var _bsouthga$elm_katex$KaTeX$renderWithOptions = F2(
	function (options, string) {
		return A2(_bsouthga$elm_katex$Native_KaTeX.render, options, string);
	});
var _bsouthga$elm_katex$KaTeX$defaultOptions = {displayMode: false, errorColor: '#cc0000'};
var _bsouthga$elm_katex$KaTeX$renderToString = function (string) {
	return A2(_bsouthga$elm_katex$KaTeX$renderToStringWithOptions, _bsouthga$elm_katex$KaTeX$defaultOptions, string);
};
var _bsouthga$elm_katex$KaTeX$render = function (string) {
	return A2(_bsouthga$elm_katex$KaTeX$renderWithOptions, _bsouthga$elm_katex$KaTeX$defaultOptions, string);
};
var _bsouthga$elm_katex$KaTeX$Options = F2(
	function (a, b) {
		return {displayMode: a, errorColor: b};
	});

var _bsouthga$elm_katex$ExactBinomial$display = F2(
	function (name, value) {
		return A2(
			_elm_lang$html$Html$div,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: _elm_lang$html$Html$text(
					A2(
						_elm_lang$core$Basics_ops['++'],
						name,
						A2(
							_elm_lang$core$Basics_ops['++'],
							' ==> ',
							_elm_lang$core$Basics$toString(value)))),
				_1: {ctor: '[]'}
			});
	});
var _bsouthga$elm_katex$ExactBinomial$outputX = function (resultVal) {
	var _p0 = resultVal;
	if (_p0.ctor === 'Ok') {
		return _elm_lang$html$Html$text('');
	} else {
		if (_p0._0 === 'Empty') {
			return A2(
				_elm_lang$html$Html$span,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('error'),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('Enter a value for the observed number of success, x.'),
					_1: {ctor: '[]'}
				});
		} else {
			return A2(
				_elm_lang$html$Html$span,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('error'),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text(_p0._0),
					_1: {ctor: '[]'}
				});
		}
	}
};
var _bsouthga$elm_katex$ExactBinomial$outputVal = function (resultVal) {
	var _p1 = resultVal;
	if (_p1.ctor === 'Ok') {
		return _elm_lang$html$Html$text('');
	} else {
		return A2(
			_elm_lang$html$Html$span,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('error'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: _elm_lang$html$Html$text(_p1._0),
				_1: {ctor: '[]'}
			});
	}
};
var _bsouthga$elm_katex$ExactBinomial$inputGroup = F4(
	function (id, $default, onchange, intext) {
		return _rundis$elm_bootstrap$Bootstrap_Form_InputGroup$view(
			A2(
				_rundis$elm_bootstrap$Bootstrap_Form_InputGroup$predecessors,
				{
					ctor: '::',
					_0: A2(
						_rundis$elm_bootstrap$Bootstrap_Form_InputGroup$span,
						{ctor: '[]'},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text(intext),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				},
				_rundis$elm_bootstrap$Bootstrap_Form_InputGroup$config(
					_rundis$elm_bootstrap$Bootstrap_Form_InputGroup$number(
						{
							ctor: '::',
							_0: _rundis$elm_bootstrap$Bootstrap_Form_Input$id(id),
							_1: {
								ctor: '::',
								_0: _rundis$elm_bootstrap$Bootstrap_Form_Input$small,
								_1: {
									ctor: '::',
									_0: _rundis$elm_bootstrap$Bootstrap_Form_Input$defaultValue($default),
									_1: {
										ctor: '::',
										_0: _rundis$elm_bootstrap$Bootstrap_Form_Input$onInput(onchange),
										_1: {ctor: '[]'}
									}
								}
							}
						}))));
	});
var _bsouthga$elm_katex$ExactBinomial$subscriptions = function (model) {
	return _elm_lang$core$Platform_Sub$none;
};
var _bsouthga$elm_katex$ExactBinomial$updateX = F2(
	function (txt, model) {
		var _p2 = model.n;
		if (_p2.ctor === 'Ok') {
			if (_elm_lang$core$String$isEmpty(txt)) {
				return _elm_lang$core$Native_Utils.update(
					model,
					{
						x: _elm_lang$core$Maybe$Nothing,
						xMsg: _elm_lang$core$Result$Err('Empty')
					});
			} else {
				var _p3 = _elm_lang$core$String$toInt(txt);
				if (_p3.ctor === 'Ok') {
					var _p4 = _p3._0;
					return ((_elm_lang$core$Native_Utils.cmp(0, _p4) < 1) && (_elm_lang$core$Native_Utils.cmp(_p4, _p2._0) < 1)) ? _elm_lang$core$Native_Utils.update(
						model,
						{
							x: _elm_lang$core$Maybe$Just(_p4),
							xMsg: _elm_lang$core$Result$Ok(_p4)
						}) : _elm_lang$core$Native_Utils.update(
						model,
						{
							x: _elm_lang$core$Maybe$Nothing,
							xMsg: _elm_lang$core$Result$Err('x must satisfy 0 <= x <= n')
						});
				} else {
					return _elm_lang$core$Native_Utils.update(
						model,
						{
							x: _elm_lang$core$Maybe$Nothing,
							xMsg: _elm_lang$core$Result$Err('X needs to be in integer')
						});
				}
			}
		} else {
			return model;
		}
	});
var _bsouthga$elm_katex$ExactBinomial$updateVal = F4(
	function (convert, validRange, errorMsg, txt) {
		var _p5 = convert(txt);
		if (_p5.ctor === 'Ok') {
			var _p6 = _p5._0;
			return validRange(_p6) ? _elm_lang$core$Result$Ok(_p6) : _elm_lang$core$Result$Err(errorMsg);
		} else {
			return _elm_lang$core$Result$Err(errorMsg);
		}
	});
var _bsouthga$elm_katex$ExactBinomial$updateN = F2(
	function (txt, model) {
		var maxN = 20000;
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				n: A4(
					_bsouthga$elm_katex$ExactBinomial$updateVal,
					_elm_lang$core$String$toInt,
					function (n) {
						return (_elm_lang$core$Native_Utils.cmp(n, 1) > 0) && (_elm_lang$core$Native_Utils.cmp(n, maxN) < 1);
					},
					A2(
						_elm_lang$core$Basics_ops['++'],
						'n needs to be a whole number with 1 < n <= ',
						_elm_lang$core$Basics$toString(maxN)),
					txt)
			});
	});
var _bsouthga$elm_katex$ExactBinomial$updateInner = F2(
	function (name, txt) {
		return A4(
			_bsouthga$elm_katex$ExactBinomial$updateVal,
			_elm_lang$core$String$toFloat,
			function (p) {
				return (_elm_lang$core$Native_Utils.cmp(p, 0) > -1) && (_elm_lang$core$Native_Utils.cmp(p, 1) < 1);
			},
			A2(_elm_lang$core$Basics_ops['++'], name, ' needs to be between 0 and 1 '),
			txt);
	});
var _bsouthga$elm_katex$ExactBinomial$updateP = F2(
	function (txt, model) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				p: A2(_bsouthga$elm_katex$ExactBinomial$updateInner, 'p', txt)
			});
	});
var _bsouthga$elm_katex$ExactBinomial$updateTail = F2(
	function (tail, model) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{tail: tail});
	});
var _bsouthga$elm_katex$ExactBinomial$twoTailLimits = F2(
	function (mean, value) {
		var valueF = _elm_lang$core$Basics$toFloat(value);
		var diff = (_elm_lang$core$Native_Utils.cmp(valueF, mean) < 1) ? (mean - valueF) : (valueF - mean);
		var maybeLower = _elm_lang$core$Basics$floor(mean - diff);
		var maybeUpper = _elm_lang$core$Basics$ceiling(mean + diff);
		var lower = (_elm_lang$core$Native_Utils.cmp(valueF, mean) < 1) ? value : maybeLower;
		var upper = (_elm_lang$core$Native_Utils.cmp(valueF, mean) < 1) ? maybeUpper : value;
		return {ctor: '_Tuple2', _0: lower, _1: upper};
	});
var _bsouthga$elm_katex$ExactBinomial$twoTail = F2(
	function (binom, x) {
		var _p7 = A2(_elm_lang$core$Dict$get, x, binom.twoPsDict);
		if (_p7.ctor === 'Just') {
			return A2(_elm_lang$core$Basics$min, 1, _p7._0);
		} else {
			return 0;
		}
	});
var _bsouthga$elm_katex$ExactBinomial$upperTail = F2(
	function (x, binom) {
		var _p8 = A2(_elm_lang$core$Dict$get, x, binom.upperPsDict);
		if (_p8.ctor === 'Just') {
			return _p8._0;
		} else {
			return (_elm_lang$core$Native_Utils.cmp(x, binom.minX) < 0) ? 1 : 0;
		}
	});
var _bsouthga$elm_katex$ExactBinomial$lowerTail = F2(
	function (x, binom) {
		var _p9 = A2(_elm_lang$core$Dict$get, x, binom.lowerPsDict);
		if (_p9.ctor === 'Just') {
			return _p9._0;
		} else {
			return (_elm_lang$core$Native_Utils.cmp(x, binom.minX) < 0) ? 0 : 1;
		}
	});
var _bsouthga$elm_katex$ExactBinomial$probX = F2(
	function (x, binom) {
		var _p10 = A2(_elm_lang$core$Dict$get, x, binom.psDict);
		if (_p10.ctor === 'Just') {
			return _p10._0;
		} else {
			return 0;
		}
	});
var _bsouthga$elm_katex$ExactBinomial$filterCol = F2(
	function (valToKeep, vals) {
		var pairs = A3(
			_elm_lang$core$List$map2,
			F2(
				function (v0, v1) {
					return {ctor: '_Tuple2', _0: v0, _1: v1};
				}),
			valToKeep,
			vals);
		var pairsToKeep = A2(
			_elm_lang$core$List$filter,
			function (tup) {
				return _elm_lang$core$Tuple$first(tup);
			},
			pairs);
		return A2(_elm_lang$core$List$map, _elm_lang$core$Tuple$second, pairsToKeep);
	});
var _bsouthga$elm_katex$ExactBinomial$addAndAppend = F2(
	function (n, acc) {
		var _p11 = _elm_lang$core$List$head(acc);
		if (_p11.ctor === 'Just') {
			return {ctor: '::', _0: n + _p11._0, _1: acc};
		} else {
			return {
				ctor: '::',
				_0: n,
				_1: {ctor: '[]'}
			};
		}
	});
var _bsouthga$elm_katex$ExactBinomial$cumultSuml = function (l) {
	return _elm_lang$core$List$reverse(
		A3(
			_elm_lang$core$List$foldl,
			_bsouthga$elm_katex$ExactBinomial$addAndAppend,
			{ctor: '[]'},
			l));
};
var _bsouthga$elm_katex$ExactBinomial$cumultSumr = function (l) {
	return A3(
		_elm_lang$core$List$foldr,
		_bsouthga$elm_katex$ExactBinomial$addAndAppend,
		{ctor: '[]'},
		l);
};
var _bsouthga$elm_katex$ExactBinomial$roundFloat = F2(
	function (digits, n) {
		var div = Math.pow(
			_elm_lang$core$Basics$toFloat(10),
			_elm_lang$core$Basics$toFloat(digits));
		var shifted = n * div;
		return function (n) {
			return n / div;
		}(
			_elm_lang$core$Basics$toFloat(
				_elm_lang$core$Basics$round(shifted)));
	});
var _bsouthga$elm_katex$ExactBinomial$probXString = F3(
	function (tail, x, binom) {
		var _p12 = x;
		if (_p12.ctor === 'Just') {
			var _p17 = _p12._0;
			var _p13 = A2(_bsouthga$elm_katex$ExactBinomial$twoTailLimits, binom.mean, _p17);
			var left = _p13._0;
			var right = _p13._1;
			var _p14 = {
				ctor: '_Tuple2',
				_0: _elm_lang$core$Basics$toString(left),
				_1: _elm_lang$core$Basics$toString(right)
			};
			var leftStr = _p14._0;
			var rightStr = _p14._1;
			var prob = A2(
				_bsouthga$elm_katex$ExactBinomial$roundFloat,
				3,
				function () {
					var _p15 = tail;
					switch (_p15.ctor) {
						case 'Left':
							return A2(_bsouthga$elm_katex$ExactBinomial$lowerTail, _p17, binom);
						case 'Right':
							return A2(_bsouthga$elm_katex$ExactBinomial$upperTail, _p17, binom);
						case 'Two':
							return A2(_bsouthga$elm_katex$ExactBinomial$twoTail, binom, _p17);
						default:
							return 0;
					}
				}());
			var probStr = _elm_lang$core$Basics$toString(prob);
			var xStr = _elm_lang$core$Basics$toString(_p17);
			var _p16 = tail;
			switch (_p16.ctor) {
				case 'Left':
					return A2(
						_elm_lang$core$Basics_ops['++'],
						'P(X \\le ',
						A2(
							_elm_lang$core$Basics_ops['++'],
							xStr,
							A2(_elm_lang$core$Basics_ops['++'], ') = ', probStr)));
				case 'Right':
					return A2(
						_elm_lang$core$Basics_ops['++'],
						'P(X \\ge ',
						A2(
							_elm_lang$core$Basics_ops['++'],
							xStr,
							A2(_elm_lang$core$Basics_ops['++'], ') = ', probStr)));
				case 'Two':
					return A2(
						_elm_lang$core$Basics_ops['++'],
						'P(X \\le ',
						A2(
							_elm_lang$core$Basics_ops['++'],
							leftStr,
							A2(
								_elm_lang$core$Basics_ops['++'],
								'\\text{ or }X \\ge',
								A2(
									_elm_lang$core$Basics_ops['++'],
									rightStr,
									A2(_elm_lang$core$Basics_ops['++'], ') = ', probStr)))));
				default:
					return '';
			}
		} else {
			return '';
		}
	});
var _bsouthga$elm_katex$ExactBinomial$displayXProb = function (model) {
	var maybeProbStr = A2(
		_elm_lang$core$Result$map,
		A2(_bsouthga$elm_katex$ExactBinomial$probXString, model.tail, model.x),
		model.binom);
	var _p18 = maybeProbStr;
	if (_p18.ctor === 'Ok') {
		return A2(
			_elm_lang$html$Html$div,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: _bsouthga$elm_katex$KaTeX$render(_p18._0),
				_1: {ctor: '[]'}
			});
	} else {
		return A2(
			_elm_lang$html$Html$div,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: _elm_lang$html$Html$text(''),
				_1: {ctor: '[]'}
			});
	}
};
var _bsouthga$elm_katex$ExactBinomial$newXs = function (n) {
	return A2(_elm_lang$core$List$range, 0, n);
};
var _bsouthga$elm_katex$ExactBinomial$binomLogCoefs = function (n) {
	var nums = A2(
		_elm_lang$core$List$map,
		_elm_lang$core$Basics$toFloat,
		A2(_elm_lang$core$List$range, 1, n));
	var logDiff = function (k) {
		return A2(
			_elm_lang$core$Basics$logBase,
			10,
			(_elm_lang$core$Basics$toFloat(n) - k) + 1) - A2(_elm_lang$core$Basics$logBase, 10, k);
	};
	var logDiffs = A2(_elm_lang$core$List$map, logDiff, nums);
	return A3(
		_elm_lang$core$List$scanl,
		F2(
			function (x, y) {
				return x + y;
			}),
		0,
		logDiffs);
};
var _bsouthga$elm_katex$ExactBinomial$newPs = F2(
	function (n, p) {
		var term = F2(
			function (logCoef, x) {
				return (logCoef + (x * A2(_elm_lang$core$Basics$logBase, 10, p))) + ((_elm_lang$core$Basics$toFloat(n) - x) * A2(_elm_lang$core$Basics$logBase, 10, 1 - p));
			});
		var binomceof = _bsouthga$elm_katex$ExactBinomial$binomLogCoefs(n);
		var xs = A2(
			_elm_lang$core$List$map,
			_elm_lang$core$Basics$toFloat,
			_bsouthga$elm_katex$ExactBinomial$newXs(n));
		var logProbs = A3(_elm_lang$core$List$map2, term, binomceof, xs);
		return A2(
			_elm_lang$core$List$map,
			function (logp) {
				return Math.pow(10, logp);
			},
			logProbs);
	});
var _bsouthga$elm_katex$ExactBinomial$binomLogCoef = F2(
	function (n, k) {
		var ks = A2(
			_elm_lang$core$List$map,
			_elm_lang$core$Basics$toFloat,
			A2(_elm_lang$core$List$range, 0, k - 1));
		var kF = _elm_lang$core$Basics$toFloat(k);
		var nF = _elm_lang$core$Basics$toFloat(n);
		var terms = A2(
			_elm_lang$core$List$map,
			function (i) {
				return A2(_elm_lang$core$Basics$logBase, 10, nF - i) - A2(_elm_lang$core$Basics$logBase, 10, kF - i);
			},
			ks);
		return _elm_lang$core$List$sum(terms);
	});
var _bsouthga$elm_katex$ExactBinomial$binomLogCoefRange = F3(
	function (n, start, stop) {
		var nums = A2(
			_elm_lang$core$List$map,
			_elm_lang$core$Basics$toFloat,
			A2(_elm_lang$core$List$range, start + 1, n));
		var logDiff = function (k) {
			return A2(
				_elm_lang$core$Basics$logBase,
				10,
				(_elm_lang$core$Basics$toFloat(n) - k) + 1) - A2(_elm_lang$core$Basics$logBase, 10, k);
		};
		var logDiffs = A2(_elm_lang$core$List$map, logDiff, nums);
		var coefStart = A2(_bsouthga$elm_katex$ExactBinomial$binomLogCoef, n, start);
		return A3(
			_elm_lang$core$List$scanl,
			F2(
				function (x, y) {
					return x + y;
				}),
			coefStart,
			logDiffs);
	});
var _bsouthga$elm_katex$ExactBinomial$probRange = F4(
	function (n, p, start, stop) {
		var term = F2(
			function (logCoef, x) {
				return (logCoef + (x * A2(_elm_lang$core$Basics$logBase, 10, p))) + ((_elm_lang$core$Basics$toFloat(n) - x) * A2(_elm_lang$core$Basics$logBase, 10, 1 - p));
			});
		var binomceof = A3(_bsouthga$elm_katex$ExactBinomial$binomLogCoefRange, n, start, stop);
		var xs = A2(
			_elm_lang$core$List$map,
			_elm_lang$core$Basics$toFloat,
			A2(_elm_lang$core$List$range, start, stop));
		var logProbs = A3(_elm_lang$core$List$map2, term, binomceof, xs);
		return A2(
			_elm_lang$core$List$map,
			function (logp) {
				return Math.pow(10, logp);
			},
			logProbs);
	});
var _bsouthga$elm_katex$ExactBinomial$meanBinom = F2(
	function (n, p) {
		return _elm_lang$core$Basics$toFloat(n) * p;
	});
var _bsouthga$elm_katex$ExactBinomial$sdBinom = F2(
	function (n, p) {
		return A2(_bsouthga$elm_katex$ExactBinomial$meanBinom, n, p) * Math.pow(1 - p, 0.5);
	});
var _bsouthga$elm_katex$ExactBinomial$elmToJS = _elm_lang$core$Native_Platform.outgoingPort(
	'elmToJS',
	function (v) {
		return v;
	});
var _bsouthga$elm_katex$ExactBinomial$makeCmd = function (model) {
	var _p19 = model.vegaSpec;
	if (_p19.ctor === 'Ok') {
		return _bsouthga$elm_katex$ExactBinomial$elmToJS(_p19._0);
	} else {
		return _elm_lang$core$Platform_Cmd$none;
	}
};
var _bsouthga$elm_katex$ExactBinomial$BinomModel = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return function (k) {
											return {xs: a, ps: b, psDict: c, lowerPsDict: d, upperPsDict: e, twoPsDict: f, n: g, p: h, mean: i, minX: j, maxX: k};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _bsouthga$elm_katex$ExactBinomial$binomDataRange = F4(
	function (n, p, start, stop) {
		var getTailProb = F2(
			function (dict, x) {
				var _p20 = A2(_elm_lang$core$Dict$get, x, dict);
				if (_p20.ctor === 'Just') {
					return _p20._0;
				} else {
					return 0;
				}
			});
		var mean = A2(_bsouthga$elm_katex$ExactBinomial$meanBinom, n, p);
		var ps = A4(_bsouthga$elm_katex$ExactBinomial$probRange, n, p, start, stop);
		var lowerPs = _bsouthga$elm_katex$ExactBinomial$cumultSuml(ps);
		var upperPs = _bsouthga$elm_katex$ExactBinomial$cumultSumr(ps);
		var xs = A2(_elm_lang$core$List$range, start, stop);
		var psDict = _elm_lang$core$Dict$fromList(
			A3(
				_elm_lang$core$List$map2,
				F2(
					function (v0, v1) {
						return {ctor: '_Tuple2', _0: v0, _1: v1};
					}),
				xs,
				ps));
		var lowerDict = _elm_lang$core$Dict$fromList(
			A3(
				_elm_lang$core$List$map2,
				F2(
					function (v0, v1) {
						return {ctor: '_Tuple2', _0: v0, _1: v1};
					}),
				xs,
				lowerPs));
		var upperDict = _elm_lang$core$Dict$fromList(
			A3(
				_elm_lang$core$List$map2,
				F2(
					function (v0, v1) {
						return {ctor: '_Tuple2', _0: v0, _1: v1};
					}),
				xs,
				upperPs));
		var _p21 = _elm_lang$core$List$unzip(
			A2(
				_elm_lang$core$List$map,
				_bsouthga$elm_katex$ExactBinomial$twoTailLimits(mean),
				xs));
		var lLim = _p21._0;
		var uLim = _p21._1;
		var leftTail = A2(
			_elm_lang$core$List$map,
			getTailProb(lowerDict),
			lLim);
		var rightTail = A2(
			_elm_lang$core$List$map,
			getTailProb(upperDict),
			uLim);
		var tailProbs = A3(
			_elm_lang$core$List$map2,
			F2(
				function (x, y) {
					return x + y;
				}),
			leftTail,
			rightTail);
		var twoDict = _elm_lang$core$Dict$fromList(
			A3(
				_elm_lang$core$List$map2,
				F2(
					function (v0, v1) {
						return {ctor: '_Tuple2', _0: v0, _1: v1};
					}),
				xs,
				tailProbs));
		return _bsouthga$elm_katex$ExactBinomial$BinomModel(xs)(ps)(psDict)(lowerDict)(upperDict)(twoDict)(n)(p)(mean)(start)(stop);
	});
var _bsouthga$elm_katex$ExactBinomial$binomDataTrimmed = F2(
	function (n, p) {
		var sd = A2(_bsouthga$elm_katex$ExactBinomial$sdBinom, n, p);
		var mean = A2(_bsouthga$elm_katex$ExactBinomial$meanBinom, n, p);
		var minX = A2(
			_elm_lang$core$Basics$max,
			0,
			_elm_lang$core$Basics$round(mean - (6 * sd)));
		var maxX = A2(
			_elm_lang$core$Basics$min,
			n,
			_elm_lang$core$Basics$round(mean + (6 * sd)));
		return A4(_bsouthga$elm_katex$ExactBinomial$binomDataRange, n, p, minX, maxX);
	});
var _bsouthga$elm_katex$ExactBinomial$updateBinom = function (model) {
	return _elm_lang$core$Native_Utils.update(
		model,
		{
			binom: A3(_elm_lang$core$Result$map2, _bsouthga$elm_katex$ExactBinomial$binomDataTrimmed, model.n, model.p)
		});
};
var _bsouthga$elm_katex$ExactBinomial$filterXs = F2(
	function (pred, binom) {
		var toKeep = A2(_elm_lang$core$List$map, pred, binom.xs);
		var newXs = A2(_bsouthga$elm_katex$ExactBinomial$filterCol, toKeep, binom.xs);
		var minX = function () {
			var _p22 = _elm_lang$core$List$minimum(newXs);
			if (_p22.ctor === 'Just') {
				return _p22._0;
			} else {
				return 0;
			}
		}();
		var maxX = function () {
			var _p23 = _elm_lang$core$List$maximum(newXs);
			if (_p23.ctor === 'Just') {
				return _p23._0;
			} else {
				return binom.n;
			}
		}();
		return A4(_bsouthga$elm_katex$ExactBinomial$binomDataRange, binom.n, binom.p, minX, maxX);
	});
var _bsouthga$elm_katex$ExactBinomial$removeTails = function (binom) {
	var $var = (_elm_lang$core$Basics$toFloat(binom.n) * binom.p) * (1.0 - binom.p);
	var sd = Math.pow($var, 0.5);
	var mean = _elm_lang$core$Basics$toFloat(binom.n) * binom.p;
	var minX = A2(_elm_lang$core$Basics$max, 0, mean - (4 * sd));
	var maxX = A2(
		_elm_lang$core$Basics$min,
		mean + (4 * sd),
		_elm_lang$core$Basics$toFloat(binom.n));
	var pred = function (x) {
		var xF = _elm_lang$core$Basics$toFloat(x);
		return (_elm_lang$core$Native_Utils.cmp(xF, minX) > -1) && (_elm_lang$core$Native_Utils.cmp(xF, maxX) < 1);
	};
	return A2(_bsouthga$elm_katex$ExactBinomial$filterXs, pred, binom);
};
var _bsouthga$elm_katex$ExactBinomial$spec = F3(
	function (tail, limit, fullBinom) {
		var selectedEnc = function (_p24) {
			return _gicentre$elm_vega$VegaLite$encoding(
				A3(
					_gicentre$elm_vega$VegaLite$position,
					_gicentre$elm_vega$VegaLite$X,
					{
						ctor: '::',
						_0: _gicentre$elm_vega$VegaLite$pName('X'),
						_1: {
							ctor: '::',
							_0: _gicentre$elm_vega$VegaLite$pMType(_gicentre$elm_vega$VegaLite$Quantitative),
							_1: {ctor: '[]'}
						}
					},
					A3(
						_gicentre$elm_vega$VegaLite$position,
						_gicentre$elm_vega$VegaLite$Y,
						{
							ctor: '::',
							_0: _gicentre$elm_vega$VegaLite$pName('P(X)'),
							_1: {
								ctor: '::',
								_0: _gicentre$elm_vega$VegaLite$pAggregate(_gicentre$elm_vega$VegaLite$Sum),
								_1: {
									ctor: '::',
									_0: _gicentre$elm_vega$VegaLite$pMType(_gicentre$elm_vega$VegaLite$Quantitative),
									_1: {ctor: '[]'}
								}
							}
						},
						A2(
							_gicentre$elm_vega$VegaLite$tooltips,
							{
								ctor: '::',
								_0: {
									ctor: '::',
									_0: _gicentre$elm_vega$VegaLite$tName('X'),
									_1: {
										ctor: '::',
										_0: _gicentre$elm_vega$VegaLite$tMType(_gicentre$elm_vega$VegaLite$Quantitative),
										_1: {ctor: '[]'}
									}
								},
								_1: {
									ctor: '::',
									_0: {
										ctor: '::',
										_0: _gicentre$elm_vega$VegaLite$tName('P(X)'),
										_1: {
											ctor: '::',
											_0: _gicentre$elm_vega$VegaLite$tMType(_gicentre$elm_vega$VegaLite$Quantitative),
											_1: {
												ctor: '::',
												_0: _gicentre$elm_vega$VegaLite$tFormat('.3f'),
												_1: {ctor: '[]'}
											}
										}
									},
									_1: {ctor: '[]'}
								}
							},
							A2(
								_gicentre$elm_vega$VegaLite$color,
								{
									ctor: '::',
									_0: _gicentre$elm_vega$VegaLite$mStr('red'),
									_1: {ctor: '[]'}
								},
								_p24)))));
		};
		var encPMF = function (_p25) {
			return _gicentre$elm_vega$VegaLite$encoding(
				A3(
					_gicentre$elm_vega$VegaLite$position,
					_gicentre$elm_vega$VegaLite$X,
					{
						ctor: '::',
						_0: _gicentre$elm_vega$VegaLite$pName('X'),
						_1: {
							ctor: '::',
							_0: _gicentre$elm_vega$VegaLite$pMType(_gicentre$elm_vega$VegaLite$Quantitative),
							_1: {ctor: '[]'}
						}
					},
					A3(
						_gicentre$elm_vega$VegaLite$position,
						_gicentre$elm_vega$VegaLite$Y,
						{
							ctor: '::',
							_0: _gicentre$elm_vega$VegaLite$pName('P(X)'),
							_1: {
								ctor: '::',
								_0: _gicentre$elm_vega$VegaLite$pAggregate(_gicentre$elm_vega$VegaLite$Sum),
								_1: {
									ctor: '::',
									_0: _gicentre$elm_vega$VegaLite$pMType(_gicentre$elm_vega$VegaLite$Quantitative),
									_1: {ctor: '[]'}
								}
							}
						},
						A2(
							_gicentre$elm_vega$VegaLite$tooltips,
							{
								ctor: '::',
								_0: {
									ctor: '::',
									_0: _gicentre$elm_vega$VegaLite$tName('X'),
									_1: {
										ctor: '::',
										_0: _gicentre$elm_vega$VegaLite$tMType(_gicentre$elm_vega$VegaLite$Quantitative),
										_1: {ctor: '[]'}
									}
								},
								_1: {
									ctor: '::',
									_0: {
										ctor: '::',
										_0: _gicentre$elm_vega$VegaLite$tName('P(X)'),
										_1: {
											ctor: '::',
											_0: _gicentre$elm_vega$VegaLite$tMType(_gicentre$elm_vega$VegaLite$Quantitative),
											_1: {
												ctor: '::',
												_0: _gicentre$elm_vega$VegaLite$tFormat('.3f'),
												_1: {ctor: '[]'}
											}
										}
									},
									_1: {ctor: '[]'}
								}
							},
							_p25))));
		};
		var binom = _bsouthga$elm_katex$ExactBinomial$removeTails(fullBinom);
		var d = function (_p26) {
			return A2(
				_gicentre$elm_vega$VegaLite$dataFromColumns,
				{ctor: '[]'},
				A3(
					_gicentre$elm_vega$VegaLite$dataColumn,
					'X',
					_gicentre$elm_vega$VegaLite$nums(
						A2(_elm_lang$core$List$map, _elm_lang$core$Basics$toFloat, binom.xs)),
					A3(
						_gicentre$elm_vega$VegaLite$dataColumn,
						'P(X)',
						_gicentre$elm_vega$VegaLite$nums(binom.ps),
						_p26)));
		};
		var mean = _elm_lang$core$Basics$toFloat(fullBinom.n) * fullBinom.p;
		var expr = function () {
			var _p27 = {ctor: '_Tuple2', _0: tail, _1: limit};
			if (_p27._1.ctor === 'Nothing') {
				return 'false';
			} else {
				switch (_p27._0.ctor) {
					case 'None':
						return 'false';
					case 'Left':
						return A2(
							_elm_lang$core$Basics_ops['++'],
							'datum.X <= ',
							_elm_lang$core$Basics$toString(_p27._1._0));
					case 'Right':
						return A2(
							_elm_lang$core$Basics_ops['++'],
							'datum.X >= ',
							_elm_lang$core$Basics$toString(_p27._1._0));
					default:
						var _p29 = _p27._1._0;
						var _p28 = A2(_bsouthga$elm_katex$ExactBinomial$twoTailLimits, mean, _p29);
						var lower = _p28._0;
						var upper = _p28._1;
						return _elm_lang$core$Native_Utils.eq(
							mean,
							_elm_lang$core$Basics$toFloat(_p29)) ? 'true' : A2(
							_elm_lang$core$Basics_ops['++'],
							'datum.X <= ',
							A2(
								_elm_lang$core$Basics_ops['++'],
								_elm_lang$core$Basics$toString(lower),
								A2(
									_elm_lang$core$Basics_ops['++'],
									' || ',
									A2(
										_elm_lang$core$Basics_ops['++'],
										'datum.X >= ',
										_elm_lang$core$Basics$toString(upper)))));
				}
			}
		}();
		var trans = function (_p30) {
			return _gicentre$elm_vega$VegaLite$transform(
				A2(
					_gicentre$elm_vega$VegaLite$filter,
					_gicentre$elm_vega$VegaLite$fiExpr(expr),
					_p30));
		};
		return _gicentre$elm_vega$VegaLite$toVegaLite(
			{
				ctor: '::',
				_0: _gicentre$elm_vega$VegaLite$width(600),
				_1: {
					ctor: '::',
					_0: _gicentre$elm_vega$VegaLite$height(400),
					_1: {
						ctor: '::',
						_0: d(
							{ctor: '[]'}),
						_1: {
							ctor: '::',
							_0: _gicentre$elm_vega$VegaLite$layer(
								{
									ctor: '::',
									_0: _gicentre$elm_vega$VegaLite$asSpec(
										{
											ctor: '::',
											_0: _gicentre$elm_vega$VegaLite$bar(
												{ctor: '[]'}),
											_1: {
												ctor: '::',
												_0: encPMF(
													{ctor: '[]'}),
												_1: {ctor: '[]'}
											}
										}),
									_1: {
										ctor: '::',
										_0: _gicentre$elm_vega$VegaLite$asSpec(
											{
												ctor: '::',
												_0: _gicentre$elm_vega$VegaLite$bar(
													{ctor: '[]'}),
												_1: {
													ctor: '::',
													_0: selectedEnc(
														{ctor: '[]'}),
													_1: {
														ctor: '::',
														_0: trans(
															{ctor: '[]'}),
														_1: {ctor: '[]'}
													}
												}
											}),
										_1: {ctor: '[]'}
									}
								}),
							_1: {ctor: '[]'}
						}
					}
				}
			});
	});
var _bsouthga$elm_katex$ExactBinomial$updateSpec = function (model) {
	return _elm_lang$core$Native_Utils.update(
		model,
		{
			vegaSpec: A2(
				_elm_lang$core$Result$map,
				A2(_bsouthga$elm_katex$ExactBinomial$spec, model.tail, model.x),
				model.binom)
		});
};
var _bsouthga$elm_katex$ExactBinomial$update = F2(
	function (msg, model) {
		var _p31 = msg;
		switch (_p31.ctor) {
			case 'ChangeTail':
				var new_model = _bsouthga$elm_katex$ExactBinomial$updateSpec(
					A2(_bsouthga$elm_katex$ExactBinomial$updateTail, _p31._0, model));
				return {
					ctor: '_Tuple2',
					_0: new_model,
					_1: _bsouthga$elm_katex$ExactBinomial$makeCmd(new_model)
				};
			case 'ChangeN':
				var new_model = _bsouthga$elm_katex$ExactBinomial$updateSpec(
					_bsouthga$elm_katex$ExactBinomial$updateBinom(
						A2(
							_bsouthga$elm_katex$ExactBinomial$updateX,
							'',
							A2(_bsouthga$elm_katex$ExactBinomial$updateN, _p31._0, model))));
				return {
					ctor: '_Tuple2',
					_0: new_model,
					_1: _bsouthga$elm_katex$ExactBinomial$makeCmd(new_model)
				};
			case 'ChangeP':
				var new_model = _bsouthga$elm_katex$ExactBinomial$updateSpec(
					_bsouthga$elm_katex$ExactBinomial$updateBinom(
						A2(
							_bsouthga$elm_katex$ExactBinomial$updateX,
							'',
							A2(_bsouthga$elm_katex$ExactBinomial$updateP, _p31._0, model))));
				return {
					ctor: '_Tuple2',
					_0: new_model,
					_1: _bsouthga$elm_katex$ExactBinomial$makeCmd(new_model)
				};
			default:
				var new_model = _bsouthga$elm_katex$ExactBinomial$updateSpec(
					A2(_bsouthga$elm_katex$ExactBinomial$updateX, _p31._0, model));
				return {
					ctor: '_Tuple2',
					_0: new_model,
					_1: _bsouthga$elm_katex$ExactBinomial$makeCmd(new_model)
				};
		}
	});
var _bsouthga$elm_katex$ExactBinomial$Model = F7(
	function (a, b, c, d, e, f, g) {
		return {n: a, p: b, x: c, xMsg: d, tail: e, binom: f, vegaSpec: g};
	});
var _bsouthga$elm_katex$ExactBinomial$None = {ctor: 'None'};
var _bsouthga$elm_katex$ExactBinomial$model = function () {
	var tail = _bsouthga$elm_katex$ExactBinomial$None;
	var xMsg = _elm_lang$core$Result$Err('Empty');
	var x = _elm_lang$core$Maybe$Nothing;
	var p = _elm_lang$core$String$toFloat('0.5');
	var n = _elm_lang$core$String$toInt('10');
	var binom = A3(_elm_lang$core$Result$map2, _bsouthga$elm_katex$ExactBinomial$binomDataTrimmed, n, p);
	var vegaSpec = A2(
		_elm_lang$core$Result$map,
		A2(_bsouthga$elm_katex$ExactBinomial$spec, tail, x),
		binom);
	return A7(_bsouthga$elm_katex$ExactBinomial$Model, n, p, x, xMsg, tail, binom, vegaSpec);
}();
var _bsouthga$elm_katex$ExactBinomial$init = {
	ctor: '_Tuple2',
	_0: _bsouthga$elm_katex$ExactBinomial$model,
	_1: _bsouthga$elm_katex$ExactBinomial$makeCmd(_bsouthga$elm_katex$ExactBinomial$model)
};
var _bsouthga$elm_katex$ExactBinomial$Two = {ctor: 'Two'};
var _bsouthga$elm_katex$ExactBinomial$Right = {ctor: 'Right'};
var _bsouthga$elm_katex$ExactBinomial$Left = {ctor: 'Left'};
var _bsouthga$elm_katex$ExactBinomial$ChangeSearch = function (a) {
	return {ctor: 'ChangeSearch', _0: a};
};
var _bsouthga$elm_katex$ExactBinomial$inputX = function (model) {
	var _p32 = model.x;
	if (_p32.ctor === 'Just') {
		return A4(
			_bsouthga$elm_katex$ExactBinomial$inputGroup,
			'x',
			_elm_lang$core$Basics$toString(_p32._0),
			_bsouthga$elm_katex$ExactBinomial$ChangeSearch,
			'x = ');
	} else {
		return A4(_bsouthga$elm_katex$ExactBinomial$inputGroup, 'x', '', _bsouthga$elm_katex$ExactBinomial$ChangeSearch, 'x = ');
	}
};
var _bsouthga$elm_katex$ExactBinomial$ChangeTail = function (a) {
	return {ctor: 'ChangeTail', _0: a};
};
var _bsouthga$elm_katex$ExactBinomial$ChangeP = function (a) {
	return {ctor: 'ChangeP', _0: a};
};
var _bsouthga$elm_katex$ExactBinomial$ChangeN = function (a) {
	return {ctor: 'ChangeN', _0: a};
};
var _bsouthga$elm_katex$ExactBinomial$sidebar = function (model) {
	return A2(
		_elm_lang$html$Html$div,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$h3,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('Exact Binomial Probability'),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: A4(_bsouthga$elm_katex$ExactBinomial$inputGroup, 'Sample Size', '10', _bsouthga$elm_katex$ExactBinomial$ChangeN, 'n = '),
				_1: {
					ctor: '::',
					_0: _bsouthga$elm_katex$ExactBinomial$outputVal(model.n),
					_1: {
						ctor: '::',
						_0: A4(_bsouthga$elm_katex$ExactBinomial$inputGroup, 'probability', '0.5', _bsouthga$elm_katex$ExactBinomial$ChangeP, 'p = '),
						_1: {
							ctor: '::',
							_0: _bsouthga$elm_katex$ExactBinomial$outputVal(model.p),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$br,
									{ctor: '[]'},
									{ctor: '[]'}),
								_1: {
									ctor: '::',
									_0: A2(
										_rundis$elm_bootstrap$Bootstrap_ButtonGroup$radioButtonGroup,
										{ctor: '[]'},
										{
											ctor: '::',
											_0: A3(
												_rundis$elm_bootstrap$Bootstrap_ButtonGroup$radioButton,
												_elm_lang$core$Native_Utils.eq(model.tail, _bsouthga$elm_katex$ExactBinomial$Left),
												{
													ctor: '::',
													_0: _rundis$elm_bootstrap$Bootstrap_Button$primary,
													_1: {
														ctor: '::',
														_0: _rundis$elm_bootstrap$Bootstrap_Button$onClick(
															_bsouthga$elm_katex$ExactBinomial$ChangeTail(_bsouthga$elm_katex$ExactBinomial$Left)),
														_1: {ctor: '[]'}
													}
												},
												{
													ctor: '::',
													_0: _elm_lang$html$Html$text('Left-tail'),
													_1: {ctor: '[]'}
												}),
											_1: {
												ctor: '::',
												_0: A3(
													_rundis$elm_bootstrap$Bootstrap_ButtonGroup$radioButton,
													_elm_lang$core$Native_Utils.eq(model.tail, _bsouthga$elm_katex$ExactBinomial$Right),
													{
														ctor: '::',
														_0: _rundis$elm_bootstrap$Bootstrap_Button$primary,
														_1: {
															ctor: '::',
															_0: _rundis$elm_bootstrap$Bootstrap_Button$onClick(
																_bsouthga$elm_katex$ExactBinomial$ChangeTail(_bsouthga$elm_katex$ExactBinomial$Right)),
															_1: {ctor: '[]'}
														}
													},
													{
														ctor: '::',
														_0: _elm_lang$html$Html$text('Right-tail'),
														_1: {ctor: '[]'}
													}),
												_1: {
													ctor: '::',
													_0: A3(
														_rundis$elm_bootstrap$Bootstrap_ButtonGroup$radioButton,
														_elm_lang$core$Native_Utils.eq(model.tail, _bsouthga$elm_katex$ExactBinomial$Two),
														{
															ctor: '::',
															_0: _rundis$elm_bootstrap$Bootstrap_Button$primary,
															_1: {
																ctor: '::',
																_0: _rundis$elm_bootstrap$Bootstrap_Button$onClick(
																	_bsouthga$elm_katex$ExactBinomial$ChangeTail(_bsouthga$elm_katex$ExactBinomial$Two)),
																_1: {ctor: '[]'}
															}
														},
														{
															ctor: '::',
															_0: _elm_lang$html$Html$text('Two-tail'),
															_1: {ctor: '[]'}
														}),
													_1: {ctor: '[]'}
												}
											}
										}),
									_1: {
										ctor: '::',
										_0: _bsouthga$elm_katex$ExactBinomial$inputX(model),
										_1: {
											ctor: '::',
											_0: _bsouthga$elm_katex$ExactBinomial$outputX(model.xMsg),
											_1: {
												ctor: '::',
												_0: A2(
													_elm_lang$html$Html$br,
													{ctor: '[]'},
													{ctor: '[]'}),
												_1: {
													ctor: '::',
													_0: A2(
														_elm_lang$html$Html$br,
														{ctor: '[]'},
														{ctor: '[]'}),
													_1: {
														ctor: '::',
														_0: A2(
															_elm_lang$html$Html$h4,
															{ctor: '[]'},
															{
																ctor: '::',
																_0: _elm_lang$html$Html$text('Probability'),
																_1: {ctor: '[]'}
															}),
														_1: {
															ctor: '::',
															_0: _bsouthga$elm_katex$ExactBinomial$displayXProb(model),
															_1: {ctor: '[]'}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		});
};
var _bsouthga$elm_katex$ExactBinomial$view = function (model) {
	return A2(
		_elm_lang$html$Html$div,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_rundis$elm_bootstrap$Bootstrap_Grid$container,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: A2(
						_rundis$elm_bootstrap$Bootstrap_Grid$row,
						{ctor: '[]'},
						{
							ctor: '::',
							_0: A2(
								_rundis$elm_bootstrap$Bootstrap_Grid$col,
								{
									ctor: '::',
									_0: _rundis$elm_bootstrap$Bootstrap_Grid_Col$sm4,
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _bsouthga$elm_katex$ExactBinomial$sidebar(model),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: A2(
									_rundis$elm_bootstrap$Bootstrap_Grid$col,
									{
										ctor: '::',
										_0: _rundis$elm_bootstrap$Bootstrap_Grid_Col$md8,
										_1: {
											ctor: '::',
											_0: _rundis$elm_bootstrap$Bootstrap_Grid_Col$sm8,
											_1: {ctor: '[]'}
										}
									},
									{
										ctor: '::',
										_0: A2(
											_elm_lang$html$Html$div,
											{
												ctor: '::',
												_0: _elm_lang$html$Html_Attributes$id('vis'),
												_1: {ctor: '[]'}
											},
											{ctor: '[]'}),
										_1: {ctor: '[]'}
									}),
								_1: {ctor: '[]'}
							}
						}),
					_1: {ctor: '[]'}
				}),
			_1: {ctor: '[]'}
		});
};
var _bsouthga$elm_katex$ExactBinomial$main = _elm_lang$html$Html$program(
	{init: _bsouthga$elm_katex$ExactBinomial$init, view: _bsouthga$elm_katex$ExactBinomial$view, update: _bsouthga$elm_katex$ExactBinomial$update, subscriptions: _bsouthga$elm_katex$ExactBinomial$subscriptions})();

var Elm = {};
Elm['ExactBinomial'] = Elm['ExactBinomial'] || {};
if (typeof _bsouthga$elm_katex$ExactBinomial$main !== 'undefined') {
    _bsouthga$elm_katex$ExactBinomial$main(Elm['ExactBinomial'], 'ExactBinomial', undefined);
}

if (typeof define === "function" && define['amd'])
{
  define([], function() { return Elm; });
  return;
}

if (typeof module === "object")
{
  module['exports'] = Elm;
  return;
}

var globalElm = this['Elm'];
if (typeof globalElm === "undefined")
{
  this['Elm'] = Elm;
  return;
}

for (var publicModule in Elm)
{
  if (publicModule in globalElm)
  {
    throw new Error('There are two Elm modules called `' + publicModule + '` on this page! Rename one of them.');
  }
  globalElm[publicModule] = Elm[publicModule];
}

}).call(this);

