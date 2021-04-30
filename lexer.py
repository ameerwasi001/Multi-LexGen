from runtime.python.combinators import bin_op, unary_op, literal_parse, tokensFromRules, modifyTokenByCondition
from runtime.python.errors import raiseInputError
import runtime.python.lexerHelpers

digit = lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: literal_parse("1", _x), "|", lambda _x: literal_parse("2", _x), _x), "|", lambda _x: literal_parse("3", _x), _x), "|", lambda _x: literal_parse("4", _x), _x), "|", lambda _x: literal_parse("5", _x), _x), "|", lambda _x: literal_parse("6", _x), _x), "|", lambda _x: literal_parse("7", _x), _x), "|", lambda _x: literal_parse("8", _x), _x), "|", lambda _x: literal_parse("9", _x), _x), "|", lambda _x: literal_parse("0", _x), _x)
lowerAlpha = lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: literal_parse("a", _x), "|", lambda _x: literal_parse("b", _x), _x), "|", lambda _x: literal_parse("c", _x), _x), "|", lambda _x: literal_parse("d", _x), _x), "|", lambda _x: literal_parse("e", _x), _x), "|", lambda _x: literal_parse("f", _x), _x), "|", lambda _x: literal_parse("g", _x), _x), "|", lambda _x: literal_parse("h", _x), _x), "|", lambda _x: literal_parse("i", _x), _x), "|", lambda _x: literal_parse("j", _x), _x), "|", lambda _x: literal_parse("k", _x), _x), "|", lambda _x: literal_parse("l", _x), _x), "|", lambda _x: literal_parse("m", _x), _x), "|", lambda _x: literal_parse("n", _x), _x), "|", lambda _x: literal_parse("o", _x), _x), "|", lambda _x: literal_parse("p", _x), _x), "|", lambda _x: literal_parse("q", _x), _x), "|", lambda _x: literal_parse("r", _x), _x), "|", lambda _x: literal_parse("s", _x), _x), "|", lambda _x: literal_parse("t", _x), _x), "|", lambda _x: literal_parse("u", _x), _x), "|", lambda _x: literal_parse("v", _x), _x), "|", lambda _x: literal_parse("w", _x), _x), "|", lambda _x: literal_parse("x", _x), _x), "|", lambda _x: literal_parse("y", _x), _x), "|", lambda _x: literal_parse("z", _x), _x)
upperAlpha = lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: bin_op(lambda _x: literal_parse("A", _x), "|", lambda _x: literal_parse("B", _x), _x), "|", lambda _x: literal_parse("C", _x), _x), "|", lambda _x: literal_parse("D", _x), _x), "|", lambda _x: literal_parse("E", _x), _x), "|", lambda _x: literal_parse("F", _x), _x), "|", lambda _x: literal_parse("G", _x), _x), "|", lambda _x: literal_parse("H", _x), _x), "|", lambda _x: literal_parse("I", _x), _x), "|", lambda _x: literal_parse("J", _x), _x), "|", lambda _x: literal_parse("K", _x), _x), "|", lambda _x: literal_parse("L", _x), _x), "|", lambda _x: literal_parse("M", _x), _x), "|", lambda _x: literal_parse("N", _x), _x), "|", lambda _x: literal_parse("O", _x), _x), "|", lambda _x: literal_parse("P", _x), _x), "|", lambda _x: literal_parse("Q", _x), _x), "|", lambda _x: literal_parse("R", _x), _x), "|", lambda _x: literal_parse("S", _x), _x), "|", lambda _x: literal_parse("T", _x), _x), "|", lambda _x: literal_parse("U", _x), _x), "|", lambda _x: literal_parse("V", _x), _x), "|", lambda _x: literal_parse("W", _x), _x), "|", lambda _x: literal_parse("X", _x), _x), "|", lambda _x: literal_parse("Y", _x), _x), "|", lambda _x: literal_parse("Z", _x), _x)
alpha = lambda _x: bin_op(lambda _x: lowerAlpha(_x), "|", lambda _x: upperAlpha(_x), _x)
keyword = lambda _x: bin_op(lambda _x: bin_op(lambda _x: literal_parse("if", _x), "|", lambda _x: literal_parse("then", _x), _x), "|", lambda _x: literal_parse("else", _x), _x)
whitespace = lambda _x: bin_op(lambda _x: bin_op(lambda _x: literal_parse("\t", _x), "|", lambda _x: literal_parse("\r", _x), _x), "|", lambda _x: literal_parse(" ", _x), _x)
ident = lambda _x: bin_op(lambda _x: alpha(_x), ">", lambda _x: unary_op("*", lambda _x: bin_op(lambda _x: alpha(_x), "|", lambda _x: digit(_x), _x), _x), _x)
num = lambda _x: unary_op("+", lambda _x: digit(_x), _x)
decimal = lambda _x: bin_op(lambda _x: num(_x), ">", lambda _x: unary_op("*", lambda _x: bin_op(lambda _x: literal_parse(".", _x), ">", lambda _x: num(_x), _x), _x), _x)

getUnmodifiedToks = lambda tok_class, _x: tokensFromRules(tok_class, [("NUM", lambda _x: decimal(_x)), ("WHITESPACE", lambda _x: whitespace(_x)), ("IDENT", lambda _x: ident(_x)), ("PLUS", lambda _x: literal_parse("+", _x)), ("MINUS", lambda _x: literal_parse("-", _x)), ("MUL", lambda _x: literal_parse("*", _x)), ("DIV", lambda _x: literal_parse("/", _x))], _x)

getToks = lambda tok_class, _x: modifyTokenByCondition(tok_class, getUnmodifiedToks(tok_class, ([], _x)), {"IDENT": [lambda t: "KEYWORD" if runtime.python.lexerHelpers.fullyApplies(t, lambda _x: keyword(_x)) else None, lambda t: "UPPERIDENT" if runtime.python.lexerHelpers.isUpper(t, 0) else None], "NUM": [lambda t: "FLOAT" if runtime.python.lexerHelpers.contains(t, ".", 1) else None, lambda t: "IPV4" if runtime.python.lexerHelpers.contains(t, ".", 3) else None, lambda t: "INT" if runtime.python.lexerHelpers.contains(t, ".", 0) else None, lambda t: raiseInputError("A number must contain either 1, 0, or 3 dots") if True else None], "WHITESPACE": [lambda t: "_" if True else None]})