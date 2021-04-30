from .errors import InputError

def fst(tup):
    (f, _) = tup
    return f

def snd(tup):
    (_, s) = tup
    return s

def literal_parse(expected, inp):
    match_str = ""
    i = 0
    first, second = inp

    if expected != None:
        if len(expected) > len(second):
            raise InputError("cannot match {} with {}".format(second, expected))

        while i < len(expected):
            character = second[0]
            second = second[1:]
            match_str = "{}{}".format(match_str, character)
            i+=1
    if expected == None and second == "":
        return (first + [match_str], second)
    elif match_str == expected:
        return (first + [match_str], second)
    else:
        raise InputError("expected {}, found {}".format(expected, match_str))

def bin_op(left, op, right, inp):
    if op == "|":
        try:
            return left(inp)
        except InputError:
            return right(inp)
    elif op == ">":
        return right(left(inp))
    raise RuntimeError("This should not have happend, I expected {} but got {}", "| or >", op)

def unary_op(op, operand, inp):
    if op == "*" or op == "+":
        try:
            output = operand(inp)
        except InputError as err:
            if op == "+": raise err
            return inp
        while True:
            try:
                output = operand(output)
            except InputError:
                return output
    elif op == "?":
        try:
            return operand(inp)
        except InputError:
            return inp
    raise RuntimeError("Can't understand unary operator {}".format(op))

def tokensFromRules(tok_class, ruleset, inp):
    generated_toks = []
    next_inp = inp
    while snd(next_inp) != "":
        i = 0
        matched = False
        while i < len(ruleset):
            tok, rule = ruleset[i]
            
            try:
                next_inp = rule(([], snd(next_inp)))
                matched = True
            except InputError:
                i+=1
                continue
            
            x = tok_class(tok, "".join(fst(next_inp)))
            generated_toks.append(x)
            i += 1
        if not matched: raise InputError("Unmatched text \"{}\"".format(snd(next_inp).split("\n")[1] if snd(next_inp)[0] == "\n" else snd(next_inp).split("\n")[0]))
    return generated_toks

def modifyTokenByCondition(tok_class, toks, modfiers):
    moded_toks = []
    for tok in toks:
        mods = modfiers.get(tok.type)
        new_tok = tok
        should_add = True
        if mods:
            for mod in mods:
                mod_res = mod(new_tok)
                if mod_res != None:
                    if mod_res == "_": should_add = False
                    new_tok = tok_class(mod_res, new_tok.val)
                    break
        if should_add: moded_toks.append(new_tok)
    return moded_toks

# if typ == tok.type and mod.get(tok.type):
#     moded_toks.append(tok_class(mod(tok), tok.val))
# else:
#     moded_toks.append(tok_class(tok.type, tok.val))

# inp = ([], "really really really really really good fan")
inp = ([], "1291.31+11*2.3")

# adj = lambda _x: bin_op(lambda _x: literal_parse("nice ", _x), "|", lambda _x: literal_parse("good ", _x), _x)
# out = lambda _x: bin_op(lambda _x: bin_op(lambda _x: adj(_x), ">", lambda _x: literal_parse("fan", _x), _x), "|", lambda _x: bin_op(lambda _x: literal_parse("really ", _x), ">", lambda _x: out(_x), _x), _x)

# out = lambda _x: bin_op(lambda _x: bin_op(lambda _x: unary_op("+", lambda _x: literal_parse("really ", _x), _x), ">", lambda _x: unary_op("*", lambda _x: adj(_x), _x), _x), ">", lambda _x: literal_parse("fan", _x), _x)

digit = lambda _x: bin_op(
    lambda _x: literal_parse("1", _x), 
    "|", 
    lambda _x: bin_op(
        lambda _x: literal_parse("2", _x),
        "|",
        lambda _x: bin_op(
            lambda _x: literal_parse("3", _x),
            "|",
            lambda _x: literal_parse("9", _x),
            _x
        ),
        _x
    ), 
    _x
)

num = lambda _x: unary_op("+", lambda _x: digit(_x), _x)
decimal = lambda _x: bin_op(lambda _x: num(_x), ">", lambda _x: unary_op("?", lambda _x: bin_op(lambda _x: literal_parse(".", _x), ">", lambda _x: num(_x), _x), _x), _x)
plus = lambda _x: literal_parse("+", _x)
minus = lambda _x: literal_parse("-", _x)
div = lambda _x: literal_parse("+", _x)
mul = lambda _x: literal_parse("*", _x)

# out = lambda _x: tokensFromRules(Token, [("num", decimal), ("plus", plus), ("minus", minus), ("div", div), ("mul", mul)], _x)

# for tok in out(inp):
#     print(tok)