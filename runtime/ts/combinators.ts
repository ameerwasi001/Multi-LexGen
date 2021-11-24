import {Token} from "./token.js"

function fst<A, B>([a, _]: [A, B]){ return a }
function snd<A, B>([_, b]: [A, B]){ return b }

const literalParse = (expected: string | null, inp: [string[], string]): [string[], string] => {
    let matchStr = ""
    let i = 0
    let [first, second] = inp

    if (expected != null){
        if (expected.length > second.length) throw `cannot match ${second} with ${expected}`
        while (i < expected.length){
            const character = second[0]
            second = second.substring(1)
            matchStr = `${matchStr}${character}`
            i += 1
        }
    }
    if (matchStr == expected) return [first.concat([matchStr]), second]
    else throw `expected ${expected}, found ${matchStr}`
}

const binOp = (left: (a: [string[], string]) => [string[], string], op: "|" | ">", right: (a: [string[], string]) => [string[], string], inp: [string[], string]): [string[], string] => {
    if (op === "|"){
        try {return left(inp)}
        catch {return right(inp)}
    } else if (op === ">") return right(left(inp))
    else throw `This should not have happend, I expected | or > but got ${op}`
}

const unaryOp = (op: "*" | "+" | "?", operand: (a: [string[], string]) => [string[], string], inp: [string[], string]): [string[], string] => {
    if (op === "*" || op === "+"){
        let output: [string[], string] = [fst(inp).concat([]), snd(inp)]
        try {
            output = operand(inp)
        } catch(err) {
            if (op === "+") throw err
            return inp
        }
        while (true){
            try {
                output = operand(output)
            } catch {
                return output
            }
        }
    } else if (op === "?"){
        try {
            return operand(inp)
        } catch {
            return inp
        }
    } else throw `This should not have happend, I expected ?, *, +, but got ${op}`
}

function tokensFromRules(ruleset: [string, (_: [string[], string]) => [string[], string]][], inp: [string[], string]): Token[] {
    const generated_toks: Token[] = []
    let next_inp = inp
    while (snd(next_inp) != ""){
        let i = 0
        let matched = false
        while (i < ruleset.length){
            let [tok, rule] = ruleset[i]
            
            try{
                next_inp = rule([[], snd(next_inp)])
                matched = true
            } catch {
                i+=1
                continue
            }
            const x = new Token(tok, fst(next_inp).join(""))
            generated_toks.push(x)
            i += 1
        }
        if (!matched) throw `Unmatched text \"${snd(next_inp)[0] === "\n" ? snd(next_inp).split("\n")[1] : snd(next_inp).split("\n")[0]}\"`
    }
    return generated_toks
}

function modifyTokenByCondition(toks: Token[], modfiers: Record<string, ((_: Token) => string | null)[]>){
    const modedToks = []
    for (const tok of toks){
        const mods = modfiers[tok.type_]
        let new_tok = tok
        let should_add = true
        if (mods){
            for (const mod of mods){
                const mod_res = mod(new_tok)
                if (mod_res !== null){
                    if (mod_res == "_") should_add = false
                    new_tok = new Token(mod_res, new_tok.val)
                    break
                }
            }
        }
        if (should_add) modedToks.push(new_tok)
    }
    return modedToks
}

function digit(a: [string[], string]) {
    return binOp(
        (a: [string[], string]) => literalParse("1", a), 
        "|", 
        (a: [string[], string]) => binOp(
            (a: [string[], string]) => literalParse("2", a),
            "|",
            (a: [string[], string]) => binOp(
                (a: [string[], string]) => literalParse("3", a),
                "|",
                (a: [string[], string]) => literalParse("9", a),
                a
            ),
            a
        ), 
        a
    )
}


function num(a: [string[], string]) { return unaryOp("+", a => digit(a), a) }
function decimal(_x: [string[], string]) { return binOp(_x => num(_x), ">", _x => unaryOp("?", _x => binOp(_x => literalParse(".", _x), ">", _x => num(_x), _x), _x), _x) }
function plus(_x: [string[], string]) { return literalParse("+", _x) }
function minus(_x: [string[], string]) { return literalParse("-", _x) }
function div(_x: [string[], string]) { return literalParse("/", _x) }
function mul(_x: [string[], string]) { return literalParse("*", _x) }

const out = (_x: [string[], string]) => tokensFromRules([["num", decimal], ["plus", plus], ["minus", minus], ["div", div], ["mul", mul]], _x)
const moded = (_x: [string[], string]) => {
    const res = out(_x)
    const modifiers: Record<string, ((tok: Token) => string | null)[]> = {
        "num": [(tok: Token) => tok.val.includes(".") ? "decimal" : null]
    }
    return modifyTokenByCondition(res, modifiers)
}

const inp: [string[], string] = [[], "1291.31+11*2.3"]
for (const tok of moded(inp)) console.log(tok.toString())