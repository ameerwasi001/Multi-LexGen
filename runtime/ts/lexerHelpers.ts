import {Token} from "./token.js"

const occurrences = (string: string, subString: string, allowOverlapping=false) => {
    string += "";
    subString += "";
    if (subString.length <= 0) return (string.length + 1);

    var n = 0,
        pos = 0,
        step = allowOverlapping ? 1 : subString.length;

    while (true) {
        pos = string.indexOf(subString, pos);
        if (pos >= 0) {
            ++n;
            pos += step;
        } else break;
    }
    return n;
}

const contains = (t: Token, str: string, times: number) => occurrences(t.val, str) === times
const isUpper = (t: Token, i: number) => t.val[i].isupper()
const fullyApplies = (t: Token, r: (_: [string[], string]) => [string[], string]) => {
    try {
        const [_, rest] = r([[], t.val])
        return rest == ""
    } catch { return false }
}
const applies = (t: Token, r: (_: [string[], string]) => [string[], string]) => {
    try {
        r([[], t.val])
        return true
    } catch { return false }
}

export {contains, isUpper, fullyApplies, applies}