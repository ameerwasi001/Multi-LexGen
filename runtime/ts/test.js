import {getToks} from "./lexer.js"

for(const x of getToks("if b then iffer+0+mx+9.2-192.168.0.1/1.2-MxC*oz2 else 0.0.0.0+72.98*10-PI")) console.log(x.toString())