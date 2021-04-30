from runtime.python.basicTokenClass import Token
from lexer import getToks

for x in getToks(Token, "if b then iffer+0+mx+9.2-192.168.0.1/1.2-MxC*oz2 else 0.0.0.0+72.98*10-PI"): print(x)