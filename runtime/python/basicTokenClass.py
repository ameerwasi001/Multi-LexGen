class Token:
    def __init__(self, tok, val=""):
        self.type = tok
        self.val = val
    
    def __str__(self):
        return f"[{self.type}:{self.val}]"

    def repr(self):
        return f"[{self.type}:{self.val}]"