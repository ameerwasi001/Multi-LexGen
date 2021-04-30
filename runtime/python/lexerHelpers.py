from .errors import InputError

def contains(t, string, times): return t.val.count(string) == times
def isUpper(t, i): return t.val[i].isupper()
def fullyApplies(t, r): 
    try:
        _, rest = r(([], t.val))
        return rest == ""
    except InputError:
        return False
def applies(t, r): 
    try:
        r(([], t.val))
        return True
    except InputError:
        return False