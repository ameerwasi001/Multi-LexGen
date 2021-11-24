class Token {
    constructor(type_, val) {
        this.type_ = type_
        this.val = val
    }

    toString(){
        return `[${this.type_}:${this.val}]`
    }
}

export {Token}