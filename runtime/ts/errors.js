class InputError extends Error {
    constructor(message, ...args) {
        super(...args)
        this.name = "InputError"
        this.message = message
        Error.captureStackTrace(this, InputError)
    }
}

class InternalError extends Error {
    constructor(message, ...args) {
        super(...args)
        this.name = "InternalError"
        this.message = message
        Error.captureStackTrace(this, InputError)
    }
}

export {InputError, InternalError}