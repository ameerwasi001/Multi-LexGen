extern crate lexerGenerator;
mod lexer;
mod lexer_helpers;

fn main() {
    let x = lexer::get_toks(
        String::from("if b then iffer+0+mx+9.2-192.168.0.1/1.2-MxC*oz2 else 0.0.0.0+72.98*10-PI")
    );
    match x {
        Ok(arr) => {
            for x in arr {
                println!("{}", x)
            }
        },
        Err(err) => panic!(err)
    }
}
