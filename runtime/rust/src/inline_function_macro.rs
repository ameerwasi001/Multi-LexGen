#[macro_export]
macro_rules! inline_fun {
    ($body: expr) => ({ fn f() { $body } f });
    ($res: ty := $body: expr) => ({ fn f() -> $res { $body } f });
    ($($n: ident: $t: ty),+ := $body: expr) => ({
        fn f($($n: $t),+) { $body } f
    });
    ($($n: ident: $t: ty),+ => $res: ty := $body: expr) => ({
        fn f($($n: $t),+) -> $res { $body } f
    })
}
