use mini_rust_ast::mini_rust_ast;

fn main() {
    println!("{}", "Hello, world!");

    mini_rust_ast!(
        trait TestTrait<T>: Debug where T: Clone + Debug {

        }

        impl<T> TestTrait<T> for Option {
            fn hello<U>(u: U) -> () {

            }
        }
        /*struct Hejsa<T> where T: Debug {
            field1: T,
            field2: String
        }*/

        //enum Option<T: Default> where T: Widget, G: Grim {
        /*enum Option<T: Default> {
            Some(T),
            None
        }

        impl Option {
            fn map(self, f: fn(u32) -> i32) -> Self {
                match self {
                    Some(n) => Some(f(n)),
                    None => None
                }
            }
        }*/

        /*trait Test: Clone + Debug {
            fn test(self) -> () {

            }
        }

        fn main() -> () {
            let i = &"Hejsa";

            i = "World" + -23;
            //let mut j = "Hejsa";

            match i {
                "Test" => {
                    23
                },
                _ => (),
            }

            return ();
        }


        struct Hejsa {
            field1: u32,
            field2: String
        }*/
    );
}
