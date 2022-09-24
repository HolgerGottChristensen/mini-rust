use macros::my_macro;

fn main() {
    println!("{}", "Hello, world!");

    my_macro!(
        enum Option {
            Some(u32),
            None
        }

        impl Option {
            fn map(self, f: fn(u32) -> i32) -> Self {
                match self {
                    Some(n) => Some(f(n)),
                    None => None
                }
            }
        }

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
