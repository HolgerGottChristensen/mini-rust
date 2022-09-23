use macros::my_macro;

fn main() {
    println!("Hello, world!");

    my_macro!(
        /*enum Option {
            Some(u32),
            None
        }*/

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
        /*enum Test {
            Case1,
            Case2,
        }

        struct Hejsa {
            field1: u32,
            field2: String
        }*/
    );
}
