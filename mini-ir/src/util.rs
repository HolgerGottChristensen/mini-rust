use paris::formatter::colorize_string;

pub static COLOR_NAMES: &'static [&'static str] = &[
    "red",
    "bright-green",
    "bright-yellow",
    "blue",
    "cyan",
    "magenta"
];

pub fn get_color(index: u32, s: &str) -> String {
    colorize_string(format!("<{}>{}</>", COLOR_NAMES[index as usize % COLOR_NAMES.len()], s))
}