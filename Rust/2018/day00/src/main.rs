use std::{
    env,
    process,
};

use day00::Config;
use day00::run;

fn main() {
    let args: Vec<String> = env::args().collect();

    let config = Config::new(&args).unwrap_or_else(|err|{
        println!("Problem parsing arguments: {}", err);
        process::exit(1);
    });
    
    run(config);
}
