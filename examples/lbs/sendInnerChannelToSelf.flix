def main(_args: Array[String]): Int & Impure =
    let c = chan Channel[Int] 10;
    let cinner = chan Int 10;
    cinner <- 0;
    c <- cinner;
    let cinnerr = <- c;
    <- cinnerr

