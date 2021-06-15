namespace TerminalPrinter {

    def eatAndPrint(c: Channel[String]): Unit & Impure =
        select {
            case x <- c => println("> "+ x); eatAndPrint(c)
        }

    pub def messageChannel(): Channel[String] & Impure =
        let c = chan String 1000;
        spawn eatAndPrint(c);
        c
}

def main(_args: Array[String]): Int & Impure =
    let c = con(</>[?], TerminalPrinter.messageChannel());
    c <- "start" <- "middle" <- "end";
    import java.lang.Thread:sleep(Int64);
    sleep(1000i64);
    0
