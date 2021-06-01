namespace A {
    pub def f(c: Channel[Unit -> Channel[Int]]): Unit & Impure =
        let innerc = (<- c)();
        println(<- innerc);
        ()
}

def main(_args: Array[String]): Int & Impure =
    let c = chan (Unit -> Channel[Int]) 100 restricted ["A"];
    let innerc = chan Int 100 restricted [];
    innerc <- 0 <- 0;
    c <- (() -> innerc);
    A.f(c);
    <- innerc
