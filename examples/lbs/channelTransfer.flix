namespace A {
    pub def f(c: Channel[Channel[Int]]): Unit & Impure =
        let innerc = <- c;
        B.f(innerc);
        ()
}

namespace B {
    pub def f(c: Channel[Int]): Unit & Impure = <- c; ()
}

def main(_args: Array[String]): Int & Impure =
    let c = chan Channel[Int] 100 restricted ["A"];
    let innerc = chan Int 100 restricted ["B"];
    innerc <- 0 <- 0;
    c <- innerc;
    A.f(c);
    <- innerc
