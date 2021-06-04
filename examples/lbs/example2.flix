namespace A {
    pub def f(c: Channel[Int]): Unit & Impure =
        c <- 0; c <- 0; B.f(c)
}

namespace B {
    pub def f(c: Channel[Int]): Unit & Impure =
        <- c; ()
}

def main(_args: Array[String]): Int & Impure =
    let c = chan Int 100;
    let g = () -> c;
    A.f(con(["A", "B"], g()));
    <- c
