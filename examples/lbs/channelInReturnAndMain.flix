def main(_args: Array[String]): Int & Impure =
    let (_n, c) = con((Int, </>[Int]), B.t());
    c <- 1;
    0

namespace A {
    pub def t(a: Array[Channel[Int]]): Int & Impure =
        <- a[0]
}

namespace B {
    pub def t(): (Int, Channel[Int]) & Impure =
        let c = chan Int 100 restricted <A, B, />;
        c <- 2;
        (2, c)
}

