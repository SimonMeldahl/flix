def main(_args: Array[String]): Int & Impure =
    let c = chan Int 100;
    c <- 0;
    A.t([c])

namespace A {
    pub def t(a: Array[Channel[Int]]): Int & Impure =
        <- a[0]
}

namespace B {
    pub def t(a: Channel[Int]): Int & Impure =
        <- a
}

