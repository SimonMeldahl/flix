def main(_args: Array[String]): Int & Impure =
    let c = con(Unit -> ["A", "<main>"], B.t());
    c <- 1;
    0

namespace A {
    pub def t(a: Array[Channel[Int]]): Int & Impure =
        <- a[0]
}

namespace B {
    pub def t(): Channel[Int] & Impure =
        let c = chan Int 100 restricted ["A", "B"];
        c <- 2;
        c
}

