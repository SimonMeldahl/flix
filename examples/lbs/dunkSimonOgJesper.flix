def main(_args: Array[String]): Int & Impure =
    let c = chan Int 100;
    c <- 0;
    A.t([c])
    /// con(Array[["A"]] -> Int, A.t([c]))
    ///    cannot nest wl in types
    /// con(Array[Channel[Int]] -> Int)
    ///    parsed as conBase
    /// Channel[Channel[Int]] -> Int
    ///    inner channeltype erasure
    /// ["A"] -> Int

namespace A {
    pub def t(a: Array[Channel[Int]]): Int & Impure =
        <- a[0]
}

namespace B {
    pub def t(a: Channel[Int]): Int & Impure =
            <- a
}

