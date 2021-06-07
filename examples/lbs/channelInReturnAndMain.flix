/// remove channels in main from all examples
/// remake newChannel to use second class values or fix main ref
/// make inner contracts
/// fix random print
def main(_args: Array[String]): Int & Impure =
    let (_n, c) = con((Int, ?), B.t());
    c <- 1;
    0

namespace A {
    pub def t(a: Array[Channel[Int]]): Int & Impure =
        <- a[0]
}

namespace B {
    pub def t(): (Int, Channel[Int]) & Impure =
        let c = chan Int 100 restricted ["A", "B", "<main>"];
        c <- 2;
        (2, c)
}

