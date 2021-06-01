namespace A {
    pub def f(c: Channel[Int]): Unit & Impure = c <- 0; ()
}

def main(_args: Array[String]): Int & Impure =
    let c = chan Int 100 ([]);
    let cc = con(Int -> ["A"], c);
    let g = () -> cc;
    A.f(g());
    <- c

