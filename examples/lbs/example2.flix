namespace A {
    pub def f(c: Channel[Int]): Unit & Impure = c <- 0; ()
}

def main(_args: Array[String]): Int & Impure =
    let c = chan Int 100 ([]);
    let g = () -> c;
    let cc = con(Int -> ["A"], f);
    A.f(g());
    <- c

