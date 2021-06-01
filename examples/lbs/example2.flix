namespace A {
    pub def f(c: Channel[Int]): Unit & Impure = c <- 0; ()
}

def main(_args: Array[String]): Int & Impure =
    let _x = A.f;
    0 as & Impure

