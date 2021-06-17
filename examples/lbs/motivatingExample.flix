namespace A {
    pub def main(): Int & Impure =
        let c = chan Int 10 restricted <A, B, C>;
        B.f(con(<A, B>[?], c)); 0
}

namespace B {
    pub def f(cc: Channel[Int]): Unit & Impure =
        let c = con(<B>[?], cc);
        c <- 2; ()
}

def main(_args: Array[String]): Int & Impure = A.main()
