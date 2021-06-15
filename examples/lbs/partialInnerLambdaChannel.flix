namespace A {
    pub def f(): Unit & Impure =
        let c = chan (Unit -> Channel[Int]) 100;
        B.f(c);
        ()
}

namespace B {
    pub def f(c: Channel[Unit -> Channel[Int]]): Unit & Impure =
        let innerc = chan Int 100;
        c <- (() -> innerc);
        C.f(c);
        ()
}

namespace C {
    pub def f(c: Channel[Unit -> Channel[Int]]): Unit & Impure =
        let innerc = (<- c)();
        innerc <- 2;
        ()
}

def main(_args: Array[String]): Int & Impure = A.f(); 0