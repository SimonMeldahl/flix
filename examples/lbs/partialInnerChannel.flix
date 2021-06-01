namespace A {
    pub def f(): Unit & Impure =
        let c = chan Channel[Int] 100;
        B.f(c);
        ()
}

namespace B {
    pub def f(c: Channel[Channel[Int]]): Unit & Impure =
        c <- (chan Int 100);
        C.f(c);
        ()
}

namespace C {
    pub def f(c: Channel[Channel[Int]]): Unit & Impure =
        let innerc = <- c;
        innerc <- 2;
        ()
}

def main(_args: Array[String]): Int & Impure = A.f(); 0