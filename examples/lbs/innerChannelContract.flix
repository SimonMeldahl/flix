def main(_args: Array[String]): Int & Impure =
    A.start()

namespace A {
    pub def start(): Unit & Impure =
        /// no guard, policy <A, B, C>
        let c = chan Channel[Int] 2 restricted <A, B, C>;

        /// guard A ->
        B.receive(con(<A, B>[<A>[Int]], c));
        C.receive(con(<A, B>[<>[Int]], c));
        0
}

namespace B {
    pub def receive(c: Channel[Channel[Int]]): Unit & Impure =
        let cinner = chan Int 2 restricted <B, C>;
        c <- con(<C>[?], cinner);
        ()
}

namespace C {
    pub def receive(c: Channel[Channel[Int]]): Unit & Impure =
        let cinner = <- c;
        cinner <- 2;
        ()
}