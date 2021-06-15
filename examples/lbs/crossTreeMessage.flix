namespace A {
    def sleep(): Unit & Impure =
        import java.lang.Thread:sleep(Int64);
        sleep(5000i64)

    pub def scenario(): Unit & Impure =
        let channel = chan Channel[Int] 10;
        spawn B.transfer(channel);
        spawn C.transfer(channel);
        sleep();
        println("Scenario over")
}

namespace B {
    pub def transfer(channel: Channel[Channel[Int]]): Unit & Impure =
        let innerChannel = chan Int 10;
        innerChannel <- 42;
        con(<A, C>[Channel[Int]], channel <- innerChannel);
        ()
}

namespace C {
    pub def transfer(channel: Channel[Channel[Int]]): Unit & Impure =
        let innerChannel = <- channel;
        println(<- innerChannel);
        ()
}

def main(_args: Array[String]): Int & Impure =
    A.scenario(); 0