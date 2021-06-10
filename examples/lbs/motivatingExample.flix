namespace ThirdPartyProducerA {
  pub def produce(consumer: Int -> _ & Impure): Unit & Impure =
    /// ...
    consumer(1);
    /// ...
    ()
}

namespace ThirdPartyProducerB {
  pub def produce(consumer: Int -> _ & Impure): Unit & Impure =
    /// ...
    consumer(2);
    /// ...
    ()
}

namespace ChannelLibrary {
  pub def receive(c: Channel[Int]): Unit & Impure =
    select {
      case x <- c => x |> println; receive(c)
      case _ => ()
    }
    /// for access control we have to trust that C does not do this
    /// ThirdPartyProducerA.produce(_x -> c as & Impure)
    /// ...
}

namespace OurCode {
  pub def main(): Int & Impure =
    let c = chan Int 100 restricted <OurCode, ChannelLibrary>;
    let f = x -> {(c <- x); ()};
    /// for access control we have to "trust" ourselves to not make this mistake
    /// let f = x -> (c <- x);
    spawn ThirdPartyProducerA.produce(f);
    spawn ThirdPartyProducerB.produce(f);
    import java.lang.Thread:sleep(Int64);
    sleep(1000i64);
    ChannelLibrary.receive(c);
    0
}

def main(_args: Array[String]): Int & Impure =
  OurCode.main()
