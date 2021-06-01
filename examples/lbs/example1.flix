namespace ThirdPartyProducerA {
    pub def name(): String & Pure = "ThirdPartyProducerA"
    pub def produce(consumer: Int -> _ & Impure, con: _): Unit & Impure =
        /// ...
        consumer(1); 
        /// ...
        ()
}

namespace ThirdPartyProducerB {
    pub def name(): String & Pure = "ThirdPartyProducerB"
    pub def produce(consumer: Int -> _ & Impure): Unit & Impure = 
        /// ...
        consumer(2); 
        /// ...
        ()
}

namespace ChannelLibrary {
    pub def name(): String & Pure = "ChannelLibrary"
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
    pub def name(): String & Pure = "OurCode"
    pub def main(): Int & Impure =
        let whitelist = [ChannelLibrary.name(), OurCode.name()];
        let c = chan Int 100 (whitelist);
        let f = x -> {(c <- x); ()};
        /// for access control we have to "trust" ourselves to not make this mistake
        /// let f = x -> (c <- x);
        con(..., ThirdPartyProducerA.produce(f));
        con(..., ThirdPartyProducerB.produce)(f);
        ChannelLibrary.receive(c);
        0
}

def main(_args: Array[String]): Int & Impure =
    OurCode.main()
