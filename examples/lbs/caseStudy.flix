use List.List;
use ToString.ToString;
def main(_args: Array[String]): Int & Impure =
    let connectionChannel = chan Channel[String] 100 restricted ["Processor"];
    let resultChannel = chan String 0 restricted ["Processor"];
    List.foreach(_ -> spawn Processor.start(connectionChannel, resultChannel), List.range(0, 5));
    let connectionStarter = c -> {con(["Processor"], connectionChannel <- c); ()};
    List.foreach(_ -> spawn Producer.start(connectionStarter), List.range(0, 10));
    List.foreach(_ -> println(<- resultChannel), List.range(0, 10));
    0

namespace Processor {
    pub def start(connectionChannel: Channel[Channel[String]], resultChannel: Channel[String]): Unit & Impure =
        select {
            case sessionChannel <- connectionChannel =>
                resultChannel <- listenAndBuild(sessionChannel, "");
                start(connectionChannel, resultChannel)
            case _ <- Timer.seconds(5i64) => ()
        }

    def listenAndBuild(sessionChannel: Channel[String], acc: String): String & Impure =
        let msg = <- sessionChannel;
        if (msg == "CLOSE") acc else listenAndBuild(sessionChannel, acc+msg)
}

namespace Producer {
    def sendStrings(number: Int, c: Channel[String]): Unit & Impure =
        if (number <= 0) () else {
            c <- ToString.toString(number);
            sendStrings(number-1, c)
        }

    pub def start(connectionStarter: Channel[String] -> Unit): Unit & Impure =
        let sessionChannel = chan String 100;
        /// test con on given lambda
        connectionStarter(sessionChannel);
        sendStrings(10, sessionChannel);
        sessionChannel <- "CLOSE";
        ()
}