use List.{range, foreach};
use ToString.{toString};
def main(_args: Array[String]): Int & Impure =
  let connectionChannel = chan Channel[String] 100 restricted ["Processor"];
  let resultChannel = chan String 0 restricted ["Processor"];
  foreach(_ -> spawn Processor.start(connectionChannel, resultChannel), range(0, 2));
  let connectionStarter = c -> {connectionChannel <- con(["Processor"], c); ()};
  foreach(_ -> spawn Producer.start(connectionStarter), range(0, 3));
  foreach(_ -> println(<- resultChannel), range(0, 3));
  0

namespace Processor {
  pub def start(connectionChannel: Channel[Channel[String]], resultChannel: Channel[String]): Unit & Impure =
    select {
      case sessionChannel <- connectionChannel =>
        resultChannel <- listenAndBuild(sessionChannel, "");
        start(connectionChannel, resultChannel)
      case _ <- con(["Processor"], Timer.seconds(5i64)) => ()
    }

  def listenAndBuild(sessionChannel: Channel[String], acc: String): String & Impure =
    let msg = <- sessionChannel;
    if (msg == "CLOSE") acc else listenAndBuild(sessionChannel, acc+msg)
}

namespace Producer {
  pub def start(connectionStarter: Channel[String] -> Unit & Impure): Unit & Impure =
    let sessionChannel = chan String 100;
    /// test con on given lambda
    connectionStarter(con(["Processor"], sessionChannel));
    foreach(i -> {sessionChannel <- toString(i); ()}, range(0, 5));
    sessionChannel <- "CLOSE";
    ()
}