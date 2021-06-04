def main(_args: Array[String]): Int & Impure =
    test();
    0

def getChannel(): Unit -> Channel[Int] & Impure = _ -> chan Int 100

def test(): Int & Impure =
    let thunk = con(Unit -> ["<main>"], getChannel());
    <- (thunk() <- 2)