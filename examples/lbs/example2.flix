namespace A {
    pub def f(c: Channel[Int]): Unit & Impure = c <- 0; ()
}

def main(_args: Array[String]): Int & Impure =
    let c = chan Int 1;
    A.f(c);
    let asd = K(contract, c);
    A.f(asd);
    K(A.f, c1, ..., cn, con)
    A.f(c, ["hej"]); /// typer/interpreter
    A{["hej"]}.f(c);
    A.f{["hej"]}(c);
    <- c

