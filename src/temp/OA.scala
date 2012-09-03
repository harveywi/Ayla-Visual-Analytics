package temp

import scala.swing._

object OA {
  // Initial object algebra interface for expressions: integers and addition
  trait ExpAlg[E] {
    def lit(x: Int): E
    def add(e1: E, e2: E): E
  }

  // An object algebra implementing that interface (evaluation)

  // The evaluation interface
  trait Eval {
    def eval(): Int
  }

  // The object algebra
  trait EvalExpAlg extends ExpAlg[Eval] {
    def lit(x: Int) = new Eval() {
      def eval() = x
    }

    def add(e1: Eval, e2: Eval) = new Eval() {
      def eval() = e1.eval() + e2.eval()
    }
  }
  
  // Evolution 1: Adding subtraction
  trait SubExpAlg[E] extends ExpAlg[E] {
    def sub(e1: E, e2: E): E
  }

  // Updating evaluation:
  trait EvalSubExpAlg extends EvalExpAlg with SubExpAlg[Eval] {
    def sub(e1: Eval, e2: Eval) = new Eval() {
      def eval() = e1.eval() - e2.eval()
    }
  }
  
  trait MultExpAlg[E] extends SubExpAlg[E] {
    def mult(e1: E, e2: E): E
  }
  
  trait EvalMultExpAlg extends EvalSubExpAlg with MultExpAlg[Eval] {
    def mult(e1: Eval, e2: Eval) = new Eval() {
      def eval() = e1.eval() * e2.eval()
    }
  }

  // Evolution 2: Adding pretty printing
  trait PPrint {
    def print(): String
  }

  trait PrintExpAlg extends SubExpAlg[PPrint] {
    def lit(x: Int) = new PPrint() {
      def print() = x.toString()
    }

    def add(e1: PPrint, e2: PPrint) = new PPrint() {
      def print() = e1.print() + " + " + e2.print()
    }

    def sub(e1: PPrint, e2: PPrint) = new PPrint() {
      def print() = e1.print() + " - " + e2.print();
    }
  }
  
  // An alternative object algebra for pretty printing:

  // Often, when precise control over the invocation of 
  // methods is not needed, we can simplify object algebras. 
  // For example, here's an alternative implementation 
  // of pretty printing that directly computes a string:

  trait PrintExpAlg2 extends SubExpAlg[String] {
    def lit(x: Int) = x.toString()

    def add(e1: String, e2: String) = "(" + e1 + " + " + e2 + ")"

    def sub(e1: String, e2: String) = "(" + e1 + " - " + e2 + ")"
  }
  
  trait PrintExpAlg3 extends PrintExpAlg2 with MultExpAlg[String] {
    def mult(e1: String, e2: String) = "(" + e1 + " * " + e2 + ")"
  }

  // Testing
  // An expression using the basic ExpAlg
  def exp1[E](alg: ExpAlg[E]) = {
    import alg._
    add(lit(3), lit(4))
  }

  // An expression using subtraction too
  def exp2[E](alg: SubExpAlg[E]) = {
    import alg._
    sub(exp1(alg), lit(4))
  }
  
  def exp3[E](alg: MultExpAlg[E]) = {
    import alg._
    mult(lit(4), exp2(alg))
  }

  def main(args: Array[String]): Unit = {
    // Some object algebras:
    val ea = new EvalExpAlg() {}
    val esa = new EvalSubExpAlg() {}
    val pa = new PrintExpAlg() {}
    val pa2 = new PrintExpAlg2() {}
    
    val myEsa = new EvalMultExpAlg() {}
    val myPa = new PrintExpAlg3() {}

    // We can call esa with exp1
    val ev = exp1(myEsa)

    // But calling ea with exp2 is an error
    // val ev_bad = exp2(ea)

    // Testing the actual algebras
    println("Evaluation of exp1 \"" + exp1(pa).print() + "\" is: " + ev.eval())
    println("Evaluation of exp2 \"" + exp2(pa).print() + "\" is: " + exp2(esa).eval())
    println("Evaluation of exp3 \"" + exp3(myPa) + "\" is: " + exp3(myEsa).eval())
    println("The alternative pretty printer works nicely too!\nexp1: " + exp1(pa2) + "\nexp2: " + exp2(pa2))
  }

}