package fpinscala.parsing

import fpinscala.testing.{Gen, Passed, Prop, Result}

import language.higherKinds

trait Parsers[ParseError, Parser[+_]] { self => // so inner classes may call methods of trait

  def char(c: Char): Parser[Char]
  def or[A,B >: A](p1: Parser[A], p2: Parser[B]): Parser[B]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
  def many[A](p: Parser[A]): Parser[List[A]]

  def map[A,B](p: Parser[A])(f: A => B): Parser[B]

  def run[A](p: Parser[A])(s:String): Either[ParseError, A]


  implicit def string(s: String): Parser[String]
  implicit def operators[A](implicit p: Parser[A]): ParserOps[A] = ParserOps(p)
  implicit def asParser[A, B](a: A)(implicit f: A => Parser[B]): ParserOps[B] =
    ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)

  }

  object ParserOps {
    implicit def getParser[A](p: ParserOps[A]): Parser[A] = p.p
  }

  object Laws {
    val numOfAs: Parser[Int] = ("a".many.map(_.size))

    val genChar: Gen[Char] = Gen.choose(Char.MinValue, Char.MaxValue).map(_.toChar)

    val parseCharCorrectly: Prop = Prop.forAll(genChar) { c =>
      run(char(c))(c.toString) == Right(c)
    }

    //ideally I'd want to handle nested forAll in Prop,
    //so that I can carry failed messages of nested properties
    val refuteAllOtherChars: Prop = {
      val differentChars: Gen[(Char, Char)] =
        genChar.flatMap(c => genChar.otherThan(c).map(d => (c, d)))

      Prop.forAll[(Char, Char)](differentChars) { case (c, d) =>
        run(char(c))(d.toString).isInstanceOf[Left[_,_]]
      }
    }

    //checking typeclass methods
    val x: Parser[String] = "abra" | "cadabra"
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}