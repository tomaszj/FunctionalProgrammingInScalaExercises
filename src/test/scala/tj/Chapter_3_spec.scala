package tj

import org.scalatest.{FlatSpec, Matchers}

class Chapter_3_spec extends FlatSpec with Matchers {

  "Exercise 3.1" should "be successful" in {
    List.magic_x should be(3)
  }

  "Exercise 3.2" should "be successful" in {
    List.tail(List(1, 2, 3)) should be (List(2, 3))
    List.tail(List()) should be (List())
  }

  "Exercise 3.3" should "be successful" in {
    List.setHead(List(1, 2), 3) should be (List(3, 2))
    List.setHead(List(), 1) should be (List(1))
  }

  "Exercise 3.4" should "be successful" in {
    List.drop(List(1, 2, 3, 4), 2) should be (List(3, 4))
    List.drop(List(1, 2), 0) should be (List(1, 2))
    List.drop(List(), 5) should be (List())
  }

  "Exercise 3.5" should "be successful" in {
    List.dropWhile(List(1, 2, 3, 4), (n: Int) => n != 3) should be (List(3, 4))
    List.dropWhile(List(1, 2), (n: Int) => n == 100) should be (List(1, 2))
    List.dropWhile(List(), (n: Int) => n != 1000) should be (List())
  }

  "Exercise 3.6" should "be successful" in {
    List.init(List(1, 2, 3, 4)) should be (List(1, 2, 3))
    List.init(List(1)) should be (List())
    List.init(List()) should be (List())
  }

  "Exercise 3.8" should "be successful" in {
    List.foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_,_)) should be(List(1, 2, 3))
  }

  "Exercise 3.9" should "be successful" in {
    List.length(List(1, 2, 3)) should be (3)
    List.length(List()) should be (0)
  }

  "Exercise 3.10" should "be successful" in {
    var longList = Nil: List[Int]
    for (i <- 100000 to 1 by -1) {
      longList = List.append(Cons(i, Nil), longList)
    }

    List.foldLeft(longList, 0)((count, _) => count + 1) should be (100000)
  }

  "Exercise 3.11" should "be successful" in {
    List.foldLeft(List(1, 2, 3), 0)(_ + _) should be (6)
    List.foldLeft(List(1, 2, 3), 1.0)(_ * _) should be (6)
    List.foldLeft(List(1, 2, 3), 0.0)((count, _) => count + 1) should be (3)
  }

  "Exercise 3.12" should "be successful" in {
    def reverse[A](l: List[A]) = {
      List.foldLeft(l, Nil: List[A])((list, el) => Cons(el, list))
    }

    reverse(List(1, 2, 3)) should be (List(3, 2, 1))
    reverse(List()) should be (List())
  }
}
