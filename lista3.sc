//Tomasz Mosur 228068

println("zadanie 1")
//zadanie 1
def exists[A](xs: List[A])(p: A => Boolean): Boolean =
  xs match {
    case h :: t => p(h) || exists(t)(p)
    case Nil => false
  }

exists(List(5, 1, 2, 3))(_ == 2) == true
exists(List(5, 1, 2, 3))(_ == 6) == false
exists(List())(_ == 6) == false

def existsFL[A](xs: List[A])(p: A => Boolean): Boolean =
  (xs foldLeft false) ((eval: Boolean, next: A) => eval || p(next))

existsFL(List(5, 1, 2, 3))(_ == 2) == true
existsFL(List(5, 1, 2, 3))(_ == 6) == false
existsFL(List())(_ == 6) == false

def existsFR[A](xs: List[A])(p: A => Boolean): Boolean =
  (xs foldRight false) ((next: A, eval: Boolean) => eval || p(next))

existsFR(List(5, 1, 2, 3))(_ == 2) == true
existsFR(List(5, 1, 2, 3))(_ == 6) == false
existsFR(List())(_ == 6) == false

println("zadanie 2")
//zadanie2
def filter[A](xs: List[A])(p: A => Boolean): List[A] =
  (xs foldRight List[A]()) ((curr: A, acc: List[A]) => if (p(curr)) curr :: acc else acc)

filter(List(2, 7, 1, 3, 7, 8, 4, 1, 6, 9))(_ > 3) == List(7, 7, 8, 4, 6, 9)
filter(List(0))(_ > 3) == List()

println("zadanie 3")
//zadanie3a
def remove1[A](xs: List[A])(p: A => Boolean): List[A] =
  xs match {
    case h :: t => if (p(h)) t else h :: remove1(t)(p)
    case Nil => Nil
  }

remove1(List(1, 2, 3, 2, 5))(_ == 2) == List(1, 3, 2, 5)
remove1(List(1, 2, 3, 2, 5))(_ == 6) == List(1, 2, 3, 2, 5)
remove1(List())(_ == 6) == List()

//zadanie 3b
def remove1b[A](xs: List[A])(p: A => Boolean): List[A] = {
  def remove1[A](xs: List[A], p: A => Boolean, acc: List[A]): List[A] = {
    xs match {
      case h :: t => if (p(h)) acc reverse_::: t else remove1(t, p, h :: acc)
      case Nil => acc.reverse
    }
  }

  remove1(xs, p, List())
}

remove1b(List(1, 2, 3, 2, 5))(_ == 2) == List(1, 3, 2, 5)
remove1b(List(1, 2, 3, 2, 5))(_ == 6) == List(1, 2, 3, 2, 5)
remove1b(List(2))(_ == 2) == List()
remove1b(List())(_ == 6) == List()

println("zadanie 4")
//zadanie4
def splitAt[A](xs: List[A])(n: Int): (List[A], List[A]) = {
  def splitAtAcc(xs: List[A])(acc1: List[A])(n: Int)
  : (List[A], List[A]) =
    xs match {
      case h :: t => if (n > 0) splitAtAcc(t)(h :: acc1)(n - 1) else (acc1.reverse, xs)
      case Nil => (acc1.reverse, Nil)
    }

  splitAtAcc(xs)(List())(n)
}

splitAt(List('a', 'b', 'c', 'd', 'e'))(2)
splitAt(List('a', 'b', 'c', 'd', 'e'))(2) == (List('a', 'b'), List('c', 'd', 'e'))