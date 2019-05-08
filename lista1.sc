//TOMASZ MOSUR 228068

//zadanie 1
val suma: List[Double] => Double = xs =>
  if (xs==Nil) 0
  else xs.head + suma(xs.tail)

suma(Nil) == 0.0
suma(List(-1, 2, 3)) == 4.0
suma(List(5.6)) == 5.6

//zadanie 2
def ends[A](xs:List[A]):(A,A) =
  if(xs==Nil) throw new NoSuchElementException("Empty List")
  else if(xs.tail==Nil) (xs.head, xs.head)
  else (xs.head, ends(xs.tail)._2)

ends(List(1, 2, 3, 5)) == (1,5)
ends(List(1)) == (1,1)
ends(Nil)

//zadanie 3.1
def posortowana(xs:List[Int]):Boolean =
  xs ==Nil || xs.tail == Nil ||
    xs.head <= xs.tail.head && posortowana(xs.tail)

posortowana(List(1,3,3,5,6,7))==true
posortowana(List(1,3,7,5,6,7))==false
posortowana(List(1,2))==true
posortowana(List(3,1))==false
posortowana(List())==true
posortowana(List(3))==true

//zadanie 4
def glue(xs:List[String], sep:String): String =
  if(xs == Nil) ""
  else if(xs.tail == Nil) xs.head
  else xs.head+sep+glue(xs.tail, sep)

glue(List("To", "jest", "napis"), "-")  == "To-jest-napis"
glue(Nil, "-")   ==   ""
glue(List("a"), "-")   ==   "a"