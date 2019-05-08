import scala.annotation.tailrec
//Tomasz Mosur 228068

//zadanie 1
def take[A](n:Int, xs:List[A]):List[A] =
  xs match {
    case h::t => if(n>0)h::take(n-1, t) else Nil
    case Nil => Nil
  }

take(2, List(1,2,3,5,6)) == List(1,2)
take(-2, List(1,2,3,5,6)) == Nil
take(8, List(1,2,3,5,6)) == List(1,2,3,5,6)
take(2, List()) == Nil

//zadanie 2
def drop[A](n:Int, xs:List[A]):List[A]=
  xs match {
    case _::t => if(n>0) drop(n-1, t) else xs
    case Nil => Nil
  }

drop(2, List(1,2,3,5,6)) == List(3,5,6)
drop(-2, List(1,2,3,5,6)) == List(1,2,3,5,6)
drop(8, List(1,2,3,5,6)) == Nil
drop(5,List()) == Nil

//zadanie 3
def reverse[A](xs:List[A]):List[A] = {
  def reverseAcc(xs: List[A], acc: List[A]): List[A] =
    xs match {
      case h :: t => reverseAcc(t, h :: acc)
      case Nil => acc
    }
  reverseAcc(xs, List())
}
reverse(List("Ala", "ma", "kota")) == List("kota", "ma", "Ala")

//zadanie 5
def root3 (a: Double): Double = {
  @tailrec
  def improve(a: Double, xi: Double): Double ={
    if(math.abs(xi*xi*xi-a)<= math.abs(a)*10E-15)
      xi
    else improve(a, xi+(a/(xi*xi)-xi)/3)
  }

  if (a > 1) improve(a, a/3)
  else improve(a, a)
}

root3(-8.0) == -2.0
root3(8.0) == 2.0
root3(-10) == -2.154434690031884
root3(0) == 0
root3(-1) == -1


//zadanie4
// na dole dodano akumulator do listy
//def adder(number: Int, counter: Int): List[Int] =
//  if(counter>0) number::adder(number, counter - 1)
//  else Nil
//
//def replicate(xs:List[Int]):List[Int]= {
////  def replicateAcc(xs: List[Int], acc: List[Int]): List[Int] =
////    match xs{
////    case h::t =>
////  }
//    xs match {
//      case h :: t => adder(h, h) ::: replicate(t)
//      case Nil => Nil
//    }
//}
//replicate (List(1,0,4,-2,3)) == List(1, 4, 4, 4, 4, 3, 3, 3)
//replicate (List(1,0,4,-2,3))

//zadanie4
//akumulator do listy
def adder(xs:List[Int], number: Int, counter: Int): List[Int] =
    xs match {
      case _::t => if(counter>0) number::adder(xs, number, counter - 1)
        else if (counter<=0 && t!=Nil) adder(t, t.head, t.head)
        else Nil
      case Nil => Nil
    }

def replicate(xs:List[Int]):List[Int]= {
  xs match {
    case h :: _ => adder(xs, h,h)
    case Nil => Nil
  }
}

replicate (List(1,0,4,-2,3)) == List(1, 4, 4, 4, 4, 3, 3, 3)
replicate (List(1,0,4,-2,3))
replicate (List()) == Nil