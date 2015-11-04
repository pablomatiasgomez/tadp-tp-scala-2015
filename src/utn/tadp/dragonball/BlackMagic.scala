package utn.tadp.dragonball

object BlackMagic {
  
   implicit class Count[T](list: List[T]) {
    def count(n: T): Int = list count (n equals)
    def count(n: T, q:Int):Boolean = list.count(n) == q
  }
   
}