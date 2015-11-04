package utn.tadp.dragonball

object BlackMagic {
  
   implicit class Count[T](list: List[T]) {
    def count(n: T): Int = list count (n equals)
    def count(n: T, q:Int):Boolean = list.count(n) == q
  }
   
   implicit class SuperiorTuple2[T,U](tupla: Tuple2[T,U]) {
    def on_1[S](f:(T=>S)):Tuple2[S,U] = (f(tupla._1), tupla._2)
    def on_2[S](g:(U=>S)):Tuple2[T,S] = (tupla._1, g(tupla._2))
  }
   
}