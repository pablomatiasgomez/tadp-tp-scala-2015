package utn.tadp.dragonball

object BlackMagic {
  
   implicit class Count[T](list: List[T]) {
    def count(n: T): Int = list count (n equals)
    def countIs(n: T, q:Int):Boolean = list.count(n) == q
    def has(q:Int,n:T):Boolean = list.countIs(n,q)
  }
   
   implicit class SuperiorTuple2[C,T >:C, U>:C](tupla: Tuple2[T,U]) {
    def onFst[S](f:( T => S )): Tuple2[S,U] = (f(tupla._1), tupla._2)
    def onSnd[S](g:( U => S )): Tuple2[T,S] = (tupla._1, g(tupla._2))
    def onEach[A,B](fTupla:( T =>A,U=>B)): Tuple2[A,B] = (fTupla._1(tupla._1),fTupla._2(tupla._2))
  }
   
   implicit class SameTypeSuperiorTuple2[T](tupla: Tuple2[T,T]){
     def onBoth[U](f:(T=>U)): Tuple2[U,U] = (f(tupla._1),f(tupla._2)) 
   }
   
   implicit class Is[T](any:T){
     def is = any == _
   }
   
   implicit class IntConPotenciaEntera(int:Int){
     def ^^(cant:Int) = List.fill(cant)(int).foldRight(1)( _ * _)
   }
   
   implicit class MagicBool[T,U](algo:T){
     def transformOnTrue(criterio:T=>Boolean)(f:T=>T):T = if(criterio(algo)) f(algo)
                                                           else algo
     def transformOnTrue(condicion:Boolean)(f:T=>T):T = algo.transformOnTrue( _ => condicion)( f)
     def becomeOnTrue(criterio:T=>Boolean)(otraCosa:T):T = algo.transformOnTrue(criterio)( _ => otraCosa )
     def becomeOnTrue(condicion:Boolean)(otraCosa:T):T = algo.transformOnTrue(condicion)( _ => otraCosa )
   }
   
}