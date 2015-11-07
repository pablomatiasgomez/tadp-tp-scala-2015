package utn.tadp.dragonball

object BlackMagic {
  
   implicit class Count[T](list: List[T]) {
    
    def count(elem: T): Int = list count(elem equals)
    
    def countIs(elem: T, cant: Int): Boolean = list.count(elem) == cant
  
    def has(cant: Int, elem: T): Boolean = list.countIs(elem, cant)
  
  }
   
   implicit class SuperiorTuple2[T, U](tupla: Tuple2[T, U]) {
    
    def onFst[S](f: (T => S)): Tuple2[S, U] = (f(tupla._1), tupla._2)
    
    def onSnd[S](g: (U => S)): Tuple2[T, S] = (tupla._1, g(tupla._2))
    
    def onEach[A, B](fTupla: (T => A, U => B)): Tuple2[A, B] = (fTupla._1(tupla._1), fTupla._2(tupla._2))
    
    def fold1[S](f:((T, U) => S)): S = f(tupla._1, tupla._2)
    
    def fold2[S](f:((U, T) => S)): S = f(tupla._2, tupla._1)
  
   }
   
   implicit class SameTypeSuperiorTuple2[T](tupla: Tuple2[T, T]){
     
     def map[U](f: (T => U)): Tuple2[U, U] = (f(tupla._1), f(tupla._2)) 
   
   }
   
   implicit class Is[T](any: T){
     
     def is = any == _
   
   }
   
   implicit class IntConPotenciaEntera(base: Int){
     
     def pow(exponente: Int) = List.fill(exponente)(base).foldRight(1)( _ * _)
     
   }
   
   implicit class MagicBool[T, U](algo: T){
     
     def transformOnTrue(criterio: T => Boolean)(f: T => T): T = if(criterio(algo)) f(algo)
                                                                 else algo
     
     def transformOnTrue(condicion: Boolean)(f: T => T): T = algo.transformOnTrue( _ => condicion)(f)
     
     def becomeOnTrue(criterio: T => Boolean)(otraCosa: T): T = algo.transformOnTrue(criterio)( _ => otraCosa)
     
     def becomeOnTrue(condicion: Boolean)(otraCosa: T): T = algo.transformOnTrue(condicion)( _ => otraCosa)
   
   }
   
   implicit class TuplaDeCombatientes(combatientes: Simulador.Combatientes) {
     
     def definirResultado: ResultadoPelea = {
       val (atacanteFinal, oponenteFinal) = combatientes
        (atacanteFinal.estado,oponenteFinal.estado) match {
            case (_, Muerto) => Ganador(atacanteFinal)
            case (Muerto, _) => Ganador(oponenteFinal)
            case (_, _) => PeleaEnCurso((atacanteFinal, oponenteFinal))
          }   
      }
     
     def diferenciaDeKi:Int = combatientes._1.energia - combatientes._2.energia
   
   }
   
}