package utn.tadp.dragonball

import utn.tadp.dragonball.Simulador._
import utn.tadp.dragonball.BlackMagic._
import scala.util.Try

case class Guerrero(
      nombre: String,
      inventario: List[Item],
      energiaMaxima: Int,
      energia: Int,
      especie: Especie,
      estado: EstadoDeLucha,
      movimientos: List[Movimiento]
      ) {

  def tusMovimientos(agregados: List[Movimiento]) = copy(movimientos = agregados)
  
  def agregaMovimientos(agregados: List[Movimiento]) = copy(movimientos = movimientos++agregados)
    
  def variarEnergia(f:(Int => Int)) = {
    
    val guerreroAfectado = copy(energia = f(energia).max(0).min(energiaMaxima))
    if(guerreroAfectado.energia == 0) guerreroAfectado.estas(Muerto)
    else guerreroAfectado
   
  }
  
  def tuEnergiaEs(nuevaEnergia: Int) = variarEnergia( _ => nuevaEnergia)
  
  def variarEnergiaMaxima(f:(Int => Int)) = copy(energiaMaxima = f(energiaMaxima))
  
  def tuEnergiaMaximaEs(nuevoMaximo: Int) = variarEnergiaMaxima ( _ => nuevoMaximo)
  
  def aumentaEnergia(aumento: Int) = variarEnergia(aumento+ )
  
  def disminuiEnergia(disminucion: Int) = aumentaEnergia(-disminucion )

  def transformateEn(transformacion: Especie) = copy(especie = transformacion)
  
  def cargarAlMaximo = tuEnergiaEs(energiaMaxima)
  
  def estas(nuevoEstado: EstadoDeLucha): Guerrero = {
    
    (nuevoEstado, especie) match {
      case (Muerto | Inconsciente, Fusionado((original, _))) => original estas nuevoEstado
      case (Inconsciente, Saiyajin(SuperSaiyajin(_, energiaOriginal), cola)) => (this transformateEn Saiyajin(Normal, cola)
                                                                                      tuEnergiaMaximaEs energiaOriginal
                                                                                      estas Inconsciente)
      case _ => copy(estado = nuevoEstado)
    }
    
  }
  
  def variarInventario(f: (List[Item] => List[Item])) = copy(inventario = f(inventario))
  
  def gastarItems(itemsUsados: List[Item]) = variarInventario( _ diff itemsUsados)
  
  def sumaAInventario(agregados: List[Item]) = variarInventario( _++agregados )
  
  def tiene(item:Item): Boolean = inventario contains item
  
  def tiene(items: List[Item]): Boolean = items forall (inventario contains)
  
  def tiene(item: Item, cantidad: Int) = inventario has (cantidad, item)
  
  type CriterioDeCombate = Combatientes => Double
  
  type PlanDeAtaque = List[Movimiento]
  
  def movimientoMasEfectivoContra(oponente: Guerrero)(criterio: CriterioDeCombate): Option[Movimiento] = {
    
    val combatientes = (this, oponente)
    
    def valorDelMovimiento:Movimiento=>Double = movimiento => criterio(movimiento(combatientes))
    
    movimientos.filter( 0 < valorDelMovimiento(_) ).optMaxBy( valorDelMovimiento )
    
  }
  
  def atacarSegun(criterioDeCombate: CriterioDeCombate): (Guerrero => Combatientes) = guerrero => {
              val combatientes = (this, guerrero)
              this.movimientoMasEfectivoContra(guerrero)(criterioDeCombate).fold(combatientes)(_(combatientes))
  }
  
  def contraAtacarA(guerrero: Guerrero): Combatientes = this.atacarSegun(mayorVentajaDeKi)(guerrero)
  
  def mayorVentajaDeKi(combatientes: Combatientes): Double =  (combatientes._1.energia - combatientes._2.energia) match {
                case diferencia if diferencia > 0 => diferencia 
                case 0 => 0.99
                case diferencia if diferencia < 0 => 0.98 / diferencia.abs
  }

              
  def pelearUnRound(movimiento: Movimiento)(oponente: Guerrero): Combatientes = {
    
    val (thisFajado, oponenteFajado) = movimiento(this, oponente)
    oponenteFajado.contraAtacarA(thisFajado).swap
  
  }
  
  def planDeAtaque(oponente: Guerrero, rounds: Int)(criterio: CriterioDeCombate): Try[PlanDeAtaque] = Try {
   
    val (planVacio, combatientes) = (List(): PlanDeAtaque, (this, oponente))

        (1 to rounds).foldLeft(planVacio, combatientes)(
          { case ((plan, (atacante, oponente)), _ ) => 
          atacante.movimientoMasEfectivoContra(oponente)(criterio)
            .fold(throw NoSePuedeGenerarPlanException)(mov => (plan :+ mov, atacante.pelearUnRound(mov)(oponente)))  
        })._1  

      
  }                                                                   
   
  def pelearContra(oponente: Guerrero)(plan: List[Movimiento]): ResultadoPelea = {
    
    val peleaEnCurso: ResultadoPelea = ResultadoPelea(this, oponente)
    
    plan.foldLeft(peleaEnCurso){(pelea, movimiento) => for{ (atacante,oponente) <- pelea }
                                                       yield(atacante.pelearUnRound(movimiento)(oponente))
                                                       }

      }
    
}

object NoSePuedeGenerarPlanException extends Exception

abstract class EstadoDeLucha

case object Luchando extends EstadoDeLucha
case class Fajado(rounds: Int) extends EstadoDeLucha
case object Inconsciente extends EstadoDeLucha
case object Muerto extends EstadoDeLucha

object ResultadoPelea {
  
  def apply(combatientes:Combatientes):ResultadoPelea = {
    val (atacante, oponente) = combatientes
    combatientes.map(_.estado) match {
      case ( _ , Muerto ) => Ganador(atacante)
      case ( Muerto, _ ) => Ganador(oponente)
      case (_, _) => PeleaEnCurso(atacante, oponente)
    }
}
  
  def apply(guerrero:Guerrero):ResultadoPelea = Ganador(guerrero)
  
}



trait ResultadoPelea {
    
    def map(f: Combatientes => Combatientes): ResultadoPelea
    
    def filter(f: Combatientes => Boolean): ResultadoPelea
    
    def flatMap(f: Combatientes => ResultadoPelea): ResultadoPelea
    
    def fold[T](seed: T)(f: Combatientes => T) : T
    
}
  
case class Ganador(guerrero: Guerrero) extends ResultadoPelea {
  
    def map(f: Combatientes => Combatientes) = this
    
    def filter(f: Combatientes => Boolean) = this
    
    def flatMap(f: Combatientes => ResultadoPelea) = this
    
    def fold[T](semilla: T)(f: Combatientes => T) = semilla
}
  
case class PeleaEnCurso(combatientes: Combatientes) extends ResultadoPelea {
  
  def map(luchar: Combatientes => Combatientes) = ResultadoPelea(luchar(combatientes))

  def filter(f: Combatientes => Boolean) = ResultadoPelea(combatientes)
  
  def flatMap(f: Combatientes => ResultadoPelea) = for( combatientesResultantes <- f(combatientes)) 
                                                   yield combatientesResultantes
  
  def fold[T](semilla: T)(f: Combatientes => T) = f(combatientes)
}