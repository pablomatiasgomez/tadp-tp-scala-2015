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
      movimientos: List[Movimiento],
      turnosFajado: Int = 0
      ) {

  def pasarTurnoFajado = copy(turnosFajado = turnosFajado + 1)
  
  def resetearTurnosFajados = copy(turnosFajado = 0)
  
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
    
    (nuevoEstado, estado, especie) match {
      case (Muerto | Inconsciente, _, Fusionado((original, _))) => original estas nuevoEstado
      case (_, estadoAnterior: EstadoSaiyajing, _) => copy(estado = nuevoEstado) tuEnergiaMaximaEs estadoAnterior.energiaOriginal(this)
      case _ => copy(estado = nuevoEstado)
    }
    
  }
  
  def variarInventario(f: (List[Item] => List[Item])) = copy(inventario = f(inventario))
  
  def gastarItems(itemsUsados: List[Item]) = variarInventario( _ diff itemsUsados)
  
  def sumaAInventario(agregados: List[Item]) = variarInventario( _++agregados )
  
  def tiene(item:Item): Boolean = inventario contains item
  
  def tiene(items: List[Item]): Boolean = items forall (inventario contains)
  
  def tiene(item: Item, cantidad: Int) = inventario has (cantidad, item)
  
  type CriterioDeCombate = Combatientes => Option[Double]
  
  type PlanDeAtaque = List[Movimiento]
  
  def movimientoMasEfectivoContra(oponente: Guerrero)(criterio: CriterioDeCombate): Option[Movimiento] = {
    
    val combatientes = (this, oponente)
    
    def valorDelMovimiento:Movimiento=> Option[Double] = movimiento => criterio(movimiento(combatientes))
    
    
    (for{movimiento <- movimientos
       valor <- valorDelMovimiento(movimiento)
    } yield(movimiento)).optMaxBy(valorDelMovimiento)
  }
  
  def atacarSegun(criterioDeCombate: CriterioDeCombate): (Guerrero => Combatientes) = guerrero => {
              val combatientes = (this, guerrero)
              this.movimientoMasEfectivoContra(guerrero)(criterioDeCombate).fold(combatientes)(_(combatientes))
  }
  
  def contraAtacarA(guerrero: Guerrero): Combatientes = this.atacarSegun(mayorVentajaDeKi)(guerrero)
  
  def mayorVentajaDeKi(combatientes: Combatientes): Option[Double] =  Option((combatientes._1.energia - combatientes._2.energia) match {
                case diferencia if diferencia > 0 => diferencia 
                case 0 => 0.99
                case diferencia if diferencia < 0 => 0.98 / diferencia.abs
  })

              
  def pelearUnRound(movimiento: Movimiento)(oponente: Guerrero): Combatientes = {
    
    val (thisFajado, oponenteFajado) = movimiento(this, oponente)
    oponenteFajado.contraAtacarA(thisFajado).swap
  
  }
  
  def planDeAtaque(oponente: Guerrero, rounds: Int)(criterio: CriterioDeCombate): Try[PlanDeAtaque] = Try {
   
    val semilla = (List(): PlanDeAtaque, (this, oponente))

        (1 to rounds).foldLeft(semilla)(
          { case ((plan, (atacante, oponente)), _ ) => 
          atacante.movimientoMasEfectivoContra(oponente)(criterio)
            .fold(throw NoSePuedeGenerarPlanException)(mov => (plan :+ mov, atacante.pelearUnRound(mov)(oponente)))  
        })._1  

      
  }                                                                   
   
  def pelearContra(oponente: Guerrero)(plan: List[Movimiento]): ResultadoPelea = {
    
    
    plan.foldLeft(ResultadoPelea(this, oponente)){(pelea, movimiento) => for{ (atacante,oponente) <- pelea }
                                                       yield(atacante.pelearUnRound(movimiento)(oponente))
                                                       }

      }
    
}

object NoSePuedeGenerarPlanException extends Exception

abstract class EstadoDeLucha{
  def energiaOriginal(guerrero: Guerrero) = guerrero.energiaMaxima
}

case object Luchando extends EstadoDeLucha
case object Inconsciente extends EstadoDeLucha
case object Muerto extends EstadoDeLucha

abstract class EstadoSaiyajing extends EstadoDeLucha{
  
  def energiaOriginal(guerrero: Guerrero): Int
  def proxNivelSSJ = 1

}

case class SuperSaiyajin(nivel: Int, energiaNormal: Int) extends EstadoSaiyajing{
  override def proxNivelSSJ = nivel + 1
  override def energiaOriginal(guerrero: Guerrero) = energiaNormal
}

case class MonoGigante(energiaNormal: Int) extends EstadoSaiyajing{
  override def energiaOriginal(guerrero: Guerrero) = energiaNormal
}



object ResultadoPelea {
  
  def apply(combatientes:Combatientes):ResultadoPelea = {
    val (atacante, oponente) = combatientes
    combatientes.map(_.estado) match {
      case ( _ , Muerto ) => Ganador(atacante)
      case ( Muerto, _ ) => Ganador(oponente)
      case _ => PeleaEnCurso(atacante, oponente)
    }
  }
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