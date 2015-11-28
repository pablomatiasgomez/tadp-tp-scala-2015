package utn.tadp.dragonball

import utn.tadp.dragonball.Simulador._
import utn.tadp.dragonball.BlackMagic._
import scala.util.Try

case class Guerrero[+E<:Especie](
      nombre: String,
      inventario: List[Item],
      energiaMaxima: Int,
      energia: Int,
      especie: E,
      estado: EstadoDeLucha[E],
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
  
  def estas[F<:Especie](nuevoEstado: EstadoDeLucha[F]): Guerrero[Especie] = {
    
    (nuevoEstado, estado, especie) match {
      case (Muerto | Inconsciente, _, Fusionado((original, _))) => original estas nuevoEstado
      case (_, estadoAnterior: EstadoSaiyajing[Saiyajin], _) => copy(estado = nuevoEstado) tuEnergiaMaximaEs estadoAnterior.energiaOriginal(this)
      case _ => copy(estado = nuevoEstado)
    }
    
  }
  
  def variarInventario(f: (List[Item] => List[Item])) = copy(inventario = f(inventario))
  
  def gastarItems(itemsUsados: List[Item]) = variarInventario( _ diff itemsUsados)
  
  def sumaAInventario(agregados: List[Item]) = variarInventario( _++agregados )
  
  def tiene(item:Item): Boolean = inventario contains item
  
  def tiene(items: List[Item]): Boolean = items forall (inventario contains)
  
  def tiene(item: Item, cantidad: Int) = inventario has (cantidad, item)
  
  type CriterioDeCombate = Combatientes[Especie,Especie] => Option[Double]
  
  type PlanDeAtaque = List[Movimiento]
  
  def movimientoMasEfectivoContra(oponente: Guerrero[Especie])(criterio: CriterioDeCombate): Option[Movimiento] = {
    
    val combatientes = (this, oponente)
    
    def valorDelMovimiento:Movimiento=> Option[Double] = movimiento => criterio(movimiento(combatientes))
    
    
    (for{movimiento <- movimientos
       valor <- valorDelMovimiento(movimiento)
    } yield(movimiento)).optMaxBy(valorDelMovimiento)
  }
  
  def atacarSegun(criterioDeCombate: CriterioDeCombate): (Guerrero[Especie] => Combatientes[Especie,Especie]) = guerrero => {
              val combatientes = (this, guerrero)
              this.movimientoMasEfectivoContra(guerrero)(criterioDeCombate).fold(combatientes:Combatientes[Especie,Especie])(_(combatientes))
  }
  
  def contraAtacarA(guerrero: Guerrero[Especie]): Combatientes[Especie, Especie] = this.atacarSegun(mayorVentajaDeKi)(guerrero)
  
  def mayorVentajaDeKi(combatientes: Combatientes[Especie, Especie]): Option[Double] =  Option((combatientes._1.energia - combatientes._2.energia) match {
                case diferencia if diferencia > 0 => diferencia 
                case 0 => 0.99
                case diferencia if diferencia < 0 => 0.98 / diferencia.abs
  })

              
  def pelearUnRound(movimiento: Movimiento)(oponente: Guerrero[Especie]): Combatientes[Especie, Especie] = {
    
    val (thisFajado, oponenteFajado) = movimiento(this, oponente)
    oponenteFajado.contraAtacarA(thisFajado).swap
  
  }
  
  def planDeAtaque(oponente: Guerrero[Especie], rounds: Int)(criterio: CriterioDeCombate): Try[PlanDeAtaque] = Try {
    val combatientes:Combatientes[Especie,Especie] = (this,oponente)
    val semilla = (List(): PlanDeAtaque, combatientes)

        (1 to rounds).foldLeft(semilla)(
          { case ((plan, (atacante, oponente)), _ ) => 
          atacante.movimientoMasEfectivoContra(oponente)(criterio)
            .fold(throw NoSePuedeGenerarPlanException)(mov => (plan :+ mov, atacante.pelearUnRound(mov)(oponente)))  
        })._1  

      
  }                                                                   
   
  def pelearContra(oponente: Guerrero[Especie])(plan: List[Movimiento]): ResultadoPelea = {
    
    
    plan.foldLeft(ResultadoPelea(this, oponente)){(pelea, movimiento) => for{ (atacante,oponente) <- pelea }
                                                       yield(atacante.pelearUnRound(movimiento)(oponente))
                                                       }

      }
    
}

object NoSePuedeGenerarPlanException extends Exception

abstract class EstadoDeLucha[+E<:Especie]{
  def energiaOriginal[F<:Especie](guerrero: Guerrero[F]) = guerrero.energiaMaxima
}

case object Luchando extends EstadoDeLucha
case object Inconsciente extends EstadoDeLucha
case object Muerto extends EstadoDeLucha

abstract class EstadoSaiyajing[+F<:Saiyajin] extends EstadoDeLucha[Saiyajin]{
 
  def energiaOriginal[E<:Especie](guerrero: Guerrero[E]): Int
}

case class SuperSaiyajin(nivel: Int, energiaNormal: Int) extends EstadoSaiyajing{
  override def energiaOriginal[E<:Especie](guerrero: Guerrero[E]) = energiaNormal
}

case class MonoGigante(energiaNormal: Int) extends EstadoSaiyajing{
  override def energiaOriginal[E<:Especie](guerrero: Guerrero[E]) = energiaNormal
}



object ResultadoPelea {
  
  def apply(combatientes:Combatientes[Especie,Especie]):ResultadoPelea = {
    val (atacante, oponente) = combatientes
    combatientes.map(_.estado) match {
      case ( _ , Muerto ) => Ganador(atacante)
      case ( Muerto, _ ) => Ganador(oponente)
      case _ => PeleaEnCurso(atacante, oponente)
    }
  }
}



trait ResultadoPelea {
    
    def map(f: Combatientes[Especie,Especie] => Combatientes[Especie,Especie]): ResultadoPelea
    
    def filter(f: Combatientes[Especie,Especie] => Boolean): ResultadoPelea
    
    def flatMap(f: Combatientes[Especie,Especie] => ResultadoPelea): ResultadoPelea
    
    def fold[T](seed: T)(f: Combatientes[Especie,Especie] => T) : T
    
}
  
case class Ganador(guerrero: Guerrero[Especie]) extends ResultadoPelea {
  
    def map(f: Combatientes[Especie,Especie] => Combatientes[Especie,Especie]) = this
    
    def filter(f: Combatientes[Especie,Especie] => Boolean) = this
    
    def flatMap(f: Combatientes[Especie,Especie] => ResultadoPelea) = this
    
    def fold[T](semilla: T)(f: Combatientes[Especie,Especie] => T) = semilla
}
  
case class PeleaEnCurso(combatientes: Combatientes[Especie,Especie]) extends ResultadoPelea {
  
  def map(luchar: Combatientes[Especie,Especie] => Combatientes[Especie,Especie]) = ResultadoPelea(luchar(combatientes))

  def filter(f: Combatientes[Especie,Especie] => Boolean) = ResultadoPelea(combatientes)
  
  def flatMap(f: Combatientes[Especie,Especie] => ResultadoPelea) = for( combatientesResultantes <- f(combatientes)) 
                                                   yield combatientesResultantes
  
  def fold[T](semilla: T)(f: Combatientes[Especie,Especie] => T) = f(combatientes)
}