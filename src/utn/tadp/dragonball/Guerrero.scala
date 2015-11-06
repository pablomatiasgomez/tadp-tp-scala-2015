package utn.tadp.dragonball
import utn.tadp.dragonball.Simulador._
import utn.tadp.dragonball.BlackMagic._

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
    
  def variarEnergia(f:(Int=>Int)) = {
    val guerreroAfectado = copy(energia = f(energia).max(0).min(energiaMaxima))
    if(guerreroAfectado.energia == 0) guerreroAfectado.estas(Muerto)
    else guerreroAfectado
  }
  
  def tuEnergiaEs(nuevaEnergia: Int) = variarEnergia( _ => nuevaEnergia )
  
  def variarEnergiaMaxima(f:(Int=>Int)) = copy(energiaMaxima = f(energiaMaxima))
  
  def tuEnergiaMaximaEs(nuevoMaximo: Int) = variarEnergiaMaxima ( _ => nuevoMaximo)
  
  def aumentaEnergia(aumento: Int) = variarEnergia( aumento + )
  
  def disminuiEnergia(disminucion: Int) = aumentaEnergia( -disminucion )

  def transformateEn(transformacion: Especie) = copy(especie = transformacion)
  
  def cargarAlMaximo = tuEnergiaEs(energiaMaxima)
  
  def estas(nuevoEstado: EstadoDeLucha): Guerrero = {
    (nuevoEstado, especie) match {
      case (Muerto, Fusionado((original, _))) => original estas Muerto
      case (Inconsciente, Fusionado((original, _))) => original estas Inconsciente
      case (Inconsciente, Saiyajin(SuperSaiyajin(_, energiaOriginal), cola)) => (this transformateEn Saiyajin(Normal, cola)
                                                                                      tuEnergiaMaximaEs energiaOriginal
                                                                                      estas Inconsciente)
      case _ => copy(estado = nuevoEstado)
    }
  }
  def variarInventario(f:(List[Item]=>List[Item])) = copy(inventario = f(inventario))
  
  def gastarItems(itemsUsados: List[Item]) = variarInventario( _ diff itemsUsados)
  
  def sumaAInventario(agregados: List[Item]) = variarInventario( _ ++ agregados )
  
  def puedeFusionarse = especie.fusionable
  
  type CriterioDeCombate = Combatientes => Double
  
  type PlanDeAtaque = List[Movimiento]
  
  def movimientoMasEfectivoContra(oponente: Guerrero)(criterio: CriterioDeCombate): Movimiento = {
    val combatientes = (this,oponente)
    val mejorMovimiento = movimientos.maxBy(movimiento => criterio( movimiento( combatientes)))
    if( criterio (mejorMovimiento (combatientes) )  > 0)
      mejorMovimiento
    else
      //throw new RuntimeException("No hay un movimiento satisfactorio") no deberiamos tirar excepcion
      DejarseFajar
  }
  
  def atacarSegun(criterioDeCombate:CriterioDeCombate): (Guerrero=>Combatientes) = guerrero => 
              this.movimientoMasEfectivoContra(guerrero)(criterioDeCombate)(this,guerrero)
  
  def contraAtacarA(guerrero:Guerrero): Combatientes =
              this.atacarSegun(mayorVentajaDeKi)(guerrero)
  

  def mayorVentajaDeKi(combatientes: Combatientes):Double =  {
    if(combatientes._2.energia == 0) mayorVentajaDeKi(combatientes.map( _ aumentaEnergia 1 ))  
    else combatientes.map( _.energia ).fold1( _ porcentajeDe _ )
}
  
  def pelearUnRound(movimiento: Movimiento)(oponente: Guerrero): Combatientes = {
    
    val (thisFajado,oponenteFajado) = movimiento(this, oponente)
    oponenteFajado.contraAtacarA(thisFajado).swap
  
  }
  
  def planDeAtaque(oponente: Guerrero, rounds: Int)(criterio: CriterioDeCombate): PlanDeAtaque = {
    
    val (planVacio,combatientes) = (List():PlanDeAtaque,(this,oponente))
    
    List.range(0, rounds).foldLeft((planVacio,combatientes))( (semilla, _) => {
        val (plan, (atacante, oponente)) = semilla
        val mejorMovimiento = atacante.movimientoMasEfectivoContra(oponente)(criterio)
        
        (plan :+ mejorMovimiento, atacante.pelearUnRound(mejorMovimiento)(oponente))
        
      })._1
      
  }
   
  
  def pelearContra(oponente: Guerrero)(plan: List[Movimiento]): ResultadoPelea = {
    
    
  val peleaEnCurso : ResultadoPelea = (this, oponente).definirResultado
  plan.foldLeft(peleaEnCurso)((pelea, movimiento) => pelea.map( {case (atacante,adversario) =>
                                                            atacante.pelearUnRound(movimiento)(adversario)} ))
        
  }
}

abstract class EstadoDeLucha

case object Luchando extends EstadoDeLucha
case class Fajado(rounds: Int) extends EstadoDeLucha
case object Inconsciente extends EstadoDeLucha
case object Muerto extends EstadoDeLucha

trait ResultadoPelea {
    
    def map(f: Combatientes=>Combatientes): ResultadoPelea
    
}
  
case class Ganador(guerrero: Guerrero) extends ResultadoPelea {
  
    def map(f: Combatientes=>Combatientes) = Ganador(guerrero)
    
}
  
case class PeleaEnCurso(combatientes: Combatientes) extends ResultadoPelea {
  
  def map(f: Combatientes=>Combatientes) = f(combatientes) definirResultado
  
}