package utn.tadp.dragonball
import utn.tadp.dragonball.Simulador._


case class Guerrero(
      nombre: String,
      inventario: List[Item],
      energiaMaxima: Int,
      energia: Int,
      especie: Especie,
      estado: EstadoDeLucha,
      movimientos: List[Movimiento]
      ) {
  
  def agregaMovimientos(agregados: List[Movimiento]) = copy(movimientos = movimientos++agregados)
      
  def variarEnergia(f:(Int=>Int)) = {
    val guerreroAfectado = copy(energia = f(energia).max(0).min(energiaMaxima))
    if(guerreroAfectado.energia == 0) guerreroAfectado.estas(Muerto)
    else guerreroAfectado
  }
  
  def tuEnergiaEs(nuevaEnergia: Int) = variarEnergia( _ => nuevaEnergia )
  
  def variarEnergiaMaxima(f:(Int=>Int)) = copy(energiaMaxima = f(energiaMaxima))
  
  def tuEnergiaMaximaEs(nuevoMaximo: Int) = variarEnergiaMaxima ( _ => nuevoMaximo)
  
  def aumentaEnergia(aumento: Int) = variarEnergia( _ + aumento )
  
  def disminuiEnergia(disminucion: Int) = aumentaEnergia( -disminucion )

  def transformateEn(transformacion: Especie) = copy(especie = transformacion)
  
  def estas(nuevoEstado: EstadoDeLucha): Guerrero = {
    (nuevoEstado, especie) match {
      case (Muerto, Fusionado((original, _))) => original.estas(Muerto)
      case (Inconsciente, Fusionado((original, _))) => original.estas(Inconsciente)
      case _ => copy(estado = nuevoEstado)
    }
  }
  
  def sumaAInventario(agregados: List[Item]) = copy(inventario = inventario++agregados)
  
  def puedeFusionarse = especie.fusionable
  
  type CriterioDeCombate = Combatientes => Int
  
  def movimientoMasEfectivoContra(oponente: Guerrero)(criterio: CriterioDeCombate): Movimiento = {
    
    val mejorMovimiento = movimientos.maxBy { movimiento => criterio(movimiento((this, oponente))) }
    if(criterio(mejorMovimiento((this, oponente))) > 0)
      mejorMovimiento
    else
      throw new RuntimeException("No hay un movimiento satisfactorio")
    
  }
  
  def mayorVentajaDeKi(combatientes: Combatientes) = {
    combatientes._2. energia - combatientes._1.energia
  }
  
  def pelearUnRound(movimiento: Movimiento)(oponente: Guerrero): Combatientes = {
    
    val oponenteFajado = movimiento((this, oponente))._2
    (oponenteFajado.movimientoMasEfectivoContra(this)(mayorVentajaDeKi)((oponenteFajado, this))._2, oponenteFajado)
  
  }
  
  def planDeAtaque(oponente: Guerrero, rounds: Int)(criterio: CriterioDeCombate): List[Movimiento] = {
    
    val (plan, combatientes) : (List[Movimiento], Combatientes)  = 
      (List(movimientoMasEfectivoContra(oponente)(criterio)),
       movimientoMasEfectivoContra(oponente)(criterio)(this,oponente)) 
    
    (1 to (rounds-1)).reverse.foldLeft((plan, combatientes))((semilla, _) => {
        val (p, (a, o)) = semilla
        (p++List(a.movimientoMasEfectivoContra(o)(criterio)),
        a.movimientoMasEfectivoContra(oponente)(criterio)(a, o))
      })._1
      
  }
  
  
  trait ResultadoPelea {
    
    def map(f: (Combatientes => Combatientes)): ResultadoPelea
    
  }
  
  case class Ganador(guerrero: Guerrero) extends ResultadoPelea {
    
    def map(f: (Combatientes => Combatientes)) = Ganador(guerrero)
    
  }
  case class PeleaEnCurso(combatientes: Combatientes) extends ResultadoPelea {
    def map(f: (Combatientes => Combatientes)) = definirResultado(f(combatientes))
  }
  
  def definirResultado(combatientes: Combatientes) = {
    
    val (atacanteFinal, oponenteFinal) = combatientes
    (atacanteFinal.estado,oponenteFinal.estado) match {
        case (Muerto, _) => Ganador(oponenteFinal)
        case (_, Muerto) => Ganador(atacanteFinal)
        case (_, _) => PeleaEnCurso((atacanteFinal, oponenteFinal))
      }   
    
  }
  
  def pelearContra(oponente: Guerrero)(plan: List[Movimiento]): ResultadoPelea = {
    
    val peleaEnCurso : ResultadoPelea = definirResultado((this, oponente))
    plan.foldLeft(peleaEnCurso)((pelea, movimiento) => { pelea.map(movimiento) } )
    
  }
  
}

abstract class EstadoDeLucha(){
  
}

case object Luchando extends EstadoDeLucha
case class Fajado(rounds: Int) extends EstadoDeLucha
case object Inconsciente extends EstadoDeLucha
case object Muerto extends EstadoDeLucha