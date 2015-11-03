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
  
  def tuEnergiaEs(nuevaEnergia: Int) = copy (energia = nuevaEnergia)
  
  def tuEnergiaMaximaEs(nuevoMaximo: Int) = copy (energiaMaxima = nuevoMaximo)
  
  def aumentaEnergia(aumento: Int) = copy(energia = (energia + aumento).min(energiaMaxima))
  
  def disminuiEnergia(disminucion: Int) = {
    /*if (energia > disminucion)
      copy(energia = (energia - disminucion))
    else
      copy(energia = 0).estas(Muerto)*/
      (energia-disminucion).max(0) match {
      case 0 => tuEnergiaEs(0).estas(Muerto)
      case n => tuEnergiaEs(n)
    }
  }

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