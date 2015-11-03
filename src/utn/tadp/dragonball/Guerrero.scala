package utn.tadp.dragonball

case class Guerrero(
      nombre: String,
      inventario: List[Item],
      energiaMaxima: Int,
      energia: Int,
      especie: Especie,
      estado: EstadoDeLucha,
      movimientos: List[Simulador.Movimiento]
      ) {
  
  def tuEnergiaEs(nuevaEnergia: Int) = copy (energia = nuevaEnergia)
  
  def tuEnergiaMaximaEs(nuevoMaximo: Int) = copy (energiaMaxima = nuevoMaximo)
  
  def aumentaEnergia(aumento: Int) = copy(energia = (energia + aumento).min(energiaMaxima))
  
  def disminuiEnergia(disminucion: Int) = {
    if (energia > disminucion)
      copy(energia = (energia - disminucion))
    else
      copy(energia = 0).estas(Muerto)
  }

  def transformateEn(transformacion: Especie) = copy(especie = transformacion)
  
  def estas(nuevoEstado: EstadoDeLucha) : Guerrero = {
    (nuevoEstado, especie) match {
      case (Muerto, Fusionado((original, _))) => original.estas(Muerto)
      case (Inconsciente, Fusionado((original, _))) => original.estas(Inconsciente)
      case _ => copy(estado = nuevoEstado)
    }
  }
  
  def sumaAInventario(agregados: List[Item]) = copy(inventario = inventario++agregados)
  
  def puedeFusionarse = especie.fusionable
  
  type CriterioDeCombate = Simulador.Combatientes => Int
  
  def movimientoMasEfectivoContra(oponente: Guerrero)(criterio: CriterioDeCombate) : Simulador.Movimiento = {
    
    val mejorMovimiento = movimientos.maxBy { movimiento => criterio(movimiento((this, oponente))) }
    if(criterio(mejorMovimiento((this, oponente))) > 0)
      mejorMovimiento
    else
      throw new RuntimeException("No hay un movimiento satisfactorio")
    
  }
  
  def mayorVentajaDeKi(combatientes: Simulador.Combatientes) = {
    combatientes._2. energia - combatientes._1.energia
  }
  
  def pelearUnRound(movimiento: Simulador.Movimiento)(oponente: Guerrero) : Simulador.Combatientes = {
    
    val oponenteFajado = movimiento((this, oponente))._2
    (oponenteFajado.movimientoMasEfectivoContra(this)(mayorVentajaDeKi)((oponenteFajado, this))._2, oponenteFajado)
  
  }
  
  def planDeAtaque(oponente: Guerrero, rounds: Int)(criterio: CriterioDeCombate) : List[Simulador.Movimiento] = {
    //XXX Mutabilidad AAGGGHHHHH
    var plan = List(movimientoMasEfectivoContra(oponente)(criterio))
    var peleando = movimientoMasEfectivoContra(oponente)(criterio)(this,oponente)
    for(round <- 1 to rounds){
      plan = plan++List(movimientoMasEfectivoContra(peleando._2)(criterio))
      peleando = movimientoMasEfectivoContra(oponente)(criterio)(peleando)
    }
    plan
  }
  
}

abstract class EstadoDeLucha(){
  
}

case object Luchando extends EstadoDeLucha
case class Fajado(rounds: Int) extends EstadoDeLucha
case object Inconsciente extends EstadoDeLucha
case object Muerto extends EstadoDeLucha