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
  
}

abstract class EstadoDeLucha(){
  
}

case object Luchando extends EstadoDeLucha
case class Fajado(rounds: Int) extends EstadoDeLucha
case object Inconsciente extends EstadoDeLucha
case object Muerto extends EstadoDeLucha

