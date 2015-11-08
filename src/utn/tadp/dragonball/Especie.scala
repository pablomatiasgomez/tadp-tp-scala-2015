package utn.tadp.dragonball

import utn.tadp.dragonball.Simulador._

abstract class Especie

trait Magico
trait Fusionable

case object Humano extends Especie with Fusionable
case object Androide extends Especie
case object Namekusein extends Especie with Fusionable with Magico
case class  Fusionado(miembros: Combatientes) extends Especie
case class Monstruo(digerir: Combatientes => Guerrero)extends Especie with Magico
case class Saiyajin(
  estado: EstadoSaiyajing,
  cola: Boolean
  ) extends Especie with Fusionable

abstract class EstadoSaiyajing{
  
  def energiaOriginal(guerrero: Guerrero): Int
  def proxNivelSSJ = 1

}

case object Normal extends EstadoSaiyajing{
  def energiaOriginal(guerrero: Guerrero) = guerrero.energiaMaxima
}

case class SuperSaiyajin(nivel: Int, energiaNormal: Int) extends EstadoSaiyajing{
  override def proxNivelSSJ = nivel + 1
  def energiaOriginal(guerrero: Guerrero) = energiaNormal
}

case class MonoGigante(energiaNormal: Int) extends EstadoSaiyajing{
  def energiaOriginal(guerrero: Guerrero) = energiaNormal
}
