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

case class Saiyajin(cola:Boolean) extends Especie with Fusionable

