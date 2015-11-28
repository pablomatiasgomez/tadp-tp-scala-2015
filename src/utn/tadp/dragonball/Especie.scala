package utn.tadp.dragonball

import utn.tadp.dragonball.Simulador._

abstract class Especie

trait Magico
trait Fusionable

case class Humano() extends Especie with Fusionable
case class Androide() extends Especie
case class Namekusein() extends Especie with Fusionable with Magico
case class Fusionado(miembros: Combatientes[Especie,Especie]) extends Especie
case class Monstruo(digerir: Combatientes[Especie,Especie] => Guerrero[Especie])extends Especie with Magico
case class Saiyajin(cola:Boolean) extends Especie with Fusionable

