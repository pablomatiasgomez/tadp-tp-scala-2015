package utn.tadp.dragonball

abstract class Item

case object SemillaDelErmitaño extends Item
case object FotoDeLaLuna extends Item
case object EsferaDelDragon extends Item
case class Arma(tipo: TipoArma) extends Item
case class Municion(tipo: TipoArmaFuego) extends Item

abstract class TipoArma

case object Roma extends TipoArma
case object Filosa extends TipoArma
case class Fuego(tipo: TipoArmaFuego) extends TipoArma

trait TipoArmaFuego

case object Ak47 extends TipoArmaFuego



