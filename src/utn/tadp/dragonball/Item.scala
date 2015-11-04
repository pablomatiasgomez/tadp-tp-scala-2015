package utn.tadp.dragonball

abstract class Item(){
  
}

abstract class TipoArma(){
  
}

trait TipoArmaFuego

case object Roma extends TipoArma
case object Filosa extends TipoArma
case class Fuego(tipo: TipoArmaFuego) extends TipoArma

case class Municion(tipo: TipoArmaFuego) extends Item

case object Ak47 extends TipoArmaFuego

case class Arma(tipo : TipoArma) extends Item
case object SemillaDelErmitaño extends Item
case object FotoDeLaLuna extends Item
case object EsferaDelDragon extends Item