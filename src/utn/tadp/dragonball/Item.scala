package utn.tadp.dragonball

abstract class Item(){
  
}

abstract class TipoArma(){
  
}

case object Roma extends TipoArma
case object Filosa extends TipoArma
case object Fuego extends TipoArma


case class Arma(tipo : TipoArma) extends Item
case object SemillaDelEmitaño extends Item
case object FotoDeLaLuna extends Item