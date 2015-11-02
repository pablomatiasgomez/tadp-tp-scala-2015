package utn.tadp.dragonball

abstract class Especie(){
  
}

case object Humano extends Especie
case class Saiyajing(
  nivel : Int  
  ) extends Especie
case object Androide extends Especie
case object Namekusein extends Especie
case object Monstruo extends Especie