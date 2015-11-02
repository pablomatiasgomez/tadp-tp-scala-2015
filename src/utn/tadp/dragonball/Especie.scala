package utn.tadp.dragonball

abstract class Especie(){
  
}

case object Humano extends Especie
case class Saiyajing(
  nivel : Int,
  cola : Boolean
  ) extends Especie
case object Androide extends Especie
case object Namekusein extends Especie
case class Monstruo(
  comer : Function1[Guerrero, Guerrero]
  )extends Especie