package utn.tadp.dragonball

abstract class Especie(val fusionable:Boolean=false){
  
}

case object Humano extends Especie(true)
case class Saiyajing(
  nivel : Int  
  ) extends Especie(true)
case object Androide extends Especie
case object Namekusein extends Especie(true)
case object Monstruo extends Especie
case class  Fusion(miembros:(Guerrero,Guerrero)) extends Especie