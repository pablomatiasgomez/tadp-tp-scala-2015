package utn.tadp.dragonball

abstract class Especie(val fusionable:Boolean=false){
  
}

case object Humano extends Especie(true)
case class Saiyajing(
  nivel : Int,
  cola : Boolean
  ) extends Especie
case object Androide extends Especie
case object Namekusein extends Especie
case class Monstruo(
  digerir : Function1[Guerrero, Guerrero]
  )extends Especie
case class  Fusion(miembros:(Guerrero,Guerrero)) extends Especie
