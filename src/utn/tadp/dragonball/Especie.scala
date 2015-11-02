package utn.tadp.dragonball

abstract class Especie(val fusionable:Boolean=false){

}

case object Humano extends Especie(true)
case object Androide extends Especie
case object Namekusein extends Especie
case class  Fusion(miembros:(Guerrero,Guerrero)) extends Especie
case class Monstruo(
  digerir : Function1[(Guerrero, Guerrero), Guerrero]
  )extends Especie
case class Saiyajing(
  estado : EstadoSaiyajing,
  cola : Boolean
  ) extends Especie

abstract class EstadoSaiyajing(){
  
}

case object Normal extends EstadoSaiyajing
case class SuperSaiyajing(nivel: Int, energiaNormal: Int) extends EstadoSaiyajing
case class MonoGigante(energiaNormal: Int) extends EstadoSaiyajing
