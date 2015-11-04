package utn.tadp.dragonball

abstract class Especie(val fusionable:Boolean=false){

}

case object Humano extends Especie(fusionable=true)
case object Androide extends Especie
case object Namekusein extends Especie(fusionable=true)
case class  Fusionado(miembros:(Guerrero,Guerrero)) extends Especie
case class Monstruo(
  digerir : Function1[(Guerrero, Guerrero), Guerrero]
  )extends Especie
case class Saiyajing(
  estado : EstadoSaiyajing,
  cola : Boolean
  ) extends Especie(fusionable=true)

abstract class EstadoSaiyajing{
  def energiaOriginal(guerrero:Guerrero):Int
  def proxNivelSSJ = 1
}

case object Normal extends EstadoSaiyajing{
  def energiaOriginal(guerrero:Guerrero) = guerrero.energiaMaxima
}
case class SuperSaiyajing(nivel: Int, energiaNormal: Int) extends EstadoSaiyajing{
  override def proxNivelSSJ = nivel + 1
  def energiaOriginal(guerrero:Guerrero) = energiaNormal
}
case class MonoGigante(energiaNormal: Int) extends EstadoSaiyajing{
  def energiaOriginal(guerrero:Guerrero) = energiaNormal
}
