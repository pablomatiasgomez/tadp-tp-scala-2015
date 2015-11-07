package utn.tadp.dragonball

abstract class Especie

case object Humano extends Especie
case object Androide extends Especie
case object Namekusein extends Especie
case class  Fusionado(miembros:(Guerrero,Guerrero)) extends Especie
case class Monstruo(digerir: Function1[(Guerrero, Guerrero), Guerrero])extends Especie
case class Saiyajin(
  estado: EstadoSaiyajing,
  cola: Boolean
  ) extends Especie

abstract class EstadoSaiyajing{
  
  def energiaOriginal(guerrero: Guerrero): Int
  def proxNivelSSJ = 1

}

case object Normal extends EstadoSaiyajing{
  def energiaOriginal(guerrero: Guerrero) = guerrero.energiaMaxima
}

case class SuperSaiyajin(nivel: Int, energiaNormal: Int) extends EstadoSaiyajing{
  override def proxNivelSSJ = nivel + 1
  def energiaOriginal(guerrero: Guerrero) = energiaNormal
}

case class MonoGigante(energiaNormal: Int) extends EstadoSaiyajing{
  def energiaOriginal(guerrero: Guerrero) = energiaNormal
}
