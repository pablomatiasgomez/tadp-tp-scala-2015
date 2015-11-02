package utn.tadp.dragonball

case class Guerrero(
      nombre : String,
      inventario : List[Item],
      energia : Int,
      especie : Especie
      ) {
  
  def aumentarKi(ki: Int) = 
      copy(energia = energia + ki)

 }

object Simulador {
 
  def dejarseFajar(combatientes : (Guerrero, Guerrero)) = combatientes
  
  def cargarKi(combatientes : (Guerrero, Guerrero)) = {
    val (guerrero,oponente) = (combatientes._1,combatientes._2)
     (combatientes._1.especie, combatientes._2) match {
      case (Saiyajing(nivel), _) if nivel > 0 => (combatientes._1.aumentarKi(150* nivel), combatientes._2) 
      case (Androide, _) => (combatientes._1, combatientes._2)
      case (_, _) => (combatientes._1.aumentarKi(100), combatientes._2)
    }
  }
  
}