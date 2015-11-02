package utn.tadp.dragonball

case class Guerrero(
      nombre : String,
      inventario : List[Item],
      energiaMaxima : Int,
      energia : Int,
      especie : Especie
      ) {
  
  def aumentarKi(ki: Int) = 
      copy(energia = (energia + ki).min(energiaMaxima))
  
  def disminuiKi(ki: Int) = 
      copy(energia = (energia - ki).max(0))

 }

object Simulador {
 
  def dejarseFajar(combatientes : (Guerrero, Guerrero)) = combatientes
  
  def cargarKi(combatientes : (Guerrero, Guerrero)) = {
    (combatientes._1.especie, combatientes._2) match {
        case (Saiyajing(nivel, _), _) if nivel > 0 => (combatientes._1.aumentarKi(150* nivel), combatientes._2) 
        case (Androide, _) => (combatientes._1, combatientes._2)
        case (_, _) => (combatientes._1.aumentarKi(100), combatientes._2)
    }
  }  
  
  def usarItem(item : Item, combatientes : (Guerrero, Guerrero)) = {
    val (oponente, guerrero) = item match {
      case Arma(tipo) =>
        tipo match {
          case Roma => 
            (combatientes._2.especie, combatientes._2.energia) match {
              case (Androide, _) => (combatientes._1, combatientes._2)
              case (_, energia) if energia < 300 => (combatientes._1, combatientes._2) //TODO: Dejar Inconsciente
              case _ => (combatientes._1, combatientes._2)
            }
          case Filosa =>  combatientes._2.especie match {
            case Saiyajing(_, cola) if cola => (combatientes._1, combatientes._2. copy(energia = 1, especie = Saiyajing(0,false)))
            case _ => (combatientes._1, combatientes._2.disminuiKi(combatientes._1.energia / 100))
          }
          case Fuego => combatientes._2.especie match {
            case Humano =>(combatientes._1, combatientes._2.disminuiKi(20))
            case Namekusein =>(combatientes._1, combatientes._2.disminuiKi(10)) //TODO: Solo si esta inconsciente
            case _ =>(combatientes._1, combatientes._2)
          }
        }
      case SemillaDelEmitaño => (combatientes._1.aumentarKi(combatientes._1.energiaMaxima), combatientes._2)
    }
  }
  
}