package utn.tadp.dragonball

case class Guerrero(
      nombre : String,
      inventario : List[Item],
      energia : Int,
      especie : Especie
      ) {
  def aumentarKi(ki: Int) = 
      copy(energia = energia + ki)
  def puedeFusionarse = especie.fusionable
 }

object Simulador {
 
  type Movimiento = (Guerrero,Guerrero)
  
  def dejarseFajar(combatientes : (Guerrero, Guerrero)):Movimiento = combatientes
  
  def cargarKi(combatientes : (Guerrero, Guerrero)):Movimiento = {
    (combatientes._1.especie, combatientes._2) match {
        case (Saiyajing(nivel), _) if nivel > 0 => (combatientes._1.aumentarKi(150* nivel), combatientes._2) 
        case (Androide, _) => (combatientes._1, combatientes._2)
        case (_, _) => (combatientes._1.aumentarKi(100), combatientes._2)
    }
  }
 
  def fusion(aliado:Guerrero,combatiente:Guerrero,oponente:Guerrero):Movimiento = {
    if(List(aliado,combatiente).forall(_.puedeFusionarse))
       (combatiente.copy(  inventario=combatiente.inventario++aliado.inventario,
                           energia=combatiente.energia + aliado.energia,
                           especie=Fusion(combatiente,aliado)),
        oponente)
    else throw new RuntimeException("No se pueden fusionar")
    }
  }
  