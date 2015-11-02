package utn.tadp.dragonball

case class Guerrero(
      nombre : String,
      inventario : List[Item],
      energiaMaxima : Int,
      energia : Int,
      especie : Especie
      ) {
  def aumentarKi(ki: Int) = copy(energia = (energia + ki).min(energiaMaxima))
  
  def disminuiKi(ki: Int) = copy(energia = (energia - ki).max(0))

  def puedeFusionarse = especie.fusionable
  
}

object Simulador {
 
  type Movimiento = (Guerrero,Guerrero)
  
  def dejarseFajar(combatientes : (Guerrero, Guerrero)):Movimiento = combatientes
  
  def cargarKi(combatientes : (Guerrero, Guerrero)):Movimiento = {
    (combatientes._1.especie, combatientes._2) match {
        case (Saiyajing(nivel, _), _) if nivel > 0 => (combatientes._1.aumentarKi(150* nivel), combatientes._2) 
        case (Androide, _) => (combatientes._1, combatientes._2)
        case (_, _) => (combatientes._1.aumentarKi(100), combatientes._2)
    }
  }  
  
  def usarItem(item : Item, combatientes : (Guerrero, Guerrero)) = {
    val (oponente, guerrero) = (item, combatientes._2.especie) match {
      case (Arma(Roma), Androide) => (combatientes._1, combatientes._2)
      case (Arma(Roma), _) if combatientes._2.energia < 300 => (combatientes._1, combatientes._2) //TODO: Dejar inconsciente
      case (Arma(Filosa), Saiyajing(_, cola)) 
          if cola => (combatientes._1, combatientes._2. copy(energia = 1, especie = Saiyajing(0,false)))
      case (Arma(Filosa), _) => (combatientes._1, combatientes._2.disminuiKi(combatientes._1.energia / 100))
      case (Arma(Fuego),Humano) => (combatientes._1, combatientes._2.disminuiKi(20))
      case (Arma(Fuego), Namekusein) =>(combatientes._1, combatientes._2.disminuiKi(10)) //TODO: Solo si esta inconsciente
      case (SemillaDelEmitaño, _) => (combatientes._1.aumentarKi(combatientes._1.energiaMaxima), combatientes._2)
      case _ => (combatientes._1, combatientes._2)
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
