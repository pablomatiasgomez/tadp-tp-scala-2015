package utn.tadp.dragonball

case class Guerrero(
      nombre: String,
      inventario: List[Item],
      energiaMaxima: Int,
      energia: Int,
      especie: Especie
      ) {
  def aumentarKi(ki: Int) = copy(energia = (energia + ki).min(energiaMaxima))
  
  def disminuiKi(ki: Int) = copy(energia = (energia - ki).max(0))

  def puedeFusionarse = especie.fusionable
  
}

object Simulador {
 
  type Movimiento = Function[(Guerrero, Guerrero), (Guerrero, Guerrero)]
  
  case object dejarseFajar extends Movimiento {
  
    def apply(combatientes: (Guerrero, Guerrero)) = combatientes
  
  }
  
  case object cargarKi extends Movimiento {
    
    def apply(combatientes: (Guerrero, Guerrero)) = {
      combatientes._1.especie match {
        case Saiyajing(nivel, _) if nivel > 0 => (combatientes._1.aumentarKi(150* nivel), combatientes._2) 
        case Androide => combatientes
        case _ => (combatientes._1.aumentarKi(100), combatientes._2)
      }  
    }
    
  }  
  
  case class usarItem(item: Item) extends Movimiento {
    
    def apply(combatientes: (Guerrero, Guerrero)) = {
      (item, combatientes._2.especie) match {
        case (Arma(Roma), Androide) => combatientes
        case (Arma(Roma), _) if combatientes._2.energia < 300 => (combatientes._1, combatientes._2) //TODO: Dejar inconsciente
        case (Arma(Filosa), Saiyajing(_, cola)) 
            if cola => (combatientes._1, combatientes._2.copy(energia = 1, especie = Saiyajing(0,false)))
        case (Arma(Filosa), _) => (combatientes._1, combatientes._2.disminuiKi(combatientes._1.energia / 100))
        case (Arma(Fuego),Humano) => (combatientes._1, combatientes._2.disminuiKi(20))
        case (Arma(Fuego), Namekusein) =>(combatientes._1, combatientes._2.disminuiKi(10)) //TODO: Solo si esta inconsciente
        case (SemillaDelEmitaño, _) => (combatientes._1.aumentarKi(combatientes._1.energiaMaxima), combatientes._2)
        case _ => combatientes
      }  
    }
    
  }
  
  case class fusion(aliado: Guerrero) extends Movimiento {
    
    def apply(combatientes: (Guerrero, Guerrero)) = {
      
      if(List(aliado, combatientes._1).forall(_.puedeFusionarse))
       (combatientes._1.copy(  inventario = combatientes._1.inventario++aliado.inventario,
                        energia = combatientes._1.energia + aliado.energia,
                        especie = Fusion((combatientes._1, aliado))),
        combatientes._2)
      else throw new RuntimeException("No se pueden fusionar")  
    }
    
  }
 
  
  

}
