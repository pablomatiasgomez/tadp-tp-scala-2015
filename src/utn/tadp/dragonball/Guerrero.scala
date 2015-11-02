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
        case Saiyajing(SuperSaiyajing(nivel, _), _) => (combatientes._1.aumentarKi(150* nivel), combatientes._2) 
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
        case (Arma(Filosa), Saiyajing(MonoGigante(energiaNormal), cola)) 
            if cola => (combatientes._1, combatientes._2.copy(energia = 1, 
                                                              especie = Saiyajing(Normal,false),
                                                              energiaMaxima = energiaNormal)) //TODO: Dejar inconsciente
        case (Arma(Filosa), Saiyajing(_, cola)) 
            if cola => (combatientes._1, combatientes._2.copy(energia = 1, especie = Saiyajing(Normal,false)))
        case (Arma(Filosa), _) => (combatientes._1, combatientes._2.disminuiKi(combatientes._1.energia / 100))
        case (Arma(Fuego),Humano) => (combatientes._1, combatientes._2.disminuiKi(20))
        case (Arma(Fuego), Namekusein) =>(combatientes._1, combatientes._2.disminuiKi(10)) //TODO: Solo si esta inconsciente
        case (SemillaDelEmitaño, _) => (combatientes._1.aumentarKi(combatientes._1.energiaMaxima), combatientes._2)
        case _ => combatientes
      }  
    }
    
  }
  
  case object comerseAlOponente extends Movimiento {
    
    def apply(combatientes: (Guerrero, Guerrero)) = {
      combatientes._1.especie match {
        case Monstruo(digerir) => (digerir(combatientes), combatientes._2) //TODO: Devolver al segundo muerto?
        case _ => combatientes
      }
    }
    
  }
  
  case object convertirseEnMono extends Movimiento {
    //XXX: El manejo de las energias maximas no me convence. Hay algo de la inmutabilidad que no estamos aprovechando
    def apply (combatientes: (Guerrero, Guerrero)) = {
      combatientes._1.especie match {  
        case Saiyajing(MonoGigante(_), _) => combatientes
        case Saiyajing(estadoSaiyajing, cola) if (cola && combatientes._1.inventario.contains(FotoDeLaLuna)) => {
            val energiaOriginal = estadoSaiyajing match {
              case Normal => combatientes._1.energiaMaxima
              case SuperSaiyajing(_, energiaO) => energiaO
            }
            (combatientes._1.copy(especie = Saiyajing(MonoGigante(energiaOriginal), true), 
                                energia = combatientes._1.energiaMaxima * 3,
                                energiaMaxima = combatientes._1.energiaMaxima * 3),
            combatientes._2)    
         } 
        case _ => combatientes
      }
    }
    
  }
  
  case object convertirseEnSaiyajing extends Movimiento {
    
    def apply (combatientes: (Guerrero, Guerrero)) = {
      combatientes._1.especie match {
        case Saiyajing(Normal, cola) => (combatientes._1.copy(especie = Saiyajing(SuperSaiyajing(1, combatientes._1.energiaMaxima), cola),
                                                           energiaMaxima = combatientes._1.energiaMaxima * 5),
                                      combatientes._2)
        case Saiyajing(SuperSaiyajing(nivel, energiaOriginal), cola) =>
          (combatientes._1.copy(especie = Saiyajing(SuperSaiyajing(nivel + 1, energiaOriginal), cola),
                                                    energiaMaxima = energiaOriginal * nivel * 5),
                                      combatientes._2)
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
 
  case class magia(cambioDeEstado: Function1[(Guerrero, Guerrero), (Guerrero, Guerrero)]) extends Movimiento {
    
    def apply (combatientes: (Guerrero, Guerrero)) = {
      if ((combatientes._1.inventario.count { item => item.isInstanceOf[EsferaDelDragon] }) == 7)  
        cambioDeEstado(combatientes)
      else
        combatientes
    }
    
  }
  
  case object muchosGolpesNinja extends Movimiento {
    
    def apply(combatientes: (Guerrero, Guerrero)) = {
      (combatientes._1.especie, combatientes._2.especie) match {
        case (Humano, Androide) => (combatientes._1.disminuiKi(10), combatientes._2)
        case _ if combatientes._1.energia < combatientes._2.energia => (combatientes._1.disminuiKi(20), combatientes._2)
        case _ if combatientes._1.energia > combatientes._2.energia => (combatientes._1, combatientes._2.disminuiKi(20))
      }
    }
    
  }
  
  case object explotar extends Movimiento {
    
    def apply(combatientes: (Guerrero, Guerrero)) = {
      (combatientes._1.especie, combatientes._2.especie) match { //TODO: Devolver Muerto al primero
        case (Androide, Namekusein) if combatientes._1.energia * 3 > combatientes._2.energia => 
          (combatientes._1, combatientes._2.copy(energia = 1))
        case (Monstruo(_), Namekusein) if combatientes._1.energia * 2 > combatientes._2.energia => 
          (combatientes._1, combatientes._2.copy(energia = 1))
        case (Androide, _) => (combatientes._1, combatientes._2.disminuiKi(combatientes._1.energia * 3))
        case (Monstruo(_), _) => (combatientes._1, combatientes._2.disminuiKi(combatientes._1.energia * 2))
        case _ => throw new RuntimeException("Sólo los monstruos y los androides pueden explotar")
      }
    }
    
  }
  
  case class onda(energiaNecesaria: Int) extends Movimiento {
    
    def apply(combatientes: (Guerrero, Guerrero)) = {
      if (combatientes._1.energia > energiaNecesaria)
        combatientes._2.especie match {
          case Androide => (combatientes._1.disminuiKi(energiaNecesaria), combatientes._2.aumentarKi(energiaNecesaria * 2))
          case Monstruo(_) => (combatientes._1.disminuiKi(energiaNecesaria), combatientes._2.disminuiKi(energiaNecesaria / 2))
          case _ => (combatientes._1.disminuiKi(energiaNecesaria), combatientes._2.disminuiKi(energiaNecesaria))
      }
      else
        throw new RuntimeException("No posee energia suficiente para lanzar la onda")
    }
    
  }
  
  case object genkidama extends Movimiento {
    
    def apply(combatientes: (Guerrero, Guerrero)) = {
      ???
    }
    
  }


}
