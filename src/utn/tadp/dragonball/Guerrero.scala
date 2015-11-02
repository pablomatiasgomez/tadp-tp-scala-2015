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
      val(atacante, oponente) = combatientes
      atacante.especie match {
        case Saiyajing(SuperSaiyajing(nivel, _), _) => (atacante.aumentarKi(150* nivel), oponente) 
        case Androide => combatientes
        case _ => (atacante.aumentarKi(100), oponente)
      }  
    }
    
  }  
  
  case class usarItem(item: Item) extends Movimiento {
    
    def apply(combatientes: (Guerrero, Guerrero)) = {
      val(atacante, oponente) = combatientes
      (item, oponente.especie) match {
        case (Arma(Roma), Androide) => combatientes
        case (Arma(Roma), _) if oponente.energia < 300 => (atacante, oponente) //TODO: Dejar inconsciente oponente
        case (Arma(Filosa), Saiyajing(MonoGigante(energiaNormal), true)) =>
              (atacante, oponente.copy(energia = 1, 
                especie = Saiyajing(Normal,false),
                energiaMaxima = energiaNormal)) //TODO: Dejar inconsciente oponente
        case (Arma(Filosa), Saiyajing(_, true)) =>
              (atacante, oponente.copy(energia = 1, especie = Saiyajing(Normal,false)))
        case (Arma(Filosa), _) => (atacante, oponente.disminuiKi(atacante.energia / 100))
        case (Arma(Fuego),Humano) => (atacante, oponente.disminuiKi(20))
        case (Arma(Fuego), Namekusein) =>(atacante, oponente.disminuiKi(10)) //TODO: Solo si esta inconsciente
        case (SemillaDelEmitaño, _) => (atacante.aumentarKi(atacante.energiaMaxima), oponente)
        case _ => combatientes
      }  
    }
    
  }
  
  case object comerseAlOponente extends Movimiento {

    def apply(combatientes: (Guerrero, Guerrero)) = {
      val(atacante, oponente) = combatientes
      atacante.especie match {
        case Monstruo(digerir) => (digerir(combatientes), oponente) //TODO: Devolver al segundo muerto?
        case _ => combatientes
      }
    }
    
  }
  
  case object convertirseEnMono extends Movimiento {
    //XXX: El manejo de las energias maximas no me convence. Hay algo de la inmutabilidad que no estamos aprovechando
    def apply (combatientes: (Guerrero, Guerrero)) = {
      val(atacante, oponente) = combatientes
      atacante.especie match {  
        case Saiyajing(MonoGigante(_), _) => combatientes
        case Saiyajing(estadoSaiyajing, true) if (atacante.inventario.contains(FotoDeLaLuna)) => {
            val energiaOriginal = estadoSaiyajing match {
              case Normal => atacante.energiaMaxima
              case SuperSaiyajing(_, energiaO) => energiaO
            }
            (atacante.copy(especie = Saiyajing(MonoGigante(energiaOriginal), true), 
                                energia = atacante.energiaMaxima * 3,
                                energiaMaxima = atacante.energiaMaxima * 3),
            oponente)    
         } 
        case _ => combatientes
      }
    }
    
  }
  
  case object convertirseEnSaiyajing extends Movimiento {
    
    def apply (combatientes: (Guerrero, Guerrero)) = {
      val(atacante, oponente) = combatientes
      atacante.especie match {
        case Saiyajing(Normal, cola) => (atacante.copy(especie = Saiyajing(SuperSaiyajing(1, atacante.energiaMaxima), cola),
                                                           energiaMaxima = atacante.energiaMaxima * 5),
                                      oponente)
        case Saiyajing(SuperSaiyajing(nivel, energiaOriginal), cola) =>
          (atacante.copy(especie = Saiyajing(SuperSaiyajing(nivel + 1, energiaOriginal), cola),
                                                    energiaMaxima = energiaOriginal * nivel * 5),
                                      oponente)
        case _ => combatientes
      }
    }
    
  }
  
  case class fusion(aliado: Guerrero) extends Movimiento {
    
    def apply(combatientes: (Guerrero, Guerrero)) = {
      val(atacante, oponente) = combatientes
      if(List(aliado, atacante).forall(_.puedeFusionarse))
       (atacante.copy(  inventario = atacante.inventario++aliado.inventario,
                        energia = atacante.energia + aliado.energia,
                        especie = Fusion((atacante, aliado))),
        oponente)
      else throw new RuntimeException("No se pueden fusionar")  
    }
    
  }
 
  case class magia(cambioDeEstado: Function1[(Guerrero, Guerrero), (Guerrero, Guerrero)]) extends Movimiento {
    
    def apply (combatientes: (Guerrero, Guerrero)) = {
      val(atacante, oponente) = combatientes
      if ((atacante.inventario.count { item => item.isInstanceOf[EsferaDelDragon] }) == 7)  
        cambioDeEstado(combatientes)
      else
        combatientes
    }
    
  }
  
  case object muchosGolpesNinja extends Movimiento {
    
    def apply(combatientes: (Guerrero, Guerrero)) = {
      val(atacante, oponente) = combatientes
      (atacante.especie, oponente.especie) match {
        case (Humano, Androide) => (atacante.disminuiKi(10), oponente)
        case _ if atacante.energia < oponente.energia => (atacante.disminuiKi(20), oponente)
        case _  => (atacante, oponente.disminuiKi(20))
      }
    }
    
  }
  
  case object explotar extends Movimiento {
    
    def apply(combatientes: (Guerrero, Guerrero)) = {
      val(atacante, oponente) = combatientes
      (atacante.especie, oponente.especie) match { //TODO: Devolver Muerto al primero
        case (Androide, Namekusein) if atacante.energia * 3 > oponente.energia => 
          (atacante, oponente.copy(energia = 1))
        case (Monstruo(_), Namekusein) if atacante.energia * 2 > oponente.energia => 
          (atacante, oponente.copy(energia = 1))
        case (Androide, _) => (atacante, oponente.disminuiKi(atacante.energia * 3))
        case (Monstruo(_), _) => (atacante, oponente.disminuiKi(atacante.energia * 2))
        case _ => throw new RuntimeException("Sólo los monstruos y los androides pueden explotar")
      }
    }
    
  }
  
  case class onda(energiaNecesaria: Int) extends Movimiento {
    
    def apply(combatientes: (Guerrero, Guerrero)) = {
      val(atacante, oponente) = combatientes
      if (atacante.energia > energiaNecesaria)
        oponente.especie match {
          case Androide => (atacante.disminuiKi(energiaNecesaria), oponente.aumentarKi(energiaNecesaria * 2))
          case Monstruo(_) => (atacante.disminuiKi(energiaNecesaria), oponente.disminuiKi(energiaNecesaria / 2))
          case _ => (atacante.disminuiKi(energiaNecesaria), oponente.disminuiKi(energiaNecesaria))
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
