package utn.tadp.dragonball

object Simulador {
 
  type Combatientes = (Guerrero, Guerrero)
  
  abstract class Movimiento(movimiento: (Combatientes => Combatientes)) extends Function[Combatientes , Combatientes ] {
    
    def apply(combatientes: Combatientes) = {
      val(atacante, oponente) = combatientes
      (atacante.estado, movimiento) match {
        case (Muerto, _) => combatientes
        case (Inconsciente, UsarItem(SemillaDelErmitaño)) => combatientes
        case (Inconsciente, _) => combatientes
        case (Luchando, _) => movimiento(combatientes)
        case (Fajado(rounds), DejarseFajar) => movimiento((atacante.estas(Fajado(rounds+1)), oponente))
        case (Fajado(_), _) => movimiento((atacante.estas(Luchando), oponente))
      }
    }
    
  }
  
  abstract class AutoMovimiento(autoMovimiento: (Guerrero => Guerrero)) 
                   extends Movimiento({case (self,otro) => (autoMovimiento(self),otro)})
  
  case object DejarseFajar extends AutoMovimiento((guerrero: Guerrero) => guerrero)
  
  case object CargarKi extends AutoMovimiento ((guerrero: Guerrero) => {
    
    guerrero.especie match {
      case Saiyajing(SuperSaiyajing(nivel, _), _) => guerrero.aumentaEnergia(150* nivel) 
      case Androide => guerrero
      case _ => guerrero.aumentaEnergia(100)
      }
    }
  ) 
  
  case class UsarItem(item: Item) extends Movimiento ((combatientes: Combatientes) => {
      
    val(atacante, oponente) = combatientes
    (item, oponente.especie) match {
      case (Arma(Roma), Androide) => combatientes
      case (Arma(Roma), _) if oponente.energia < 300 => (atacante, oponente.estas(Inconsciente))
      case (Arma(Filosa), Saiyajing(MonoGigante(energiaNormal), true)) => (atacante, 
                                                                           oponente.tuEnergiaEs(1)
                                                                                   .tuEnergiaMaximaEs(energiaNormal)
                                                                                   .transformateEn(Saiyajing(Normal,false))
                                                                                   .estas(Inconsciente))
      case (Arma(Filosa), Saiyajing(_, true)) => (atacante, oponente.tuEnergiaEs(1)
                                                                    .transformateEn(Saiyajing(Normal,false)))
      case (Arma(Filosa), _) => (atacante, oponente.disminuiEnergia(atacante.energia / 100))
      case (Arma(Fuego),Humano) => (atacante, oponente.disminuiEnergia(20)) //TODO: Controlar el tema de las balas
      case (Arma(Fuego), Namekusein) if (oponente.estado == Inconsciente) => (atacante, oponente.disminuiEnergia(10))
      case (SemillaDelErmitaño, _) => (atacante.aumentaEnergia(atacante.energiaMaxima), oponente)
      case _ => combatientes
      }  
    
  })
  
  case object ComerseAlOponente extends Movimiento ((combatientes: Combatientes) => {
    //XXX Como implementariamos Majin Buu? Deberiamos revisar cuales son los poderes adquiridos y cuales no.
    val(atacante, oponente) = combatientes
    atacante.especie match {
      case Monstruo(digerir) => (digerir(combatientes), oponente.estas(Muerto)) //XXX: Oponente deberia estar muerto?
      case _ => combatientes
    }
    
  })
  
  case object ConvertirseEnMono extends AutoMovimiento ((guerrero: Guerrero) => {
    //XXX: El manejo de las energias maximas no me convence. Hay algo de la inmutabilidad que no estamos aprovechando  
    guerrero.especie match {  
      case Saiyajing(MonoGigante(_), _) => guerrero
      case Saiyajing(estadoSaiyajing, true) if (guerrero.inventario.contains(FotoDeLaLuna)) => {
        val energiaOriginal = estadoSaiyajing match {
          case Normal => guerrero.energiaMaxima
          case SuperSaiyajing(_, energiaO) => energiaO
          }
        guerrero.tuEnergiaEs(guerrero.energiaMaxima * 3) //Esto funca?, no es un estado invalido un guerrero
                 .tuEnergiaMaximaEs(guerrero.energiaMaxima * 3)//con energia mayor a su energia maxima?
                 .transformateEn(Saiyajing(MonoGigante(energiaOriginal), true))    
      } 
      case _ => guerrero
    }
    
  } )
  
  case object ConvertirseEnSaiyajing extends AutoMovimiento ((guerrero: Guerrero) => {
      
    guerrero.especie match {
      case Saiyajing(Normal, cola) => 
          guerrero.transformateEn(Saiyajing(SuperSaiyajing(1, guerrero.energiaMaxima), cola))
                   .tuEnergiaMaximaEs(guerrero.energiaMaxima * 5)
      case Saiyajing(SuperSaiyajing(nivel, energiaOriginal), cola) =>
          guerrero.transformateEn(Saiyajing(SuperSaiyajing(nivel + 1, energiaOriginal), cola))
                   .tuEnergiaMaximaEs(guerrero.energiaMaxima * 5)
      case _ => guerrero
      }
    } 
  )
  
  case class Fusion(aliado: Guerrero) extends AutoMovimiento ((guerrero: Guerrero) => {
      
    if(List(aliado, guerrero).forall(_.puedeFusionarse))
      guerrero.sumaAInventario(aliado.inventario)
               .tuEnergiaMaximaEs(guerrero.energia + aliado.energia)
               .aumentaEnergia(aliado.energia)
               .transformateEn(Fusionado(guerrero, aliado))
    else guerrero
        
  } )
 
  case class Magia(cambioDeEstado: Function1[Combatientes, Combatientes]) extends Movimiento ((combatientes: Combatientes) => {
  
    val(atacante, oponente) = combatientes
    if ((atacante.inventario.count(_.isInstanceOf[EsferaDelDragon])) == 7)  
      cambioDeEstado(combatientes)
    else
      combatientes
    
  } )
  
  case object MuchosGolpesNinja extends Movimiento ({case (atacante,oponente) => {
    
    (atacante.especie, oponente.especie) match {
      case (Humano, Androide) => (atacante.disminuiEnergia(10), oponente)
      case _ if atacante.energia < oponente.energia => (atacante.disminuiEnergia(20), oponente)
      case _  => (atacante, oponente.disminuiEnergia(20))
    }
  }
  } )
  
  case object Explotar extends Movimiento ((combatientes: Combatientes) => {
      
    val(atacante, oponente) = (combatientes._1.estas(Muerto), combatientes._2)
    (atacante.especie, oponente.especie) match {
      case (Androide, Namekusein) if atacante.energia * 3 > oponente.energia => 
          (atacante, oponente.tuEnergiaEs(1))
      case (Monstruo(_), Namekusein) if atacante.energia * 2 > oponente.energia => 
          (atacante, oponente.tuEnergiaEs(1))
      case (Androide, _) => (atacante, oponente.disminuiEnergia(atacante.energia * 3))
      case (Monstruo(_), _) => (atacante, oponente.disminuiEnergia(atacante.energia * 2))
      case _ => combatientes
    }
    
  } )
  
  case class Onda(energiaNecesaria: Int) extends Movimiento ((combatientes: Combatientes) => {
    
    val(atacante, oponente) = combatientes
    if (atacante.energia > energiaNecesaria)
      oponente.especie match {
        case Androide => (atacante.disminuiEnergia(energiaNecesaria), oponente.aumentaEnergia(energiaNecesaria * 2))
        case Monstruo(_) => (atacante.disminuiEnergia(energiaNecesaria), oponente.disminuiEnergia(energiaNecesaria / 2))
        case _ => (atacante.disminuiEnergia(energiaNecesaria), oponente.disminuiEnergia(energiaNecesaria))
      }
     else
      combatientes
    
  } )
  
  case object Genkidama extends Movimiento ((combatientes: Combatientes) => {
    
    val(atacante, oponente) = combatientes
    atacante.estado match {
      case Fajado(rounds) => (atacante.estas(Luchando), oponente.disminuiEnergia(10^rounds))
      case _ => combatientes
    }
    
  } )

}