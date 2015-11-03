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
  
  case object DejarseFajar extends Movimiento((combatientes: Combatientes) => combatientes)
  
  case object CargarKi extends Movimiento ((combatientes: Combatientes) => {
      
    val(atacante, oponente) = combatientes
    atacante.especie match {
      case Saiyajing(SuperSaiyajing(nivel, _), _) => (atacante.aumentaEnergia(150* nivel), oponente) 
      case Androide => combatientes
      case _ => (atacante.aumentaEnergia(100), oponente)
    }
    
  } ) 
  
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
      case (Arma(Fuego),Humano) => (atacante, oponente.disminuiEnergia(20))
      case (Arma(Fuego), Namekusein) if (oponente.estado == Inconsciente) => (atacante, oponente.disminuiEnergia(10))
      case (SemillaDelErmitaño, _) => (atacante.aumentaEnergia(atacante.energiaMaxima), oponente)
      case _ => combatientes
      }  
    
  })
  
  case object ComerseAlOponente extends Movimiento ((combatientes: Combatientes) => {
  
    val(atacante, oponente) = combatientes
    atacante.especie match {
      case Monstruo(digerir) => (digerir(combatientes), oponente.estas(Muerto)) //XXX: Oponente deberia estar muerto?
      case _ => combatientes
    }
    
  })
  
  case object ConvertirseEnMono extends Movimiento ((combatientes: Combatientes) => {
    //XXX: El manejo de las energias maximas no me convence. Hay algo de la inmutabilidad que no estamos aprovechando  
    val(atacante, oponente) = combatientes
    atacante.especie match {  
      case Saiyajing(MonoGigante(_), _) => combatientes
      case Saiyajing(estadoSaiyajing, true) if (atacante.inventario.contains(FotoDeLaLuna)) => {
        val energiaOriginal = estadoSaiyajing match {
          case Normal => atacante.energiaMaxima
          case SuperSaiyajing(_, energiaO) => energiaO
          }
        (atacante.tuEnergiaEs(atacante.energiaMaxima * 3)
                 .tuEnergiaMaximaEs(atacante.energiaMaxima * 3)
                 .transformateEn(Saiyajing(MonoGigante(energiaOriginal), true)),
        oponente)    
      } 
      case _ => combatientes
    }
    
  } )
  
  case object ConvertirseEnSaiyajing extends Movimiento ((combatientes: Combatientes) => {
      
    val(atacante, oponente) = combatientes
    atacante.especie match {
      case Saiyajing(Normal, cola) => 
          (atacante.transformateEn(Saiyajing(SuperSaiyajing(1, atacante.energiaMaxima), cola))
                   .tuEnergiaMaximaEs(atacante.energiaMaxima * 5),
            oponente)
      case Saiyajing(SuperSaiyajing(nivel, energiaOriginal), cola) =>
          (atacante.transformateEn(Saiyajing(SuperSaiyajing(nivel + 1, energiaOriginal), cola))
                   .tuEnergiaMaximaEs(atacante.energiaMaxima * 5),
           oponente)
      case _ => combatientes
    
    }
    
  } )
  
  case class Fusion(aliado: Guerrero) extends Movimiento ((combatientes: Combatientes) => {
      
    val(atacante, oponente) = combatientes
    if(List(aliado, atacante).forall(_.puedeFusionarse))
      (atacante.sumaAInventario(aliado.inventario)
               .tuEnergiaMaximaEs(atacante.energia + aliado.energia)
               .aumentaEnergia(aliado.energia)
               .transformateEn(Fusionado(atacante, aliado)),
      oponente)
    else combatientes 
        
  } )
 
  case class Magia(cambioDeEstado: Function1[Combatientes, Combatientes]) extends Movimiento ((combatientes: Combatientes) => {
  
    val(atacante, oponente) = combatientes
    if ((atacante.inventario.count { item => item.isInstanceOf[EsferaDelDragon] }) == 7)  
      cambioDeEstado(combatientes)
    else
      combatientes
    
  } )
  
  case object MuchosGolpesNinja extends Movimiento ((combatientes: Combatientes) => {
    
      
    val(atacante, oponente) = combatientes
    (atacante.especie, oponente.especie) match {
      case (Humano, Androide) => (atacante.disminuiEnergia(10), oponente)
      case _ if atacante.energia < oponente.energia => (atacante.disminuiEnergia(20), oponente)
      case _  => (atacante, oponente.disminuiEnergia(20))
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