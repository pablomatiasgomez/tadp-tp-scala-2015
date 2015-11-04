package utn.tadp.dragonball

import utn.tadp.dragonball.BlackMagic._

object Simulador {
 
  type Combatientes = (Guerrero, Guerrero)
  
  abstract class Movimiento(movimiento: (Combatientes => Combatientes)) extends Function[Combatientes , Combatientes ] {
    
    def apply(combatientes: Combatientes) = {
      val(atacante, oponente) = combatientes
      (atacante.estado, this) match {
        case (Muerto, _) => combatientes
        case (Inconsciente, UsarItem(SemillaDelErmitaño)) => movimiento(combatientes)
        case (Inconsciente, _) => combatientes
        case (Luchando, DejarseFajar) => movimiento(atacante estas Fajado(1), oponente)
        case (Luchando, _) => movimiento(combatientes)
        case (Fajado(rounds), DejarseFajar) => movimiento(atacante estas Fajado(rounds+1), oponente)
        case (Fajado(_), _) => movimiento(atacante estas Luchando, oponente)
      }
    }
    
  }
  
  abstract class AutoMovimiento(autoMovimiento: (Guerrero => Guerrero)) 
                   extends Movimiento( _ onFst autoMovimiento)
  
  case object DejarseFajar extends AutoMovimiento( guerrero => guerrero )
  
  case object CargarKi extends AutoMovimiento ( guerrero => {
    guerrero.especie match {
      case Saiyajin(SuperSaiyajin(nivel, _), _) => guerrero aumentaEnergia (150* nivel) 
      case Androide => guerrero
      case _ => guerrero aumentaEnergia 100
      }
    }
  ) 

  case class UsarItem(item: Item) extends Movimiento ( combatientes => {
      
    def disparado:Especie=>Guerrero=>Guerrero = ({ 
      case Humano => _ disminuiEnergia 20
      case Namekusein => _.transformOnTrue( _.estado == Inconsciente)(_ disminuiEnergia 10)
    })
    
    val(atacante, oponente) = combatientes
    combatientes.becomeOnTrue(atacante.inventario contains item)(
      (item, oponente.especie) match {
        case (Arma(Roma), Androide) => combatientes
        case (Arma(Roma), _) if oponente.energia < 300 => (atacante, oponente estas Inconsciente) 
        case (Arma(Filosa), Saiyajin(MonoGigante(energiaNormal), true)) => (atacante, 
                                                                             oponente tuEnergiaEs 1
                                                                                      tuEnergiaMaximaEs energiaNormal
                                                                                      transformateEn Saiyajin(Normal,false)
                                                                                      estas Inconsciente)
        case (Arma(Filosa), Saiyajin(fase, true)) => (atacante, oponente tuEnergiaEs 1
                                                                          transformateEn Saiyajin(fase,false))
        case (Arma(Filosa), _) => (atacante, oponente disminuiEnergia (atacante.energia / 100))
        case (Arma(Fuego(tipo)), especieAtacado) if atacante.inventario.contains( Municion(tipo) ) =>
          (atacante gastarItems (List( Municion(tipo) )), disparado(especieAtacado)(oponente))
        case (SemillaDelErmitaño, _) => (atacante tuEnergiaEs (atacante.energiaMaxima), oponente)
        case _ => combatientes
        }    )   
    
  })
  

  case object ComerseAlOponente extends Movimiento (combatientes => {
    //XXX Como implementariamos Majin Buu? Deberiamos revisar cuales son los poderes adquiridos y cuales no.
    val(atacante, oponente) = combatientes
    atacante.especie match {
      case Monstruo(digerir) => (digerir(combatientes), oponente estas Muerto) //XXX: Oponente deberia estar muerto?
      case _ => combatientes
    }
    
  })
  
  case object ConvertirseEnMono extends AutoMovimiento (guerrero => {

    (guerrero.especie,guerrero.energiaMaxima) match {  
      case (Saiyajin(MonoGigante(_), _),_) => guerrero
      case (Saiyajin(fase, true),energiaMaxima) if (guerrero.inventario contains FotoDeLaLuna) =>
                                  val energiaO = fase.energiaOriginal(guerrero)
                                  (guerrero transformateEn Saiyajin(MonoGigante(energiaO), true)
                                            tuEnergiaMaximaEs (3*energiaO)
                                            cargarAlMaximo)
      case _ => guerrero
    }
    
  } )
  
  case object ConvertirseEnSuperSaiyajing extends AutoMovimiento (guerrero => {
    (guerrero.especie,guerrero.energia,guerrero.energiaMaxima) match {
      case (Saiyajin(MonoGigante(_), _),_, _) => guerrero
      case (Saiyajin(fase, cola), ki, kiMaximo) if (ki > kiMaximo/2) => 
                                    val (nivel, energiaOriginal) = (fase.proxNivelSSJ, fase.energiaOriginal(guerrero))
                                    (guerrero transformateEn Saiyajin(SuperSaiyajin(nivel,energiaOriginal),cola)
                                              tuEnergiaMaximaEs (5 * nivel * energiaOriginal))
      case _ => guerrero
      }
    } 
  )
  
  case class Fusion(aliado: Guerrero) extends AutoMovimiento (guerrero => {
      
    guerrero.transformOnTrue(
        List(aliado, guerrero) forall ( _.puedeFusionarse ))(
            _ sumaAInventario (aliado.inventario)
            variarEnergiaMaxima (aliado.energiaMaxima+)
            aumentaEnergia (aliado.energia)
            transformateEn (Fusionado(guerrero, aliado)))
        
  } )
 
  case class Magia(cambioDeEstado: Function1[Combatientes, Combatientes]) extends Movimiento (combatientes => {
    
    val(atacante, oponente) = combatientes
    atacante.especie match{
      case Namekusein => cambioDeEstado(combatientes)
      case Monstruo(_) => cambioDeEstado(combatientes)
      case _ if (atacante.inventario count EsferaDelDragon is 7) => 
                              cambioDeEstado(atacante gastarItems (List.fill(7)(EsferaDelDragon)), oponente)
      case _ => combatientes
    }
    
  } )
  
  case object MuchosGolpesNinja extends Movimiento ({case (atacante,oponente) => {
    
    (atacante.especie, oponente.especie) match {
      case (Humano, Androide) => (atacante disminuiEnergia 10, oponente)
      case _ if atacante.energia < oponente.energia => (atacante disminuiEnergia 20, oponente)
      case _  => (atacante, oponente disminuiEnergia 20)
    }
  }
  } )
  
  case object Explotar extends Movimiento (combatientes => {
      
    val(atacante, oponente) = combatientes onFst ( _ estas Muerto)
    (atacante.especie, oponente.especie) match {
      case (Androide, Namekusein) if atacante.energia * 3 > oponente.energia => 
          (atacante tuEnergiaEs(0), oponente tuEnergiaEs 1)
      case (Monstruo(_), Namekusein) if atacante.energia * 2 > oponente.energia => 
          (atacante tuEnergiaEs(0), oponente tuEnergiaEs 1)
      case (Androide, _) => (atacante tuEnergiaEs(0), oponente disminuiEnergia (atacante.energia * 3))
      case (Monstruo(_), _) => (atacante tuEnergiaEs(0), oponente disminuiEnergia (atacante.energia * 2))
      case _ => combatientes
    }
    
  } )
  
  case class Onda(energiaNecesaria: Int) extends Movimiento ({case (atacante, oponente)=> {
    
    (atacante,oponente).becomeOnTrue(atacante.energia > energiaNecesaria)(
      (atacante disminuiEnergia energiaNecesaria,
       oponente disminuiEnergia (oponente.especie match {
                                 case Androide => - energiaNecesaria * 2
                                 case Monstruo(_) => energiaNecesaria / 2
                                 case _ => energiaNecesaria*2 })
      ))
    
  } })
  
  case object Genkidama extends Movimiento ({case (atacante,oponente) => {
    
    atacante.estado match {
      case Fajado(rounds) => (atacante estas Luchando, oponente disminuiEnergia (10 ^^ rounds))
      case _ => (atacante, oponente)
    }
    
  } })

}