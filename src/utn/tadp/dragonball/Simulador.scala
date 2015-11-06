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
        case (Luchando, DejarseFajar) => movimiento(combatientes) onFst (_ estas Fajado(1))
        case (Luchando, _) => movimiento(combatientes)
        case (Fajado(rounds), DejarseFajar) => movimiento(combatientes) onFst (_ estas Fajado(rounds + 1))
        case (Fajado(_), _) => movimiento(combatientes) onFst (_ estas Luchando)
      }
    }
    
  }
  
  abstract class AutoMovimiento(autoMovimiento: (Guerrero => Guerrero)) extends Movimiento( _ onFst autoMovimiento)
  
  case object DejarseFajar extends AutoMovimiento(guerrero => guerrero)
  
  case object CargarKi extends AutoMovimiento (guerrero => {
    
    guerrero.especie match {
      case Saiyajin(SuperSaiyajin(nivel, _), _) => guerrero aumentaEnergia (150 * nivel) 
      case Androide => guerrero
      case _ => guerrero aumentaEnergia 100
      }
    
  }) 

  case class UsarItem(item: Item) extends Movimiento ( combatientes => {
      
    def disparado: Especie => Guerrero => Guerrero = ({ 
      case Humano => _ disminuiEnergia 20
      case Namekusein => _.transformOnTrue( _.estado == Inconsciente)(_ disminuiEnergia 10)
    })
    
    def perderCola(modoSaiyajin: EstadoSaiyajing): Guerrero => Guerrero = modoSaiyajin match{
      case MonoGigante(energiaNormal) => (_ tuEnergiaMaximaEs energiaNormal
                                            transformateEn Saiyajin(Normal,false)
                                            estas Inconsciente)
      case fase => _ transformateEn Saiyajin(fase,false) 
    }
    
    val(atacante, oponente) = combatientes
    combatientes.becomeOnTrue(atacante tiene item)(
      (item, oponente.especie) match {
        case (Arma(Roma), Androide) => combatientes
        case (Arma(Roma), _) if oponente.energia < 300 => (atacante, oponente estas Inconsciente)
        case (Arma(Filosa), Saiyajin(modo, true)) => (atacante, perderCola(modo)(oponente tuEnergiaEs 1))
        case (Arma(Filosa), _) => (atacante, oponente disminuiEnergia (atacante.energia / 100))
        case (Arma(Fuego(tipo)), especieAtacado) if atacante tiene Municion(tipo) =>
          (atacante gastarItems (List(Municion(tipo))), disparado(especieAtacado)(oponente))
        case (SemillaDelErmitaño, _) => (atacante tuEnergiaEs (atacante.energiaMaxima), oponente)
        case _ => combatientes
        })   
    
  })
  

  case object ComerseAlOponente extends Movimiento (combatientes => {
    
    val(atacante, oponente) = combatientes
    atacante.especie match {
      case Monstruo(digerir) => (digerir(combatientes), oponente estas Muerto)
      case _ => combatientes
    }
    
  })
  
  case object ConvertirseEnMono extends AutoMovimiento (guerrero => {

    (guerrero.especie, guerrero.energiaMaxima) match {  
      case (Saiyajin(MonoGigante(_), _), _) => guerrero
      case (Saiyajin(fase, true), energiaMaxima) if (guerrero tiene FotoDeLaLuna) =>
                                  val energiaO = fase.energiaOriginal(guerrero)
                                  (guerrero transformateEn Saiyajin(MonoGigante(energiaO), true)
                                            tuEnergiaMaximaEs (3 * energiaO)
                                            cargarAlMaximo)
      case _ => guerrero
    }
    
  })
  
  case object ConvertirseEnSuperSaiyajing extends AutoMovimiento (guerrero => {
    
    (guerrero.especie, guerrero.energia, guerrero.energiaMaxima) match {
      case (Saiyajin(MonoGigante(_), _), _, _) => guerrero
      case (Saiyajin(fase, cola), ki, kiMaximo) if (ki > kiMaximo/2) => 
                                    val (nivel, energiaOriginal) = (fase.proxNivelSSJ, fase.energiaOriginal(guerrero))
                                    (guerrero transformateEn Saiyajin(SuperSaiyajin(nivel, energiaOriginal), cola)
                                              tuEnergiaMaximaEs (5 * nivel * energiaOriginal))
      case _ => guerrero
      }
    
  })
  
  case class Fusion(aliado: Guerrero) extends AutoMovimiento (guerrero => {
      
    guerrero.transformOnTrue(
        List(aliado, guerrero) forall ( _.puedeFusionarse ))(
            _ sumaAInventario (aliado.inventario)
            variarEnergiaMaxima (aliado.energiaMaxima+)
            aumentaEnergia (aliado.energia)
            transformateEn (Fusionado(guerrero, aliado)))
        
  })
 
  case class Magia(cambioDeEstado: Function1[Combatientes, Combatientes]) extends Movimiento (combatientes => {

    val(atacante, oponente) = combatientes
    val paseDeMagia: Combatientes => Combatientes = atacante.especie match {
      case Namekusein | Monstruo(_) => cambioDeEstado
      case _ if (atacante tiene (EsferaDelDragon, 7)) => ({ case(atacante, oponente) =>
                              cambioDeEstado (atacante gastarItems (List.fill(7)(EsferaDelDragon)), oponente)
      })
      case _ => identity
    }
  
    paseDeMagia(combatientes)
  
  })
  
  case object MuchosGolpesNinja extends Ataque(Fisico, ({ case (atacante, oponente) => {
    
         (atacante.especie, oponente.especie) match {
           case (Humano, Androide) => (10, 0)
           case _ if atacante.energia < oponente.energia => (20, 0)
           case _  => (0, 20)
         } 
       }
     })
     
   )
  
  case object Explotar extends Ataque(Fisico, ({ case (atacante, oponente) => {
    
        val energiaAtaque = atacante.energia
        
        def daniosBase = atacante.especie match {
          case Androide | Monstruo(_) => (energiaAtaque, calcOfensiva(energiaAtaque))
          case _ => (0, 0)
        }
        
        def calcOfensiva:Int=>Int = atacante.especie match {
            case Androide => 3*
            case Monstruo(_) => 2*
        }    
        
        def calcDefensiva: Int => Int = oponente.especie match {
          case Namekusein =>  _ min (oponente.energia - 1)
          case _ => identity _
        }
        
        daniosBase onSnd calcDefensiva
        
      }
    })
    
  )
  
  case class Onda(energiaNecesaria: Int) extends Ataque(Energia, ({ case (atacante, oponente) => {
    
        def poderOfensivo = oponente.especie match {
                         case Monstruo(_) => energiaNecesaria / 2
                         case _ => energiaNecesaria * 2 }
     
        if (atacante.energia > energiaNecesaria) (energiaNecesaria, poderOfensivo )
        else (0, 0)
      
      } 
    })
    
  )
  
  case object Genkidama extends Ataque(Energia, ({ case(atacante, _) =>
     
      val poderAcumulado = atacante.estado match {
                    case Fajado(rounds) => 10 pow rounds
                    case _ => 0 } 
      
      (0, poderAcumulado)
    })  
  
  )
  
  
  type Danios = (Int, Int)
  
  trait TipoAtaque
  
  case object Energia extends TipoAtaque
  case object Fisico extends TipoAtaque
  
  class Ataque(tipoAtaque: TipoAtaque, funcionDanio: (Combatientes => Danios)) extends Movimiento(combatientes => {
    
    val (danioAtacante, danioAtacado) = funcionDanio(combatientes)
    def efectoEn(guerrero:Guerrero) = (tipoAtaque, guerrero.especie) match {
      case (Energia, Androide) => guerrero.aumentaEnergia _
      case _ => guerrero.disminuiEnergia _
    }
    
    combatientes.onEach( _ disminuiEnergia danioAtacante, efectoEn(_)(danioAtacado) )
  }
  
  )

}