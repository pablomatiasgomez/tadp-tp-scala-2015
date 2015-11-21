package utn.tadp.dragonball

import utn.tadp.dragonball.BlackMagic._

object Simulador {
 
  type Combatientes = (Guerrero, Guerrero)
  
  
  abstract class Movimiento extends Function[Combatientes , Combatientes ] {
    def movimiento:Combatientes=>Combatientes
    def apply(combatientes: Combatientes) = {
      val(atacante, oponente) = combatientes
      (atacante.estado, this) match {
        case (Muerto, _) => combatientes
        case (Inconsciente, UsarItem(SemillaDelErmitaño)) => movimiento(combatientes)
        case (Inconsciente, _) => combatientes
        case (_, DejarseFajar) => movimiento(combatientes) //POR QUE HABIAMOS PUESTO FAJADO(_),DejarseFajar ACA?
        case (_, _) => movimiento(combatientes) onFst (_ resetearTurnosFajados)

      }
    }
    
  }
  
  abstract class AutoMovimiento extends Movimiento {
    def autoMovimiento: Guerrero=>Guerrero
    def movimiento = ( _ onFst autoMovimiento)
  }
  

  
  case object DejarseFajar extends AutoMovimiento {
    def autoMovimiento = _ pasarTurnoFajado    
  }
    
  case object CargarKi extends AutoMovimiento { 
    def autoMovimiento = guerrero => {
      (guerrero.especie, guerrero.estado) match {
        case (_, SuperSaiyajin(nivel, _)) => guerrero aumentaEnergia (150 * nivel) 
        case (Androide, _) => guerrero
        case _ => guerrero aumentaEnergia 100
      }
    }
  }

  case class UsarItem(item: Item) extends Movimiento {
    def movimiento = combatientes => {
      
    def disparado: Especie => Guerrero => Guerrero = ({ 
      case Humano => _ disminuiEnergia 20
      case Namekusein => _.transformOnTrue( _.estado == Inconsciente)(_ disminuiEnergia 10)
      case _ => identity
    })
    
    def perderCola(guerrero: Guerrero): Guerrero = guerrero.estado match{
      case MonoGigante(energiaNormal) => (guerrero tuEnergiaMaximaEs energiaNormal
                                                   estas Inconsciente)
      case fase => guerrero
    }
    
    val(atacante, oponente) = combatientes
    combatientes.becomeOnTrue(atacante tiene item)(
      (item, oponente.especie) match {
        case (Arma(Roma), Androide) => combatientes
        case (Arma(Roma), _) if oponente.energia < 300 => (atacante, oponente estas Inconsciente)
        case (Arma(Filosa), Saiyajin(true)) => (atacante, perderCola(oponente transformateEn Saiyajin(false) tuEnergiaEs 1))
        case (Arma(Filosa), _) => (atacante, oponente disminuiEnergia (atacante.energia / 100))
        case (Arma(Fuego(tipo)), especieAtacado) if atacante tiene Municion(tipo) =>
          (atacante gastarItems (List(Municion(tipo))), disparado(especieAtacado)(oponente))
        case (SemillaDelErmitaño, _) => (atacante tuEnergiaEs (atacante.energiaMaxima), oponente)
        case _ => combatientes
        })   
    
     }
  }
  

  case object ComerseAlOponente extends Movimiento { 
    def movimiento = combatientes => {
    
      val(atacante, oponente) = combatientes
      atacante.especie match {
        case Monstruo(digerir) => (digerir(combatientes), oponente estas Muerto)
        case _ => combatientes
    }
    
    }
  }
  
  case object ConvertirseEnMono extends AutoMovimiento { 
    def autoMovimiento = guerrero => {
  
      (guerrero.especie, guerrero.estado, guerrero.energiaMaxima) match {  
        case (_, MonoGigante(_), _) => guerrero
        case (Saiyajin(true), fase, energiaMaxima) if (guerrero tiene FotoDeLaLuna) =>
                                                    val energiaO = fase.energiaOriginal(guerrero)
                                                    (guerrero estas MonoGigante(energiaO)
                                                      tuEnergiaMaximaEs (3 * energiaO)
                                                      cargarAlMaximo)
        case _ => guerrero
      }
      
    }
  }
  
  case object ConvertirseEnSuperSaiyajing extends AutoMovimiento {
    def autoMovimiento = guerrero => {
      
      (guerrero.especie, guerrero.estado, guerrero.energia, guerrero.energiaMaxima) match {
        case (Saiyajin(_), MonoGigante(_), _, _) => guerrero
        case (Saiyajin(_), fase:EstadoSaiyajing, ki, kiMaximo) if (ki > kiMaximo/2) => 
                                      val (nivel, energiaOriginal) = (fase.proxNivelSSJ, fase.energiaOriginal(guerrero))
                                      (guerrero estas SuperSaiyajin(nivel, energiaOriginal)
                                                tuEnergiaMaximaEs (5 * nivel * energiaOriginal))
        case (Saiyajin(_), fase, ki, kiMaximo) if (ki > kiMaximo/2) => 
                                      val (nivel, energiaOriginal) = (1, fase.energiaOriginal(guerrero))
                                      (guerrero estas SuperSaiyajin(nivel, energiaOriginal)
                                                tuEnergiaMaximaEs (5 * nivel * energiaOriginal))
        case _ => guerrero
        }
      
    }
  }
  
  case class Fusion(aliado: Guerrero) extends AutoMovimiento  {
    def autoMovimiento = guerrero => {
        
      (guerrero.especie,aliado.especie) match{
        case (_:Fusionable, _:Fusionable) => (guerrero sumaAInventario (aliado.inventario)
                                                       variarEnergiaMaxima (aliado.energiaMaxima+)
                                                       aumentaEnergia (aliado.energia)
                                                       transformateEn (Fusionado(guerrero, aliado)))
        case _ => guerrero
      }   
    }}
   
    case class Magia(paseDeMagia: Combatientes => Combatientes) extends Movimiento {
    def movimiento = combatientes => {
      val(atacante, oponente): Combatientes = combatientes
      atacante.especie match {
        case _:Magico => paseDeMagia(combatientes)
        case _ if (atacante tiene (EsferaDelDragon, 7)) =>
                                paseDeMagia (atacante gastarItems (List.fill(7)(EsferaDelDragon)), oponente)
        case _ => combatientes
      }
    
    }
  }
  
    
  type Daños = (Int, Int)
  
  trait TipoAtaque
  
  case object Energia extends TipoAtaque
  case object Fisico extends TipoAtaque
  
  abstract class Ataque(tipoAtaque: TipoAtaque) extends Movimiento {

    def funcionDaño:Combatientes => Daños
    
    def movimiento = combatientes => {
      
      val (dañoAtacante, dañoAtacado) = funcionDaño(combatientes)
      
      def efectoEnAtacado(guerrero:Guerrero) = (this, tipoAtaque, guerrero.especie) match {
        case (_, Energia, Androide) => guerrero.aumentaEnergia _
        case _ => guerrero.disminuiEnergia _
      }
      
  
      
      combatientes.onEach( _ disminuiEnergia dañoAtacante, efectoEnAtacado(_)(dañoAtacado) )
    }
  
  }

  case object MuchosGolpesNinja extends Ataque(Fisico){
    
    def funcionDaño = { case (atacante, oponente) => {
    
         (atacante.especie, oponente.especie) match {
           case (Humano, Androide) => (10, 0)
           case _ if atacante.energia < oponente.energia => (20, 0)
           case _  => (0, 20)
         } 
       }
     }
  }
  
  case object Explotar extends Ataque(Fisico){
    
    def funcionDaño = { case (atacante, oponente) => {
  
    
        val energiaDeExplosion = atacante.energia
        
        def factorExplosivo: Int = atacante.especie match {
            case Androide => 3
            case Monstruo(_) => 2
        }    
        
        def daño = atacante.especie match {
          case Androide | Monstruo(_) => (energiaDeExplosion, energiaDeExplosion * factorExplosivo)
          case _ => (0, 0)
        }
        
        def esquivaLaMuerte: Int => Int = oponente.especie match {
          case Namekusein =>  _ min (oponente.energia - 1)
          case _ => identity _
        }
        
        daño onSnd esquivaLaMuerte
        
      }
    }
    
  }
  
  case class Onda(energiaNecesaria: Int) extends Ataque(Energia){
  
  def funcionDaño = { case (atacante, oponente) => {
    
        def poderDeOnda = oponente.especie match {
                         case Monstruo(_) => energiaNecesaria / 2
                         case _ => energiaNecesaria * 2 }
     
        if (atacante.energia > energiaNecesaria) (energiaNecesaria, poderDeOnda)
        else (0, 0)
      
      } 
    }
    
  
}
  
  case object Genkidama extends Ataque(Energia){
    
    def funcionDaño = { case(atacante, _) =>
     
      val poderAcumulado = 10 pow atacante.turnosFajado
      
      (0, poderAcumulado)
      
    }  
  }
}