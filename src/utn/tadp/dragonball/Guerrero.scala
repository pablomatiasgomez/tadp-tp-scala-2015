package utn.tadp.dragonball

case class Guerrero(
      nombre: String,
      inventario: List[Item],
      energiaMaxima: Int,
      energia: Int,
      especie: Especie,
      estado: EstadoDeLucha
      ) {
  def aumentarKi(ki: Int) = copy(energia = (energia + ki).min(energiaMaxima))
  
  def disminuiKi(ki: Int) = copy(energia = (energia - ki).max(0))

  def puedeFusionarse = especie.fusionable
  
  def estas(nuevoEstado: EstadoDeLucha) = copy(estado = nuevoEstado)
  
}

abstract class EstadoDeLucha(){
  
}

case object Luchando extends EstadoDeLucha
case class Fajado(rounds: Int) extends EstadoDeLucha
case object Inconsciente extends EstadoDeLucha
case object Muerto extends EstadoDeLucha

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
      case Saiyajing(SuperSaiyajing(nivel, _), _) => (atacante.aumentarKi(150* nivel), oponente) 
      case Androide => combatientes
      case _ => (atacante.aumentarKi(100), oponente)
    }
    
  } ) 
  
  case class UsarItem(item: Item) extends Movimiento ((combatientes: Combatientes) => {
    
    val(atacante, oponente) = combatientes
    (item, oponente.especie) match {
      case (Arma(Roma), Androide) => combatientes
      case (Arma(Roma), _) if oponente.energia < 300 => (atacante, oponente.estas(Inconsciente))
      case (Arma(Filosa), Saiyajing(MonoGigante(energiaNormal), true)) => (atacante, 
                                                                           oponente.copy(energia = 1, 
                                                                                         especie = Saiyajing(Normal,false),
                                                                                         energiaMaxima = energiaNormal)
                                                                                         .estas(Inconsciente))
      case (Arma(Filosa), Saiyajing(_, true)) => (atacante, oponente.copy(energia = 1, especie = Saiyajing(Normal,false)))
      case (Arma(Filosa), _) => (atacante, oponente.disminuiKi(atacante.energia / 100))
      case (Arma(Fuego),Humano) => (atacante, oponente.disminuiKi(20))
      case (Arma(Fuego), Namekusein) if (oponente.estado == Inconsciente) => (atacante, oponente.disminuiKi(10))
      case (SemillaDelErmitaño, _) => (atacante.aumentarKi(atacante.energiaMaxima), oponente)
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
        (atacante.copy(especie = Saiyajing(MonoGigante(energiaOriginal), true), 
                       energia = atacante.energiaMaxima * 3,
                       energiaMaxima = atacante.energiaMaxima * 3),
        oponente)    
      } 
      case _ => combatientes
    }
    
  } )
  
  case object ConvertirseEnSaiyajing extends Movimiento ((combatientes: Combatientes) => {
      
    val(atacante, oponente) = combatientes
    atacante.especie match {
      case Saiyajing(Normal, cola) => 
          (atacante.copy(especie = Saiyajing(SuperSaiyajing(1, atacante.energiaMaxima), cola),
                         energiaMaxima = atacante.energiaMaxima * 5),
            oponente)
      case Saiyajing(SuperSaiyajing(nivel, energiaOriginal), cola) =>
          (atacante.copy(especie = Saiyajing(SuperSaiyajing(nivel + 1, energiaOriginal), cola),
                         energiaMaxima = energiaOriginal * nivel * 5),
           oponente)
      case _ => combatientes
    
    }
    
  } )
  
  case class Fusion(aliado: Guerrero) extends Movimiento ((combatientes: Combatientes) => {
      
    val(atacante, oponente) = combatientes
    if(List(aliado, atacante).forall(_.puedeFusionarse))
      (atacante.copy(  inventario = atacante.inventario++aliado.inventario,
                        energia = atacante.energia + aliado.energia,
                        especie = Fusionado((atacante, aliado))),
      oponente)
    else throw new RuntimeException("No se pueden fusionar")  
        
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
      case (Humano, Androide) => (atacante.disminuiKi(10), oponente)
      case _ if atacante.energia < oponente.energia => (atacante.disminuiKi(20), oponente)
      case _  => (atacante, oponente.disminuiKi(20))
    }
    
  } )
  
  case object Explotar extends Movimiento ((combatientes: Combatientes) => {
      
    val(atacante, oponente) = (combatientes._1.estas(Muerto), combatientes._2)
    (atacante.especie, oponente.especie) match {
      case (Androide, Namekusein) if atacante.energia * 3 > oponente.energia => 
          (atacante, oponente.copy(energia = 1))
      case (Monstruo(_), Namekusein) if atacante.energia * 2 > oponente.energia => 
          (atacante, oponente.copy(energia = 1))
      case (Androide, _) => (atacante, oponente.disminuiKi(atacante.energia * 3))
      case (Monstruo(_), _) => (atacante, oponente.disminuiKi(atacante.energia * 2))
      case _ => throw new RuntimeException("Sólo los monstruos y los androides pueden explotar")
    }
    
  } )
  
  case class Onda(energiaNecesaria: Int) extends Movimiento ((combatientes: Combatientes) => {
    
    val(atacante, oponente) = combatientes
    if (atacante.energia > energiaNecesaria)
      oponente.especie match {
        case Androide => (atacante.disminuiKi(energiaNecesaria), oponente.aumentarKi(energiaNecesaria * 2))
        case Monstruo(_) => (atacante.disminuiKi(energiaNecesaria), oponente.disminuiKi(energiaNecesaria / 2))
        case _ => (atacante.disminuiKi(energiaNecesaria), oponente.disminuiKi(energiaNecesaria))
      }
     else
      throw new RuntimeException("No posee energia suficiente para lanzar la onda")
    
  } )
  
  case object Genkidama extends Movimiento ((combatientes: Combatientes) => {
    
    val(atacante, oponente) = combatientes
    atacante.estado match {
      case Fajado(rounds) => (atacante.estas(Luchando), oponente.disminuiKi(10^rounds))
      case _ => combatientes
    }
    
  } )

}