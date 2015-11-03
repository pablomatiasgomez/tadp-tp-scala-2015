package utn.tadp.dragonball

import utn.tadp.dragonball.Item

object Simulador {
  
trait EstadoGuerrero 

case object Muerto extends EstadoGuerrero

case object Inconsciente extends EstadoGuerrero

case object Luchando extends EstadoGuerrero

type Combatientes = (Guerrero, Guerrero)
  
abstract class Movimiento(funcion:(Combatientes=>Combatientes)) extends Function1[Combatientes,Combatientes]{
  def apply(combatientes:Combatientes):Combatientes = {
    val (atacante,atacado) = combatientes
    (atacante.estado,atacado.estado,this) match{
      case (Muerto,_,_) => combatientes
      case (Inconsciente,_,usarItem(SemillaDelEmitaño)) => funcion.apply(combatientes)
      case (Inconsciente,_,_) => combatientes
      case _ => funcion.apply(combatientes)
    }
  }
}
  
case class Guerrero(
      nombre: String,
      inventario: List[Item],
      energiaMaxima: Int,
      energia: Int,
      especie: Especie,
      movimientos: List[Movimiento],
      estado:EstadoGuerrero
      ) {
  def aumentarKi(ki: Int) = copy(energia = (energia + ki).min(energiaMaxima))
  
  def disminuiKi(ki: Int) = copy(energia = (energia - ki).max(0))

  def puedeFusionarse = especie.fusionable
  
  def cambiarEstado(nuevoEstado:EstadoGuerrero) = copy(estado=nuevoEstado)
  
  def movimientoMasEfectivoContra(oponente:Guerrero)(criterio:Combatientes=>Int) = {
    def calcularValor(movimiento:Movimiento):Int = criterio(movimiento(this,oponente))
    val mejorMovimiento = movimientos.maxBy(calcularValor(_))
    if (calcularValor(mejorMovimiento)>=0) mejorMovimiento
    else throw new RuntimeException
        }
  
  def pelearUnRound(movimiento:Movimiento)(oponente:Guerrero) = {
    def criterioContraAtaque(combatientes:Combatientes) = combatientes._1.energia-combatientes._1.energia
    val resultadoAtaque = movimiento(this,oponente)
    val resultadoContraAtaque = oponente.movimientoMasEfectivoContra(this)(criterioContraAtaque)(oponente,this)
  }
  
  
  }


case object dejarseFajar extends Movimiento((combatientes:Combatientes) => combatientes) 


case object cargarKi extends Movimiento((combatientes:Combatientes) => {
  
  val (atacante,atacado) = combatientes
    
    (atacante.especie match {
      case Saiyajing(SuperSaiyajing(nivel,_),_) => atacante.aumentarKi(nivel*150)
      case Androide => atacante
      case _ => atacante.aumentarKi(100)
    },atacado) 
    
  })

case class usarItem(item:Item) extends Movimiento((combatientes:Combatientes) => {
  
  val (atacante,atacado) = combatientes
  
  (item,atacante.energia,(atacado.especie,atacado.energia,atacado.estado)) match{
    case (SemillaDelEmitaño,_,_) => (atacante.copy(energia=atacante.energiaMaxima),atacado)
    case (Arma(Roma),_,(Androide,_,_)) => combatientes
    case (Arma(Roma),_,(_,ki,_)) if ki<300 => (atacante,atacado.cambiarEstado(Inconsciente))
    case (Arma(Filosa),_,(Saiyajing(MonoGigante(energiaNormal),true),_,_)) => (atacante,atacado.copy( energia=1,
                                                                                                    especie=Saiyajing(Normal,false),
                                                                                                    energiaMaxima=energiaNormal)
                                                                                                    .cambiarEstado(Inconsciente))
    case (Arma(Filosa),_,(Saiyajing(fase,true),_,_)) => (atacante,atacado.copy(energia=1,especie=Saiyajing(fase,false)))
    case (Arma(Filosa),kiAtk,_) => (atacante,atacado.disminuiKi(kiAtk/100))
    case (Arma(Fuego),_,(Humano,_,_)) => (atacante,atacado.disminuiKi(20))
    case (Arma(Fuego),_,(Namekusein,_,Inconsciente)) => (atacante,atacado.disminuiKi(10))
    case _ => combatientes
    
  }
  
  
})

case object comerOponente extends Movimiento((combatientes:Combatientes) => {
  
  val (atacante,atacado) = combatientes
  
  (atacante.especie,atacado) match {
    case (Monstruo(comer),_) => (comer(atacante,atacado),atacado.cambiarEstado(Muerto))
    case _ => combatientes
  }
})


case object convertirseEnMono extends Movimiento((combatientes:Combatientes) => {
  
  val (atacante,atacado) = combatientes
  (atacante.especie match{
    case Saiyajing(Normal,true) => atacante  .copy(especie=Saiyajing(MonoGigante(atacante.energiaMaxima),true))
                                             .copy(energiaMaxima=atacante.energiaMaxima*3)
                                             .copy(energia=atacante.energiaMaxima)
    case _ => atacante
  },atacado)
})

}