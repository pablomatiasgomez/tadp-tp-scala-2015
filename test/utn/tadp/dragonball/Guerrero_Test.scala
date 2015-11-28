package utn.tadp.dragonball

import utn.tadp.dragonball.Simulador._
import utn.tadp.dragonball.Guerrero._
import utn.tadp.dragonball.BlackMagic._
import org.scalatest._
import scala.util.Try
import scala.util.Success
class Guerrero_Test extends FlatSpec with Matchers {

  val todosSaben: List[Movimiento] = List(DejarseFajar, CargarKi, MuchosGolpesNinja, Onda(10))
  val goku : Guerrero[Saiyajin] = new Guerrero("Goku", List(SemillaDelErmitaño, FotoDeLaLuna), 2500, 1300, Saiyajin(true), SuperSaiyajin(1, 500), todosSaben++List(Onda(99), Genkidama), 3)
  val vegeta : Guerrero[Saiyajin] = new Guerrero("Vegeta", List(), 1001, 801, Saiyajin(false), Luchando, todosSaben++List(Onda(100), Fusion(goku)), 0)
  val cell: Guerrero[Monstruo] = new Guerrero("Cell", List(), 500, 500, Monstruo({ a => a._1 }), Luchando, List(Onda(45)), 0)
  val yajirobe : Guerrero[Humano] = new Guerrero("Yajirobe", List(SemillaDelErmitaño), 100, 100, Humano(), Luchando, List(UsarItem(Arma(Filosa)), UsarItem(SemillaDelErmitaño)), 0)
  
  def quedoConMasKi(combatientes: Combatientes[Especie, Especie]): Option[Double] = Option(combatientes._1.energia)
  def mayorVentajaDeKi(combatientes: Combatientes[Especie, Especie]): Option[Double] = Option((combatientes._1.energia - combatientes._2.energia) match {
                case diferencia if diferencia > 0 => diferencia 
                case 0 => 0.99
                case diferencia if diferencia < 0 => 0.98/ diferencia
            })
            
  def diferenciaDeKi(combatientes: Combatientes[Especie, Especie]): Option[Double] = None
  
  def quedoMasFajado(combatientes: Combatientes[Especie, Especie]): Option[Double] = Option(combatientes._1.turnosFajado)
  
  "movimientoMasEfectivoContra" should "be None" in {
    goku.movimientoMasEfectivoContra(vegeta)(diferenciaDeKi) shouldBe None
  }
  
  
  "movimientoMasEfectivoContra" should "goku elige CargarKi porque lo deja con mas ki" in {
    goku.movimientoMasEfectivoContra(vegeta)(quedoConMasKi) shouldBe Some(CargarKi)
  }
  
  "movimientoMasEfectivoContra" should "goku elige Genkidama porque lo deja con mayor ventaja" in {
    goku.movimientoMasEfectivoContra(vegeta)(mayorVentajaDeKi) shouldBe Some(Genkidama)
  }
  
  "movimientoMasEfectivoContra" should " vegeta elige Fusion porque lo deja con mas ki y con mas Venjata" in {
    vegeta.movimientoMasEfectivoContra(goku)(quedoConMasKi) shouldBe Some(Fusion(goku))
    vegeta.movimientoMasEfectivoContra(goku)(mayorVentajaDeKi) shouldBe Some(Fusion(goku))
  }
  
  "movimientoMasEfectivoContra" should "goku elige DejarseFajar porque lo deja mas fajado" in {
    goku.movimientoMasEfectivoContra(vegeta)(quedoMasFajado) shouldBe Some(DejarseFajar)
  }
  
  "pelearUnRound" should "goku carga ki mientras que vegeta se fusiona como contra ataque" in {
    val movimientoDeGoku = goku.movimientoMasEfectivoContra(vegeta)(quedoConMasKi)

    val (g, v) = goku.pelearUnRound(movimientoDeGoku.get)(vegeta)
    
    movimientoDeGoku shouldBe Some(CargarKi)
    g.estado shouldBe SuperSaiyajin(1,500)
    g.energia shouldBe 1450
    v.energia shouldBe 2101
    v.energiaMaxima shouldBe 3501
    v.especie shouldBe Fusionado(vegeta, goku)
  }
  
  "planDeAtaque" should "goku arma un plan dejandose fajar siempre" in {
    goku.planDeAtaque(vegeta, 4)(quedoMasFajado) shouldBe Success(List(DejarseFajar, DejarseFajar, DejarseFajar, DejarseFajar))
  }
  
  "planDeAtaque" should "yajirobe arma un plan primero usando la espada y luego la semilla del ermitaño" in {
    yajirobe.planDeAtaque(cell, 2)(quedoConMasKi) shouldBe Success(List(UsarItem(Arma(Filosa)), UsarItem(SemillaDelErmitaño)))
  }
  
  "pelearContra" should "yajirobe pelea con cell y terminan peleando" in {
    val (yajirobeFinal, cellFinal) = (yajirobe disminuiEnergia(90), cell disminuiEnergia(90))
    
    yajirobe.pelearContra(cell)(yajirobe.planDeAtaque(cell, 2)(quedoConMasKi).get) shouldBe PeleaEnCurso((yajirobeFinal, cellFinal))
  }
  
  "pelearContra" should "goku pelea con vegeta y se deja fajar pero lo mata con una Genkidama" in {
    val vegetaMono = new Guerrero("MonoV", List(), 3003, 3003, Saiyajin(true), MonoGigante(1001), todosSaben)
    
    goku.pelearContra(vegetaMono)(List(DejarseFajar, Genkidama)) shouldBe Ganador(goku disminuiEnergia(20) resetearTurnosFajados)
  }
  
}