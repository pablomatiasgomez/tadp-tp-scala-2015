package utn.tadp.dragonball

import utn.tadp.dragonball.Simulador._
import utn.tadp.dragonball.Guerrero._
import utn.tadp.dragonball.BlackMagic._
import org.scalatest._

class Guerrero_Test extends FlatSpec with Matchers {

  val todosSaben: List[Movimiento] = List(DejarseFajar, CargarKi, MuchosGolpesNinja, Onda(10))
  val goku : Guerrero = new Guerrero("Goku", List(SemillaDelErmitaño, FotoDeLaLuna), 2500, 1300, Saiyajin(SuperSaiyajin(1, 500), true), Fajado(3), todosSaben++List(Onda(99), Genkidama))
  val vegeta : Guerrero = new Guerrero("Vegeta", List(), 1001, 801, Saiyajin(Normal, false), Luchando, todosSaben++List(Onda(100), Fusion(goku)))
  val cell: Guerrero = new Guerrero("Cell", List(), 500, 500, Monstruo({ a => a._1 }), Luchando, List(Onda(45)))
  val yajirobe : Guerrero = new Guerrero("Yajirobe", List(SemillaDelErmitaño), 100, 100, Humano, Luchando, List(UsarItem(Arma(Filosa)), UsarItem(SemillaDelErmitaño)))
  
  def quedoConMasKi(combatientes: Combatientes) = combatientes._1.energia
  def mayorVentajaDeKi(combatientes: Combatientes) = (combatientes._1.energia - combatientes._2.energia) match {
                case diferencia if diferencia > 0 => diferencia 
                case 0 => 0.99
                case diferencia if diferencia < 0 => 0.98/ diferencia
            }
  
  def quedoMasFajado(combatientes: Combatientes) = combatientes._1.estado match {
                                                                                case Fajado(rounds) => rounds
                                                                                case _ => 0
                                                                          }
  
  
  "goku" should "elegir CargarKi porque lo deja con mas ki" in {
    goku.movimientoMasEfectivoContra(vegeta)(quedoConMasKi) shouldBe CargarKi
  }
  
  "goku" should "elegir Genkidama porque lo deja con mayor ventaja" in {
    goku.movimientoMasEfectivoContra(vegeta)(mayorVentajaDeKi) shouldBe Genkidama
  }
  
  "vegeta" should "elegir Fusion porque lo deja con mas ki y con mas Venjata" in {
    vegeta.movimientoMasEfectivoContra(goku)(quedoConMasKi) shouldBe Fusion(goku)
    vegeta.movimientoMasEfectivoContra(goku)(mayorVentajaDeKi) shouldBe Fusion(goku)
  }
  
  "goku" should "elegir DejarseFajar porque lo deja mas fajado" in {
    goku.movimientoMasEfectivoContra(vegeta)(quedoMasFajado) shouldBe DejarseFajar
  }
  
  "goku" should "cargar ki mientras que vegeta se fusiona como contra ataque" in {
    val movimientoDeGoku = goku.movimientoMasEfectivoContra(vegeta)(quedoConMasKi)

    val (g, v) = goku.pelearUnRound(movimientoDeGoku)(vegeta)
    
    movimientoDeGoku shouldBe CargarKi
    g.estado shouldBe Luchando
    g.energia shouldBe 1450
    v.energia shouldBe 2101
    v.energiaMaxima shouldBe 3501
    v.especie shouldBe Fusionado(vegeta, goku)
  }
  
  "goku" should "armar un plan dejandose fajar siempre" in {
    goku.planDeAtaque(vegeta, 4)(quedoMasFajado) shouldBe List(DejarseFajar, DejarseFajar, DejarseFajar, DejarseFajar)
  }
  
  "yajirobe" should "armar un plan primero usando la espada y luego la semilla del ermitaño" in {
    yajirobe.planDeAtaque(cell, 2)(quedoConMasKi) shouldBe List(UsarItem(Arma(Filosa)), UsarItem(SemillaDelErmitaño))
  }
  
  "yajirobe" should "pelear con cell y terminar peleando" in {
    val (yajirobeFinal, cellFinal) = (yajirobe disminuiEnergia(90), cell disminuiEnergia(90))
    
    yajirobe.pelearContra(cell)(List(UsarItem(Arma(Filosa)), UsarItem(SemillaDelErmitaño))) shouldBe PeleaEnCurso((yajirobeFinal, cellFinal))
  }
  
  "goku" should "pelear con vegeta y se deja fajar pero lo mata con una Genkidama" in {
    val vegetaMono = new Guerrero("MonoV", List(), 3003, 3003, Saiyajin(MonoGigante(1001), true), Luchando, todosSaben)
    
    goku.pelearContra(vegetaMono)(List(DejarseFajar, Genkidama)) shouldBe Ganador(goku disminuiEnergia(20) estas Luchando)
  }
  
}