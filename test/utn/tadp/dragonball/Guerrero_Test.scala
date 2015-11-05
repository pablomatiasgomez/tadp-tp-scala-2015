package utn.tadp.dragonball

import utn.tadp.dragonball.Simulador._
import utn.tadp.dragonball.Guerrero._
import org.scalatest._

class Guerrero_Test extends FlatSpec with Matchers {

  def deberia = should
  val todosSaben: List[Movimiento] = List(DejarseFajar, CargarKi, MuchosGolpesNinja, Onda(10))
  val goku : Guerrero = new Guerrero("Goku", List(SemillaDelErmitaño, FotoDeLaLuna), 2500, 1300, Saiyajin(SuperSaiyajin(1, 500), true), Fajado(3), todosSaben++List(Onda(99), Genkidama))
  val vegeta : Guerrero = new Guerrero("Vegeta", List(), 1001, 801, Saiyajin(Normal, false), Luchando, todosSaben++List(Onda(100), Fusion(goku)))

  def quedoConMasKi(combatientes: Combatientes) = combatientes._1.energia
  def mayorVentajaDeKi(combatientes: Combatientes) = (combatientes._2. energia - combatientes._1.energia).abs
  def quedoMasFajado(combatientes: Combatientes) = combatientes._1.estado match {
                                                                                case Fajado(rounds) => rounds
                                                                                case _ => 0
                                                                          }
  
  
  "goku" should "elegir CargarKi porque lo deja con mas ki" in {
    goku.movimientoMasEfectivoContra(vegeta)(quedoConMasKi) should be (CargarKi)
  }
  
  "goku" should "elegir Genkidama porque lo deja con mayor ventaja" in {
    goku.movimientoMasEfectivoContra(vegeta)(mayorVentajaDeKi) should be (Genkidama)
  }
  
  "vegeta" should "elegir Fusion porque lo deja con mas ki y con mas Venjata" in {
    vegeta.movimientoMasEfectivoContra(goku)(quedoConMasKi) should be (Fusion(goku))
    vegeta.movimientoMasEfectivoContra(goku)(mayorVentajaDeKi) should be (Fusion(goku))
  }
  
  "goku" should "elegir DejarseFajar porque lo deja mas fajado" in {
    goku.movimientoMasEfectivoContra(vegeta)(quedoMasFajado) should be (DejarseFajar)
  }
  
}