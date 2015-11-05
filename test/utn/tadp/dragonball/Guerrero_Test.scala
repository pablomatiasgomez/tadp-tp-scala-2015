package utn.tadp.dragonball

import utn.tadp.dragonball.Simulador._
import utn.tadp.dragonball.Guerrero._
import utn.tadp.dragonball.BlackMagic._
import org.scalatest._

class Guerrero_Test extends FlatSpec with Matchers {

  val todosSaben: List[Movimiento] = List(DejarseFajar, CargarKi, MuchosGolpesNinja, Onda(10))
  val goku : Guerrero = new Guerrero("Goku", List(SemillaDelErmitaño, FotoDeLaLuna), 2500, 1300, Saiyajin(SuperSaiyajin(1, 500), true), Fajado(3), todosSaben++List(Onda(99), Genkidama))
  val vegeta : Guerrero = new Guerrero("Vegeta", List(), 1001, 801, Saiyajin(Normal, false), Luchando, todosSaben++List(Onda(100), Fusion(goku)))

  def quedoConMasKi(combatientes: Combatientes) = combatientes._1.energia
  def mayorVentajaDeKi(combatientes: Combatientes) = {
                val(energiaA, energiaO) = combatientes.map { guerrero => guerrero.energia }
                if(energiaA > energiaO)
                  (energiaA - energiaO) * 1000
                else
                  energiaO - energiaA
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
  
}