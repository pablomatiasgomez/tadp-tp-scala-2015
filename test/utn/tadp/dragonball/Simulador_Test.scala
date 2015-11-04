package utn.tadp.dragonball

import utn.tadp.dragonball.Simulador._
import utn.tadp.dragonball.Guerrero._
import utn.tadp.dragonball.Especie._

import org.junit.Before
import org.junit.Test

import org.junit.Assert._

class Simulador_Test {
  
  //Guerrero(nombre, inventario, energiaMaxima, energia, especie, estado, movimientos)
  val todosSaben: List[Movimiento] = List(DejarseFajar, CargarKi, MuchosGolpesNinja, Onda(10))
  val esferasDelDragon: List[Item] = List(EsferaDelDragon(1), EsferaDelDragon(2), EsferaDelDragon(3), EsferaDelDragon(4), EsferaDelDragon(5), EsferaDelDragon(6), EsferaDelDragon(7))
  val dejarInconsciente: Function1[Combatientes, Combatientes] = { case (atacante, oponente) => (atacante, oponente.estas(Inconsciente)) }
  
  val krilin: Guerrero = new Guerrero("Krilin", List(Arma(Roma)), 100, 50, Humano, Luchando, todosSaben++List(UsarItem(Arma(Roma))))
  val numero18: Guerrero = new Guerrero("N18", List(Arma(Fuego)), 300, 100, Androide, Luchando, todosSaben++List(Explotar, UsarItem(Arma(Fuego))))
  val piccolo : Guerrero = new Guerrero("Piccolo", esferasDelDragon, 500, 200, Namekusein, Luchando, todosSaben++List(Fusion(krilin), Magia(dejarInconsciente), Onda(40)))
  val majinBuu: Guerrero = new Guerrero("Majin Buu", List(Arma(Filosa)), 700, 300, Monstruo({case (a, o) => {a.tusMovimientos(o.movimientos)} }), Luchando, todosSaben++List(UsarItem(Arma(Filosa)), ComerseAlOponente))
  val cel: Guerrero = new Guerrero("Cel", List(), 500, 250, Monstruo({case (a, o) => {a.agregaMovimientos(o.movimientos)} }), Luchando, todosSaben++List(Explotar, ComerseAlOponente))
  val mono : Guerrero = new Guerrero("Goku", List(), 3000, 3000, Saiyajing(MonoGigante(1000), true), Luchando, todosSaben)
  val goku : Guerrero = new Guerrero("Goku", List(), 2500, 800, Saiyajing(SuperSaiyajing(1, 500), true), Luchando, todosSaben++List(Onda(99), Genkidama))
  val vegeta : Guerrero = new Guerrero("Goku", List(), 1000, 801, Saiyajing(Normal, false), Luchando, todosSaben++List(Onda(100)))

  @Test
  def krilinSeDejaFajarTest() = {
    val (k, p) = DejarseFajar(krilin, piccolo)
    
    assertEquals(krilin, k)
    assertEquals(piccolo, p)
  }
  
  @Test
  def superSaiyajingCargaKiTest() ={
    val(g, k) = CargarKi(goku, krilin)
    
    assertEquals(950, g.energia)
    assertEquals(krilin, k)
  }
  
  @Test
  def androideNoCargaKiTest() ={
    val(n18, k) = CargarKi(numero18, krilin)
    
    assertEquals(numero18.energia, n18.energia)
    assertEquals(krilin, k)
  }
  
  @Test
  def otroCargaKiTest() ={
    val(p, k) = CargarKi(piccolo, krilin)
    
    assertEquals(300, p.energia)
    assertEquals(krilin, k)
  }
  
}