package utn.tadp.dragonball

import utn.tadp.dragonball.Simulador._

import org.junit.Before
import org.junit.Test

import org.junit.Assert._

class Simulador_Test {
  
  //Guerrero(nombre, inventario, energiaMaxima, energia, especie, estado, movimientos)
  val todosSaben: List[Movimiento] = List(DejarseFajar, CargarKi, MuchosGolpesNinja, Onda(10))
  val esferasDelDragon: List[Item] = List(EsferaDelDragon(1), EsferaDelDragon(2), EsferaDelDragon(3), EsferaDelDragon(4), EsferaDelDragon(5), EsferaDelDragon(6), EsferaDelDragon(7))
  
  val krilin: Guerrero = new Guerrero("Krilin", List(Arma(Roma)), 50, 100, Humano, Luchando, todosSaben++List(UsarItem(Arma(Roma))))
  val numero18: Guerrero = new Guerrero("N18", List(Arma(Fuego)), 100, 300, Androide, Luchando, todosSaben++List(Explotar, UsarItem(Arma(Fuego))))
  val piccolo : Guerrero = new Guerrero("Piccolo", esferasDelDragon, 200, 500, Namekusein, Luchando, todosSaben++List(Fusion(krilin), Magia(???), Onda(40)))
  val majinBuu: Guerrero = new Guerrero("Majin Buu", List(Arma(Filosa)), 300, 700, Monstruo(???), Luchando, todosSaben++List(UsarItem(Arma(Filosa)), ComerseAlOponente))
  val cel: Guerrero = new Guerrero("Cel", List(), 250, 500, Monstruo({case (a, o) => {a.agregaMovimientos(o.movimientos)} }), Luchando, todosSaben++List(Explotar, ComerseAlOponente))
  val goku : Guerrero = new Guerrero("Goku", List(), 800, 1000, Saiyajing(Normal, true), Luchando, todosSaben++List(Onda(99), Genkidama))
  val vegeta : Guerrero = new Guerrero("Goku", List(), 801, 1001, Saiyajing(Normal, false), Luchando, todosSaben++List(Onda(100)))


}