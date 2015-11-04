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
  val esferasDelDragon: List[Item] = List(EsferaDelDragon, EsferaDelDragon, EsferaDelDragon, EsferaDelDragon, EsferaDelDragon, EsferaDelDragon, EsferaDelDragon)
  
  val dejarInconsciente: Function1[Combatientes, Combatientes] = { case (atacante, oponente) => (atacante, oponente estas Inconsciente) }
  val convertirEnHumano: Function1[Combatientes, Combatientes] = { case (atacante, oponente) => (atacante, oponente transformateEn Humano) }
  
  val digerirMajinBuu: Function1[Combatientes, Guerrero] = {case (a, o) => {a tusMovimientos(o movimientos)} }
  val digerirCell: Function1[Combatientes, Guerrero] = {case (a, o) => { if(o.especie == Androide)
                                                                           a agregaMovimientos(o.movimientos)
                                                                         else
                                                                           a} }
      
  val krilin: Guerrero = new Guerrero("Krilin", List(Arma(Roma))++esferasDelDragon, 100, 50, Humano, Luchando, todosSaben++List(UsarItem(Arma(Roma)), Magia(convertirEnHumano)))
  val numero18: Guerrero = new Guerrero("N18", List(Arma(Fuego(Ak47)), Municion(Ak47)), 300, 100, Androide, Luchando, todosSaben++List(Explotar, UsarItem(Arma(Fuego(Ak47)))))
  val piccolo : Guerrero = new Guerrero("Piccolo", List(), 500, 200, Namekusein, Luchando, todosSaben++List(Fusion(krilin), Magia(dejarInconsciente), Onda(40)))
  val majinBuu: Guerrero = new Guerrero("Majin Buu", List(Arma(Filosa)), 700, 300, Monstruo(digerirMajinBuu), Luchando, todosSaben++List(UsarItem(Arma(Filosa)), ComerseAlOponente))
  val cell: Guerrero = new Guerrero("Cell", List(), 500, 250, Monstruo(digerirCell), Luchando, todosSaben++List(Explotar, ComerseAlOponente))
  val mono : Guerrero = new Guerrero("Mono", List(), 3000, 3000, Saiyajin(MonoGigante(1000), true), Luchando, todosSaben)
  val goku : Guerrero = new Guerrero("Goku", List(SemillaDelErmitaño, FotoDeLaLuna), 2500, 1300, Saiyajin(SuperSaiyajin(1, 500), true), Luchando, todosSaben++List(Onda(99), Genkidama))
  val vegeta : Guerrero = new Guerrero("Vegeta", List(), 1001, 801, Saiyajin(Normal, false), Luchando, todosSaben++List(Onda(100), Fusion(goku)))

  @Test
  def krilinSeDejaFajarTest() = {
    val (k, p) = DejarseFajar(krilin, piccolo)
    
    assertEquals(krilin estas Fajado(1), k)
    assertEquals(piccolo, p)
  }
  
  @Test
  def superSaiyajingCargaKiTest() ={
    val(g, k) = CargarKi(goku, krilin)
    
    assertEquals(1450, g energia)
    assertEquals(krilin, k)
  }
  
  @Test
  def androideNoCargaKiTest() ={
    val(n18, k) = CargarKi(numero18, krilin)
    
    assertEquals(numero18 energia, n18 energia)
    assertEquals(krilin, k)
  }
  
  @Test
  def otroCargaKiTest() ={
    val(p, k) = CargarKi(piccolo, krilin)
    
    assertEquals(300, p energia)
    assertEquals(krilin, k)
  }
  
  @Test
  def usaArmaRomaContraAndroideTest() ={
    val(k, n18) = UsarItem(Arma(Roma))(krilin, numero18)
    
    assertEquals(n18, numero18)
    assertEquals(krilin, k)
  }
  
  @Test
  def usaArmaRomaYDejaInconscienteTest() ={
    val(k, p) = UsarItem(Arma(Roma))(krilin, piccolo)
    
    assertEquals(Inconsciente, p estado)
    assertEquals(krilin, k)
  }
  
  @Test
  def usaArmaRomaNoPasaNadaTest() ={
    val(k, g) = UsarItem(Arma(Roma))(krilin, goku)
    
    assertEquals(goku, g)
    assertEquals(krilin, k)
  }
  
  @Test
  def siNoTieneUnArmaRomaEnElInventarioNoPasaNadaTest() ={
    val(g, p) = UsarItem(Arma(Roma))(goku, piccolo)
    
    assertEquals(goku, g)
    assertEquals(piccolo, p)
  }
  
  @Test
  def usaArmaFilosaConMonoTest() ={
    val(buu, m) = UsarItem(Arma(Filosa))(majinBuu, mono)
    
    assertEquals(majinBuu, buu)
    assertEquals(Inconsciente, m estado)
    assertEquals(1, m energia)
    assertEquals(Saiyajin(Normal, false), m especie)
    
  }
  
  @Test
  def usaArmaFilosaConSSJTest() ={
    val(buu, g) = UsarItem(Arma(Filosa))(majinBuu, goku)
    
    assertEquals(majinBuu, buu)
    assertEquals(1, g energia)
    assertEquals(Saiyajin(SuperSaiyajin(1, 500), false), g especie)
    
  }
  
  @Test
  def usaArmaFilosaConSJSinColaTest() ={
    val(buu, v) = UsarItem(Arma(Filosa))(majinBuu, vegeta)
    
    assertEquals(majinBuu, buu)
    assertEquals(798, v energia)
    assertEquals(Saiyajin(Normal, false), v especie)
    
  }
  
  @Test
  def usaArmaFilosaConOtroTest() ={
    val(buu, c) = UsarItem(Arma(Filosa))(majinBuu, cell)
    
    assertEquals(majinBuu, buu)
    assertEquals(247, c energia)
    
  }
  
  @Test
  def usaArmaFuegoConHumanoTest() ={
    val(n18, k) = UsarItem(Arma(Fuego(Ak47)))(numero18, krilin)
    
    assertEquals(numero18 gastarItems List(Municion(Ak47)), n18)
    assertEquals(30, k energia)
    
  }
  
  @Test
  def usaArmaFuegoConNamekuseinInconscienteTest() ={
    val(n18, p) = UsarItem(Arma(Fuego(Ak47)))(numero18, piccolo estas Inconsciente)
    
    assertEquals(numero18 gastarItems(List(Municion(Ak47))), n18)
    assertEquals(190, p energia)
    
  }
  
  @Test
  def usaArmaFuegoConNamekuseinLuchandoTest() ={
    val(n18, p) = UsarItem(Arma(Fuego(Ak47)))(numero18, piccolo)
    
    assertEquals(numero18 gastarItems List(Municion(Ak47)), n18)
    assertEquals(piccolo, p)
    
  }
  
  @Test
  def usaSemillaDelErmitañoTest() ={
    val(g, p) = UsarItem(SemillaDelErmitaño)(goku, piccolo)
    
    assertEquals(goku energiaMaxima, g energia)
    assertEquals(piccolo, p)
    
  }
  
  @Test
  def usarUnaEsferaDelDragonSolaNoHaceNadaTest(){
    val(k, n18) = UsarItem(EsferaDelDragon)(krilin, numero18)
    
    assertEquals(krilin, k)
    assertEquals(numero18, n18)
  }
  
  @Test
  def usaSemillaDelErmitañoAunEstandoInconscienteTest() ={
    val(g, p) = UsarItem(SemillaDelErmitaño)(goku estas Inconsciente, piccolo)
    
    assertEquals(g energiaMaxima, g energia)
    assertEquals(piccolo, p)
    
  }
  
  @Test
  def majinBuuSeComeAGokuTest() ={
    val(m, g) = ComerseAlOponente(majinBuu, goku)
    
    assertEquals(goku estas Muerto , g)
    assertEquals(goku movimientos, m movimientos)
    
  }
  
  @Test
  def cellIntentaComerseAGokuSinEfectoTest() ={
    val(c, g) = ComerseAlOponente(cell, goku)
    
    assertEquals(goku estas Muerto, g)
    assertEquals(cell, c)
    
  }
  
  @Test
  def cellSeComeANumero18Test() ={
    val(c, n18) = ComerseAlOponente(cell, numero18)
    
    assertEquals(numero18 estas Muerto , n18)
    assertEquals(cell.movimientos++numero18.movimientos, c movimientos)
    
  }
  
  @Test
  def unNoMonstruoIntentaComerseAlOponentePeroSinSurgirEfectoTest() ={
    val(g, p) = ComerseAlOponente(goku, piccolo)
    
    assertEquals(goku, g)
    assertEquals(piccolo, p)
    
  }
  
  @Test
  def unMonoConvirtiendoseEnMonoMonoQuedaTest() ={
    val(m, k) = ConvertirseEnMono(mono, krilin)
    
    assertEquals(mono, m)
    assertEquals(krilin, k)
    
  }
  
  @Test
  def sinColaNoHayMonoTest() ={
    val(v, k) = ConvertirseEnMono(vegeta, krilin)
    
    assertEquals(vegeta, v)
    assertEquals(krilin, k)
    
  }
  
  @Test
  def sinSaiyajingNoHayMonoTest() ={
    val(p, k) = ConvertirseEnMono(piccolo, krilin)
    
    assertEquals(piccolo, p)
    assertEquals(krilin, k)
    
  }
  
  @Test
  def sinFotoDeLaLunaNoHayMonoTest() ={
    val(g, k) = ConvertirseEnMono(goku copy(inventario = List()), krilin)
    
    
    assertEquals(goku copy(inventario = List()), g)
    assertEquals(krilin, k)
    
  }
  
  @Test
  def gokuSeTransformaEnMonoTest() ={
    val(g, k) = ConvertirseEnMono(goku, krilin)
    
    assertEquals(Saiyajin(MonoGigante(500),true), g especie)
    assertEquals(1500, g energia)
    assertEquals(g energiaMaxima, g energia)
    assertEquals(krilin, k)
    
  }
  
  @Test
  def vegetaSeConvierteEnSSJTest(){
    val (v, g) = ConvertirseEnSuperSaiyajing(vegeta, goku)
    
    assertEquals(5005, v energiaMaxima)
    assertEquals(Saiyajin(SuperSaiyajin(1, 1001), false), v especie)
    assertEquals(goku, g)
  }
  
  @Test
  def vegetaNoSeConvierteEnSSJConPocoKiTest(){
    val (v, g) = ConvertirseEnSuperSaiyajing(vegeta disminuiEnergia 500, goku)
    
    assertEquals(v, vegeta disminuiEnergia 500)
    assertEquals(goku, g)
  }
  
  @Test
  def gokuSeConvierteEnSSJ2Test(){
    val (g, v) = ConvertirseEnSuperSaiyajing(goku, vegeta)
    
    assertEquals(5000, g energiaMaxima)
    assertEquals(Saiyajin(SuperSaiyajin(2, 500), true), g especie)
    assertEquals(vegeta, v)
  }
  
  @Test
  def monoNoSeConvierteEnSSJTest(){
    val (m, v) = ConvertirseEnSuperSaiyajing(mono, vegeta)
    
    assertEquals(mono, m)
    assertEquals(vegeta, v)
  }
  
  @Test
  def krilinNoSeConvierteEnSSJTest(){
    val (k, v) = ConvertirseEnSuperSaiyajing(krilin, vegeta)
    
    assertEquals(krilin, k)
    assertEquals(vegeta, v)    
  }
  
  @Test
  def ssjSeVuelveNormalAlQuedarInconscienteTest(){
    val (g, v) = (goku estas Inconsciente, vegeta)
    
    assertEquals(500, g energiaMaxima)
    assertEquals(Saiyajin(Normal, true), g especie)
    assertEquals(vegeta, v)
  }
  
  @Test
  def krilinSeFusionaConPiccolo(){
    val (f, v) = Fusion(krilin)(piccolo, vegeta)
    
    assertEquals(piccolo.inventario++krilin.inventario, f inventario)
    assertEquals(piccolo.energia + krilin.energia, f energia)
    assertEquals(piccolo.energiaMaxima + krilin.energiaMaxima, f energiaMaxima)
    assertEquals(Fusionado(piccolo, krilin), f especie)
    assertEquals(vegeta, v)
  }
  
  @Test
  def vegetaSeFusionaConGoku(){
    val (f, m) = Fusion(goku)(vegeta, majinBuu)
    
    assertEquals(vegeta.inventario++goku.inventario, f inventario)
    assertEquals(vegeta.energia + goku.energia, f energia)
    assertEquals(vegeta.energiaMaxima + goku.energiaMaxima, f energiaMaxima)
    assertEquals(Fusionado(vegeta, goku), f especie)
    assertEquals(majinBuu, m)
  }
  
  @Test
  def noTodosPuedenFusionarse(){
    val (k, c) = Fusion(numero18)(krilin, cell)
    
    assertEquals(krilin, k)
    assertEquals(cell, c)
  }
  
  @Test
  def vegetaSeFusionaConGokuYMuere(){
    val (f, m) = Fusion(goku)(vegeta, majinBuu)
    
    
    assertEquals(vegeta estas Muerto, f estas Muerto)
    assertEquals(majinBuu, m)
  }
  
  @Test
  def vegetaSeFusionaConGokuYQuedaInconsciente(){
    val (f, m) = Fusion(goku)(vegeta, majinBuu)
    
    
    assertEquals(vegeta estas Inconsciente, f estas Inconsciente)
    assertEquals(majinBuu, m)
  }
  
  @Test
  def piccoloUsaMagia(){
    val (p, g) = Magia(dejarInconsciente)(piccolo, goku)
    
    assertEquals(piccolo, p)
    assertEquals(goku estas Inconsciente, g)
  }
  
  @Test
  def majinBuuUsaMagia(){
    val (m, g) = Magia(dejarInconsciente)(majinBuu, goku)
    
    assertEquals(majinBuu, m)
    assertEquals(goku estas Inconsciente, g)
  }
  
  @Test
  def numero18NoUsaMagia(){
    val (n18, g) = Magia(convertirEnHumano)(numero18, numero18)
    
    assertEquals(numero18, n18)
  }
  
  @Test
  def krilinUsaMagia(){
    val (k, n18) = Magia(convertirEnHumano)(krilin, numero18)
    
    assertEquals(krilin.inventario diff esferasDelDragon, k.inventario)
    assertEquals(numero18 transformateEn Humano, n18)
  }
  
  
}