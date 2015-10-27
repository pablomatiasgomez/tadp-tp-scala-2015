package utn.tadp.dragonball

import org.junit.Before
import org.junit.Test

import org.junit.Assert._

class Persona_Test {
  var persona: Persona = null

  @Before
  def setup() = {
    persona = new Persona(1)
  }

  @Test
  def cumpliAnio_test() = {
    persona.cumpliAnio
    assertEquals(2, persona.edad)
  }
}