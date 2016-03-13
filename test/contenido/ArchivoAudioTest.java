package contenido;

import org.junit.Test;
import static org.junit.Assert.*;

import java.util.Collection;

public class ArchivoAudioTest {

    /* Casos de que prueban la funcionalidad de busqueda sobre un contenido de tipo ArchivoAudio */

    @Test
    public void buscarPorVariasPalabrasTest() {
	Contenido lavigne1 = new ArchivoAudio("Avril Lavigne: Girlfriend", "http://servidor/alavigne/bestdamnthing/1", 216, "Punk pop");
	Collection <Contenido> resultado = lavigne1.buscar("Avril Lavigne");
	assertEquals(1, resultado.size());
    }
	
    @Test
    public void buscarPorVariasPalabrasDesordenadasTest() {
	Contenido lavigne1 = new ArchivoAudio("Avril Lavigne: Girlfriend", "http://servidor/alavigne/bestdamnthing/1", 216, "Punk pop");
	Collection <Contenido> resultado = lavigne1.buscar("Lavigne Avril");
	assertEquals(1, resultado.size());
    }
    
    @Test
    public void buscarPorUnaPalabraExactaTest() {
	Contenido lavigne1 = new ArchivoAudio("Avril Lavigne: Girlfriend", "http://servidor/alavigne/bestdamnthing/1", 216, "Punk pop");
	Collection <Contenido> resultado = lavigne1.buscar("Lavigne");
	assertEquals(1, resultado.size());
    }
    
    @Test
    public void buscarPorUnaPalabraConMayusculasMinusculasTest() {
	Contenido lavigne1 = new ArchivoAudio("Avril Lavigne: Girlfriend", "http://servidor/alavigne/bestdamnthing/1", 216, "Punk pop");
	Collection <Contenido> resultado = lavigne1.buscar("LavIgnE");
	assertEquals(1, resultado.size());
    }
    
    @Test
    public void buscarPorUnaPalabraConEspaciosAntesDespuesTest(){
	Contenido lavigne1 = new ArchivoAudio("Avril Lavigne: Girlfriend", "http://servidor/alavigne/bestdamnthing/1", 216, "Punk pop");
	Collection <Contenido> resultado = lavigne1.buscar(" Lavigne ");
	assertEquals(1, resultado.size());
    }
    
    @Test(expected = RuntimeException.class)
	public void agregarContenidoTest(){
		Contenido lavigne1 = new ArchivoAudio("Avril Lavigne: Girlfriend", "http://servidor/alavigne/bestdamnthing/1", 216, "Punk pop");
		Contenido lavigne2 = new ArchivoAudio("Avril Lavigne: I can do better", "http://servidor/alavigne/bestdamnthing/2", 197, "Punk pop");
		lavigne1.agregar(lavigne2, null);
	}
	@Test(expected = RuntimeException.class)
	public void eliminarContenidoTest(){
		Contenido lavigne1 = new ArchivoAudio("Avril Lavigne: Girlfriend", "http://servidor/alavigne/bestdamnthing/1", 216, "Punk pop");
		lavigne1.eliminar(lavigne1);
	}
	
	@Test
	public void notEqualsTest(){
		Contenido lavigne1 = new ArchivoAudio("Avril Lavigne: Girlfriend", "http://servidor/alavigne/bestdamnthing/1", 216, "Punk pop");
		Contenido otro = new ArchivoAudio("Otro: Cancioncita", "http://servidor/otro/cancioncita/1", 216, "Punk pop");
		
		assertEquals(false,lavigne1.equals(otro));
	}
	
	@Test
	public void obtenerPadreTest(){
		Contenido lavigne1 = new ArchivoAudio("Avril Lavigne: Girlfriend", "http://servidor/alavigne/bestdamnthing/1", 216, "Punk pop");
		assertEquals(null,lavigne1.obtenerPadre());
	}
		
	@Test
	public void obtenerListaReproduccionTest(){
		Contenido lavigne1 = new ArchivoAudio("Avril Lavigne: Girlfriend", "http://servidor/alavigne/bestdamnthing/1", 216, "Punk pop");
		Collection<String> resultado = lavigne1.obtenerListaReproduccion();
		assertEquals(1,resultado.size());
		assertEquals(true, resultado.toArray()[0].equals("http://servidor/alavigne/bestdamnthing/1"));
				
	}
    
}
