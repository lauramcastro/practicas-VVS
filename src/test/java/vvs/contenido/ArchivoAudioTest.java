package vvs.contenido;

import org.junit.Test;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Collection;

public class ArchivoAudioTest {

    /* Casos de que prueban la funcionalidad de busqueda sobre un contenido de tipo ArchivoAudio */

	Contenido lavigne1 = new ArchivoAudio("Avril Lavigne: Girlfriend", "http://servidor/alavigne/bestdamnthing/1", 216, "Punk pop");
	Contenido lavigne2 = new ArchivoAudio("Avril Lavigne: I can do better", "http://servidor/alavigne/bestdamnthing/2", 197, "Punk pop");
	Contenido eminem1 = new ArchivoAudio("Eminem", "http://servidor/alavigne/bestdamnthing/1", 216, "Rap");
	
    @Test
    public void buscarPorVariasPalabrasTest() {
	    Collection <Contenido> col = new ArrayList<Contenido>();
	    col.add(lavigne1);
	    Collection <Contenido> resultado = lavigne1.buscar("Avril Lavigne");
		assertEquals(resultado, col);
    }
	
    @Test
    public void buscarPorVariasPalabrasDesordenadasTest() {
		Collection <Contenido> col = new ArrayList<Contenido>();
	    col.add(lavigne1);
		Collection <Contenido> resultado = lavigne1.buscar("Lavigne Avril");
		assertEquals(resultado, col);
    }
    
    @Test
    public void buscarPorUnaPalabraExactaTest() {
    	Collection <Contenido> col = new ArrayList<Contenido>();
        col.add(lavigne1);
        Collection <Contenido> resultado = lavigne1.buscar("Lavigne");
    	assertEquals(resultado, col);
		assertEquals(1, resultado.size());
    }
    
    @Test
    public void buscarPorUnaPalabraConMayusculasMinusculasTest() {
		Collection <Contenido> resultado = lavigne1.buscar("LavIgnE");
		Collection <Contenido> col = new ArrayList<Contenido>();
	    col.add(lavigne1);
		assertEquals(resultado, col);
    }
    
    @Test
    public void buscarPorUnaPalabraConEspaciosAntesDespuesTest(){
		Collection <Contenido> col = new ArrayList<Contenido>();
	    col.add(lavigne1);
	    Collection <Contenido> resultado = lavigne1.buscar(" Lavigne ");
		assertEquals(resultado, col);
    }
    
    @Test(expected = RuntimeException.class)
	public void agregarContenidoTest(){
		lavigne1.agregar(lavigne2, null);
	}
	@Test(expected = RuntimeException.class)
	public void eliminarContenidoTest(){
		lavigne1.eliminar(lavigne1);
	}
	
	@Test
	public void notEqualsTest(){
		assertEquals(false, lavigne1.equals(eminem1));
	}
	
	@Test
	public void obtenerPadreTest(){
		assertEquals(null, lavigne1.obtenerPadre());
	}
		
	@Test
	public void obtenerListaReproduccionTest(){
		Collection<String> resultado = lavigne1.obtenerListaReproduccion();
		assertEquals(true, resultado.toArray()[0].equals("http://servidor/alavigne/bestdamnthing/1"));
				
	}
	
	@Test
	public void obtenerDuracionTest(){
		assertEquals(216, lavigne1.obtenerDuracion());
	}
    
	@Test(expected =ExcepcionContenido.class)
	public void ObtenerDuracionNegativaTest(){
		new ArchivoAudio("Avril Lavigne: I can do better", "http://servidor/alavigne/bestdamnthing/2", -197, "Punk pop");
	}
}
