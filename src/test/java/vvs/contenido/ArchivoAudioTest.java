package vvs.contenido;

import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Collection;

import static org.junit.Assert.*;

import net.java.quickcheck.Generator;
import net.java.quickcheck.generator.PrimitiveGenerators;
import net.java.quickcheck.generator.iterable.Iterables;
import net.java.quickcheck.characteristic.Classification;

/**
 * Casos de que prueban la funcionalidad de busqueda sobre un
 * contenido de tipo ArchivoAudio
 */

public class ArchivoAudioTest {

    private Contenido lavigne1;
    private Contenido lavigne2;
    private Contenido eminem1;

    @Before
    public void initialize() throws ExcepcionContenido {
	lavigne1 = new ArchivoAudio("Avril Lavigne: Girlfriend", "http://servidor/alavigne/bestdamnthing/1", 216, "Punk pop");
	lavigne2 = new ArchivoAudio("Avril Lavigne: I can do better", "http://servidor/alavigne/bestdamnthing/2", 197, "Punk pop");
	eminem1 = new ArchivoAudio("Eminem", "http://servidor/alavigne/bestdamnthing/1", 216, "Rap");
    }

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
    public void buscarPorUnaPalabraConEspaciosAntesDespuesTest() {
	Collection <Contenido> col = new ArrayList<Contenido>();
	col.add(lavigne1);
	Collection <Contenido> resultado = lavigne1.buscar(" Lavigne ");
	assertEquals(resultado, col);
    }

    @Test
    public void busquedasAleatorias() {
	// Classification c = new Classification();
	for (ArchivoAudio contenido : Iterables.toIterable(new GeneradorArchivoAudio())) {
	    for (String cadena : Iterables.toIterable(PrimitiveGenerators.strings())) {
		// System.out.println("[busquedasAleatorias] ===> " + cadena + " ? " + contenido);
		if (contenido.obtenerTitulo().contains(cadena)) {
		     c.classifyCall("presente");
		}
		else {
		    c.classifyCall("ausente");
		}
		assertFalse(contenido.obtenerTitulo().contains(cadena)
			    &&
			    contenido.buscar(cadena).isEmpty());
	    }
	}
	for (Object cat : c.getCategories()) {
    	    System.out.println("[busquedasAleatorias] ===> " + cat + " => " + c.getFrequency(cat));
    	}
    }

    @Test(expected = RuntimeException.class)
    public void agregarContenidoTest() {
	lavigne1.agregar(lavigne2, null);
    }

    @Test(expected = RuntimeException.class)
    public void eliminarContenidoTest() {
	lavigne1.eliminar(lavigne1);
    }

    @Test
    public void notEqualsTest() {
	assertEquals(false, lavigne1.equals(eminem1));
    }

    @Test
    public void obtenerPadreTest() {
	assertEquals(null, lavigne1.obtenerPadre());
    }

    @Test
    public void obtenerListaReproduccionTest() {
	Collection<String> resultado = lavigne1.obtenerListaReproduccion();
	assertEquals(true, resultado.toArray()[0].equals("http://servidor/alavigne/bestdamnthing/1"));

    }

    @Test
    public void obtenerDuracionTest() {
	assertEquals(216, lavigne1.obtenerDuracion());
    }

    @Test(expected =ExcepcionContenido.class)
    public void ObtenerDuracionNegativaTest() throws ExcepcionContenido {
	new ArchivoAudio("Avril Lavigne: I can do better", "http://servidor/alavigne/bestdamnthing/2", -197, "Punk pop");
    }

    // Generadores propios

    private class GeneradorArchivoAudio implements Generator<ArchivoAudio> {
    	private Generator<String> s = PrimitiveGenerators.strings();
    	private Generator<Integer> i = PrimitiveGenerators.integers(0); // valor mínimo

    	@Override
    	public ArchivoAudio next() {
	    try {
		return new ArchivoAudio(s.next(), s.next(), i.next(), s.next());
	    } catch (ExcepcionContenido e) {
		System.err.println("No se ha podido generar el archivo de audio" + e.getMessage());
		return null;
	    }
    	}
    }

}
