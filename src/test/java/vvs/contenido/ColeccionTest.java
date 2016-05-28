package vvs.contenido;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Collection;

import org.junit.Before;
import org.junit.Test;

/**
 * Clase de pruebas sin ningún tipo de documentación.
 */
public class ColeccionTest {

	private Coleccion coleccionCorrecta;
	private ArchivoAudio eminem1;
	private ArchivoAudio eminem2;
	private ArchivoAudio eminem3;
	private ArchivoAudio eminem4;
	
	@Before
	public void initialize() throws ExcepcionContenido{
		coleccionCorrecta = new Coleccion("Eminem");
		eminem1 = new ArchivoAudio("Lose Yourself", "url", 10, "Rap");
		eminem2 = new ArchivoAudio("Mockingbird", "url2", 8, "Rap");
		eminem3 = new ArchivoAudio("Rapgod", "url3", 9, "Rap");
		eminem4 = new ArchivoAudio("8 Mile", "url4", 11, "Rap");
	}
	
	@Test
	public void obtenerDuracionTest(){
		coleccionCorrecta.agregar(eminem1, null);
		assertEquals(coleccionCorrecta.obtenerDuracion(), eminem1.obtenerDuracion());
		coleccionCorrecta.agregar(eminem2, eminem1);
		assertEquals(coleccionCorrecta.obtenerDuracion(), eminem1.obtenerDuracion() + eminem2.obtenerDuracion());
	}

	@Test
	public void obtenerListaReproduccionTest(){
		coleccionCorrecta.agregar(eminem1, null);
		Collection<String> listaRep = coleccionCorrecta.obtenerListaReproduccion();
		assertEquals(listaRep, eminem1.obtenerListaReproduccion());
	}
	
	@Test
	public void obtenerGeneroTest(){
		assertEquals(coleccionCorrecta.obtenerGenero(), "Mix");
	}
	
	
	/* Este test no me tiene sentido O.O ¿por qué sigue aquí? */
	@Test
	public void buscarTest(){
		Collection<Contenido> col = coleccionCorrecta.buscar("nem");
		Collection<Contenido> col2 = new ArrayList<Contenido>();
		col2.add(coleccionCorrecta);
		assertEquals(col, col2);
	}
	
	@Test
	public void agregarTest(){
		eminem1.establecerPadre(null);
		coleccionCorrecta.agregar(eminem1, null);
		assertEquals(coleccionCorrecta.obtenerListaReproduccion().size(), 1);
		coleccionCorrecta.agregar(eminem2, eminem1);
		assertEquals(coleccionCorrecta.obtenerListaReproduccion().size(), 2);
		coleccionCorrecta.agregar(eminem3, eminem2);
		assertEquals(coleccionCorrecta.obtenerListaReproduccion().size(), 3);
		coleccionCorrecta.agregar(eminem4, eminem3);
		assertEquals(coleccionCorrecta.obtenerListaReproduccion().size(), 4);
	}
	
	@Test
	public void eliminarTest(){
		coleccionCorrecta.agregar(eminem1, null);
		coleccionCorrecta.agregar(eminem2, eminem1);
		assertEquals(coleccionCorrecta.obtenerListaReproduccion().size(), 2);
		coleccionCorrecta.eliminar(eminem1);
		assertEquals(coleccionCorrecta.obtenerListaReproduccion().size(), 1);
		coleccionCorrecta.eliminar(eminem2);
		assertEquals(coleccionCorrecta.obtenerListaReproduccion().size(), 0);
	}
	
	@Test
	public void recuperarTest(){
		coleccionCorrecta.agregar(eminem1, null);
		assertEquals(coleccionCorrecta.recuperar(0).obtenerTitulo(), eminem1.obtenerTitulo());
	}
}
