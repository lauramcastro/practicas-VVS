package vvs.contenido;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Collection;

import org.junit.Test;

public class ColeccionTest {

	private Coleccion coleccionCorrecta = new Coleccion("Eminem");
	private ArchivoAudio eminem1 = new ArchivoAudio("Lose Yourself", "url", 10, "Rap");
	private ArchivoAudio eminem2 = new ArchivoAudio("Mockingbird", "url2", 8, "Rap");
	
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
	
	
	/* Este test no me tiene sentido O.O */
	@Test
	public void buscarTest(){
		Collection<Contenido> col = coleccionCorrecta.buscar("nem");
		Collection<Contenido> col2 = new ArrayList<Contenido>();
		col2.add(coleccionCorrecta);
		assertEquals(col, col2);
	}
	
	@Test
	public void agregarTest(){
		coleccionCorrecta.agregar(eminem1, null);
		assertEquals(coleccionCorrecta.obtenerListaReproduccion().size(), 1);
		coleccionCorrecta.agregar(eminem2, eminem1);
		assertEquals(coleccionCorrecta.obtenerListaReproduccion().size(), 2);
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
