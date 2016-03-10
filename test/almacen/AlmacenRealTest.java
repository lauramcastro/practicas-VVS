package almacen;

import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;

public class AlmacenRealTest {
	
	private String nombreAlmacen;
	private Almacen almacen;
	
	@Before
	public void init(){
		nombreAlmacen = "Almacen Real 1";
		almacen = new AlmacenReal(nombreAlmacen);
	}
	
	@Test
	public void obtenerNombreTest(){
		assertTrue(nombreAlmacen.equalsIgnoreCase(almacen.obtenerNombre()));
	}
	

}
