/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package almacen;

import contenido.ArchivoAudio;
import contenido.Contenido;

import java.util.ArrayList;
import java.util.Collection;
import java.util.concurrent.TimeUnit;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

/**
 *
 * @author hmia
 */
public class AlmacenRestringidoTest {

    Almacen real;
    Almacen restringido;
    int busquedas;
    int minutos;
    int size;
    String nombre = "EMI";

    Contenido coldplay1;
    Contenido winehouse;
    Contenido coldPlayWineHouse;
    Collection<Contenido> contenidoAnadido;

    /**
     * Iniciamos los datos que necesitamos para realizar las pruebas
     *
     * @throws ExcepcionAlmacen
     */
    @Before
    public void setUp() throws ExcepcionAlmacen {
        busquedas = 3;
        minutos = 1;
        size = 0;

        contenidoAnadido = new ArrayList<Contenido>();
        real = new AlmacenReal(nombre);
        restringido = new AlmacenRestringido(real, busquedas, minutos);

//        coldplay1 = new ArchivoAudio("Coldplay: Speed of Sound", "http://servidor/coldplay/xy/7", 288, "Rock alternativo");
//        winehouse = new ArchivoAudio("Amy Winehouse: Rehab", "http://servidor/winehouse/back2black/1", 215, "Soul");
//        coldPlayWineHouse = new ArchivoAudio("Coldplay: Rehab", "http://servidor/alavigne/bestdamnthing/1", 216, "Punk pop");

        coldplay1 = Mockito.mock(ArchivoAudio.class);
        Mockito.when(coldplay1.obtenerTitulo()).thenReturn("Coldplay: Speed of Sound");
        Mockito.when(coldplay1.buscar(Mockito.anyString())).thenCallRealMethod();
        winehouse = Mockito.mock(ArchivoAudio.class);
        Mockito.when(winehouse.obtenerTitulo()).thenReturn("Amy Winehouse: Rehab");
        Mockito.when(winehouse.buscar(Mockito.anyString())).thenCallRealMethod();
        coldPlayWineHouse = Mockito.mock(ArchivoAudio.class);
        Mockito.when(coldPlayWineHouse.obtenerTitulo()).thenReturn("Coldplay: Rehab");
        Mockito.when(coldPlayWineHouse.buscar(Mockito.anyString())).thenCallRealMethod();
        /*anadimos comportamiento a los mocks*/
    	
        
    	
        
        restringido.agregarContenido(coldplay1);
        contenidoAnadido.add(coldplay1);
        size++;
        restringido.agregarContenido(winehouse);
        contenidoAnadido.add(winehouse);
        size++;
        restringido.agregarContenido(coldPlayWineHouse);
        contenidoAnadido.add(coldPlayWineHouse);
        size++;

    }

    @Test
    public void obternerNombreTest() {
        assertEquals(nombre, restringido.obtenerNombre());
        assertEquals(nombre, real.obtenerNombre());
        Almacen nuevo = new AlmacenReal(null);
        assertNull(nuevo.obtenerNombre());
    }

    /**
     * Añadir un contenido ya existente en el almacen restringido en el real,
     * debe enviar una ExcepcionAlmacen
     *
     * @throws ExcepcionAlmacen
     */
    @Test(expected = ExcepcionAlmacen.class)
    public void agregarContenidoException() throws ExcepcionAlmacen {
        real.agregarContenido(winehouse);

    }

    @Test
    public void agregarContenido() throws ExcepcionAlmacen {
//        Contenido coldplay = new ArchivoAudio("Coldplay: Speed of Sound 1",
//                "http://servidor/coldplay/xy/7", 288, "Rock alternativo");
        Contenido coldplay = Mockito.mock(ArchivoAudio.class);
        Mockito.when(coldplay.obtenerTitulo()).thenReturn("Coldplay: Speed of Sound 1");
        Mockito.when(coldplay.buscar(Mockito.anyString())).thenCallRealMethod();
        real.agregarContenido(coldplay);
        size++;
        assertTrue(real.obtenerContenidos().contains(coldplay));
        assertEquals(size, restringido.obtenerContenidos().size());

    }

    /**
     * Eliminar contenido winehouse (real) deberia de eliminar sin problemas,
     * luego al volverlo a intentar (restringido) deberia lanzar una exception
     *
     * @throws ExcepcionAlmacen
     */
    @Test(expected = ExcepcionAlmacen.class)
    public void eliminarContenidoException() throws ExcepcionAlmacen {
        real.eliminarContenido(winehouse);
        size--;
        assertFalse(real.obtenerContenidos().contains(winehouse));
        restringido.eliminarContenido(winehouse);
        size--;
    }

    /**
     * Verificamos que al realizar n busquedas la sexta no pueda realizar la
     * busqueda y nos lance una excepcion
     *
     * @throws ExcepcionAlmacen
     */
    
    @Test(expected = ExcepcionAlmacen.class)
    public void buscarExcepcionTest() throws ExcepcionAlmacen {
        /* Buscamos n veces (en nuestro caso 3 veces) y luego comprobamos que se envia la restriccion*/
        int valorEsperado = 2; /* winehouse, coldPlayWineHouse */
        
        assertEquals(valorEsperado, restringido.buscar("Rehab").size());
        valorEsperado = 1; /* coldPlay*/
        
        assertEquals(valorEsperado, restringido.buscar("Speed").size());
        valorEsperado = size; /* todas las opciones */

        restringido.buscar("Speed");       

        /*Al realizar la cuerta busqueda, deberia lanzar la excepcion */
        restringido.buscar("Speed");
  
    }

    /**
     * Realizamos las busquedas y luego esperamos a que la sexta la haga cuando
     * sin ningun problema cuando se lance la excepcion.
     */
    @Test
    public void buscarTest() throws ExcepcionAlmacen {
        /* Buscamos n veces (en nuestro caso 3 veces) y luego comprobamos que se envia la restriccion*/
        int valorEsperado = 1; /* winehouse*/
        
        assertEquals(valorEsperado, restringido.buscar("Amy").size());
        valorEsperado = 2; /* Coldplay*/
        
        assertEquals(valorEsperado, restringido.buscar("Cold").size());

        restringido.buscar("Cold");
        /*Esperamos los minutos que hemos indicado en el almacen restringido */
        try {
            TimeUnit.MINUTES.sleep(minutos);
        } catch (InterruptedException ex) {

        }
        /*Al sobrepasar el tiempo no deberia de mandarnos ninguna exception y realizar bien la busqueda */
        restringido.buscar("Cold");

    }

    /**
     * Obtener contenidos
     */
    @Test
    public void obternerContenidosTest() {
        boolean exception = false;
        try {

            assertNotNull(restringido.obtenerContenidos());
            assertTrue(real.obtenerContenidos().containsAll(contenidoAnadido));
            assertEquals(size, restringido.obtenerContenidos().size());
            assertTrue(real.obtenerContenidos().containsAll(contenidoAnadido));

            Contenido nuevoContenido = null;
            real.agregarContenido(nuevoContenido);
            contenidoAnadido.add(nuevoContenido);
            size++;
            assertTrue(real.obtenerContenidos().contains(nuevoContenido));
            assertEquals(size, restringido.obtenerContenidos().size());
            assertTrue(real.obtenerContenidos().containsAll(contenidoAnadido));

        } catch (ExcepcionAlmacen ex) {
            exception = true;
        }
        assertFalse(exception);
    }

}
