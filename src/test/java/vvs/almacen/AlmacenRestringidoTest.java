/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package vvs.almacen;

import vvs.contenido.ArchivoAudio;
import vvs.contenido.Contenido;

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
import vvs.contenido.Bonus;

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

        //contenidoAnadido = new ArrayList<Contenido>();
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
    	
    }

    /**
     * En Obtener nombre comprobamos que el nombre devuelto es igual al esperado
     * en los casos (con nombre, o nombre nulo)
     */
    @Test
    public void obternerNombreTest() {
        assertEquals(nombre, restringido.obtenerNombre());
        assertEquals(nombre, real.obtenerNombre());
        Almacen nuevo = new AlmacenReal(null);
        assertNull(nuevo.obtenerNombre());
    }

    @Test
    public void agregarContenido() throws ExcepcionAlmacen {
        int size = 0;
        boolean excepcion = false;

        /*Añadir contenido: que se añada sin ningun problema*/
        try {
            restringido.agregarContenido(coldplay1);
            size++;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
        }
        assertFalse(excepcion);
        assertEquals(size, restringido.obtenerContenidos().size());
        assertEquals(restringido.obtenerContenidos().iterator().next(), coldplay1);
    }    
    
     /**
     * test que comprueba que no se puede agregar como contenido un null a un almacen
     * restringido
     */
    @Test
    public void agregarContenidoNullTest() {
        int size = 0;
        boolean excepcion = false;

        try {
            restringido.agregarContenido(null);
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
        } 
        assertTrue(excepcion);
        assertEquals(size, restringido.obtenerContenidos().size());
    }
    
    
    /**
     * test que comprueba que no se pueden agregar contenidos duplicados a un
     * almacen restringido
     */
    @Test
    public void agregarContenidoDuplicadoTest() {
        int size = 0;
        boolean excepcion = false;

        /*Añadir contenido: que se añada sin ningun problema*/
        try {
            restringido.agregarContenido(coldplay1);
            size++;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
        }
        assertFalse(excepcion);
        assertEquals(size, restringido.obtenerContenidos().size());
        assertEquals(restringido.obtenerContenidos().iterator().next(), coldplay1);
        
        /*Añadir contenido: Contenido repetido*/
        try {
            restringido.agregarContenido(coldplay1);
            size++;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
        }

        assertTrue(excepcion);
        assertEquals(size, restringido.obtenerContenidos().size());
        assertEquals(restringido.obtenerContenidos().iterator().next(), coldplay1);
    }

    /**
     * test que comprueba la eliminacion de contenidos en almacen restringido
     */
    @Test
    public void eliminarContenidoTest() {
        int size = 0;
        boolean excepcion = false;

        /*Añadir contenido: que se añada sin ningun problema*/
        try {
            restringido.agregarContenido(coldplay1);
            size++;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
        }
        assertFalse(excepcion);
        assertEquals(size, restringido.obtenerContenidos().size());
        assertEquals(restringido.obtenerContenidos().iterator().next(), coldplay1);

        /*Eliminar contenido: Sin ningun problema*/
        try {
            restringido.eliminarContenido(coldplay1);
            size--;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
        }

        assertFalse(excepcion);
        assertEquals(size, restringido.obtenerContenidos().size());
        assertFalse(restringido.obtenerContenidos().contains(coldplay1));
    }
    
    
    /**
     * test que cmprueba que no se pueden eliminar contenidos que no se hayan agregado
     * en un almacen restringido
     */
    @Test
    public void eliminarContenidoNoAgregadoTest() {
        int size = 0;
        boolean excepcion = false;

        try {
            restringido.eliminarContenido(coldplay1);
            size--;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
        }

        assertTrue(excepcion);
        assertEquals(size, restringido.obtenerContenidos().size());
        assertFalse(restringido.obtenerContenidos().contains(coldplay1));
    }
    
    /**
     * test que comprueba que no se puede agregar un contenido bonus si ya
     * se habia agregado el mismo archivo de audio que se uso para crear ese bonus,
     * en un almacen restringido.
     * @throws ExcepcionAlmacen 
     */
    @Test
    public void agregarContenidoDuplicadoContenidoEnOtroContenidoTest() throws ExcepcionAlmacen {
        int size = 0;
        boolean excepcion = false;

        //ArchivoAudio Audio = new ArchivoAudio("titulo", "URLAudio", 5, "genero");
        ArchivoAudio Audio = Mockito.mock(ArchivoAudio.class);
        Contenido coldplay2 = Audio;

        Bonus bonus = new Bonus(Audio, new ArchivoAudio("titulo2", "URLAudio2", 5, "genero2"));

        /*Añadir contenido: que se añada sin ningun problema*/
        try {
            restringido.agregarContenido(coldplay2);
            size++;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
        }
        assertFalse(excepcion);
        assertEquals(size, restringido.obtenerContenidos().size());
        assertEquals(restringido.obtenerContenidos().iterator().next(), coldplay2);

        try {
            restringido.agregarContenido(bonus);
            size++;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
        }

        assertTrue(excepcion);
        assertEquals(size, restringido.obtenerContenidos().size());
        restringido.eliminarContenido(coldplay2);
        assertTrue(restringido.obtenerContenidos().isEmpty());
    }

    /**
     * test que comprueba la busqeuda sobre un almacen restringido vacio
     * @throws ExcepcionAlmacen 
     */
    @Test
    public void buscarVacioTest() throws ExcepcionAlmacen {
        assertTrue(restringido.buscar("Coldplay").isEmpty());
    }

    /**
     * test que comprueba la busqueda normal de contenido, en un almacen restringido
     * @throws ExcepcionAlmacen 
     */
    @Test
    public void buscarTest() throws ExcepcionAlmacen {
        restringido.agregarContenido(coldplay1);
        assertFalse(restringido.buscar("ColdPLay Sound").isEmpty());
        assertTrue(restringido.buscar("ColdplAY Sound").contains(coldplay1));
        assertEquals(restringido.buscar("Coldplay SouND").size(), 1);
    }

    /**
     * test que comprueba la busqueda con un null,, en un almacen restringido
     * @throws ExcepcionAlmacen 
     */
    @Test
    public void buscarNullTest() throws ExcepcionAlmacen {
        assertTrue(restringido.buscar(null).isEmpty());
    }

    /**
     * test que comprueba la busqueda de un elemento que no esta agregado
     * en un almacen restringido
     * @throws ExcepcionAlmacen 
     */
    @Test
    public void buscarNoExistenteTest() throws ExcepcionAlmacen {
        restringido.agregarContenido(coldplay1);
        assertTrue(restringido.buscar("Amy").isEmpty());
        assertFalse(restringido.buscar("Amy").contains(coldplay1));
    }

    /**
     * test que comprueba la busqeuda de multiples contenidos a la vez, en un
     * almacen restringido
     * @throws ExcepcionAlmacen 
     */
    @Test
    public void buscarVariosTest() throws ExcepcionAlmacen {
        restringido.agregarContenido(coldplay1);
        restringido.agregarContenido(winehouse);
        
        assertTrue(restringido.buscar("").contains(coldplay1));
        assertTrue(restringido.buscar("").contains(winehouse));
        assertEquals(restringido.buscar("").size(), 2);
    }
    
    /**
     * test que comprueba la busqueda de un elemento con espacios antes y despues
     * @throws ExcepcionAlmacen 
     */
    @Test
    public void buscarConEspaciosTest() throws ExcepcionAlmacen {
        restringido.agregarContenido(coldplay1);
        assertFalse(restringido.buscar(" Sound ").isEmpty());
        assertTrue(restringido.buscar(" Sound ").contains(coldplay1));
        assertEquals(restringido.buscar(" Sound ").size(), 1);
    }
    
    /**
     * test que comprueba la busqueda de un elemento con palabras desordenadas
     * @throws ExcepcionAlmacen 
     */
    @Test
    public void buscarPalabrasDesordenadasTest() throws ExcepcionAlmacen {
        restringido.agregarContenido(coldplay1);
        assertFalse(restringido.buscar(" Sound Speed ").isEmpty());
        assertTrue(restringido.buscar(" Sound Speed ").contains(coldplay1));
        assertEquals(restringido.buscar(" Sound Speed ").size(), 1);
    }
    
    /**
     * test que comprueba que si se sobrepasa el numero de busqeudas permitidas 
     * en un almacen restringido, salta la excepcion
     * @throws ExcepcionAlmacen 
     */    
    @Test(expected = ExcepcionAlmacen.class)
    public void sobrepasarNumeroDeBusquedasTest() throws ExcepcionAlmacen {
        /* Buscamos n veces (en nuestro caso 3 veces) y luego comprobamos que se envia la restriccion*/
        restringido.agregarContenido(coldPlayWineHouse);
                
        assertTrue(restringido.buscar("Rehab").contains(coldPlayWineHouse));     
        assertTrue(restringido.buscar("Rehab").contains(coldPlayWineHouse));
        assertTrue(restringido.buscar("Rehab").contains(coldPlayWineHouse));

        /*Al realizar la cuerta busqueda, deberia lanzar la excepcion */
        restringido.buscar("Rehab");
    }

    /**
     * test que comprueba que si se espera el tiempo necesario antes de sobrepasar
     * el numero de busqeudas permitidas en un almacen restringido, no salta
     * la excepcion
     * @throws ExcepcionAlmacen 
     */ 
    @Test
    public void esperarYSobrepasarNumeroDeBusquedasTest() throws ExcepcionAlmacen {
        /* Buscamos n veces (en nuestro caso 3 veces) y luego comprobamos que se envia la restriccion*/
        restringido.agregarContenido(coldplay1);
        
        assertTrue(restringido.buscar("Coldplay").contains(coldplay1));
        assertTrue(restringido.buscar("Coldplay").contains(coldplay1));
        assertTrue(restringido.buscar("Coldplay").contains(coldplay1));
        
        /*Esperamos los minutos que hemos indicado en el almacen restringido */
        try {
            TimeUnit.MINUTES.sleep(minutos);
        } catch (InterruptedException ex) {

        }
        /*Al sobrepasar el tiempo no deberia de mandarnos ninguna exception y realizar bien la busqueda */
        assertTrue(restringido.buscar("Coldplay").contains(coldplay1));
    }

    /**
     * test que comprueba que un almacen restringido no se modifica al establecer proveedor
     */
    @Test
    public void establecerProveedor() throws ExcepcionAlmacen{
        
        Almacen proveedor1 = restringido.obtenerProveedor();
        restringido.establecerProveedor(restringido);
        Almacen proveedor2 = restringido.obtenerProveedor();
        assertEquals(proveedor1,proveedor2);
    }
    
    /**
     * test que comprueba que obtener proveedor en un almacen restringido 
     * devuelve siempre null (redundante respecto al test anterior)
     */
    @Test
    public void obtenerProveedor(){
        assertEquals(restringido.obtenerProveedor(),null);
    }

}
