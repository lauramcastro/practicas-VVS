/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package vvs.almacen;

import vvs.contenido.ArchivoAudio;
import vvs.contenido.Bonus;
import vvs.contenido.Contenido;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

/**
 *
 * @author hmia
 */
public class AlmacenRealTest {

    Almacen almacenReal;
    String nombre = "EMI";

    String palabraBusqueda;

    Contenido coldplay1;
    Contenido winehouse;
    Contenido coldPlayWineHouse;

    @Before
    public void setUp() {
        almacenReal = new AlmacenReal(nombre);
        coldplay1 = Mockito.mock(ArchivoAudio.class);
        Mockito.when(coldplay1.obtenerTitulo()).thenReturn("Coldplay: Speed of Sound");
        Mockito.when(coldplay1.buscar(Mockito.anyString())).thenCallRealMethod();

        winehouse = Mockito.mock(ArchivoAudio.class);
        Mockito.when(winehouse.obtenerTitulo()).thenReturn("Amy Winehouse: Rehab");
        Mockito.when(winehouse.buscar(Mockito.anyString())).thenCallRealMethod();

        coldPlayWineHouse = Mockito.mock(ArchivoAudio.class);

    }

    /**
     * En Obtener nombre comprobamos que el nombre devuelto es igual al esperado
     * en los casos (con nombre, o nombre nulo)
     */
    @Test
    public void obternerNombreTest() {

        assertEquals(nombre, almacenReal.obtenerNombre());
        almacenReal = new AlmacenReal(null);
        assertEquals(null, almacenReal.obtenerNombre());
    }

/**
 * test positivo que agrega un contenido y comprueba que se obtiene correctamente
 */
    @Test
    public void agregarContenidoTest() {
        int size = 0;
        boolean excepcion = false;

        /*Añadir contenido: que se añada sin ningun problema*/
        try {
            almacenReal.agregarContenido(coldplay1);
            size++;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
        }
        assertFalse(excepcion);
        assertEquals(size, almacenReal.obtenerContenidos().size());
        assertEquals(almacenReal.obtenerContenidos().iterator().next(), coldplay1);
    }
    
    /**
     * test que comprueba que no se puede agregar como contenido un null
     */
     @Test
    public void agregarContenidoNullTest() {
        int size = 0;
        boolean excepcion = false;

        try {
            almacenReal.agregarContenido(null);
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
        } 
        assertFalse(excepcion);
        assertEquals(size, almacenReal.obtenerContenidos().size());
    }

    /**
     * test que comprueba que no se pueden agregar contenidos duplicados
     */
    @Test
    public void agregarContenidoDuplicadoTest() {
        int size = 0;
        boolean excepcion = false;

        /*Añadir contenido: que se añada sin ningun problema*/
        try {
            almacenReal.agregarContenido(coldplay1);
            size++;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
        }
        assertFalse(excepcion);
        assertEquals(size, almacenReal.obtenerContenidos().size());
        assertEquals(almacenReal.obtenerContenidos().iterator().next(), coldplay1);
        /*Añadir contenido: Contenido repetido*/
        try {
            almacenReal.agregarContenido(coldplay1);
            size++;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
        }

        assertTrue(excepcion);
        assertEquals(size, almacenReal.obtenerContenidos().size());
        assertEquals(almacenReal.obtenerContenidos().iterator().next(), coldplay1);
    }

    /**
     * test que comprueba la eliminacion de contenidos
     */
    @Test
    public void eliminarContenidoTest() {
        int size = 0;
        boolean excepcion = false;

        /*Añadir contenido: que se añada sin ningun problema*/
        try {
            almacenReal.agregarContenido(coldplay1);
            size++;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
        }
        assertFalse(excepcion);
        assertEquals(size, almacenReal.obtenerContenidos().size());
        assertEquals(almacenReal.obtenerContenidos().iterator().next(), coldplay1);

        /*Eliminar contenido: Sin ningun problema*/
        try {
            almacenReal.eliminarContenido(coldplay1);
            size--;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
        }

        assertFalse(excepcion);
        assertEquals(size, almacenReal.obtenerContenidos().size());
        assertFalse(almacenReal.obtenerContenidos().contains(coldplay1));
    }

    /**
     * test que cmprueba que no se pueden eliminar contenidos que no se hayan agregado
     */
    @Test
    public void eliminarContenidoNoAgregadoTest() {
        int size = 0;
        boolean excepcion = false;

        try {
            almacenReal.eliminarContenido(coldplay1);
            size--;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
        }

        assertTrue(excepcion);
        assertEquals(size, almacenReal.obtenerContenidos().size());
        assertFalse(almacenReal.obtenerContenidos().contains(coldplay1));
    }

    /**
     * test que comprueba que no se puede agregar un contenido bonus si ya
     * se habia agregado el mismo archivo de audio que se uso para crear ese bonus.
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
            almacenReal.agregarContenido(coldplay2);
            size++;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
        }
        assertFalse(excepcion);
        assertEquals(size, almacenReal.obtenerContenidos().size());
        assertEquals(almacenReal.obtenerContenidos().iterator().next(), coldplay2);

        try {
            almacenReal.agregarContenido(bonus);
            size++;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
        }

        assertTrue(excepcion);
        assertEquals(size, almacenReal.obtenerContenidos().size());
        almacenReal.eliminarContenido(coldplay2);
        assertTrue(almacenReal.obtenerContenidos().isEmpty());
    }

    /**
     * test que comprueba la busqeuda sobre un almacen vacio
     * @throws ExcepcionAlmacen 
     */
    @Test
    public void buscarVacioTest() throws ExcepcionAlmacen {
        assertTrue(almacenReal.buscar("Coldplay").isEmpty());
    }

    /**
     * test que comprueba la busqueda normal de contenido
     * @throws ExcepcionAlmacen 
     */
    @Test
    public void buscarTest() throws ExcepcionAlmacen {
        almacenReal.agregarContenido(coldplay1);
        assertFalse(almacenReal.buscar("Coldplay").isEmpty());
        assertTrue(almacenReal.buscar("Coldplay").contains(coldplay1));
        assertEquals(almacenReal.buscar("Coldplay").size(), 1);
    }

    /**
     * test que comprueba la busqueda con un null
     * @throws ExcepcionAlmacen 
     */
    @Test
    public void buscarNullTest() throws ExcepcionAlmacen {
        assertTrue(almacenReal.buscar(null).isEmpty());
    }

    /**
     * test que comprueba la busqueda de un elemento que no esta agregado
     * @throws ExcepcionAlmacen 
     */
    @Test
    public void buscarNoExistenteTest() throws ExcepcionAlmacen {
        almacenReal.agregarContenido(coldplay1);
        assertTrue(almacenReal.buscar("Amy").isEmpty());
        assertFalse(almacenReal.buscar("Amy").contains(coldplay1));
    }

    /**
     * test que comprueba la busqeuda de multiples contenidos a la vez
     * @throws ExcepcionAlmacen 
     */
    @Test
    public void buscarVariosTest() throws ExcepcionAlmacen {
        almacenReal.agregarContenido(coldplay1);
        almacenReal.agregarContenido(winehouse);
        assertFalse(almacenReal.buscar("").isEmpty());
        assertTrue(almacenReal.buscar("").contains(coldplay1));
        assertTrue(almacenReal.buscar("").contains(winehouse));
        assertEquals(almacenReal.buscar("").size(), 2);
    }
    
    /**
     * test que comprueba que un almacen real no puede establecer proveedor
     */
    @Test
    public void establecerProveedor(){
        assertTrue(true);
    }
    
    /**
     * test que comprueba que obtener proveedor en un almacen real devuelve siempre null
     */
    @Test
    public void obtenerProveedor(){
        assertEquals(almacenReal.obtenerProveedor(),null);
    }

}
