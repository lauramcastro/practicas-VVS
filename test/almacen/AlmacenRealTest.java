/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package almacen;

import contenido.ArchivoAudio;
import contenido.Bonus;
import contenido.Contenido;

import java.util.ArrayList;
import java.util.Collection;

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
        Mockito.when(coldplay1.buscar("Coldplay")).thenCallRealMethod();
        
        winehouse = Mockito.mock(ArchivoAudio.class);
        Mockito.when(winehouse.obtenerTitulo()).thenReturn("Amy Winehouse: Rehab");
        Mockito.when(winehouse.buscar("Amy")).thenCallRealMethod();
        
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
     * Comprobamos que se añaden y se eliminen correctamente el contenido, e
     * incluso verificamos que la excepcion se ejecute cuando tiene que hacerlo
     * (ya sea cuando al añadir tenemos contenido repetido o cuando eliminamos
     * no exista el contenido que queremos eliminar)
     */
    public void agregarYeliminarContenido() {
        int size = 0;
        boolean excepcion = false;

        /*Añadir contenido: que se añada sin ningun problema*/
        try {
            almacenReal.agregarContenido(coldplay1);
            size++;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
            size--;

        }
        assertFalse(excepcion);
        assertEquals(size, almacenReal.obtenerContenidos().size());
        excepcion = false;

        /*Añadir contenido: Contenido repetido*/
        try {
            almacenReal.agregarContenido(coldplay1);
            size++;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
            size--;
        }

        assertTrue(excepcion);
        assertEquals(size, almacenReal.obtenerContenidos().size());
        excepcion = false;

        /*Eliminar contenido: Sin ningun problema*/
        try {
            almacenReal.eliminarContenido(coldplay1);
            size--;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
            size++;
        }

        assertFalse(excepcion);
        assertEquals(size, almacenReal.obtenerContenidos().size());
        excepcion = false;

        /*Eliminar contenido: contenido que no existe*/
        try {

            almacenReal.eliminarContenido(coldplay1);
            size--;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
            size++;
        }

        assertTrue(excepcion);
        assertEquals(size, almacenReal.obtenerContenidos().size());
        excepcion = false;

        /*Volvemos a añadir la cancion que antes nos daba error pero que hemos eliminado ya. */
        try {

            almacenReal.agregarContenido(coldplay1);
            size++;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
            size--;
        }
        assertFalse(excepcion);
        assertEquals(size, almacenReal.obtenerContenidos().size());
        excepcion = false;

        Contenido wineandcold = new Bonus((ArchivoAudio) winehouse, coldplay1);
        try {
            almacenReal.agregarContenido(wineandcold);
            size++;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
            size--;
        }

        assertTrue(excepcion);
        assertEquals(size, almacenReal.obtenerContenidos().size());

    }

    /**
     * Comprobamos al añadir contenido cuando llamamamos a btenerContenidos nos
     * devuelva esos contenidos esperados
     */
    @Test
    public void obternerContenidosTest() {
        boolean exception = false;
        int size = 0;
        Collection<Contenido> contenidoEncontrado = new ArrayList<Contenido>();
        Collection<Contenido> contenidoAnadido = new ArrayList<Contenido>();

        try {

            assertTrue(almacenReal.obtenerContenidos().isEmpty());
            almacenReal.agregarContenido(coldplay1);
            contenidoAnadido.add(coldplay1);
            size++;
            assertEquals(size, almacenReal.obtenerContenidos().size());
            assertTrue(almacenReal.obtenerContenidos().contains(coldplay1));
            almacenReal.eliminarContenido(coldplay1);
            contenidoAnadido.remove(coldplay1);
            size--;
            almacenReal.agregarContenido(winehouse);
            contenidoAnadido.add(winehouse);
            size++;
            almacenReal.agregarContenido(coldPlayWineHouse);
            contenidoAnadido.add(coldPlayWineHouse);
            size++;
            assertEquals(size, almacenReal.obtenerContenidos().size());
            contenidoEncontrado = almacenReal.obtenerContenidos();
            assertEquals(contenidoAnadido, contenidoEncontrado);

        } catch (ExcepcionAlmacen ex) {
            exception = true;
        }
        assertFalse(exception);
    }

    /**
     *
     * @throws ExcepcionAlmacen Buscamos el titulo xx y esperamos que nos
     * devuelva el contenido esperado
     */
    @Test
    public void buscarTest() throws ExcepcionAlmacen {
        int size = 0;

        /* Buscar todos los contenidos*/
        Collection<Contenido> contenidos = almacenReal.buscar("Coldplay");
        assertTrue(contenidos.isEmpty());
        almacenReal.agregarContenido(coldplay1);
        size++;
        contenidos.addAll(almacenReal.buscar("Coldplay"));
        assertTrue(!contenidos.isEmpty());

        almacenReal.agregarContenido(winehouse);
        size++;
        contenidos.addAll(almacenReal.buscar("Amy"));
        assertEquals(size, contenidos.size());
    }

}
