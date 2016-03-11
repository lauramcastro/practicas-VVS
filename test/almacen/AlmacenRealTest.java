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
import static junit.framework.Assert.assertTrue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import org.junit.Before;
import org.junit.Test;

/**
 *
 * @author hmia
 */
public class AlmacenRealTest {

    Almacen emi;
    String nombre = "EMI";

    Contenido coldplay1;
    Contenido winehouse;
    Contenido coldPlayWineHouse;

    @Before
    public void setUp() {
        emi = new AlmacenReal(nombre);
        coldplay1 = new ArchivoAudio("Coldplay: Speed of Sound", "http://servidor/coldplay/xy/7", 288, "Rock alternativo");
        winehouse = new ArchivoAudio("Amy Winehouse: Rehab", "http://servidor/winehouse/back2black/1", 215, "Soul");
        coldPlayWineHouse = new ArchivoAudio("Coldplay: Rehab Amy", "http://servidor/alavigne/bestdamnthing/1", 216, "Punk pop");

    }

    /**
     * En Obtener nombre comprobamos que el nombre devuelto es igual al esperado
     * en los casos (con nombre, o nombre nulo)
     */
    @Test
    public void obternerNombreTest() {

        assertEquals(nombre, emi.obtenerNombre());
        emi = new AlmacenReal(null);
        assertEquals(null, emi.obtenerNombre());
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
            emi.agregarContenido(coldplay1);
            size++;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
            size--;

        }
        assertFalse(excepcion);
        assertEquals(size, emi.obtenerContenidos().size());
        excepcion = false;

        /*Añadir contenido: Contenido repetido*/
        try {
            emi.agregarContenido(coldplay1);
            size++;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
            size--;
        }

        assertTrue(excepcion);
        assertEquals(size, emi.obtenerContenidos().size());
        excepcion = false;

        /*Eliminar contenido: Sin ningun problema*/
        try {
            emi.eliminarContenido(coldplay1);
            size--;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
            size++;
        }

        assertFalse(excepcion);
        assertEquals(size, emi.obtenerContenidos().size());
        excepcion = false;

        /*Eliminar contenido: contenido que no existe*/
        try {

            emi.eliminarContenido(coldplay1);
            size--;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
            size++;
        }

        assertTrue(excepcion);
        assertEquals(size, emi.obtenerContenidos().size());
        excepcion = false;

        /*Volvemos a añadir la cancion que antes nos daba error pero que hemos eliminado ya. */
        try {

            emi.agregarContenido(coldplay1);
            size++;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
            size--;
        }
        assertFalse(excepcion);
        assertEquals(size, emi.obtenerContenidos().size());
        excepcion = false;

        Contenido wineandcold = new Bonus((ArchivoAudio) winehouse, coldplay1);
        try {
            emi.agregarContenido(wineandcold);
            size++;
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
            size--;
        }

        assertTrue(excepcion);
        assertEquals(size, emi.obtenerContenidos().size());

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

            assertTrue(emi.obtenerContenidos().isEmpty());
            emi.agregarContenido(coldplay1);
            contenidoAnadido.add(coldplay1);
            size++;
            assertEquals(size, emi.obtenerContenidos().size());
            assertTrue(emi.obtenerContenidos().contains(coldplay1));
            emi.eliminarContenido(coldplay1);
            contenidoAnadido.remove(coldplay1);
            size--;
            emi.agregarContenido(winehouse);
            contenidoAnadido.add(winehouse);
            size++;
            emi.agregarContenido(coldPlayWineHouse);
            contenidoAnadido.add(coldPlayWineHouse);
            size++;
            assertEquals(size, emi.obtenerContenidos().size());
            contenidoEncontrado = emi.obtenerContenidos();
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
        assertTrue(emi.buscar("").isEmpty());
        emi.agregarContenido(coldplay1);
        size++;
        assertFalse(emi.buscar("").isEmpty());
        assertTrue(emi.buscar("").contains(coldplay1));
        assertTrue(emi.buscar("").contains(coldplay1));

        emi.agregarContenido(winehouse);
        size++;
        assertEquals(size, emi.buscar("").size());
        assertTrue(emi.buscar("").contains(winehouse));
        assertTrue(emi.buscar("").contains(coldplay1));

        int valorEsperado = 1; /*winehouse */

        assertEquals(valorEsperado, emi.buscar(winehouse.obtenerTitulo()).size());
        assertFalse(emi.buscar(winehouse.obtenerTitulo()).contains(coldplay1));
        assertTrue(emi.buscar(winehouse.obtenerTitulo()).contains(winehouse));


        /*Buscamos contenido con las palabra clave reh */
        valorEsperado = 1; /*winehouse */

        assertEquals(valorEsperado, emi.buscar("reh").size());
        assertTrue(emi.buscar("reh").contains(winehouse));

        emi.agregarContenido(coldPlayWineHouse);
        size++;

        valorEsperado = 2; /*coldPlayWineHouse y winehouse */
        
        assertEquals(valorEsperado, emi.buscar("Rehab").size());
        assertTrue(emi.buscar("Rehab Amy").contains(coldPlayWineHouse));
        assertTrue(emi.buscar("Rehab Amy").contains(winehouse));
    }

}
