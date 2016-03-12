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
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;

/**
 *
 * @author hmia
 */
public class RegistroAlmacenTest {

    Almacen almacenReal;
    Almacen almacenRestringido;

    /* */
    Contenido coldplay2;
    Contenido winehouse1;
    Contenido winehouse2;
    Contenido otra;

    int busquedas = 3;
    int minutos = 1;
    String nombreReal = "almacen real";
    String nombreRestringido = "almacen restringido";
    Collection<Contenido> contenidoAnadidoReal;
    Collection<Contenido> contenidoAnadidoRestringido;

    @Before
    public void setUp() throws ExcepcionAlmacen {

        contenidoAnadidoReal = new ArrayList<Contenido>();
        contenidoAnadidoRestringido = new ArrayList<Contenido>();

        almacenReal = new AlmacenReal(nombreReal);
        almacenRestringido = new AlmacenRestringido(new AlmacenReal(nombreRestringido), busquedas, minutos);

        coldplay2 = new ArchivoAudio("Coldplay: Speed of Sound part2", "http://servidor/coldplay/xy/7", 288, "Rock alternativo");
        winehouse1 = new ArchivoAudio("Amy Winehouse: Rehab part1", "http://servidor/winehouse/back2black/1", 215, "Soul");
        winehouse2 = new ArchivoAudio("Amy Winehouse: Rehab part2", "http://servidor/alavigne/bestdamnthing/1", 216, "Punk pop");
        otra = new ArchivoAudio("otra: cancion", "http://servidor/otra/xy/7", 288, "Rock alternativo");

        /*Añadimos winehouse1 y  winehouse2 */
        almacenRestringido.agregarContenido(winehouse1);
        almacenRestringido.agregarContenido(winehouse2);
        contenidoAnadidoRestringido.add(winehouse1);
        contenidoAnadidoRestringido.add(winehouse2);

        /*Añadimos coldplay2 */
        almacenReal.agregarContenido(coldplay2);
        contenidoAnadidoReal.add(coldplay2);
    }

    @Test
    public void obternerNombreTest() {
        Almacen emi = new RegistroAlmacen(almacenReal);
        assertEquals(nombreReal, emi.obtenerNombre());
        emi = new RegistroAlmacen(almacenRestringido);
        assertEquals(nombreRestringido, emi.obtenerNombre());
    }

    @Test
    public void AnadirObternerEliminarContenidosTest() {
        boolean excepcion = false;

        Almacen emi = new RegistroAlmacen(almacenReal);
        assertEquals(emi.obtenerContenidos(), contenidoAnadidoReal);

        try {
            emi.agregarContenido(otra);
            contenidoAnadidoReal.add(otra);
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
        }

        assertFalse(excepcion);
        assertEquals(emi.obtenerContenidos(), contenidoAnadidoReal);
        assertEquals(emi.obtenerContenidos(), contenidoAnadidoReal);
        excepcion = false;

        /* Intentamos duplicar contenido*/
        try {
            emi.agregarContenido(otra);
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
        }
        assertTrue(excepcion);
    }

    @Test
    public void buscarTest() {
        boolean excepcion = false;

        try {
            assertTrue(almacenRestringido.buscar("").contains(winehouse1));
            //almacenRestringido.buscar("");
        } catch (ExcepcionAlmacen ex) {
            excepcion = true;
        }
        assertFalse(excepcion);
    }

}
