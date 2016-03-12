/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package almacen;

import contenido.ArchivoAudio;
import contenido.Contenido;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;

/**
 *
 * @author hmia
 */
public class ProveedorAlmacenTest {

    Almacen almacenReal;
    Almacen almacenRestringido;

    /* */
    Contenido coldplay2;
    Contenido winehouse1;
    Contenido winehouse2;
    Contenido otra;

    int busquedas = 3;
    int minutos = 1;

    @Before
    public void setUp() throws ExcepcionAlmacen {

        almacenReal = new AlmacenReal("xxx-padre");
        almacenRestringido = new AlmacenRestringido(new AlmacenReal("xxx-padre"), busquedas, minutos);

        coldplay2 = new ArchivoAudio("Coldplay: Speed of Sound part2", "http://servidor/coldplay/xy/7", 288, "Rock alternativo");
        winehouse1 = new ArchivoAudio("Amy Winehouse: Rehab part1", "http://servidor/winehouse/back2black/1", 215, "Soul");
        winehouse2 = new ArchivoAudio("Amy Winehouse: Rehab part2", "http://servidor/alavigne/bestdamnthing/1", 216, "Punk pop");
        otra = new ArchivoAudio("otra: cancion", "http://servidor/otra/xy/7", 288, "Rock alternativo");

        /*Añadimos  winehouse1 y  winehouse2 */
        almacenRestringido.agregarContenido(winehouse1);
        almacenRestringido.agregarContenido(winehouse2);

        /*Añadimos coldplay2 */
        almacenReal.agregarContenido(coldplay2);
    }

    /**
     * Establecemos el proveedor y para comprobar su funcionamiento obtenemos el
     * proveedor.
     */
    @Test
    public void EstablecerYObtenerProveedorTest() {
        /* Establecemos el por defecto y lo comprobamos obteniendo proveedor*/
        Almacen proveedor = new ProveedorAlmacen(almacenReal, almacenRestringido);
        assertEquals(almacenRestringido, proveedor.obtenerProveedor());
        assertNotSame(almacenReal, proveedor.obtenerProveedor());

        /*Creamos un almacen nuevo y establecemos al proveedor anterior el nuevo proveedor*/
        Almacen nuevoReal = new AlmacenReal("nuevo");
        proveedor.establecerProveedor(nuevoReal);
        assertEquals(nuevoReal, proveedor.obtenerProveedor());
        assertFalse(almacenRestringido.equals(proveedor.obtenerProveedor()));
        assertTrue(nuevoReal.equals(proveedor.obtenerProveedor()));
        assertNotSame(almacenReal, proveedor.obtenerProveedor());

    }

    /**
     * Probar obtener proveedor con almacenes que no estan decorados como
     * proveedores
     */
    @Test
    public void probarProveedorConUnAlmacenSinDecorar() {
        almacenReal.establecerProveedor(almacenRestringido);
        assertNull(almacenReal.obtenerProveedor());
    }

    /**
     * Buscamos test cuando se establece
     */
    @Test
    public void BuscarTest() {
        boolean excepcion = false;
        Almacen proveedor = new ProveedorAlmacen(almacenReal, almacenRestringido);
        try {
            /* Son los contenidos que almacen real no tiene*/
            assertFalse(proveedor.obtenerContenidos().contains(winehouse1)); //busca en su almacen
            assertFalse(proveedor.obtenerContenidos().contains(winehouse1)); //busca en su almacen
            assertFalse(proveedor.buscar("Speed").contains(otra)); //busca en su almacen

            /*Buscamos un contenido que el almacen tiene por lo tanto no debe de buscar en su proveedor*/
            assertTrue(proveedor.buscar("Speed").contains(coldplay2)); //almacen
            assertEquals(1, proveedor.buscar("Speed").size()); //busca en su almacen
            almacenRestringido.agregarContenido(otra);//busca en su almacen
            assertEquals(1, proveedor.buscar("Speed").size()); //coldplay2

            /* Ahora vamos a buscar un contenido que no dispone pero si su proveedor restringido 
             3 veces, mas no buscamos porque no queremos que nos mande la excepcion*/
            assertTrue(proveedor.buscar("amy").contains(winehouse1)); //busca en su proveedor 1
            assertTrue(proveedor.buscar("amy").contains(winehouse2)); //busca en su proveedor 2
            assertEquals(2, proveedor.buscar("amy").size()); //winehouse1 y winehouse2 //busca en su proveedor 3

            /* Cambiamos de proveedor y verificamos que ahora no encuentra AMY y encuentra otra*/
            Almacen nuevo = new AlmacenReal("nuevo");
            nuevo.agregarContenido(otra);
            proveedor.establecerProveedor(nuevo);
            assertTrue(proveedor.buscar("Speed").contains(coldplay2));  //busca en almacen
            assertFalse(proveedor.buscar("amy").contains(winehouse1)); //busca en proveedor
            assertFalse(proveedor.buscar("Speed").contains(otra)); //busca en almacen
            assertFalse(proveedor.buscar("amy").contains(winehouse1)); //busca en almacen y proveedor
            assertFalse(proveedor.buscar("amy").contains(winehouse2)); //busca en almacen y proveedor
            assertTrue(proveedor.buscar("cancion").toString().contains("cancion")); //busca en almacen y en proveedor

        } catch (ExcepcionAlmacen ex) {
            System.out.println("EXCEPCION: " + ex.getMessage());
            excepcion = true;
        }
        assertFalse(excepcion);
    }

    /**
     * Cuando se realizan las busquedas en el almacen restringido y las
     * encuentra va restando de modo que cuando nos terminen el numero de
     * busquedas disponibles nos lanza la excepción.
     *
     * @throws ExcepcionAlmacen
     */
    @Test(expected = ExcepcionAlmacen.class)
    public void buscarRestringidoExcepcionTest() throws ExcepcionAlmacen {
        Almacen proveedor = new ProveedorAlmacen(almacenRestringido, almacenReal);
        /*Verificamos que el almacen restringido tiene el mismo contenido que proveedor */
        assertTrue(proveedor.buscar("Amy").contains(winehouse1)); //busca en su almacen y lo encuentra 1
        assertTrue(proveedor.buscar("Amy").contains(winehouse2)); //busca en u almacen y lo encuentra 2
        assertFalse(proveedor.buscar("cancion").contains(otra)); //busca y no la encuentra
        assertFalse(proveedor.buscar("").contains(coldplay2)); //busca y no la encuentra
        assertTrue(proveedor.buscar("SPEED").contains(coldplay2)); //busca en su almacen y lo encuentra 3
    }
    
    /*Comprobamos que salta la excepcion de un almacen restringido cuando actua como almacen proveedor*/
    @Test(expected = ExcepcionAlmacen.class)
    public void BuscarExceptionRestringidoProveedorTest() throws ExcepcionAlmacen {
        
        Almacen proveedor = new ProveedorAlmacen(almacenReal, almacenRestringido);
                  
        /* Ahora vamos a buscar un contenido que no dispone, pero sí su proveedor restringido, 4 veces*/
        assertTrue(proveedor.buscar("amy").contains(winehouse1)); //busca en su proveedor 1
        assertTrue(proveedor.buscar("amy").contains(winehouse2)); //busca en su proveedor 2
        assertTrue(proveedor.buscar("amy").contains(winehouse1)); //busca en su proveedor 1
        assertTrue(proveedor.buscar("amy").contains(winehouse1)); //busca en su proveedor 1
     
    }
    
    
    /*Comprobamos que salta la excepcion de un almacen restringido cuando actua como almacen base*/
    @Test(expected = ExcepcionAlmacen.class)
    public void BuscarExceptionRestringidoBaseTest() throws ExcepcionAlmacen {
        
        Almacen proveedor = new ProveedorAlmacen(almacenRestringido, almacenReal);
                  
        /* Ahora vamos a buscar un contenido que si dispone, 4 veces*/
        assertTrue(proveedor.buscar("amy").contains(winehouse1)); //busca en su proveedor 1
        assertTrue(proveedor.buscar("amy").contains(winehouse2)); //busca en su proveedor 2
        assertTrue(proveedor.buscar("amy").contains(winehouse1)); //busca en su proveedor 1
        assertTrue(proveedor.buscar("amy").contains(winehouse1)); //busca en su proveedor 1
     
    }

}
