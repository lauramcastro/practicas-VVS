/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package vvs.almacen;

import vvs.contenido.ArchivoAudio;
import vvs.contenido.Contenido;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

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

//        coldplay2 = new ArchivoAudio("Coldplay: Speed of Sound part2", "http://servidor/coldplay/xy/7", 288, "Rock alternativo");
//        winehouse1 = new ArchivoAudio("Amy Winehouse: Rehab part1", "http://servidor/winehouse/back2black/1", 215, "Soul");
//        winehouse2 = new ArchivoAudio("Amy Winehouse: Rehab part2", "http://servidor/alavigne/bestdamnthing/1", 216, "Punk pop");
//        otra = new ArchivoAudio("otra: cancion", "http://servidor/otra/xy/7", 288, "Rock alternativo");
        
        coldplay2 = Mockito.mock(ArchivoAudio.class);
        Mockito.when(coldplay2.obtenerTitulo()).thenReturn("Coldplay: Speed of Sound part2");
        Mockito.when(coldplay2.buscar(Mockito.anyString())).thenCallRealMethod();
      	winehouse1 = Mockito.mock(ArchivoAudio.class);
      	Mockito.when(winehouse1.obtenerTitulo()).thenReturn("Amy Winehouse: Rehab part1");
      	Mockito.when(winehouse1.buscar(Mockito.anyString())).thenCallRealMethod();
      	winehouse2 = Mockito.mock(ArchivoAudio.class);
      	Mockito.when(winehouse2.obtenerTitulo()).thenReturn("Amy Winehouse: Rehab part2");
      	Mockito.when(winehouse2.buscar(Mockito.anyString())).thenCallRealMethod();
      	otra = Mockito.mock(ArchivoAudio.class);
      	Mockito.when(otra.obtenerTitulo()).thenReturn("otra: cancion");
      	Mockito.when(otra.buscar(Mockito.anyString())).thenCallRealMethod();
        
        /*Anadimos  winehouse1 y  winehouse2 */
        almacenRestringido.agregarContenido(winehouse1);
        almacenRestringido.agregarContenido(winehouse2);

        /*Anadimos coldplay2 */
        almacenReal.agregarContenido(coldplay2);
    }

    /**
     * Test que comprueba el correcto funcionamiento de obtenerProveedor
     */
    @Test
    public void ObtenerProveedorTest() {
        Almacen proveedor = new ProveedorAlmacen(almacenReal, almacenRestringido);
        assertEquals(almacenRestringido, proveedor.obtenerProveedor());
    }
    
    /**
     * Test que comprueba el correcto funcionamiento de establecerProveedor
     */
    @Test
    public void establecerProveedorTest() {
        Almacen proveedor = new ProveedorAlmacen(almacenReal, almacenRestringido);
                
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
     * test que comprueba la busqueda de contenido que no esta en el almacenReal
     * @throws vvs.almacen.ExcepcionAlmacen
     */
    @Test
    public void buscarNoContenidoTest() throws ExcepcionAlmacen {
        
        boolean excepcion = false;
        Almacen proveedor = new ProveedorAlmacen(almacenReal, almacenRestringido);
        
        try {
            /* Son los contenidos que almacen real no tiene*/
            assertFalse(proveedor.obtenerContenidos().contains(winehouse1)); //busca en su almacen
            assertFalse(proveedor.obtenerContenidos().contains(winehouse1)); //busca en su almacen
            assertFalse(proveedor.buscar("Speed").contains(otra)); //busca en su almacen

        } catch (ExcepcionAlmacen ex) {
            System.out.println("EXCEPCION: " + ex.getMessage());
            excepcion = true;
        }
        assertFalse(excepcion);
    }
    
     /**
     * Test que comprueba el correcto funcionamiento de la busqueda de contenido
     * cuando si está en el almacenReal
     * @throws vvs.almacen.ExcepcionAlmacen
     */
    @Test
    public void buscarEnAlmacenRealTest() throws ExcepcionAlmacen {
        
        boolean excepcion = false;
                
        Almacen proveedor = new ProveedorAlmacen(almacenReal, almacenRestringido);
        
        try {
            /*Buscamos un contenido que el almacen tiene por lo tanto no debe de buscar en su proveedor*/
            assertTrue(proveedor.buscar("Speed").contains(coldplay2)); //almacen
            assertEquals(1, proveedor.buscar("Speed").size()); //busca en su almacen
            almacenRestringido.agregarContenido(otra);//busca en su almacen
            assertEquals(1, proveedor.buscar("Speed").size()); //coldplay2

        } catch (ExcepcionAlmacen ex) {
            System.out.println("EXCEPCION: " + ex.getMessage());
            excepcion = true;
        }
        assertFalse(excepcion);
    }
    
     /**
     * Test que comprueba el correcto funcionamiento de la busqueda cuando buscamos
     * contenido que esta en el proveedor
     * @throws vvs.almacen.ExcepcionAlmacen
     */
    @Test
    public void buscarEnProveedorTest() throws ExcepcionAlmacen {
        
        boolean excepcion = false;
                
        Almacen proveedor = new ProveedorAlmacen(almacenReal, almacenRestringido);
        
        try {
            
            /* Ahora vamos a buscar un contenido que no dispone pero si su proveedor restringido 
             3 veces, mas no buscamos porque no queremos que nos mande la excepcion*/
            assertTrue(proveedor.buscar("amy").contains(winehouse1)); //busca en su proveedor 1
            assertTrue(proveedor.buscar("amy").contains(winehouse2)); //busca en su proveedor 2
            assertEquals(2, proveedor.buscar("amy").size()); //winehouse1 y winehouse2 //busca en su proveedor 3

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
    public void buscarExceptionRestringidoProveedorTest() throws ExcepcionAlmacen {
        
        Almacen proveedor = new ProveedorAlmacen(almacenReal, almacenRestringido);
                  
        /* Ahora vamos a buscar un contenido que no dispone, pero sí su proveedor restringido, 4 veces*/
        assertTrue(proveedor.buscar("amy").contains(winehouse1)); //busca en su proveedor 1
        assertTrue(proveedor.buscar("amy").contains(winehouse2)); //busca en su proveedor 2
        assertTrue(proveedor.buscar("amy").contains(winehouse1)); //busca en su proveedor 1
        assertTrue(proveedor.buscar("amy").contains(winehouse1)); //busca en su proveedor 1
     
    }
    
    
    /*Comprobamos que salta la excepcion de un almacen restringido cuando actua como almacen base*/
    @Test(expected = ExcepcionAlmacen.class)
    public void buscarExceptionRestringidoBaseTest() throws ExcepcionAlmacen {
        
        Almacen proveedor = new ProveedorAlmacen(almacenRestringido, almacenReal);
                  
        /* Ahora vamos a buscar un contenido que si dispone, 4 veces*/
        assertTrue(proveedor.buscar("amy").contains(winehouse1)); //busca en su proveedor 1
        assertTrue(proveedor.buscar("amy").contains(winehouse2)); //busca en su proveedor 2
        assertTrue(proveedor.buscar("amy").contains(winehouse1)); //busca en su proveedor 1
        assertTrue(proveedor.buscar("amy").contains(winehouse1)); //busca en su proveedor 1
    }
    
    /*Comprobamos que la busqueda funciona cuando establecemos como proveedor 
    un alamcen null*/
    @Test
    public void buscarProveedorNullTest() throws ExcepcionAlmacen {
        
        Almacen proveedor = new ProveedorAlmacen(almacenRestringido, null);
        assertFalse(proveedor.buscar("coldplay").contains(coldplay2)); //busca en su proveedor 
        
        proveedor.establecerProveedor(null);                  
        assertFalse(proveedor.buscar("coldplay").contains(coldplay2)); //busca en su proveedor 
    }
}
