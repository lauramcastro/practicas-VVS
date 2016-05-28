package vvs.almacen;

import java.util.Iterator;
import net.java.quickcheck.characteristic.Classification;
import net.java.quickcheck.generator.PrimitiveGenerators;
import net.java.quickcheck.generator.iterable.Iterables;

import vvs.contenido.ArchivoAudio;
import vvs.contenido.Bonus;
import vvs.contenido.Contenido;
import vvs.contenido.ExcepcionContenido;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

/**
 * Clase de pruebas para <code>AlmacenReal</code>.
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
     * Comprobamos que el nombre devuelto es igual al esperado
     * en los casos: con nombre, o nombre nulo, aunque deberían ser dos pruebas diferentes.
     */
    @Test
    public void obternerNombreTest() {
        assertEquals(nombre, almacenReal.obtenerNombre());
        almacenReal = new AlmacenReal(null);
        assertEquals(null, almacenReal.obtenerNombre());
    }

    /**
     * Test positivo que agrega un contenido y comprueba que se obtiene
     * correctamente.
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
     * Test que comprueba que no se puede agregar como contenido un null.
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
        assertTrue(excepcion);
        assertEquals(size, almacenReal.obtenerContenidos().size());
    }

    /**
     * Test que comprueba que no se pueden agregar contenidos duplicados.
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
     * Test que comprueba la eliminación de contenidos.
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
     * Test que comprueba que no se pueden eliminar contenidos que no se hayan
     * agregado.
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
     * Test que comprueba que no se puede agregar un contenido bonus si ya se
     * había agregado el mismo archivo de audio que se usó para crear ese bonus.
     *
     * @throws ExcepcionAlmacen Excepción que se captura.
     * @throws ExcepcionContenido Excepción que podría lanzarse si la prueba falla.
     */
    @Test
    public void agregarContenidoDuplicadoContenidoEnOtroContenidoTest() throws ExcepcionAlmacen, ExcepcionContenido {
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
     * Test que comprueba la búsqueda sobre un almacén vacío.
     *
     * @throws ExcepcionAlmacen Excepción que podría lanzarse si la prueba falla.
     */
    @Test
    public void buscarVacioTest() throws ExcepcionAlmacen {
        assertTrue(almacenReal.buscar("Coldplay").isEmpty());
    }

    /**
     * Test que comprueba la búsqueda normal de contenido.
     *
     * @throws ExcepcionAlmacen Excepción que podría lanzarse si la prueba falla.
     */
    @Test
    public void buscarTest() throws ExcepcionAlmacen {
        almacenReal.agregarContenido(coldplay1);
        assertFalse(almacenReal.buscar("ColdPLay Sound").isEmpty());
        assertTrue(almacenReal.buscar("ColdplAY Sound").contains(coldplay1));
        assertEquals(almacenReal.buscar("Coldplay SouND").size(), 1);
    }

    /**
     * Test que comprueba la búsqueda con un null.
     *
     * @throws ExcepcionAlmacen Excepción que podría lanzarse si la prueba falla.
     */
    @Test
    public void buscarNullTest() throws ExcepcionAlmacen {
        assertTrue(almacenReal.buscar(null).isEmpty());
    }

    /**
     * Test que comprueba la búsqueda de un elemento que no está agregado.
     *
     * @throws ExcepcionAlmacen Excepción que podría lanzarse si la prueba falla.
     */
    @Test
    public void buscarNoExistenteTest() throws ExcepcionAlmacen {
        almacenReal.agregarContenido(coldplay1);
        assertTrue(almacenReal.buscar("Amy").isEmpty());
        assertFalse(almacenReal.buscar("Amy").contains(coldplay1));
    }

    /**
     * Test que comprueba la búsqueda de múltiples contenidos a la vez.
     *
     * @throws ExcepcionAlmacen Excepción que podría lanzarse si la prueba falla.
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
     * Test que comprueba la búsqueda de un elemento con espacios antes y
     * después.
     *
     * @throws ExcepcionAlmacen Excepción que podría lanzarse si la prueba falla.
     */
    @Test
    public void buscarConEspaciosTest() throws ExcepcionAlmacen {
        almacenReal.agregarContenido(coldplay1);
        assertFalse(almacenReal.buscar(" Sound ").isEmpty());
        assertTrue(almacenReal.buscar(" Sound ").contains(coldplay1));
        assertEquals(almacenReal.buscar(" Sound ").size(), 1);
    }

    /**
     * Test que comprueba la búsqueda de un elemento con palabras desordenadas.
     *
     * @throws ExcepcionAlmacen Excepción que se puede lanzar.
     */
    @Test
    public void buscarPalabrasDesordenadasTest() throws ExcepcionAlmacen {
        almacenReal.agregarContenido(coldplay1);
        assertFalse(almacenReal.buscar(" Sound Speed ").isEmpty());
        assertTrue(almacenReal.buscar(" Sound Speed ").contains(coldplay1));
        assertEquals(almacenReal.buscar(" Sound Speed ").size(), 1);
    }

    /**
     * Test que comprueba que un almacén real no se modifica al establecer
     * proveedor.
     *
     * @throws ExcepcionAlmacen Excepción que se puede lanzar.
     */
    @Test
    public void establecerProveedor() throws ExcepcionAlmacen {

        Almacen proveedor1 = almacenReal.obtenerProveedor();
        almacenReal.establecerProveedor(almacenReal);
        Almacen proveedor2 = almacenReal.obtenerProveedor();
        assertEquals(proveedor1, proveedor2);
    }

    /**
     * Test que comprueba que obtener proveedor en un almacén real devuelve
     * siempre null (redundante respecto al test anterior: ¿por qué no se borra? ¡las pruebas son código a mantener!).
     */
    @Test
    public void obtenerProveedor() {
        assertEquals(almacenReal.obtenerProveedor(), null);
    }

    /**
     * Prueba sin documentación.
     *
     * @throws ExcepcionAlmacen Excepción que se puede lanzar.
     */
    @Test
    public void busquedasAleatorias() throws ExcepcionAlmacen {
        Classification c = new Classification();
        Almacen almacen = new AlmacenReal("almacenReal");
        Iterable<Contenido> gC = Iterables.toIterable(new GeneradorContenido());
        Iterable<String> cB = Iterables.toIterable(PrimitiveGenerators.strings());
        
       
        for (int i = 0; i<10; i++) {            
            Contenido contenido = (Contenido) gC.iterator().next();
            almacen.agregarContenido(contenido);
            for (int j = 0; j<10; j++) {
                String cadenaABuscar = (String) cB.iterator().next();
                System.out.println("[busquedasAleatorias1] ===> " + cadenaABuscar + " ? " + contenido);
                if (almacen.buscar(cadenaABuscar).contains(cadenaABuscar)) {
                    c.classifyCall("presente");
                } else {
                    c.classifyCall("ausente"); /* Ojo, en ejecución sale un 100% de casos ausentes... ¡no es una prueba muy efectiva! */
                }
                assertFalse(almacen.buscar(cadenaABuscar).contains(cadenaABuscar)
                        && almacen.buscar(cadenaABuscar).isEmpty());
            }
        }

        System.out.println("[busquedasAleatorias2] ===> " + c);
        for (Object cat : c.getCategories()) {
            System.out.println("[busquedasAleatorias3] ===> " + cat + " => " + c.getFrequency(cat));
        }
    }
}
