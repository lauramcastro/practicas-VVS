package vvs.almacen;

import vvs.contenido.ArchivoAudio;
import vvs.contenido.Contenido;
import vvs.contenido.ExcepcionContenido;

import java.util.ArrayList;
import java.util.Collection;
import java.util.concurrent.TimeUnit;
import net.java.quickcheck.characteristic.Classification;
import net.java.quickcheck.generator.PrimitiveGenerators;
import net.java.quickcheck.generator.iterable.Iterables;

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
 * Clase de prueba para <code>AlmacenRestringido</code>.
 * @author hmia
 */
public class AlmacenRestringidoTest {

    private Almacen real;
    private Almacen restringido;
    private int busquedas;
    private int minutos;
    private int size;
    private String nombre = "EMI";

    private Contenido coldplay1;
    private Contenido winehouse;
    private Contenido coldPlayWineHouse;
    private Collection<Contenido> contenidoAnadido;

    /**
     * Iniciamos los datos que necesitamos para realizar las pruebas
     *
     * @throws ExcepcionAlmacen Excepción que se puede lanzar si la inicialización produce errores.
     */
    @Before
    public void setUp() throws ExcepcionAlmacen {
        busquedas = 3;
        minutos = 1;
        size = 0;

        //contenidoAnadido = new ArrayList<Contenido>();
        real = new AlmacenReal(nombre);
        restringido = new AlmacenRestringido(real, busquedas, minutos);

        // ¿Por qué tenemos código muerto aquí? ¡El código de pruebas también hay que mantenerlo!
        
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
     * Comprobamos que el nombre devuelto es igual al esperado
     * en los casos: con nombre, o nombre nulo.
     */
    @Test
    public void obternerNombreTest() {
        assertEquals(nombre, restringido.obtenerNombre());
        assertEquals(nombre, real.obtenerNombre());
        Almacen nuevo = new AlmacenReal(null);
        assertNull(nuevo.obtenerNombre());
    }

    /**
     * Prueba sin documentar.
     *
     * @throws ExcepcionAlmacen Excepción que se captura.
     */
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
     * Test que comprueba que no se puede agregar como contenido un nulo a un
     * almacén restringido.
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
     * Test que comprueba que no se pueden agregar contenidos duplicados a un
     * almacén restringido.
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
     * Test que comprueba la eliminación de contenidos en almacén restringido.
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
     * Test que comprueba que no se pueden eliminar contenidos que no se hayan
     * agregado en un almacén restringido.
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
     * Test que comprueba que no se puede agregar un contenido bonus si ya se
     * había agregado el mismo archivo de audio que se usó para crear ese bonus,
     * en un almacén restringido.
     *
     * @throws ExcepcionAlmacen Excepción que se captura.
     * @throws ExcepcionContenido Excepción que se captura.
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
     * Test que comprueba la búsqueda sobre un almacén restringido vacío.
     *
     * @throws ExcepcionAlmacen Excepción que se puede lanzar.
     */
    @Test
    public void buscarVacioTest() throws ExcepcionAlmacen {
        assertTrue(restringido.buscar("Coldplay").isEmpty());
    }

    /**
     * Test que comprueba la búsqueda normal de contenido, en un almacén
     * restringido.
     *
     * @throws ExcepcionAlmacen Excepción que se puede lanzar.
     */
    @Test
    public void buscarTest() throws ExcepcionAlmacen {
        restringido.agregarContenido(coldplay1);
        assertFalse(restringido.buscar("ColdPLay Sound").isEmpty());
        assertTrue(restringido.buscar("ColdplAY Sound").contains(coldplay1));
        assertEquals(restringido.buscar("Coldplay SouND").size(), 1);
    }

    /**
     * Test que comprueba la búsqueda con un nulo, en un almacén restringido.
     *
     * @throws ExcepcionAlmacen Excepción que se puede lanzar.
     */
    @Test
    public void buscarNullTest() throws ExcepcionAlmacen {
        assertTrue(restringido.buscar(null).isEmpty());
    }

    /**
     * Test que comprueba la búsqueda de un elemento que no está agregado en un
     * almacén restringido.
     *
     * @throws ExcepcionAlmacen Excepción que se puede lanzar.
     */
    @Test
    public void buscarNoExistenteTest() throws ExcepcionAlmacen {
        restringido.agregarContenido(coldplay1);
        assertTrue(restringido.buscar("Amy").isEmpty());
        assertFalse(restringido.buscar("Amy").contains(coldplay1));
    }

    /**
     * Test que comprueba la búsqueda de múltiples contenidos a la vez, en un
     * almacén restringido.
     *
     * @throws ExcepcionAlmacen Excepción que se puede lanzar.
     */
    @Test
    public void buscarVariosTest() throws ExcepcionAlmacen {
        restringido.agregarContenido(coldplay1);
        restringido.agregarContenido(winehouse);

        assertTrue(restringido.buscar("").contains(coldplay1));
        assertTrue(restringido.buscar("").contains(winehouse));
        assertEquals(restringido.buscar("").size(), 2);
    }
    
    /** Prueba sin documentación.
     *
     * @throws ExcepcionAlmacen Excepción que se puede lanzar.
     */
    @Test
    public void busquedasAleatorias() throws ExcepcionAlmacen {
        Classification c = new Classification();
        int duracion = 1, busquedas = 1000, contBusquedas = 0;
        Almacen almacen = new AlmacenRestringido(
                new AlmacenReal("almacenRestringido"), busquedas, duracion);

        for (Contenido contenido : Iterables.toIterable(new GeneradorContenido())) {
            almacen.agregarContenido(contenido);
            for (String cadenaABuscar : Iterables.toIterable(PrimitiveGenerators.strings())) {
                if (contBusquedas < busquedas) {
                    if (almacen.buscar(cadenaABuscar).contains(cadenaABuscar)) {
                        c.classifyCall("presente");
                    } else {
                        c.classifyCall("ausente"); /* Ojo, en ejecución sale un 100% de casos ausentes... ¡no es una prueba muy efectiva! */
                    }
                    contBusquedas++;
                    assertFalse(almacen.buscar(cadenaABuscar).contains(cadenaABuscar)
                            && almacen.buscar(cadenaABuscar).isEmpty());
                }                
            }
        }
        for (Object cat : c.getCategories()) {
            System.out.println("[busquedasAleatorias] ===> " + cat + " => " + c.getFrequency(cat));
        }
    }

    /**
     * Test que comprueba la búsqueda de un elemento con espacios antes y
     * después.
     *
     * @throws ExcepcionAlmacen Excepción que se puede lanzar.
     */
    @Test
    public void buscarConEspaciosTest() throws ExcepcionAlmacen {
        restringido.agregarContenido(coldplay1);
        assertFalse(restringido.buscar(" Sound ").isEmpty());
        assertTrue(restringido.buscar(" Sound ").contains(coldplay1));
        assertEquals(restringido.buscar(" Sound ").size(), 1);
    }

    /**
     * Test que comprueba la búsqueda de un elemento con palabras desordenadas.
     *
     * @throws ExcepcionAlmacen Excepción que se puede lanzar.
     */
    @Test
    public void buscarPalabrasDesordenadasTest() throws ExcepcionAlmacen {
        restringido.agregarContenido(coldplay1);
        assertFalse(restringido.buscar(" Sound Speed ").isEmpty());
        assertTrue(restringido.buscar(" Sound Speed ").contains(coldplay1));
        assertEquals(restringido.buscar(" Sound Speed ").size(), 1);
    }

    /**
     * Test que comprueba que si se sobrepasa el número de búsquedas permitidas
     * en un almacén restringido, salta la excepción.
     *
     * @throws ExcepcionAlmacen Excepción que se espera lanzar.
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
     * Test que comprueba que si se espera el tiempo necesario antes de
     * sobrepasar el número de búsquedas permitidas en un almacén restringido,
     * no salta la excepción.
     *
     * @throws ExcepcionAlmacen Excepción que se puede lanzar si falla la prueba.
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
     * Test que comprueba que un almacén restringido no se modifica al
     * establecer proveedor.
     *
     * @throws ExcepcionAlmacen Excepción que se puede lanzar.
     */
    @Test
    public void establecerProveedor() throws ExcepcionAlmacen {

        Almacen proveedor1 = restringido.obtenerProveedor();
        restringido.establecerProveedor(restringido);
        Almacen proveedor2 = restringido.obtenerProveedor();
        assertEquals(proveedor1, proveedor2);
    }

    /**
     * Test que comprueba que obtener proveedor en un almacén restringido
     * devuelve siempre nulo (redundante respecto al test anterior: ¿por qué se mantiene, entonces?)
     */
    @Test
    public void obtenerProveedor() {
        assertEquals(restringido.obtenerProveedor(), null);
    }

}
