package vvs.almacen;

import vvs.contenido.ArchivoAudio;
import vvs.contenido.Contenido;
import java.util.Date;
import java.util.ArrayList;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import static java.lang.System.in;
import java.util.Collection;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 *
 * @author hmia
 */
public class RegistroAlmacenTest {

    Almacen registroAlmacen;
    Almacen registroReal;
    Almacen registroRestringido;
    int busquedas, minutos;
    Contenido coldplay2;
    Contenido winehouse1;
    Contenido winehouse2;
    String nombreRegistroAlmacenReal = "Registro almacen real";
    String nombreResgistroAlmacenRestringido = "Registro almacen restringido";
    Collection<Contenido> contenidoAnadidoRegistroAlmacen;
    static final String match = "Amy";

    @Before
    public void setUp() throws ExcepcionAlmacen {
        busquedas = 3;
        minutos = 1;
        contenidoAnadidoRegistroAlmacen = new ArrayList<Contenido>();
        registroReal = new AlmacenReal(nombreRegistroAlmacenReal);
        registroRestringido = new AlmacenRestringido(new AlmacenReal(nombreResgistroAlmacenRestringido), busquedas, minutos);

        coldplay2 = Mockito.mock(ArchivoAudio.class);
        Mockito.when(coldplay2.obtenerTitulo()).thenReturn("Coldplay: Speed of Sound");
        Mockito.when(coldplay2.buscar(Mockito.anyString())).thenCallRealMethod();
        winehouse1 = Mockito.mock(ArchivoAudio.class);
        Mockito.when(winehouse1.obtenerTitulo()).thenReturn(match + " Winehouse: Rehab part1");
        Mockito.when(winehouse1.buscar(Mockito.anyString())).thenCallRealMethod();
        winehouse2 = Mockito.mock(ArchivoAudio.class);
        Mockito.when(winehouse2.obtenerTitulo()).thenReturn(match + " Winehouse: Rehab");
        Mockito.when(winehouse2.buscar(Mockito.anyString())).thenCallRealMethod();
        /*Añadimos winehouse1 y  winehouse2 a almacen real*/
        registroReal.agregarContenido(winehouse1);
        registroReal.agregarContenido(coldplay2);
        /*Añadirmos wineHouse2 a almacen restringido*/
        registroRestringido.agregarContenido(winehouse2);
    }

    /*
    *Verificar que se crear el archivo de log al crear el almacen restringido
    */
    @Test
    public void crearAlmacenRestringidoTest() {
        Date fechaInicion = new Date(System.currentTimeMillis());
        registroAlmacen = new RegistroAlmacen(registroReal);
        assertTrue(new File(sanitize(nombreRegistroAlmacenReal + "-" + fechaInicion) + ".log").exists());
    }

    /* 
    * Busqueda sin ninguun problema y buscamos en el archivo de log
    */
    @Test
    public void buscarRegistroAlmacenlTest() throws ExcepcionAlmacen {
        Date dataInit = new Date(System.currentTimeMillis());
        registroAlmacen = new RegistroAlmacen(registroReal);
        String nombreArchivo = sanitize(nombreRegistroAlmacenReal + "-" + dataInit) + ".log";
        String palabraABuscar = match.toUpperCase();
        int coincidencias = 1;
        Collection<Contenido> encontrado = registroAlmacen.buscar(palabraABuscar);
        assertTrue(encontrado.contains(winehouse1));
        assertEquals(coincidencias, encontrado.size());
        /*Comprobar el archivo de log */
        assertTrue(buscarEnArchivoDeLog(nombreArchivo, "Buscado " + palabraABuscar + " en almacen " + nombreRegistroAlmacenReal));
        assertEquals(encontrado.size(), coincidencias);
        assertTrue(buscarEnArchivoDeLog(nombreArchivo, "Encontradas "
                + coincidencias + " coincidencias"));
    }

    /* 
    * Busqueda con parametros null
    */
    @Test
    public void buscarRegistroAlmacenlNullTest() throws ExcepcionAlmacen {
        Date dataInit = new Date(System.currentTimeMillis());
        registroAlmacen = new RegistroAlmacen(registroReal);
        String nombreArchivo = sanitize(nombreRegistroAlmacenReal + "-" + dataInit) + ".log";
        String palabraABuscar = null;
        int coincidencias = 0;
        Collection<Contenido> encontrado = registroAlmacen.buscar(palabraABuscar);
        assertTrue(encontrado.isEmpty());
        assertEquals(coincidencias, encontrado.size());
        /*Comprobar el archivo de log */
        assertTrue(buscarEnArchivoDeLog(nombreArchivo, "Buscado " + palabraABuscar + " en almacen " + nombreRegistroAlmacenReal));
        assertTrue(buscarEnArchivoDeLog(nombreArchivo, "Encontradas "
                + coincidencias + " coincidencias"));
        assertEquals(encontrado.size(), coincidencias);
    }

    /* 
    * Buscar con cero coincidencias
    */
    @Test
    public void buscarRegistroAlmacenConNingunaCoincidenciaTest() throws ExcepcionAlmacen {
        Date dataInit = new Date(System.currentTimeMillis());
        registroAlmacen = new RegistroAlmacen(registroReal);
        String nombreArchivo = sanitize(nombreRegistroAlmacenReal + "-" + dataInit) + ".log";
        String palabraABuscar = "7ASDdas7349FDAFNcasçsadRsS";
        int coincidencias = 0;
        Collection<Contenido> encontrado = registroAlmacen.buscar(palabraABuscar);
        assertTrue(encontrado.isEmpty());
        assertEquals(coincidencias, encontrado.size());
        /*Comprobar el archivo de log */
        assertTrue(new File(nombreArchivo).exists());
        assertTrue(buscarEnArchivoDeLog(nombreArchivo,  "Buscado " + palabraABuscar 
                + " en almacen " + nombreRegistroAlmacenReal));
        assertTrue(buscarEnArchivoDeLog(nombreArchivo, "Encontradas "
                + coincidencias + " coincidencias"));
        assertEquals(encontrado.size(), coincidencias);
    }

    /* 
    *Busquedad con todas las coincidencias
    */
    @Test
    public void buscarTodoElContenidoRegistroTest() throws ExcepcionAlmacen {
        Date dataInit = new Date(System.currentTimeMillis());
        registroAlmacen = new RegistroAlmacen(registroReal);
        String nombreArchivo = sanitize(nombreRegistroAlmacenReal + "-" + dataInit) + ".log";
        String palabraABuscar = "";
        int coincidencias = 2;
        Collection<Contenido> encontrado = registroAlmacen.buscar(palabraABuscar);
        assertEquals(coincidencias, encontrado.size());
        /*Comprobar el archivo de log */
        assertTrue(new File(nombreArchivo).exists());
        assertTrue(buscarEnArchivoDeLog(nombreArchivo,  "Buscado " + palabraABuscar                 
                +" en almacen " + nombreRegistroAlmacenReal));
        assertTrue(buscarEnArchivoDeLog(nombreArchivo, "Encontradas "
                + coincidencias + " coincidencias"));
        assertEquals(encontrado.size(), coincidencias);
    }

    /*Busqueda del error por ser un almacen restringido, cuando se realiza n+1 busquedas
    * luego comprobamos en el archivo log que ese mensaje existe
    */
    
    @Test
    public void buscarRegistroRestringidoErrorTest() {
        Date dataInit = new Date(System.currentTimeMillis());
        registroAlmacen = new RegistroAlmacen(registroRestringido);
        String nombreArchivo = sanitize(nombreRegistroAlmacenReal + "-" + dataInit) + ".log";
        String palabraABuscar = match;
        boolean error = false;
        int coincidencias = 1;
        String errorMessage = "";

        try {
            for (int i = 0; i < busquedas + 1; i++) {
                registroAlmacen.buscar(palabraABuscar);
            }
        } catch (ExcepcionAlmacen ex) {
            errorMessage = ex.getMessage();
            error = true;
        }
        assertTrue(error);
        assertTrue(new File(nombreArchivo).exists());
        assertTrue(buscarEnArchivoDeLog(nombreArchivo, errorMessage));
    }

    private boolean buscarEnArchivoDeLog(String nombre, String phraseToFind) {
        boolean encontrado = false;
        try {
            FileInputStream fstream = new FileInputStream(nombre);
            BufferedReader br = new BufferedReader(new InputStreamReader(fstream));
            String strLine;
            /* read log line by line */
            while ((strLine = br.readLine()) != null) {
                /* parse strLine to obtain what you want */
                System.out.println("linea " + strLine);
                if (strLine.contains(phraseToFind)) {
                    System.out.println("Son iguales " + strLine);
                    encontrado = true;
                }
            }
            in.close();
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
        }
        return encontrado;
    }
    
    private String sanitize(String cadena) {
        return cadena.replaceAll("[^a-zA-Z0-9.-]", "_");
    }
}
