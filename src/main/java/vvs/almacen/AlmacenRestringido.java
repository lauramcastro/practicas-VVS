package vvs.almacen;

import java.util.Collection;

import vvs.contenido.Contenido;

/**
 * Implementación de Almacén que funciona como proxy de algún otro almacén.
 * La funcionalidad del proxy consiste en limitar el número de búsquedas
 * por intervalo de tiempo dado.
 *
 * @author Carlos Abalde, Laura Castro, Javier París
 * @version 1.0
 */

public class AlmacenRestringido implements Almacen {

    /**
     * Crea un <code>AlmacenRestringido</code>.
     *
     * @param almacen almacén a limitar
     * @param busquedas número de búsquedas máximo
     * @param minutos intervalo de tiempo
     */
    public AlmacenRestringido(Almacen almacen, int busquedas, int minutos) {
        this.almacen = almacen;
        this.busquedas = busquedas;
        this.busquedasDisponibles = busquedas;
        this.timestamp = System.currentTimeMillis();
        this.minutos = minutos;
    }

    /**
     * Recupera el nombre del almacén.
     *
     * @return nombre del almacén
     */
    public String obtenerNombre() {
        return this.almacen.obtenerNombre();
    }

    /**
     * Proporciona la lista de contenidos que el almacén gestiona.
     *
     * @return lista de contenidos manejados
     */
    public Collection<Contenido> obtenerContenidos() {
        return this.almacen.obtenerContenidos();
    }

    /**
     * Añade un nuevo contenido al conjunto de contenidos manejados
     * por el almacén.
     *
     * @param contenido nuevo contenido a añadir
     * @throws ExcepcionAlmacen si por alguna razón no es posible
     *                          incorporar el nuevo contenido
     */
    public void agregarContenido(Contenido contenido)
        throws ExcepcionAlmacen {
        this.almacen.agregarContenido(contenido);
    }

    /**
     * Elimina un contenido del conjunto de contenidos gestionados por
     * el almacén.
     *
     * @param contenido el contenido a eliminar
     * @throws ExcepcionAlmacen si algún motivo impide eliminar el
     *                          contenido indicado
     */
    public void eliminarContenido(Contenido contenido)
        throws ExcepcionAlmacen {
        this.almacen.eliminarContenido(contenido);
    }

    /**
     * Busca una cadena de texto entre el conjunto de contenidos que
     * el almacén maneja. El resultado que se devuelve es una lista de
     * los contenidos que efectivamente incluyen esa subcadena en sus
     * títulos. El número de búsquedas realizadas se limita por intervalo
     * de tiempo según la configuración del proxy.
     *
     * @param subcadena cadena de texto a buscar
     * @return lista de contenidos con coincidencia
     * @throws ExcepcionAlmacen si por alguna razón la búsqueda no
     *                          ha podido llevarse a cabo
     */
    public Collection<Contenido> buscar(String subcadena)
        throws ExcepcionAlmacen {
        if (this.busquedasDisponibles > 0) {
            Collection<Contenido> resultado = this.almacen.buscar(subcadena);
            if (!resultado.isEmpty()) {
                this.busquedasDisponibles--;
            }
            return resultado;
        } else {
            if ((System.currentTimeMillis() - this.timestamp) > this.minutos * MILISEGUNDOS) {
                this.timestamp = System.currentTimeMillis();
                this.busquedasDisponibles = this.busquedas;
                return buscar(subcadena);
            }
        }
        throw new ExcepcionAlmacen("Espere "
                                   + (this.minutos * MILISEGUNDOS
                                      - (System.currentTimeMillis() - this.timestamp)) / SEGUNDOS
                                   + " segundos hasta su próxima búsqueda.");
    }

    /**
     * Proporciona la referencia del proveedor del almacén.
     *
     * @return el almacén que es proveedor de este almacén
     */
    public Almacen obtenerProveedor() {
        return this.almacen.obtenerProveedor();
    }

    /**
     * Establece la identidad del almacén que funcionará como
     * proveedor de este almacén.
     *
     * @param almacen el almacén proveedor
     */
    public void establecerProveedor(Almacen almacen) {
        this.almacen.establecerProveedor(almacen);
    }

    // ========== atributos privados ==========

    /**
     * Almacén que se limita.
     */
    private Almacen almacen;
    /**
     * Momento que marca el inicio del intervalo de limitación de búsquedas.
     */
    private long timestamp;
    /**
     * Número de búsquedas máximo que pueden realizarse en un intervalo dado.
     */
    private final int busquedas;
    /**
     * Duración del intervalo de limitación del proxy.
     */
    private final int minutos;
    /**
     * Número de búsquedas que aún pueden realizarse en el presente intervalo
     * antes de alcanzar el máximo que limita el proxy.
     */
    private int busquedasDisponibles;

    /**
     * Constante para conversión de minutos a milisegundos.
     */
    private static final int MILISEGUNDOS = 60000;
    /**
     * Constante para conversión de milisegundos a segundos.
     */
    private static final int SEGUNDOS = 1000;
}
