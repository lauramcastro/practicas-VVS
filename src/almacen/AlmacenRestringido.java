package almacen;

import java.util.Collection;

import contenido.Contenido;

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
        _almacen = almacen;
        _busquedas = busquedas;
        _busquedasDisponibles = busquedas;
        _timestamp = System.currentTimeMillis();
        _minutos = minutos;
    }

    /**
     * Recupera el nombre del almacén.
     *
     * @return nombre del almacén
     */
    public String obtenerNombre() {
        return _almacen.obtenerNombre();
    }

    /**
     * Proporciona la lista de contenidos que el almacén gestiona.
     *
     * @return lista de contenidos manejados
     */
    public Collection<Contenido> obtenerContenidos() {
        return _almacen.obtenerContenidos();
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
        _almacen.agregarContenido(contenido);
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
        _almacen.eliminarContenido(contenido);
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
    public Collection buscar(String subcadena)
        throws ExcepcionAlmacen {
        if (_busquedasDisponibles > 0) {
            Collection resultado = _almacen.buscar(subcadena);
            if (!resultado.isEmpty()) {
                _busquedasDisponibles--;
            }
            return resultado;
        } else {
            if ((System.currentTimeMillis() - _timestamp) > _minutos * 60000) {
                _timestamp = System.currentTimeMillis();
                _busquedasDisponibles = _busquedas;
                return buscar(subcadena);
            }
        }
        throw new ExcepcionAlmacen("Espere " +
				   (_minutos*60000-(System.currentTimeMillis()-_timestamp))/1000  +
				   " segundos hasta su próxima búsqueda.");
    }

    /**
     * Proporciona la referencia del proveedor del almacén.
     *
     * @return el almacén que es proveedor de este almacén
     */
    public Almacen obtenerProveedor() {
        return _almacen.obtenerProveedor();
    }

    /**
     * Establece la identidad del almacén que funcionará como
     * proveedor de este almacén.
     *
     * @param almacen el almacén proveedor
     */
    public void establecerProveedor(Almacen almacen) {
        _almacen.establecerProveedor(almacen);
    }

    // ========== atributos privados ==========

    /**
     * Almacén que se limita.
     */
    private Almacen _almacen;
    /**
     * Momento que marca el inicio del intervalo de limitación de búsquedas.
     */
    private long _timestamp;
    /**
     * Número de búsquedas máximo que pueden realizarse en un intervalo dado.
     */
    private final int _busquedas;
    /**
     * Duración del intervalo de limitación del proxy.
     */
    private final int _minutos;
    /**
     * Número de búsquedas que aún pueden realizarse en el presente intervalo
     * antes de alcanzar el máximo que limita el proxy.
     */
    private int _busquedasDisponibles;

}
