package vvs.almacen;

import java.util.Collection;

import vvs.contenido.Contenido;

/**
 * Abstracción de Almacén que posibilita la incorporación de
 * decoradores para este tipo de objetos.
 *
 * @author Carlos Abalde, Laura Castro, Javier París
 * @version 1.0
 */

public abstract class ComplementoAlmacen implements Almacen {

    /**
     * Inicializa el estado común a todos los decoradores
     * de <code>Almacen</code>.
     *
     * @param almacen almacén a decorar
     */
    public ComplementoAlmacen(Almacen almacen) {
        this.almacen = almacen;
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
     * el almacén maneja.
     *
     * @param subcadena cadena de texto a buscar
     * @return lista de contenidos con coincidencia
     * @throws ExcepcionAlmacen si por alguna razón la búsqueda no
     *                          ha podido llevarse a cabo
     */
    public Collection<Contenido> buscar(String subcadena)
        throws ExcepcionAlmacen {
        return this.almacen.buscar(subcadena);
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
     * Almacén que se decora.
     */
    private Almacen almacen;

}
