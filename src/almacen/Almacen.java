package almacen;

import java.util.Collection;

import contenido.Contenido;

/**
 * Fachada del subsistema almacén. Incluye las operaciones básicas que
 * se requieren de los almacenes.
 *
 * @author Carlos Abalde, Laura Castro, Javier París
 * @version 1.0
 */

public interface Almacen {

    /**
     * Recupera el nombre del almacén.
     *
     * @return nombre del almacén
     */
    public String obtenerNombre();

    /**
     * Proporciona la lista de contenidos que el almacén gestiona.
     *
     * @return lista de contenidos manejados
     */
    public Collection obtenerContenidos();

    /**
     * Añade un nuevo contenido al conjunto de contenidos manejados
     * por el almacén.
     *
     * @param contenido nuevo contenido a añadir
     * @throws ExcepcionAlmacen si por alguna razón no es posible
     *                          incorporar el nuevo contenido
     */
    public void agregarContenido(Contenido contenido)
        throws ExcepcionAlmacen;

    /**
     * Elimina un contenido del conjunto de contenidos gestionados por
     * el almacén.
     *
     * @param contenido el contenido a eliminar
     * @throws ExcepcionAlmacen si algún motivo impide eliminar el
     *                          contenido indicado
     */
    public void eliminarContenido(Contenido contenido)
        throws ExcepcionAlmacen;

    /**
     * Busca una cadena de texto entre el conjunto de contenidos que
     * el almacén maneja. El resultado que se devuelve es una lista de
     * los contenidos que efectivamente incluyen esa subcadena en sus
     * títulos.
     *
     * @param subcadena cadena de texto a buscar
     * @return lista de contenidos con coincidencia
     * @throws ExcepcionAlmacen si por alguna razón la búsqueda no
     *                          ha podido llevarse a cabo
     */
    public Collection buscar(String subcadena)
        throws ExcepcionAlmacen;

    /**
     * Proporciona la referencia del proveedor del almacén.
     *
     * @return el almacén que es proveedor de este almacén
     */
    public Almacen obtenerProveedor();

    /**
     * Establece la identidad del almacén que funcionará como
     * proveedor de este almacén.
     *
     * @param almacen el almacén proveedor
     */
    public void establecerProveedor(Almacen almacen);

}
