package almacen;

/**
 * Excepción que indica una anomalía en el comportamiento de los
 * almacenes.
 *
 * @author Carlos Abalde, Laura Castro, Javier París
 * @version 1.0
 */

public class ExcepcionAlmacen extends Exception {

    /**
     * Construye una excepción de tipo <code>ExcepcionAlmacen</code>.
     *
     * @param descripcion descripción de la anomalía detectada
     */
    public ExcepcionAlmacen(String descripcion) {
        super(descripcion);
    }

}
