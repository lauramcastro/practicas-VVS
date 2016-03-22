package vvs.contenido;

/**
 * Excepción que indica una anomalía en el comportamiento de los
 * contenidos.
 *
 * @author Laura Castro
 * @version 1.0
 */

public class ExcepcionContenido extends Exception {

    /**
     * Construye una excepción de tipo <code>ExcepcionContenido</code>.
     *
     * @param descripcion descripción de la anomalía detectada
     */
    public ExcepcionContenido(String descripcion) {
        super(descripcion);
    }

}
