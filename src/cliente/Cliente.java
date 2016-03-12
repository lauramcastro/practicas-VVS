package cliente;

import almacen.Almacen;
import contenido.Contenido;

/**
 * Fachada de los clientes. Incluye las operaciones básicas que
 * se requiere de un cliente.
 *
 * @author Carlos Abalde, Laura Castro, Javier París
 * @version 1.0
 */

public interface Cliente {

    /**
     * Añade un nuevo almacén al conjunto de almacenes conocidos
     * por el cliente.
     *
     * @param almacen nuevo almacén a gestionar
     */
    public void gestionarAlmacen(Almacen almacen);

    /**
     * Añade un nuevo contenido al conjunto de contenidos conocidos
     * por el cliente.
     *
     * @param contenido nuevo contenido a gestionar
     */
    public void gestionarContenido(Contenido contenido);

}
