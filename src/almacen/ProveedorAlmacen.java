package almacen;

import java.util.Collection;

import contenido.Contenido;

/**
 * Extensión concreta de decorador de almacenes que permite
 * implementar una cadena de responsabilidad de almacenes proveedores.
 *
 * @author Carlos Abalde, Laura Castro, Javier París
 * @version 1.0
 */

public class ProveedorAlmacen extends ComplementoAlmacen {

    /**
     * Inicializa el estado del decorador.
     *
     * @param almacen almacén a decorar
     * @param proveedor almacén que servirá de proveedor
     *                  (cadena de responsabilidad)
     */
    public ProveedorAlmacen(Almacen almacen, Almacen proveedor) {
        super(almacen);
        _proveedor = proveedor;
    }

    /**
     * Busca una cadena de texto entre el conjunto de contenidos que
     * el almacén maneja. El resultado que se devuelve es una lista de
     * los contenidos que efectivamente incluyen esa subcadena en sus
     * títulos. Antes de devolver un resultado vacío, se delega en un
     * almacén proveedor (de existir), como implementación de una cadena
     * de responsabilidad.
     *
     * @param subcadena cadena de texto a buscar
     * @return lista de contenidos con coincidencia
     * @throws ExcepcionAlmacen si por alguna razón la búsqueda no
     *                          ha podido llevarse a cabo
     */
    public Collection<Contenido> buscar(String subcadena)
        throws ExcepcionAlmacen {
        Collection<Contenido> resultado = super.buscar(subcadena);
        if ((resultado.isEmpty()) && (obtenerProveedor()!= null)) {
            resultado = obtenerProveedor().buscar(subcadena);
        }
        return resultado;
    }

    /**
     * Proporciona la referencia del proveedor del almacén.
     *
     * @return el almacén que es proveedor de este almacén
     */
    public Almacen obtenerProveedor() {
        return _proveedor;
    }

    /**
     * Establece la identidad del almacén que funcionará como
     * proveedor de este almacén.
     *
     * @param almacen el almacén proveedor
     */
    public void establecerProveedor(Almacen almacen) {
        _proveedor = almacen;
    }

    // ========== atributos privados ==========

    /**
     * Almacén que funciona como proveedor.
     */
    private Almacen _proveedor;

}
