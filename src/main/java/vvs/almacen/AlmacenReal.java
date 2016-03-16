package vvs.almacen;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import vvs.contenido.Contenido;

/**
 * Implementación de ejemplo de la interfaz
 * <code>Almacen</code>. Proporciona una referencia del comportamiento
 * de las operaciones presentes en dicha interfaz, para el caso más
 * simple.
 *
 * @author Carlos Abalde, Laura Castro, Javier París
 * @version 1.0
 */

public class AlmacenReal implements Almacen {

    /**
     * Crea un <code>AlmacenReal</code>.
     *
     * @param nombre nombre del almacén
     */
    public AlmacenReal(String nombre) {
        _nombre = nombre;
        _contenidos = new ArrayList<Contenido>();
    }

    /**
     * Recupera el nombre del almacén.
     *
     * @return nombre del almacén
     */
    public String obtenerNombre() {
        return _nombre;
    }

    /**
     * Proporciona la lista de contenidos que el almacén gestiona.
     *
     * @return lista de contenidos manejados
     */
    public Collection<Contenido> obtenerContenidos() {
        return _contenidos;
    }

    /**
     * Añade un nuevo contenido al conjunto de contenidos manejados
     * por el almacén.
     *
     * @param contenido nuevo contenido a añadir
     * @throws ExcepcionAlmacen si el nuevo contenido ya forma parte
     *                          del conjunto manejado por el almacén
     */
    public void agregarContenido(Contenido contenido)
        throws ExcepcionAlmacen {
	if (contenido != null) {
	    if (!_contenidos.contains(contenido)) {
		_contenidos.add(contenido);
	    } else {
		throw new ExcepcionAlmacen("Contenido duplicado.");
	    }
	} else {
	    throw new ExcepcionAlmacen("Contenido inválido.");
        }
    }

    /**
     * Elimina un contenido del conjunto de contenidos gestionados por
     * el almacén.
     *
     * @param contenido el contenido a eliminar
     * @throws ExcepcionAlmacen si el contenido que se pretende
     *                          eliminar no consta entre los gestionados
     *                          por el almacén
     */
    public void eliminarContenido(Contenido contenido)
        throws ExcepcionAlmacen {
        if (_contenidos.contains(contenido)) {
            _contenidos.remove(contenido);
        } else {
            throw new ExcepcionAlmacen("Contenido inexistente.");
        }
    }

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
    public Collection<Contenido> buscar(String subcadena)
        throws ExcepcionAlmacen {
        Collection<Contenido> resultado = new ArrayList<Contenido>();
        Iterator<Contenido> contenidos = _contenidos.iterator();
        while (contenidos.hasNext()) {
            resultado.addAll((contenidos.next()).buscar(subcadena));
        }
        return resultado;
    }

    /**
     * Proporciona la referencia del proveedor del almacén.
     *
     * @return el almacén que es proveedor de este almacén
     */
    public Almacen obtenerProveedor() {
        return null;
    }

    /**
     * Establece la identidad del almacén que funcionará como
     * proveedor de este almacén.
     *
     * @param almacen el almacén proveedor
     */
    public void establecerProveedor(Almacen almacen) {
    }

    // ========== atributos privados ==========

    /**
     * Nombre del almacén.
     */
    private String _nombre;

    /**
     * Lista de contenidos conocidos por el almacén.
     */
    private Collection<Contenido> _contenidos;

}
