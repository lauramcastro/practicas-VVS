package contenido;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

/**
 * Implementación de una colección de contenidos. Incluye el comportamiento
 * de los contenidos adaptado al caso específico de las colecciones.
 *
 * @author Carlos Abalde, Laura Castro, Javier París
 * @version 1.0
 */

public class Coleccion extends ContenidoAbstracto {

    /**
     * Inicializa el estado de la colección: su título y el
     * conjunto de contenidos (vacío).
     *
     * @param titulo el título del contenido
     */
    public Coleccion(String titulo) {
    	super(titulo);
    	_contenidos = new ArrayList<Contenido>();
    }

    /**
     * Obtiene la duración de la colección completa (expresada en segundos).
     *
     * @return segundos que dura la reproducción de la colección
     */
    public int obtenerDuracion() {
    	int duracion = 0;
    	for (int i = 0; i < _contenidos.size(); i++) {
    	    duracion += recuperar(i).obtenerDuracion();
    	}
    	return duracion;
    }

    /**
     * Obtiene la clasificación de género de la colección.
     *
     * @return género de la colección
     */
    public String obtenerGenero() {
    	return "Mix";
    }

    /**
     * Recupera la lista ordenada de URLs (cadenas de texto) que
     * habría que invocar para la reproducción de la colección.
     *
     * @return lista ordenada de URLs (cadenas de texto)
     */
    public Collection<String> obtenerListaReproduccion() {
    	Collection<String> listaReproduccion = new ArrayList<String>();
	Iterator<Contenido> contenidos = _contenidos.iterator();
	while (contenidos.hasNext()) {
	    listaReproduccion.addAll(contenidos.next().obtenerListaReproduccion());
    	}
    	return listaReproduccion;
    }

    /**
     * Comprueba si una cadena de texto proporcionada forma parte de
     * alguno de los títulos de sus contenidos. El resultado que se devuelve
     * es una lista a la que se habrán añadido (recursivamente) los contenidos
     * que cumplen el criterio de búsqueda, así como la propia colección
     * si su título también la cumple.
     *
     * @param subcadena criterio de búsqueda
     * @return lista de contenidos que contienen en su título la
     *         cadena buscada
     */
    public Collection<Contenido> buscar(String subcadena) {
        return super.buscar(subcadena);
    }

    /**
     * Método de gestión de la composición de contenidos que permite
     * agregar un nuevo contenido ordenado.
     *
     * @param contenido Contenido a agregar
     * @param predecesor Contenido que precede en orden a aquél que se va
     *                   a agregar
     */
    public void agregar(Contenido contenido, Contenido predecesor) {
    	if (contenido.obtenerPadre() != null) {
    	    contenido.obtenerPadre().eliminar(contenido);
    	}
        ((ContenidoAbstracto) contenido).establecerPadre(this);
        if (predecesor != null) {
	    int i = 0;
	    while ((i<_contenidos.size()) && !(((ContenidoAbstracto) recuperar(i)).equals(predecesor))) {
		i++;
            }
	    if (i == _contenidos.size()) {
		_contenidos.add(contenido);
	    } else {
		_contenidos.add(i, contenido);
	    }
        } else {
	    _contenidos.add(contenido);
        }
    }

    /**
     * Método de gestión de la composición de contenidos que permite
     * eliminar un contenido agregado.
     *
     * @param contenido Contenido a eliminar
     */
    public void eliminar(Contenido contenido) {
    	for (int i = 0; i<_contenidos.size(); i++) {
	    if (((ContenidoAbstracto) recuperar(i)).equals(contenido)) {
		_contenidos.remove(i);
	    }
    	}
    	((ContenidoAbstracto) contenido).establecerPadre(null);
    }

    /**
     * Método de gestión de la composición de contenidos que permite
     * recuperar un determinado contenido agregado.
     *
     * @param n Número de orden del contenido a recuperar
     * @return Contenido que ocupa la enésima posición de orden en la
     *         agregación de contenidos.
     */
    public Contenido recuperar(int n) {
    	return ((Contenido) _contenidos.get(n));
    }

    // ========== atributos privados ==========

    /**
     * Colección de contenidos almacenados.
     */
    private ArrayList<Contenido> _contenidos;

}
