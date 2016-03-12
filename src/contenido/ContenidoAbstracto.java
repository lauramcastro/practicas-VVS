package contenido;

import java.lang.RuntimeException;
import java.util.ArrayList;
import java.util.Collection;

/**
 * Implementación abstracta de un contenido. Incluye las operaciones básicas
 * que serán comunes a todo tipo de contenidos concretos.
 *
 * @author Carlos Abalde, Laura Castro, Javier París
 * @version 1.0
 */

public abstract class ContenidoAbstracto implements Contenido {

    /**
     * Inicializa el estado común de todo contenido: su título y el
     * posible contenido que lo agrega.
     *
     * @param titulo el título del contenido
     */
    public ContenidoAbstracto(String titulo) {
	_titulo = titulo;
	_padre = null;
    }

    /**
     * Obtiene el título del contenido multimedia.
     *
     * @return título del contenido
     */
    public String obtenerTitulo() {
        return _titulo;
    }

    /**
     * Comprueba si una cadena de texto proporcionada forma parte del
     * título del contenido. El resultado que se devuelve es una lista
     * a la que se habrá añadido este contenido si cumple el criterio
     * de búsqueda.
     *
     * @param subcadena criterio de búsqueda
     * @return lista de contenidos que contienen en su título la
     *         cadena buscada
     */
    public Collection<Contenido> buscar(String subcadena) {
        Collection<Contenido> resultado = new ArrayList<Contenido>();
	String[] tokens = subcadena.toLowerCase().trim().split(" ");
	String   titulo = obtenerTitulo().toLowerCase();
	for (int i = 0 ; i < tokens.length ; i++) {
	    if (! titulo.contains(tokens[i])) {
		return resultado;
	    }
	}
	resultado.add(this);
	return resultado;
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
    	throw new RuntimeException("El contenido " + this + " no aloja otros contenidos.");
    }

    /**
     * Método de gestión de la composición de contenidos que permite
     * eliminar un contenido agregado.
     *
     * @param contenido Contenido a eliminar
     */
    public void eliminar(Contenido contenido) {
        throw new RuntimeException("El contenido " + this + " no aloja otros contenidos.");
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
        throw new RuntimeException("El contenido " + this + " no aloja otros contenidos.");
    }

    /**
     * Método de gestión de la composición (enlace al padre) que permite
     * recuperar el padre (compuesto: colección) de un elemento
     * (componente: contenido)
     *
     * @return Componente que agrega al contenido sobre el que se invoca
     *         este método (<code>null</code> en caso de no existir).
     */
    public Contenido obtenerPadre() {
        return _padre;
    }

    /**
     * Método de gestión de la composición (enlace al padre) que permite
     * establecer el padre (compuesto: colección) de un elemento
     * (componente: contenido). Su visibilidad es protegida porque sólo
     * puede ser invocado dentro de la propia jerarquía de la composición.
     *
     * @param padre Componente que agrega al contenido sobre el que se invoca
     *              este método (<code>null</code> en caso de no existir).
     */
    protected void establecerPadre(Contenido padre) {
    	_padre = padre;
    }

    /**
     * Método que devuelve una cadena de texto que representa el
     * contenido.
     *
     * @return cadena representativa del contenido
     */
    public String toString() {
        return obtenerTitulo();
    }

    // ========== atributos privados ==========

    /**
     * Título del contenido.
     */
    private String _titulo;
    /**
     * Contenido que agrega este contenido.
     */
    private Contenido _padre;

}
