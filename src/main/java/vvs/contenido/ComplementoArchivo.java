package vvs.contenido;

import java.util.Collection;

/**
 * Complemento abstracto de un contenido simple. Incluye las operaciones básicas
 * que serán comunes a todo tipo de complementos de contenidos.
 *
 * @author Carlos Abalde, Laura Castro, Javier París
 * @version 1.0
 */

public abstract class ComplementoArchivo extends ArchivoSimple {

    /**
     * Inicializa el estado común de todo complemento de contenido.
     *
     * @param archivo el contenido a complementar
     */
    public ComplementoArchivo(ArchivoSimple archivo) {
        super("");
        _archivo = archivo;
    }

    /**
     * Obtiene el título del contenido multimedia.
     *
     * @return título del contenido
     */
    public String obtenerTitulo() {
        return _archivo.obtenerTitulo();
    }

    /**
     * Obtiene la duración del contenido (expresada en segundos).
     *
     * @return segundos que dura la reproducción del contenido
     */
    public int obtenerDuracion() {
    	return _archivo.obtenerDuracion();
    }

    /**
     * Obtiene la clasificación de género del contenido.
     *
     * @return género del contenido
     */
    public String obtenerGenero() {
        return _archivo.obtenerGenero();
    }

    /**
     * Recupera la lista ordenada de URLs (cadenas de texto) que
     * habría que invocar para la reproducción del contenido.
     *
     * @return lista ordenada de URLs (cadenas de texto)
     */
    public Collection<String> obtenerListaReproduccion() {
        return _archivo.obtenerListaReproduccion();
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
    	Collection<Contenido> resultado = _archivo.buscar(subcadena);
    	return mantenerIntegridad(resultado);
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
    	_archivo.agregar(contenido, predecesor);
    }

    /**
     * Método de gestión de la composición de contenidos que permite
     * eliminar un contenido agregado.
     *
     * @param contenido Contenido a eliminar
     */
    public void eliminar(Contenido contenido) {
    	_archivo.eliminar(contenido);
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
    	return _archivo.recuperar(n);
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
    	return _archivo.obtenerPadre();
    }

    /**
     * Redefinición del método <code>equals</code> de comparación
     * de objetos, con la finalidad de controlar la igualdad entre las
     * especializaciones de ArchivoSimple.
     *
     * @param object Objeto con el que se quiere comparar éste
     * @return igualdad entre los objetos comparados
     */
    public boolean equals(Object object) {
    	return _archivo.equals(object);
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
    	_archivo.establecerPadre(padre);
    }

    /**
     * Método para mantener la integridad del objeto con decorador de cara a
     * posibles operaciones que puedan devolver, entre otras cosas, el objeto
     * decorado.
     *
     * @param resultado Colección donde es posible que se encuentre el objeto
     *                  decorado
     * @return Colección donde, si aparecía el objeto decorado, ahora aparece
     *         el objeto decorador
     */
    private Collection<Contenido> mantenerIntegridad(Collection<Contenido> resultado) {
    	if (resultado.contains(_archivo)) {
	    resultado.remove(_archivo);
	    resultado.add(this);
    	}
    	return resultado;
    }

    // ========== atributos privados ==========

    /**
     * Objeto decorado.
     */
    private ArchivoSimple _archivo;

}
