package contenido;

import java.util.Collection;

/**
 * Fachada del subsistema contenido. Incluye las operaciones básicas
 * que será necesario realizar sobre dichos contenidos multimedia.
 *
 * @author Carlos Abalde, Laura Castro, Javier París
 * @version 1.0
 */

public interface Contenido {

    /**
     * Obtiene el título del contenido multimedia.
     *
     * @return título del contenido
     */
    public String obtenerTitulo();

    /**
     * Obtiene la duración del contenido (expresada en segundos).
     *
     * @return segundos que dura la reproducción del contenido
     */
    public int obtenerDuracion();

    /**
     * Obtiene la clasificación de género del contenido.
     *
     * @return género del contenido
     */
    public String obtenerGenero();

    /**
     * Recupera la lista ordenada de URLs (cadenas de texto) que
     * habría que invocar para la reproducción del contenido.
     *
     * @return lista ordenada de URLs (cadenas de texto)
     */
    public Collection<String> obtenerListaReproduccion();

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
    public Collection<Contenido> buscar(String subcadena);

    /**
     * Método de gestión de la composición de contenidos que permite
     * agregar un nuevo contenido ordenado.
     *
     * @param contenido Contenido a agregar
     * @param predecesor Contenido que precede en orden a aquél que se va
     *                   a agregar
     */
    public void agregar(Contenido contenido, Contenido predecesor);

    /**
     * Método de gestión de la composición de contenidos que permite
     * eliminar un contenido agregado.
     *
     * @param contenido Contenido a eliminar
     */
    public void eliminar(Contenido contenido);

    /**
     * Método de gestión de la composición de contenidos que permite
     * recuperar un determinado contenido agregado.
     *
     * @param n Número de orden del contenido a recuperar
     * @return Contenido que ocupa la enésima posición de orden en la
     *         agregación de contenidos.
     */
    public Contenido recuperar(int n);

    /**
     * Método de gestión de la composición (enlace al padre) que permite
     * recuperar el padre (compuesto: colección) de un elemento
     * (componente: contenido)
     *
     * @return Componente que agrega al contenido sobre el que se invoca
     *         este método (<code>null</code> en caso de no existir).
     */
    public Contenido obtenerPadre();

}
