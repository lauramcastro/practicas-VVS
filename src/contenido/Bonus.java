package contenido;

import java.util.Collection;

/**
 * Complemento concreto de un contenido simple en forma de "bonus".
 * Incluye las operaciones específicas de este tipo de complemento.
 *
 * @author Carlos Abalde, Laura Castro, Javier París
 * @version 1.0
 */

public class Bonus extends ComplementoArchivo {

    /**
     * Inicializa el estado del complemento "bonus".
     *
     * @param archivo el contenido a complementar
     * @param bonus el complemento del contenido
     */
    public Bonus(ArchivoSimple archivo, Contenido bonus) {
        super(archivo);
        _bonus = bonus;
    }

    /**
     * Obtiene el título del contenido multimedia complementado con "bonus".
     *
     * @return título del contenido complementado
     */
    public String obtenerTitulo() {
        return super.obtenerTitulo() + " con bonus (" + _bonus.obtenerTitulo() + ")";
    }

    /**
     * Obtiene la duración del contenido complementado (expresada en segundos).
     *
     * @return segundos que dura la reproducción del contenido complementado
     */
    public int obtenerDuracion() {
        return super.obtenerDuracion() + _bonus.obtenerDuracion();
    }

    /**
     * Recupera la lista ordenada de URLs (cadenas de texto) que
     * habría que invocar para la reproducción del contenido y su complemento.
     *
     * @return lista ordenada de URLs (cadenas de texto)
     */
    public Collection<String> obtenerListaReproduccion() {
        Collection<String> listaReproduccion = super.obtenerListaReproduccion();
        listaReproduccion.addAll(_bonus.obtenerListaReproduccion());
        return listaReproduccion;
    }

    // ========== atributos privados ==========

    /**
     * Objeto añadido al decorar.
     */
    private Contenido _bonus;

}
