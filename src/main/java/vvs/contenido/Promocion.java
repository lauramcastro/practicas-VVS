package vvs.contenido;

import java.util.Collection;

/**
 * Complemento concreto de un contenido simple en forma de "promoción".
 * Incluye las operaciones específicas de este tipo de complemento.
 *
 * @author Carlos Abalde, Laura Castro, Javier París
 * @version 1.0
 */

public class Promocion extends ComplementoArchivo {

    /**
     * Inicializa el estado del complemento "promoción".
     *
     * @param archivo el contenido a complementar
     * @param URL el complemento del contenido
     */
    public Promocion(ArchivoSimple archivo, String URL) {
        super(archivo);
        _URL = URL;
    }

    /**
     * Obtiene el título del contenido multimedia complementado con "promoción".
     *
     * @return título del contenido complementado
     */
    public String obtenerTitulo() {
        return super.obtenerTitulo() + " con promoción";
    }

    /**
     * Obtiene la duración del contenido complementado (expresada en segundos).
     *
     * @return segundos que dura la reproducción del contenido complementado
     */
    public int obtenerDuracion() {
        return super.obtenerDuracion() + DURACION;
    }

    /**
     * Recupera la lista ordenada de URLs (cadenas de texto) que
     * habría que invocar para la reproducción del contenido y su complemento.
     *
     * @return lista ordenada de URLs (cadenas de texto)
     */
    public Collection<String> obtenerListaReproduccion() {
        Collection<String> listaDeReproduccion = super.obtenerListaReproduccion();
        listaDeReproduccion.add(_URL);
        return listaDeReproduccion;
    }

    // ========== atributos privados ==========

    /**
     * URL de la promoción con la que se decora.
     */
    private String _URL;
    /**
     * Duración (fija) de las promociones.
     */
    private static int DURACION = 15;

}
