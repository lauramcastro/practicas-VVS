package vvs.contenido;

import java.util.ArrayList;
import java.util.Collection;

/**
 * Implementación de ejemplo de la interfaz
 * <code>Contenido</code>. Proporciona una referencia del
 * comportamiento de las operaciones presentes en dicha interfaz, para
 * los supuestos más simples.
 *
 * @author Carlos Abalde, Laura Castro, Javier París
 * @version 1.0
 */
public class ArchivoAudio extends ArchivoSimple {

    /**
     * Construye un contenido simple (archivo de audio), dotándole de un título y
     * asignándole una URL donde reside físicamente, así como una duración en
     *  segundos y un género.
     *
     * @param titulo el título del contenido
     * @param URL cadena de texto representando una URL
     * @param duracion duración del contenido en segundos
     * @param genero clasificación de género del contenido
     */
    public ArchivoAudio(String titulo, String URL, int duracion, String genero)
	throws ExcepcionContenido {
        super(titulo);
	if (duracion >= 0) {
	    _URL = URL;
	    _duracion = duracion;
	    _genero = genero;
	} else {
	    throw new ExcepcionContenido("Duración imposible.");
	}
    }

    /**
     * Obtiene la duración del contenido (expresada en segundos).
     *
     * @return segundos que dura la reproducción del contenido
     */
    public int obtenerDuracion() {
        return _duracion;
    }

    /**
     * Obtiene la clasificación de género del contenido.
     *
     * @return género
     */
    public String obtenerGenero() {
        return _genero;
    }

    /**
     * Recupera la lista ordenada de URLs (cadenas de texto) que
     * habría que invocar para la reproducción del contenido.
     *
     * @return lista ordenada de URLs (cadenas de texto)
     */
    public Collection<String> obtenerListaReproduccion() {
        Collection <String> listaReproduccion = new ArrayList<String>();
        listaReproduccion.add(_URL);
        return listaReproduccion;
    }

    /**
     * Redefinición del método <code>equals</code> de comparación
     * de objetos, con la finalidad de controlar la igualdad entre las
     * especializaciones de ArchivoSimple, en particular en relación con
     * los decoradores.
     *
     * @param object Objeto con el que se quiere comparar éste
     * @return igualdad entre los objetos comparados
     */
    public boolean equals(Object object) {
    	if (object instanceof ArchivoAudio) {
	    return super.equals(object);
    	} else {
	    return object.equals(this);
    	}
    }

    // ========== atributos privados ==========

    /**
     * Cadena de texto representado la URL que indica la ubicación del
     * contenido.
     */
    private String _URL;

    /**
     * Duración del contenido en segundos.
     */
    private int _duracion;

    /**
     * Género en el que se clasifica el contenido.
     */
    private String _genero;

}
