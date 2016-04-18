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
     * Construye un contenido simple (archivo de audio), dotándole de
     * un título y asignándole una URL donde reside físicamente, así
     * como una duración en segundos y un género.
     *
     * @param titulo el título del contenido
     * @param url cadena de texto representando una URL
     * @param duracion duración del contenido en segundos
     * @param genero clasificación de género del contenido
     *
     * @throws ExcepcionContenido si la duración es negativa
     */
    public ArchivoAudio(String titulo, String url, int duracion, String genero)
        throws ExcepcionContenido {
        super(titulo);
        if (duracion >= 0) {
            this.url = url;
            this.duracion = duracion;
            this.genero = genero;
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
        return this.duracion;
    }

    /**
     * Obtiene la clasificación de género del contenido.
     *
     * @return género
     */
    public String obtenerGenero() {
        return this.genero;
    }

    /**
     * Recupera la lista ordenada de URLs (cadenas de texto) que
     * habría que invocar para la reproducción del contenido.
     *
     * @return lista ordenada de URLs (cadenas de texto)
     */
    public Collection<String> obtenerListaReproduccion() {
        Collection<String> listaReproduccion = new ArrayList<String>();
        listaReproduccion.add(this.url);
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

    /**
     * Es una buena práctica redefinir el método <code>hashCode</code>
     * cuando se redefine el método <code>equals</code>.
     *
     * @return código hash para el objeto
     */
    public int hashCode() {
        int hash = 1;
        hash = hash * 31 + this.url.hashCode();
        hash = hash * 31 + this.genero.hashCode();
        return hash;
    }

    // ========== atributos privados ==========

    /**
     * Cadena de texto representado la URL que indica la ubicación del
     * contenido.
     */
    private String url;

    /**
     * Duración del contenido en segundos.
     */
    private int duracion;

    /**
     * Género en el que se clasifica el contenido.
     */
    private String genero;

}
