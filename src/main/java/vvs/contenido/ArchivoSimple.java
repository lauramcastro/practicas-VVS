package vvs.contenido;

/**
 * Implementación abstracta de un contenido simple. Incluye las operaciones básicas
 * que serán comunes a todo tipo de contenidos simples concretos.
 *
 * @author Carlos Abalde, Laura Castro, Javier París
 * @version 1.0
 */

public abstract class ArchivoSimple extends ContenidoAbstracto {

    /**
     * Inicializa el estado común de todo contenido simple (su título),
     * llamando al constructor de la superclase.
     *
     * @param titulo el título del contenido simple
     */
    public ArchivoSimple(String titulo) {
        super(titulo);
    }

}
