package vvs.contenido;

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
        this.contenidos = new ArrayList<Contenido>();
    }

    /**
     * Obtiene la duración de la colección completa (expresada en segundos).
     *
     * @return segundos que dura la reproducción de la colección
     */
    public int obtenerDuracion() {
        int duracion = 0;
        for (int i = 0; i < this.contenidos.size(); i++) {
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
        Iterator<Contenido> iteradorContenidos = this.contenidos.iterator();
        while (iteradorContenidos.hasNext()) {
            listaReproduccion.addAll(iteradorContenidos.next().obtenerListaReproduccion());
        }
        return listaReproduccion;
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
            while (i < this.contenidos.size()
                   && !((ContenidoAbstracto) recuperar(i)).equals(predecesor)) {
                i++;
            }
            if (i == this.contenidos.size()) {
                this.contenidos.add(contenido);
            } else {
                this.contenidos.add(i, contenido);
            }
        } else {
            this.contenidos.add(contenido);
        }
    }

    /**
     * Método de gestión de la composición de contenidos que permite
     * eliminar un contenido agregado.
     *
     * @param contenido Contenido a eliminar
     */
    public void eliminar(Contenido contenido) {
        for (int i = 0; i < this.contenidos.size(); i++) {
            if (((ContenidoAbstracto) recuperar(i)).equals(contenido)) {
                this.contenidos.remove(i);
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
        return ((Contenido) this.contenidos.get(n));
    }

    // ========== atributos privados ==========

    /**
     * Colección de contenidos almacenados.
     */
    private ArrayList<Contenido> contenidos;

}
