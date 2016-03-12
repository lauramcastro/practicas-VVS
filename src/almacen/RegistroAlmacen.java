package almacen;

import java.util.Collection;

/**
 * Extensión concreta de decorador de almacenes que permite hacer log
 * de algunas operaciones relevantes.
 *
 * @author Carlos Abalde, Laura Castro, Javier París
 * @version 1.0
 */

public class RegistroAlmacen extends ComplementoAlmacen {

    /**
     * Inicializa el estado del decorador.
     *
     * @param almacen almacén a decorar
     */
    public RegistroAlmacen(Almacen almacen) {
        super(almacen);
    }

    /**
     * Busca una cadena de texto entre el conjunto de contenidos que
     * el almacén maneja. El resultado que se devuelve es una lista de
     * los contenidos que efectivamente incluyen esa subcadena en sus
     * títulos. Se hace log de esta operación por consola.
     *
     * @param subcadena cadena de texto a buscar
     * @return lista de contenidos con coincidencia
     * @throws ExcepcionAlmacen si por alguna razón la búsqueda no
     *                          ha podido llevarse a cabo
     */
    public Collection buscar(String subcadena)
        throws ExcepcionAlmacen {
	System.out.print("Buscado '" + subcadena + "' en almacen " + obtenerNombre() + ": ");
	try {
	    Collection resultado = super.buscar(subcadena);
	    System.out.println(resultado.size() + " coincidencias");
	    return resultado;
	} catch (ExcepcionAlmacen e) {
	    System.out.println(e.getMessage());
	    throw e;
	}
    }

}
