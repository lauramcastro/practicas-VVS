package vvs.almacen;

import vvs.contenido.Contenido;

import java.io.IOException;
import java.util.Collection;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.FileHandler;
import java.util.logging.SimpleFormatter;

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
	try {
	    FileHandler handler = new FileHandler(obtenerNombre() + "-" + new Date(System.currentTimeMillis()) + ".log");
	    _logger = Logger.getLogger(RegistroAlmacen.class.getName());
	    _logger.addHandler(handler);
	    handler.setFormatter(new SimpleFormatter());
	    handler.setLevel(Level.ALL);
	    _logger.setLevel(Level.ALL);
	} catch (IOException e) {
	    e.printStackTrace();
	}
        _logger.log(Level.INFO, "--- Log started ---");
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
    public Collection<Contenido> buscar(String subcadena)
        throws ExcepcionAlmacen {
	_logger.log(Level.INFO, "Buscado {0} en almacen {1}",
		    new Object[]{subcadena, obtenerNombre()});
	try {
	    Collection<Contenido> resultado = super.buscar(subcadena);
	    _logger.log(Level.INFO, "Encontradas {0} coincidencias",
			new Object[]{resultado.size()});
	    return resultado;
	} catch (ExcepcionAlmacen e) {
	    _logger.log(Level.SEVERE, e.getMessage());
	    throw e;
	}
    }

    // ========== atributos privados ==========

    /**
     * Manejador de log.
     */
    private Logger _logger;

}
