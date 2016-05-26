package vvs.almacen;

import java.util.logging.Level;
import java.util.logging.Logger;
import vvs.contenido.Contenido;

import org.junit.Assert;


import org.graphwalker.core.machine.ExecutionContext;
import org.graphwalker.core.condition.TimeDuration;
import org.graphwalker.java.annotation.GraphWalker;
import vvs.contenido.ArchivoAudio;
import vvs.contenido.ExcepcionContenido;
import vvs.util.GrapAux;

//@GraphWalker(value = "random(edge_coverage(100))")
@GraphWalker(value = "random(time_duration(10))")
public class AlmacenRealGraphWalker extends ExecutionContext implements AlmacenRealModel {

    private Almacen almacen;
    private Contenido presente;
    
    // TRANSICIONES
    
    /*public void iniciar() {
        try {
            almacen = new AlmacenReal("Universal Music Group");
            presente = new ArchivoAudio("Amy Winehouse: Rehab", "http://servidor/winehouse/back2black/1", 215, "Soul");
        } catch (ExcepcionContenido ex) {
            System.err.println("Problema al crear contenidos");
        }
    }*/

    public void agregarContenido() {
        // System.out.println("Running: agregarContenido");
	try {
            int num = GrapAux.getNumCont();
            presente = new ArchivoAudio("Amy Winehouse: Rehab"+num, "http://servidor/winehouse/back2black/1"+num, 215, "Soul");
	    GrapAux.getAlmacen().agregarContenido(presente);
            GrapAux.addNumCont();
	} catch (ExcepcionAlmacen e) {
	    System.err.println("Problema al almacenar contenidos en almacenes");
	} catch (ExcepcionContenido ex) {
            Logger.getLogger(AlmacenRealGraphWalker.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    public void eliminarContenido() {
        // System.out.println("Running: eliminarContenido");
	try {	    
            GrapAux.getAlmacen().eliminarContenido((Contenido)GrapAux.getAlmacen().obtenerContenidos().toArray()[0]);
	} catch (ExcepcionAlmacen e) {
	    System.err.println("Problema al eliminar contenidos de almacenes");
	}
    }

    // ESTADOS
    public void AlmacenVacio() {
        Assert.assertTrue(GrapAux.getNumCont()==0);
    }

    public void AlmacenConContenido() {
        Assert.assertFalse(GrapAux.getNumCont()!=0);
    }
}
