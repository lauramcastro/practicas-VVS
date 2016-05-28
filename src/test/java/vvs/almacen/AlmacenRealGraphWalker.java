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
@GraphWalker(value = "random(time_duration(1))")
public class AlmacenRealGraphWalker extends ExecutionContext implements AlmacenRealModel {

    private Contenido presente;
    
    // TRANSICIONES

    public void agregarContenido() {
	try {
            int num = GrapAux.getNumCont();
            presente = new ArchivoAudio("Amy Winehouse: Rehab"+num, "http://servidor/winehouse/back2black/1"+num, 215, "Soul");
	    GrapAux.getAlmacen().agregarContenido(presente);
            GrapAux.addNumCont();
            setAttribute("numCont",GrapAux.getNumCont());
	} catch (ExcepcionAlmacen e) {
	    System.err.println("Problema al almacenar contenidos en almacenes");
	} catch (ExcepcionContenido ex) {
            Logger.getLogger(AlmacenRealGraphWalker.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    public void eliminarContenido() {
	try {	 
            if (GrapAux.getNumCont()>0){
                GrapAux.getAlmacen().eliminarContenido((Contenido)GrapAux.getAlmacen().obtenerContenidos().toArray()[0]);
                GrapAux.removeNumCont();
                setAttribute("numCont",GrapAux.getNumCont());
            }
	} catch (ExcepcionAlmacen e) {
	    System.err.println("Problema al eliminar contenidos de almacenes");
	}
    }

    // ESTADOS
    public void AlmacenVacio() {
        Assert.assertTrue(GrapAux.getNumCont()==0);
    }

    public void AlmacenConContenido() {
        Assert.assertTrue(GrapAux.getNumCont()>0);
    }
}
