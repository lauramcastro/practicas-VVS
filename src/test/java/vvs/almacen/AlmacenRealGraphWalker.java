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

    //private Almacen almacen;
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
            setAttribute("numCont",GrapAux.getNumCont());
	} catch (ExcepcionAlmacen e) {
	    System.err.println("Problema al almacenar contenidos en almacenes");
	} catch (ExcepcionContenido ex) {
            Logger.getLogger(AlmacenRealGraphWalker.class.getName()).log(Level.SEVERE, null, ex);
        }
        //System.out.println("AgregarContenido - Numero de items: "+GrapAux.getNumCont());
    }

    public void eliminarContenido() {
        // System.out.println("Running: eliminarContenido");
	try {	 
            if (GrapAux.getNumCont()>0){
                GrapAux.getAlmacen().eliminarContenido((Contenido)GrapAux.getAlmacen().obtenerContenidos().toArray()[0]);
                GrapAux.removeNumCont();
                setAttribute("numCont",GrapAux.getNumCont());
            }
	} catch (ExcepcionAlmacen e) {
	    System.err.println("Problema al eliminar contenidos de almacenes");
	}
        //System.out.println("eliminarContenido - Numero de items: "+GrapAux.getNumCont());
    }

    // ESTADOS
    public void AlmacenVacio() {
        //System.out.println("AlmacenVacio - Numero de items: "+GrapAux.getNumCont());
        Assert.assertTrue(GrapAux.getNumCont()==0);
    }

    public void AlmacenConContenido() {
        //System.out.println("AlmacenConContenido - Numero de items: "+GrapAux.getNumCont());
        Assert.assertTrue(GrapAux.getNumCont()>0);
    }
}
