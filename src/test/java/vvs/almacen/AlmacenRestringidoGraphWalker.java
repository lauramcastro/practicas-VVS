package vvs.almacen;

import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;
import vvs.contenido.Contenido;

import org.junit.Assert;


import org.graphwalker.core.machine.ExecutionContext;
import org.graphwalker.java.annotation.GraphWalker;
import vvs.contenido.ArchivoAudio;
import vvs.contenido.ExcepcionContenido;
import vvs.util.GrapAux;

//@GraphWalker(value = "random(edge_coverage(100))")
@GraphWalker(value = "random(time_duration(1))")
public class AlmacenRestringidoGraphWalker extends ExecutionContext implements AlmacenRestringidoModel {

    //private Almacen almacen;
    private Contenido presente;
    
    // TRANSICIONES

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
            Logger.getLogger(AlmacenRestringidoGraphWalker.class.getName()).log(Level.SEVERE, null, ex);
        }
        //System.out.println("AgregarContenido - Numero de items: "+GrapAux.getNumCont());
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

    public void buscar() {
        try {
            if(GrapAux.getNumBusq()>0){
                GrapAux.getAlmacen().buscar("Amy");
                GrapAux.lessNumBusq();
                setAttribute("numBusq",GrapAux.getNumBusq());
                TimeUnit.MILLISECONDS.sleep(1000);
                GrapAux.addToTimeElapsed(1000);
                setAttribute("time",GrapAux.getTimeElapsed());
            } else if(GrapAux.getTimeElapsed() < 59999){
                TimeUnit.MILLISECONDS.sleep(1000);
                GrapAux.addToTimeElapsed(1000);
                setAttribute("time",GrapAux.getTimeElapsed());
            } else {
                GrapAux.resetTimeElapsed();
                GrapAux.resetNumBusq();                
                GrapAux.getAlmacen().buscar("Amy");
                GrapAux.lessNumBusq();
                setAttribute("numBusq",GrapAux.getNumBusq());
                TimeUnit.MILLISECONDS.sleep(1000);
                GrapAux.addToTimeElapsed(1000);
                setAttribute("time",GrapAux.getTimeElapsed());
            }
        } catch (ExcepcionAlmacen ex) {
            Logger.getLogger(AlmacenRestringidoGraphWalker.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InterruptedException ex) {
            Logger.getLogger(AlmacenRestringidoGraphWalker.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
    //ESTADOS

    public void AlmacenSinContenidoSinBusquedas() {
        Assert.assertEquals(0,GrapAux.getNumBusq());
        Assert.assertEquals(0,GrapAux.getNumCont());
    }

    public void AlmacenConContenidoConBusquedas() {
        Assert.assertTrue(GrapAux.getNumBusq()>0);
        Assert.assertTrue(GrapAux.getNumCont()>0);
    }

    public void AlmacenSinContenidoConBusquedas() {
        Assert.assertTrue(GrapAux.getNumBusq()>0);
        Assert.assertTrue(GrapAux.getNumCont()==0);
    }

    public void AlmacenConContenidoSinBusquedas() {
        System.out.println("Busq:"+GrapAux.getNumBusq());
        System.out.println("Cont:"+GrapAux.getNumCont());
        Assert.assertTrue(GrapAux.getNumBusq()==0);
        Assert.assertTrue(GrapAux.getNumCont()>0);
    }
}
