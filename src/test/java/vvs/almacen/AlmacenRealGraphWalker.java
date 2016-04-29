package vvs.almacen;

import vvs.contenido.Contenido;

import org.junit.Assert;

import org.graphwalker.core.machine.ExecutionContext;
import org.graphwalker.java.annotation.GraphWalker;
import vvs.contenido.ArchivoAudio;
import vvs.contenido.ExcepcionContenido;

@GraphWalker(value = "random(edge_coverage(100))", start = "iniciar")
public class AlmacenRealGraphWalker extends ExecutionContext implements AlmacenRealModel {

    private Almacen almacen;
    private Contenido presente;
    
    // TRANSICIONES
    
    public void iniciar() {
        try {
            almacen = new AlmacenReal("Universal Music Group");
            presente = new ArchivoAudio("Amy Winehouse: Rehab", "http://servidor/winehouse/back2black/1", 215, "Soul");
        } catch (ExcepcionContenido ex) {
            System.err.println("Problema al crear contenidos");
        }
    }

    public void agregarContenido() {
        // System.out.println("Running: agregarContenido");
	try {
	    almacen.agregarContenido(presente);
	} catch (ExcepcionAlmacen e) {
	    System.err.println("Problema al almacenar contenidos en almacenes");
	}
    }

    public void eliminarContenido() {
        // System.out.println("Running: eliminarContenido");
	try {	    
            almacen.eliminarContenido(presente);
	} catch (ExcepcionAlmacen e) {
	    System.err.println("Problema al eliminar contenidos de almacenes");
	}
    }

    // ESTADOS
    public void AlmacenVacio() {
        // System.out.println("In state: sin proveedor");
	try {
	    Assert.assertTrue(almacen.buscar(presente.obtenerTitulo()).isEmpty());
	} catch (ExcepcionAlmacen e) {
	    System.err.println("Problema inesperado");
	}
    }

    public void AlmacenConContenido() {
        // System.out.println("In state: con proveedor");
	try {
	    Assert.assertFalse(almacen.buscar(presente.obtenerTitulo()).isEmpty());
	} catch (ExcepcionAlmacen e) {
	    System.err.println("Problema inesperado");
	}
    }
}
