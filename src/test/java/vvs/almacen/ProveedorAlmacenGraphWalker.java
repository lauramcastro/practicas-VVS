package vvs.almacen;

import vvs.contenido.ArchivoAudio;
import vvs.contenido.Contenido;
import vvs.contenido.ExcepcionContenido;

import org.junit.Assert;

import org.graphwalker.core.machine.ExecutionContext;
import org.graphwalker.java.annotation.GraphWalker;

@GraphWalker(value = "random(edge_coverage(100))", start = "iniciar")
public class ProveedorAlmacenGraphWalker extends ExecutionContext implements ProveedorAlmacenModel {

    // TRANSICIONES
    
    public void iniciar() {
        // System.out.println("Running: iniciar");
	try {
	    almacen = new ProveedorAlmacen(new AlmacenReal("Universal Music Group"), null);
	    proveedor = new AlmacenReal("Sony BMG Music");
	    presente = new ArchivoAudio("Amy Winehouse: Rehab", "http://servidor/winehouse/back2black/1", 215, "Soul");
	    ausente = new ArchivoAudio("Avril Lavigne: Girlfriend", "http://servidor/alavigne/bestdamnthing/1", 216, "Punk pop");
	    proveedor.agregarContenido(ausente);
	} catch (ExcepcionContenido c) {
	    System.err.println("Problema al crear contenidos");
	} catch (ExcepcionAlmacen c) {
	    System.err.println("Problema al crear almacenes");
	}
    }

    public void establecerProveedor() {
        // System.out.println("Running: establecerProveedor");
	almacen.establecerProveedor(proveedor);
    }

    public void eliminarProveedor() {
        // System.out.println("Running: eliminarProveedor");
	almacen.establecerProveedor(null);
    }

    public void agregarContenido() {
        // System.out.println("Running: agregarContenido");
	try {
	    if (almacen.obtenerContenidos().isEmpty()) {
		almacen.agregarContenido(presente);
	    } // senón saltaría a excepción de contido duplicado
	} catch (ExcepcionAlmacen e) {
	    System.err.println("Problema al almacenar contenidos en almacenes");
	}
    }

    public void eliminarContenido() {
        // System.out.println("Running: eliminarContenido");
	try {
	    if (!almacen.obtenerContenidos().isEmpty()) {
		almacen.eliminarContenido(presente);
	    } // senón saltaría a excepción de contido inexistente
	} catch (ExcepcionAlmacen e) {
	    System.err.println("Problema al eliminar contenidos de almacenes");
	}
    }

    // ESTADOS
    public void SinProveedor() {
        // System.out.println("In state: sin proveedor");
	try {
	    Assert.assertTrue(almacen.obtenerProveedor() == null);
	    Assert.assertTrue(almacen.buscar(ausente.obtenerTitulo()).isEmpty());
	} catch (ExcepcionAlmacen e) {
	    System.err.println("Problema inesperado");
	}
    }

    public void ConProveedor() {
        // System.out.println("In state: con proveedor");
	try {
	    Assert.assertFalse(almacen.obtenerProveedor() == null);
	    Assert.assertFalse(almacen.buscar(ausente.obtenerTitulo()).isEmpty());
	} catch (ExcepcionAlmacen e) {
	    System.err.println("Problema inesperado");
	}
    }

    private Almacen almacen;
    private Almacen proveedor;
    private Contenido presente;
    private Contenido ausente;

}
