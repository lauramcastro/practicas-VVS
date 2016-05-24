package vvs.almacen;

import edu.umd.cs.findbugs.annotations.SuppressWarnings;
import vvs.contenido.ArchivoAudio;
import vvs.contenido.Contenido;
import vvs.contenido.ExcepcionContenido;

import org.junit.Assert;

import org.graphwalker.core.machine.ExecutionContext;
import org.graphwalker.java.annotation.GraphWalker;
import vvs.contenido.GrapAux;

@GraphWalker(value = "random(edge_coverage(100))", start = "iniciar")
public class ProveedorAlmacenGraphWalker extends ExecutionContext implements ProveedorAlmacenModel {

    // TRANSICIONES
    public void iniciar() {
        try {
            almacen = new ProveedorAlmacen(new AlmacenReal("Universal Music Group"), null);
            proveedor = new AlmacenReal("Sony BMG Music");
            presente = new ArchivoAudio("Amy Winehouse: Rehab", "http://servidor/winehouse/back2black/1", 215, "Soul");
            ausente = new ArchivoAudio("Avril Lavigne: Girlfriend", "http://servidor/alavigne/bestdamnthing/1", 216, "Punk pop");
            proveedor.agregarContenido(ausente);   
            /*INIT */
            GrapAux.setAlmacen(almacen);            
            GrapAux.resetNumBusq();
            
        } catch (ExcepcionContenido c) {
            System.err.println("Problema al crear contenidos");
        } catch (ExcepcionAlmacen c) {
            System.err.println("Problema al crear almacenes");
        }
    }

    public void establecerProveedor() {
        System.out.println("Running: establecerProveedor");
        GrapAux.getAlamcen().establecerProveedor(proveedor);
    }

    public void eliminarProveedor() {
        System.out.println("Running: eliminarProveedor");       
        GrapAux.getAlamcen().establecerProveedor(null);
    }

    public void agregarContenido() {
        System.out.println("Running: agregarContenido");
        try {
            if (GrapAux.getAlamcen().obtenerContenidos().isEmpty()) {
                GrapAux.getAlamcen().agregarContenido(presente);
                GrapAux.addNumCont();
            } // senón saltaría a excepción de contido duplicado
        } catch (ExcepcionAlmacen e) {
            System.err.println("Problema al almacenar contenidos en almacenes");
        }
    }

    public void eliminarContenido() {
        System.out.println("Running: eliminarContenido");
        try {
            if (!GrapAux.getAlamcen().obtenerContenidos().isEmpty()) {
                GrapAux.getAlamcen().eliminarContenido(presente);
                GrapAux.removeNumCont();
            } // senón saltaría a excepción de contido inexistente
        } catch (ExcepcionAlmacen e) {
            System.err.println("Problema al eliminar contenidos de almacenes");           
        }
    }   
    
    public void SinProveedor() {
        System.out.println("In state: sin proveedor");
        try {
            Assert.assertTrue(GrapAux.getAlamcen().obtenerProveedor() == null);
            Assert.assertTrue(GrapAux.getAlamcen().buscar(ausente.obtenerTitulo()).isEmpty());            
        } catch (ExcepcionAlmacen e) {
            System.err.println("Problema inesperado");
        }
    }

    public void ConProveedor() {
        System.out.println("In state: con proveedor");
        try {
            Assert.assertFalse(GrapAux.getAlamcen().obtenerProveedor() == null);
            Assert.assertFalse(GrapAux.getAlamcen().buscar(ausente.obtenerTitulo()).isEmpty());
        } catch (ExcepcionAlmacen e) {
            System.err.println("Problema inesperado");
        }
    }
    
    private Almacen almacen;
    private Almacen proveedor;
    private Contenido presente;
    private Contenido ausente;
}
