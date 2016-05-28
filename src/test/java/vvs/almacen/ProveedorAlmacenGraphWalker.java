package vvs.almacen;

import edu.umd.cs.findbugs.annotations.SuppressWarnings;

import vvs.contenido.ArchivoAudio;
import vvs.contenido.Contenido;
import vvs.contenido.ExcepcionContenido;
import vvs.util.GrapAux;

import org.junit.Assert;

import org.graphwalker.core.machine.ExecutionContext;
import org.graphwalker.java.annotation.GraphWalker;

@GraphWalker(value = "random(edge_coverage(50))", start = "iniciar")
//@GraphWalker(value = "random(length(25) or time_duration(10))", start = "iniciar")
public class ProveedorAlmacenGraphWalker extends ExecutionContext implements ProveedorAlmacenModel {

    // TRANSICIONES
    public void iniciar() {
        try {
            Almacen real = new AlmacenReal("Universal Music Group");
            Almacen restringido = new AlmacenRestringido(real, 3, 1);
            almacen = new ProveedorAlmacen(restringido, null);
            proveedor = new AlmacenReal("Sony BMG Music");
            presente = new ArchivoAudio("Amy Winehouse: Rehab", "http://servidor/winehouse/back2black/1", 215, "Soul");
            ausente = new ArchivoAudio("Avril Lavigne: Girlfriend", "http://servidor/alavigne/bestdamnthing/1", 216, "Punk pop");
            proveedor.agregarContenido(ausente);
            /*INIT */
            GrapAux.setAlmacen(almacen);            
            GrapAux.resetNumBusq();
            setAttribute("numCont",GrapAux.getNumCont());
            
        } catch (ExcepcionContenido c) {
            System.err.println("Problema al crear contenidos");
        } catch (ExcepcionAlmacen c) {
            System.err.println("Problema al crear almacenes");
        }
    }

    public void establecerProveedor() {
        GrapAux.getAlmacen().establecerProveedor(proveedor);
    }

    public void eliminarProveedor() {
        GrapAux.getAlmacen().establecerProveedor(null);
    } 
    
    public void SinProveedor() {
        try {
            Assert.assertTrue(GrapAux.getAlmacen().obtenerProveedor() == null);
            Assert.assertTrue(GrapAux.getAlmacen().buscar(ausente.obtenerTitulo()).isEmpty());            
        } catch (ExcepcionAlmacen e) {
            System.err.println("Problema inesperado");
        }
    }

    public void ConProveedor() {
        try {
            Assert.assertFalse(GrapAux.getAlmacen().obtenerProveedor() == null);
            Assert.assertFalse(GrapAux.getAlmacen().buscar(ausente.obtenerTitulo()).isEmpty());
        } catch (ExcepcionAlmacen e) {
            System.err.println("Problema inesperado");
        }
    }
    
    private Almacen almacen;
    private Almacen proveedor;
    private Contenido presente;
    private Contenido ausente;
}
