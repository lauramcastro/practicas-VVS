package vvs.contenido;

import org.junit.Assert;

import org.graphwalker.core.machine.ExecutionContext;
import org.graphwalker.java.annotation.GraphWalker;

@GraphWalker(value = "random(edge_coverage(100))", start = "iniciar")
public class ColeccionGraphWalker extends ExecutionContext implements ColeccionModel {
	
	private Coleccion coleccion;
	private Contenido muse1, muse2;

	//TRANSICIONES
	public void iniciar() {
		try {
			coleccion = new Coleccion("Muse");
			muse1 = new ArchivoAudio("Muse: Knights of Cydonia", "http://servidor/muse/1", 215, "Rock");
			muse2 = new ArchivoAudio("Muse: Sing For Absolution", "http://servidor/muse/2", 312, "Rock");
		} catch (ExcepcionContenido c) {
			System.err.println("Problema al crear contenidos");
		}
	}

	public void agregar(){
		if(coleccion.obtenerDuracion() == 0) {
			coleccion.agregar(muse1, null);
		} else {
			coleccion.agregar(muse2, muse1);
		}
	}

	public void eliminar(){
		if(coleccion.obtenerDuracion() != 0) {
			coleccion.eliminar(muse2);
		}
	}

	// ESTADOS
	public void ColeccionVacia() {
		Assert.assertTrue(coleccion.obtenerDuracion() == 0);
	}

	public void ColeccionConContenido() {
		Assert.assertFalse(coleccion.obtenerDuracion() == 0);
	}

}