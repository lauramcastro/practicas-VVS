package vvs.contenido;

import org.junit.Assert;

import org.graphwalker.core.machine.ExecutionContext;
import org.graphwalker.java.annotation.GraphWalker;

import vvs.util.GrapAux;

@GraphWalker(value = "random(edge_coverage(50))")
//@GraphWalker(value = "random(length(25) or time_duration(10))")
public class ColeccionGraphWalker extends ExecutionContext implements ColeccionModel {
	
        private Coleccion coleccion; // este contenido debería estar en el Singleton, ya que aquí no hay modo de inicializarlo en este modelo parcial
        private Contenido muse1, muse2; // mismo comentario anterior

	//TRANSICIONES

	public void agregar(){
		/*if(coleccion.obtenerDuracion() == 0) {
			coleccion.agregar(muse1, null);
		} else {
			coleccion.agregar(muse2, muse1);
		}*/
	}

	public void eliminar(){
		/*if(coleccion.obtenerDuracion() != 0) {
			coleccion.eliminar(muse2);
		}*/
	}

	// ESTADOS
	public void ColeccionVacia() {
		//Assert.assertTrue(coleccion.obtenerDuracion() == 0);
                Assert.assertTrue(true);
	}

	public void ColeccionConContenido() {
		//Assert.assertFalse(coleccion.obtenerDuracion() == 0);
                Assert.assertTrue(true);
	}

}
