package contenido;

import org.junit.Test;
import static org.junit.Assert.*;

import java.util.Collection;

public class ArchivoAudioTest {

    /* Casos de que prueban la funcionalidad de busqueda sobre un contenido de tipo ArchivoAudio */

    @Test
    public void buscarPorVariasPalabrasTest() {
	Contenido lavigne1 = new ArchivoAudio("Avril Lavigne: Girlfriend", "http://servidor/alavigne/bestdamnthing/1", 216, "Punk pop");
	Collection <Contenido> resultado = lavigne1.buscar("Avril Lavigne");
	assertEquals(1, resultado.size());
    }
	
    @Test
    public void buscarPorVariasPalabrasDesordenadasTest() {
	Contenido lavigne1 = new ArchivoAudio("Avril Lavigne: Girlfriend", "http://servidor/alavigne/bestdamnthing/1", 216, "Punk pop");
	Collection <Contenido> resultado = lavigne1.buscar("Lavigne Avril");
	assertEquals(1, resultado.size());
    }
    
    @Test
    public void buscarPorUnaPalabraExactaTest() {
	Contenido lavigne1 = new ArchivoAudio("Avril Lavigne: Girlfriend", "http://servidor/alavigne/bestdamnthing/1", 216, "Punk pop");
	Collection <Contenido> resultado = lavigne1.buscar("Lavigne");
	assertEquals(1, resultado.size());
    }
    
    @Test
    public void buscarPorUnaPalabraConMayusculasMinusculasTest() {
	Contenido lavigne1 = new ArchivoAudio("Avril Lavigne: Girlfriend", "http://servidor/alavigne/bestdamnthing/1", 216, "Punk pop");
	Collection <Contenido> resultado = lavigne1.buscar("LavIgnE");
	assertEquals(1, resultado.size());
    }
    
    @Test
    public void buscarPorUnaPalabraConEspaciosAntesDespuesTest(){
	Contenido lavigne1 = new ArchivoAudio("Avril Lavigne: Girlfriend", "http://servidor/alavigne/bestdamnthing/1", 216, "Punk pop");
	Collection <Contenido> resultado = lavigne1.buscar(" Lavigne ");
	assertEquals(1, resultado.size());
    }
    
}
