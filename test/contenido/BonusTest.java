package contenido;

import org.junit.Test;
import static org.junit.Assert.*;

import java.util.Collection;
import java.util.Iterator;

public class BonusTest {

    private Bonus bonus = new Bonus(new ArchivoAudio("titulo", "URLAudio", 5, "genero"), new ArchivoAudio("titulo2", "URLAudio2", 5, "genero2"));

    @Test
    public void obtenerTituloTest() {
        assertEquals(bonus.obtenerTitulo(), "titulo con bonus (titulo2)");
    }

    @Test
    public void obtenerDuracionTest() {
        assertEquals(bonus.obtenerDuracion(), 10);
    }

    @Test
    public void obtenerListaReproduccion() {
        Collection<String> listaReproduccion = bonus.obtenerListaReproduccion();
        Iterator it = listaReproduccion.iterator();
        assertEquals(listaReproduccion.size(), 2);
        assertEquals(it.next(), "URLAudio");
        assertEquals(it.next(), "URLAudio2");
    }
    
}
