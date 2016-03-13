
import contenido.ArchivoAudio;
import contenido.ArchivoSimple;
import contenido.Contenido;
import contenido.Promocion;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import org.junit.Test;
import static org.junit.Assert.*;

public class PromocionTest {

    private Promocion promocion = new Promocion(new ArchivoAudio("titulo", "URLAudio", 5, "genero"), "URLPromocion");

    @Test
    public void obtenerTituloTest() {
        assertEquals(promocion.obtenerTitulo(), "titulo con promoción");
    }

    @Test
    public void obtenerDuracionTest() {
        assertEquals(promocion.obtenerDuracion(), 20);
    }

    @Test
    public void obtenerListaReproduccion() {
        Collection<String> listaReproduccion = promocion.obtenerListaReproduccion();
        Iterator it = listaReproduccion.iterator();
        assertEquals(listaReproduccion.size(), 2);
        assertEquals(it.next(), "URLAudio");
        assertEquals(it.next(), "URLPromocion");
    }

}
