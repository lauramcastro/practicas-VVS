package vvs.almacen;

import net.java.quickcheck.Generator;
import net.java.quickcheck.generator.PrimitiveGenerators;
import net.java.quickcheck.generator.iterable.Iterables;
import org.mockito.Mockito;
import vvs.contenido.ArchivoAudio;
import vvs.contenido.Contenido;
import vvs.contenido.ExcepcionContenido;

/**
 *
 * @author hmia
 */
public class GeneradorContenido implements Generator<Contenido> {

    Contenido contenidoPrincipal = null;
    private Generator<String> s = PrimitiveGenerators.strings();
    private Generator<Integer> i = PrimitiveGenerators.integers(0); // valor mínimo

    @Override
    public Contenido next() {
        try {
            return new ArchivoAudio(s.next(), s.next(), i.next(), s.next());
        } catch (ExcepcionContenido e) {
            System.err.println("No se ha podido generar el archivo de audio" + e.getMessage());
            return null;
        }
    }
}
