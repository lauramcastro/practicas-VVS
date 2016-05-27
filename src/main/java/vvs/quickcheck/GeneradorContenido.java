/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package vvs.quickcheck;

import java.util.Random;
import net.java.quickcheck.Generator;
import net.java.quickcheck.generator.PrimitiveGenerators;
import vvs.contenido.ArchivoAudio;
import vvs.contenido.Bonus;
import vvs.contenido.Contenido;
import vvs.contenido.ExcepcionContenido;
import vvs.contenido.Promocion;

/**
 *
 * @author Elías
 */
public class GeneradorContenido implements Generator<Contenido> {

    private Generator<String> s = PrimitiveGenerators.strings();
    private Generator<Integer> i = PrimitiveGenerators.integers(0); // valor mínimo    
    Random generator = new Random();    

    @Override
    public Contenido next() {
        try {
            ArchivoAudio archivo = new ArchivoAudio(s.next(), s.next(), i.next(), s.next());
            //contador = i.next();
            int contador = generator.nextInt(2);
            if (contador == 0) {
                //Bonus
                return new Bonus(archivo, this.next());
            } else if (contador == 1) {
                //Promocion
                return new Promocion(archivo, s.next());
            } else {
                //Archivo Audio
                return archivo;
            }
        } catch (ExcepcionContenido e) {
            System.err
                    .println("No se ha podido generar el contenido"
                            + e.getMessage());
            return null;
        }
    }
}
