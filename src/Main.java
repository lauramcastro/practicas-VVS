import almacen.Almacen;
import almacen.AlmacenReal;
import almacen.AlmacenRestringido;
import almacen.RegistroAlmacen;
import almacen.ProveedorAlmacen;
import almacen.ExcepcionAlmacen;

import cliente.Cliente;
import cliente.ClienteImp;

import contenido.Contenido;
import contenido.ArchivoAudio;
import contenido.Bonus;
import contenido.Coleccion;
import contenido.Promocion;

/**
 * Clase de inicialización para la ejecución de la práctica. Se
 * ocupará de crear una instancia de la la interfaz, así como las
 * instancias de los diferentes elementos que integran el diseño del
 * sistema, con el fin de poder comprobar el correcto comportamiento
 * de los escenarios propuestos en el enunciado.
 *
 * @author Carlos Abalde, Laura Castro, Javier París
 * @version 1.0
 */

public class Main {

    /**
     * Método principal que se invocará al ejecutar la aplicación.
     *
     * @param argv lista de argumentos
     */
    public static void main(String argv[]) {

        // Se crea una instancia de ClienteImp, una implementación de
        // la interfaz Cliente.
        Cliente spotiVVSy = new ClienteImp();

        // Se crean algunos almacenes
        Almacen sony = new AlmacenRestringido(new AlmacenReal("Sony BMG Music"), 5, 2);
        Almacen emi = new RegistroAlmacen(new AlmacenReal("EMI"));
        Almacen universal = new ProveedorAlmacen(new AlmacenReal("Universal Music Group"), null);

        // Se añaden los almacenes al cliente
        spotiVVSy.gestionarAlmacen(sony);
        spotiVVSy.gestionarAlmacen(emi);
        spotiVVSy.gestionarAlmacen(universal);

        // Se crean varios contenidos
        Contenido lavigne1 = new ArchivoAudio("Avril Lavigne: Girlfriend", "http://servidor/alavigne/bestdamnthing/1", 216, "Punk pop");
        Contenido lavigne2 = new ArchivoAudio("Avril Lavigne: I can do better", "http://servidor/alavigne/bestdamnthing/2", 197, "Punk pop");
        Contenido lavigne3 = new ArchivoAudio("Avril Lavigne: The best damn thing", "http://servidor/alavigne/bestdamnthing/4", 190, "Punk pop");
        Contenido coldplay1 = new ArchivoAudio("Coldplay: Speed of Sound", "http://servidor/coldplay/xy/7", 288, "Rock alternativo");
        Contenido coldplay2 = new ArchivoAudio("Coldplay: Shiver", "http://servidor/coldplay/parachutes/2", 299, "Rock alternativo");
        Contenido winehouse = new ArchivoAudio("Amy Winehouse: Rehab", "http://servidor/winehouse/back2black/1", 215, "Soul");

        Contenido lavigne = new Coleccion("Avril Lavigne: Greatest Hits");
        lavigne.agregar(lavigne1, null);
        lavigne.agregar(lavigne2, lavigne1);
        lavigne.agregar(lavigne3, lavigne2);
        Contenido coldplay = new Coleccion("Coldplay: Singles");
        coldplay.agregar(coldplay1, null);
        coldplay.agregar(coldplay2, coldplay1);

        Contenido wineandcold = new Bonus((ArchivoAudio) winehouse, coldplay);
        Contenido winepromo = new Promocion((ArchivoAudio) winehouse, "http://servidor/publicidad/gadis/vivamos-como-galegos/1");

        // Se añaden los contenidos creados al cliente
        spotiVVSy.gestionarContenido(lavigne);
        spotiVVSy.gestionarContenido(lavigne1);
        spotiVVSy.gestionarContenido(lavigne2);
        spotiVVSy.gestionarContenido(lavigne3);
        spotiVVSy.gestionarContenido(coldplay);
        spotiVVSy.gestionarContenido(coldplay1);
        spotiVVSy.gestionarContenido(coldplay2);
        spotiVVSy.gestionarContenido(winehouse);
        spotiVVSy.gestionarContenido(wineandcold);
        spotiVVSy.gestionarContenido(winepromo);

        // Se añaden los contenidos a los almacenes
        try {
            sony.agregarContenido(lavigne);
            emi.agregarContenido(coldplay);
            universal.agregarContenido(winehouse);
            // universal.agregarContenido(wineandcold); // almacen.ExcepcionAlmacen: Contenido duplicado.
            // universal.agregarContenido(winepromo); // almacen.ExcepcionAlmacen: Contenido duplicado.
        } catch (ExcepcionAlmacen e) {
            e.printStackTrace();
        }
    }

}
