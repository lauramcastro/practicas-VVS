package vvs.contenido;

import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import org.junit.Assert;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
/**
 *
 * @author Elías
 */
public class ComplementoArchivoTest {
    private ArchivoAudio archivo;
    private Promocion promocion;
    private Promocion promocion2;
    
    @Before
	public void initialize() throws ExcepcionContenido{
    	archivo = new ArchivoAudio("titulo", "URLAudio", 5, "genero");
        promocion = new Promocion(archivo, "URLPromocion");
        promocion2 = new Promocion(archivo, "URLPromocion");
	}
    
    
    @Test
    public void buscarTest(){
        Collection<Contenido> contenidos = promocion.buscar("titulo");
        Iterator it = contenidos.iterator();
        assertEquals(it.next(), promocion);
    }
    
    @Test(expected=RuntimeException.class)
    public void agregarTest(){
        promocion.agregar(null, null);
    }
    
    @Test(expected=RuntimeException.class)
    public void eliminarTest(){
        promocion.eliminar(null);
    }
    
    @Test
    public void obtenerGeneroTest(){
        assertEquals(promocion.obtenerGenero(),"genero");
    }
    
    @Test(expected=RuntimeException.class)
    public void recuperarTest(){
        promocion.recuperar(0);
    }
    
    @Test
    public void obtenerPadreTest(){
        promocion.establecerPadre(archivo);
        Assert.assertTrue(promocion.obtenerPadre().equals(archivo));
    }
    @Test
    public void hashCodeTest(){
        Assert.assertTrue(promocion.hashCode() == promocion2.hashCode());
    }
    
    
    @Test
    public void equalsTest(){
        assertTrue(promocion.equals(archivo));
    }
    
    /*@Test
    public void establecerPadreTest(){
        ((ContenidoAbstracto)promocion).
        assertEquals(promocion.obtenerPadre(),);
                
    }*/
    
    /*@Test
    public void mantenerIntegridadTest(){
    
    }*/
}
