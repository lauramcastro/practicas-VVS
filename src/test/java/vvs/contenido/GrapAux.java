package vvs.contenido;

import vvs.almacen.Almacen;

public class GrapAux {
	
	private static Almacen almacen;
	private static int numCont =0;
	private static int numBusq = 3;
	private static int timeElapsed=0;


	private static final GrapAux INSTANCE = new GrapAux();

    // El constructor privado no permite que se genere un constructor por defecto.
    // (con mismo modificador de acceso que la definición de la clase) 
    private GrapAux() {}

    public static GrapAux getInstance() {
        return INSTANCE;
    }

    public static void setAlmacen(Almacen alm){
    	almacen = alm;
    }
    
    public static Almacen getAlamcen(){
    	return almacen;
    }
    
    public static void addNumCont(){
    	numCont++;
    }
    
    public static void removeNumCont(){
    	numCont--;
    }
    
    public static int getNumCont(){
    	return numCont;
    }
    
    public static void resetNumCont(){
    	numCont =0;
    }
    
    public static int getNumBusq(){
    	return numBusq;
    }
    
    public static void lessNumBusq(){
    	numBusq--;
    }
    
    public static void resetNumBusq(){
    	numBusq = 3;
    }
    
    public static int getTimeElapsed(){
    	return timeElapsed;
    }
    
    public static void addToTimeElapsed(int time){
    	timeElapsed += time;
    }
    
    public static void resetTimeElapsed(){
    	timeElapsed = 0;
    }
}
