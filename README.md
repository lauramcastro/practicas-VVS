# Prácticas VVS

[![Build Status](https://travis-ci.org/lauramcastro/practicas-VVS.svg?branch=master)](https://travis-ci.org/lauramcastro/practicas-VVS)

Repositorio de prácticas de la asignatura Validación y Verificación del Software (2015/2016, 2º cuatrimestre).

## Descripción del contenido del repositorio

Se presenta la implementación de un sistema dedicado al almacenamiento y distribución mediante streaming de contenidos multimedia (SpotiVVSy), representada en el diagrama de clases:

![diagrama de clases SpotiVVSy](https://github.com/lauramcastro/practicas-VVS/blob/master/doc/Diagrama%20de%20clases.png "Diagrama de clases (SpotiVVSy)")

Los elementos fundamentales del sistema son:

* El *cliente* del sistema, que maneja un conjunto de almacenes (`gestionarAlmacen`) y un conjunto de contenidos (`gestionarContenido`), de forma que posibilita la asignación de contenidos a almacenes, la búsqueda de contenidos disponibles en almacenes y la manipulación de la cadena de aprovisionamiento entre estos almacenes.
* Los *contenidos* disponen de un método `obtenerTitulo` que devuelve una cadena de texto con el título del contenido multimedia, así como los métodos `obtenerDuracion` y `obtenerGenero` que proporcionan, respectivamente, la duración en segundos y la clasificación de un contenido. Asimismo, cada contenido también dispone de un método `obtenerListaReproduccion` que proporciona la lista ordenada de URLs (cadenas de texto) que deberían ser solicitadas al servidor para la reproducción del contenido. Por último, los contenidos proporcionan también un método `buscar` que devuelve una lista con los contenidos (y
subcontenidos, en caso de haberlos) que contengan en su título una subcadena especificada. Paralelamente, la organización de los contenidos se ve simplificada gracias a su posible estructuración en forma de colecciones. Una colección puede a su vez incluir otras colecciones (y así sucesivamente), pero habrá de tenerse en cuenta que un contenido (ya sea un archivo individualo un grupo de ellos) podrá pertenecer a una única colección. Como es lógico, los contenidos dentro de una colección guardarán un orden entre ellos (por ejemplo, el orden de las pistas -archivos- dentro de un álbum -colección-).
* Por otro lado, cada *almacén* de contenidos gestiona un subconjunto de los contenidos (`agregarContenido`, `eliminarContenido`), permitiendo la realización de búsquedas (`buscar`) entre su selección de contenidos. Para facilitar la distribución de los contenidos entre los almacenes, éstos pueden organizarse jerárquicamente de forma que un almacén puede actuar como proveedor de otro. De esta manera, en caso de que una búsqueda no arroje ningún resultado, el almacén podrá consultar a su proveedor. Para posibilitar la gestión de dicha jerarquía cada almacén dispone de los métodos `obtenerProveedor` y `establecerProveedor`. Por último, de cara a la interacción con el cliente, se proporcionan además los métodos
`obtenerNombre` y `obtenerContenidos` que suministran el nombre del almacén y el mencionado conjunto de contenidos gestionados por éste.

El repositorio contiene los ficheros `.java` correspondientes a la inicialización (`Main.java`) y a las interfaces contenido,
almacén y cliente (`Contenido.java`, `Almacen.java` y `Cliente.java`), fachadas de los subsistemas correspondientes, junto con sus implementaciones reflejadas en el diagrama antes mencionado. También se encuentra una implementación JAVA/Swing de la interfaz cliente en forma de un conjunto de clases ya compiladas y empaquetadas en un JAVA Archive (`ui.jar`). **No se incluye ninguna prueba**.

La compilación se lleva a cabo situándose en el directorio base del proyecto y ejecutando el comando `ant` (Apache Ant). Del mismo modo, los comandos `ant clean` y `ant javadoc` pueden utilizarse, respectivamente, para eliminar copias de seguridad y archivos compilados, y para generar la documentación Javadoc. Una vez compilado el proyecto, el comando `ant run` facilitará la ejecución del mencionado cliente gráfico.

### Detalles de la especificación

En esta sección iremos completando la especificación con la información que corresponda, cuando vayan detectándose imprecisiones o inconsistencias.

* Los almacenes no almacenarán contenidos repetidos, considerándose que todo contenido es igual a sí mismo, con o sin promociones o extras asociados (puntualización derivada de la [issue #7](https://github.com/lauramcastro/practicas-VVS/issues/7)).
* Como medida de aseguramiento de la disponibilidad del sistema, protección contra ataques de DDoS, etc., debe ser posible restringir las búsquedas realizadas en cualquier almacén dado a un número determinado por unidad de tiempo. Superada la cantidad de búsquedas especificada, un almacén restringido debe rechazar más peticiones al método buscar hasta que no transcurra el tiempo determinado (detalle derivado de la [issue #11](https://github.com/lauramcastro/practicas-VVS/issues/11)).
* Los almacenes pueden configurarse de manera que se registre por salida estándar la información de las peticiones de búsqueda enviadas (detalle derivado de la [issue #15](https://github.com/lauramcastro/practicas-VVS/issues/15)).
