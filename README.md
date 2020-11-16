# E-Nano2020 README

Esta es la documentación del proyecto del 2020 de nombre de E-Nano2020 el cual es una aplicación web a la que se le ingresa código en formato E-Nano el cual es transpilado a Java para luego ser compilado; luego usando el nombre del archivo se puede ejecutar el `main` de dicho código.

Se recomienda al lector leer la documentación completamente antes de ejecutar el programa.

## Pre-requisitos

Para poder ejecutar este proyecto se requiere tener lo siguiente instalado en el computador (preferiblemente la última versión):

1. **Java 14**
2. **Gradle**
3. **SWI-Prolog**
4. Un navegador, preferiblemente **Google Chrome**.

## Ejecución del proyecto

### Compilación y ejecución de los servidores

Para ejecutar el proyecto se debe ejecutar el archivo `compile-execute.bat` el cual se encuentra en la carpeta raíz del proyecto a como se puede ver a continuación. Este archivo ejecuta otros tres scripts distintos en diferentes ventanas los cuales compilan y ejecutan cada uno de los servidores del proyecto, los cuales son:

- Servidor de router para compilar código Java y extraer información de la base de datos.

- Servidor web estático que se encarga de servir los archivos estáticos.

- Servidor de Prolog el cual se encarga de transpilar el código E-Nano a Prolog.

![image](https://user-images.githubusercontent.com/37723586/99212218-24235000-2790-11eb-9d88-d87a0405a00a.png)

Una vez los tres servidores se compilaron y ejecutar satisfactoriamente, a como se puede ver en la siguiente imagen se puede proceder a ejecutar la aplicación web en el navegador ingresando la url del servidor estático, la cual por defecto es http://localhost:8088/, en el navegador.

![image](https://user-images.githubusercontent.com/37723586/99212488-c2afb100-2790-11eb-9f0f-4ffe04494856.png)

### Ingreso a la aplicación web

Una vez se ingresó la url del servidor estático en el navegador se va a ver una pantalla como la siguiente:

![image](https://user-images.githubusercontent.com/37723586/99212593-0e625a80-2791-11eb-89c7-b785042756e1.png)

#### Panel de código

En el panel que está arriba a la derecha se puede ingresar código E-Nano el cual al apretar el botón que dice **Compile** se va a transpilar a código de Java para luego ser compilado y guardado en el servidor _Router_. Los errores o mensajes de compilación o transpilación se pueden ver en el panel de mensajes.

Este panel se puede limpiar con el botón que está justo debajo de este que dice **Clear**.

Adicionalmente, el código de Java transpilado se puede ver en la consola del servidor **Router** una vez que se envió la petición de compilación a este servidor.

#### Panel de mensajes

En este panel, el cual se encuentra arriba a la izquierda, se pueden ver mensajes de éxito o error de compilación y/o transpilación una vez que algún código fue compilado. Los mensajes se muestran en código de colores según su tipo:

- Los mensajes **rojos** son de **error**, significan que la compilación/transpilación no se pudo llevar con éxito.

- Los mensajes **verdes** son de **éxito**, significan que la compilación se llevo a cabo con éxito.

- Los mensajes **amarillos** son de **advertencia**, sirven para llamar la atención del usuario sobre algún detalle peculiar que deben tener presente.

- Los mensajes **blancos** son de **anotación**, significan algo que el usuario puede querer saber pero no necesariamente algo que deban arreglar.

#### Campo de nombre del archivo

Este campo se pude ver justo **debajo** del **panel de código** y sirve para ingresar el nombre que se desea que el archivo tenga al ser compilado y transpilado, esto afecta el nombre de la clase durante la compilación y el identificador con el que se va a guardar el archivo ejecutable en el servidor.

Un nombre válido que se puede ingresar en este campo es un nombre formado por una letra inicial y luego letras o números finalizando con un `.no`. Por ejemplo: `File.no`.

Si este campo tiene un dato erróneo se va a mostrar con un borde rojo y el botón de compilar se va a desactivar.

Si se compila un archivo cuyo nombre fue utilizado antes para otro archivo se va a mostrar una advertencia en el panel de mensajes.

#### Panel de ejecución

Este panel se puede abajo del todo, debajo de los dos paneles de mensajes y código.

En este panel se puede ingresar algunos de los siguientes comandos, y una vez que se presiona la tecla `enter` este comando se va a ejecutar, los comandos son los siguientes:

- `<filename>.main()`: este comando sirve para ejecutar el archivo llamado `<filename>` el cual no debe tener la terminación `.no` y debió ser compilado con anterioridad. Un ejemplo es: `File.main()`.

- `clear`: sirve para limpiar el panel de ejecución.

- `help`: sirve para mostrar un mensaje de ayuda para saber los comandos que se pueden utilizar.

#### Ejemplo de uso del programa

![image](https://user-images.githubusercontent.com/37723586/99213493-6b5f1000-2793-11eb-977f-894c5051a873.png)

## Configuración

Algunos parámetros de los servidores se pueden configurar, esto se hace modificando el archivo `server.properties` dentro de `/resources` del servidor (e.g. `/router/resources/config/server.properties` o `/static-server/resources/config/server.properties`).

### Port

Este parámetro está disponible para el servidor de router y el servidor estático.

**Ejemplo:**

```properties
port=8080
```

Esto haría que el servidor utilice el puerto `8080`.

## Estructura del proyecto

La estructura del proyecto es la siguiente:

```tree
\ [root]
├───.github
│   └───workflows
├───.shell-scripts
├───router
│   ├───app
│   │   └───src
│   │       ├───main
│   │       │   └───java
│   │       │       └───org
│   │       │           └───una
│   │       │               └───app
│   │       └───test
│   │           └───java
│   │               └───org
│   │                   └───una
│   │                       └───app
│   ├───buildSrc
│   │   └───src
│   │       └───main
│   │           └───groovy
│   ├───compiler
│   │   └───src
│   │       └───main
│   │           └───java
│   │               └───org
│   │                   └───una
│   │                       └───compiler
│   ├───gradle
│   │   └───wrapper
│   ├───resources
│   │   └───config
│   └───utilities
│       └───src
│           └───main
│               └───java
│                   └───org
│                       └───una
│                           └───utilities
├───static-server
│   ├───app
│   │   └───src
│   │       ├───main
│   │       │   └───java
│   │       │       └───org
│   │       │           └───una
│   │       │               └───app
│   │       └───test
│   │           └───java
│   │               └───org
│   │                   └───una
│   │                       └───app
│   ├───buildSrc
│   │   └───src
│   │       └───main
│   │           └───groovy
│   ├───gradle
│   │   └───wrapper
│   ├───resources
│   │   ├───config
│   │   └───web
│   │       ├───dist
│   │       │   ├───css
│   │       │   └───js
│   │       ├───public
│   │       └───src
│   │           ├───components
│   │           └───plugins
│   └───utilities
│       └───src
│           └───main
│               └───java
│                   └───org
│                       └───una
│                           └───utilities
└───transpiler
```

Los archivos web están en la carpeta `/static-server/resources/web/dist`; el resto del proyecto se maneja de la forma estándar de Java.

## Extras

### MongoDB

De extras está implementado la extra de traer los datos de **About** de una base de datos _MongoDB_.

---

A continuación se muestra la estructura de las colecciones en _MongoDB_.

![image](https://user-images.githubusercontent.com/37723586/95703412-3e579480-0c0c-11eb-818f-e2708762c803.png)

![image](https://user-images.githubusercontent.com/37723586/95703518-50393780-0c0c-11eb-9280-3d80886fee71.png)

---

### Typescript

Otro extra que está implementado es el de utilizar _Typescript_ para hacer el servidor web, en este caso fue implementado usando *VueCLI* con *Typescript*.

## Créditos

**Grupo de trabajo No.03:**
- David Alberto Guevara Sánchez\
  402450355
- Joy Bonilla Fley\
  402360421
- José Barrantes Araya\
  207600954
- Natalia Solano Azofeifa\
  117290958
- Luis David Villalobos González\
  117540697  

**Horario:** 10am.