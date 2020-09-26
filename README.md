# E-Nano2020 README

## Pre-requisitos

Para poder ejecutar este proyecto se requiere tener lo siguiente instalado en el computador (preferiblemente la última versión):

1. **Java 14**

2. **Maven**

## Compilación y ejecución

### TLDR

```shell
mvn package
mvn exec:java -Dexec.mainClass="com.group03.App"
```

---

**NOTA**: antes de empezar a ejecutar comandos se debe entrar a cualquiera de las dos carpetas de proyecto de _Maven_, las cuales son: _E-Nano2020 Router_ y _E-Nano2020 Static Server_, las cuales contiene un archivo `pom.xml`; esto dependiendo del proyecto que desee ejectuar. A continuación unos ejemplos:

#### Raíz del repositorio - NO ingresar comandos de _Maven_ ahí.

![image](https://user-images.githubusercontent.com/37723586/93397727-69caa780-f837-11ea-9151-19fccf2a6396.png)

#### Raíz de los proyectos de Maven - SÍ ingresar comandos de _Maven_ ahí.

#### _E-Nano2020 Router_

![image](https://user-images.githubusercontent.com/37723586/94331775-7c0db980-ff8c-11ea-8237-8442f112bba6.png)

#### _E-Nano2020 Static Server_

![image](https://user-images.githubusercontent.com/37723586/94331785-8af46c00-ff8c-11ea-85cd-74f54244763d.png)

### Compilación

Para compilar se usa el comando:

```shell
mvn package
```

**Nota:** si aparecen warnings es un problema de la última versión de maven, que tiene problemas con un acceso reflectivo de google, no da problemas para ejecutar el proyecto.

El proyecto terminó de compilarse una vez que aparece un texto como este:

```shell
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
[INFO] Total time:  11.073 s
[INFO] Finished at: 2020-09-16T15:50:33-06:00
[INFO] ------------------------------------------------------------------------
```

### Ejecución

Para ejecutar el servidor se utiliza el comando:

```shell
mvn exec:java -Dexec.mainClass="com.group03.App"
```

Una vez se terminó de iniciar el servidor va a aparecer una pantalla similar a la siguiente:

![image](https://user-images.githubusercontent.com/37723586/93396337-b660b380-f834-11ea-9277-78b2bb011a9b.png)

**Nota:** Una vez que el servidor se está ejecutando no lo cierre hasta que haya terminado completamente de usar el proyecto, posiblemente con `CTRL+C` o `CTRL+Z` - dependiendo de su sistema operativo.

### Conexión desde el navegador

La url para la conexión debe aparecer en la consola una vez se inició el servidor, pero la por defecto es la siguiente: `http://localhost:8088/`.

Una vez entró a la url le debe salir una pantalla similar a la siguiente:

![image](https://user-images.githubusercontent.com/37723586/94331792-a1022c80-ff8c-11ea-84ea-960a4e2222a3.png)

~~En el recuadro negro de la derecha puede introducir el código E-Nano a ejecutar, y en el derecha se producirá el resultado de la ejecución del código después de usted apretar el botón verde **Submit**.~~

En el recuadro negro a la derecha, por esta ser una versión incompleta, al introducir un texto y luego darle a **Submit** aparecerá un mensaje haciendo _echo_ del mensaje que usted escribió como garantía de que el servidor respondió a su petición y está funcionando correctamente.

Los botones de **Clean** sirven para limpiar los datos de cualquiera de los dos lados.

El botón de arriba a la derecha que dice **About** sirve para ver los integrantes del equipo de trabajo, los cuales son pedidos como _JSON_ al servidor.

## Estructura del proyecto

La estructura del proyecto es la siguiente:

```tree
.[root]
└───E-Nano2020
    └───src
        ├───main
        │   ├───java
        │   │   └───com
        │   │       └───group03
        │   │           └───utils
        │   └───resources
        │       ├───css
        │       ├───icon
        │       ├───info
        │       └───js
        └───test
          └───java
            └───com
              └───group03
```

Los archivos web están en la carpeta `/E-Nano2020/src/main/resources`; el resto del proyecto se maneja de la forma estándar de Java.

## Créditos

**Grupo de trabajo No.03:**
- David Alberto Guevara Sánchez\
  402450355
- Joy Bonilla Fley\
  402360421
- Jose Barrantes Araya\
  207600954
- Natalia Solano Azofeifa\
  117290958
- Luis David Villalobos Gonzalez\
  117540697  
  
**Horario:** 10am.
