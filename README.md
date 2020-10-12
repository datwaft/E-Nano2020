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

**NOTA**: antes de empezar a ejecutar comandos se debe entrar a cualquiera de las dos carpetas de proyecto de _Maven_, las cuales son: _E-Nano2020 Router_ y _E-Nano2020 Static Server_, las cuales contienen un archivo `pom.xml`; esto dependiendo del proyecto que desee ejecutar. **Lo recomendable es ejecutar los dos proyectos a la vez, ya que están intrínsecamente enlazados**. A continuación unos ejemplos:

#### Raíz del repositorio - NO ingresar comandos de _Maven_ ahí.

![image](https://user-images.githubusercontent.com/37723586/94335309-00b20500-ff98-11ea-84c0-cb660f77f779.png)

#### Raíz de los proyectos de Maven - SÍ ingresar comandos de _Maven_ ahí.

#### _E-Nano2020 Router_

![image](https://user-images.githubusercontent.com/37723586/94331775-7c0db980-ff8c-11ea-8237-8442f112bba6.png)

#### _E-Nano2020 Static Server_

![image](https://user-images.githubusercontent.com/37723586/94331785-8af46c00-ff8c-11ea-85cd-74f54244763d.png)

### Configuración

Algunos parámetros de los servidores se pueden configurar, esto se hace modificando el archivo `server.properties` dentro de `/resources` del servidor (e.g. `/E-Nano2020 Router/src/main/resources/server.properties`).

#### Port

**Ejemplo:**

```properties
port=8080
```

Esto haría que el server use el puerto `8080`.

### Compilación

Para compilar se usa el comando:

```shell
mvn package
```

**Nota:** si aparecen _warnings_ es un problema de la última versión de Maven, que tiene problemas con un acceso reflectivo de Google, esto no causa problemas para ejecutar el proyecto satisfactoriamente.

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

---

A continuación un ejemplo de los dos servidores corriendo usando _TMUX_ para hacer las divisiones en la consola.

![image](https://user-images.githubusercontent.com/37723586/95702923-fe43e200-0c0a-11eb-9659-18e291c7d8fc.png)

### Conexión desde el navegador

La url para la conexión debe aparecer en la consola una vez se inició el servidor, pero la por defecto es la siguiente: `http://localhost:8088/`.

Una vez entró a la url le debe salir una pantalla similar a la siguiente:

![image](https://user-images.githubusercontent.com/37723586/95702614-1a934f00-0c0a-11eb-97fe-b249be318e6a.png)

En el recuadro negro de la izquierda se puede introducir el código Java a compilar, y una vez el botón verde **Submit** es apretado en el recuadro negro de la derecha va a parecer si se compiló o no correctamente al igual que los posibles errores de compilación.

---

A continuación unos ejemplos:

![image](https://user-images.githubusercontent.com/37723586/95703022-3cd99c80-0c0b-11eb-8f89-cf0770e0cb51.png)

![image](https://user-images.githubusercontent.com/37723586/95703043-4fec6c80-0c0b-11eb-81cb-552aa7f6503c.png)

---

Los botones de **Clear** sirven para limpiar los datos de cualquiera de los dos recuadros, primero se solicita una confirmación al usuario.

---

A continuación un ejemplo:

![image](https://user-images.githubusercontent.com/37723586/95703088-74e0df80-0c0b-11eb-986c-f5c988ab8744.png)

---

El botón de arriba a la derecha que dice **About** sirve para ver los integrantes del equipo de trabajo, los cuales son pedidos como _JSON_ al servidor luego de que este hace una petición a una base de datos _MongoDB_ para traer los datos, es posible que dure un rato trayendo los datos.

---

A continuación un ejemplo:

![image-20201011214857091](C:\Users\David\AppData\Roaming\Typora\typora-user-images\image-20201011214857091.png)

## Estructura del proyecto

La estructura del proyecto es la siguiente:

```tree
\ [root]
├───E-Nano2020 Router
│   └───src
│       ├───main
│       │   ├───java
│       │   │   └───com
│       │   │       └───group03
│       │   │           ├───compiler
│       │   │           └───utils
│       │   └───resources
│       └───test
│           └───java
│               └───com
│                   └───group03
└───E-Nano2020 Static Server
    └───src
        ├───main
        │   ├───java
        │   │   └───com
        │   │       └───group03
        │   │           └───utils
        │   └───resources
        │       └───web
        │           ├───css
        │           ├───icon
        │           └───js
        └───test
            └───java
                └───com
                    └───group03
```

Los archivos web están en la carpeta `/E-Nano2020 Static Server/src/main/resources/web`; el resto del proyecto se maneja de la forma estándar de Java.

## Extras

De extras se está implementado la extra de traer los datos de **About** de una base de datos MongoDB.

---

A continuación se muestra la estructura de las colecciones en _MongoDB_.

![image](https://user-images.githubusercontent.com/37723586/95703412-3e579480-0c0c-11eb-818f-e2708762c803.png)

![image](https://user-images.githubusercontent.com/37723586/95703518-50393780-0c0c-11eb-9280-3d80886fee71.png)

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
