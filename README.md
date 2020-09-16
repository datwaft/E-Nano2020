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

**Nota:** Una vez que el servidor se está ejecutando no lo cierre hasta que haya terminado completamente de usar el proyecto, posiblemente con CTRL+C o CTRL+Z - dependiendo de su sistema operativo.

### Conexión desde el navegador

La url para la conexión debe aparecer en la consola una vez se inició el servidor, pero la por defecto es la siguiente: `http://localhost:8088/`.

