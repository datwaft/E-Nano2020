# Especificación General de un Proyecto de Programación: Un Lenguaje Prototipo y su Entorno Web de Evaluación

Acrónimo de referencia rápido para efectos del curso: _E-Nano2020_.

## Introducción

Este documento especifica los elementos de partida de la definición de un proyecto programado sobre paradigmas lenguajes de programación y compiladores en el marco y alcances del curso. El producto final esperado es una aplicación Web demostrativa, la que permite escribir y evaluar código de un lenguaje prototipo cuyas características y diseño serán especificadas incrementalmente durante el curso. A ese lenguaje lo llamaremos E-Nano.

## Puntos de Referencia

Para darse un mejor idea inicial de lo que se busca, sería en principio una idea similar a este sitio (https://repl.it/languages/java10). Otro es este (https://www.programiz.com/cpp-programming/online-compiler/). Pero para un sólo lenguaje propio y más customizado (denominado E-Nano). El factor Prolog también será un diferenciador muy importante.

Una referencia que he usado con mucho interés para construir un demo de la parte embebida liviana y que les presentaré pronto es este (https://github.com/javaterminal/tryjshell). Esta última corresponde a un JShell (https://tryjshell.org/) corriendo en Web. Este se basa en NanoHttpd. De ahí el nombre E-Nano.

Este sitio puede también ser un muy buen punto de referencia en cuanto a qué significa transpilar (https://es6console.com/).

## Requerimientos Generales

La aplicación se desarrollará en dos grandes etapas. En la primera se usa un cliente HTML/JS/CSS estándar y un backend en Java y Prolog. Se usará un servidor embebido liviano. Se dispone de libertad para usar librerías en el cliente, a aprobar primero por el profesor que funge como product manager y principal stake holder.

**Esta especificación asume que el proyecto evoluciona ágilmente sin que desde el inicio se conozcan totalmente todos los alcances completamente. Es posible entonces que estos cambien según el avance del proyecto y del curso.**

Se debe desarrollar:

1. Una aplicación Web estilo SPA (single-page-application) y un API de servicios web que le permite a un programador editar, compilar y evaluar código en el lenguaje prototipo E-Nano. Dicho lenguaje será especificado en su debido momento en adenda a este SPEC. La edición ocurre en el cliente (con servicios de apoyo del backend), la compilación y evaluación sólo en el backend. Los resultados de la compilación y evaluación son mostrados en el cliente una vez que se produzcan. El backend trabaja por servicios bien definidos para acceder a un soporte a la edición de texto, la compilación y evaluación; los servicios entregan sólo datos no vista. Una sóla página en HTML de index o home es la única vista que el backend genera la primera vez que se inicia la aplicación. Lo demás de vista es generado todo en el cliente.
2. Un transpilador del lenguaje que transforma el E-Nano en un subset del lenguaje Java (para un ejemplo de un transpilador de ES6 online, ver acá). El transpilador incluye un analizador sintáctico (lexer y parser), las estructuras de datos de representación de código y un generador de código Java. Podría, si el tiempo lo permite, requerirse un analizador semántico inicial. Se dejará previsto.
3. Un proceso de evaluación del lenguaje Java generado por el transpilador, lo compila y construye lo necesario para poder evaluarlo y lo evalúa. Esto podrá requerir de un runtime de apoyo dependiendo de las características de E-Nano.
4. Los servicios necesarios para darle funcionalidad al cliente en cuanto apoyo a la edición, compilación y evaluación. Para efectos de ejercitar distintos temas del curso, el servicio de análisis sintáctico y pre-generación de código Java estará en un servidor en Prolog usando websockets. La comunicación entre los servicios y el ciente no dependerá de esa tercera capa en Prolog. La misma sería sustituida en un momento posterior por una equivalente en Java y ANTLR4.
5. La interacción necesaria para mantener la comunicación entre el cliente y los servicios mencionados.
6. Un proceso de build de la aplicación automatizado que permita construirla de manera expedita y frecuente que incluya unit-testing y empacamiento, fuera de un IDE.
7. Casos de prueba unitarios funcionales para el transpilador, el proceso de evaluación, el cliente Para esto se debe permitir en el cliente cargar archivos fuente del lenguaje ya preparados que prueban los servicios de manera más fluida. Para los servicios de Web se pueden hacer casos de prueba fuera del browser (en los demos mencionados adelante se explicará en su momento más).
8. Una estructura de proyecto apropiada para la comprensión de la aplicación y su build automático, empacamiento y distribución. Al usar un servidor embebido la parte de Java se debe entregar en un JAR no en WAR.
9. Opcionalmente: una capa de persistencia que le permita al usuario almacenar código en el backend. Si se incluye debe ser una capa separada de la lógica de transpilación-evaluación.
10. Una funcionalidad de edición de texto en el cliente. Esta es recomendable basarla mejor en alguna librería de cliente, cuya escogencia es libre.

## Demos de Apoyo

En su debido momento se entregarán por parte del profesor demos y se explicarán, para que sirvan como puntos de partida para el desarrollo de este proyecto y su comprensión.

## Etapas

Se divide el entregable en dos grandes etapas.
- La primera etapa que cubra más el cliente y lo básico del backend (eventualmente usando mocking de servicios para probar la interacción entre capas), el proceso de build y la distribución de la app.
- La segunda etapa itera sobre la anterior de forma que los servicios ya funcionen para la versión del lenguaje que exista en ese punto del proyecto. Entre esas dos etapas (que serían dos sprints “largos”) podrán pedirse sprints cortos de avance.

## Criterios de Evaluación

El profesor revisará a su **criterio personal y profesional** el trabajo tanto en forma como en fondo. Los criterios de evaluación incluyen los señalados abajo. En su momento se dará una guía de puntajes específica con suficiente tiempo antes de la revisión final.
a) Cobertura de los objetivos planteados (lo contrario puede anular el proyecto)
b) Uso de las herramientas pedidas (lo contrario puede anular el proyecto)
c) Cobertura y Correctitud de las funcionalidades específicas pedidas (lo contrario puede anular el proyecto).
d) Suficiente calidad del código y uso de FP en los lugares y situaciones que lo ameriten en la adecuada combinación OOP-FP. Tanto en Java, JS y Prolog. (lo contrario puede anular el proyecto)
e) Calidad de la página principal y las funcionalidades requeridas
f) Respeto a la arquitectura pedida (lo contrario puede anular el proyecto)
g) Adecuada construcción de la app y su apropiado testing
h) Cumplimiento de sprints cortos que se pidan para evaluación parcial de avance
i) Entregable de fácil distribución para pruebas fuera del entorno de desarrollo

## Herramientas

- Lenguajes: JS (ES>=6), Java (jdk>=14), Prolog. Eventualmente Kotlin se podría pedir.
- Backend: Un server liviano para Webserver embedido. Los demos que se darán servirán como base. Opcional y alternativamente puede usarse SpringBoot (que usa un server embebido), pero inicialmente se quiere usar más directamente una librería menos “opinionada” (ver extras)
- Frameworks y librerías de Cliente: libre escogencia (debe primero consultarse con el profesor). Se recomienda investigar una de apoyo a la edición de texto en el cliente que permita fácil integración son servicios asincrónicos con estilo FP.
- No se pide capa de persistencia, si se incluye (ver extras) queda libre la escogencia.
- Build: Maven o Gradle. Flexible, a discutir con el profesor.

## Valor en la Nota

Este trabajo determina la nota dedicada a proyectos en la carta al estudiante con un valor de un 100% a ese rubro. Cada etapa (entre la I y II mencionadas) aporta un 50% (corresponden a proyecto I y II de la Carta al Estudiante, respectivamente). Se permiten puntos extras en cada etapa, los que pueden hacer la nota mayor que 100 hasta un 25% extra.

Igualmente, este trabajo podrá según su calidad ser usado más allá por el docente como un elemento de valoración y aprecio del interés por aprender y ganar el curso y reflejarlo en el cálculo de la nota final del curso

## Forma de Trabajo y Fechas

Se desarrolla solamente en los grupos de trabajo formalmente establecidos en el curso y conocidos por los estudiantes. Si la situación COVID-19 lo permitiera el profesor podría pedir las revisiones en persona. Sino se harán en sesiones no presenciales. Tentativamente: La primera etapa semana 10 y la segunda semana 17. Se entregan en forma y hora a definir al menos una semana antes. Se podrán pedir avances que afecten la nota final del trabajo.

## Puntos Extra

Se pueden, a criterio del profesor, conceder puntos extra de hasta un máximo de un 25% adicional sobre la nota del trabajo, siempre que el trabajo cumpla los mínimos pedidos. Debe ser presentados por los estudiantes y aceptados de previo por el profesor. Una vez comprometido un extra se vuelve obligatorio. Se recomienda medir bien los riesgos de las decisiones de extras por las curvas de aprendizaje. Posibles iniciativas para extras (pueden sugerir otras):
a) Capa de persistencia; si usan el extra SpringBoot se combina, un extra sería usar una BD
NoSql en SprinBoot.
b) Manejo de perfil de usuario y seguridad (login, autenticación y autorización
c) SpringBoot (pero en dos modos: uno sin SpringBoot otro con él)
d) React, Vue o Angular (medir el riesgo de eso por la curva de aprendizaje).
e) Typescript en vez de JS(ES6)
