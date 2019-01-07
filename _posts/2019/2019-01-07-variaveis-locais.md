---
layout: post
title: "Variáveis Locais Deveriam ter Nomes Curtos"
date: 2019-01-07
permalink: /:title
description:
  Já pensou que você pode estar dificultando a leitura do seu código por utilizar nomes mais longos e verbosos em variáveis locais do que utilizando nomes mais curtos?
image: /images/2019/chuttersnap-413007-unsplash.jpg
tags:
  - Object Pascal
keywords:
  - freepascal
  - fpc
  - delphi
  - lazarus
  - pascal
  - object pascal
  - object-oriented
  - oop
  - mdbs99
  - naming
  - local variables
---

Já pensou que você pode estar dificultando a leitura do seu código por utilizar nomes mais longos e verbosos em variáveis locais do que utilizando nomes mais curtos?

<!--more-->

![Unsplash image]({{ page.image }})
<br><span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by chuttersnap on Unsplash</span>

Nomenclatura é quase uma arte, especialmente no desenvolvimento de software.

Escolher bons nomes irá deixar seu código mais legível e isso será imprescindível para a manutenção do mesmo no longo prazo.

Entretanto, um bom nome não quer dizer um nome mais longo e verboso.

Um bom nome irá depender de alguns fatores. Por exemplo: se o nome identificar uma variável de *índice* para um *loop*, a maioria dos programadores irá utilizar "i".

Por quê "i" e não "index", já que este último é mais explícito?

É claro que haverá todo tipo e código, todo tipo de nomenclatura. Mas, me refiro a maioria. E a maioria utiliza "i" para a nomenclatura de variáveis de índices.

E se quisermos nos referir a uma posição na tela? Eu diria que a nomenclatura mais utiliza é "x,y". Podemos declarar um *record* ou classe como `TPoint` mas é bem provável que seus atributos sejam `X` e `Y`.

"Convenção" — você diria.

Sim, convenção, porém eu acho que o motivo está além disso. 

Trata-se de **legibilidade**.

Quando estamos trabalhando em um contexto bem menor, como a implementação de um método por exemplo, não precisamos de nomes longos e verbosos. Sabemos o que X e Y significam; sabemos que "i" é o índice de um loop; sabemos que "s1" e "s2" (provavelmente) significam *strings* 1 e 2, respectivamente.

Não me entenda mal. Não estou dizendo que *todas* as variáveis locais devem ter apenas uma ou duas letras. Depende. E talvez não haja uma regra para determinar qual nomenclatura mais adequada, uma regra que abrange todos os casos possíveis. Mas podemos utilizar o bom senso.

Sugiro que os nomes sejam os menores possíveis, porém sem haver *ambiguidades*. Por exemplo: seria estranho haver duas variáveis assim:

    var
      s1: string;      
      s2: TStream;

São dois tipos completamente diferentes, porém utilizando a mesma nomenclatura. É ambíguo e confuso. Por quê `s2` representa o `TStream` e não `s1`? Falta lógica.

Porém ainda assim, na minha opinião, ainda seriam melhores que isso:

    var
      ClientNameTrimed: string;      
      ClientReturnedAsStream: TStream;

O motivo é simples: se você está nomeando suas variáveis de forma tão longa, é por quê o método está fazendo coisas demais e você precisa identificar cada variável — talvez existam mais de cinco — como única para tentar melhorar a legibilidade do método, mas ignorando os indícios de que o método deveria ser refatorado (dividido) em partes menores. Métodos menores poderiam ter menos variáveis, com nomenclatura mais simples.

Seguindo o primeiro exemplo, podemos refatorar as variáveis:

    var
      str: string;      
      stm: TStream;

Apesar de ainda haver certa ambiguidade — `str` ainda poderia ser considerada uma abreviação de `TStream` — a legibilidade é melhor. Poderíamos simplificar, refatorando `str` para apenas `s` já que trata-se de um tipo primitivo, aplicando a lógica de que "tipos primitivos estão abaixo de classes" e, portanto, utilizam menos caracteres...

O importante é haver alguma *lógica de nomenclatura* entre a equipe.

Outro exemplo de nomenclatura interessante é dar nomes próprios às variáveis, quando estas são do tipo interface ou classe. Vejamos:

    var
      bob: TStream;      
      john: TStream;

Temos 2 *streams* e, ao invés de nomear como `stm1` e `stm2` ou algo parecido, apenas damos nomes próprios e curtos, de *fácil* memorização.

Uma vez que seu cérebro já sabe que `bob` contém a entrada de dados (apenas um exemplo) e `john` irá conter a saída processada, seria mais difícil esquecer dos nomes comparado a `stm1` e `stm2`.

Mantenha os nomes de variáveis locais o mais curto possível.

Essa é a minha sugestão.

Até logo.
