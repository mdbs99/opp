---
layout: post
title: "Interfaces e Casting"
date: 2018-07-23
permalink: /:title
description:
  Seria um anti-padrão verificar se um objeto implementa determinada(s) interface(s)? Seria essa prática considerada o mesmo que casting?
image: /images/2018/stefan-stefancik-257625-unsplash.jpg
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
  - interface
  - supports
  - casting
---

Seria um anti-padrão verificar se um objeto implementa determinada(s) interface(s)? Seria essa prática considerada o mesmo que *casting*?

<!--more-->

![Unsplash image]({{ page.image }})
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Štefan Štefančík on Unsplash</span>

Já faz mais de 2 anos que [escrevi]({% post_url 2016-04-18-nao-utilize-casting %}) que não devemos utilizar casting. 

Essa pode ser uma afirmação difícil de acreditar, se você não por sua mente no contexto certo antes.

Alguns desenvolvedores tem me perguntado, como é possível fazer uma aplicação sem haver *casting*?

Bem, em uma aplicação de médio tamanho eu diria que pode ser até difícil, mas não impossível. Entretanto, irá depender do *tipo* de casting que estamos tratando.

Na programação orientada a objetos só haverá objetos. Então vamos esquecer os tipos primitivos. Um cast de uma *string* em um *Integer*, por exemplo, é algo *necessário* em linguagens que possuem tipos primitivos.

Esse não é o problema.

O casting a que me refiro é sobre *objetos*.

Passar um parâmetro para um método com o tipo `TObject` para, então, fazer um casting interno para outro objeto é como ter um "pré-conceito" sobre o objeto que irá fazer o trabalho.

O objeto em si não é suficiente?

Se você está verificando o *tipo* do objeto para fazer o trabalho, é pré-conceito.

Você não confia nele. Você tem sempre que *gerenciá-lo*, dizendo quem ele deve *representar* em cada parte do código.

Mas, há alguma maneira de trabalhar com objetos sem utilizar casting?

O "segredo" está nas [Interfaces]({% post_url 2016-01-18-interfaces-em-todo-lugar %}). Uma interface é um contrato entre o cliente (sistema) e o trabalhador (objeto).

No entanto, um objeto pode implementar mais de uma interface, tendo muitas *especialidades* por assim dizer.

Então, é possível que um mesmo objeto possa trabalhar em muitas especialidades diferentes. Mas a única maneira de saber isso é *perguntar* ao próprio objeto se ele *sabe* fazer o serviço especializado.

Vejamos um exemplo:

<script src="https://gist.github.com/mdbs99/e8312b8e539ac0e1b1baac28be18efe3.js"></script>

O mesmo empregado pode ser um analista e/ou programador. 

A função [Supports]({% post_url 2018-03-12-supports-traicoeira %}) "pergunta" ao objeto se ele também sabe fazer um "outro trabalho", determinado pelo "contrato" no segundo parâmetro da função.

A diferença é sutil, mas importante: Tentar transformar um objeto em outro é equivocado, mas perguntar à ele se tem uma determinada especialidade, não.

Até logo.




