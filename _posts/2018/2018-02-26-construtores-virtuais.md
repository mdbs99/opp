---
layout: post
title: "Construtores Virtuais"
date: 2018-02-26
permalink: /:title
description:
  A linguagem Object Pascal possui uma feature que não existe em nenhuma outra linguagem *mainstream* do mercado.
image: /images/2018/photo-eddie-kopp-264813-unsplash.jpg
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
  - construtor 
  - constructor
  - construtor virtual
  - virtual constructor
---

A linguagem Object Pascal possui uma feature que não existe em nenhuma outra linguagem *mainstream* do mercado.

<!--more-->

![Unsplash image]({{ page.image }})
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Eddie Kopp on Unsplash</span>

## Introdução {#introducao}

Construtores Virtuais permitem a criação customizada de novas instâncias sem a necessidade do uso de classes de fábricas.

É possível que desenvolvedores Object Pascal ao redor do mundo utilizem essa feature diariamente mesmo sem saber que só é possível utilizá-la na linguagem Object Pascal.

Toda classe possui um ou mais [construtores]({% post_url 2016-03-21-construtores-da-classe-primario-secundarios %}) no qual são responsáveis por instanciar um novo objeto.

Entretanto, ter um construtor virtual [não é comum](https://stackoverflow.com/questions/3651354/in-net-can-a-class-have-virtual-constructor) em outras linguagens.

Diferentemente das demais linguagens, os construtores na linguagem Object Pascal são como métodos "marcados" para serem construtores. Um fato sobre isso é que, em Object Pascal, podemos dar o nome que quisermos aos construtores — mesmo que o padrão de nomenclatura recomendado e utilizado seja <code>Create</code> — e como os métodos podem ser <i>virtuais</i>, assim também são os construtores.

Mas, qual é a grande vantagem de utilizá-los?

## Construtores Virtuais {#construtores}

Na documentação sobre *Factory and Repository patterns* do framework [Synopse mORMot](https://synopse.info/files/html/Synopse%20mORMot%20Framework%20SAD%201.18.html#TITLE_600) você encontrará o seguinte texto:

> In fact, Delphi is ahead of Java or C#, since it allows virtual constructors to be defined. Those virtual constructors are in fact a clean and efficient way of implementing a Factory, and also fulfill SOLID principles, especially the Liskov Substitution Principle: the parent class define an abstract constructor on which you rely, but the implementation will take place in the overridden constructor.

No texto acima, eu sugiro apenas alterarmos a palavra "Delphi" por "Object Pascal", visto que essa é uma feature da linguagem Object Pascal e não apenas da implementação do Delphi.

Na maioria das linguagens, o padrão Factory é implementado através de métodos (estáticos, abstratos ou virtuais) das classes e não através dos construtores. Esse é um dos motivos para o autor afirmar que o Delphi (Object Pascal) está a frente de C# e Java nesse quesito. Eu concordo.

Em Object Pascal podemos criar uma fábrica de objetos onde o tipo da classe original é a própria fábrica.

Pense no modelo clássico implementado em várias linguagens. Por exemplo, teríamos uma entidade principal <code>TFoo</code> e uma fábrica <code>TFooFactory</code>,  que retorna classes-filhas da entidade principal. É importante lembrar que <code>TFoo</code> deveria [implementar alguma Interface]({% post_url 2016-01-18-interfaces-em-todo-lugar %}) relacionada ao domínio, porém isso é irrelevante para o entendimento do problema.

O código principal iria utilizar a fábrica para ter instâncias de <code>TFoo</code>. A fábrica, no entanto, poderia retornar instâncias de novas classes-filhas, herdadas de <code>TFoo</code> e isso seria transparente para o código principal. Essa é uma das features que queremos ao utilizarmos uma fábrica.

Mas, primeiro teríamos que criar a fábrica para que ela retornasse as instâncias que realmente necessitamos.

Em Object Pascal não precisamos de uma classe específica para ser a fábrica de objetos. Nós podemos ter variáveis/argumentos do tipo classe e, dessa maneira, podemos passar diretamente a classe que deve ser utilizada para criar a instância; sem intermediários.

Imagine que temos uma simples library (lib) que soma 2 números. Essa lib tem uma classe <i>default</i> para fazer a soma, mas ela permite que a classe seja customizada, lembrando muito o [padrão Strategy](https://en.wikipedia.org/wiki/Strategy_pattern).

<script src="https://gist.github.com/mdbs99/0294f260c0d44b7c8b7f9a78df7662c1.js"></script>

No exemplo acima temos a classe <code>TLibCalc</code> que representa a lib. No construtor podemos customizar a classe que representa o algoritmo da soma, caso contrário a lib irá utilizar a classe <code>TSum</code> padrão.

A classe <code>TSum</code> é simples. Ela soma dois inteiros. Mas repare que seu construtor é <code>virtual</code>.

O próximo passo é criar um programa de teste.

<script src="https://gist.github.com/mdbs99/8589ff42e90b1311aa3f4b5bab2b7d16.js"></script>

A saída do programa será: <code>Result is 8</code>.

Se tudo estiver funcionando por aí, vamos aos próximos passos:

  1. criar nossa própria classe para somar 2 números
  2. alterar o programa, customizando a lib

<script src="https://gist.github.com/mdbs99/e364be559715b056463868d849e681ef.js"></script>

No código acima foi implementado uma nova classe <code>TMySum</code> e inicializamos a lib <code>TLibCalc.Create(TMySum);</code> com essa classe.

Execute o programa e veja o resultado.

Não foi necessária nenhuma fábrica. Nenhum "registro de classes" também foi necessário. Implementamos um padrão utilizando apenas classes simples da linguagem.

Finalmente, a linguagem Object Pascal é tão simples que bastou utilizarmos uma <i>instância de classe</i> contendo <i>construtores virtuais</i> para instanciar a classe correta.

## Conclusão {#conclusao}

Em Object Pascal não é necessário termos fábricas; a classe customizada é a própria fábrica.

Até onde eu sei, não existe essa feature em nenhuma linguagem <i>mainstream</i>, apenas em Object Pascal.

Com essa feature não é necessário criar classes de fábrica apenas para inicializar nossos verdadeiros objetos. Teremos menos classes, menos objetos, menos métodos, menos uso de memória, mais eficiência e [simplicidade]({% post_url 2016-12-19-simplicidade %}).

Até logo.
