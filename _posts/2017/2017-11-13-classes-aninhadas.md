---
layout: post
title: "Classes Aninhadas"
date: 2017-11-13
permalink: /:title
description:
  As Classes Aninhadas reduzem o número de Classes públicas, melhoram o encapsulamento, o polimorfismo local e a legibilidade do código.
image: /images/2017/photo-iorni-321845.jpg
tags:
  - Pascal
keywords:
  - freepascal
  - fpc
  - delphi
  - lazarus
  - object-oriented
  - oop
  - mdbs99
  - classe-aninhadas
  - nested-class
  - nested-classes
---

As Classes Aninhadas reduzem o número de Classes públicas, melhoram o encapsulamento, o polimorfismo local e a legibilidade do código.

<!--more-->

![Unsplash image]({{ page.image }}) 
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by iorni on Unsplash</span>

## Introdução {#introducao}

A maneira mais fácil (preguiçosa e errada) de reaproveitar código é utilizando [herança]({% post_url 2016-05-23-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-1 %}).

Esse é o início do aprendizado sobre [Orientação a Objetos]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}).

Como muito mais tempo, aprendemos que a [Decoração]({% post_url 2016-05-02-decorator-pattern %}) e Composição de Objetos é o *melhor* caminho para *adicionar* e *reaproveitar* comportamento, respectivamente.

Classes Aninhadas tem tudo haver com Decoração e Composição.

Nesse artigo você irá aprender como utilizar Classes Aninhadas para melhorar o encapsulamento e polimorfismo, acrescentando comportamento específico e localizado, sem o uso de *"Class Helpers"*.

## Classes Aninhadas {#classes-aninhadas}

A linguagem Object Pascal, apesar de ser bem antiga, está em constante desenvolvimento.

Mas, eu sou avesso a grandes mudanças na linguagem. Em qualquer linguagem.

Também não sou a favor de acrescentar novas *features* apenas por quê existem em *outras* linguagens.

Quanto mais [simples]({% post_url 2016-12-19-simplicidade %}) for uma linguagem, mais fácil será o entendimento do código.

Na minha opinião, ao invés de acrescentar novas *features* numa linguagem, os desenvolvedores deviam trabalhar em novas *Libraries* que resolvem problemas específicos.

No entanto, algumas *features* realmente podem fazer a diferença, ou seja, tornar o código melhor e mais simples.

O conceito de Classes Aninhadas é simples: Permitir a declaração de Classes dentro de Classes. Sejam elas privadas ou públicas.

Veja um exemplo da sintaxe:

<script src="https://gist.github.com/mdbs99/96820c816ffa3e1eb5296a330e3a1cef.js"></script>

No código acima, para declarar uma Classe `TBar`, foi necessário fazer a referência a Classe `TFoo`, primeiramente.

Então, `TBar` é uma Classe Aninhada em `TFoo` ou, em outras palavras, `TBar` está encapsulada em `TFoo`.

Acredito que para o compilador, uma Classe Aninhada é apenas outra qualquer. Não há penalidades no código. A primeira Classe da hierarquia *não* irá criar Objetos maiores devido as Classes Aninhadas.

Para nós, desenvolvedores, é apenas um [*Syntactic sugar*](https://en.wikipedia.org/wiki/Syntactic_sugar) que parece não valer muito a pena prestar atenção.

No entanto, para um *design* realmente [Orientado a Objetos]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos%}), esta é uma *feature* que proporciona uma *grande* melhoria no código.

## Redescobrindo {#redescobrindo}

O fato é que não faz tanto tampo que eu descobri que a linguagem Object Pascal tem suporte a Classes Aninhadas. Bem, talvez eu já tenha lido sobre isso já faz alguns anos, mas a verdade é que eu nunca tinha utilizado tal recurso em sistemas reais.

Há 3 semanas atrás, esse conceito retornou a minha mente após receber um e-mail relacionado a uma nova *issue* num sistema privado.

Eu tinha que fazer uma manutenção no código e um dos requisitos dizia que uma parte do sistema deveria se comportar de maneira ligeiramente diferente da condição atual, mas por um curto período de tempo. 

O motivo era que um segundo sistema estava sendo alterado e, enquanto essa tarefa não estivesse concluída, o primeiro sistema deveria se comportar de acordo com o requisito citado.

Como o sistema é privado, o requisito real é irrelevante.

Mas, para que você entenda como as Classes Aninhadas me ajudaram a implementar o requisito de forma simples, clássica e seguindo os [princípios](lista-vip/) da Orientação a Objetos, vou apresentar uma forma bem simplificada do que deveria ser alterado no código para a solução da tarefa.

## O Problema {#problema}

Imagine que você tenha uma Unit com algumas Classes.

Essas Classes utilizam outras Classes de outras Units.

Para implementar o requisito, uma dessas Classes deveria ser alterada. No entanto, a alteração era relacionada com a Composição de outra Classe — na verdade eram outras Classes, no plural, mas vamos manter simples.

Então temos o seguinte:

  1. A Classe `TFoo` é a que temos que alterar. No entanto ela trabalha com uma instância de `TBar` internamente — novamente, por motivos de simplificação, vamos esquecer que a instância de `TBar` deveria ser [injetada]({% post_url 2017-10-16-injecao-de-dependencia %}) no [construtor]({% post_url 2016-03-21-construtores-da-classe-primario-secundarios %}) da Classe `TFoo`.
  1. A interação entre as Classes `TFoo` e `TBar` era o que deveria ser alterado para a conclusão do requisito. No entanto, todas as outras Classes pertencentes a mesma Unit de `TFoo` deveriam continuar com o mesmo comportamento atual. Em outras palavras, a Classe `TBar` deveria ser alterada *apenas* na implementação (local) de `TFoo`.

Temos várias maneiras de implementar isso, é claro.

Antes, vamos [nomear]({% post_url 2016-07-18-nomeando-unidades %}) algumas coisas para facilitar o entendimento:

  * `Acme.Foos.Clss` -> é a Unit onde a Classe `TFoo` está declarada;
  * `Acme.Bars.Clss` -> é a Unit onde a Classe `TBar` está declarada;

Então, para implementar o requisito, nós poderíamos:

  1. Implementar uma nova Classe na Sessão de Interface da `Acme.Bars.Clss` chamada `TBarEx` ou;
  1. Implementar uma nova Classe na Sessão de Interface da `Acme.Foos.Clss` chamada `TBarEx` ou;
  1. Criar uma nova Unit `Acme.Bars.Ex.Clss` e implementar uma nova Classe chamada `TBarEx` ou;
  1. Implementar uma nova Classe chamada `TBarEx` na Sessão de Implementação da unit `Acme.Foos.Clss`.

O caminho a ser escolhido é quase infinito, porém essas seriam as opções mais comuns, considerando que não queremos fazer uma refatoração muito grande no código. Será uma alteração temporária, lembra?

Infelizmente, existem problemas de *design* com todas as opções acima.

  1. Implementar a nova Classe `TBarEx` na Sessão de Interface de `Acme.Bars.Clss` ou `Acme.Foos.Clss` irá disponibilizar a nova Classe para todo o sistema. Como é uma Classe temporária, ela não deveria estar disponível... não dessa forma.
  1. Criar uma nova Unit `Acme.Bars.Ex.Clss` é ainda pior, pois depois de algum tempo a Unit deverá ser removida, havendo mais alterações no código.
  1. Por fim, implementar uma nova Classe chamada `TBarEx` na Sessão de Implementação da unit `Acme.Foos.Clss` seria a opção mais viável. No entanto não teria sido um código elegante, pois essa nova Classe seria criada para apenas para um caso específico mas elas estaria disponível para a Unit inteira. Também tem o fato de que a Classe *deve* ter um nome diferente (`TBar` vs `TBarEx`) — esse é um dos motivos de adicionarmos prefixos ou sufixos estranhos — para não haver conflito com todas as instâncias de `TBar` que continuam sendo utilizadas por toda a Unit `Acme.Foos.Clss`. O ideal seria continuar utilizando a nomenclatura `TBar`, de acordo com o domínio. Mas, infelizmente, a técnica de [redeclaração de Classes]({% post_url 2017-08-21-redeclarando-classes %}) não seria possível, pois só queremos uma `TBar` diferente em apenas um lugar na Unit.

Eu não escolhi nenhuma das possíveis soluções acima.

## A Solução {#solucionando}

Após pensar por alguns minutos sobre como resolver o requisito, o conceito de Classes Aninhadas retornou a minha mente como a melhor escolha.

A solução é tão simples que você já a conhece. Está no início desse artigo!

Vou apenas adicionar mais código para deixar a implementação mais contextualizada:

<script src="https://gist.github.com/mdbs99/b292e33439d9867b2c8c92d8caba4a96.js"></script>

Agora, em todos os métodos de `TFoo` que for preciso decorar instâncias de `IBar`, uma Classe de [nomenclatura]({% post_url 2016-04-25-nomeando-classes %}) simples e *local* poderá ser utilizada sem haver conflito de nomes na Unit.

A Classe `TFoo` tem sua própria implementação privada de `TBar`. Nenhum outro código no sistema poderá utilizar essa implementação. 

É uma solução *local*, sem *workarounds*.

## Conclusão {#conclusao}

Espero que os exemplos não tenham sido muito abstratos. A ideia foi mostrar o potencial dessa simples *feature* sem deixar o artigo muito longo.

É uma solução simples e bastante elegante. Totalmente de acordo com os princípios da Orientação a Objetos, encapsulamento e polimorfismo.

A nomenclatura das Classes é simples, mantendo a legibilidade do código.

A linguagem Object Pascal continua a me surpreender, mesmo após mais de 16 anos que a utilizo.

Até logo.