---
layout: post
title: Procedural é fácil, Orientado a Objetos é difícil
date: 2016-09-05
description: 
summary: 
image: /images/photo-1470208564179-dd5b52a0d010.jpg
categories: 
  - Pascal
tags:
  - procedural
  - oop
keywords:
  - orientado a objetos
  - procedural
--- 

Por que ainda temos discussões sobre a Programação Procedural ser mais fácil do que a Programação Orientada a Objetos? É claro que Programar no paradigma Procedural é mais fácil.

<!--more-->

![Imagem]({{ page.image }})

Você olha para um código com 5 instâncias de Objetos, agumentos de Construtores, consumo de memória, polimorfismo e pensa:

**Eu poderia fazer a mesma coisa com 10 linhas de código Procedural.**

A performance — você diria — poderia ser melhor, iria consumir menos memória e, claro, é muito mais fácil codificar.

Eu concordo... em parte.

Então por que escolher a [Orientação a Objetos]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %})? 

Por que fazer da maneira "mais difícil"? 

Por que criar um [*overhead*](https://en.wikipedia.org/wiki/Overhead_(computing)) "desnecessário"?

<iframe src="https://www.facebook.com/plugins/post.php?href=https%3A%2F%2Fwww.facebook.com%2Fobjectpascalprogramming%2Fposts%2F316968578655967&width=500" width="500" height="180" style="border:none;overflow:hidden" scrolling="no" frameborder="0" allowTransparency="true"></iframe>

Se pensarmos apenas no *overhead* que traz a Orientação a Objetos, vamos então codificar nossos sistemas em apenas um único arquivo, usando *GO TO* e código ASSEMBLY.

Orientação a Objetos é sobre **manutenabilidade** no longo prazo.

É sobre não pensar como um controlador ou CPU, mas como [Entidades]({% post_url 2016-02-29-objetos-representam-entidades %}) auto-suficientes que [pensam e tomam decisões]({% post_url 2016-03-14-objetos-pensam-e-tomam-decisoes %}) por conta própria.

Não é sobre estar certo ou errado, fazer diagramas bonitos ou porque fica legal por no Currículo.

**Orientação a Objetos é sobre resolver um problema de forma sustentável.**

Por isso escolhemos desenvolver seguindo esse paradigma.

Em 1 hora é possível fazer muita coisa de forma Procedural, enquanto na Orientação a Objetos podemos ficar 1 hora apenas tentando identificar o melhor [nome para uma Classe]({% post_url 2016-04-25-nomeando-classes %}), quais são as [Entidades]({% post_url 2016-02-29-objetos-representam-entidades %}) que fazem parte do problema, quais os Contextos...

E se eu lhe dissesse que podemos ficar "apenas" [pensando]({% post_url 2016-01-03-pensando-em-objetos %}) por 1 ou 2 semanas, sem escrever nenhuma linha de código?

É verdade, acontece.

Não adianta fazer um sistema rapidamente se seu código não é **sustentável** no longo prazo.

*Softwares* sempre **mudam**.

**Orientação a Objetos é sobre organização.**

Nós passamos a maior parte do tempo não codificando novas *features*, mas sim lendo e tentando entender o código.

De que adianta ter um código de execução mais rápida em 100 ms se daqui a um mês ninguém sabe como alterá-lo?

Orientação a Objetos não é mais difícil de codificar, é apenas é mais trabalhoso.

**Mas vale a pena.**

Por que no fim você irá trabalhar menos. 

Talvez com um pouco mais de código, é verdade, mas terá menos *bugs*.

Menos *debugging*.

Menos problemas.

Orientação a Objetos agrega **qualidade**.

E qualidade, é o que mais importa.

Até logo.