---
layout: post
title: A Definição de Objeto
date: 2017-01-23
description:
  Nesse artigo/vídeo eu explico a definição correta de 
  Objeto dentro do paradigma da Orientação a Objetos.
categories: 
  - Video
tags:
  - oop
keywords:
  - orientação a objetos
  - object oriented
  - object
  - objetos
--- 

Nesse artigo/vídeo eu explico a definição correta de Objeto dentro do paradigma da Orientação a Objetos.

<!--more-->

<iframe width="600" height="355" src="https://www.youtube.com/embed/nia7UqcpOAc" frameborder="0" allowfullscreen></iframe>

## Introdução {#introducao}

Então, o que é um Objeto?

Para responder a essa pergunta precisamos 
saber o [Contexto]({% post_url 2016-10-03-delegacao-de-implementacao-de-interfaces %}#contextos).

No contexto puramente técnico, um Objeto 
nada mais é do que uma estrutura com dados
e funções.

No contexto da [Orientação a Objetos]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}),
no  entanto, um Objeto é a representação 
abstraída de uma Entidade.

Uma [Entidade]({% post_url 2016-02-29-objetos-representam-entidades %}) pode ser uma pessoa, um animal, 
uma bola, um pixel, um relatório, um arquivo,
um diretório... qualquer coisa que deva ser 
representado dentro do software é um forte 
candidato a ser um Objeto.

Mas muitos desenvolvedores não conseguem 
fazer essa distinção, entre o contexto técnico e o contexto da arquitetura.

Eles continuam enxergando um Objeto (apenas) 
como uma estrutura de dados e funções e não 
conseguem entender a diferença entre programação 
Procedural e a Orientada a Objetos.

## Abstrações e Camadas {#abstracoes-e-camadas}

Desenvolvedores precisam entender que existem 
abstrações e camadas para o contexto Técnico 
e também para o contexto da Arquitetura. 

Desde a época do cartão perfurado, passando 
pela linguagem ASSEMBLY, até as linguagens 
mais modernas de hoje em dia, os engenheiros 
foram adicionando mais e mais camadas e 
abstrações às linguagens.

As linguagens mais modernas hoje são frutos 
dessas adições de abstrações e camadas técnicas, 
com o objetivo de melhorar o código e a 
codificação em si.

Paradigmas como a Orientação a Objetos 
adicionam abstrações para melhorar a arquitetura.

Mas cada abstração ou camada, seja técnica ou 
arquitetural, são apenas simplificações da 
realidade, pois para o computador, no fim, 
tudo será representado por "zeros e uns".

A maioria dos programadores entendem e utilizam 
as abstrações técnicas no dia-a-dia sem nem 
mesmo pensar sobre isso.

Estou falando do uso de estruturas de repetição
 como FOR ou WHILE e estruturas condicionais 
 como IF ou CASE.

Quando você utiliza uma estrutura FOR, por 
exemplo, o compilador irá gerar uma série de 
comandos ASSEMBLY com instruções GOTO.

Você não pensa sobre isso, não é? É como se 
a estrutura FOR fosse um Objeto e você apenas 
espera que ele exerça seu trabalho depois de 
passar os argumentos de inicialização.

Você deveria ter esse mesmo pensamento quanto 
estiver trabalhando com Objetos, dentro do 
paradigma da Orientação a Objetos.

## Objetos {#objetos}

Olhe para um Objeto do mesmo modo que você 
olha para uma estrutura FOR, WHILE, IF, etc., 
ou seja, você não pensa como essas estruturas 
funcionam internamente (encapsulamento). 
Você só sabe quais são seus argumentos de 
inicialização (construtor) e espera que eles 
trabalhem (comportamento) corretamente.

Orientação a Objetos é isso, apenas mais 
uma camada, mais uma abstração de arquitetura 
acima do código procedural.

Entender esse conceito sutil é o início 
para você começar a utilizar a Orientação a 
Objetos da forma mais correta.

Orientação Objetos não substitui o paradigma 
Procedural, ela o complementa; o aprimora.

## Conclusão {#conclusao}

Os computadores continuam trabalhando com a 
mesma  premissa procedural desde que eles 
foram inventados.

O fato é que estamos presos a isso, de certa
forma.

Mas podemos abstrair, podemos adicionar 
camadas que facilitem o desenvolvimento da 
arquitetura e entendimento do código. 
A Orientação a Objetos faz isso muito bem 
utilizando artefatos que nós chamamos de... Objetos.

Até logo.
