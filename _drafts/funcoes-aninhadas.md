---
layout: post
title: "Funções Aninhadas Melhoram a Legibilidade do Código"
date: 2016-10-17
description:
  Funções Aninhadas podem e devem ser utilizadas, pois melhoram
  a legibilidade do Código e sua manutenção.
summary: 
  Funções Aninhadas podem e devem ser utilizadas na Orientação a Objetos
image: /images/photo.jpg
categories: 
  - Object Pascal
tags:
  - Language
keywords:
  - funções aninhadas
  - nested functions
  - organização do código
--- 

Se você tem um Método coeso, que faz uma única tarefa,
mas mesmo assim o código parece complicado, dificultando
o entendimento e a manutenção... o que fazer?

<!--more-->

![Imagem]({{ page.image }})

## Introdução {#introducao}

[Funções Aninhadas](https://en.wikipedia.org/wiki/Nested_function)
é algo que não tem em todas as linguagens.
A linguagem Pascal tem e acho que devemos aproveitar essa *feature*.

Funções Aninhadas nada mais são do que funções declaradas dentro de
outras funções ou Métodos.

Esse artigo irá mostrar os motivos para utilizarmos Funções Aninhadas
e algumas regras que devemos seguir.

## Motivos {#motivos}

Funções Aninhadas é uma opção bem melhor do que 
[pular linhas]({% post_url 2016-09-19-linhas-em-branco-no-metodo-e-mal-cheiro %})
dentro da implementação de um Método com o intuito de separar blocos
de código.

Cada função já define um bloco, com a vantagem de ser reutilizável em
outra parte do Método.

Elas estão muito perto do paradigma da Orientação a Objetos (leia esse
[artigo](http://blog.synopse.info/post/2012/05/20/Recursive-calls-and-private-objects)
).

Cada Método que contenha Funções Aninhadas é como uma Classe Anônima.
As variáveis locais serão como atributos e as Funções Aninhada serão
como Métodos privados.

Funções Aninhadas facilitam o uso correto do
[*WITH*]({% post_url 2016-09-12-a-declaracao-with-do-e-do-mal %})
pois irão evitar o conflito entre identificadores, nos casos em que
a implementação do Método seja complexa ou utilize muita composição de
Objetos.

O código fica bem organizado, fácil de ler e alterar.

## Regras {#regras}

Qualquer *feature* que é utilizada de forma indiscriminada poderá ser um
problema futuramente.

Precisamos de regras e disciplina.

Então cito aqui algumas regras que utilizo em meus projetos.

### Regra 1: Não utilize mais do que 3 Funções Aninhadas

É uma regra óbvia.
Se você tem muitas Funções Aninhadas dentro e um único Método, 
é bem provável que ele esteja fazendo coisas demais.
Pense na refatoração e decomposição em outros Métodos ou mesmo
na criação de uma nova Classe.

Existem pouquíssimas exceções a essa regra.

### Regra 2: Evite compartilhar as Variáveis Locais

Falei acima que um Método com Funções Aninhadas é como uma Classe
Anônima. No entanto, sabemos que não são verdadeiras Classes. 

É melhor que você isole cada Função Aninhada em suas próprias 
variáveis e argumentos, ou seja, evite compartilhar a variável
local do Método dentro das Funções Aninhadas.

Essa disciplina irá ajudá-lo na extração e refatoração das 
Funções Aninhadas para criar outros Métodos com o mínimo de 
impacto possível.

Ao invés de utilizar a variável local do Método, passe
a referência como argumento da função, mantendo-as isoladas.

Existem poucas exceções a esta regra. Por exemplo.
Se as Funções Aninhadas trabalham sempre com
os mesmos Objetos (variáveis locais), é mais fácil compartilhar
as variáveis entre as funções; ou refatore e crie uma Classe.

### Regra 3: Apenas 1 Nível

Não complique. Use apenas um "nível" de Função Aninhada. Se
você tiver utilizando mais de um nível, é provável que a Função
deveria ser um Método. 

Refatore. Nesse caso o "nível 2" passaria a ser o "nível 1" 
no novo Método criado.

Sem exceções aqui!

## Exemplos {#exemplos}


## Conclusão {#conclusao}

