---
layout: post
title: "Linhas em Branco no Método é um Mal Cheiro no Código"
date: 2016-09-18
description: "O motivo é simples: Você está fazendo coisas demais no Método."
summary: "O motivo é simples: Você está fazendo coisas demais no Método."
image: /images/photo-1417309807426-472e833fa5d0.jpg
categories: 
  - Code
tags:
  - Tips
keywords:
  - mal cheiro
  - bad smell
  - blank lines
  - linhas em branco
--- 

Pular linhas dentro de um Método é um "mal cheiro"
no código. Significa que você está fazendo coisas 
demais num único Método.

<!--more-->

![Imagem]({{ page.image }})

## Introdução {#introducao}

O que significa mal cheiro no código?

Um [mal cheiro](https://en.wikipedia.org/wiki/Code_smell),
se refere a qualquer sintoma no código-fonte 
de um programa que possivelmente indica um problema 
mais profundo.

Eu já falei sobre isso
[aqui]({% post_url 2016-01-03-pensando-em-objetos %}#exemplo-1)
e nesse artigo vou abordar melhor esse assunto.

Você irá entender porque não devemos utilizar 
linhas em branco dentro de Métodos.

## Blocos de Código

Por que muitos programadores ainda utilizam linhas em
branco dentro de Métodos?

Simples. Eles estão separando o código em 
blocos de código. E isso é, errado.

Eu mesmo fiz isso por anos. 

Algumas vezes eu conseguia enxergar esse mal cheiro, então 
refatorava — criando outros Métodos — e seguia a vida. 
Mas na maioria das vezes eu pensava que estava "organizando o código".
Tornando mais legível, mas "limpo"... não era verdade.

Quando li esse
[artigo](http://www.yegor256.com/2014/11/03/empty-line-code-smell.html)
eu percebi que sempre houve algo de errado quando 
utilizamos linhas em branco. Desde então eu não codifico
mais dessa forma.

## Não basta retirar as Linhas em Branco {#nao-seja-leviano}

Não seja leviano.

Não basta abrir seu editor e começar a remover toda
linha em branco que encontrar dentro dos Métodos. 

Não!

A palavra é **refatoração**.

A linha em branco não é o problema em si. 
Ela só indica que há um problema, ou seja, seu Método 
está fazendo coisas demais. Apenas retirar essas linhas 
não vai sanar o problema.

## Refatorando {#refatorando}

Se você realmente entendeu o conceito, não irá mais utilizar
linhas em branco dentro de Métodos de agora em diante.

Mas você pode ir além. Você pode querer refatorar todo tipo
de código antigo ou fazer uma refatoração *on demand*:
Refatorar apenas se encontrar Métodos com linhas em branco.

Você pode seguir esses passos:

  1. Encontre um Método;
  2. Verifique se há linhas em branco separando blocos;
  3. Para cada bloco, extrair o código criando um novo Método;
  4. Faça a chamada do novo Método no mesmo lugar que extraiu
  o código;
  5. Repita o processo enquanto for necessário.

## Conclusão {#conclusao}

Parece besteira, mas não é.

Escrever código qualquer um faz. Mas escrever um **bom** código, 
no entanto, não é pra qualquer um. É necessário pensar no
longo prazo, no acoplamento, na coesão, no *design*, deixar 
as coisas simples... é Engenharia, mas também Arte.

**E a verdadeira Arte está nos detalhes**.

Até logo.