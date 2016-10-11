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
A linguagem Pascal tem e devemos aproveitar essa *feature*.

Nada mais são do que Funções declaradas dentro de Funções ou Métodos.

Funções Aninhadas é uma opção melhor do que 
[pular linha]({% post_url 2016-09-19-linhas-em-branco-no-metodo-e-mal-cheiro %})
dentro da implementação de um Método com o intuito de separar blocos
de código.

## Conclusão {#conclusao}

