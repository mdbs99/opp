---
layout: post
title: "Herança pode ser o Mal da Orientação a Objetos — Parte 3"
date: 2016-06-06
description: Não faça da Herança a sua primeira escolha para reutilizar código.
summary: Não faça da Herança a sua primeira escolha para reutilizar código.
image: /images/photo-1456087468887-17b7d7b076e0.jpg
categories: 
  - OO
tags:
  - herança
keywords:
  - herança
  - inheritance
  - evil
  - mal
  - orientação a objetos
  - oop
  - poo
  - encapsulamento
  - polimorfismo
--- 



<!--more-->

![Imagem]({{ page.image }})

##Hierarquias Complexas

>Se um gato possui raça e patas, e um cachorro possui raça, patas e tipoDoPelo, logo Cachorro extends Gato? 

O texto acima é emprestado [desse artigo](http://blog.caelum.com.br/como-nao-aprender-orientacao-a-objetos-heranca/) que
fala sobre Herança e Hierarquia de Classes. O artigo fala sobre o erro de *design* dos projetistas da linguagem Java quando
codificaram as Classes `Stack` e `Properties`. Não precisa conhecer Java para entender o artigo. 

No mesmo artigo ele mostra o erro grotesco no *design* da Classe `HttpServlet`. Não sei se você já utilizou Java — pelo 
menos na faculdade — e precisou extender essa Classe e se deparou com o mesmo problema citado no artigo.

Na época da faculdade eu vi isso mas, **ignorei**. Achei que eu estava errado. Ora, uma grande empresa que projetou o Java
não poderia estar errada, não é mesmo?

O artigo termina propondo uma solução para o *design* de `HttpServlet` que, não por coincidência, a mesma solução proposta 
para quase todos os problemas relatados nos artigos desse blog, ou seja, 
[utilize Interfaces]({% post_url 2016-01-18-interfaces-em-todo-lugar %}).



