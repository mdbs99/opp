---
layout: post
title: "Herança pode ser o Mal da Orientação a Objetos — Parte 2"
date: 2016-05-30
description: Não faça da Herança a sua primeira escolha para reutilizar código.
summary: Não faça da Herança a sua primeira escolha para reutilizar código.
image: /images/photo-1444212477490-ca407925329e.jpg
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

##Herança Viola o Encapsulamento

Vou repetir: **A Herança Viola o Encapsulamento**.

Isso é um **fato lógico**. Pode não parecer lógico se é a primeira vez que você lê essa afirmação, mas você entenderá.

Quando li isso a primeira vez tive o sentimento de **negação** que você pode estar tendo agora:

— "Ora, isso não faz o menor sentido!"

Como um um dos pilares da Orientação a Objetos, a Herança, pode se **contrapor** a outro princípio, o Encapsulamento?

Se a Subclasse precisa conhecer os métodos da Super Classe e, consequentemente, como eles funcionam, já houve a Violação
do Encapsulamento.

##Herança leva a um Forte Acoplamento



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

##Composição

Deveríamos utilizar a [Composição de Objetos](https://en.wikipedia.org/wiki/Composition_over_inheritance) ao 
invés de **Herança de Classes**.

Composição significa **tem-um** ao invés de **é-um** como na Herança.

---

  - Go language não tem herança: https://talks.golang.org/2012/splash.article#TOC_15.


