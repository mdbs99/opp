---
layout: post
title: "Objetos pensam e tomam decisões"
date: 2016-03-21
description: Objetos pensam e tomam decisões
summary: Objetos pensam e tomam decisões.
image: /images/matrix20.jpg
categories: 
  - oop
  - object pascal
tags:
  - entidade
keywords:
  - entidade
  - entidade real
  - entity
  - real entity
  - criatura
  - contexto
---

Se você [Pensar em Objetos]({% post_url 2016-01-03-pensando-em-objetos %}) ou invés de pensar em procedimentos e funções
conseguirá implementar um código onde os Objetos conversam entre si ao invés de utilizar Programação Procedural, instruindo o 
compilador sobre o que fazer linha-a-linha.

Não queremos instruir o compilador. Não fazemos isso na Orientação a Objetos. Seu Objeto sabe o que fazer. Ele não
precisa de um "controlador" (você) para lhe dizer o que fazer. Ele sabe.
Ele foi [contratado]({% post_url 2016-01-18-interfaces-em-todo-lugar %}#interfaces-sao-contratos) pra isso. Deixe-o trabalhar.

<!--more-->

##Pense em Objetos, não em procedimentos {#pense-em-objetos}

Um Objeto deve ter uma identidade bem definida. Sua classe deve, sempre, 
[implementar uma Interface]({% post_url 2016-01-18-interfaces-em-todo-lugar %})
que representa a abstração da Entidade real. Cada Interface deverá ter apenas um, e somente um, objetivo.

Assim é uma Entidade, ela pensa por si mesma. 

...

Sempre comece pela Interface. 

Como saber se seu Objeto representa uma Entidade ou se ele é apenas um 
[balde de funções e dados]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}#objeto-nao-e-um-balde-de-funcoes-e-dados)?

Quando postei sobre [*DataModules*]({% post_url 2016-02-22-datamodule-e-apenas-um-container %})...