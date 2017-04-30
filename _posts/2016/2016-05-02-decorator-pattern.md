---
layout: post
title: Decorator Pattern
date: 2016-05-02
description: Agregar responsabilidade a Objetos individuais, em tempo de execução, independente de sua Classe.
summary: Agregar responsabilidade a Objetos individuais, em tempo de execução, independente de sua Classe.
image: /images/photo-1461709444300-a6217cec3dff.jpg
categories: 
  - OO
tags:
  - decorator pattern
keywords:
  - decorator
  - pattern
---

Como agregar responsabilidade a Objetos individuais, em tempo de execução, independente de sua Classe?

Utilizando *Decorator Pattern*.

Os Decoradores oferecem uma alternativa simples e flexível ou uso de subclasses para extensão de funcionalidades. 

<!--more-->

![Decorator]({{ page.image }})

Para mim, [*Decorador*](https://en.wikipedia.org/wiki/Decorator_pattern) é um dos mais importantes padrões de *design*.
Infelizmente esse padrão quase nunca é utilizado, pelo menos nos projetos que vi nos últimos anos, independentemente
da linguagem de programação utilizada.

Por que?

Não tenho uma resposta exata, mas suponho que o motivo do não uso de Decoradores seja devido a escolha "natural" de 
utilizarmos **Herança de Classes** para estender funcionalidades.

Aprendemos que a Orientação a Objetos tem 3 pilares: **Herança, Encapsulamento e Polimorfismo.**

Herança de Classe é utilizada para estender funcionalidades, implementando uma nova Classe que herda as funcionalidades
da primeira e de toda uma hierarquia. Então você acha que usar Herança sempre será a escolha mais correta para extensão de funcionalidades?

Eu acho que não. Em 90~95% dos casos, eu utilizo **Decoradores** e **Composição de Objetos**.

<blockquote>
  <p>
    Um Decorador oferece uma abordagem do tipo "use quando for necessário” para adição de responsabilidades. Em vez de tentar
    suportar todas as características previsíveis em uma Classe complexa e customizada, você pode definir uma Classe simples e 
    acrescentar funcionalidade de modo incremental com objetos Decoradores.
  </p>
  <footer><cite title="Padrões de Projeto">— Padrões de Projeto, Erich Gamma, pág.173</cite></footer>
</blockquote>

Decoradores são tão simples de implementar como Herança, mas traz mais benefícios.

  * Você não precisa prever o futuro utilizando Decoradores. Utilizando Herança você tem que se preocupar com a hierarquia
  das Classes desde o início, desde a Classe mais básica até a mais específica, como se pudesse prever o futuro;
  * Evita Classes sobrecarregadas de características e funcionalidades;
  * Classes pequenas e coesas;
  
Exemplos de código?

  1. Parte do código de um projeto em produção que utiliza Decoradores para consumir um WebService, [aqui]({% post_url 2016-01-03-pensando-em-objetos %}#exemplo-2).
  2. Implementação de um Log com Decoradores para memória, arquivo, banco de dados e e-mail, [aqui]({% post_url 2016-03-07-classes-devem-implementar-apenas-uma-responsabilidade %}#implementacao-oo);
  3. Utilizando a Classe `TEmployeeSalaryWithCommission` como Decorador de `TEmployeeSalary`, [aqui]({% post_url 2016-03-14-objetos-pensam-e-tomam-decisoes %}#versao-orientada-a-objetos);
  
 
Até logo.
