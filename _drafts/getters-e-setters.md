---
layout: post
title: "Métodos Getters e Setters"
date: 2016-10-26
description: Não codifique Classes que anêmicas como baldes de dados.
summary: Não codifique Classes que anêmicas como baldes de dados.
image: /images/none.jpg
categories: 
  - OO
tags:
  - getter
  - setter
keywords:
  - getter
  - setter
  - evil
--- 


<!--more-->

![Imagem]({{ page.image }})


http://www.javaworld.com/article/2073723/core-java/why-getter-and-setter-methods-are-evil.html

Os Objetos devem ter métodos para interagir com o mundo exterior. Esses métodos podem executar ações e
retornar outros Objetos, no entanto o mundo exterior nunca deveria saber se o Objeto (valor) retornado no método
é ou não parte do Estado interno do Objeto.

Muitos programadores não entendem ou ignoram esse conceito. Por exemplo, se você tem uma Classe que tem apenas
métodos `Get` e `Set` para seus atributos, você não terá Objetos dessa Classe mas sim
[baldes de funções e dados]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}#objeto-nao-e-um-balde-de-funcoes-e-dados).

Os métodos `Get` são, conceituamente, mal entendidos e erroneamente utilizados. 
Em outras palavras, se você tem um método `GetName` que retorna um atributo `FName` que faz parte do Estado do Objeto, 
bem, conceitualmente falando, fica explícito para o mundo exterior que informações internas do Objeto estão sendo 
retornados apenas porque estamos utilizando o prefixo `Get`.
Essa é uma forma de Violação de Encapsulamento... no entanto é uma **violação teórica**, baseada 
apenas na nomenclatura e intenção. Sério.

Bem, se deixar o mundo exterior bisbilhotar dentro de um Objeto utilizando seus métodos `Get` para obter todo
o seu Estado interno é ruim, imagine então permitir alterações no Estado desse Objeto utilizando métodos `Set`.

<blockquote>
  Um Objeto não pode ter seu estado interno alterado, pois
  isso entraria em conflito com a realidade da Entidade na
  qual ele representa.
  <footer><cite title="eBook OPP">eBook — ObjectPascalProgramming.com</cite></footer>
</blockquote>

Objetos devem ser Imutáveis. 

Objetos Imutáveis são livres de efeitos colaterais externos. Eles são criados representando
um momento específico no tempo e devem permanecer inalterados até a sua morte.
  
