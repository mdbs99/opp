---
layout: post
title: "Métodos Getters e Setters"
date: 2016-06-27
description: Não codifique Classes anêmicas como se fosse um balde de dados.
summary: Não codifique Classes anêmicas como se fosse um balde de dados.
image: /images/photo-1466921583968-f07aa80c526e.jpg
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

Seus Objetos representam alguma Entidade real ou são apenas um "balde de dados e funções"? Classes que possuem somente métodos `Get/Set` não geram Objetos reais seguindo o real conceito da Orientação a Objetos. 

<!--more-->

![Imagem]({{ page.image }})

##Introdução {#introducao}

Os Objetos devem ter métodos para interagir com o mundo exterior. Esses métodos podem executar ações e retornar outros Objetos. No entanto o mundo exterior não deveria ter o conhecimento se os Objetos ou valores retornados nesses métodos fazem ou não parte do Estado interno dos Objetos.

<blockquote>
  Ao projetar com cuidado e se concentrar no que você deve fazer ao invés de como você vai fazer, elimina a grande maioria dos métodos getters/setters em seu programa.
  <footer><cite title="JavaWorld">— Why getter and setter methods are evil, JavaWorld</cite></footer>
</blockquote>

Muitos programadores não entendem ou ignoram esse conceito.

Um Objeto não deve representar um [registro numa tabela]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}#objeto-nao-e-um-balde-de-funcoes-e-dados), mas sim [Entidades]({% post_url 2016-02-29-objetos-representam-entidades %}) exteriores ao sistema no qual o Objeto foi criado.

##Métodos Getters {#getters}

Os métodos `Getters` são, conceituamente, mal entendidos e erroneamente utilizados. Por exemplo. Se você tem um método `GetName`, público, que retorna um atributo `FName: string` que faz parte do Estado do Objeto, conceitualmente falando, fica **explícito** para o mundo exterior que informações internas do Objeto estão sendo retornadas apenas porque estamos utilizando o prefixo `Get`.

Essa é uma forma de [Violação de Encapsulamento]({% post_url 2016-05-30-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-2 %}#encapsulamento). No entanto é uma **violação teórica**, baseada apenas na intenção da nomenclatura escolhida para o método.

Repito. É uma violação **teórica**. Isso quer dizer que não tem efeito prático em utilizar ou não o prefixo `Get`. Mas estou falando de intenção, ou seja, qual será o trabalho do método. Usar `Get` como prefixo dá a entender que **algo interno será dado a quem pede**. E isso não é bom.

Mas a linguagem *Object Pascal* tem *properties* — o mesmo conceito copiado para C# :) — e não precisamos utilizar métodos `Get`, encapsulando do mundo exterior como a informação será processada antes de ser retornada. Pode vir de um método `Get` privado ou diretamente do atributo. Ponto positivo para o *Object Pascal*.

##Métodos Setters {#setters}

Bem, se deixar o mundo exterior bisbilhotar dentro de um Objeto utilizando seus métodos `Getters` públicos para obter todo o seu Estado interno é ruim, imagine então permitir alterações no Estado desse Objeto utilizando métodos `Setters`.

<blockquote>
  Um Objeto não pode ter seu estado interno alterado, pois
  isso entraria em conflito com a realidade da Entidade na
  qual ele representa.
  <footer><cite title="eBook OPP">eBook — @ObjectPascalProgramming</cite></footer>
</blockquote>

Objetos devem ser Imutáveis. 

Objetos Imutáveis são livres de efeitos colaterais externos. Eles são criados representando um momento específico no tempo e devem permanecer inalterados até a sua morte.
  
Esse é um conceito muito menos abstrato do que a violação de encapsulamento utilizando `Getters`, mas pode ser ainda mais difícil de entender. E é por isso que terei que falar sobre imutabilidade em outro artigo.

##Conclusão {#conclusao}

Esse artigo serve para lembrá-lo que codificar Classes que representam registros em tabelas não é Orientação a Objetos.

Métodos `Getters` públicos não são necessários.

Métodos `Setters` ou *properties* que podem ser alteradas devem ser evitadas a todo custo. Nem todo Objeto pode ser imutável, infelizmente. Objetos *widgets*, por exemplo, serão mutáveis por conveniência e performance. Mas tente manter seus Objetos de Negócio imutáveis.

Podemos remover ambos, `Getters/Setters`, de nossos sistemas Orientado a Objetos quase por completo. Classes imutáveis não tem `Setters` e para `Getters` o Objeto deve passar a informação **ele mesmo** e não **alguém pegar a informação** dele. Falarei sobre isso futuramente.

Se você não precisasse de métodos `Setters` e utilizar `Getters` é no mínimo duvidoso, simplifique utilizando somente funções.

Como sempre faço uso de [Interfaces]({% post_url 2016-01-18-interfaces-em-todo-lugar %}), eu nunca implemento *properties* porque seria obrigado a declarar seus métodos de leitura/escrita, ou seja, `Get/Set`. Esses métodos poderiam ser privados, é claro, mas essa verbosidade é desnecessária se você estiver utilizando Objetos imutáveis.

Você acha que sou o único a pensar dessa maneira? Bem, então [aqui está](http://www.javaworld.com/article/2073723/core-java/why-getter-and-setter-methods-are-evil.html) um ótimo artigo que fala sobre o assunto. Foi dele que traduzi a citação no início do artigo. E se quiser mais um, [aqui está](http://www.yegor256.com/2014/09/16/getters-and-setters-are-evil.html). Boa leitura.

Até logo.