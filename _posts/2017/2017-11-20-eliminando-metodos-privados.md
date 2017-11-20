---
layout: post
title: "Eliminando Métodos Privados"
date: 2017-11-20
permalink: /:title
description:
  A utilização de Métodos Privados é um erro comum no design do código.
image: /images/2017/photo-james-sutton-187816.jpg
tags:
  - OOP
keywords:
  - freepascal
  - fpc
  - delphi
  - lazarus
  - object-oriented
  - oop
  - mdbs99
  - método privado
  - private
  - private method
---

A utilização de Métodos Privados é um erro comum no *design* do código.

<!--more-->

![Unsplash image]({{ page.image }})
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by James Sutton on Unsplash</span>

## Introdução {#introducao}

> *"Existe um pensamento que diz que todo Método Privado deveria ser, na verdade, a implementação de outro Objeto"*.

Eu escrevi esse texto acima em outro [artigo]({% post_url 2017-01-30-metodos-privados %}) aqui no blog... e eu acho que faz muito sentido.

No entanto, o artigo tentou demonstrar que os Métodos Privados não representam um mal no código e que podemos conviver com eles pacificamente.

É verdade que eles não representam um *grande* mal ao projeto. Existem tantos outros males [piores]({% post_url 2016-05-23-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-1 %}) para nos preocuparmos no dia-a-dia que não damos muita importância aos Métodos Privados.

Eu mesmo venho utilizando-os em meus projetos à muitos anos — tanto que já escrevi um artigo defendendo-os.

Mas, toda vez que eu codifico um Método Privado eu tenho a sensação de que alguma coisa está *errada*, mesmo quando eles *facilitam* a minha vida.

## Métodos Privados {#metodos-privados}

Você provavelmente sabe que Métodos Privados são utilizados somente pela Classe na qual eles foram implementados e nenhum Objeto externo tem acesso à eles.

Praticamente, toda linguagem que suporta o paradigma da [Orientação a Objetos]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}) tem uma sintaxe para a codificação de Métodos Privados.

É de uso comum.

Todos nós utilizamos, diariamente.

Mas, em teoria, a interface de um Objeto — seus Métodos Públicos — deveria ser suficiente para ele exercer o trabalho que lhe foi designado.

Se o trabalho é complexo e o Objeto necessitar de "ajuda", ele deveria delegar à outros Objetos e *não* tentar fazer tudo "sozinho" utilizando Métodos Privados.

No entanto, as linguagens dão suporte para escrevê-los.

As linguagens diferenciam Métodos privados, protegidos e públicos.

Então, entendemos que era correto utilizá-los e estamos fazendo isso desde então. 

Entretanto, se por um lado os Métodos Privados não representam um grande mal, por outro eles também não são o *ideal*.

## Facilitadores {#facilitadores}

Métodos Privados são apenas *facilitadores*.

Eles são utilizados do mesmo jeito que a maioria das pessoas utilizam, por exemplo, a fita [Silver Tape®](https://pt.wikipedia.org/wiki/Silver_Tape), ou seja, como uma *solução alternativa*.

Se algo quebrou no seu carro durante uma viagem (uma mangueira arrebentou, algo rachou, vazamentos, etc) e você não tem tempo para fazer um reparo perfeito ou as ferramentas apropriadas, você improvisa.

Você pode utilizar uma fita para manter as coisas no lugar na esperança de que, no futuro, você possa fazer o reparo do jeito correto.

Infelizmente podemos nos esquecer desses reparos temporários e seguir adiante, já que a "solução alternativa" continua mantendo tudo funcionando "perfeitamente" — como a mangueira de esguicho de água do para-brisa do meu carro que rachou e está com uma fita a mais de 2 anos.

Um Método Privado é algo localizado e sem muita importância por que está encapsulado numa Classe e pode ser alterado a qualquer momento (em teoria) sem efeitos colaterais.

Eu utilizo pouco o esguicho de água do para-brisa e mesmo assim ele continua funcionando mas, mesmo se parar de funcionar, não vai por minha vida em risco... teoricamente. Não haverá efeitos colaterais se eu fizer o conserto agora ou mais tarde.

Nota: Não me entenda mal. Eu faço a manutenção do meu carro quase que religiosamente, mas a prioridade sempre será o motor, fluídos e suspensão.

Projetos de Software também tem prioridades nos dizendo para não "perder tempo" codificando outras Classes, quando é mais *rápido* codificar Métodos Privados e resolver o problema da maneira mais "fácil".

Entretanto, sabemos que "mais fácil" não combina com *sustentabilidade* no longo prazo.

## Motivos {#motivos}

Há 9 meses atrás houve uma [discussão](http://objectpascalprogramming.com/posts/metodos-privados/#comment-3127567762) na área de comentários do blog sobre utilizar ou não Métodos Privados.

Num dos comentários eu escrevi: 

*"Minha opinião é que, como são Privados, não há nenhum impacto direto no código. Sempre posso modificá-los a qualquer momento."*

E isso é, de fato, verdade.

Se algo é privado a Classe, você pode modificá-lo sem haver impactos externos, na teoria. Faz parte do conceito de [encapsulamento]({% post_url 2016-05-30-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-2 %}#encapsulamento) de toda linguagem OO. Se é privado, somente a Classe tem acesso — sem levar em conta algo chamado *Reflexion* ou RTTI.

Então, quais seriam os motivos para deixar de utilizar Métodos Privados?

**Ao introduzir Métodos Privados, você pode estar codificando de forma procedural ao invés de Orientada a Objetos.**

Você pode estar introduzindo *funções procedurais* ao invés de Métodos que implementam o comportamento da [Entidade]({% post_url 2016-02-29-objetos-representam-entidades %}) representada.

<script src ="https://gist.github.com/mdbs99/bbdc509810890ea5724a94a021f16791.js"></script>

Na Classe acima, o método `CleanName` é apenas uma *função utilitária*. A função não pertence ao contexto representado por um `Employee` e por isso eu não a considero como um Método do Objeto.

Um `Employee` não "limpa" o próprio nome.

Essa é a tendência ao utilizarmos Métodos Privados: Adicionar funções utilitárias, por quê é fácil, conveniente e "ninguém está vendo".

Os nomes dessas funções também tem a tendência de serem [nomes compostos]({% post_url 2016-07-25-nomeando-variaveis-e-metodos %}#metodos) que é mais um *indício* mostrando que a função pode pertencer a *outro* contexto.

**Métodos Privados também podem gerar [duplicação de código]({% post_url 2017-01-09-codigo-duplicado-talvez-nao %}), escondendo [comportamentos]({% post_url 2016-03-14-objetos-pensam-e-tomam-decisoes %}) que poderiam ser compartilhados entre os Objetos mas, como são privados, não podem reutilizados**.

Veja mais um exemplo:

<script src="https://gist.github.com/mdbs99/a303d0bf83930fcf580aee818ee23a52.js"></script>

O Método `SplitName` — também um função utilitária — acima poderia ser reutilizado por outra Classe que também encapsula uma instância de `IName`. Mas aqui a Classe `TEmployeeName` tem sua própria implementação privada, não havendo outra maneira de reutilizar o mesmo algoritmo fora da Classe.

Em outras palavras, caso você precisasse utilizar o mesmo algoritmo implementado em `SplitName`, você teria que fazer uma cópia.

Acredito que estes podem ser bons motivos para eliminarmos os Métodos Privados dos nossos projetos desde o início ou quando for possível fazer a refatoração.

## Eliminando {#eliminando}

Acredito que podemos remover quaisquer Métodos Privados utilizando apenas Classes.

As Classes podem ser *públicas* (disponível a todo o sistema) e/ou [Aninhadas]({% post_url 2017-11-13-classes-aninhadas %}) a outras Classes.

Talvez os exemplos acima podem não ter convencido você.

Você poderia dizer que haverá casos em que os Métodos Privados fazem, sim, parte do mesmo contexto e por isso deveriam *existir*.

Por exemplo.

<script src="https://gist.github.com/mdbs99/8afe1f70a803556a59513fb2dff36e21.js"></script>

A Classe `THttpClient` não está completa, mas a parte que falta é irrelevante.

Veja que há o Método `Send` que é utilizado em 2 outros Métodos: `Get` e `Post`.

Então, é possível substituir o Método `Send` por uma Classe?

Com certeza.

<script src="https://gist.github.com/mdbs99/3cda576e9e13ac25ee5b4005a3b47a1a.js"></script>

E a vantagem de fazer isso é que deixamos a Classe mais simples e compacta, removendo o comportamento privado para uma nova Classe; agora podemos reutilizar o código externamente; se quisermos decorar `THttpProtocol` com outras Classes como Log, por exemplo, também podemos.

## Conclusão {#conclusao}

Após muito ponderar, cheguei a conclusão que prover uma sintaxe para codificar Métodos Privados é um *erro* no *design* da linguagem. Não só em Object Pascal, mas também em Java, C# e tantas outras.

Considere, então, que o uso de Métodos Privados foi o primeiro nível de aprendizado para sabermos dividir código dentro de uma Classe, entendê-los e saber como utilizá-los de forma consciente, *facilitando* a transição da Programação Procedural para a Orientada a Objetos.

Sabemos que particularidades da linguagem nos fazem criar Métodos Privados intencionalmente como, por exemplo, Métodos que executam [eventos]({% post_url 2017-05-08-eventos-e-objetos %}) — o Método Privado irá verificar se um evento foi ou não [setado]({% post_url 2016-06-27-getters-e-setters %}#setters) — e tudo bem se o método é apenas uma implementação de [infraestrutura]({% post_url 2017-01-09-codigo-duplicado-talvez-nao %}#infraestrutura).

Lembre-se também que, ao criar mais Classes públicas, ao invés de Métodos Privados, haverá mais código para documentar. No entanto, você poderá manter essas Classes "escondidas" do usuário final utilizando o conceito de [API Unit]({% post_url 2017-10-30-api-unit %}), declarando na API apenas as Classes que você "permite" utilizar.

Um código final com um mínimo de Métodos Privados pode representar um bom sinal de *design* do código, com Classes [simples]({% post_url 2016-12-19-simplicidade %}) e elegantes.

Então, não codifique Métodos Privados, a não ser que haja uma real necessidade para fazê-lo. Evite-os.

Até logo.
