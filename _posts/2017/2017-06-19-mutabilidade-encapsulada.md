---
layout: post
title: Mutabilidade Encapsulada
date: 2017-06-19
permalink: /:title
description:
  Um Objeto pode mudar internamente, mas permanecer imutável para o mundo exterior.
image: /images/2017/photo-ben-white-197668.jpg
categories:
  - OO
tags:
  - mutabilidade
keywords:
  - freepascal
  - fpc
  - delphi
  - lazarus
  - object-oriented
  - oop
  - mdbs99
  - immutable
  - imutablidade
  - mutable
  - mutability
  - mutável
  - encapsulado
  - encapsulated
---

Um Objeto pode mudar internamente, mas permanecer imutável para o mundo exterior.

<!--more-->

![Unsplash image]({{ page.image }})

## Introdução {#introducao}

Uma das atribuições da Orientação a Objetos é *gerenciar* a mutabilidade.

Então foi criado o conceito de atributos privados, onde o acesso a esses atributos não seria permitido ou só seria permitido através de [métodos públicos]({% post_url 2016-06-27-getters-e-setters %}).

O gerenciamento da mutabilidade nos proporcionou um código mais seguro, melhorando a munutenabilidade.

Entretanto, Objetos Mutáveis são os opostos dos [Objetos Imutáveis]({% post_url 2016-07-04-objetos-imutaveis %}). Esses últimos são mais *simples* de usar e ainda *mais seguros*.

Mas nem sempre conseguimos — ou queremos — codificar um Objeto *completamente* imutável. Seja por questão de performance, *design* ou restrição tecnológica.

Esse artigo irá lhe mostrar que podemos ter um Objeto *imutável* para o mundo externo, mesmo ele sendo *mutável* internamente.

## Mutabilidade {#mutabilidade}

Na Orientação a Objetos, toda mutabilidade deve ser controlada.

Nenhum atributo deve estar disponível para alteração de forma direta. Devemos, sempre, prover uma restrição com a máxima segurança aos atributos de um Objeto.

A maioria das linguagens com suporte a Orientação a Objetos já faz isso por padrão. Basta declararmos os atributos como `private` para que eles não sejam vistos fora do escopo do Objeto.

Infelizmente, o uso das [técnicas de reflexão](https://en.wikipedia.org/wiki/Reflection_(computer_programming)) acabaram com essa segurança, nos levando de volta aos tempos da programação procedural e variáveis globais, onde tudo está acessível a todos... mas, nesse artigo, vamos fingir que isso não existe. 
Voltemos aos atributos e a mutabilidade.

Não me refiro aos atributos que fazem parte do [Estado]({% post_url 2017-06-05-estado-do-objeto %}) do Objeto, pois esses não deveriam ser violados.

A mutabilidade estará em algo que chamo de [atributos secundários]({% post_url 2017-06-12-atributos-primarios-secundarios %}#secundario).

Esses tipo de atributo não faz parte do Estado do Objeto.
Então *ninguém*, além do próprio Objeto, sabe de sua existência.

Vejamos o mesmo exemplo do [artigo anterior]({% post_url 2017-06-12-atributos-primarios-secundarios%}#secundario):

<script src="https://gist.github.com/mdbs99/48237c902271b6f2c3a314d3b6f5b8c0.js"></script>

O atributo `FStream` não faz parte do Estado do Objeto. Esse atributo foi criado pensando na performance, ou seja, toda vez que o método `TFile.Stream` for chamado, apenas na primeira vez o *stream* do arquivo será lido em disco.

É um atributo secundário.

Mas algo muito importante deve ficar claro:
Poderíamos *remover* esse atributo.

Para o Objeto exercer seu [contrato]({% post_url 2016-01-18-interfaces-em-todo-lugar %}#interfaces-sao-contratos), ele não é necessário!

A implementação do Método poderia ser apenas isso:

<script src="https://gist.github.com/mdbs99/0aea75fefaf8612d0bc84d5fc9ce85a0.js"></script>
    
O Objeto continuaria funcionando, no entanto teríamos o "problema" da performance.

Então o programador opta por implementar algo interno ao Objeto. Algo que pode ser alterado a qualquer momento, sem impacto na manutenção do código externo ao Objeto.

## Conclusão {#conclusao}

Vimos que não há problemas em termos atributos secundários mutáveis dentro de Objetos — se o Objeto souber gerenciá-los corretamente.

O que acontece dentro de um Objeto, é segredo do Objeto.

Então seria vantagem criar tais atributos? Depende.

Haverá casos que é mais simples criar algo interno ao Objeto. Mas outras vezes, não. 

Você irá decidir com base nas Regras de Negócio, performance, simplicidade, *design* das Classes... e prazo de entrega do seu software.

Felizmente, como são atributos privados, você pode alterar o código qualquer momento.

Então, não pense tanto sobre isso.

Até logo.
