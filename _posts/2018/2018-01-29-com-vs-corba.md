---
layout: post
title: "Interfaces COM vs. CORBA"
date: 2018-01-29
permalink: /:title
description:
  Existem dois tipos de Interfaces no Free Pascal definidos como COM e CORBA. Será que existem vantagens ao utilizar o tipo CORBA?
image: /images/2018/photo-hermes-rivera-265372.jpg
tags:
  - Object Pascal
keywords:
  - freepascal
  - fpc
  - delphi
  - lazarus
  - pascal
  - object pascal
  - object-oriented
  - oop
  - mdbs99
  - COM
  - CORBA
  - interface-com
  - interface-corba
---

Existem dois tipos de Interfaces no Free Pascal definidos como COM e CORBA. No Delphi temos apenas o tipo COM. Mas será que existem vantagens ao utilizar o tipo CORBA?

<!--more-->

![Unsplash image]({{ page.image }})
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Hermes Rivera on Unsplash</span>

## Introdução {#introducao}

No Delphi, quando definimos que um Objeto é do tipo Interface, quer dizer que (por padrão) ele será a liberado automaticamente da memória pelo compilador quando a variável sair do escopo e sua [contagem de referência]({% post_url 2016-10-10-interfaces-delegacao-problemas-solucoes %}#contador-de-referencias) chegar a zero.

Para utilizar essa *feature*, basta implementar sua Classe herdando de `TInterfacedObject`, afim de utilizar a implementação padrão para Interfaces COM.

Se você não quiser utilizar [herança]({% post_url 2016-05-23-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-1 %}), terá que implementar os [3-métodos]({% post_url 2016-10-10-interfaces-delegacao-problemas-solucoes %}#contador-de-referencias) obrigatórios para toda Classe que implementa Interfaces COM e isso realmente pode parecer estranho para programadores que estão vindo de outras linguagens.

*"Por quê eu deveria implementar métodos que não estão definidos na Interface?"*

Realmente, não parece fazer muito sentido.

## CORBA {#corba}

No Free Pascal você tem uma escolha. Pode-se utilizar Interfaces COM ou CORBA. O primeiro tipo se comporta do mesmo jeito que no Delphi. Tem contagem de referência e liberação automática de memória, enquanto o tipo CORBA é exatamente o contrário. Esse tipo não tem contagem de referência e, por isso, não tem a ajuda do compilador para liberar a memória automaticamente. Cabe ao programador liberar os Objetos utilizando seus respectivos destrutores.

Também não é possível liberar uma instância apenas atribuindo `nil`. É necessário que se tenha a instância de uma Classe ou que a Interface tenha algum método que possa ser utilizado como destrutor, por exemplo, `Free`.

    type
      IFoo = interface
        procedure Execute;
        procedure Free;
      end;

O tipo CORBA não exige do programador a implementação "obscura" de métodos de [infraestrutura]({% post_url 2017-01-09-codigo-duplicado-talvez-nao %}#infraestrutura) além do que está declarado na Interface. Apenas os métodos das Interfaces devem ser implementados nas Classes. Essa é uma vantagem em relação ao tipo COM.

Então, se você estiver desenvolvendo utilizando Free Pascal e não quer trabalhar com contagem de referência, utilizar CORBA pode ser uma boa opção.  Sua sintaxe e *design* "limpos" não obrigam o programador a implementar os 3-métodos padrões de COM.

Entretanto, lembre-se que todas as Interfaces dentro da mesma Unit terão o mesmo tipo, seja COM ou CORBA.

## COM {#com}

Muitos programadores não gostam de utilizar o tipo COM devido aos problemas com referência de Objetos. Mesmo que a [solução]({% post_url 2016-01-10-interfaces-e-o-metodo-estatico-new%}) seja simples para resolvê-los.

Enquanto o tipo CORBA pode ter um *design* mais simples, o tipo COM é mais *customizável* pois pode ser implementado sem contagem de referência. Basta implementarmos os 3-métodos obrigatórios sem incrementar a contagem.

Pessoalmente, sempre prefiro utilizar o tipo COM e a contagem de referência para a liberação automática dos objetos na memória. Entretanto, se eu quiser que apenas algumas Classes não tenham a contagem de referência, basta sobrescrever os métodos necessários para ter o mesmo comportamento das Classes que implementam Interfaces CORBA.

Considero isso uma *grande* vantagem!

Por exemplo. Posso ter uma única Interface mas com implementações distintas em Classes diferentes, utilizando ou não a contagem de referência.

## Conclusão {#conclusao}

Minha conclusão é que o tipo COM é *melhor* do que CORBA por quê podemos implementar os dois tipos de comportamentos (com ou sem contagem de referência), enquanto com CORBA implementamos apenas um comportamento.

O comportamento deve estar nas Classes, não nas Interfaces ou diretivas de compilação.

Até logo.
