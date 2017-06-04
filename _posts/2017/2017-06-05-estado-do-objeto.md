---
layout: post
title: Estado do Objeto
date: 2017-06-05
permalink: /:title
description:
  Objetos possuem Estado que pode ser implícito ou explícito mas o mais importante é que ele deveria ser inviolável.
image: /images/2017/photo-tim-evans-88330.jpg
categories: 
  - OO
tags:
  - estado
keywords:
  - freepascal
  - fpc
  - delphi
  - lazarus
  - object-oriented
  - oop
  - mdbs99
  - estado
  - state
---

Estado de um Objeto. Existem dois tipos. Entenda-os e torne-os invioláveis.

<!--more-->

![Unsplash image]({{ page.image }}) 

## Introdução {#introducao}

Venho escrevendo sobre Orientação a Objetos e já falei  várias vezes sobre o *Estado de um Objeto*, mas nunca havia explicado o que exatamente isso significa.

Saber esse conceito é primordial para o entendimento da verdadeira Orientação a Objetos.

## O que é Estado {#estado}

Todo Objeto é composto de Estado e Comportamento.

Enquanto o comportamento é o que o Objeto *faz*, Estado é o que o Objeto *sabe* sobre a [Entidade]({% post_url 2016-02-29-objetos-representam-entidades %}) que ele está representando dentro do *software*.

Todo Objeto tem um Estado, mesmo que você não saiba sobre ele.

Existem basicamente dois tipos de Estado.

## Estado Implícito {#implicito}

Todo Objeto tem ou [deveria]({% post_url 2016-04-04-objetos-sem-estado %}) ter um Estado. No entanto existem casos especiais onde um Estado inicial não faz sentido *tecnicamente*.

Um exemplo de um Objeto sem Estado inicial é uma Lista. Podemos inicializar a lista com um item, porém nem sempre esse será o caso. Na maioria das vezes queremos uma lista para depois ir adicionando itens. Então iniciamos a lista sem Estado aparente.

Isso é fato se tivermos uma visão estritamente técnica. Mas quando essa lista for instânciada na memória do computador, seu Estado será inicializado. Em outras palavras, a área de memória que o Sistema Operacional concedeu ao Objeto será, também, o seu Estado.

Pensando *filosoficamente*, no
[construtor]({% post_url 2016-03-21-construtores-da-classe-primario-secundarios %}) de uma Classe Lista que é inicializada vazia, "sem estado", seria passado o endereço de memória onde a lista iria residir na memória do computador. Porém construtores já fazem isso por padrão — encontram um endereço de memória disponível — então não há necessidade de sabermos endereços de memória e muito menos passá-los no construtor.

Estados implícitos devem ser utilizados apenas para Objetos primários ou muito genéricos, como uma lista.

## Estado Explícito {#explicito}

Argumentos nos [construtores]({% post_url 2016-03-21-construtores-da-classe-primario-secundarios %}) de Classes mostram a intenção do desenvolvedor em instanciar um Objeto com um Estado Explícito.

No entanto tais argumentos podem ou não fazer parte do Estado do Objeto que queremos inicializar.

É o Objeto que irá decidir o que utilizar no seu Estado.

Por exemplo. Numa Classe `TSalary`, que representa um salário de alguém, é esperado um argumento no construtor do tipo `IMoney`, que é uma [Interface]({% post_url 2016-01-18-interfaces-em-todo-lugar %}).

A Classe `TSalary` poderia ter um outro construtor secundário com um argumento do tipo `string`. Porém sabemos que um *TSalary* não é uma *string* e seu Estado não pode ser inicializado com um valor que não seja um `IMoney`. Então porque esse construtor existe?

Um exemplo de implementação desses construtores poderia ser assim:
    
    // primary
    construtor TSalary.Create(Value: IMoney);
    begin
      FValue := Value;
    end;
    
    // secondary
    construtor TSalary.Create(const Value: string);
    begin
      Create(TStringAsMoney.New(Value));
    end;
    
No exemplo acima o construtor secundário chama o primário com o argumento correto, do tipo `IMoney`, porém outro Objeto é utilizado. O argumento `Value` do tipo *string* não faz parte do estado de `TSalary` porém faz parte do estado de `TStringAsMoney`.

Como eu disse antes, o Objeto que decide.

Estados explícitos devem ser utilizados em todos os Objetos de alto nível e de regras de negócio.

## Conclusão {#conclusao}

De todo esse papo técnico mas também filosófico, o que você nunca deve esquecer quando estiver construindo uma Classe é:

*O estado de um Objeto jamais deve ser violado.*

Após o Objeto ser criado, seu Estado — implícito ou explícito — nunca poderia ser alterado, permanecendo intacto até o fim da sua vida.

Se esse Estado for violado significa que seu Objeto não possui [encapsulamento]({% post_url 2016-05-30-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-2 %}#encapsulamento).

Um Objeto que não encapsula não pode ser considerado um Objeto. Chame-o de estrutura de dados.

Objetos tem Estado, estruturas tem Dados.

Até logo.
