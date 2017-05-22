---
layout: post
title: Observando Objetos
date: 2017-05-22
permalink: /:title
description:
  Nesse artigo iremos implementar Eventos que possam notificar mais de um Objeto de uma só vez.
image: /images/2017/photo-hermes-rivera-253620.jpg
categories: 
  - Projeto
tags:
  - projeto
keywords:
  - freepascal
  - fpc
  - delphi
  - lazarus
  - object-oriented
  - oop
  - mdbs99
  - eventos
  - events
  - observador
  - observer
---

Um semáforo de trânsito está prestes a mudar de vermelho para verde e vários pedestres estão aguardando para atravessar a rua.

A mudança de estado do semáforo irá impactar a vida de apenas algumas pessoas, ou seja, só daquelas que estão realmente interessadas, aguardando e observando este Evento que irá ocorrer em breve.

<!--more-->

![Unsplash image]({{ page.image }}) 

## Introdução {#introducao}

No [artigo anterior]({% post_url 2017-05-08-eventos-e-objetos %}) escrevi sobre [Eventos]({% post_url 2017-05-08-eventos-e-objetos%}#eventos) que ocorrem entre dois Objetos e como eles podem ser implementados de forma bem simples.

Nesse artigo iremos implementar Eventos que possam notificar mais de um Objeto de uma só vez.

## Observer Pattern {#observer-pattern}

Você pode utilizar um Evento para mostrar um *ProgressBar* enquanto um processamento está ocorrendo ou notificar o usuário quando algum outro processo terminar.

No entanto, em todos esses casos, os Eventos ocorrem entre apenas 2 Objetos: Transmissor e Receptor.

Há casos, porém, onde o Transmissor deverá notificar mais de um Receptor.

Há um padrão de implementação conhecido chamdo de [*Observer Pattern*](https://en.wikipedia.org/wiki/Observer_pattern).

Mas ao invés de só implementarmos o *Observer Pattern* na sua forma clássica, utilizando seus termos como *Subject* e *Observers*, vamos entender como esse padrão funciona, utilizando uma abordagem mais direta, simples e *"Pascalized"*.

## Implementação {#implementacao}

A maneira mais óbvia — ou talvez a única — para implementar uma notificação que seja propagada não em apenas um ponto do código, mas sim em infinitos pontos ou infinitos Objetos, é utilizando uma simples... *lista*.

A pergunta é: Onde estará a instância dessa lista?

A lista é global? Com certeza não, pois não trabalhamos com variáveis globais.

A lista é implementada no Objeto transmissor? Também não, pois o transmissor — no exemplo é um semáforo — não dá a mínima para quem o está observando. O transmissor apenas dá a *possíbilidade* de observação do evento, aceitando um ponteiro para o evento no [construtor]({% post_url 2016-03-21-construtores-da-classe-primario-secundarios %}), primário ou secundário.

Se não houvesse ninguém para observar o semáforo, ele iria parar de funcionar? Não! Portanto a *responsabilidade* de notificação a vários Objetos não é do transmissor original.

Então a lista é implementada em algum outro Objeto? Com certeza!

### 1-Notificando um Objeto {#exemplo-1}

Primeiramente vamos implementar um Evento simples, como no último artigo.

Teremos um Objeto `Semaphore`, que será o objeto transmissor de um Evento: Sempre que haver mudança de cor, o evento será disparado.

Como estamos trabalhando com Orientação a Objetos, antes precisamos definir o [contrato]({% post_url 2016-01-18-interfaces-em-todo-lugar %}#interfaces-sao-contratos), ou seja, a Interface que representa a entidade Semáforo.

Veja o código baixo.

<script src="https://gist.github.com/mdbs99/acfb7acf71670182b6cf509c51540949.js"></script>

Vamos entender o código acima:

1. Há uma Interface `ISemaphore` com apenas um Método `Color`. Não há Eventos na Interface.
2. O tipo `TChangeEvent` é a assinatura do Evento.
3. A classe `TSemaphore` implementa a Interface `ISemaphore`. Novamente, também não há Eventos na definição da Classe, apenas no construtor. O ato de notificação através de Eventos é um **detalhe de implementação** e não faz parte da entidade na qual a Interface e a Classe [representam]({% post_url 2016-02-29-objetos-representam-entidades %}).
4. A Classe `TSemaphore` instância um `TTimer`. É através desse *timer* que a mudança de cores irá ocorrer (a cada 2 segundos).

Agora, num novo projeto, adicionem um `TShape` num `TForm`, alterem a `property Shape` para `stCircle` e utilize o código abaixo:

<script src="https://gist.github.com/mdbs99/f4221d8c6be58201bbb5a82da26fe1a3.js"></script>

O código acima é *simples*, *elegante*, *Orientado a Objetos* e não possui *Setters*, nem mesmo para o Evento.

Se você fez tudo certo, verá algo parecido com a imagem abaixo — e a cada 2 segundos a cor é alterada.

![semaphore](/images/2017/screenshot-semaphore-1.jpg)

Mas, mesmo que tudo esteja funcionando, temos apenas 1 Objeto (o *Form*) que está "observando" o Evento de mudança de cor do semáforo.

O que queremos é poder notificar "N" Objetos de uma só vez.

### 2-Notificando muitos Objetos {#exemplo-2}

Para notificar muitos Objetos precisamos de uma lista com os Objetos previamente adicionados.

Vamos modificar o código existe e adicionar mais Interfaces e Classes.

Essas são as Interfaces:

<script src="https://gist.github.com/mdbs99/b5ebdc91138eb140bce07f5ef0dde976.js"></script>

São mais duas Interfaces com nomes bem genéricos. Talvez, num projeto maior, seus nomes poderiam ser `ISemaphoreObserver` e `ISemaphoreObservers` respectivamente, afim de diminuir as chances de haver conflito de nomes. Mas isso é irrelevante por enquanto.

Sendo nomes genéricos, eu posso implementar em Classes bem distintas que, inicialmente, não teriam nada haver com o contexto de Semáforo.

Confuso?

Vamos a um exemplo de implementação: *Pessoas* que gostariam de ser notificadas a respeito das mudanças do semáforo:

<script src="https://gist.github.com/mdbs99/74622575318450edc1fc40b8cd265f85.js"></script>

Vamos entender o código acima:

1. Primeiramente, o *GUID* na Interface `IObserver` serve apenas para ser possível fazer o *casting* no método `Get` de `TPeople`. Fique a vontade para usar *Generics*.
2. A Interface `IObservers` é um subtipo de `IObserver` apenas para aproveitar a definição do método `Changed`.
3. O método `Changed` faz parte da definição da Interface `IObserver`, sim, porque a entidade que ela representa necessita: Um observador precisa saber das mudanças do Objeto observado.
4. Um Objeto `TPeople` é apenas uma lista que irá repassar o Evento para seus itens.
5. Quando um Objeto `TPerson` tem seu método `Changed` chamado na lista, ele irá gerar um novo Evento `OnStatus`. Esse "status" é apenas para notificar o que o Objeto está fazendo.

Ok. Só falta agora ligar tudo isso junto.

Vamos alterar um pouco o código inicial do *Form*:

<script src="https://gist.github.com/mdbs99/31851f1e8e5d70f893ab84c0df2614f7.js"></script>

Vamos entender o código acima:

1. Adicionei um `TMemo`.
2. Adicionei um método privado `PersonStatus`.
3. Renomeei `FSemaphore` para `FSemaphore1` e criei outro do mesmo tipo.
4. Adicionei um atributo `FPeople`.
5. Por fim foi alterado o `FormCreate`.

Acho que você já entendeu o que irá ocorrer aqui, certo?

Se você replicou tudo certo aí, verá uma imagem parecida com essa abaixo:

![semaphore](/images/2017/screenshot-semaphore-2.jpg)

## Conclusão {#conclusao}

Utilizar Eventos não quer dizer, necessariamente, que você está utilizando RAD.

Eventos são necessários para unir duas peças que, talvez, não tenham nada em comum mas que precisam se comunicar.

Nesse artigo tentei mostrar uma maneira de implementar eventos de forma mais Orientada a Objetos, utilizando Interfaces e Classes e não apenas ponteiros para métodos.

Mostrei que a lista de notificações não deve ficar no transmissor. Assim poderíamos acrescentar mais listas de notificações. Listas que notificam listas, se necessário. E os itens dessas listas poderiam ser instâncias de Classes complemente diferentes umas das outras, assim como o que iriam fazer — seu comportamento — ao receber a notificação da lista. Isso deixa nosso código bem extensível, sem que precisemos alterar o que já foi concluído.

Até logo.
