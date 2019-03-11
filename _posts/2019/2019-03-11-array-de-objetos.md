---
layout: post
title: "Array de Objetos"
date: 2019-03-11
permalink: /:title
description:
  Arrays são ótimas opções para trabalhar com objetos em memória e passagem de dados entre contextos.
image: /images/2019/antonio-garcia-339626-unsplash.jpg
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
  - array
  - dynamic array
---

Quando você precisa de uma simples lista de objetos, qual estrutura você utiliza?

<!--more-->

![Unsplash image]({{ page.image }})
<br><span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Antonio Garcia on Unsplash</span>

## Introdução {#introducao}

Todo sistema utiliza listas para armazenar desde valores primitivos até instancias de objetos.

Aqui eu sempre advoguei sobre utilizar objetos em (quase) todos os lugares. Mas temos que ser eficientes e não desperdiçar recursos. Não devemos criar estruturas mais complexas, como classes e interfaces, quando outra estrutura pode resolver o problema, além de simplificar a solução.

Basicamente uma lista pode ser representada por um array, um [objeto genérico]({% post_url 2018-02-19-listas-genericas-sem-generics %}) (ex: `TList`) ou um objeto especializado que possui regras de negócio que irão trabalhar com os elementos de acordo com requisitos *específicos* do usuário.

Se você não precisa de uma lista com [regras de negócio]({% post_url 2017-01-09-codigo-duplicado-talvez-nao %}#regras-de-negocio), arrays podem ser um boa escolha.

## Array vs Lista {#vs}

Poderíamos utilizar lista de objetos em todos os lugares, mas os arrays continuam relevantes.

Arrays armazenam informações no *stack*, enquanto Listas utilizam o *heap*.

A utilização do stack tem muito mais performance pois o acesso a esse tipo de memória é muito mais rápido. A alocação da memória é determinada já na compilação do programa, tornando fácil o seu gerenciamento.

O stack é protegido e mais rápido, pois é mais fácil para o compilador alocar/desalocar memória lá. Entretanto, ele é mais limitado quanto ao espaço, comparado ao heap. O heap é "ilimitado" (pode ocupar toda a memória RAM), porém mais lento. Sendo o heap compartilhado por toda a aplicação, é mais difícil/custoso para o compilador alocar/desalocar a memória. E, sendo esses dados "globais" à aplicação, eles muitas vezes devem ser sincronizados quando os acessamos utilizando multi-threading.

Então, como escolher entre eles?

Podemos definir 3 tópicos básicos que você deverá levar em conta, quando for tomar sua decisão: Performance, Regras de Negócio e Transferência de Dados.

### Performance {#performance}

Você usa o stack quando souber a quantidade de dados à alocar na memória já em tempo de compilação. No entanto, não deve ser uma massa muito grande de dados—mesmo havendo configurações/diretivas em cada compilador para customização—ou você poderá obter um erro de memória em tempo de execução.

O heap é utilizado quando você não souber, exatamente, quantos dados irá precisar em tempo de execução ou se houver a tendência de que será uma massa muito grande de dados.

O stack sempre será mais rápido que o heap. Então, se você necessitar da máxima performance, tenha isso em mente quando tiver instanciando novas listas de objetos.

### Regras de Negócio {#regras}

Imagine um sistema de estacionamento de veículos. Se você tem uma classe que representa o estacionamento, então você tem uma lista especializada para armazenar as vagas e carros ou ao menos uma lista genérica dentro da classe especializada.

Você teria regras de negócios referente ao estacionamento como: horário de entrada/saída, número da vagas, serviço de manobrista, etc. Não vem ao caso pensar em todas as regras de negócio de um estacionamento. O mais importante é sabermos que *haveria* regras de negócio para essa *lista especializada*, o estacionamento.

Apesar da maioria dos estacionamentos terem um número limitado de vagas, virtualmente pode-se adicionar mais carros em "vagas não-oficiais", deixando um total de vagas "em aberto"—se isso não acontece na sua cidade/país, bem, apenas considere a possibilidade.

Se o número de vagas é bem grande ou incerto, a melhor opção de escolha nesse caso seria uma lista especializada, alocada no heap, com outros métodos adicionais que implementam as regras de negócio.

### Transferência de Dados {#transferencia}

Se sua lista existe apenas para armazenar temporariamente objetos ou para passagem de parâmetro entre contextos, por exemplo, um simples array poderia ser utilizado.

Chamo de transferência de [dados]({% post_url 2016-11-07-pensando-em-dados %}), pois você poderá transferir desde elementos simples e primitivos até objetos complexos de listas especializadas, entre contextos distintos. Esse seria o meio-termo entre performance vs especialização. Você pode ter listas de objetos, carregando instâncias de objetos complexos, mas utilizar os arrays para transportar apenas parte desses objetos de forma eficiente.

Exemplo. A sua lista de vagas acima, o estacionamento, possui um número X de vagas em uso em um determinado momento. Cada vaga pode ser representada por uma instância. Imagine que o usuário selecionou 3 vagas para "deixar livre". Após a seleção, é necessário fazer algumas operações com essas instâncias (vagas). Essas operações irão receber um "array de vagas" como parâmetro.

O array seria utilizado por quê você já sabe a quantidade de elementos que ele iria ter—determinado pela seleção do usuário. Criar uma nova lista de vagas (do tipo estacionamento) para adicionar apenas 3 instâncias e então passar como parâmetro seria um desperdício de processamento, além de não fazer muito sentido passar "todo um estacionamento"—uma instância da mesma classe—ou uma cópia da lista oficial com menos registros, apenas para fazer o processamento da liberação das vagas.

Um array seria a escolha mais simples. No entanto, não há só um tipo de array.

## Utilizando Arrays Dinâmicos {#utilizando}

Existem alguns tipos de arrays como estáticos, constantes, dinâmicos, multi-dimensionais, etc.

Para esse artigo, iremos utilizar os *arrays dinâmicos*.

Utilizar arrays dinâmicos no Object Pascal é relativamente fácil, porém temos que prestar atenção em duas regras principais:

  1. O índice do primeiro elemento sempre será 0 (zero);
  1. É necessário determinar o tamanho do array antes de inserir seus elementos;

**O índice** do primeiro elemento sempre será zero, diferentemente de arrays contantes, por exemplo, onde você pode informar outro valor para o primeiro elemento. Tenha sempre isso em mente quando utilizar o array em loops.

**O tamanho** de um array é determinado pela função `SetLength(array, size)` padrão.

Abaixo um exemplo do uso da função e a carga de um `array of Integer` com valores:

    var
      a: array of Integer;
    begin
      SetLength(a, 10); // from 0 to 9 positions
      for i := 0 to 10 do
        a[i] := i;
    end;

Diferentemente de classes e interfaces, para definir um array para armazenar elementos de um determinado tipo, basta apenas uma linha de código, como visto acima.

Foi definido um array para `Integer`, mas a mesma sintaxe vale para tipos de classes ou interfaces. Se estivermos trabalhando com Vagas—instâncias da interface `ISpot`, por exemplo—poderíamos definir um array dessa forma:

    type
      TSpotArray = array of ISpot;

Não precisamos nos preocupar com herança, métodos ou sobrescrita de métodos, caso estivéssemos definindo uma nova classe.

Apenas uma única linha define um novo *container* dinâmico de instâncias de `ISpot`.

Utilizando uma variação do algoritmo acima, você é capaz de inicializar o array e adicionar os 3 itens que o usuário selecionou, passando-o como um parâmetro para o algoritmo que irá desalocar as vagas.

Não há necessidade de se preocupar com o desalocamento de memória, pois tudo será feito automaticamente pelo compilador.

## Conclusão {#conclusao}

Arrays são ótimas opções para trabalhar com objetos em memória e passagem de dados entre contextos.

Eles são [simples]({% post_url 2016-12-19-simplicidade %}), gerenciados pelo compilador no stack e muito mais rápidos que listas de objetos.

Até logo.
