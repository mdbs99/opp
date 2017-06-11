---
layout: post
title: Atributos Primários e Secundários
date: 2017-06-12
permalink: /:title
description:
  Um Objeto pode ter Atributos Primários e Secundários.
image: /images/2017/photo-tim-gouw-60216.jpg
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
  - atributo
  - primário
  - secundário
  - attribute
  - primary
  - secondary
---

Um Objeto pode ter Atributos Primários e Secundários.

<!--more-->

![Unsplash image]({{ page.image }})

## Introdução {#introducao}

Os [atributos]({% post_url 2016-11-28-menos-e-mais %}#atributos) de um Objeto são a representação do seu conhecimento, ou seja, tudo que o Objeto [encapsula]({% post_url 2016-05-30-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-2 %}#encapsulamento) é representado por atributos.

Assim como uma Classe pode ter 2 tipos de [construtores]({% post_url 2016-03-21-construtores-da-classe-primario-secundarios %}), um Objeto pode ter 2 tipos de atributos: Primários e Secundários.

## Primários {#primarios}

Atributos Primários são aqueles que fazem parte do [Estado do Objeto]({% post_url 2017-06-05-estado-do-objeto%}).

São os atributos inicializados pela Classe, passados ao Objeto através do construtor com a finalidade de representar uma [Entidade]({% post_url 2016-02-29-objetos-representam-entidades %}#o-que-e-uma-entidade).

Vejamos um exemplo:

<script src="https://gist.github.com/mdbs99/5952c6f39810803d91b05a6f1c4b8766.js"></script>

Na Classe `TFile` acima, há apenas 1 argumento no construtor. Esse argumento será parte do Estado do Objeto e, portanto, irá inicializar um Atributo Primário chamado `FFilePath`.

## Secundários {#secundario}

Atributos Secundários são todos os atributos que não foram inicializados pelo construtor da Classe.

São atributos de *suporte tecnológico*.

Vejamos um exemplo:

<script src="https://gist.github.com/mdbs99/48237c902271b6f2c3a314d3b6f5b8c0.js"></script>

Nesse exemplo acima a Classe `TFile` foi atualizada. Um novo atributo `FStream` foi adicionado.

Esse atributo não foi passado no construtor, então ele é um Atributo Secundário.

O método `Stream` irá retornar o *stream* do arquivo, no entanto os dados do arquivo em disco serão lidos apenas na primeira execução, pois após inicializar o atributo `FStream` ele não é mais atualizado.

A Classe `TDataStream` é irrelevante para o entendimento, mas veja que o *path* do arquivo é passado. Então essa Classe irá ler o arquivo em disco e retornar uma instância de `IDataStream`.

## Conclusão {#conclusao}

Atributos Primários são inicializados através do construtor da Classe e *devem* ser [imutáveis]({% post_url 2016-07-04-objetos-imutaveis %}).

Atributos Secundários podem ou não ser inicializados através do construtor da Classe, pois eles podem ser inicializados a qualquer momento e em qualquer lugar do Objeto.

Tudo são atributos, mas saber essa diferença sutil entre Atributos Primários e Secundários é essencial para construírmos Classes melhores.

Até logo.