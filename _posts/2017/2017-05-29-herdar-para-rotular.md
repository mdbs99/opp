---
layout: post
title: Herdar para Rotular
date: 2017-05-29
permalink: /:title
description:
  A Herança de Classe pode ter outras finalidades que você ainda não havia pensado.
image: /images/2017/photo-matt-briney-160808.jpg
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
  - herança
  - inherited
  - label
  - rótulo
---

A Herança de Classe pode ter outras finalidades que você ainda não havia pensado.

<!--more-->

![Unsplash image]({{ page.image }}) 

## Introdução {#introducao}

Já escrevi bastante sobre Herança [aqui]({% post_url 2016-05-23-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-1 %}) (série de artigos em 5 partes) e vimos o mal que seu uso pode trazer ao código Orientado a Objetos.

No entanto, assim como uma arma de fogo pode ser inofensiva, pois é o ser humano que faz mal uso da arma, a herança pode ser inofensiva se for utilizada de forma [apropriada]({% post_url 2017-03-06-como-utilizar-heranca-apropriadamente %}).

Nesse artigo você irá ver mais uma forma de uso da Herança de Classe, mas sem sobrescrita de métodos ou uso de propriedades e métodos protegidos.

Vamos utilizar a Herança para *rotular*.

## Contextualizando {#contextualizando}

De vez em quanto eu costumo utilizar Herança de Classe apenas como um rótulo.

Para que você entenda o benefício do uso de um rótulo, é necessário haver uma contextualização para compararmos o uso do código sem rótulo e, depois, com rótulo.

Vamos ver um exemplo.

<script src="https://gist.github.com/mdbs99/0676083483d1d9b75f21b8527e6e4897.js"></script>

É uma Classe bem simples, apenas para exemplificar o que eu quero lhe mostrar.

Se você não entendeu para que serve o método *New*, veja sobre ele [aqui]({% post_url 2016-01-10-interfaces-e-o-metodo-estatico-new %}).

Toda Classe deve implementar ao menos uma [Interface]({% post_url 2016-01-18-interfaces-em-todo-lugar %}), então temos a Interface `IMoney` com apenas um único método `Value: Currency`.

Imagine agora que seu *software* trata de vários tipos de Taxas e Descontos, mas vou citar apenas dois como exemplo:

1. Taxa de Inscrição (1%)
2. Desconto (10% se menor de 18 anos)

Você, sendo um dos programadores numa empresa de Cursos de Mecânica de Automóveis, foi encarregado de implementar o cálculo de Inscrição e Desconto.

Se o valor da inscrição é R$ 100 vamos imaginar um botão para apenas mostrar o valor total a pagar pelo novo aluno:

<script src="https://gist.github.com/mdbs99/95395ad64ac9d1a7b3eed55ebf436724.js"></script>

O código acima tem uma condicional, proporcionado pela classe `TIf` (não relacionada nesse artigo), que é bem simples de entender:

Se o (futuro) estudante tem idade menor do que 18 anos, ele irá ter um desconto de 10%, senão não haverá desconto.

Depois há a função primitiva `CurrToStr` para converter o somatório do Valor Inscrição + a Taxa de Inscrição (1%) - o Desconto (se houver) em `string` para ser exibido num simples `ShowMessage`.

Bem, você sabe isso porque eu estou lhe dizendo. Eu preparei o contexto antes que você visse o código. Se fosse tivesse visto apenas isso...

`TPercentCalculated.New(V, 1).Value`

...saberia que se trata de uma Taxa que calcula 1% do valor, mas não saberia dizer sobre *qual* taxa o código se refere.

É aí que usamos um rótulo.

## Rotulando {#rotulando}

Um rótulo serve para marcar alguma coisa para facilitar a visão, a leitura, a identificação.

Uma Classe também é um rótulo.

O código abaixo é a refatoração do código que lhe acabei de mostrar:

<script src="https://gist.github.com/mdbs99/02a4ab8115cbebe3fe671905d51c1dd1.js"></script>

O código agora ficou muito mais [simples]({% post_url 2016-12-19-simplicidade %}).

Está claro que o `valor` é somado com uma Taxa de Inscrição e com um Desconto.

O formulário que contém esse código, não "sabe" qual é o percentual da Taxa de Registro e nem como é feito o cálculo para o Desconto... e é assim que deve ser.

Então a primeira refatoração seria criar 2 classes distintas, uma para cada tipo de cálculo. Mas aí teríamos um problema: [Código duplicado]({% post_url 2017-01-09-codigo-duplicado-talvez-nao %}).

Se o cálculo é o mesmo, ou seja, apenas um percentual simples, porque eu iria duplicar o mesmo código em duas Classes? Não faz sentido.

Mas podemos fazer isso:

<script src="https://gist.github.com/mdbs99/0e8ac1cd0af1f631b5204061c554dbaf.js"></script>

Agora ambas as Classes tem como herança `TPercentCalculated`. No entanto eu não estou sobrecarregando nenhum Método e não estou usando nada protegido... na verdade esse código não está *tocando em nada* da super Classe.

Ambas as Classes apenas inicializam, de forma apropriada e contextualizada, a Classe `TPercentCalculated` utilizando o [construtor]({% post_url 2016-03-21-construtores-da-classe-primario-secundarios %}) da super Classe.

Apenas isso.

Mas para o utilizador das Classes, elas parecem ser completamente independentes.

Até logo.
