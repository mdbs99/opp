---
layout: post
title: "Builder Classes Simplificado"
date: 2018-09-11
permalink: /:title
description:
  Nesse artigo irei utilizar um Builder simplificado, que irá utilizar a mesma classe tanto para construir como para representar a instância final.
image: /images/2018/christopher-burns-360244-unsplash.jpg
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
  - builder
---

O padrão Builder utiliza no mínimo 2 classes: 1 classe para instância a ser construída e 1 classe para a instância construtora.

Nesse artigo irei utilizar um Builder simplificado, que irá utilizar a mesma classe tanto para construir como para representar a instância final.

<!--more-->

![Unsplash image]({{ page.image }})
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Christopher Burns on Unsplash</span>

O padrão [Builder](https://en.wikipedia.org/wiki/Builder_pattern) já é muito conhecido e utilizado mundo afora.

Esse artigo não tem a pretensão de ensiná-lo para que serve esse padrão — há milhares de artigos sobre isso na Internet, com implementação em várias linguagens diferentes.

No entanto, um bom resumo é encontrado na Wikipedia:

> "Separar a construção de um objeto complexo da sua representação de modo que o mesmo processo de construção possa criar diferentes representações."

Como podemos ver nos exemplos em C# da Wikipedia, foi utilizado uma classe `Car` e uma interface `ICarBuilder`. Depois, temos uma implementação dessa interface com a definição da classe `FerrariBuilder` e, finalmente, outra classe para customizar a Ferrari chamada `SportsCarBuildDirector`.

Podemos simplificar esse exemplo?

Primeiramente, `Car` poderia ser uma interface `ICar` e não uma classe. Então, poderíamos ter a classe `TFerrari` que implementa `ICar`.

Na minha opinião, o único `Builder` do exemplo é a classe `TSportsCarBuildDirector` que customiza uma Ferrari para diretores. No entanto, eu também não concordo com sua existência e acho que podemos simplificar ainda mais esse exemplo — veja mais abaixo.

Vamos começar pela primeira interface, mantendo-a [simples]({% post_url 2016-12-19-simplicidade %}), retornando tipos primitivos:

    ICar = interface
      function Model: string;
      function NumDoors: Integer;
      function Color: TColor;
      procedure Run;
    end;      

Depois, codificamos a classe `TFerrari`, que implementa `ICar`. Veja que essa classe não é um Builder, mas sim apenas uma classe comum.

    TFerrari = class(TInterfacedObject, ICar)
    private
      fModel: string;
      fNumDoors: Integer;
      fColor: TColor;
    public
      constructor Create(const aModel: string;
        aNumDoors: Integer; aColor: TColor);
      function Model: string;
      function NumDoors: Integer;
      function Color: TColor;
      procedure Run;
    end;

Como disse acima, não acho necessidade de haver um `Builder` chamado `TSportsCarBuildDirector`. Para implementar essa classe seria necessário implementar uma nova interface `ICarBuilder` além da própria classe, o que seria desnecessário.

Podemos simplificar esse exemplo utilizando [herança]({% post_url 2017-03-06-como-utilizar-heranca-apropriadamente %}) dessa forma:

    TDirectorSportCar = class(TFerrari)
    public
      constructor Create(aColor: TColor); reintroduce;
    end;

    constructor TDirectorSportCar.Create(aColor: TColor);
    begin
      inherited Create('488 Spider', 2, aColor);
    end;

Acabamos de criar um Builder mas utilizando a própria classe que irá representar um `ICar`, ou seja, a `TDirectorSportCar`. Não precisamos de mais uma interface abstrata como `ICarBuilder` ou sua implementação.

Para demonstrar o código, poderíamos ter algo assim:

    procedure DoSomethingWithCars;
    var
      car: ICar;
    begin
      car := TDirectorSportCar.Create(clRed);
      car.Run;
    end;

Sem Builders (aparentes), sem o método "Construct", sem o método "GetResult", e utilizando objetos [imutáveis]({% post_url 2016-07-04-objetos-imutaveis %}).

Até logo.

