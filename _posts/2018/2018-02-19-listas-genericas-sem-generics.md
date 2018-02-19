---
layout: post
title: "Listas Genéricas, sem Generics"
date: 2018-02-19
permalink: /:title
description:
  É possível utilizarmos listas genéricas, com checagem de tipo, mas sem o uso da sintaxe com *Generics*?
image: /images/2018/photo-john-murzaku-269690.jpg
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
  - generics
---

É possível utilizarmos listas genéricas, com checagem de tipo, mas sem o uso da sintaxe com *Generics*?

<!--more-->

![Unsplash image]({{ page.image }})
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by John Murzaku on Unsplash</span>

Muitos programadores adoram utilizar novas *features* dos compiladores. Uma dessas "novidades" — bem, já faz alguns anos — é o uso de Generics.

Com Generics é possível, por exemplo, termos listas genéricas que podem ser utilizados com qualquer tipo de classe ou interface. Generics possuem uma "checagem estática" de tipo, tanto para armazenamento como para a recuperação dos itens da lista.

Ao invés de termos as "velhas" listas de ponteiros, fazendo *casting* dos objetos quando acessamos seus itens, hoje, utilizando Generics, podemos utilizar classes de listas já existentes na VCL/LCL para instanciar e manter uma lista de objetos que irão trabalhar com os tipos já definidos no nosso código.

Na declaração de uma variável de uma lista genérica, informamos o *tipo* dos itens que irão compor a lista e o compilador faz o resto do trabalho.

Esse artigo não irá abordar as vantagens e desvantagens no uso de Generics. Ao invés disso, irei apenas mostrar como *simular* uma lista genérica utilizando Object Pascal "clássico".

Por quê isso é relevante?

Existem alguns motivos como, por exemplo, estar trabalhando com código legado numa versão do compilador sem suporte a Generics; almejar uma melhor performance, mesmo utilizando objetos; manter projetos (libs e frameworks) que podem ser utilizados desde os compiladores mais antigos até a versão mais atual.

O problema de termos uma lista genérica, sem o uso de Generics, é o *casting* que precisaríamos fazer todas as vezes que recuperarmos os itens da lista.

O mesmo *casting* seria [replicado]({% post_url 2017-01-09-codigo-duplicado-talvez-nao %}) várias vezes, em todos os pontos que fossem necessário recuperar os objetos.

Por exemplo:

    uses
      Contnrs;

    type
      TFoo = class
        Name: string;
      end;

    procedure TForm1.Button1Click(Sender: TObject);
    var
      F: TFoo;
      OL: TObjectList;
    begin
      OL := TObjectList.Create(True);
      try
        F := TFoo.Create;
        F.Name := 'Bar';
        OL.Add(F);
        ShowMessage(
          (OL.Items[0] as TFoo).Name // <<< casting
        );
      finally
        OL.Free;
      end;
    end;

O código acima utiliza a classe `TObjectList` da unit `Contnrs`. Essa classe trabalha com itens de objetos. Em seu construtor, um argumento `boolean` indica que os objetos serão automaticamente liberados quando a lista for destruída.

Funciona. Mas, o inconveniente é termos que fazer o casting na chamada de `Items[]`.

No entanto, é possível não utilizar casting, mesmo sem o uso de Generics. Para tal, vamos utilizar apenas classes simples e sintaxe clássica de Object Pascal.

Primeiro, precisamos codificar nossa própria lista de objetos, [sem herdar diretamente de classes de infraestrutura]({% post_url 2017-03-06-como-utilizar-heranca-apropriadamente %}#classes-de-suporte):

    type
      TMyObjects = class
      private
        FList: TObjectList;
      public
        constructor Create;
        destructor Destroy; override;
        function Add(Obj: TObject): TMyObjects;
        function Get(Index: Integer): TObject;
        function Count: Integer;
    end;

    { TMyObjects }

    constructor TMyObjects.Create;
    begin
      FList := TObjectList.Create(True);
    end;

    destructor TMyObjects.Destroy;
    begin
      FList.Free;
      inherited;
    end;

    function TMyObjects.Add(Obj: TObject): TMyObjects;
    begin
      Result := Self;
      FList.Add(Obj);
    end;

    function TMyObjects.Get(Index: Integer): TObject;
    begin
      Result := FList.Items[Index];
    end;

    function TMyObjects.Count: Integer;
    begin
      Result := FList.Count;
    end;

A classe `TMyObjects` poderá ser utilizada para armazenar qualquer tipo de instância de `TObject`. Mas, o inconveniente ainda permanece, pois ao utilizar `Get(Index)` o casting será necessário.

Segundo, a solução mais [simples]({% post_url 2016-12-19-simplicidade %}) é utilizarmos [herança de classe]({% post_url 2016-05-23-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-1 %}) e *reintroduzir* o método que recupera os objetos, utilizando uma nova assinatura.

    type
      TFooObjects = class(TMyObjects)
      public
        function Get(Index: Integer): TFoo; reintroduce;
      end;

    { TFooObjects }

    function TFooObjects.Get(Index: Integer): TFoo;
    begin
      Result := inherited Get(Index) as TFoo;
    end;

A nova classe `TFooObjects` tem uma nova assinatura de `Get()`. Isso quer dizer que quando o desenvolvedor utilizar o *code-completion* da IDE, apenas essa "versão" do `Get(Index: Integer): TFoo` será exibida.

Então, podemos alterar o código de teste anterior para utilizar a nova classe:

    procedure TForm1.Button1Click(Sender: TObject);
    var
      F: TFoo;
      OL: TFooObjects;
    begin
      OL := TFooObjects.Create;
      try
        F := TFoo.Create;
        F.Name := 'Bar';
        OL.Add(F);
        ShowMessage(OL.Get(0).Name);
      finally
        OL.Free;
      end;
    end;

Sem casting, limpo e mais "elegante" do que utilizar Generics.

A mesma técnica deve ser utilizada para o método `Add(Obj)` afim de certificarmos que apenas instâncias de `TFoo` serão armazenadas.

A classe `TMyObjects` foi codificada apenas uma vez e poderá ser reutilizada como base para várias outras classes.

Entretanto, alguns desenvolvedores poderão sugerir que tivemos que implementar "muito" código para ter a mesma funcionalidade que a sintaxe de Generics já nos dá. Bem, isso depende do ponto de vista. Além dos motivos sugeridos nesse artigo, o *não* uso de Generics pode fazer muito sentido quando estamos trabalhando na Camada de Negócio, onde o código deve representar as Regras de Negócio do cliente. Uma classe "inchada" com dezenas de métodos de manipulação de itens na lista pode não representar fielmente, por exemplo, uma "lista de produtos" do cliente.

Finalmente, é você quem vai decidir sobre os *trade-offs* envolvidos no design do seu código. Mas, sempre tenha em mente que nem tudo que é *antigo* será pior que o mais *novo*.

Até logo.