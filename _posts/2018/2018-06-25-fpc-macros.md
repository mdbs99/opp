---
layout: post
title: "Free Pascal Macros"
date: 2018-06-25
permalink: /fpc-macros
description:
  Uma linguagem simples e como um design elegante como Object Pascal não necessitaria de suporte a macros como existem em C/C++. No entanto, Free Pascal as tem. Devemos utilizá-las?
image: /images/2018/remi-jacquaint-441559-unsplash.jpg
tags:
  - fpWeb
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
  - macro
---

Uma linguagem simples e como um design elegante como Object Pascal não necessitaria de suporte a macros como existem em C/C++. No entanto, Free Pascal as tem. Devemos utilizá-las?

<!--more-->

![Unsplash image]({{ page.image }})
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Rémi Jacquaint on Unsplash</span>

## Introdução {#introducao}

O uso de [macros](https://en.wikipedia.org/wiki/Macro_(computer_science)) já existe no Free Pascal faz tempo, porém nunca havia pensando utilizar tal feature.

Se você é leitor assíduo do blog já deve ter percebido que eu prefiro ter um bom design e consistência no código do que utilizar "hacks" para escrever menos, porém com a perda da legibilidade e da manutenibilidade.

Na minha opinião, macros são *hacks*.

Bem, talvez a maioria...

Não acho que que o suporte a macros não tenha sido implementado com o objetivo de melhorar o design e legibilidade do código.

No entanto, acredito que podemos tirar proveito dessa feature.

## Macros {#macros}

No código Pascal podemos definir macros que serão pré-processadas pelo compilador. Esse processamento irá gerar um resultado, que será como uma constante. Mas, essa constante pode representar uma expressão, um algorítimo, uma unidade, etc.

Vejamos um exemplo:

    {$macro on}
    {$define VALUE := A+B}

    function Sum(A, B: Integer): Integer;
    begin
      Result := VALUE;
    end;

    function Concat(const A, B: string): string;
    begin
      Result := VALUE;
    end;
    { TForm1 }

    procedure TForm1.Button1Click(Sender: TObject);
    begin
      ShowMessage(Sum(1,2).ToString); // 3
      ShowMessage(Concat('A','B'));   // 'AB'
    end;

A primeira coisa a se fazer é ativar o uso de macros utilizando a diretiva `{$macro on}`, pois macros não são ativadas por padrão. Outra maneira de ativá-as para todas as unidades é passando o argumento `-Sm` para o compilador.

O exemplo acima não faz muito sentido, pois para se fazer uma soma não é necessário o uso de macros. No entanto, serviu para lhe mostrar o que é possível fazer com elas.

Repare que `A` e `B` são definidos na macro como 2 identificadores que ainda não existem. Esses identificadores virão do contexto onde as macros serão chamadas, ou seja, as funções `Sum` e `Concat`.

Para a macro, não importa o tipo de `A` e `B`. Então, elas funcionam como *Generics*, o que já é bem legal.

Mas *Generics* são muito mais poderosos do que isso, então não pense em macros como substitutos aos Generics.

Vamos a outro exemplo onde Generics não ajudam em nada:

    {$macro on}
    {$ifdef debug}
      {$define foo := foo.debug}
    {$else}
      {$define foo := foo.run}
    {$endif}

    uses
      foo;
    begin
      foo.SomeFunc;
    end.

Nesse exemplo acima, há um teste se a diretiva `debug` foi definida. Se for verdadeiro, definimos uma macro `foo` com o nome da unidade `foo.debug`, senão definimos `foo` como `foo.run`.

Repare que na cláusula `uses` há somente `foo`. Imagine agora por as diretivas em outro lugar, talvez arquivos `.inc`, e teremos uma unidade com o código limpo que irá utilizar as classes corretas dependendo da diretiva escolhida.

Bem legal, não é?

Entretanto, isso também não foi o que mais me chamou atenção no uso das macros. Pois, para fazer o mesmo que no exemplo acima, não é preciso macros. Basta você ter unidades como o mesmo nome, porém em diretórios diferentes, e passar o path correspondente ao compilador dependendo se há ou não a diretiva `debug`. O Lazarus IDE lhe dá essa opção.

Se há outras alternativas na própria linguagem para o uso de macros, por quê então utilizá-las?

## Unit Alias {#alias}

Faz muito tempo que solicitei a implementação de uma nova sintaxe à equipe Free Pascal para ser possível [redeclarar]({% post_url 2017-08-21-redeclarando-classes %}) uma unidade diretamente no código. A linguagem C# tem isso, assim como Python.

A razão para esse pedido é poder declarar unidades com nomes longos no código, mas dar-lhes um apelido curto para servir de prefixo a algumas classes que tem nomes iguais em unidades diferentes.

Então pensei numa alternativa que chamei de [API Unit]({% post_url 2017-10-30-api-unit %}). As vantagens e desvantagens estão no artigo.

Infelizmente essa ideia não se mostrou muito eficaz para projetos com muitas unidades, já que é necessário redeclarar todas as unidades, suas classes e interfaces em um único arquivo.

Mas com macros, tudo muda.

Hipoteticamente, vamos supor que as unidades `Windows` e `Graphics` pertencem ao namespace `FPC.RTL.`, ou seja, um "nome longo".

Então, minha proposta (antiga) seria utilizar essa sintaxe:

    uses
      FPC.RTL.Windows as Win,
      FPC.RTL.Graphics as Graph;
    var
      B1: Win.TBitmap;
      B2: Graph.TBitmap;

Eu não estou me referindo aqui o uso de namespaces do Delphi — que já foi implementado no Free Pascal — onde poderíamos definir um `NS="FPC.RTL"` e declarar apenas `Windows` e `Graphics` respectivamente.

Não é isso.

Utilizando essa sintaxe, teríamos o *controle local* sobre a nomenclatura das unidades.

Quando solicitei isso na época, eu pensava que iria exigir mudanças no parser da linguagem, que daria muito trabalho, etc.

Não era prioridade.

Porém, veja que com o uso de macros, bastaria o compilador substituir os `uses` acima, que utiliza a palavra reservada `as`, pelo código abaixo:

    uses
      {$macro on}
      FPC.RTL.Windows,
      {$define Win := FPC.RTL.Windows}
      FPC.RTL.Graphics;
      {$define Graph := FPC.RTL.Graphics}
    var
      B1: Win.TBitmap;
      B2: Graph.TBitmap;

E então eu poderia "renomear" as unidades com nomes longos como `Acme.SysFoo.Finances.Billing.Utils.Classes` para apenas `Billing`. Porém, em outra unidade, eu poderia renomear para `Utils` se assim o desejasse.

Teríamos a possibilidade de definir um apelido local (por unidade) a cada unidade. Perfeito.

## Conclusão {#conclusao}

A resposta a pergunta inicial é, sim. O uso de macros, quando bem utilizado, pode melhorar muito a legibilidade e design do código.

Não temos (ainda?) a sintaxe com o uso de `as`, que seria apenas uma *syntax sugar* para o uso de macros, porém já podemos renomear unidades localmente. O que é um grande avanço, na minha opinião.

O único contra que eu vejo é que, infelizmente, essa feature não existe no Delphi... por enquanto?

Até logo.
