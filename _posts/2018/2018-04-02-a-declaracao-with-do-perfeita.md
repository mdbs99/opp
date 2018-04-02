---
layout: post
title: "A Declaração WITH-DO Perfeita"
date: 2018-04-02
permalink: /:title
description:
  A utilização do WITH-DO pode deixar o código mais simples de ler, pois haverá menos declarações, atribuições e inicializações de variáveis. No entanto, a sintaxe atual não é perfeita. Dependendo do seu uso, o benefício pode ser o inverso.
image: /images/2018/photo-alex-wong-17993-unsplash.jpg
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
  - with-do
---

A utilização do WITH-DO pode deixar o código mais simples de ler, pois haverá menos declarações, atribuições e inicializações de variáveis. No entanto, a sintaxe atual não é perfeita. Dependendo do seu uso, o benefício pode ser o inverso.

<!--more-->

![Unsplash image]({{ page.image }})
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Alex wong on Unsplash</span>

## Introdução {#introducao}

A declaração WITH-DO [pode ser utilizada]({% post_url 2016-09-12-a-declaracao-with-do-e-do-mal %}) mas sua sintaxe não é perfeita.

Vai depender muito do design do seu código para que você possa utilizar o WITH-DO sem problemas.

Por exemplo. Classes com muitos atributos ou métodos poderá aumentar a probabilidade de haver alguma [colisão de nomes]({% post_url 2016-04-25-nomeando-classes %}) entre outras classes e/ou variáveis locais.

Esse artigo é a minha proposta para melhorar a sintaxe do WITH-DO nos compiladores Object Pascal.

## A Sintaxe Perfeita {#sintaxe}

O grande problema ao utilizarmos WITH-DO é a ambiguidade que ele pode trazer ao código.

Considere o código abaixo:

    type
       TFoo = class
          X: Integer;
       end;

    procedure Execute;
    var
      X: Integer;
    begin
      with TFoo.Create do
      try
         X := 10;
      finally
         Free;
      end;
    end;

A linha <code>X := 10;</code> atribui 10 ao atributo de <code>TFoo</code> ou a variável local <code>X</code>?

Outro problema é o seguinte: se renomearmos <code>TFoo.X</code> para <code>TFoo.Z</code> poderíamos esperar um erro de compilação, no entanto o código iria continuar funcionando devido ao compilador localizar o identificador <code>X</code> como uma variável local.

A ambiguidade pode ser ainda maior quando utilizamos múltiplas instancias na declaração WITH-DO, como a seguir:

    with TFoo.Create, TBar.Create do
    try
      X := 10;
      Z := 20;
    finally
      Free; // belongs to TFoo or TBar?
      Free; // belongs to TFoo or TBar?
    end;

Esse são apenas alguns casos do uso irrestrito do WITH-DO. E, por essas e outras razões, que muitos desenvolvedores consideram o WITH-DO um mal no código.

No entanto, o que não está correto não é seu uso, mas sua sintaxe.

Na minha opinião, seria muito melhor se a sintaxe fosse algo parecido com o exemplo abaixo:

    procedure Execute;
    begin
      with F =: TFoo.New, B =: TBar.New do
      begin
        F.Execute;
        B.Execute;
      end;
    end;

No código acima não há variáveis declaradas explicitamente, no entanto as instâncias são referenciadas por algum tipo de <i>alias</i>.

Afim de diferenciar essa nova sintaxe, para não haver ambiguidades com a sintaxe já existente de atribuição, escolhi utilizar uma "atribuição invertida" neste formato "<code>=:</code>".

Alguns puristas do Pascal iriam dizer que o código acima não é muito "Pascalish" por quê tudo no Pascal deve ser explicitamente declarado antes e, neste caso, as variáveis não foram declaradas previamente. Eu concordo. Porém acho que deveríamos considerar essa exceção.

A sintaxe para a captura de exceções, por exemplo, tem (quase) a <i>mesma</i> sintaxe proposta qui e todos convivem bem com isso.

Por exemplo:

    procedure Execute;
    var
      X: Integer;
    begin
      try
         X := 10 / 0;
      except
        on E: EDivByZero do
           ShowMessage(E.Message);
      end;
    end;

A variável <code>E</code> não foi previamente declarada na seção de variáveis locais. Isso vai contra a filosofia Pascal, onde devemos declarar tudo antes de utilizar. Mas, tudo bem. Essa é uma exceção a regra que é bem vinda.

Voltando ao primeiro exemplo, você pode ter sentido falta às chamadas aos destrutores das instâncias. Bem, eu utilizei a [função New]({% post_url 2016-01-10-interfaces-e-o-metodo-estatico-new %}) justamente para não haver essa necessidade — considerando que esses métodos retornam uma instância de [interface]({% post_url 2016-01-18-interfaces-em-todo-lugar %}). 

Mas se você não quiser utilizar essa técnica, basta reescrever assim:

    with F =: TFoo.Create, B =: TBar.Create do
    try
      F.Execute;
      B.Execute;
    finally
      F.Free;
      B.Free;
    end;

Na linguagem C# eles tem a declaração <code>using</code> que é bem parecido com o que estou propondo — a criação de <i>alias</i> para as instâncias. No entanto, lá o objetivo é o compilador chamar o método <code>Dispose</code> de cada instância no fim do bloco, independentemente se houver ou não uma exceção. Para que essa "mágica" aconteça, a instância deve implementar a interface <code>IDisposable</code>.

Eu acho essa abordagem desnecessária para o Pascal — no entanto, possível — pois o código fica mais explicito com o uso do <code>try-finally</code> se você não estiver utilizando instâncias de interfaces.

## Conclusão {#conclusao}

Acredito que o uso de WITH-DO proposto aqui iria nos ajudar a implementar um código mais simples.

A nova sintaxe com a atribuição invertida não iria entrar em conflito com a sintaxe atual.

Poderíamos criar blocos de código independentes dentro de métodos. Visto que, no exemplos acima, as variáveis <code>F</code> e <code>B</code> só poderiam ser utilizadas dentro do WITH-DO e não em todo o escopo do método.

Só iríamos declarar variáveis "globais ao método" na sessão de variáveis locais ao método, mas todas as outras instâncias seriam inicializadas em blocos WITH-DO para restringir ainda mais o escopo e visibilidade às variáveis.

Até logo.