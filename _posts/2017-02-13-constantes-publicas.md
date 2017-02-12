---
layout: post
title: Constantes Públicas
date: 2017-02-13
description:
  Constantes Públicas não deveriam ser utilizadas em projetos Orientados a Objetos.
image: /images/photo-ty8v7jjcgvg-pierre-bouillot.jpg
categories: 
  - Linguagem
tags:
  - constante
keywords:
  - constante
  - constant
  - public constant
  - delphi
  - freepascal
  - lazarus
---

Quantas vezes você precisou instanciar uma Classe mas não sabia, com exatidão, o que passar num dos argumentos do construtor? Essa dúvida quase sempre acontece com argumentos do tipo *String* e a maioria das API's disponibilizam constantes para serem utilizados nos argumentos.

Bem, Constantes Públicas não deveriam ser utilizadas em projetos Orientados a Objetos.

<!--more-->

![Unsplash image]({{ page.image }})  

## Introdução {#introducao}

Em linguagens puramente procedurais com C, por exemplo, constantes são normalmente utilizadas. Não há nada de errado com isso.

No entanto, em projetos Orientados a Objetos, não há sentido em continuarmos utilizando o pensamento procedural de C, ou seja, disponibilizando *Constantes Públicas* para serem utilizadas como argumentos de Métodos e Construtores.

## Constantes Públicas {#constantes-publicas}

Eu já expliquei nesse [artigo]({% post_url 2017-01-16-tipos-primitivos-nos-argumentos %}) que utilizar tipos primitivos em argumentos não é uma boa ideia na maioria dos casos. O problema é agravado quando utilizamos argumentos do tipo *String*, pois o valor pode ser qualquer sequência de caracteres.

Vejamos um exemplo:

    var
      S: IString;
    begin
      S := TStringBuilder.New(
        TFile.New('foo.txt').Stream,
        'UTF-8'
      );
    end;

As Classes acima não fazem parte da RTL, são apenas exemplos.

Vejamos. Uma Classe `TStringBuilder` recebe dois argumentos: a) Um *Stream* de dados e b) uma *String* que corresponde ao *encoding* que será utilizado.

Quais as chances de codificar o segundo parâmetro errado? 

Você poderia escrever assim `UTF-8`, assim `UTF8`, assim `utf-8`, assim `utf8`...

Para não haver erros de digitação, os programadores de API's disponibilizam Constantes Públicas, exemplo:

    var
      S: IString;
    begin
      S := TStringBuilder.New(
        TFile.New('foo.txt').Stream,
        UTF8_ENCODING
      );
    end;

Ah, agora tá "perfeito". Não haverá erro de digitação, pois não é mais uma *String*, mas sim uma Constante que o compilador pode checar.

O problema, no entanto, é que o [argumento]({% post_url 2017-01-16-tipos-primitivos-nos-argumentos %}#argumento) continua sendo do tipo *String* e mesmo o *code-completion* não pode ajudá-lo para lhe mostrar onde está a Constante (se existir!) para utilizá-la no argumento.

O que fazer?

## Objetos {#objetos}

Se você quer representar um `UTF8String` por quê não criar uma Classe que instancia um Objeto que represanta exatamente o que você precisa?

Vamos refatorar o código:

    var
      S: IString;
    begin
      S := TUTF8String.New(
        TFile.New('foo.txt').Stream
      );
    end;

Muito mais [simples]({% post_url 2016-11-28-menos-e-mais %}) e direto, não acha?

Então você pensa: "Mas eu terei uma Classe para cada tipo de *encoding* no meu código?"

Sim! Mas não pense que você terá duplicação de código por causa disso.

## Reutilizando {#reutilizando}

Sabemos que já existem muitas funções para trabalhar com *encoding* de *String* em qualquer linguagem. Mas aqui trabalhamos com Objetos, então... criamos Objetos.

Mas não queremos reinventar a roda. Precisamos utilizar as funções já existentes, seja funções *built-in* ou de alguma *lib* de terceiros. Não importa.

No seu código Orientado a Objetos você não deve expor esses detalhes tecnológicos para seu domínio.

Então, como fazer?

Na sua *Unit*, você irá implementar uma Classe Privada. Essa Classe irá receber sim, um argumento do tipo *String*. Mas não há problema, pois a Classe é privada à *Unit*.

Essa Classe teria uma implementação assim:

    type
      TEncodedString = class(TInterfacedObject, IString)
      private
        FEncoding: string;
      public
        constructor Create(
          Stream: IDataStream; 
          const Encoding: string
        );
        class function New(
          Stream: IDataStream; 
          const Encoding: string
        ): IString;
        function Value: string;
        // more methods...
      end;

Cada Classe especialista, como a `TUTF8String` por exemplo irá delegar para a `TEncodedString` a construção da *String* de retorno.

O argumento `Encoding` poderá ser uma *String* pura ou uma **Constante Privada** — seja uma Constante privada da Classe ou da *Unit*, não importa.

Então, considerando que `IString` tem o método `Value: string`, a implementação desse método em `TUTF8String` seria algo assim:

    function TUTF8String.Value: string;
    begin
      Result := 
        TEncodedString.New(
          FStream, 'UTF-8'
        ).Value
    end;
    
## Conclusão {#conclusao}

Constantes Públicas não devem ser utilizadas na programação Orientada a Objetos pelas razões explicadas acima. Porém, devido a questões tecnológicas e detalhes de implementação, as Constantes Privadas ainda são bastante úteis.

Ao invés de tentar deixar uma Classe o mais gérica possível, tente "quebrá-la" em várias Classes especialistas. Assim você irá simplificar o uso e a manutenção dessas Classes.

Até logo.