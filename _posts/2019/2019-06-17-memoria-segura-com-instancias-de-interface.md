---
layout: post
title: "Memória Segura Utilizando Instâncias de Interfaces"
date: 2019-06-17
permalink: /:title
description:
  A utilização de instâncias de interfaces pode ser inseguras?
image: /images/2019/gabriel-gurrola-424227-unsplash.jpg
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
  - interfaces
---

Você sabe que ao utilizar instâncias de interfaces não é necessário destruir o objeto manualmente, utilizando o método `Free`. 

Isso ocorre por quê instâncias de interfaces possuem uma contagem de referência e, assim, o compilador sabe quando desalocar a instância automaticamente.

Mas e se se ocorrer uma exceção dentro de um método de uma instância, numa composição de instâncias de interfaces, seria seguro não utilizar `try-finally` para ter a certeza que todos os objetos serão desalocados, atribuindo `nil` as instâncias?

<!--more-->

![Unsplash image]({{ page.image }})
<br><span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Gabriel Gurrola on Unsplash</span>

## Introdução {#introducao}

Na semana passada eu recebi um email de um leitor do blog, o Carlos, e ele me questionou sobre ter ou não segurança na desalocação apropriada de memória quando utilizamos instâncias de interfaces.

A dúvida dele é interessante.

Segundo Carlos, utilizar instancias de interfaces seria inseguro em comparação com instâncias de classes, pois "se uma exceção ocorrer dentro de algum método, você não terá a chance de desalocar as variáveis de interfaces como fazemos com o `try-finally` e instâncias de classes" — ele escreveu.

Então ele me enviou um projeto de teste que "provaria" que instâncias de interfaces poderiam ser inseguras e que não há vantagem em relação às instâncias de classes no quesito desalocação de memória pois, segundo ele, seríamos obrigados a utilizar o `try-finally` para estarmos "100% seguro".

O exemplo era um pouco mais complexo do que deveria, então eu o reescrevi e enviei à ele. Ele concordou que ficou mais [simples]({% post_url 2016-12-19-simplicidade %}) de entender mas, o novo exemplo ainda demonstrava que ele estava certo — ele afirmou — pois ainda haviam os vazamentos de memória no final da execução do programa.

Mas até aquele momento, eu só queria ter uma base de código mais simples para, então, mostrar-lhe uma segunda opinião.

## Código {#codigo}

O código original foi reescrito, porém a nova versão contem todos os "problemas" apontados por ele e, consequentemente, a mesma saída no final da execução.

Esse é o código completo:

    program Project1;

    {$mode delphi}

    uses
      SysUtils, Classes;

    type
      IFoo = interface
        procedure Execute;
      end;

      IBar = interface
        procedure Execute;
      end;

      TFoo = class(TInterfacedObject, IFoo)
      private
        fBar: IBar;
      public
        constructor Create(const aBar: IBar);
        procedure Execute;
      end;

      TBar = class(TInterfacedObject, IBar)
      public
        procedure Execute;
      end;

    { TFoo }

    constructor TFoo.Create(const aBar: IBar);
    begin
      inherited Create;
      fBar := aBar;
    end;

    procedure TFoo.Execute;
    begin
      fBar.Execute;
    end;

    { TBar }

    procedure TBar.Execute;
    begin
      raise Exception.Create('exception!!!');
    end;

    var
      foo: IFoo;
    begin
      foo := TFoo.Create(TBar.Create);
      foo.Execute;
    end.

É um aplicativo de linha-de-comando.

Crie um novo projeto e não se esqueça de marcar a opção para ver o resultado da memória — se você estiver utilizando Lazarus, marque a opção "Use Heaptrc..." em Project Options, debugging.

O término do programa deverá ser algo parecido com isso:

    > project1.exe
    An unhandled exception occurred at $004017DA:
    Exception: exception!!!
      $004017DA  TBAR__EXECUTE,  line 46 of W:/md/dev/proj/test/ide/projects/project1.lpr
      $004017C8  TFOO__EXECUTE,  line 40 of W:/md/dev/proj/test/ide/projects/project1.lpr
      $004018D2  main,  line 54 of W:/md/dev/proj/test/ide/projects/project1.lpr

    Heap dump by heaptrc unit of W:\md\dev\proj\test\ide\projects\project1.exe
    74 memory blocks allocated : 2075/2232
    71 memory blocks freed     : 1975/2128
    3 unfreed memory blocks : 100
    True heap size : 262144 (112 used in System startup)
    True free heap : 261600
    Should be : 261640
    Call trace for block $014D7628 size 64
      $0040A1AC
      $004017FA  TBAR__EXECUTE,  line 47 of W:/md/dev/proj/test/ide/projects/project1.lpr
      $004017C8  TFOO__EXECUTE,  line 40 of W:/md/dev/proj/test/ide/projects/project1.lpr
      $004018D2  main,  line 54 of W:/md/dev/proj/test/ide/projects/project1.lpr
    Call trace for block $014A8D70 size 24
      $0040A1AC
      $004017FA  TBAR__EXECUTE,  line 47 of W:/md/dev/proj/test/ide/projects/project1.lpr
      $004017C8  TFOO__EXECUTE,  line 40 of W:/md/dev/proj/test/ide/projects/project1.lpr
      $004018D2  main,  line 54 of W:/md/dev/proj/test/ide/projects/project1.lpr
    Call trace for block $014A8CF0 size 12
      $004017EE  TBAR__EXECUTE,  line 47 of W:/md/dev/proj/test/ide/projects/project1.lpr
      $004017C8  TFOO__EXECUTE,  line 40 of W:/md/dev/proj/test/ide/projects/project1.lpr
      $004018D2  main,  line 54 of W:/md/dev/proj/test/ide/projects/project1.lpr

Não importando as diferenças na finalização do programa entre Lazarus e Delphi, apenas tenha atenção nessa linha:

    3 unfreed memory blocks

O código não tem nenhum `try-finally` e estamos utilizando instâncias de interfaces, como `IFoo` e `IBar`, e a saída são 3 blocos de memória não liberados, devido ter ocorrido uma exceção em `TBar.Execute`. Do contrário, não haveria nenhum vazamento de memória — você pode verificar isso comentando a linha que gera a exceção.

Isso prova que interfaces são inseguras se exceções ocorrem, certo?

Bem, não tão rápido.

Primeiro de tudo, sugiro que você leia o código, linha a linha, e veja se consegue descobrir o que há de errado.

## O Diabo Mora nos Detalhes {#detalhes}

Para começar, o vazamento de memória acima não tem nada haver com instâncias de interfaces. Na verdade, você pode remover as interfaces completamente, adicionar `try-finally` para desalocar a memória manualmente e, mesmo assim, a saída do Heaptrc irá continuar a mesma.

Não acredita?

Tente você:

    program Project1;

    {$mode delphi}

    uses
      SysUtils, Classes;

    type
      TBar = class;

      TFoo = class
      private
        fBar: TBar;
      public
        constructor Create(const aBar: TBar);
        procedure Execute;
      end;

      TBar = class
      public
        procedure Execute;
      end;

    { TFoo }

    constructor TFoo.Create(const aBar: TBar);
    begin
      inherited Create;
      fBar := aBar;
    end;

    procedure TFoo.Execute;
    begin
      fBar.Execute;
    end;

    { TBar }

    procedure TBar.Execute;
    begin
      raise Exception.Create('exception!!!');
    end;

    var
      bar: TBar;
      foo: TFoo;
    begin
      bar := TBar.Create;
      foo := TFoo.Create(bar);
      try
        foo.Execute;
      finally
        bar.Free;
        foo.Free;
      end;
    end.

Se você compilou e executou essa versão, viu que a saída do programa com o relatório do Heaptrc (ou similar) é praticamente o mesmo.

Os 3 vazamentos de memória continuam, provando que o "problema" não são as interfaces.

Será um problema no compilador?

## Solução {#solucao}

Primeiramente, vamos separar o código principal em uma unidade para ser utilizada tanto em uma aplicação Console como GUI.

Assim vou conseguir demostrar um ponto importante, futuramente.

Aqui está o código refatorado, ou seja, apenas as interfaces e classes separadas em uma nova unidade chamada `MyClasses`.

    unit MyClasses;

    {$mode delphi}

    interface

    uses
      SysUtils, Classes;

    type
      IFoo = interface
        procedure Execute;
      end;

      IBar = interface
        procedure Execute;
      end;

      TFoo = class(TInterfacedObject, IFoo)
      private
        fBar: IBar;
      public
        constructor Create(const aBar: IBar);
        procedure Execute;
      end;

      TBar = class(TInterfacedObject, IBar)
      public
        procedure Execute;
      end;

    implementation

    { TFoo }

    constructor TFoo.Create(const aBar: IBar);
    begin
      inherited Create;
      fBar := aBar;
    end;

    procedure TFoo.Execute;
    begin
      fBar.Execute;
    end;

    { TBar }

    procedure TBar.Execute;
    begin
      raise Exception.Create('exception!!!');
    end;

    end.

Agora, crie um novo projeto GUI. Adicione um Form. Depois um botão nesse Form.

Novamente, se você estiver utilizando Lazarus, marque a opção "Use Heaptrc..." em Project Options, debugging.

Adicione `MyClasses` ao projeto e ao `uses` do Form.

No click do botão, codifique:

    procedure TForm1.Button1Click(Sender: TObject);
    var
      foo: IFoo;
    begin
      foo := TFoo.Create(TBar.Create);
      foo.Execute;
    end;

Ao executar o projeto e clicar no botão, você verá a mensagem de exceção:

    Project project1 raised exception class 'Exception' with message: exception!!!
    In file 'MyClasses.pas' at line 51

Clique em continuar para ver a verdadeira exceção e depois em OK.

Finalmente, feche o programa.

Se você não esqueceu de marcar o Heaptrc, um `ShowMessage` será mostrado:

    Heap dump by heaptrc unit of W:\md\dev\proj\test\ide\projects\project1.exe
    1764 memory blocks allocated : 1804912/1810440
    1764 memory blocks freed     : 1804912/1810440
    0 unfreed memory blocks : 0
    True heap size : 688128 (112 used in System startup)
    True free heap : 688016

Nenhum vazamento de memória, utilizando o mesmo código do início do artigo.

O que está acontecendo aqui?

O verdadeiro problema é não capturar a exceção em *nenhum* lugar do programa. 

Mas um programa GUI, por padrão, captura todas as exceções caso o programador não o faça deliberadamente.

E isso faz com que o programa continue em execução "mais ou menos" no ponto onde a exceção foi gerada, ou seja, o programa continua em execução.

E aqui entra uma vantagem das instâncias de interfaces: o compilador adiciona um `try-finally` implícito para que suas contagens de referência sejam decrementadas.

Sabemos que um `try-finally` sempre será executado, independentemente de houve uma exceção dentro do seu bloco.

Por isso as instâncias de `IFoo` e `IBar` são decrementadas e, consequentemente, destruídas mesmo após ter ocorrido uma exceção.

Vamos voltar ao programa de linha-de-comando.

Crie um novo programa e adicione a unidade `MyClasses`.

O código é similar ao código do botão, porém é necessário um `try-except`, pois aplicações console (por padrão) não tem um *handle* de exceções:

    program Project1;

    {$mode delphi}

    uses
      SysUtils, Classes, MyClasses;

    var
      foo: IFoo;
    begin
      foo := TFoo.Create(TBar.Create);
      try
        foo.Execute;
      except
        on e: Exception do
           writeln(e.Message);
      end;
    end.

Compile e execute.

Veja que não há mais vazamentos de memória.

    > project1.exe
    exception!!!
    Heap dump by heaptrc unit of W:/md/dev/proj/test/ide/projects/project1.exe
    72 memory blocks allocated : 1916/2064
    72 memory blocks freed     : 1916/2064
    0 unfreed memory blocks : 0
    True heap size : 196608 (112 used in System startup)
    True free heap : 196496

Mesmo sem o `try-finally` as instâncias de `IBar` e `IFoo` foram destruídas automaticamente pelo compilador.

## Conclusão {#conclusao}

A utilização de instâncias de interface é, de certa forma, mais segura do que instância de classes.

No entanto, se você começar a brincar com ponteiros, cast entre instâncias de classe vs interface, [referência circular]({% post_url 2016-02-01-interfaces-e-a-referencia-circular-entre-objetos %}), etc, sem saber o que está fazendo, será grande a chance de haver vazamentos de memória.

Porém, utilizando o código padrão para instanciar objetos do tipo interface, seu código estará mais seguro contra vazamentos de memória, pois o compilador sempre irá desalocar as instâncias, caso você já as tenha feito atribuindo `nil` as variáveis.

Até logo.