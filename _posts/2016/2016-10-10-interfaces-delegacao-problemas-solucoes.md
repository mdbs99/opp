---
layout: post
title: "Interfaces, Delegação, Problemas e Soluções"
date: 2016-10-10
description: "Como utilizar Interfaces e Delegação de Implementação sem vazamentos de memória"
summary: "Como utilizar Interfaces e Delegação de Implementação sem vazamentos de memória"
image: /images/photo-1474406716381-8399fe67fbcf.jpg
categories: 
  - Object Pascal
tags:
  - Language
keywords:
  - delegation
  - delegates
  - delegação
  - implements
  - object delegation
--- 

Delegação de Implementação através da composição de
Objetos é uma *feature* incrível, só disponível na
Linguagem *Object Pascal*, porém existem alguns 
problemas intrínsecos no uso dessa tecnologia.

<!--more-->

![Imagem]({{ page.image }})

## Introdução {#introducao}

No [artigo anterior]({% post_url 2016-10-03-delegacao-de-implementacao-de-interfaces %})
escrevi sobre a Delegação de Implementação de Interfaces, 
utilizando a Composição de Objetos.

É uma *feature* bem legal.

Infelizmente existem algumas armadilhas quando trabalhamos
com Interfaces e sua liberação automática de memória.

A Delegação utilizando Objetos também possui armadilhas que,
se não forem verificadas, podem arruinar seu projeto devido
aos vazamentos de memória que essa prática pode causar.

Nesse artigo vou lhe mostrar os problemas e propor soluções.

## Vazamentos de memória {#vazamentos-de-memoria}

A Linguagem Object Pascal nos dá a possibilidade de trabalharmos
utilizando variáveis do tipo Interface. Essas são liberadas
automaticamente, pelo compilador, quando saem do escopo de execução
na qual elas foram criadas.

Essa facilidade nos traz alguns problemas ou cria 
condições para problemas como, por exemplo, a 
[referência circular]({% post_url 2016-02-01-interfaces-e-a-referencia-circular-entre-objetos %})
e a [não existência de uma variável]({% post_url 2016-01-10-interfaces-e-o-metodo-estatico-new %})
na construção de um Objeto.

Esse são alguns dos inúmeros equívocos na implementação que geram
os tão temidos vazamentos de memória.

Outro problema grave tem haver com a 
[hierarquia de herança]({% post_url 2016-05-23-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-1 %}) 
que você irá utilizar nas Classes que irão gerar Objetos que 
implementam Interfaces por delegação.

## Contador de Referências {#contador-de-referencias}

Vou ser justo e dizer que o problema, em si, não é especificamente sobre
herança, mas como você implementa os Métodos especiais que são utilizados
pelo compilador para fazer a liberação da memória automaticamente.

São eles: `QueryInterface`, `_AddRef` e `_Release`.

Esses são Métodos especiais que são utilizados para 
implementar um Contador de Referências. 
Eles servem para incrementar e decrementar um contador 
de referência da instância e, com isso, ter o controle para saber 
quando liberar o Objeto da memória.

Ao utilizarmos Interfaces, somos obrigados a implementar esses 3 Métodos
— a não ser que utilizemos Interfaces CORBA, que não tem contagem de
referência e nem liberação automática da memória.

Então, precisamos herdar de alguma Classe que já implemente esses Métodos.

E aqui começam os problemas.

## Problemas {#problemas}

Como eu não utilizo mais herança de Classes eu não preciso pensar sobre
hierarquias de Classes e todas as suas complexidades. 
Todas as minhas Classes herdam de `TInterfacedObject`, pois essa Classe
já implementa os 3 Métodos apropriadamente.

Mas para programadores que ainda utilizam
[herança]({% post_url 2016-05-23-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-1 %})
esse pode ser o primeiro problema a enfrentar.

Exemplo. Você vai desenhando toda a sua hierarquia de Classes, "perfeitamente" e 
descobre que Cão não pode herdar de Mamífero porque você quer utilizar Interfaces
com contagem de referência... então Cão herda de `TInterfacedObject` — 
quebrando a hierarquia — ou você é obrigado a copiar/colar o código 
de `TInterfacedObject` dentro da sua Classe — o que também é abominável. 

Como eu dizia, todas as minhas Classes herdam de `TInterfacedObject`.
Sendo assim, as Classes que serão utilizadas para delegar a implementação
das Interfaces, também serão filhas de `TInterfacedObject` e... BUM! 
Temos vazamento de memória.

Pois é. As Classes delegadas não podem herdar de `TInterfacedObject`
se eu quiser instanciar a Classe principal como uma Interface.

Confuso? Eu também fiquei.

### Exemplo 1

Vamos utilizar o exemplo do
[artigo anterior]({% post_url 2016-10-03-delegacao-de-implementacao-de-interfaces %})
para explicar onde iríamos ter problemas com vazamento de memória.

A Classe final ficou assim:

      TTheClient = class(TInterfacedObject, IFinances, IAccess)
      private
        FFinances: IFinances;
        FAccess: IAccess;
        property Finances: IFinances read FFinances implements IFinances;
        property Access: IAccess read FAccess implements IAccess;
      end;

E os [exemplos]({% post_url 2016-10-03-delegacao-de-implementacao-de-interfaces %}#como-utilizar-a-delegacao)
de implementação utilizando delegação foram as Classes `TSimpleFinances` 
e `TSimpleAccess`. Ambas herdando de `TInterfacedObject`.

      TSimpleFinances = class(TInterfacedObject, IFinances)
      public
        construtor ...
        function Current: Currency;
        function AsString: string;
      end;
      
      TSimpleAccess = class(TInterfacedObject, IAccess)
      public
        construtor ...
        function List: IDataList;
        function AsString: string;
      end;

Devido os atributos `FFinances` e `FAccess` forem do tipo Interface, haverá
vazamentos de memória se estanciarmos `TTheClient` como sendo representante
de alguma das Interfaces que ele implementa (por delegação).

Isso ocorre porque o compilador irá instanciar 2 Objetos — `TTheClient`
e o Objeto delegado que implementa a Interface — mas no nosso código
só teremos a referência a um Objeto.
A princípio tudo deveria funcionar, já que também temos a referência ao
Objeto delegado através do atributo privado, mas não é assim que funciona.

Há dois problemas no código acima, se considerarmos a utilização normal de 
instâncias com contagem de referência:

  1. Propriedades e atributos delegados não podem ser Interfaces;
  2. Objetos delegados não devem ter `TInterfacedObject` como herança.

Então a Classe final `TTheClient` tem um problema.

Vamos ao um exemplo mais completo.

### Exemplo 2

O exemplo abaixo foi submetido à lista de discussão oficial do FreePascal com
o título "*A serious Memleak using delegates/implements...*" no dia 5 de Outubro, 
o que gerou alguma polêmica por lá.

Se você não faz parte da lista, pode ler as mensagens dessa *thread* 
[aqui](https://www.mail-archive.com/fpc-pascal@lists.freepascal.org/msg43835.html).

    program Project1;

    {$mode objfpc}{$H+}

    uses
      Classes, SysUtils;

    type
      IValue = interface
        function AsString: string;
      end;

      TIntegerValue = class(TInterfacedObject, IValue)
      private
        FValue: Integer;
      public
        constructor Create(Value: Integer);
        destructor Destroy; override;
        function AsString: string;
      end;

      TMyApp = class(TInterfacedObject, IValue)
      private
        FValue: IValue;
      public
        constructor Create(Value: Integer);
        destructor Destroy; override;
        property Value: IValue read FValue implements IValue;
      end;

    { TIntegerValue }

    constructor TIntegerValue.Create(Value: Integer);
    begin
      inherited Create;
      FValue := Value;
      WriteLn('TIntegerValue.Create');
    end;

    destructor TIntegerValue.Destroy;
    begin
      WriteLn('TIntegerValue.Destroy');
      inherited Destroy;
    end;

    function TIntegerValue.AsString: string;
    begin
      Result := 'Number is ' + IntToStr(FValue);
    end;

    { TMyApp }

    constructor TMyApp.Create(Value: Integer);
    begin
      inherited Create;
      FValue := TIntegerValue.Create(Value);
      WriteLn('TMyApp.Create');
    end;

    destructor TMyApp.Destroy;
    begin
      WriteLn('TMyApp.Destroy');
      inherited Destroy;
    end;

    // Program

    procedure ExecuteIntegerValue;
    var
      V: IValue;
    begin
      WriteLn;
      WriteLn('IntegerValue:');
      V := TIntegerValue.Create(5);
      WriteLn(V.AsString);
    end;

    procedure ExecuteMyApp;
    var
      App: TMyApp;
    begin
      WriteLn;
      WriteLn('MyApp:');
      App := TMyApp.Create(10);
      try
        WriteLn(App.Value.AsString);
      finally
        App.Free;
      end;
    end;

    procedure ExecuteMyAppAsInterface;
    var
      V: IValue;
    begin
      WriteLn;
      WriteLn('MyAppAsInterface:');
      V := TMyApp.Create(20);
      WriteLn(V.AsString);
    end;

    begin
      ExecuteIntegerValue;
      ExecuteMyApp;
      ExecuteMyAppAsInterface;
      ReadLn;
    end.

O programa acima compila sem nenhum erro utilizando o FreePascal ou Delphi.
No entanto, se habilitarmos a saída de verificação de vazamentos
de memória (chama-se [Heaptrc](http://www.freepascal.org/docs-html/rtl/heaptrc/usage.html) no FreePascal)
podemos ver que o término do programa não foi elegante como deveria:

    W:\temp>project1.exe

    IntegerValue:
    TIntegerValue.Create
    Number is 5
    TIntegerValue.Destroy

    MyApp:
    TIntegerValue.Create
    TMyApp.Create
    Number is 10
    TMyApp.Destroy
    TIntegerValue.Destroy

    MyAppAsInterface:
    TIntegerValue.Create
    TMyApp.Create
    Number is 20

    Heap dump by heaptrc unit
    83 memory blocks allocated : 2017/2200
    81 memory blocks freed     : 1981/2160
    2 unfreed memory blocks : 36
    True heap size : 229376 (80 used in System startup)
    True free heap : 229104
    Should be : 229128
    Call trace for block $01812928 size 20
      $004017DA  TMYAPP__CREATE,  line 59 of W:/temp/project1.lpr
      $00401B82  EXECUTEMYAPPASINTERFACE,  line 101 of W:/temp/project1.lpr
      $00401C08  main,  line 108 of W:/temp/project1.lpr
    Call trace for block $018128C8 size 16
      $00401B82  EXECUTEMYAPPASINTERFACE,  line 101 of W:/temp/project1.lpr
      $00401C08  main,  line 108 of W:/temp/project1.lpr

    W:\temp>

O problema de vazamento de memória ocorre somente na chamada à função `MyAppAsInterface`.
    
Ao vermos isso a primeira impressão é: Delegação de Implementação não funciona.

Bem, funciona sim. Não é tão elegante como eu gostaria que fosse, mas ao
menos é contornável.
    
## Soluções {#solucoes}

A partir de agora, a Classe que você sempre deverá se lembrar, depois da 
`TInterfacedObject`, será a Classe `TAggregatedObject`. 
Ambas existem no FreePascal e também no Delphi.

A Classe `TAggregatedObject` também implementa os 3 métodos especiais, mas
é uma implementação diferente da utilizada em `TInterfacedObject`.

Essa Classe deverá ser utilizada para implementar Objetos que são utilizados
na Delegação de Implementação utilizando a sintaxe "*implements*".

Em [poucas palavras](http://www.freepascal.org/docs-html/rtl/system/taggregatedobject.html), 
os Objetos de `TAggregatedObject` delegam a contagem de 
referência ao "Objeto controlador", ou seja, delegam seus próprios "tempos de vida"
ao Objeto externo que implementa as Interfaces.

Primeiro vamos implementar uma nova Classe que herda de `TAggregatedObject`:

    TDelegatedIntegerValue = class(TAggregatedObject, IValue)
    private
      FValue: Integer;
    public
      constructor Create(AController: IInterface; Value: Integer);
      destructor Destroy; override;
      function AsString: string;
    end;

    { TDelegatedIntegerValue }

    constructor TDelegatedIntegerValue.Create(AController: IInterface;
      Value: Integer);
    begin
      inherited Create(AController);
      FValue := Value;
      WriteLn('TDelegatedIntegerValue.Create');
    end;

    destructor TDelegatedIntegerValue.Destroy;
    begin
      WriteLn('TDelegatedIntegerValue.Destroy');
      inherited Destroy;
    end;

    function TDelegatedIntegerValue.AsString: string;
    begin
      Result := 'Number is ' + IntToStr(FValue);
    end;

Infelizmente também teremos que alterar a definição da Classe `TMyApp`,
redefinindo a propriedade e atributo para um tipo concreto de Classe:

    TMyApp = class(TInterfacedObject, IValue)
    private
      FValue: TDelegatedIntegerValue;
    public
      constructor Create(Value: Integer);
      destructor Destroy; override;
      property Value: TDelegatedIntegerValue read FValue implements IValue;
    end;

    { TMyApp }

    constructor TMyApp.Create(Value: Integer);
    begin
      inherited Create;
      FValue := TDelegatedIntegerValue.Create(Self, Value);
      WriteLn('TMyApp.Create');
    end;

    destructor TMyApp.Destroy;
    begin
      FValue.Free;
      WriteLn('TMyApp.Destroy');
      inherited Destroy;
    end;

Ao criamos a instância `FValue`, um dos argumentos do construtor
da Classe `TDelegatedIntegerValue` é o *Controller* que irá controlar
o tempo de vida da instância que, nesse caso, é a instância (*Self*)
de `TMyApp`.

Após compilar e executar novamente a aplicação, podemos ver um novo resultado:

    c:\temp>project1.exe

    IntegerValue:
    TIntegerValue.Create
    Number is 5
    TIntegerValue.Destroy

    MyApp:
    TDelegatedIntegerValue.Create
    TMyApp.Create
    Number is 10
    TDelegatedIntegerValue.Destroy
    TMyApp.Destroy

    MyAppAsInterface:
    TDelegatedIntegerValue.Create
    TMyApp.Create
    Number is 20
    TDelegatedIntegerValue.Destroy
    TMyApp.Destroy

    Heap dump by heaptrc unit
    83 memory blocks allocated : 2009/2184
    83 memory blocks freed     : 2009/2184
    0 unfreed memory blocks : 0
    True heap size : 196608 (80 used in System startup)
    True free heap : 196528

    c:\temp>

## Conclusão {#conclusao}

Os *designers* da linguagem foram muito infelizes ao criarem "métodos especiais"
para liberação de memória. Isso é algo difícil de explicar para programadores
não-Pascal. É até um pouco vergonhoso...

Poderiam ter criado uma Interface especial que só pelo fato de dizer que sua
a Classe à implementa, o compilador poderia fazer sua mágica.

Felizmente há possíveis soluções para contornar esse erro no *design*, mas se 
pudessemos utilizar propriedades de Objetos delegados como sendo do 
tipo Interface, como sugeri no artigo anterior, seria quase perfeito. Poderíamos
utilizar Injeção de Dependência através dos construtores dos Objetos, inicializando
as instâncias delegadas, que seriam do tipo Interface. 

Com a implementação atual, até podemos utilizar a Injeção de Dependência, 
mas os argumentos deverão ser do tipo Classe e não Interface — isso na maioria dos
casos normais onde queremos utilizar a contagem de referência — 
então o polimorfismo iria ficar por conta da herança de Classes, o que não é muito bem
vindo devido aos problemas já relatados em artigos anteriores.

O jeito é instanciar as Classes delegadas internamente. O que não é um real problema.

Se formos pensar bem, todos os métodos de Implementação das Interfaces deveriam ser 
implementas dentro do Objeto, quando não utilizamos Delegação. Então não estaríamos 
perdendo muito, apenas estamos deixando de ganhar mais.

Até logo.