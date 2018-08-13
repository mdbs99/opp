---
layout: post
title: "Como Transformar uma Instância de Classe em Instância de Interface"
date: 2018-08-13
permalink: /:title
description:
  E se a linguagem Object Pascal nos permitisse codificar uma instância de classe que se auto destrói quando não mais precisamos dele?
image: /images/2018/julie-north-720719-unsplash.jpg
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
  - interface
---

E se a linguagem Object Pascal nos permitisse codificar uma instância de classe que se auto destrói quando não mais precisamos dele?

É possível transformar uma instância de classe para se comportar como uma instância de interface?

<!--more-->

![Unsplash image]({{ page.image }})
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Julie North on Unsplash</span>

Há alguns dias eu acompanhei alguns desenvolvedores solicitarem algumas "novas features" para o compilador Free Pascal..

Uma dessas features é a possibilidade de fazer um *"auto free"* de uma instância de classe. Sim, como no Java ou C#, onde um objeto do tipo de uma classe é liberado automaticamente quando a VM acha que deve fazê-lo.

No entanto, devemos lembrar dos princípios da linguagem Object Pascal antes de solicitar tais mudanças. A linguagem Object Pascal não possui uma VM, portanto não temos um "robozinho" para ficar monitorando nossos objetos. Devemos liberar nossos objetos "manualmente" chamando `obj.Free` na maioria dos casos.

[Simples]({% post_url 2016-12-19-simplicidade %}) e eficiente.

Mas eu posso entender o motivo de tal solicitação: É muito cômodo declarar objetos sem se preocupar em liberá-los da memória... e eu tenho feito isso há anos utilizando a "técnica" do [Método New]({% post_url 2016-01-10-interfaces-e-o-metodo-estatico-new %}) em conjunto com instâncias de interfaces.

O tipo de [interface]({% post_url 2016-01-18-interfaces-em-todo-lugar %}) é liberado automaticamente pelo compilador quando a variável sai do escopo. Mas isso não existe para instâncias de classe, proposto no pedido da nova feature.

Então, se houvesse um jeito de liberar automaticamente as instâncias de classes, "convertendo-as" em instâncias de interfaces, sem utilizar nenhum tipo de [casting]({% post_url 2016-04-18-nao-utilize-casting %}), teríamos a liberação automática sem que fosse necessária alterações no compilador.

Bem, o que vou lhe apresentar agora pode já estar sendo utilizando por muitos desenvolvedores ao redor do mundo, e é algo tão simples que podemos até mesmo substituir o [Método New]({% post_url 2016-01-10-interfaces-e-o-metodo-estatico-new %}) — tão difundido na comunidade brasileira e mundo afora — por essa simples "técnica" que mostrarei mais abaixo.

Mas antes, o que há de errado com o [Método New]({% post_url 2016-01-10-interfaces-e-o-metodo-estatico-new %})?

Esse método foi criado com o intuito de implementar a liberação automática da instância, sem haver a necessidade de declarar uma variável local para incrementar a contagem de referência.

Funciona.

No entanto, há alguns problemas com essa abordagem.

Primeiro, ele é um método estático da classe. Deveríamos <del>não utilizar</del> evitar métodos de classe a todo custo, dando [preferência] em utilizar instâncias de interfaces, onde não existem métodos estáticos.

Segundo, é redundante ter que codificar sempre os mesmos parâmetros da(s) mesma(s) assinatura do(s) construtor(es). Isso, além de ser no mínimo chato, é um pouco custoso para a manutenção pois (quase sempre) haverá 2 lugares para alterar os argumentos.

Terceiro, o método não impede que um programador que não conheça a técnica, chame diretamente o construtor da classe, *by passing* o método estático. Isso pode gerar *memory leaks*, como já foi explicado em alguns outros artigos aqui do blog.

Embora haja todos esses problemas, a utilização do [Método New]({% post_url 2016-01-10-interfaces-e-o-metodo-estatico-new %}) se mostrou eficiente, melhorando e simplificando a base de código em muitos projetos.

Entretanto, a técnica a seguir traz todas as vantagens do `New` porém sem as desvantagens.

Você pode fazer essa "mágica" adicionando mais um método na classe.

Eu o chamo de Método `Ref`, que significa *reference for*:

    function TFoo.Ref: ISomething;
    begin
      result := self;
    end;

Sim, é só isso. 

Um método que retorna `self`, sendo o tipo do retorno a interface que a classe implementa.

Diferentemente do `New`, o método `Ref` não é estático e não duplica a assinatura do(s) construtor(s), eliminando as primeiras duas desvantagens.

Sim, há a possibilidade do desenvolvedor esquecer de utilizá-lo ao chamar o construtor da classe diretamente ao invés do `New`e isso poderia ocasionar *memory leaks*. Mas sua vantagem é que você tem a *opção* em não utilizá-lo para aumentar a performance na criação dos objetos *by passing* um método de classe (o método `New`) antes de chegar ao construtor, ganhando, talvez, alguns milionésimos de segundos.

Veja um exemplo:

    procedure DoIt;
    begin
      TFoo.Create(TBar.Create).Ref.Execute;
    end;

Se ambas as classes implementam interfaces, temos a possibilidade de não utilizar `Ref` no argumento do construtor de `TFoo`, chamando apenas o construtor de `TBar`. Mas, somos "obrigados" a utilizar `Ref` na chamada mais externa para executar algum método da interface que `TFoo` implementa, afim de evitar *memory leaks*, pois não há nenhuma variável (local) para receber a instância da interface.

Agora você deve estar se perguntando, o que isso tem have com o título do artigo?

Tudo.

É padrão da linguagem que, ao trabalharmos com instâncias de interfaces, o compilador utilize um contador de referências para saber quando destruir um objeto, liberando a memória alocada.

Então, se você criar um objeto do tipo de classe e depois utilizar o método `Ref` ou apenas atribuir a instância a uma variável do tipo interface, você irá "transformar" sua instância de classe em instância de interface, ganhando a contagem de referência e a auto destruição do objeto... de graça.

Vejamos um exemplo completo:

    program Project1;

    {$mode delphi}

    uses
      SysUtils,
      Classes;

    type
      IFoo = interface
      ['{1D26066C-984B-4632-95B2-B25253AF149E}']
        procedure ShowMsg;
      end;

    type
      TFoo = class(TInterfacedObject, IFoo)
      private
        fText: string;
      public
        constructor Create(const aText: string);
        destructor Destroy; override;
        function Ref: IFoo;
        procedure ShowMsg;
      end;

    { TFoo }

    constructor TFoo.Create(const aText: string);
    begin
      inherited Create;
      fText := aText;
      writeln('Creating '+ FRefCount.ToString);
    end;

    destructor TFoo.Destroy;
    begin
      writeln('Destroing ' + FRefCount.ToString);
      inherited Destroy;
    end;

    function TFoo.Ref: IFoo;
    begin
      result := self;
    end;

    procedure TFoo.ShowMsg;
    begin
      writeln(fText);
    end;

    var
      f: TFoo; // class, not an interface
    begin
      f := TFoo.Create('teste');
      f.Ref.ShowMsg;
      // f.Free; << you don't need it
    end.

Execute o código acima e verá que não existe nenhum *memory leak*, mesmo `f` sendo do tipo `TFoo`, uma classe.

A mágica acontece quando chamamos `Ref`. Ele retorna um tipo de interface que é o próprio objeto `self`. Assim, a "nova" instância passa a ter uma contagem de referência e será destruída automaticamente.

    c:\temp>project1.exe
    Creating 1
    teste
    Destroing 0
    Heap dump by heaptrc unit of c:\temp\project1.exe
    76 memory blocks allocated : 1967/2120
    76 memory blocks freed     : 1967/2120
    0 unfreed memory blocks : 0
    True heap size : 196608 (80 used in System startup)
    True free heap : 196528

A linguagem Object Pascal pode parecer simples demais para alguns desenvolvedores (que não conhecem bem a linguagem).

Alguns dizem que é "apenas uma linguagem para aprendizado" ou que não acompanhou as "novas features" das linguagens mais "modernas".

Me diga então, qual linguagem você conhece que nos dá a eficiência em poder liberar nossos objetos no melhor momento que quisermos, mas também ter objetos que se "auto destroem" e ainda conseguir fazer a transição de um tipo para o outro?

Até logo.