---
layout: post
title: "A função Supports pode ser traiçoeira"
date: 2018-03-12
permalink: /:title
description:
  Quando você se acostuma a utilizar somente instancias de interfaces, pode ser difícil entender alguns erros em tempo de execução, utilizando instâncias de classes com suporte a interfaces.
image: /images/2018/photo-michal-parzuchowski-224092-unsplash.jpg
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
  - supports
  - function
---

Quando você se acostuma a utilizar somente instancias de interfaces, pode ser difícil entender alguns erros em tempo de execução, utilizando instâncias de classes com suporte a interfaces.

<!--more-->

![Unsplash image]({{ page.image }})
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Michał Parzuchowski on Unsplash</span>

Você pode desenvolver seu projeto do zero, utilizando apenas [instâncias de interfaces]({% post_url 2016-01-18-interfaces-em-todo-lugar %}), e (quase) tudo estará sob seu controle.

Entretanto, há muitos projetos onde só iremos fazer algum tipo de manutenção. Esses projetos podem não utilizar o conceito de interfaces como explicado em vários artigos desse blog. Nesses casos, é necessário codificar de forma que mantenha o código legado funcionando, ao mesmo tempo que introduzimos boas práticas de [Orientação a Objetos]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}).

Em tais projetos, é comum vermos variáveis declaradas como tipo de alguma classe ao invés de alguma interface.

Isso quer dizer que haverá chamadas aos métodos <code>Free</code> de cada instância, por todo o código.

E não há nada de errado com isso. Essa é uma das características da linguagem Object Pascal que a torna muito eficiente para projetos de alta [performance]({% post_url 2017-12-11-argumentos-const %}).

No entanto, como já foi dito muitas vezes aqui nesse blog, o uso de instâncias de interfaces pode trazer muitos benefícios quando utilizada apropriadamente.

Os benefícios, entretanto, podem ser ofuscados pelo não funcionamento correto do código ao utilizarmos instâncias de classes junto com interfaces.

Veja o código abaixo e, antes de compilar e executar o programa no seu computador, tente descobrir se há algo errado:

    program Project1;

    uses
      SysUtils;

    type
      IBar = interface
      ['{C22FB8F4-1EC6-42C4-81E4-F2A52CC52258}']
      end;

      TBar = class(TInterfacedObject, IBar)
      end;

    var
      Bar: TBar;
    begin
      Bar := TBar.Create;
      try
        if Supports(Bar, IBar) then
          Writeln('Bar supports...');
      finally
        Bar.Free;
      end;
    end.

O código compila?

O código possui <i>memory leaks</i>?

O código irá lançar algum <i>Access Violation</i> (AV)?

Pense por um minuto...

Bem, eu posso lhe afirmar que o código compila sem erros mas há um grande problema de <i>design</i>.

A lógica geral está correta. Temos uma instância de classe chamada <code>Bar</code>; depois, checamos se essa instância suporta ou implementa a interface <code>IBar</code> utilizando a função <code>Supports()</code> da <code>SysUtils</code>; então, imprimimos algo na tela em caso afirmativo; finalmente, a instância <i>deveria</i> ser destruída utilizando <code>Bar.Free</code>, visto que é uma instância de classe e não de interface. Certo?

Errado.

Você irá receber um <i>AV</i> quando o compilador tentar destruir o objeto na linha <code>Bar.Free</code>.

E por quê isso ocorre?

Estamos utilizando uma das implementações da função <code>Supports()</code> — há alguns <i>overloads</i> — que retorna apenas um <code>boolean</code> para dizer ao programa se determinada instância suporta ou não uma determinada interface.

Ao fazer isso, <code>Supports()</code> irá obter uma instância de <code>IBar</code>, extraída da variável <code>Bar</code>. Isso irá alterar a contagem de referência e após o retorno da função, a instância <code>Bar</code> será destruída... antes de chegar na linha <code>Bar.Free</code>!

Na minha opinião, isso é um <i>erro</i> mesmo sendo <i>[by design](http://docwiki.embarcadero.com/Libraries/XE2/en/System.SysUtils.Supports)</i>.

Delphi e FPC funcionam da mesma forma.

Então, como consertar o código?

Basta alterar o tipo da variável para <code>Bar: IBar</code>. Ao fazer isso, o compilador irá lhe dizer que não é mais necessário chamar <code>Bar.Free</code>. Remova também essa linha e, consequentemente, a construção <code>try-finally</code>:

    var
      Bar: IBar;
    begin
      Bar := TBar.Create;
      if Supports(Bar, IBar) then
        Writeln('Bar supports...');
    end.

Após fazer isso, compile e execute novamente o programa.

Utilizando FPC 3.0.4 com <code>-gl</code> habilitado, eu vejo a saída abaixo, sem nenhum <i>memory leak</i> ou erros:

    c:\temp>project1.exe
    Bar supports...
    Heap dump by heaptrc unit
    48 memory blocks allocated : 1189/1296
    48 memory blocks freed     : 1189/1296
    0 unfreed memory blocks : 0
    True heap size : 163840 (80 used in System startup)
    True free heap : 163760

Muito cuidado ao utilizar instâncias de classes com suporte a interfaces e contagem de referência.

Até logo.
