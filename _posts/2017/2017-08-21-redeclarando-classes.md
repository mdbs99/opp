---
layout: post
title: Redefinindo Classes
date: 2017-08-21
permalink: /:title
description:
  A redeclaração de Classes é um método prático para minimizar a colisão de nomes entre Classes, mesmo utilizando identificadores curtos.
image: /images/2017/photo-mpumelelo-macu-283883.jpg
tags:
  - object pascal
keywords:
  - freepascal
  - fpc
  - delphi
  - lazarus
  - object-oriented
  - oop
  - mdbs99
  - pascal
  - object pascal
  - redeclarando
  - redefinindo
  - redefining
  - alias
---

A redeclaração de Classes é um método prático para minimizar a colisão de nomes entre Classes, mesmo utilizando identificadores curtos.

<!--more-->

![Unsplash image]({{ page.image }}) 
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Mpumelelo Macu on Unsplash</span>

## Introdução {#introducao}

A linguagem *Object Pascal* tem uma *feature* muito útil que, acredito, não é muito utilizada pelos desenvolvedores.

Não sei se há um nome específico pra isso, mas eu a chamo de *Redefinição de Classes*.

Na verdade essa *feature* pode ser utilizada para redeclarar Classes, Constantes e até mesmo Funções.

Porém, na Orientação a Objetos, basicamente só utilizamos Classes. Então esqueça o resto e vamos nos concentrar nelas.

Vamos ver algumas técnicas que podem ser aplicadas utilizando tal conceito.

## Renomeando {#renomeando}

Imagine que você quer utilizar uma classe de alguma *Lib*, mas essa Classe tem o mesmo nome — exemplo `TSmartMemory`— de uma de suas Classes que você já utiliza por todo o seu código. O que fazer?

A primeira opção é nunca utilizar ambas as Classes numa mesma Unidade. Mas talvez você não tenha essa sorte.

A segunda opção é prefixar uma das Classes com o nome da Unidade — muito comum ver isso em projetos Java — por exemplo:

    uses
      LibMemory,
      MyMemory;

    begin
      M := TSmartMemory.New;  // your class
      L := LibMemory.TSmartMemory.Create;
    end

A terceira opção, que eu utilizo na maioria das vezes, é *"renomear"* a Classe da *Lib* para uma nomenclatura que não colide com a nomenclatura já utilizada no meu projeto. Vejamos um exemplo:

    unit MyMemory;

    uses
      LibMemory;

    type
      // TSmartMemory from LibMemory
      TLibStartMemory = TSmartMemory; 

      // my new class
      TSmartMemory = class
        // ...
      end;


Nesse exemplo ambas as Classes estão declaradas na mesma Unidade existente no projeto — a `MyMemory` — e o projeto poderá utilizar ambas as Classes sem colisão de nomenclatura.

    begin
      M := TSmartMemory.New;
      L := TLibSmartMemory.Create;
    end;

Utilizando essa técnica evitamos o conflito de nomes, o que é muito útil.

Ao invés de utilizarmos o nome real da Classe, podemos lhe dar um *apelido*. O código fica mais limpo, simples e com identificadores curtos, pois não temos a necessidade de utilizar a Unidade como prefixo.

A linguagem C# [tem algo muito parecido](https://docs.microsoft.com/dotnet/csharp/language-reference/keywords/using-directive) o que me faz pensar de onde será que o principal arquiteto da linguagem C# tirou essa ideia.
    
## Visibilidade {#visibilidade}

Muitas vezes precisamos utilizar uma composição de diferentes Classes para resolver um problema.

Mas se cada uma dessas Classes tiver sido declarada numa Unidade diferente, precisaremos declarar todas essas Unidades, para ter acesso a cada uma dessas Classes.

Por exemplo. Precisamos da `TClass1`, `TClass2` e `TClass3`. Cada uma delas em Unidades diferentes, `Unit1`, `Unit2` e `Unit3`, respectivamente.

    unit MyUnit;

    uses
      Unit1, Unit2, Unit3;  
      
    begin
      TClass3.New(
        TClass2.New(
          TClass1.New
        )
      )
    end;

Se precisarmos utilizar essa composição em muitos lugares do código, teremos sempre que lembrar em quais Unidades essas Classes estão.

Outra opção é dar visibilidade às Classes, redeclarando todas elas numa única Unidade, por exemplo `Unit123`, para que possamos utilizá-las de forma mais simples, porém ainda mantendo cada implementação em Unidades diferentes.

    unit Unit123;

    uses
      Unit1, Unit2, Unit3;

    type
      TClass1 = Unit1.TClass1;
      TClass2 = Unit2.TClass2;
      TClass3 = Unit3.TClass3;

Agora, basta utilizarmos a `Unit123` no código para ter acesso a todas as 3 Classes que anteriormente só poderiam ser acessadas em Unidades distintas.

    unit UnitTest;

    uses
      Unit123;  
      
    begin
      TClass3.New(
        TClass2.New(
          TClass1.New
        )
      )
    end;

Esta técnica é bastante útil para simplificar uma API, fornecendo aos desenvolvedores somente algumas poucas Classes para uso em contextos bem específicos.
    
## Herança {#heranca}

Há momentos que queremos utilizar [Herança de Classes]({% post_url 2017-03-06-como-utilizar-heranca-apropriadamente %}) — mesmo sendo a Composição de Objetos a melhor escolha — porém gostaríamos de utilizar o mesmo nome da Classe ancestral.

Vamos imaginar que estamos desenvolvendo um software e que em uma de suas Unidades tem uma Classe que [representa]({% post_url 2016-02-29-objetos-representam-entidades %}) um arquivo PDF. Nomeamos essa Classe como `TPDFFile`.

No entanto sabemos que poderá haver dezenas ou centenas de *Libs* que já trabalham com PDF. Então vamos chamar uma dessas *Libs* de *"PDFLib"*, apenas como exemplo.

Na *PDFLib* temos uma Classe chamada `TPDFFile` que é exatamente o mesmo nome que já decidimos que será utilizada no nosso software, mas o arquiteto do projeto diz que nossa Classe deve herdar de `PDFLib.TPDFFile`.

Acho que você já sabe a resposta:

    unit MyPDFUnit;

    uses
      PDFLib;

    type
      TPDFFile = class(PDFLib.TPDFFile)
        // more methods
      end;
      
Prefixando a Classe da *Lib* podemos identificá-la de forma diferente da nossa Classe declarada na mesma Unidade.

A Classe `TPDFFile` é agora uma extensão de `PDFLib.TPDFFile` que pertence a uma *Lib* externa. Mas para todo o restante do código no projeto, só existirá a Classe `MyPDFUnit.TPDFFile` que representa um PDF. 

## Estendendo {#estendendo}

A técnica que vou mostrar agora já era utilizada antes mesmo de haver a sintaxe para *Classes Helpers*.

Por exemplo, imagine que você queira incluir novas propriedades ou métodos na Classe `TEdit`. O primeiro pensamento é utilizar herança para criar um novo componente. No entanto você não quer substituir cada `TEdit` em todos os `TForm` já existentes no projeto. Então, o que fazer?

A resposta continua sendo utilizar Herança. Mas há um truque ou *hack* que irei mostrar na técnica a seguir. Mas gostaria de lembrar que isso deve ser utilizado com muita parcimônia. Eu só utilizei essa técnica pouquíssimas vezes e somente para Classes que representam *widgets*, ou seja, Classes de componentes que são utilizadas em Formulários.

Para extender um `TEdit` sem criar uma nova Classe e sem ter que alterar os Formulários, basta utilizar a mesma técnica acima:

    unit MyStdCtrls;

    type
      TEdit = class(StdCtrls.TEdit) // or Vcl.StdCtrls.TEdit
        // more methods and properties
      end;

Você não precisa alterar os *widgets* nos Formulários. A renderização do *design* do Formulário irá funcionar utilizando os recursos (\*.lfm \| \*.dfm) corretamente.

Porém há um truque que você não pode esquecer: Em cada um desses Formulários você precisará [declarar]({% post_url 2017-07-17-declarando-unidades %}) sua Unidade **depois** da Unidade `StdCtrls` real.

    unit MyForm1;

    uses
      StdCtrls, MyStdCtrls;
      
Esse *hack* é necessário para que o compilador ache a declaração da sua Unidade antes de encontrá-la na `StdCtrls` padrão.

Por isso é tão importante ter uma [ordem]({% post_url 2017-07-17-declarando-unidades %}) na declaração das Unidades.

## Nomenclatura Opcional {#opcional}

Essa técnica pode ser considerada o oposto da primeira mostrada nesse artigo, ou seja, ao invés de você "renomear" uma Classe pertencente a outra *Lib*, você irá dar ao desenvolvedor opções de nomenclatura para o uso das suas Classes.

Imagine que você codificou um *package* com algumas Classes. Nesse *package* você gosta de utilizar nomes simples como `TStream`, `TMemoryStream`, `TString`, `TInteger`, etc.

Veja que já temos alguns possíveis "problemas" aqui. Vejamos:

1. As Classes `TStream` e `TMemoryStream` já existem no FPC\|Delphi e por isso poderá haver colisão de nomes quando os desenvolvedores forem utilizar seu *package*, pois é bem provável que eles já estejam utilizando tais Classes padrão em seus projetos;
2. As Classes `TString` e `TInteger` são Classes com nomes muito simples ou genéricos e, novamente, é provável que alguma outra *Lib* ou mesmo os próprios projetos desses desenvolvedores já utilizem tais nomes.

O problema aqui é o nome *limpo* e *curto*. Ao mesmo tempo que esses nomes são *perfeitos*, eles tem o grande potencial de gerar o problema da colisão de nomenclatura.

Mas quando você está criando seu *package*, você tem que abstrair o mundo externo. O [contexto]({% post_url 2016-10-03-delegacao-de-implementacao-de-interfaces %}#contextos) do *package* que deve influenciar a nomenclatura das Classes e não o mundo externo que, talvez, irá utilizá-lo!

Eu "lutei" contra esse problema por muito anos. Antes de "descobrir" essa técnica, eu prefixava minhas Classes com 1, 2 ou 3 letras — esse parece ser o padrão utilizado por todos os desenvolvedores de componentes do mercado — porém você pode descobrir com o tempo que "seu prefixo" escolhido para suas Classes já foi utilizado em outra *Lib* de terceiros.

Imagine você ter que utilizar nomes verbosos como `TXyzMemoryStream` por todo o seu código para, então, descobrir que `Xyz` já é o prefixo utilizado por uma grande "fazedora de componentes" do mercado.

Então eu descobri que a linguagem *Object Pascal* já tinha uma resposta e eu poderia ter o melhor dos dois mundos. Eu poderia utilizar nomes simples, compactos e limpos dentro do meu contexto (*package*, projeto, *lib*) mas dar aos potenciais usuários\|desenvolvedores um nome mais verboso, porém com menor possibilidade de haver colisão de nomes, se assim eu desejasse.

Vejamos um exemplo. Uma das Classes do [James](https://github.com/mdbs99/james/blob/master/src/james.data.clss.pas) tem o nome `TDataStream`. Esse nome já evita a colisão com `TStream` que é basicamente o que essa Classe representa, porém o "prefixo" `Data` não foi utilizado para minimizar a colisão de nomenclatura, mas sim devido a sua semântica.

Mas digamos que agora o Lazarus e/ou Delphi implementaram uma Classe de uso geral chamada... `TDataStream`.

Eu deveria mudar o nome dessa Classe no [Projeto James](https://github.com/mdbs99/james) e em todos os meus projetos que já utilizam essa nomenclatura? É claro que não!

Eu só iria dar a opção para os (novos) desenvolvedores utizarem outra nomenclatura para a mesma Classe, desse jeito:

    type
      TDataStream = class sealed(TInterfacedObject, IDataStream)
      // some methods
      end;
      
      TJamesDataStream = TDataStream;

O código agora tem 2 possibilidades de uso de nomenclatura para a mesma Classe!

Você pode continuar utilizando `TDataStream` por todo o seu código, desde que mantenha a [ordem de declaração]({% post_url 2017-07-17-declarando-unidades %}) das Unidades, ou pode utilizar a nova opção `TJamesDataStream`.

E se mesmo assim ainda houver colisão de nomenclatura, aí caberá ao desenvolvedor utilizar a primeira técnica desse artigo.

Dessa forma ambos os lados (desenvolvedores de *packages* e usuários de *packages*) estão *livres* para utilizarem os nomes que quiserem em suas Classes.

A única exceção, até onde eu sei, são os componentes que são instalados na IDE. Ambas as IDE's (Lazarus e Delphi) não deixam instalar componentes com o mesmo nome, mesmo estando em *packages* distintos. No entanto, na minha opinião, isso é um erro de *design* das IDE's. Ambas deveriam permitir a instalação e, ao utilizarmos tais componentes, bastaria a IDE declarar os *fields* no Formulário utilizando a nomenclatura totalmente qualificada, como `Edit1: StdCtrls.TEdit;` e tudo iria funcionar.

Eu fiz essa [proposta](http://lists.lazarus-ide.org/pipermail/lazarus/2011-January/123929.html) na lista oficial do Lazarus em Janeiro/2011, porém ainda sem previsão, prioridade ou interesse.

## Conclusão {#conclusao}

A redeclaração ou renomeação de Classes (e constantes, funções, etc) é uma técnica que existe quase desde os primórdios da linguagem, mas que a grande maioria dos desenvolvedores não utiliza, seja por desconhecimento, inabilidade ou mesmo falta de interesse.

É um conceito *antigo*, mas que nos proporciona várias técnicas e possibilidades para codificar melhor *hoje*, conforme demonstrado nesse artigo.

Até logo.
