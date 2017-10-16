---
layout: post
title: Injeção de Dependência sem XML, Atributos/Anotações ou Frameworks
date: 2017-10-16
permalink: /:title
description:
  Injeção de Dependência sem depender de características específicas da linguagem, Arquivos XML, Atributos/Anotações ou Frameworks.
image: /images/2017/photo-vadim-sherbakov-36.jpg
categories:
  - Pascal
tags:
  - naming
keywords:
  - freepascal
  - fpc
  - delphi
  - lazarus
  - object-oriented
  - oop
  - mdbs99
  - injeção-dependencia
  - dependency-injection
---

É possível utilizar a Injeção de Dependência na Orientação Objetos sem depender de características específicas da linguagem, Arquivos XML, Atributos/Anotações ou *Frameworks*?

<!--more-->

![Unsplash image]({{ page.image }}) 
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Vadim Sherbakov on Unsplash</span>

## Introdução {#introducao}

A [Injeção de Dependência](https://en.wikipedia.org/wiki/Dependency_injection) ou apenas DI (em Inglês: *"Dependency Injection"*) pode ser um assunto bem controverso que ainda geram muitas dúvidas entre desenvolvedores.

Será que DI é algo complexo que exige um *framework* para gerenciar as dependências entre Classes de um *software*?

É necessário um arquivo XML para que o desenvolvedor possa configurar tais dependências?

Talvez possamos utilizar Atributos/Anotações para facilitar essas configurações?

Como podemos utilizar o conceito de [IoC](https://en.wikipedia.org/wiki/Inversion_of_control) sem um *framework*?

O mercado tem várias maneiras de "injetar" alguma coisa, porém eu lhe digo: Injeção de Dependência é apenas passagem de parâmetro entre Objetos.

## Artefatos {#artefatos}

Qualquer sistema [Orientado a Objetos]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}) que faça algo de útil, terá dependências entre suas Classes ou Módulos.

Um Módulo/Classe `A` que usa `B` que usa `C` (`A->B->C`) estão conectados entre si.

Sempre devemos minimizar essas dependências afim de facilitarmos a manutenção do código, porém é impossível não tê-las.

Mesmo assim, o mercado resolveu criar todo tipo de "artefatos" como *frameworks*, atributos/anotações ou configurações em arquivos XML, para "facilitar" a comunicação entre os Objetos, tornando "transparente" para o desenvolvedor as dependências existentes entre as Classes.

Na minha opinião, eles trouxeram mais problemas e *complexidade* do que uma real solução.

Pense bem:

  * Um *Framework* para Injeção de Dependência, deixa seu código *dependente* do próprio *framework* que gerencia as dependências!

  * Os Atributos/Anotações que informam quais atributos do Objeto devem ser inicializados "magicamente" utilizando *reflection/RTTI* ou utilizando [*Services Locators*](https://en.wikipedia.org/wiki/Service_locator_pattern), retiram do Objeto seus [construtores]({% post_url 2016-03-21-construtores-da-classe-primario-secundarios %}), tornando-o *anêmico* e *procedural*, pois é preciso chamar [*Setters*]({% post_url 2016-06-27-getters-e-setters %}#setters) para "configurar" o Objeto antes de fazer algo de útil com ele. Seu uso é totalmente desencorajado pois viola o [encapsulamento]({% post_url 2016-05-30-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-2 %}#encapsulamento) transformando o Objeto num ["saco de dados"]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}#objeto-nao-e-um-balde-de-funcoes-e-dados), além de introduzir [acoplamento temporal](http://blog.ploeh.dk/2011/05/24/DesignSmellTemporalCoupling/).

  * Arquivos XML de configuração que armazenam quais Classes serão utilizadas para instanciar tais [Interfaces]({% post_url 2016-01-18-interfaces-em-todo-lugar %}) é algo abominável: é estático, não segue o princípio [DRY](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself), e não tem checagem de sintaxe.

Tais "soluções" criam, na verdade, outros problemas.

Mas, e se fosse possível ter um código mais [simples]({% post_url 2016-12-19-simplicidade %}), sem XML, sem Atributos/Anotações ou mesmo sem *Frameworks*, para trabalharmos com a Injeção de Dependência utilizando *somente* Classes e Objetos, você iria continuar utilizando outros artefatos complexos?

Vamos ver quais são os tipos de dependências mais utilizadas e então veremos se é possível termos soluções simples para cada uma delas.

## Dependência por Construtor {#dependencia-por-construtor}

Dependência por Construtor é a maneira mais simples e eficaz de resolver dependências entre Objetos.

Esse é o tipo de *dependência pura* que é negligenciado por muitos desenvolvedores.

Exemplo:

    THash.New(
      TFile.New('file.xml').Stream
    ).Calc

No exemplo acima a Classe `THash` recebe um `Stream` (dependência) no seu construtor. Obviamente o tipo do argumento é uma Interface (ex: `IStream`) para que não haja uma dependência "física" entre as Classes.

Não há *nenhum* problema a ser solucionado aqui. Essa é a forma mais correta e eficaz para "injetar" uma dependência.

Sempre tente utilizar construtores para passar as dependências para os Objetos. A única exigência é que os argumentos do construtor sejam do tipo Interface ao invés de Classes.  

## Dependência por Decoração {#dependencia-por-decoracao}

A utilização de bibliotecas (Libs) é muito útil para desenvolvermos mais rápido. Ao invés de criamos tudo que o software precisa, podemos delegar vários serviços às Libs especialistas no assunto. É como "contratar" consultores profissionais para cada especialidade.

O "problema" é que essas Libs utilizam seus próprios Objetos. Bem, elas sabem o que fazem, porém há momentos que gostaríamos de "dizer" a essas Libs quais Objetos deveriam retornar em alguns momentos.

Seria interessante se pudéssemos [decorar]({% post_url 2016-05-02-decorator-pattern %}) instâncias retornadas por Objetos pertencentes a Lib.

Por exemplo. No código abaixo nós obtemos uma instância de um Objeto que representa um Atributo num arquivo XML:

    Attr := 
      TXMLPack.New(
        TFile.New('file.xml').Stream
      )
      .Node('/root')
      .Attrs
      .Item('id');
    ShowMessage(Attr.Text);

Veja que o desenvolvedor não tem controle sobre como a Lib irá instanciar esse Objeto internamente, pois primeiro `TXMLPack` retorna outro Objeto em `Node()` e então retorna uma lista `Attrs` para então pesquisar e retornar a instância que queremos.

É claro que podemos decorar qualquer instância retornada por qualquer Lib dentro do nosso próprio código, mas teríamos que fazer isso todas as vezes em todos os lugares.

Por exemplo, se quiséssemos decorar instâncias de `TXMLAttribute` (Lib) com a nossa Classe `TMyAttribute`, teríamos que implementar dessa forma:

    Node := TXMLPack.New(
      TFile.New('file.xml').Stream
    )
    .Node('/root');
    
    // id attribute
    IdAttr := 
      TMyAttribute.New(
        Node('/root').Attrs.Item('id')
      );
    
    // name attribute
    NameAttr := 
      TMyAttribute.New(
        Node('/root').Attrs.Item('name')
      );

Veja acima que foi preciso decorar cada instância (Atributos `id` e `name`) com a nossa Classe. 
      
Mas mesmo sendo um código *repetitivo*, é *viável*.

No entanto, se a Lib for desenvolvida para dar a opção ao desenvolvedor para decorar esses Objetos criados internamente, é provável que ela utilize o padrão [*Abstract Factory*](https://en.wikipedia.org/wiki/Abstract_factory_pattern).

Essa seria uma opção simples para utilizar Injeção de Dependência de forma totalmente desacoplada e sem repetições.

Vamos a um exemplo:

    { First we need to include a factory
      in some place of the code }
      
    TXAttributeFactories.New
      .Add(
        // your own factory
        TMyAttributeFactory.New
      );

    // ...  
      
    { Then, Lib's code will use these factories
      to decorate each new Attribute instance }
      
    function TCAttributes.Item(AIndex: Integer): IXAttribute;
    var
      A: TDOMNode;
    begin
      A := FNode.Attributes.Item[AIndex];
      if not Assigned(A) then
        raise EXError.CreateFmt(
          'Node not found on index %d.', [AIndex]
        );

      { At this point, factories instance will 
        find TMyAttributeFactory instance and it 
        will use to return a new instance, 
        wrapping TCAttribute instance }

      Result := 
        TXAttributeFactories.New
          .Decorate(
            TCAttribute.New(FNode, A)
          );
    end;

O código acima é do [Projeto Xavier](https://github.com/mdbs99/xavier/blob/47240049ff594904a856320d0b6e4ed8c979529f/src/xavier.xml.fpc.pas#L179), porém a chamada à Classe `TXAttributeFactories` é apenas uma simulação, exemplificando como a implementação poderia ser codificada.

Veja que o método `Decorate(Attr: IXAttribute)` recebe apenas uma instância já criada, ou seja, a fábrica não sabe como criar uma nova instância de `IXAttribute`, ela apenas sabe como decorar uma instância já existente.
    
## Dependência Tardia {#dependencia-tardia}

Chamo de dependência tardia quando um `Objeto A` só precisa de uma instância `B` num dado momento, dependendo de uma escolha do usuário ou evento externo. Em outras palavras, não sabemos se iremos precisar ou não da instância do `Objeto B` quando estivermos compondo nossos Objetos para trabalhar numa determinada tarefa.

Talvez esse tenha sido o grande problema que mais motivou a construção dos artefatos e *frameworks* já citados.

Vamos a um exemplo:

Suponha que o `Objeto A` receba uma conexão com um SGBD através de seu construtor e em um dos seus métodos é instanciado um `Objeto B`.

A instância de `B` também precisa da conexão com o SGBD, então basta passarmos no construtor de `B` a conexão já recebida no `Objeto A`.

Então `B`, em um de seus métodos, instancia e retorna um `Objeto C` e, nesse caso, `C` não precisa de uma conexão. No seu construtor não há nenhum argumento desse tipo.

O programa continua e agora essa instância de `C` precisa instanciar um `Objeto D`. Esse último Objeto, tem métodos que retornam outros Objetos, porém para instanciá-los uma conexão ao SGBD é necessária... mas nesse ponto não temos esse Objeto da conexão.

    A (conection)
      B (conection)
        C ()
          D ()
            E (conection ?)

Esse é o *grande* problema.

Onde o `Objeto D` iria conseguir uma conexão ao SGBD para utilizar como argumento na criação de outros Objetos que necessitam de tal argumento?

Nas linguagens Funcionais, existe o conceito de [Currying](https://pt.wikipedia.org/wiki/Currying).

Ao invés de termos funções que recebem N parâmetros, haverá funções com apenas 1 parâmetro que chamará outra função, e outra, e mais outra... 

Esse vídeo explica o conceito:

<iframe width="560" height="315" src="https://www.youtube.com/embed/ZasXwtTRkio" frameborder="0" allowfullscreen></iframe>

No entanto, pode ser um conceito difícil de entender pra quem é acostumando com linguagens imperativas.

Bem, não temos *Currying* nas linguagens Orientadas a Objetos, mas temos *Classes* e Objetos.

Então, utilizando apenas Classes e Objetos, como podemos fazer a Injeção de Dependência e obter argumentos onde e quando for preciso?

A resposta é a mesma: [*Abstract Factory*](https://en.wikipedia.org/wiki/Abstract_factory_pattern).

No entanto, ao invés de apenas decorar uma instância, a fábrica iria criar uma nova do início.

Essas fábricas teriam mais métodos, possivelmente métodos com as mesmas assinaturas dos construtores e [Métodos New]({% post_url 2016-01-10-interfaces-e-o-metodo-estatico-new %}) já existente na Classe original.

No exemplo anterior, o método `Decorate(...)` foi utilizado para decorar uma instância já existente. Aqui, sugiro utilizar a nomenclatura `Make(...)` que irá criar uma nova instância.

Esse tipo de fábrica seria muito mais utilizado em aplicações do que em Libs, no entanto.

## Conclusão {#conclusao}

Injeção de Dependência é [passagem de parâmetro](http://blog.ploeh.dk/2017/01/27/dependency-injection-is-passing-an-argument/).

Vimos nesse artigo algumas possíveis opções para resolver os tipos de Dependência mais comuns.

Vimos que não é necessário *frameworks* complexos ou qualquer outro artefato além de Classes e Objetos simples para utilizarmos Injeção de Dependência na sua forma mais pura.

Infelizmente o espaço de um único artigo não é suficiente para implementarmos uma solução completa.

Seja simples. Utilize Classes e respeite seus Objetos sem quebrar o encapsulamento.

Até logo.
