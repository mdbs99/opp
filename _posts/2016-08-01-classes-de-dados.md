---
layout: post
title: "Classes de Dados"
date: 2016-08-01
description: Como manipulamos os Dados em Projetos Orientados a Objetos.
summary: Como manipulamos os Dados em Projetos Orientados a Objetos.
image: /images/photo-1457904375453-3e1fc2fc76f4.jpg
categories: 
  - Pascal
tags:
  - data
keywords:
  - data
  - dados
  - classes
--- 

Objetos são constituídos de Estado e Comportamento, enviam mensagens uns aos outros e devem representar Entidades reais de tudo que existe fora do Contexto do Software.

E os Dados, onde estão os Dados?

<!--more-->

![Imagem]({{ page.image }})

##Introdução {#introducao}

Tenho dito, por várias vezes, que um Objeto não deve ser visto ou utilizado como um [balde de dados e funções]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}#objeto-nao-e-um-balde-de-funcoes-e-dados).

Objetos não são [*containers*]({% post_url 2016-02-22-datamodule-e-apenas-um-container %}) de transporte de dados. Eles devem [representar uma Entidade]({% post_url 2016-02-29-objetos-representam-entidades %}) fora do Contexto do Software. Não importa se a Entidade é um objeto, pessoa ou Dados.

E o que são Dados?

É difícil definir a diferença entre Dados e o Estado de um Objeto. Ambos, tecnicamente, podem ser considerados Dados. No entanto, eu acredito, há uma diferença teórica entre eles que muda nosso **modo de pensar** quando estamos construindo um *software* Orientado a Objetos.

>Dados também são representados por Objetos, pois eles também estão fora do Contexto do Software.

Mas como um Objeto não deve ser um "balde de dados" e ao mesmo tempo deve representar Dados?

##Dados e Estado {#dados-e-estado}

Tudo são Objetos. 

Tudo deve ser representado por Objetos. 

Não há diferenças, pensando em termos de Objetos, entre um **Carro** e o **número 9**, por exemplo. Ambos são Entidades que deveriam ser representados dentro do *software* por Objetos.

Mas a vida é difícil. Desenvolver *software* Orientado a Objetos de forma *perfeita* é quase impossível. Precisamos pensar na performance, na alocação de memória, uso de recursos, etc. E aí começa a confusão entre Dados e Estado.

Vejamos. Se tenho uma Classe `TUrl` com uma propriedade privada `FValue: string`, a propriedade é um **Dado** ou o **Estado** do Objeto?

Eu diria que representa o Estado do Objeto. A propriedade `FValue` é a URL em forma de string.

Mas como `string` é um tipo primitivo (no Pascal), pensamos em termos de Dados e não de Estado do Objeto.

Vejamos outro exemplo:

{% highlight pascal %}
type
  TFile = class(TInterfacedObject, IFile)
  private
    FPath: string;
    FName: string;
    FStream: IDataStream;
  public
    constructor Create(const Path, Name: string; 
      Stream: IDataStream);
    function Path: string;
    function Name: string;
    function Stream: IDataStream; 
  end;
{% endhighlight text %}

No exemplo acima, tem diferença entre Dados e Estado?

Sim.

Todas as 3 propriedades fazem parte do Estado do Objeto, no entanto a propriedade `FStream` representa os Dados do arquivo que estão no HD do computador.

A Classe `TFile` faz uso da Composição de Objetos. Não olhamos para `TFile` como um *container* de *bytes*, mas como um Objeto que representa um Arquivo no computador.

Mas, se olharmos apenas para o Objeto `FStream` dentro de `TFile`, ele poderia ser implementado assim:

{% highlight pascal %}
type
  TDataStream = class sealed(TInterfacedObject, IDataStream)
  private
    FStream: TMemoryStream;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    procedure Save(Stream: TStream); overload;
    procedure Save(const FileName: string); overload;
    function Size: Int64;
  end;
{% endhighlight text %}

Agora o Contexto mudou.

A propriedade `FStream` na Classe `TDataStream` representa o Estado do Objeto e não Dados do Objeto. Essa Classe representa um *stream* mas ela não sabe se o *stream* é proveniente de um Arquivo ou de registros em uma Tabela.

Estamos olhando para um novo Contexto e isso muda nossa percepção sobre o que é ou não Dados.

Veja que internamente `TDataStream` foi implementado utilizando um `TMemoryStream` para "guardar" o *stream* — mais uma vez utilizando Composição de Objetos — mas eu poderia optar por utilizar apenas *bytes* e um ponteiro para a memória. Não importa. Isso tudo é privado e ninguém sabe o que acontece dentro de um Objeto. O importante é saber distinguir a diferença entre Dados e Estado do Objeto.

##Dados na forma de Objetos {#dados-na-forma-de-objetos}

Classes de Dados são aquelas que geram Objetos que representam Dados.

Nós utilizamos Dados na forma de Objetos a muito tempo.

Uma Classe `TJsonObject` representa um *stream* de JSON, assim como uma Classe `TXmlDocument` representa um *stream* de XML.

Se não tivéssemos essas Classes, teríamos que trabalhar os Dados no nível de *bytes* e ponteiros na memória. Programação Procedural.

Queremos utilizar Objetos, pois eles facilitam nossas vidas. Então encapsulamos arquivos, *streams*, registros de Tabelas, etc em Classes que geram Objetos para manipular os Dados.
  
##Conclusão {#conclusao}

Não há problema se seu Objeto encapsula apenas Dados, contanto que esses Dados fazem parte da Entidade real que seu Objeto está representando.

Entender a sutil diferença entre Dados e Estado do Objeto irá fazer você codificar Classes melhores. 

Você não irá mais pensar em definir uma única Classe que encapsula todos os Dados, mas sim Classes que representam Entidades, que utilizam a Composição de Objetos para Encapsular outros Objetos que, por fim, irão representar os Dados.

Até logo.
