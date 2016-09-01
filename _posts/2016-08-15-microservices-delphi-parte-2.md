---
layout: post
title: Microservices com Delphi — Parte 2
date: 2016-08-15
description: Como implementar uma simples API para fazer a comunicação com Microservices em Java.
summary: Como implementar uma simples API para fazer a comunicação com Microservices em Java.
image: /images/photo-1457305237443-44c3d5a30b89.jpg
categories: 
  - Pascal
tags:
  - microservices
keywords:
  - microservices
  - java
  - delphi
--- 

Não basta apenas utilizar um Protocolo HTTP para fazer a comunicação com os Microservices. É necessário, também, codificar um Localizador de Microservices.

Veja como um implementar um Localizador simples, utilizando um SGBD, e também a implementação de um *Client* para fazer a comunicação com qualquer Microservice no nosso *framework*.

<!--more-->

![Imagem]({{ page.image }})

##Introdução {#introducao}

No [artigo anterior]({% post_url 2016-08-08-microservices-delphi-parte-1 %}) eu escrevi sobre o Projeto codificado em Delphi 7 que está sendo recodificado utilizando Microservices em Java.

Nesse artigo irei falar sobre o Módulo Microservices, que possui todo o necessário para fazer a comunicação com os serviços.

##Módulo MicroService {#modulo-microservice}

O Módulo MicroService é composto de apenas 4 simples Interfaces (até agora).

  * **IMicroServiceParams**: Representa um localizador de MicroService;
  * **IMicroServiceResponse**: Representa um `Response` após uma chamada a um MicroService;
  * **IMicroServiceClient**: Representa o *Client* para todos os Microservices;
  * **IMicroServiceAction**: Representa uma Ação (irei falar sobre essa Interface em outro artigo).

###Unit AcmeMicroServices.pas {#unit-microservices}

Aqui está a implementação das Interfaces.

Há algumas dependências que não estarão no escopo desses artigos, mas acredito que seja fácil abstrair as Classes ou Interfaces como, por exemplo, `IDataParams` e `IXMLDocument`.
  
{% highlight pascal %}
type
  IMicroServiceParams = interface
    function Find: IDataParams;
  end;

  IMicroServiceResponse = interface
    function Code: Integer;
    function Empty: Boolean;
    function XML: IXMLDocument;
  end;

  IMicroServiceClient = interface
    function Send(const Content: string): IMicroServiceResponse; overload;
    function Send(XML: IXMLDocument): IMicroServiceResponse; overload;
  end;

  IMicroServiceAction = interface
    function Act: IMicroServiceResponse;
  end;
{% endhighlight text %}


##Localizando Serviços {#localizando-servicos}

Se sua aplicação utiliza apenas 3 ou 5 Microservices, não haveria necessidade de implementar um Localizador. Mas se sua aplicação lida com dezenas ou mais de Microservices, então é necessário haver um "índice" de Microservices.

O índice está no SGBD, em apenas 1 tabela (atualmente), e a localização de qualquer serviço é feita através do seu *name*.

Sim, é só isso.

A tabela — vamos dar o nome de Micro_Services para esse artigo — possue algumas colunas.

As mais relevantes são:

  * **[name]**: O nome do serviço. Esse nome deve ser único;
  * **[server]**: O IP do servidor onde o MicroService está hospedado;
  * **[path]**: O *path* do serviço, exemplo: http://10.20.0.10/execute onde "/execute" é o *path*;
  * **[port]**: A porta onde o serviço está hospedado, exemplo: http://10.20.0.10:8020/execute (8020 é a porta);
  * **[encoding]**: Alguns serviços trabalham com *encode* diferente de UTF-8;
  * **[verb]**: Existem serviços que utilizam GET outros POST, etc. Essa coluna informa qual verbo HTTP utilizar;

###Unit AcmeMicroServiceA.pas {#unit-microservicesa}

Essa *unit* encapsula as Classes que implementam as Interfaces acima.

A parte importante por aqui é o método `TMicroServiceClient.Response`. Esse método utiliza o resultado de `TMicroServiceParams.Find`, que é um localizador de serviços cadastrados, para montar uma requisição HTTP completa.

Consegue ver a beleza do método `TMicroServiceClient.Response` onde (quase) tudo são Objetos conversando e [tomando decisões]({% post_url 2016-03-14-objetos-pensam-e-tomam-decisoes %}) entre si?

{% highlight pascal %}
type
  EMicroService = class(Exception);

  TMicroServiceParams = class(TInterfacedObject, IMicroServiceParams)
  private
    FServiceName: string;
  public
    constructor Create(const ServiceName: string);
    class function New(const ServiceName: string): IMicroServiceParams;
    function Find: IDataParams; overload;
  end;

  TMicroServiceResponse = class(TInterfacedObject, IMicroServiceResponse)
  private
    FCode: Integer;
    FXML: IXMLDocument;
  public
    constructor Create(Code: Integer; XML: IXMLDocument);
    class function New(Code: Integer; XML: IXMLDocument): IMicroServiceResponse;
    function Code: Integer;
    function Empty: Boolean;
    function XML: IXMLDocument;
  end;

  TMicroServiceClient = class(TInterfacedObject, IMicroServiceClient)
  private
    FParams: IDataParams;
    function Response(const Content: string): IHttpResponse;
  public
    constructor Create(ServiceParams: IDataParams);
    class function New(ServiceParams: IDataParams): IMicroServiceClient;
    function Send(const Content: string): IMicroServiceResponse; overload;
    function Send(XML: IXMLDocument): IMicroServiceResponse; overload;
  end;

implementation

uses
  XMLIntf;

{ TMicroServiceParams }

constructor TMicroServiceParams.Create(const ServiceName: string);
begin
  inherited Create;
  FServiceName := ServiceName;
end;

class function TMicroServiceParams.New(const ServiceName: string): IMicroServiceParams;
begin
  Result := Create(ServiceName);
end;

function TMicroServiceParams.Find: IDataParams;
var 
  Q: ISQLQuery;
begin
  // Através de uma Query (instância Q), 
  // pesquisa pelo nome do serviço (FServiceName).
  // Utilizando os Fields, retornamos uma instância TDataParams.
  Result := TDataParams.New(Q.Fields);
end;

{ TMicroServiceResponse }

constructor TMicroServiceResponse.Create(Code: Integer; XML: IXMLDocument);
begin
  inherited Create;
  FCode := Code;
  FXML := XML;
end;

class function TMicroServiceResponse.New(Code: Integer; XML: IXMLDocument): IMicroServiceResponse;
begin
  Result := Create(Code, XML);
end;

function TMicroServiceResponse.Code: Integer;
begin
  Result := FCode;
end;

function TMicroServiceResponse.Empty: Boolean;
begin
  Result := (FCode = 204) or (FXML.ChildNodes.Count = 0);
end;

function TMicroServiceResponse.XML: IXMLDocument;
begin
  Result := FXML;
end;

{ TMicroServiceClient }

function TMicroServiceClient.Response(const Content: string): IHttpResponse;
begin
  try
    Result :=
      THttpClient.New(
        TWebURL.New(
          FParams.Param('server').AsString,
          FParams.Param('path').AsString,
          FParams.Param('port').AsInteger
        )
        .AsString,
        'application/xml;charset=' +
          FParams.Param('encoding').AsString,
        TDataStream.New(Content)
      )
      .Execute(FParams.Param('verb').AsString);
  except
    on E: Exception do
    begin
      raise EMicroService.Create(
        'Service: ' + FParams.Param('name').AsString + #13 +
        'Error: ' + E.Message
      );
    end;
  end;
end;

constructor TMicroServiceClient.Create(ServiceParams: IDataParams);
begin
  inherited Create;
  FParams := ServiceParams;
end;

class function TMicroServiceClient.New(ServiceParams: IDataParams): IMicroServiceClient;
begin
  Result := Create(ServiceParams);
end;

function TMicroServiceClient.Send(const Content: string): IMicroServiceResponse;
begin
  with Response(Content) do
  begin
    Result := TMicroServiceResponse.New(
      Code,
      TXMLFactory.New('ISO-8859-1', Stream).Document
    );
  end;
end;

function TMicroServiceClient.Send(XML: IXMLDocument): IMicroServiceResponse;
begin
  Result := Send(XML.XML.Text);
end;
{% endhighlight text %}

##No próximo artigo… {#no-proximo-artigo}

Isso é código real, em produção!

E onde está o Tratamento de Exceções, Classes de Negócio, etc?

No [próximo artigo]({% post_url 2016-08-22-microservices-delphi-parte-3 %}) você irá ver como construir uma **Classe de Negócio** que faz uso de todo esse arcabouço.

Até logo.
