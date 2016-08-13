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


<!--more-->

![Imagem]({{ page.image }})

##Introdução {#introducao}

No [artigo anterior]({% post_url 2016-08-08-microservices-delphi-parte-1 %}) 


###Localizando Serviços {#localizando-servicos}



####Unit MicroServiceA

Essa *unit* encapsula as Classes relativas a toda comunicação com os Microservices.

A parte importante por aqui é o método `TMicroServiceClient.Response`. Esse método utiliza o resultado de `TMicroServiceParams.Find`, que é um localizador de serviços cadastrados, para montar uma requisição HTTP completa.

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
    function Response(XML: IXMLDocument): IHttpResponse;
  public
    constructor Create(ServiceParams: IDataParams);
    class function New(ServiceParams: IDataParams): IMicroServiceClient;
    function Send(XML: IXMLDocument): IMicroServiceResponse;
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

function TMicroServiceClient.Response(XML: IXMLDocument): IHttpResponse;
begin
  try
    Result :=
      THttpClient.New(
        TWebURL.Create(
          FParams.Param('server').AsString,
          FParams.Param('path').AsString,
          FParams.Param('port').AsInteger
        )
        .AsString,
        'application/xml;charset=' +
          FParams.Param('encoding').AsString,
        TDataStream.New(XML.XML)
      )
      .Execute(FParams.Param('verb').AsString);
  except
    on E: Exception do
    begin
      raise EMicroService.Create(
        'Service: ' + FParams.Param('name').AsString + #13 +
        'Error: ' + E.Message + #13
        //Result.AsString
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

function TMicroServiceClient.Send(XML: IXMLDocument): IMicroServiceResponse;
begin
  with Response(XML) do
  begin
    Result := TMicroServiceResponse.New(
      Code,
      TXMLFactory.New('ISO-8859-1', Stream).Document
    );
    // Exceptions... veja mais abaixo
  end;
end;
{% endhighlight text %}

###Consumindo um Serviço {#consumindo-um-servico}



###Tratamento de Exceções {#tratamento-de-excecoes}

{% highlight pascal %}
function TMicroServiceClient.Send(XML: IXMLDocument): IMicroServiceResponse;
begin
  with Response(XML) do
  begin
    Result := TMicroServiceResponse.New(
      Code,
      TXMLFactory.New('ISO-8859-1', Stream).Document
    );
    case Code of
      // BAD REQUEST
      400..499:
        raise EMicroService.Create(
          Result.XML.DocumentElement.ChildNodes['UserMessage'].Text
        );
      // SERVER ERROR
      500..510:
        raise EMicroService.Create(
          Result.XML.DocumentElement.ChildNodes['DevMessage'].Text
        );
    end;
  end;
end;
{% endhighlight text %}


##Conclusão {#conclusao}

O código é real e está em produção. E parece bem simples, não?

WebServices, Multi-camadas, Sistemas distribuídos... tudo isso parece muito complicado. Mas se você souber como as coias funcionam, poderá remover tudo que é **desnecessário** e se concentrar no **essencial**.

A migração está longe de estar concluída. O sistema tem poucos meses, mas apenas poucos dias de trabalho *full time* apenas na codificação e integração dos Microservices.

Bem, por enquanto estamos indo bem.

Até logo.
