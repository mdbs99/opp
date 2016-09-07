---
layout: post
title: Microservices com Delphi — Parte 1
date: 2016-08-08
description: Como implementar uma simples API para fazer a comunicação com Microservices em Java.
summary: Como implementar uma simples API para fazer a comunicação com Microservices em Java.
image: /images/photo-1453230806017-56d81464b6c5.jpg
categories: 
  - Pascal
tags:
  - microservices
keywords:
  - microservices
  - java
  - delphi
--- 

Imagine um grande Sistema ERP codificado em Delphi 7, com Regras de Negócio rigidamente codificadas para serem utilizadas somente numa aplicação Desktop.

Se você tivesse que evoluir esse sistema para proporcionar uma interoperabilidade com outros Sistemas, versões Web ou mesmo simplificar sua manutenção, o que você faria?

Minha resposta, Microservices.

<!--more-->

![Imagem]({{ page.image }})

##Introdução {#introducao}

Sistemas Web muitas vezes são preferíveis do que Sistemas Desktop. Não é necessária instalação, drivers, DLL's... basta apontar seu browser para uma URL e começar a utilizar. Isso é um fato.

Por outro lado Sistemas Desktop ainda são importantes. Proporcionam uma melhor experiência de UI para o usuário — mas UI Web estão melhorando a cada dia — com uma melhor performance e melhor integração com o Sistema Operacional.

Não esqueçamos, também, das versões Mobile.

A chave é criarmos uma API para disponibilizarmos as Regras de Negócio num único lugar, para que sejam utilizadas em qualquer versão do sistema ou mesmo qualquer dispositivo.

Esse é o problema apresentado em um dos Projetos que estou envolvido.

Um grande Projeto ERP que deverá ter muitas de suas funcionalidades exportadas, recodificadas e disponibilizadas para uso, seja pelo mesmo Sistema ERP, Web, Mobile ou mesmo outros Sistemas.

##Microservices {#microservices}

O que são Microservices?

Na [Wikipedia](https://en.wikipedia.org/wiki/Microservices) temos a seguinte definição:

>Microservices são uma interpretação mais concreta e moderna de arquiteturas orientadas a serviços (SOA) usada para construir sistemas de software distribuídos.

A sigla SOA faz-nos lembrar logo de WebServices e o protocolo SOAP. Mas não estou me referindo a esta tecnologia.

Microservices, na minha opinião, deve ser algo mais simples de construir e utilizar. Devem utilizar o padrão REST. Devem ser fácies de serem substituídos e reescritos em qualquer linguagem que possa interagir com requisições HTTP.

##O Projeto {#o-projeto}

O ERP foi codificado em Delphi 7 e os Microservices — por determinação da empresa — será codificado em Java.

O protocolo de comunicação é HTTP e REST.

Os dados serão transportados utilizando XML, mas houveram argumentos em utilizar JSON. Bem, o Delphi 7 não tem uma *Lib* padrão de JSON, mas tem uma *Lib* padrão para interagir com XML, utilizando [Interfaces]({% post_url 2016-01-18-interfaces-em-todo-lugar %}), que funciona muito bem.

Não irei comentar sobre o código em Java, pois é irrelevante. Esses artigos irão demonstrar como codificar a parte *Client* e não a *Server*.

Também não irei entrar em detalhes de cada Microservice, pois trata-se de código privado.

Mas vou lhe apresentar a infra estrutura, a parte genérica do código que pode ser utilizado por qualquer outro software *Object Pascal*. Qualquer outro detalhe mais específico desse projeto será omitido, por questões óbvias.

Então, a perguta é:

Como codificar uma API simples e Orientada a Objetos, para o compilador do Delphi 7, que possa ser utilizada em qualquer outro projeto Object Pascal?

Vamos aos módulos.

##Módulo Web

Um sistema precisa ser [modularizado]({% post_url 2016-07-18-nomeando-unidades %}) o suficiente para haver o reaproveitamento de código em outros sistemas ou em outros módulos.

Esse Módulo tem 2 *units* de Classes que são relevantes para essa implementação.

###Unit AcmeWebA.pas {#unit-weba}

Essa *unit* encasula tudo que é genérico sobre Web.

A Classe mais relevante é a `TWebURL`, que será utilizada na solução.

    type
      TWebURL = class(TInterfacedObject, IWebURL)
      private
        FServer: string;
        FPathInfo: string;
        FPort: Integer;
      public
        constructor Create(const Server, PathInfo: string; 
          Port: Integer); reintroduce;
        class function New(const Server, PathInfo: string; 
          Port: Integer): IWebURL;
        function Server: string;
        function PathInfo: string;
        function Port: Integer;
        function AsString: string;
      end;

    implementation

    { TWebURL }

    constructor TWebURL.Create(const Server, PathInfo: string; 
      Port: Integer);
    begin
      inherited Create;
      FServer := Server;
      FPathInfo := PathInfo;
      FPort := Port;
    end;

    class function TWebURL.New(const Server, PathInfo: string;
      Port: Integer): IWebURL;
    begin
      Result := Create(Server, PathInfo, Port);
    end;

    function TWebURL.Server: string;
    begin
      Result := FServer;
    end;

    function TWebURL.PathInfo: string;
    begin
      Result := FPathInfo;
    end;

    function TWebURL.Port: Integer;
    begin
      Result := FPort;
    end;

    function TWebURL.AsString: string;
    begin
      Result := Format(
        'http://%s:%d%s', [
          FServer, FPort, FPathInfo
        ]
      );
    end;

###Unit AcmeWebHttpA.pas {#unit-webhttpa}

Essa *unit* encapsula o protocolo HTTP.

A Classe mais relevante é a `THttpClient`. Essa Classe ainda deve ser refatorada para utilizar instâncias de `TWebURL` ao invés de URL's do tipo `string` — código real nunca é perfeito.

Internamente é utilizada o *framework* [Synapse](http://synapse.ararat.cz/doku.php/download) que implementa o protocolo HTTP. Quem já conhece esse *framework* não terá dificuldade para entender o código. Se ainda não conhece, sugiro baixar seus fontes.

    type
      THttpResponse = class(TInterfacedObject, IHttpResponse)
      private
        FCode: Integer;
        FStream: IDataStream;
      public
        constructor Create(Code: Integer; 
          Stream: IDataStream); reintroduce;
        class function New(Code: Integer; 
          Stream: IDataStream): IHttpResponse; overload;
        function Code: Integer;
        function Stream: IDataStream;
      end;

      EHttpClient = class(Exception);

      THttpClient = class(TInterfacedObject, IHttpClient)
      private
        FURL: string;
        FMimeType: string;
        FStream: TStringStream;
        function Send(const Method: string): IHttpResponse;
      public
        constructor Create(const URL, MimeType: string; 
          Stream: IDataStream); reintroduce;
        class function New(const URL, MimeType: string; 
          Stream: IDataStream): IHttpClient; overload;
        class function New(const URL, MimeType: string; 
          Stream: TStream): IHttpClient; overload;
        class function New(const URL, MimeType: string): IHttpClient; overload;
        destructor Destroy; override;
        function Execute(const Verb: string): IHttpResponse;
        function Get: IHttpResponse;
        function Post: IHttpResponse;
      end;

    implementation

    uses
      // synapse
      httpsend, synacode, synautil, ssl_openssl;

    { THttpResponse }

    constructor THttpResponse.Create(Code: Integer; 
      Stream: IDataStream);
    begin
      inherited Create;
      FCode := Code;
      FStream := Stream;
    end;

    class function THttpResponse.New(Code: Integer;
      Stream: IDataStream): IHttpResponse;
    begin
      Result := Create(Code, Stream);
    end;

    function THttpResponse.Code: Integer;
    begin
      Result := FCode;
    end;

    function THttpResponse.Stream: IDataStream;
    begin
      Result := FStream;
    end;

    { THttpClient }

    function THttpClient.Send(const Method: string): IHttpResponse;
    var
      URL: string;
    begin
      URL := FURL;
      with THTTPSend.Create do
      try
        try
          MimeType := FMimeType;
          if Method = 'GET' then
            URL := URL + FStream.DataString
          else
            WriteStrToStream(Document, FStream.DataString);
          if not HTTPMethod(Method, URL) then
            raise Exception.Create(Sock.LastErrorDesc);
          Document.Position := soFromBeginning;
        except
          on E: Exception do
            raise EHttpClient.Create(
              E.Message +
              #13'Method: ' + Method +
              #13'Code: ' + IntToStr(ResultCode)
            );
        end;
      finally
        Result := THttpResponse.New(
          ResultCode, 
          TDataStream.New(Document)
        );
        Free;
      end;
    end;

    constructor THttpClient.Create(const URL, MimeType: string; 
      Stream: IDataStream);
    begin
      inherited Create;
      FURL := URL;
      FMimeType := MimeType;
      FStream := TStringStream.Create('');
      Stream.Save(FStream);
    end;

    class function THttpClient.New(const URL, MimeType: string; 
      Stream: IDataStream): IHttpClient;
    begin
      Result := Create(URL, MimeType, Stream);
    end;

    class function THttpClient.New(const URL, MimeType: string; 
      Stream: TStream): IHttpClient;
    begin
      Result := New(URL, MimeType, TDataStream.New(Stream));
    end;

    class function THttpClient.New(const URL, MimeType: string): IHttpClient;
    begin
      Result := New(URL, MimeType, TDataStream.New);
    end;

    destructor THttpClient.Destroy;
    begin
      FStream.Free;
      inherited;
    end;

    function THttpClient.Execute(const Verb: string): IHttpResponse;
    begin
      Result := Send(Verb);
    end;

    function THttpClient.Get: IHttpResponse;
    begin
      Result := Execute('GET');
    end;

    function THttpClient.Post: IHttpResponse;
    begin
      if FStream.Size = 0 then
        raise EHttpClient.Create('HTTP.Send: No data.');
      Result := Execute('POST');
    end;

O mais importante na Classe `THttpClient` é seu método `Send`, mas não tem nada complicado.

Veja essa parte:

    if Method = 'GET' then
      URL := URL + FStream.DataString
    else
      WriteStrToStream(Document, FStream.DataString);

Caso o VERBO seja GET, então `FStream` irá conter um PATH_INFO para complementar a URL. Do contrário, `FStream` irá conter um BODY, ou seja, um XML de requisição com os parâmetros necessários para o Microservice.

##No próximo artigo… {#no-proximo-artigo}

Até aqui nada de mais. Você viu apenas algumas Classes simples que encapsulam [Entidades]({% post_url 2016-02-29-objetos-representam-entidades %}) reais, utilizando Orientação a Objetos.

No [próximo artigo]({% post_url 2016-08-15-microservices-delphi-parte-2 %}) escrevo sobre o módulo `MicroService` que contém todas as Classes que fazem a comunicação com os Microservices codificados em Java.

Até logo.
