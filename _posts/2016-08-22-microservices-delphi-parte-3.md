---
layout: post
title: Microservices com Delphi — Parte 3
date: 2016-08-22
description: Como implementar uma simples API para fazer a comunicação com Microservices em Java.
summary: Como implementar uma simples API para fazer a comunicação com Microservices em Java.
image: /images/photo-1448932223592-d1fc686e76ea.jpg
categories: 
  - Pascal
tags:
  - microservices
keywords:
  - microservices
  - java
  - delphi
--- 

Podemos utilizar diretamente uma instância de `TMicroServiceClient`, passar um XML como parâmetro e obter a resposta. Mas isso não seria o ideal. Devemos ter Classes de Negócio, com suas próprias Regras e entrada/saída de informações.

Vamos codificar uma Classe de Negócio, que construa seu próprio XML e utilize, internamente, uma instância do *Client*.

<!--more-->

![Imagem]({{ page.image }})

##Introdução {#introducao}

No [artigo anterior]({% post_url 2016-08-15-microservices-delphi-parte-2 %}) eu escrevi sobre o Módulo MicroService e Localização de Serviços.

Nesse artigo você irá aprender a codificar uma Classe de Negócio que irá consumir um Microservice, utilizando o Módulo já apresentado ateriormente.

Também aprenderá como transformar XML em dados tabulares para apresentá-los numa *Grid*, por exemplo.

##Classe TXMLFactory {#classe-txmlfactory}

Uma instância de `TXMLFactory` é utilizada no *Client*, especificamente no método `TMicroServiceClient.Send`, que não foi abordada no artigo anterior. 

É uma Classe simples mas importante, utilizada tanto para montar tanto o XML de envio como o de retorno.

O Delphi 7 não trabalha com *encode* `UTF-8` por padrão mas os Microservices em Java trabalham no formato `UTF-8`. Então o *Client* deve fazer todas as solicitações nesse formato.

O resultado, no entanto, é convertido para `ISO-8859-1` — que é o formato que estou utilizando no Delphi 7 — antes dos dados serem disponibilizados para o resto da aplicação.

A Classe `TXMLFactory` facilita o envio e retorno.

    type
      TXMLFactory = class(TInterfacedObject, IXMLFactory)
      private
        FEncoding: string;
        FStream: IDataStream;
      public
        constructor Create(const Encoding: string; Stream: IDataStream); reintroduce;
        class function New(const Encoding: string; Stream: IDataStream): IXMLFactory; overload;
        class function New(const Encoding: string): IXMLFactory; overload;
        class function New: IXMLFactory; overload;
        function Document: IXMLDocument;
      end;

    { TXMLFactory }

    constructor TXMLFactory.Create(const Encoding: string; Stream: IDataStream);
    begin
      inherited Create;
      FEncoding := Encoding;
      FStream := Stream;
    end;

    class function TXMLFactory.New(const Encoding: string;
      Stream: IDataStream): IXMLFactory;
    begin
      Result := Create(Encoding, Stream);
    end;

    class function TXMLFactory.New(const Encoding: string): IXMLFactory;
    begin
      Result := New(Encoding, TDataStream.New);
    end;

    class function TXMLFactory.New: IXMLFactory;
    begin
      Result := New('UTF-8');
    end;

    function TXMLFactory.Document: IXMLDocument;
    var
      Buf: TMemoryStream;
    begin
      Result := TXMLDocument.Create(nil);
      Buf := TMemoryStream.Create;
      try
        Result.Options := [];
        Result.Active := True;
        Result.Version := '1.0';
        if FStream.Size > 0 then
        begin
          FStream.Save(Buf);
          Result.LoadFromStream(Buf);
        end;
        Result.Encoding := FEncoding;
      finally
        Buf.Free;
      end;
    end;

##Consumindo um Serviço {#consumindo-um-servico}

Nesse projeto eu defini que todo consumo de qualquer Microservice será feito através de Classes de Negócio. Então poderei ter o mesmo número de Classes correspondende ao número de Microservices ou até mais, pois uma Classe de Negócio poderia fazer uso de mais de um Microservice.

Iremos implementar uma Classe de Negócio simples e hipotética, visto que não poderia disponibilizar o código real do sistema quando se trata do negócio da empresa. 

A Classe proposta é a `TFaturasService`.

A instância dessa Classe irá receber um parâmetro do tipo `IDataUId` — uma classe que encapsula um *GUID* — para retornar os dados de uma Fatura.

Segue sua implementação abaixo:

    type
      TFaturasService = class(TInterfacedObject, IMicroServiceAction)
      private
        FUId: IDataUId;
      public
        constructor Create(UId: IDataUId); reintroduce;
        class function New(UId: IDataUId): IMicroServiceAction;
        function Act: IMicroServiceResponse;
      end;

    { TFaturasService }

    constructor TFaturasService.Create(UId: IDataUId);
    begin
      inherited Create;
      FUId := UId;
    end;

    class function TFaturasService.New(
      UId: IDataUId): IMicroServiceAction;
    begin
      Result := Create(UId);
    end;

    function TFaturasService.Act: IMicroServiceResponse;

      function XML: IXMLDocument;
      begin
        Result := TXMLFactory.New.Document;
        with Result.AddChild('Params') do
        begin
          AddChild('uid').Text := FUId.AsString;
          AddChild('active').Text := 'True';
        end;
      end;

    begin
      Result :=
        TMicroServiceClient.New(
          TMicroServiceParams.New(
            'financ:faturas-service'
          )
          .Find
        )
        .Send(XML)
    end;

###O código passo-a-passo {#passo-a-passo}

Vamos entender o código juntos:

  1. `TFaturasService` é uma Classe de Negócio que implementa `IMicroServiceAction` (veja [aqui]({% post_url 2016-08-15-microservices-delphi-parte-2 %}#introducao));
  3. O Objeto `TXMLFactory` é utilizado para gerar o XML de envio;
  4. O Objeto `TMicroServiceParams` localiza o serviço através do seu `name` (único) e traz as informações em forma de parâmetros;
  5. O Objeto `TMicroServiceClient` envia o XML de envio e retorna uma instância de `IMicroServiceResponse`.
  2. O objeto retornado contém um `XML: IXMLDocument` que é a resposta do Microservice codificado em Java;

E é isso.

Uma implementação completa de um serviço.

Se você esperava algo mais complexo, sinto decepcioná-lo, pois a implementação é tão simples quanto isso. :)  
  
###Utilizando a Classe de Negócio {#utilizando-a-classe}
  
Uma vez que a Classe de Negócio foi implementada, poderá ser utilizada em muitas partes do sistema sem haver duplicação de código; não precisará montar o mesmo XML em vários lugares; não precisará passar os mesmos parâmetros para o *Client*, etc.

Então, como utilizar a nova Classe no nosso código?

Aqui está um exemplo:

    procedure TForm1.FillData;
    begin
      TXMLClientDataSetAdapter.New(
        FModule.FaturaClient,
        TFaturasService.New(FUId).Act.XML
      )
      .Adapt;

No Delphi 7 utilizamos `TClientDataSet` para manter os dados tabulares (linhas e colunas) em memória, exibir numa *Grid* ou em qualquer outro *widget*. Mas os dados dos Microservices vem no formato de XML. Esses dados precisam ser convertidos para um formato tabular.

Para isso temos uma outra Classe responsável por fazer essa conversão da forma mais genérica possível. 

Essa Classe é a `TXMLClientDataSetAdapter`. 

Ela recebe 2 parâmetros:
  
  1. Uma instância de um `TClientDataSet`, que no exemplo está em um `TDataModule`;
  2. Uma instância de `IXMLDocument`, que é obtido através da chamada `Act.XML`;
  
Então executamos o método `Adapt` e pronto, temos um XML convertido em dados tabulares.  

Será que o código da Classe `TXMLClientDataSetAdapter` é complexo?

Não. É tão simples quanto a implementação da Classe de Negócio.

Veja a implementação do método `Adapt`:

    function TXMLClientDataSetAdapter.Adapt: IDataAdapter;
    var
      I: Integer;
      Field: TField;
    begin
      Result := Self;
      if not Assigned(FSource) then
        Exit;
      FDest.DisableControls;
      try
        while Assigned(FSource) do
        begin
          FDest.Append;
          for I := 0 to FSource.ChildNodes.Count -1 do
          begin
            with FSource.ChildNodes[I] do
            begin
              Field := FDest.FindField(NodeName);
              if Assigned(Field) and (Text <> '') then
                Field.Value := Text;
            end;
          end;
          FSource := FSource.NextSibling;
        end;
      finally
        FDest.First;
        FDest.EnableControls;
      end;
    end;

Ele adapta o XML para o formato tabular e no fim temos uma instância de `TClientDataSet` com os dados provenientes do XML.

##Será um substituto ao *DataSnap*?

Eu ainda não terminei essa série, porém já recebi alguns e-mails de alguns leitores perguntando se é possível utilizar essa solução como um substituto ao [*Delphi DataSnap*](http://docwiki.embarcadero.com/RADStudio/Seattle/en/Developing_DataSnap_Applications).

Minha resposta é que o *DataSnap* tem muito, muito código implementado com inúmeras facilidades para os desenvolvedores. Então não é um substituto — e mesmo se esse fosse o objetivo, estaríamos muito longe de concluí-lo.

É melhor ou pior?

Depende do tipo do projeto.

Nesse projeto a versão do Delphi é a 7. A empresa não quer mais investir no Delphi e vai substituí-lo, aos poucos, por Java ou mesmo C#. Então não haveria motivos em utilizar *DataSnap*, visto que a linguagem irá mudar no futuro.

##Desvantagens *by design*

É claro que há desvantagens na solução proposta. Sempre há desvantagens.

Uma grande desvantagem é que novas informações não são, necessariamente, visíveis automaticamente quando há alterações nos Microservices.

Por exemplo.

Caso o Microservice acima enviasse uma nova informação chamada "status", ela não iria aparecer automaticamente numa *Grid* do Delphi. Esse campo teria que ser incluso, manualmente, no *ClientDataSet* que fosse receber os dados.

O que eu quero dizer é que a aplicação Delphi não se adapta automaticamente as mudanças dos Microservices. É possível fazer, porém exigiria muito mais tempo de desenvolvimento.

A solução é simples porque estamos utilizando a premissa [*Convention over configuration*](https://en.wikipedia.org/wiki/Convention_over_configuration).

  1. Se há dados, então eles estarão no formato XML;
  2. Os *fields* de um *ClientDataSet* devem ter o mesmo nome dos *fields* que são esperados nos Microservices e vice-versa;
  3. Utilizamos os mesmos formatos para Data, Hora e Números decimais.
  4. Etc.
  
Apesar de não vermos isso como desvantagens, não deixa de ler uma limitação.

##No próximo artigo… {#no-proximo-artigo}

Você acabou de ver uma implementação simples, *"made in home"*, para trabalhar com interoperabilidade entre sistemas distintos utilizando Microservices que nada mais são do que requisições HTTP enviando e recebendo XML.

Finalizaremos essa série no [próximo artigo]({% post_url 2016-08-29-microservices-delphi-parte-final %}).

Até logo.
