---
layout: post
title: Microservices com Delphi — Parte 3
date: 2016-08-22
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
