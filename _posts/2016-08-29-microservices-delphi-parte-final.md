---
layout: post
title: Microservices com Delphi — Parte Final
date: 2016-08-29
description: Como implementar uma simples API para fazer a comunicação com Microservices em Java.
summary: Como implementar uma simples API para fazer a comunicação com Microservices em Java.
image: /images/photo-1454023989775-79520f04322c.jpg
categories: 
  - Pascal
tags:
  - microservices
keywords:
  - microservices
  - java
  - delphi
--- 

Uma arquitetura que utiliza Microservices tem seus prós e contras como qualquer outra tecnologia. Será que essa arquitetura foi uma boa escolha para o projeto?

<!--more-->

![Imagem]({{ page.image }})

##Introdução {#introducao}

No [artigo anterior]({% post_url 2016-08-22-microservices-delphi-parte-3 %}) eu lhe mostrei como codificar uma Classe de Negócio para fazer a comunicação com um Microservice utilizando todo o arcabouço apresentado na primeira parte dessa série.

Nesse artigo você irá ver:

  1. **Tratamento de Exceções**: Quando algo dá errado nos Microservices ou na comunicação com eles;
  2. **Links**: Separei alguns artigos interessantes sobre o assunto;

##Tratamento de Exceções {#tratamento-de-excecoes}

Todo *software* precisa tratar erros e exceções, e é claro que estamos fazendo isso.

Na Unit [AcmeMicroServiceA.pas]({% post_url 2016-08-15-microservices-delphi-parte-2 %}#unit-microservicesa) o tratamento de exceções é feito no método `TMicroServiceClient.Send`, verificando o `Code` de retorno do protocolo HTTP.

Se houver erro, definimos um protocolo privado que descreve como a mensagem para o *Client* deve ser. Nada mais é do que um XML com *nodes* específicos que descrevem o erro ou exceção.

Abaixo o código modificado que trata erros e exceções. O código em produção é um pouco mais elaborado, no entanto acredito que com esse exemplo você irá entender como o tratamento ocorre no projeto real.

{% highlight pascal %}
function TMicroServiceClient.Send(
  const Content: string): IMicroServiceResponse;
begin
  with Response(Content) do
  begin
    Result := TMicroServiceResponse.New(
      Code,
      TXMLFactory.New('ISO-8859-1', Stream).Document
    );
    case Code of
      // BAD REQUEST
      400..499:
        raise EMicroService.Create(
          Result
            .XML
            .DocumentElement
            .ChildNodes['UserMessage'].Text
        );
      // SERVER ERROR
      500..510:
        raise EMicroService.Create(
          Result
            .XML
            .DocumentElement
            .ChildNodes['DevMessage'].Text
        );
    end;
  end;
end;
{% endhighlight text %}

Repare que para cada tipo de erro/exceção, um *node* (ou *nodes*) específico é utilizado para obter a informação.

No *Client* Delphi, o `Code` também é acessível através da Interface `IMicroServiceResponse`, então o *Client* pode tomar decisões específicas para casos que não gerem uma exceção.

Veja [aqui](http://www.restapitutorial.com/httpstatuscodes.html) uma lista de *HTTP Status Codes*.

##Links {#links}

Separei alguns links interessantes de artigos e vídeos que serviram de inspiração para escrever essa série, assim como foram fontes de informações para o projeto.

* [Vantagens e desvantagens de uma arquitetura microservices: uma abordagem rica em exemplos](https://www.infoq.com/br/presentations/vantagens-e-desvantagens-de-uma-arquitetura-microservices) é uma apresentação em vídeo de quase 1 hora que fala sobre os prós e contras em utilizar essa arquitetura. O autor fala sobre [Strangler Application](http://www.martinfowler.com/bliki/StranglerApplication.html) que é o mesmo princípio que estamos utilizando no projeto.

* [Sete antipadrões para microservices](https://www.infoq.com/br/articles/seven-uservices-antipatterns) alerta sobre algumas coisas que devem ser evitadas, na opinião do autor, quando utilizamos a arquitetura de Microservices.

* [Preventing Tight Data Coupling Between Microservices](https://medium.com/@mwhitt.w/preventing-tight-data-coupling-between-microservices-df30e1e24311#.wl6r7hcfz) é um artigo em Inglês interessante sobre acoplamento de dados.

* [Microservices: Real Architectural Patterns](https://medium.com/@skamille/microservices-real-architectural-patterns-68bd83bbb6cd#.hrsdvq3rj) é outro artigo em Inglês que fala sobre arquitetura e CRUD.

##Conclusão {#conclusao}

A escolha pela arquitetura de Microservices está sendo satisfatória.

Microservices são como [instâncias de Objetos]({% post_url 2016-03-14-objetos-pensam-e-tomam-decisoes %}) pela rede.

Assim como Objetos, cada Microservice deve implementar apenas [uma única responsabilidade]({% post_url 2016-03-07-classes-devem-implementar-apenas-uma-responsabilidade %}).

Quando falamos sobre Microservices, WebServices, Multi-camadas, protocolos... tudo isso parece muito complicado. Mas se você souber como as coisas funcionam, poderá remover tudo que é **desnecessário** para o **seu negócio** e se concentrar apenas no **essencial**.

Você não precisa implementar **tudo** o que dizem para implementar. Utilize tecnologias que fazem sentido para seu negócio e comece simples.

A migração está longe de estar concluída. O sistema tem poucos meses, mas apenas poucos dias de trabalho *full time* na codificação e integração dos Microservices.

Por enquanto estamos indo bem.

Até logo.
