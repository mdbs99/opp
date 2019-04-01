---
layout: post
title: "Trabalhando com Exceções em Requisições HTTP"
date: 2019-04-01
permalink: /:title
description:
  Em requisições HTTP, devemos levantar uma exceção quando recebermos, por exemplo, um código 500?
image: /images/2019/nathan-dumlao-744702-unsplash.jpg
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
  - http
  - exception
---

Em requisições HTTP, devemos levantar uma exceção quando recebermos, por exemplo, um código 500?

<!--more-->

![Unsplash image]({{ page.image }})
<br><span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Nathan Dumlao on Unsplash</span>

Tivemos uma discussão em um grupo do [Telegram](https://telegram.org/) sobre utilizar ou não exceções em requisições HTTP, o que me inspirou a escrever esse artigo.

Como todos sabemos, o [protocolo HTTP](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol) é baseado em texto simples.

Cada requisição do cliente é composto por um cabeçalho e um corpo (opcional):

    GET /index.html HTTP/1.1
    Host: www.example.com

A resposta do servidor segue o mesmo protocolo, contendo um cabeçalho e um corpo (também opcional) de resposta:

    HTTP/1.1 200 OK
    Date: Mon, 23 May 2005 22:38:34 GMT
    Content-Type: text/html; charset=UTF-8
    Content-Length: 138
    Last-Modified: Wed, 08 Jan 2003 23:11:55 GMT
    Server: Apache/1.3.3.7 (Unix) (Red-Hat/Linux)
    ETag: "3f80f-1b6-3e1cb03b"
    Accept-Ranges: bytes
    Connection: close

    <html>
    <head>
      <title>An Example Page</title>
    </head>
    <body>
      Hello World, this is a very simple HTML document.
    </body>
    </html>

A beleza desse protocolo é que mesmo seres humanos, não apenas máquinas, podem ler entender (a maioria do) seu conteúdo.

Então podemos perceber que há um padrão em ambas as mensagens acima, identificado na primeira linha, indicando o sucesso ou erro da requisição ou resposta.

O padrão é determinado por um par de *código* e *texto* no qual identificaremos por *status*.

No exemplo acima, o status é determinado pelo código 200 e o texto "OK". Mostrando uma resposta padrão de *sucesso*.

Há uma [grande lista](https://en.wikipedia.org/wiki/List_of_HTTP_status_codes) de códigos pré-determinados e toda aplicação deveria respeitá-los em todas as  requisições e respostas.

Agora, repare que não existem objetos e tão pouco *objetos de exceções*.

No início do artigo eu disse que a dúvida que gerou a discussão foi utilizar ou não exceções, ou seja, objetos `Exception` e derivados em clientes HTTP.

Minha resposta?

*Você não deveria utilizar exceções em clientes HTTP, somente nas suas classes de negócio.*

Se o protocolo não tem exceções, por quê implementá-lo na classe mais básica, ou seja, a que faz a ponte entre o seu sistema e a requisição HTTP?

Para mim, não faz sentido.

É verdade que existem vários frameworks que implementam clientes HTTP como, por exemplo, [Indy](https://www.indyproject.org/), [Synapse](http://www.ararat.cz/synapse/doku.php/start) ou até mesmo os clientes padrão da linguagem.

Talvez esses clientes podem gerar uma exceção quando recebem um código 500 ou talvez não. Não importa. Você deveria encapsular essas classes de terceiros em suas próprias classes de acesso HTTP. Assim, você poderia mudar de framework apenas alterando essas classes.

Pense apenas em um cliente HTTP genérico, cru, que implementa o protocolo como ele deve ser: envio e recebimento de mensagens no formato texto. A partir dele você constrói classes de negócio que utilizam o cliente, adicionando comportamento referente as regras de negócio.

Na [parte final]({% post_url 2016-08-29-microservices-delphi-parte-final %}) da série "Microservices com Delphi" você poderá ver mais um exemplo de como fazer o tratamento de códigos de retorno do cliente HTTP. Esse é apenas mais um exemplo e não um padrão. Tudo irá depender da sua aplicação, quais códigos verificar e o que fazer quando recebê-los.

Apenas mantenha os clientes o mais simples possível.

Até logo.



