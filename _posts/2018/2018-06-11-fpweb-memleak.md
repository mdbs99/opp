---
layout: post
title: "fpWeb: Verificando Vazamentos de Memória"
date: 2018-06-11
permalink: /fpweb-memleak
description:
  Veja nesse artigo como visualizar o relatório de vazamentos de memória de uma aplicação Web.
image: /images/2018/photo-steve-johnson-548294-unsplash.jpg
tags:
  - fpWeb
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
  - fpweb
  - web
  - web development
---

A linguagem Object Pascal, assim como C ou C++, não possui *Garbage Collector* para liberar os objetos e recursos da memória automaticamente. Pode ser difícil ter 100% de certeza que todos os objetos estão sendo liberados (manualmente) pelo código. Mas, se algum objeto não for liberado, ocorrerá um vazamento de memória.

<!--more-->

![Unsplash image]({{ page.image }})
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Steve Johnson on Unsplash</span>

Quando utilizamos a linguagem Object Pascal precisamos nos preocupar com [vazamentos de memória]({% post_url 2016-10-10-interfaces-delegacao-problemas-solucoes %}#vazamentos-de-memoria), até mesmo nos sistemas mais simples.

Vazamentos de memória podem ser ainda mais prejudiciais em sistemas Web comparado com sistemas Desktop. Se uma aplicação Desktop tem vazamentos de memória, em algum momento o usuário poderá fechar a aplicação e todo o vazamento será recuperado pelo Sistema Operacional. Mas em aplicações Web, onde podem haver múltiplas requisições por segundo enquanto o servidor mantém em memória cópias do executável (FastCGI), tais vazamentos poderiam diminuir drasticamente com os recursos do servidor.

No [artigo anterior]({% post_url 2018-06-04-fpweb-hello %}) fizemos um programa simples que responde "Hello!" quando uma determinada URL  acessada.

Como não há vazamentos de memória nesse programa, teremos que simulá-lo, alterando o método `HandleRequest` conforme abaixo:

    procedure THelloRoute.HandleRequest(
      ARequest: TRequest; AResponse: TResponse);
    begin
      TStringList.Create;
      AResponse.Content := 'Hello!';
      AResponse.SendResponse;
    end;

Mesmo após compilar o programa com o [HeapTrc](https://www.freepascal.org/docs-html/rtl/heaptrc/usage.html) habilitado, iniciá-lo e executá-lo utilizando o browser, não será possível ver o vazamento — diferentemente de aplicações Desktop que, ao término do programa, mostra um "relatório" sobre a memória *utilizada vs. liberada*.

Para ser possível ver o relatório, você tem que finalizar a aplicação apropriadamente. O problema é que, mesmo após finalizada a requisição pelo browser, ela continua em execução aguardando mais e mais requisições.

Felizmente, há uma maneira de ver o relatório do HeapTrc, alterando o código conforme a seguir:

<script src="https://gist.github.com/mdbs99/90b319e7bcf5d37383c1083179b69546.js"></script>

No código acima, foi introduzido um procedimento `TerminateCall`, algumas configurações do `heaptrc` e a inclusão de uma nova rota `/quit`.

Compile e execute a aplicação novamente. Depois, faça uma requisição a URL `/hello`.

Então, aponte seu browser para fazer uma requisição a URL `/quit`. Nesse momento você verá que a janela da aplicação foi finalizada.

Finalmente, abra o arquivo `log.txt`, localizado no mesmo diretório do executável, para visualizar todos os vazamentos de memória.

Até logo.