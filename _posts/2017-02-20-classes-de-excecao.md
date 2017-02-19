---
layout: post
title: Classes de Exceção
date: 2017-02-20
description:
  Devemos codificar uma Classe para cada tipo de Exceção ou utilizar apenas um único tipo?
image: /images/photo-hans-eiskonen-136904.jpg
categories: 
  - Linguagem
tags:
  - exceptions
keywords:
  - exception
  - exceções
  - raise exception
  - delphi
  - freepascal
  - lazarus
---

Devemos codificar uma Classe para cada tipo de Exceção ou utilizar apenas um único tipo?

<!--more-->

![Unsplash image]({{ page.image }})  

## Introdução {#introducao}

O conceito de Exceção foi criado para simplificar o código, removendo condicionais e verificações, para que o programador fique focado na lógica que o programa deve fazer ao invés de se preocupar em testar cada linha do código.

A idéia principal é fazer com que todos os possíveis erros (exceções) sejam concentrados e verificados num ponto em comum.

Esse ponto em comum pode ser único para todo o sistema, mas também pode ser um ponto em comum por Módulo, por *Unit*, por Classe. Depende. Mas isso é um assunto para outro artigo.

Nesse artigo o foco é outro. Vamos ter uma conversa se devemos criar ou não novos tipos de exceção para cada tipo de erro ou problema.

## Classes de Exceção {#classes-de-excecao}

Divisão por zero. Erro na leitura de um *stream*. Erro de banco de dados...

Temos Classes de Exceção definida para cada um desses erros. Em qualquer linguagem é assim.

Por quê?

Não é para tomada de decisões, com certeza. Você não deve mudar o fluxo de um programa de acordo com a exceção gerada. Todas as exceções tem um único objetivo: parar o fluxo normal do programa.

Para o usuário não importa o Tipo da Classe, mas sim a **mensagem** que é exibida quando um erro ocorre.

Uma exceção define um ponto de ruptura no fluxo do código. A execução normal é interrompida e o fluxo vai para o ponto de tratamento das exceções.

Então não haveria necessidade de criar novos tipos de Classes.
Basta informar ao usuário o que ocorreu. A mensagem.

No entanto... eu vejo um motivo para criar novos Tipos de Exceções: [Reaproveitamento]({% post_url 2017-01-09-codigo-duplicado-talvez-nao %}) de código.

Por exemplo, se um arquivo não é encontrado poderíamos gerar uma exceção do tipo `Exception` — a exceção mais genérica — ou criar uma nova Classe `EFileNotFound`.

Para dar a informação ao usuário, as duas exceções abaixo são equivalentes:

    raise Exception.CreateFmt('File %s not found', ['foo.txt']);
    
    raise EFileNotFound.Create('foo.txt');

A diferença é que `EFileNotFound` já encapsula a mensagem *"File %s not found"* e não precisamos repetir a mesma mensagem toda vez que quisermos dizer ao usuário que um arquivo não foi encontrado.

Assim fica fácil mudar a mensagem num único ponto no código.

## Conclusão {#conclusao}

Não utilize Tipos de Classes de Exceção para controlar o fluxo do programa. Isso é errado.

Toda Exceção *deve* definir um ponto de ruptura. Algo de muito errado aconteceu. Para tudo. Não importa o Tipo de Classe.

Para o usuário apenas a mensagem importa. Basta ser bem explicativa.

Devemos criar novas Classe de Exceção apenas para reaproveitar e encapsular informações.

Até logo.