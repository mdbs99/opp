---
layout: post
title: "Construindo uma Aplicação Básica com fpWeb"
date: 2018-06-04
permalink: /fpweb-hello
description:
  As vezes, tudo que precisamos é uma aplicação "Alô Mundo".
image: /images/2018/photo-rawpixel-296622-unsplash.jpg
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

As vezes, tudo que precisamos é uma aplicação "Alô Mundo".

<!--more-->

![Unsplash image]({{ page.image }})
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by rawpixel on Unsplash</span>

## Introdução {#introducao}

Neste artigo você verá como preparar seu ambiente Lazarus para o desenvolvimento Web com fpWeb.

Eu estou utilizando a versão mais atual e revisada do Lazarus 1.8.5,  [fixes_1_8](https://svn.freepascal.org/svn/lazarus/branches/fixes_1_8), revisão 58049.

Para o compilador, também estou utilizando a versão mais atual e revisada do FPC 3.0.5, [fixes_3_0](https://svn.freepascal.org/svn/fpc/branches/fixes_3_0), revisão 38970.

O artigo presume que você já tem o FPC e Lazarus instalados e funcionando no seu computador.

## Instalando fpWeb {#instalando}

A primeira instalação a ser realizada será a do pacote `lazwebextra`. Esse pacote adiciona opções para a criação de novos projetos do tipo Web como CGI Application, FastCGI Application e outros.

Esse pacote não é obrigatório, mas facilita a criação de novos projetos. No entanto, se você não quiser instalá-lo, ainda poderá criar uma aplicação console simples e ir alterando manualmente o código conforme for lendo o artigo.

Para prosseguir com a instalação, abra o menu `Package>Install/Uninstall Packages` e localize o pacote `lazwebextra` no box "Available for installation".

Na figura abaixo, o pacote é mostrado no box esquerdo, pois já foi instalado no meu computador.

![Install/Uninstall Packages](/images/2018/laz-install-lazwebextra.jpg)

Clique em "Save e Rebuild IDE".

Se tudo deu certo, você poderá seguir para o próximo passo, ou seja, o início da construção de um sistema Web simples.

## Desenvolvimento {#desenvolvimento}

Após a instalação do pacote de integração com a IDE, você estará habilitado a construir aplicativos Web utilizando o fpWeb framework.

Abra o menu `File>New` e escolha a opção "FastCGI Application".

Apesar de escolhermos essa opção, não iremos criar uma aplicação FastCGI que precisará ser executada em algum servidor Web como Apache ou Microsoft IIS. Ao invés disso, iremos criar um executável Standalone que irá responder as requisições do browser sem a necessidade de existir um servidor.

Após clicar nessa opção, um novo projeto será criado.

Infelizmente, nessa versão do Lazarus eles ainda mantém a versão antiga do fpWeb como padrão, ou seja, aquela baseada no modelo Web Broker. No entanto, queremos utilizar a [nova]({% post_url 2018-05-28-fpweb %}) versão e para isso precisaremos fazer algumas modificações manuais.

Feche a `Unit1` sem salvar e remova-a do projeto (`lpr`). Salve o projeto, escolhendo um nome — eu digitei `hello.lpr`.

    program hello;

    {$mode objfpc}{$H+}

    uses
     fpFCGI;

    begin
      Application.Title := 'Hello World';
      { Uncomment the port setting here if you want to run the
        FastCGI application stand-alone (e.g. for NGINX) }
      // Application.Port:=2015; // For example
      Application.Initialize;
      Application.Run;
    end.

Por enquanto esse é todo o projeto. Ele ainda não faz nada de útil. É necessário criar alguma rota que execute alguma ação.

Como iremos criar uma aplicação Standalone, precisaremos substituir a seção `uses` com outras unidades e também definir a porta de acesso que estará em execução quando acessarmos `localhost`:

    program hello;

    {$mode objfpc}{$H+}

    uses
      SysUtils, httpdefs, httproute, fphttpapp;

    begin
      Application.Title := 'Hello World';
      Application.Port := 8080;
      Application.Initialize;
      Application.Run;
    end.

As duas unidades mais importantes são `httproute`, que define os objetos relacionados às rotas e `fphttpapp`, que substitui o tipo de projeto de FastCGI para Standalone.

Feito isso é necessário definir ao menos uma rota para executar alguma ação.

A maneira mais fácil é implementar uma nova classe, tendo como base a `TRouteObject`, padrão do fpWeb. Essa nova classe deverá sobrescrever o método `HandleRequest` e retornar algo como resposta.

Vamos criar uma nova classe chamada `THelloRoute`, implementando o método `HandleRequest` da forma mais simples possível. Depois, é necessário registrar essa rota em algum lugar, antes da aplicação realmente iniciar — você poderá utilizar as seções `initialization` de cada nova unidade que você criar, porém aqui, para manter simples, basta registrar a rota antes de `Application.Run`.

Abaixo pode-se ver o programa completo:

    program hello;

    {$mode objfpc}{$H+}

    uses
      SysUtils, httpdefs, httproute, fphttpapp;

    type
      THelloRoute = class(TRouteObject)
      public
        procedure HandleRequest(
          ARequest: TRequest; AResponse: TResponse); override;
      end;

    { THelloRoute }

    procedure THelloRoute.HandleRequest(
      ARequest: TRequest; AResponse: TResponse);
    begin
      AResponse.Content := 'Hello!';
      AResponse.SendResponse;
    end;

    begin
      HTTPRouter.RegisterRoute('/hello', rmAll, THelloRoute);
      Application.Title := 'Hello World';
      Application.Port := 8080;
      Application.Initialize;
      Application.Run;
    end.

Agora execute a aplicação diretamente pela IDE.

Depois, abra algum browser se sua preferência e digite: [http://localhost:8080/hello](http://localhost:8080/hello)

Se tudo deu certo, você estará vendo "Hello!" como resposta.

## Conclusão {#conclusao}

Esse artigo mostrou como criar uma aplicação Web simples que pode ser executada e testada sem a necessidade de um servidor Web.

Tenha em mente que "todo esse trabalho" só será feito uma vez. Após criar um projeto de "template", bastará copiar/colar e renomear o projeto. Todo o restante do trabalho será criando novas unidades e rotas relacionadas às suas regras de negócio.

Até a data de publicação desse artigo não havia documentação sobre esse novo estilo de desenvolvimento utilizando o fpWeb framework. Então, talvez esse seja um dos primeiros artigos a explicar essa nova tecnologia.

Espero que você tenha ficado motivado a pesquisar mais a respeito sobre desenvolvimento Web com Free Pascal e Lazarus.

Até logo.