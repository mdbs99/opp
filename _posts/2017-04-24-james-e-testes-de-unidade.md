---
layout: post
title: James e Testes de Unidade
date: 2017-04-24
description:
  James é um pacote de Classes e Interfaces para projetos Orientados a Objetos.
image: /images/photo-clark-young-135435.jpg
categories: 
  - James
tags:
  - james
  - package
  - open-source
keywords:
  - freepascal
  - fpc
  - delphi
  - lazarus
  - c#
  - csharp
  - java
  - object-oriented
  - oop
  - mdbs99
  - james
  - james-package
  - james-lib
  - open-source
  - github
---

James foi atualizado com Testes de Unidade.

Veja nesse artigo como tudo foi configurado e como você pode adicionar seus próprios testes apenas alterando um arquivo XML.

<!--more-->

![Unsplash image]({{ page.image }})  

## Introdução {#introducao}

A utilização de testes automatizados é obrigatório em qualquer projeto.

Você pode criar os testes utilizando pequenos programas que serão compilados e executados no *console*. Dependendo do código de resultado ao terminar, você saberá se o programa teve ou não sucesso.

Alguns *frameworks* facilitam esse processo. Chamados de [xUnit](https://en.wikipedia.org/wiki/XUnit), eles dão aos desenvolvedores todo o suporte para criar todo tipo de teste no código. Basta você herdar de alguma classe padrão e criar métodos de teste.

No entanto, não importa se esses testes utilizam algum *framework* específico, nem se foram criados de acordo com o processo de [*Test-driven development* (TDD)](https://en.wikipedia.org/wiki/Test-driven_development).

O importante é ter os testes.

## Pacote de Testes {#pacote-de-testes}

No [James](https://github.com/mdbs99/james) eu utilizei um *framework* chamado [FPCUnit](http://wiki.freepascal.org/fpcunit). 

É um *framework* xUnit padrão do FPC/Lazarus muito parecido com o [DUnit](http://dunit.sourceforge.net/) do Delphi.

Basta clicar em `File->New->FPCUnit Test Application`, digitar o nome da Classe de Teste, clicar em "criar unit" e pronto. Depois basta ir criando os métodos com os testes.

No James a configuração é um pouco mais sofisticada e é importante que você saiba como tudo funciona.

O James tem 2 pacotes (atualmente):

1. james.lpk
2. james.tests.lpk

Ambos estão no diretório `/pkg`.

O pacote `james.lpk` contém as Classes principais e o `james.tests.lpk` apenas as Classes de Testes.

Esse último tem a dependência com o primeiro. 

![james.tests.lpk]( /images/photo-pkg-james.tests-01001.jpg) 

Para o usuário do James, apenas o pacote `james.lpk` é necessário.

Então por quê criar um outro pacote apenas de testes?

O motivo é que eu posso ter tipos de aplicações de testes diferentes (desktop ou console) e basta atualizar os fontes do pacote para que qualquer aplicação fique atualizada.

Essa técnica eu aprendi com meu amigo [João Morais](https://github.com/jcmoraisjr) quando comecei a acompanhar um de seus projetos chamado [JCore](https://github.com/jcmoraisjr/jcore).

Nos fontes do James, há o projeto [james.tests.gui](https://github.com/mdbs99/james/blob/master/test/james.tests.gui.lpr) que utiliza o pacote de testes.

## Teste com FPCUnit {#fpcunit}

O projeto `james.tests.gui` utiliza a interface do FPCUnit.

Ao executar o projeto a tela abaixo será exibida. Basta então executar todos os testes (F9) e verificar se houve algum erro.

![FPCUnit loading]( /images/photo-fpcunit-loading-552.jpg) 

Se você baixar, compilar e executar esse projeto, ele irá exibir alguns erros dizendo que o arquivo `james.tests.template.xml` não foi encontrado.

O que você deve fazer é copiar esse arquivo da pasta `/test` para a pasta `/bin` e modificar seu conteúdo com seus próprios arquivos e configurações.

Veja um exemplo de configuração para alguns dos meus arquivos de teste:

    <?xml version="1.0" encoding="utf-8"?>
    <Tests>
      <TStreamDividedTest>
        <files>
          <file-1 filename="tests/04DF86C9.PDF" part="3" />
        </files>
      </TStreamDividedTest>
      <TStreamPartialFromTextTest>
        <files>
          <file-1 filename="tests/04DF86C9.PDF" text="encoding=" />
        </files>
      </TStreamPartialFromTextTest>
    </Tests>

Veja que de acordo com o path dos arquivos no XML, meus arquivos ficam em `/bin/tests/`. Pois `/bin/` é a pasta padrão onde o executável será gerado.

São arquivos pessoais e/ou privados de projetos reais. Esses arquivos não podem subir para o Github. Por isso meus testes utilizam um XML configurável.

Então, se você quiser utilizar os Testes de Unidade do James, cabe a você configurar sua versão do XML.

Se tudo der certo, você verá a tela abaixo.

![FPCUnit green]( /images/photo-fpcunit-green-552.jpg ) 

## Como ajudar {#ajudando}

Se você quiser ajudar esse projeto — ou algum outro dos [meus projetos](https://github.com/mdbs99?tab=repositories) — siga esses passos:

1. Faça um fork do projeto;
2. Utilize o branch `develop` para fazer qualquer modificação;
3. Faça um *Pull Request* utilizando `develop`.

Eu irei fazer a revisão das mudanças e se tudo tiver de acordo, sua contribuição será mesclada no `develop`.

O branch `master` só irá conter a versão de produção, 100% compilada, com todos os Testes de Unidade funcionando. Por isso qualquer contribuição deverá ser enviada para o `develop`.

## Conclusão {#conclusao}

James é um projeto que está no começo. Estou refatorando meus códigos privados para criar o James (e outros projetos, futuramente).

Tudo Open Source.

Creio que todos esses projetos poderão ser compilados no Delphi, porém adaptações devem ser feitas.

Se você tem interesse no projeto, preciso de ajuda para criar essas adaptações.

Aguardo seus comentários.

Até logo.