---
layout: post
title: Scripts para Compilar FreePascal e Lazarus no Windows
date: 2017-02-06
description:
  Saiba como compilar uma nova versão do compilador FreePascal ou atualizar a IDE Lazarus utilizando diretamente o código fonte.
image: /images/photo-v0zxmzw_-e0-john-mark-kuznietsov.jpg
categories: 
  - Scripts
tags:
  - script
  - freepascal
  - lazarus
keywords:
  - scripts
  - compilar
  - compiler
  - freepascal
  - lazarus
---

Saiba como compilar uma nova versão do compilador FreePascal ou atualizar a IDE Lazarus utilizando diretamente o código fonte.

<!--more-->

![Unsplash image]({{ page.image }})  

## Introdução {#introducao}

A maneira mais fácil de começar a utilizar o FreePascal e Lazarus não é tentando compilar o compilador ou a IDE utilizando Scripts.

Hoje em dia temos algumas opções de instalação:

  1. [FPC Oficial](http://freepascal.org/download.var) ➝ apenas o compilador
  2. [Lazarus Oficial](http://www.lazarus-ide.org/) ➝ FPC + IDE
  3. [GetLazarus](https://www.getlazarus.org/setup/?download) ➝ FPC+Lazarus (versão modificada)
  4. [NewPascal](http://newpascal.org/download.html) ➝ FPC+Lazarus (versão modificada)
  5. [CodeTyphon](http://www.pilotlogic.com/) ➝ versão ultra-modificada
  
Caso você queira se aventurar em outra IDE que não é compatível com o Delphi mas é multiplataforma — todos os *widgets* tem a mesma aparência em qualque Sistema Operacional — você pode tentar a IDE [MSE+GUI](http://www.msegui.com/).

Se você só "ouviu falar" de Lazarus e não sabia que existiam tantas opções para programar em *Object Pascal* bem, deve ter sido uma boa surpresa, não?

## Por que utilizar Scripts {#porque-utilizar-scripts}

Se existem tantas opções de instaladores automatizados, por quê precisamos de scripts para compilação manual?

Há aqueles programadores que querem ter o controle total de seu compilador e IDE.

Há aqueles que modificam o compilador, FCL ou RTL.

Há aqueles que modificam a IDE, a LCL, os componentes e as ferramentas.

Há aqueles que ajudam a comunidade FreePascal. Baixam as últimas versões do código fonte, compilam, testam e mandam *feedback* através do [Mantis](http://bugs.freepascal.org/main_page.php).

Se você é um desses, você **precisa** saber como compilar e instalar o FreePascal e Lazarus, utilizando diretamente o código fonte.

## Compilar utilizando Scripts {#compilar-com-scripts}

Eu nunca executei nenhum instalador do FreePascal ou Lazarus. Na época que comecei a utilizar tais ferramentas, os instaladores não eram tão bons. Além disso eu queria me manter atualizado com os fontes, pois melhorias eram (ainda são) feitas diariamente.

Você sabia que o compilador FreePascal é compilado em FreePascal? Isso mesmo! Você precisa ter a última versão do compilador estável para poder compilar uma nova versão do próprio compilador. Isso é chamdo de [Self-hosting](https://en.wikipedia.org/wiki/Self-hosting).

A única maneira de estar sempre atualizado é acompanhar as mudanças dos fontes (FreePascal e Lazarus) através do SVN ou *feed*, atualizar seus fontes local e compilar ambos no seu computador.

Então eu precisava ter um script mínimo para fazer essa tarefa. Na época eu utilizava Windows XP (ainda utilizo em alguns lugares), então um Script *Batch* seria simples e eficaz.

Eu utilizava a versão 2."alguma coisa" do compilador FreePascal. Fiz o script e, depois de algum tempo de uso, publiquei-o para a comunidade [aqui](http://wiki.freepascal.org/Installing_Lazarus#Compiling.2Finstalling_FPC_and_Lazarus_from_Sources_of_SVN_.28Win32.29).

Dias atrás eu descobri que o script vem sendo atualizado pela comunidade — OpenSource é legal, não? — e isso me inspirou escrever esse artigo.

## Aprimoramentos {#aprimoramentos}

Fui utilizando esse script inicial por bastante tempo, mas como eu fazia testes com novas funcionailidades do FPC e Lazarus, precisei ir incrementando o script — hoje tenho alguns scripts — conforme minhas necessidades pessoais.

Hoje em dia tudo está mais fácil. Se você quiser instalar FPC e Lazarus, utilizando o fontes e sem querer aprender sobre esse processo, poderá utilizar o projeto [fpCup](http://wiki.lazarus.freepascal.org/fpcup). Tem até um aprimoramento desse projeto chamado [fpCup Deluxe](https://github.com/newpascal/fpcupdeluxe). 

Bem, eu não tinha nada disso na época, então fui aprimorando meus scripts que alias, funcionam muito bem até hoje, obrigado.

Ontem eu transformei esses scripts num projeto no GitHub para que a comunidade também possa aprimorá-los.

Eu nomeei esse projeto de **fp-scripts**. 
Aqui está o [link](https://github.com/mdbs99/fp-scripts) do projeto no GitHub. Não deixe de ler o [README](https://github.com/mdbs99/fp-scripts/blob/master/README.md) do projeto.

Futuramente vou subir para o GitHub minha versão do FPC, Lazarus e Libs que utilizo nos meus projetos em produção.

## Conclusão {#conclusao}

Esse artigo mostrou opções de instalação do FreePascal, Lazarus ou outros "sabores" para desenvolvimento em *Object Pascal*.

Vimos também que é necessário conhecermos a compilação através de scripts para termos um maior controle e opções sobre nosso ambiente.

Criei um novo projeto no GitHub chamado [fp-scripts](https://github.com/mdbs99/fp-scripts) que contém scripts para compilação e instalação do FPC e Lazarus, no Windows, através de simples scripts em *Batch*. Espero que você e a comunidade goste.

Até logo.