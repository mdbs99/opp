---
layout: post
title: "Como Trabalhar com Libraries sem Pacotes"
date: 2018-07-09
permalink: /:title
description:
  Como seria trabalhar em um projeto que utiliza libraries que não possuem pacotes?
image: /images/2018/aditya-romansa-117300-unsplash.jpg
tags:
  - Object Pascal
  - macro
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
  - pacotes
  - packages
  - search path
---

Como seria trabalhar em um projeto que utiliza libraries que não possuem pacotes? Se cada desenvolvedor tiver cópias das libraries em paths diferentes, as configurações do projeto não devem utilizar paths pré-determinados.

<!--more-->

![Unsplash image]({{ page.image }})
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Aditya Romansa on Unsplash</span>

Pacotes facilitam muito o desenvolvimento, pois eles registram na IDE onde estão o código-fonte de suas libraries. Assim, cada desenvolvedor pode ter suas próprias cópias de libraries salvos em quaisquer pastas em seus computadores.

Um projeto que utiliza pacotes apenas irá fazer referência à eles e deixará a IDE o trabalho de localizá-los para informar ao compilador os códigos-fontes referentes aos pacotes.

Mas, e se uma library não possuir pacotes?

Nesse caso basta adicionarmos o path da library no *search path* do projeto. Ao compilarmos, os fontes serão localizados pela IDE.

Funciona. Porém, se o projeto tiver muitos desenvolvedores, como garantir que todos irão utilizar o mesmo path? Há uma grande possibilidade de haver divergências de paths em projetos colaborativos com mais de um desenvolvedor.

Desenvolvedores podem utilizar diferentes paths para projetos, componentes e libraries. Cada um organiza seus projetos da forma que achar melhor em seus próprios computadores.

Um projeto sem pacotes não tem a ajuda da IDE para localizar os fontes. O desenvolvedor deve informar onde eles estão.  Como cada desenvolvedor "gosta" de ter sua própria hierarquia de pastas, como configurar o *search path* do projeto?

*Macros.*

No Lazarus há [macros](http://wiki.lazarus.freepascal.org/IDE_Macros_in_paths_and_filenames) que podem ser interpretadas em *run-time* pela IDE.

A macro que precisamos para resolver o problema de paths é a `$Env(name)`.

Agora, imagine um library chamada `DBFast`, *sem* pacotes.

O desenvolvedor José tem a cópia da library em `c:\dev\libs\dbfast` enquanto o Márcio tem a mesma library em `d:\pascal\components\dbfast`.

Dois paths bem diferentes para um mesmo projeto.

Felizmente, utilizando macros a configuração é simplificada.

José e Márcio devem criar uma variável de ambiente em seus computadores apontando para esses paths. A variável de ambiente deve ter o *mesmo* nome, por exemplo, `DBFastSource`.

Finalmente, basta adicionarmos a macro `$Env(DBFastSource)` no [search path](http://wiki.freepascal.org/IDE_Window:_Compiler_Options#Paths) do projeto para que tudo funcione com paths diferentes para cada desenvolvedor.

Até logo.