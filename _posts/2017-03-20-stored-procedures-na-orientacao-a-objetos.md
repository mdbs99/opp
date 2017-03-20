---
layout: post
title: Stored Procedures na Orientação a Objetos
date: 2017-03-20
description:
  As Stored Procedures ainda são necessárias ou a Orientação a Objetos é suficiente e podemos jogar fora essa antiga tecnologia?
image: /images/photo-clem-onojeghuo-143740.jpg
categories: 
  - Database
tags:
  - Database
keywords:
  - freepascal
  - delphi
  - lazarus
  - c#
  - csharp
  - java
  - object-oriented
  - oop
  - mdbs99
  - sp
  - storedprocedure
  - storedproc
  - stored-procedure
  - database
---

Stored Procedures são sub-rotinas armazenadas no SGBD disponíveis às aplicações que o acessam.

Será que as Stored Procedures ainda são necessárias ou a Orientação a Objetos é suficiente e podemos jogar fora essa *antiga tecnologia*?

<!--more-->

![Unsplash image]({{ page.image }})  

## Introdução {#introducao}

Todo SGBD disponibiliza *Stored Procedures* (SP's) para o DBA e desenvolvedores construirem e executarem scripts SQL.

As SP's podem retornar um cursor de dados, atualizar registros, executar rotinas de manutenção do SGBD, etc.

As SP's também podem conter estruturas de controle e repetição como `IF`, `WHILE`, `LOOP`, `REPEAT`, `FOR`, `CASE`, etc.

No entanto as SP's nos fazem lembrar de *procedimentos*, programas imperativos e sequenciais. E é exatamente essa [visão]({% post_url 2016-11-07-pensando-em-dados %}) que não queremos ter quando programamos utilizando o paradigma da Orientação a Objetos.

Então podemos declarar a *morte* das SP's e utilizar apenas a Orientação a Objetos a partir de agora?

Definitivamente, não.

## Ferramenta de Dados {#ferramenta-de-dados}

As *Stored Procedures* são ferramentas para trabalhar com *dados*. Elas conseguem ter uma melhor performance do que qualquer outra linguagem fora do SGBD no qual ela reside.

Só isso já seria um argumento suficiente para continuarmos a utilizar SP's em nossos sistemas.

Mas nem todos pensam assim.

Tenho alguns amigos desenvolvedores que abandonaram completamente as SP's, codificando seus sistemas *apenas* utilizando alguma linguagem de programação com suporte a Orientação a Objetos.

<blockquote>
  <p>
Stored Procedures podem  reduzir o tráfego na rede, visto que os comandos são executados diretamente no servidor. Além de melhorar a performance, criar mecanismos de segurança entre a manipulação dos dados do Banco de Dados. 
  </p>
  <footer><cite title="Wikipedia">— Wikipedia</cite></footer>
</blockquote>

No entanto, esses desenvolvedores esquecem que cada ferramenta é focada em resolver determinados problemas.

O foco das Stored Procedures são os dados. Na manipulação dos dados e assuntos relacionados ao Banco de Dados.

O foco da Orientação a Objetos são os [Objetos]({% post_url 2016-02-29-objetos-representam-entidades %}).

São contextos completamente diferentes.

Então por que usaríamos uma ferramenta *pior* ou menos eficaz para a manipulação de dados?

## Regras de Negócio {#regras-de-negocio}

Meus amigos afirmam que as *Regras de Negócio* não devem ser codificadas em (Argh!) procedimentos!

Utilizamos uma linguagem de auto nível com suporte a [Objetos inteligentes]({% post_url 2016-03-14-objetos-pensam-e-tomam-decisoes %}), [encapsulamento]({% post_url 2016-05-30-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-2 %}#encapsulamento), polimorfismo... não queremos trabalhar com algo tão [primitivo]({% post_url 2017-01-16-tipos-primitivos-nos-argumentos %}) — eles dizem.

E eu concordo com eles.

Em aplicações Orientadas a Objetos, as Regras de Negócio devem estar codificadas em Classes.

Utilizamos as Classes e Objetos para [representar]({% post_url 2016-02-29-objetos-representam-entidades %}) as entidades que irão interagir entre si.

Os Dados, no entanto, não interagem entre si. São *passivos*.

Os Dados precisam que algum *procedimento* ou *Objeto* que os anime.

Os Objetos podem e devem [trabalhar com dados]({% post_url 2016-08-01-classes-de-dados %}), mas a manipulação dos *dados em massa* é trabalho para Stored Procedures; são imbatíveis.

A combinação entre Objetos e Stored Procedures é a chave para um sistema Orientado a Objetos com ótima manutenabilidade mas, também, com ótima performance na manipulação dos dados.

## Scripts dinâmicos {#scripts-dinamicos}

Outro equívoco é pensar que somos obrigados a criar Scripts SQL no código da aplicação porque somente assim conseguiremos fazer filtros otimizados, combinando as informações que os usuários desejam para construir o melhor e mais otimizado SQL.

Em parte isso faz sentido, mas na maioria das vezes é apenas falta de conhecimento do SGBD em uso.

Alguns SGBD's como o Microsoft SQL Server, por exemplo, podem receber argumentos XML. Esses argumentos podem ser lidos e utilizados como filtros dinâmicos nas SP's.

Isso é muito melhor que ter "pedaços" de código SQL espalhados no código para concatenar de acordo com os filtros que os usuários desejam.

Alguns desenvolvedores poderão afirmar que utilizam algum framework ORM que tem embutido uma pseudo-linguagem que simula o SQL, mas utilizando Objetos. Assim eles não teríam SQL espalhado pelo código.

O conceito de ORM na Orientação a Objetos é errado. Mas esse assunto foge do escopo desse artigo. No entanto, se você utiliza um ORM para fazer *queries* dinâmicas eu posso lhe afirmar o seguinte: Você pode até tentar, mas nunca fará queries mais performáticas e/ou com o código mais simples do que utilizando SQL puro.

Não estou falando sobre Objetos que retornam pequenas listas de objetos-filhos utilizando a pseudo-linguagem do ORM. Estou falando sobre *queries* de 5 páginas ou mais de script que trazem dados para relatórios pesados em aplicações reais. Tentar reproduzir um script dessa magnitude utilizando Objetos é, no mínimo, ingenuidade...

## Conclusão {#conclusao}

As *Stored Procedures* continuaram a existir, enquanto houverem SGBD's relacionais.

Continuaram a existir enquanto não inventarem algo com mais performance.

Utilizar *Stored Procedures* não torna seu software *menos* Orientado a Objetos, se você as utiliza apenas para a manipulação de dados e não para as Regras de Negócio.

Utilize a ferramenta certa para cada tipo de problema.

Até logo.