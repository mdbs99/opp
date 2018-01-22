---
layout: post
title: "Versões Antigas dos Compiladores"
date: 2018-01-22
permalink: /:title
description:
  Vale a pena manter seu código executando nas versões antigas do compilador?
image: /images/2018/photo-danielle-macinnes-222441.jpg
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
  - old-school
  - delphi 6
  - delphi 7
  - mormot
---

A primeira versão do Delphi data de 1995, após o Turbo Pascal,enquanto o compilador Free Pascal é de 1993.

Estamos em 2018 agora. Será que vale a pena manter seu código executando nas versões antigas do compilador?

<!--more-->

![Unsplash image]({{ page.image }})
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Danielle MacInnes on Unsplash</span>

Talvez não valha a pena manter o código dos seus projetos funcionando nas versões antigas, uma vez que você tenha conseguido migrar todo o código-fonte para uma versão mais nova. No entanto, suas bibliotecas — especialmente as públicas, Open Source — poderiam (ou deveriam?) continuar compilando nas versões mais antigas dos compiladores.

**O primeiro motivo** é bem simples. *Compatibilidade*.

Mas por quê isso é importante?

Trabalho profissionalmente com o Delphi desde sua versão 4 e um dos critérios que me fizeram continuar na ferramenta foi a retrocompatibilidade. Um código que executava na versão anterior, continuava executando na versão mais nova.

Lembro das palestras e encontros proporcionados pela antiga Borland, onde o palestrante abria aquele "Projeto dos Peixinhos" feito no Delphi 1~2 e ele continuava compilando normalmente, sem alterações, na versão mais nova.

É difícil ver esse nível de comprometimento com o usuário/desenvolvedor hoje em dia. Quantos frameworks a Microsoft já criou e matou na plataforma .NET? Temos que ficar correndo como "ratos" atrás do próximo "queijo sofisticado" para fazer o mesmo tipo de projeto que já estávamos fazendo a 20 anos atrás? Tenho certeza que não.

**O segundo motivo** é proporcionar mais *opções* de bibliotecas à comunidade Object Pascal.

Hoje em dia a linguagem se tornou um (pequeno) nicho. Enquanto nascem "centenas" de frameworks JavaScript todos os dias, não vemos o mesmo empenho na comunidade Pascal. Então, é importante manter o código legado funcionando para manter os desenvolvedores fieis a linguagem. Senão eles irão procurar alternativas lá fora. Melhor ainda, seria se tivéssemos mais opções a escolher, no entanto.

Eu já publiquei algumas bibliotecas Open Source a alguns anos, mas agora elas não existem mais. Não valiam a pena. Eram só "brinquedos". Elas deveriam mesmo desaparecer. No entanto existem bibliotecas que fizeram e ainda fazem muita diferença no desenvolvimento hoje como a RX e JEDI. Acredito que essas bibliotecas não compilem utilizando versões antigas, no entanto elas foram atualizadas, o que já é um grande mérito para seus mantenedores.

Existem ainda alguns *ninjas* do Object Pascal que mantém uma *única* base de código desde o Delphi 6 até o Delphi 10.2 Tokyo, compilando também no FPC 2.7.1/3.1.1 em diante. Me refiro ao framework [mORMot](https://synopse.info/fossil/wiki?name=SQLite3+Framework).

**mORMot é o estado-da-arte** dentre todos os projetos Object Pascal que conheço. Com uma *única* base de código e sintaxe "old-school", sem generics, sem classes anônimas, sem métodos anônimos, sem n.a.m.e.s.p.a.c.e.s, ou seja, "apenas" com Classes, Interfaces e funções, o autor Arnaud Bouchez e colaboradores foram capazes de criar e manter ao longo dos anos, um framework Client-Server ORM/ODM SOA MVC completo, quase sem dependências externas, com performance *superior* as soluções *modernas* como Node.js.

Isso prova que [simplicidade]({% post_url 2016-12-19-simplicidade %}) e arquitetura bem feita são muito mais importantes que novas *features* na linguagem, novos componentes, nova IDE...

A alguns anos eu voltei a publicar projetos Open Source, com uma nova abordagem e profissionalismo. Mas quando vejo projetos como o mORMot, tenho a plena convicção do quanto eu ainda tenho que aprender.

Hoje utilizo FPC/Lazarus mais do que Delphi. Comecei a migrar meus sistemas e bibliotecas privadas para a "plataforma concorrente" a alguns anos. Nessa época o Lazarus era um "brinquedo" mas, felizmente, continuei a utilizá-lo e hoje o Lazarus é tão bom quanto o Delphi — talvez melhor. No entanto, o *Delphi 7* ainda faz parte do meu dia-a-dia. Então, vale a pena fazer bibliotecas para essa versão? Talvez.

Saber que as mesmas bibliotecas que eu sempre utilizei foram sendo atualizadas ao londo de todos esses anos, é um conforto. Não precisarei reescrever meus sistemas... pelo menos enquanto mantermos a linguagem viva.

Até logo.
