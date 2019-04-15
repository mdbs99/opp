---
layout: post
title: Xavier Package
date: 2017-09-18
permalink: /:title
description:
  Xavier é uma biblioteca leve, Orientada a Objetos, para trabalhar com XML.
image: /images/2017/photo-xavier.jpg
categories:
  - Xavier
tags:
  - xavier
  - package
  - open-source
  - lib
keywords:
  - freepascal
  - fpc
  - delphi
  - lazarus
  - object-oriented
  - oop
  - mdbs99
  - xavier
  - xavier-package
  - xavier-lib
  - open-source
  - package
  - github
  - xml
  - xml lib
  - xpath
  - node
  - attribute
---

Xavier é uma biblioteca leve, Orientada a Objetos, para trabalhar com XML.

<!--more-->

![Unsplash image]({{ page.image }}) 

## Introdução {#introducao}

Hoje em dia, todas as bibliotecas XML são muito complexas. Cada classe pode ter tantos métodos que pode ser muito difícil utilizá-las e compreendê-las. Essas implementações são muito procedurais também.

Que tal se tivéssemos uma *Library* leve, [Orientada a Objetos]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}), que trabalha com XML e que compila em [*Free Pascal*](https://freepascal.org/) e [*Delphi*](https://www.embarcadero.com/products/delphi)?

Hoje quero apresentar a você o meu mais novo projeto.

## Xavier {#xavier}

Eu não trabalhava no [Projeto James](https://github.com/mdbs99/james) já fazia mais ou menos um mês. Além da falta de tempo — os projetos dos clientes são prioridade, certo? — <del>temos</del> tínhamos uma [*issue*](https://github.com/mdbs99/james/issues/65) que não nos deixava prosseguir.

Estávamos tentando compatibilizar o código com o *Delphi*, porém a parte que trata de XML é um tanto complicado.

O maior problema é que o *Free Pascal* não tem a mesma implementação do *Delphi* para leitura e escrita de *streams* e arquivos XML.

No *Free Pascal* a implementação é baseada apenas em Classes, enquanto no *Delphi* a implementação é baseada em Interfaces — ponto para o *Delphi*.

Os desenvolvedores do *Free Pascal* sempre tentam compatibilizar suas implementações com o *Delphi* porém, ao que parece, a implementação de XML no *Free Pascal* foi feita *antes* da implementação do *Delphi* e eles escolheram fazer tudo apenas com Classes.

A maioria das *libs* que são compativeis entre ambos os compiladores utilizam-se de [diretivas de compilação]({% post_url 2017-07-24-diretivas-de-compilacao %}) para que seus códigos funcionem. Essa técnica, apesar de muito útil, pode transformar seu código em "espaguete" rapidamente.

Veja esse exemplo de como salvar um `TXMLDocument` em *stream*, no compilador *Free Pascal* e *Delphi*, utilizando diretivas:

<script src="https://gist.github.com/mdbs99/c630eb809a9d030917af1efcd7e453d6.js"></script>

E que tal isso?

<script src="https://gist.github.com/mdbs99/96e6f11f9a2184e4e9e932202751b9d0.js"></script>

Meio bizarro, improdutivo e confuso, não?

Quero escrever código mais elegante, limpo e sustentável usando a Orientação a Objetos e, infelizmente, as opções *Open Source* atuais não foram suficientes, considerando meus critérios pessoais.

Então, criei o [Xavier](https://github.com/mdbs99/xavier).

O principal objetivo do Xavier é substituir o código procedural comum em nossos projetos, que podem ter tantas condicionais e variáveis, num código mais declarativo e Orientado a Objetos, para trabalhar mais facilmente com o XML, sem diretivas de compilação no código final.

## Conclusão {#conclusao}

A implementação do Xavier ainda está no início mas todo o código disponível hoje está funcionando, com cobertura de testes automatizados.

Eu escrevi o código em apenas algumas horas. Ainda há muito o que fazer.

Estou fazendo minha parte...

E você, que tal fazer um [*fork*](https://github.com/mdbs99/xavier#fork-destination-box) do projeto e colaborar?

Link do projeto [https://github.com/mdbs99/xavier](https://github.com/mdbs99/xavier).

Até logo.