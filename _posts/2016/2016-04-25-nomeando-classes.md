---
layout: post
title: "Nomeando Classes"
date: 2016-04-25
description: O nome de uma Classe deve significar quem ela representa e não o que ela faz.
summary: O nome de uma Classe deve significar quem ela representa e não o que ela faz.
image: /images/photo-1453946610176-6be21147c400.jpg
categories: 
  - Pascal
tags:
  - naming
keywords:
  - naming
  - nomeando
  - classes
---

O nome de uma Classe deve significar quem ela representa e não o que ela faz.

Não utilize nomes como Validador, Leitor, Controlador, *Parser*, 
Executor, *Manager*, *Handle*, etc. Esses nomes dizem o que fazem, não o que são.

<!--more-->

![Naming]({{ page.image }})

Considere alguns exemplos:

**\#1** Seu software trabalha com **Arquivos Texto** enviados pelo Banco
(arquivos de retorno de pagamentos). Então *TBankFileParser* é o nome da sua Classe?

**\#2** Você precisa **importar** um arquivo CSV proveniente de outro software. Então *TCSVImporter*
é o nome da sua Classe?

Não faça isso.

>"O nome de uma Classe deve corresponder ou representar uma Entidade pura ou uma versão dela"

Se não conseguimos encontrar um bom nome para as Classes e Interfaces, quer dizer que não 
sabemos o suficiente sobre o domínio do problema ou é apenas preguiça para pensar em nomes melhores.

Temos que pensar nas **Entidades** em si, e não nas **funções** que elas exercem.

Por exemplo, considere *File*. Um nome de **Entidade pura**, ou seja, um Arquivo de computador ou arquivo em
papel, dependendo do contexto. Se temos um arquivo para ler **não** devemos nomear a classe como *Reader*!
Utilize *File* ou uma variação deste.

Um nome como *TextFile*, é uma versão ou variação de *File*. Também está correto. Simples. Perfeito.

Então se temos um Arquivo Texto do Banco (exemplo #1) ele também deveria representar uma
Entidade. Seu nome deveria ser <code>TBankFile</code> ou algo parecido.

Se enviamos o Arquivo para o Banco (Remessa) e obtemos outro Arquivo de resposta
(Retorno), então poderíamos ter uma única interface <code>IBankFile</code> e duas classes 
<code>TBankShipmentFile</code> e <code>TBankReturnFile</code>. Ambas as Classes implementariam 
a Interface.

Mas o nome de uma Classe nem sempre será **exatamente igual** a mesma nomenclatura da Entidade real que está representando.

Em um dos meus projetos, o [AWS Lib](https://github.com/mdbs99/aws), tenho classes como <code>TS3Service</code>,
<code>TS3Bucket</code>, <code>TAWSSignatureVersion1</code> e muitas outras.

Por que não apenas *Service*, *Bucket* ou *Signature* respectivamente?
Porque Classes precisam de um **contexto** — um prefixo — quando seus nomes são muito ambíguos. 

Na vida real o contexto está implícito, mesmo se for somente através da linguagem corporal. Exemplo:
Quando falamos para uma pessoa "sua **manga** está suja"[^1] olhando para sua **camisa** fica claro do que se trata. 
Não há confusão com a **fruta** manga se ninguém está comendo no momento.

No código é mais difícil. Só tem... texto! Tudo tem a mesma "forma". Então como diferenciar os nomes?

Temos *units* que são *namespaces* e que poderiam ser utilizadas como "prefixo"
de todas as classes, mas quem faz isso?! Ficaria muito verboso. Ninguém quer isso — bem, talvez o pessoal do Java, 
eles gostam de escrever... :)

Então para ter um padrão no código eu tenho uma regra para a nomenclatura das Classes: **Contexto + Nome**.

Isso evita a colisão de nomes com Classes já existentes na VCL/LCL e também deixa o código mais homogêneo, mais
pradronizado. Ajuda na visualização e facilita o *code-completion* da IDE.

Mas cuidado para não ir somando Contextos um após o outro. 

Exemplo: <code>TAWSNetHTTPClient</code> 

Que tal esse nome de Classe?

A Classe tem 3 contextos (AWS, Net e HTTP) e mais o nome *Client*. 
Apesar da **vantagem** que ganhamos ao evitar a colisão de nomes, é **verboso** demais para codificar e, o mais 
importante, **ler**.

Não é uma regra perfeita, mas funciona pra mim.

Mesmo assim, após anos de codificação, ainda é uma **arte** bem difícil.

Até logo.

---

[^1]: Não fará sentido se você traduzir o artigo para o Inglês, já que a ambiguidade da palavra "manga" só faz sentido em Português.
