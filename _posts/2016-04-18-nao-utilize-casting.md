---
layout: post
title: "Não Utilize Casting"
date: 2016-04-18
description: Não Utilize Casting em Regras de Negócio.
summary: Não Utilize Casting em Regras de Negócio.
image: /images/photo-1445451983996-ac6b92ffb1fb.jpg
categories: 
  - Pascal
tags:
  - cast
keywords:
  - cast
  - casting
--- 

Sua equipe precisa de um programador Object Pascal para trabalhar num projeto que está
sendo codificado em FreePascal. Este projeto terá integração com um ERP codificado em Java e
um website codificado em PHP. Tudo utilizando MS SQLServer como SGBD.

Como você iria descrever o anúncio dessa vaga? 

<!--more-->

![Casting]({{ page.image }})

Uma tradução para *Casting*, em Português, seria **moldagem**. Você recebe algo e o "molda" em outra coisa.

Fazemos isso muitas vezes. Temos um *Integer* e o moldamos em uma *String* e vice-versa.

Mas será que o uso de *Casting* é necessário para codificarmos nossas **Regras de Negócio** utilizando
Objetos?

<center><p><strong>. . .</strong></p></center>

O arquiteto da equipe — sr. Anderson — já definiu o projeto.

Anderson definiu a API de integração, diagramas de classes, componentes, módulos, etc. Mas o projeto 
ficou maior que o esperado e agora precisam de um programador experiente para compor a equipe.

O arquiteto solicita ao seu gerente o novo programador, mas sem lhe dar instruções específicas.

O gerente repassa o pedido ao pessoal do RH e então eles anunciam a vaga em sites:

*"Empresa ACME Inc. precisa de programador experiente."*

A empresa recebe currículos com especialidade em COBOL, Delphi, C/C++, Java... PHP foi a maioria. :) 

Todos perdem tempo, pois terão muito mais currículos para **analisar e verificar** se o canditato 
se **encaixa nos critérios**, pois não definiram as **habilidades necessárias** que o programador deve ter
quando fizeram o anúncio.

O mesmo acontece quando utilizamos *Casting*. É necessário ter **código de verificação** para o Objeto 
afim de determinar sua Classe.

<blockquote>
  Seus Objetos devem trabalhar sob contratos, ou seja, interfaces. Somente Objetos
  qualificados — que implementam a Interface — poderão ser utilizados para fazer o serviço.
</blockquote>

Se você tem um trabalho a fazer, chame um **especialista** apto para fazer o trabalho — ou faça você mesmo.

O trabalho é determinado por um **contrato** que especifica as habilidades necessárias que o especialista 
precisa ter.

Na Orientação a Objetos os [contratos são representados por Interfaces]({% post_url 2016-01-18-interfaces-em-todo-lugar %}#interfaces-sao-contratos).

Interfaces, **não Classes.**

<center><p><strong>. . .</strong></p></center>

Anderson precisa de um programador. 

O RH altera os requisitos da vaga.

O programador deve saber:

  * Object Pascal
  * MS SQLServer
  * Desejável PHP, Java

Agora ficou mais fácil de encontrar o programador específico para o trabalho.

É isso que você deve fazer no seu código. Especifique Interfaces que definam o contrato necessário para fazer 
o trabalho.

Não defina argumentos do tipo de **Classes genéricas** — TObject, etc — sendo necessário utilizar *Casting* para verificar
qual o tipo de Classe do Objeto para fazer algum **processamento específico** dependendo da Classe. Não faça isso!

Interfaces são contratos de trabalho.

Defina bem seus contratos e você não precisará fazer *Casting* pois saberá que
somente **Objetos especialistas** irão se "candidatar" ao trabalho. Do contrário não fique surpreso de receber 
um **tomate** quando você esperava uma **maçã**! :)
  
Até logo.
