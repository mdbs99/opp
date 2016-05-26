---
layout: post
title: "Herança pode ser o Mal da Orientação a Objetos — Parte 2"
date: 2016-05-26
description: Não faça da Herança a sua primeira escolha para reutilizar código.
summary: Não faça da Herança a sua primeira escolha para reutilizar código.
image: /images/photo-1456087468887-17b7d7b076e0.jpg
categories: 
  - OO
tags:
  - herança
keywords:
  - herança
  - inheritance
  - evil
  - mal
  - orientação a objetos
  - oop
  - poo
  - encapsulamento
  - polimorfismo
--- 



<!--more-->

![Imagem]({{ page.image }})

[Clique aqui]({% post_url 2016-05-23-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-1 %}) para ler a **Parte #1** 
dessa série, caso ainda não tenha lido.

##Indrodução {#introducao}

Há muitos artigos na internet que falam sobre esse assunto, mas não encontrei nenhum que tenha apresentado esse conceito,
de forma satisfatória, para programadores *Object Pascal* e, aqui estamos.

Então, como um dos pilares da Orientação a Objetos, a Herança, pode se **contrapor** a outro princípio, o **Encapsulamento**?

<blockquote>
  ...a herança de classe permite definir a implementação de uma classe em termos da implemetação de outra.
  A reutilização por meio de sublcasses é frequentemente chamada de reutilização de caixa branca (ou aberta).
  O termo "caixa branca" se refere à visibilidade: com herança, os interiores das classes ancestrais são frequentemente
  visíveis para subclasses.
  <footer><cite title="Padrões de Projetos">Padrões de Projetos, 2002 — p.34 — Erich Gamma e outros — ISBN 85-7307-610-0</cite></footer>
</blockquote>

O texto é claro.

O autor escreveu que **"...os interiores das classes ancestrais são frequentemente visíveis para subclasses"**.
Ele não escreveu, especificamente, que a Herança Viola o Encapsulamento mas você percebeu que é a mesma coisa?

O interior de uma Classe não pode ser visível, mesmo para Subclasses, porque isso significaria **violação de 
encapsulamento**. Ponto.

Se ainda não acredita ou não concorda, continue lendo.

<blockquote>
  Porque a herança expõe para uma subclasse os detalhes da implementação dos seus ancestrais, frequentemente
   é dito que "a herança viola a encapsulação"[Sny86]
  <footer><cite title="Padrões de Projetos">Padrões de Projetos, 2002 — p.34</cite></footer>
</blockquote>

Vou repetir: A Herança Viola o Encapsulamento.

Isso é um **fato lógico**. Pode não parecer lógico se é a primeira vez que você lê essa afirmação, mas você entenderá.

Quando li isso a primeira vez, pouco tempo depois que comecei a codificar software "Orientado a Objetos", tive o 
sentimento de **negação** que você pode estar tendo agora:

— "Ora, isso não faz o menor sentido!"

##Encapsulamento {#encapsulamento}

Precisamos definir o que é Encapsulamento que, de acordo com o dicionário, significa:
**colocar ou encerrar em cápsula; capsular**.

Uma cápsula não pode — ou não deveria — ser quebrada. O mundo externo ao Objeto não pode saber o que há em seu
interior a menos que o Objeto queira lhes dizer "com suas próprias palavras", ou seja, através de métodos que 
retornam informações mas não necessariamente seu [Estado]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}#objeto-nao-e-um-balde-de-funcoes-e-dados).

O mundo externo só deve conhecer os métodos públicos de um Objeto. Seus métodos públicos representam sua
[Interface]({% post_url 2016-01-18-interfaces-em-todo-lugar %}) para fazer o que ele deve fazer. Qualquer método ou
atributo interno ao Objeto é de propriedade dele e de mais ninguém.

Um Objeto pode ter um único método em sua Interface
porém pode haver 10 outros métodos privados apenas para resolver um único problema. Mas o mundo externo não sabe e não deve
saber como o Objeto trabalha internamente.

Um subtipo de Classe viola esse conceito.

##Herança de Classe

O motivo de termos Subclasses é **acrescentar** mais funcionalidade às Classes ancestrais.
Ao acrescentar funcionalidades a nova Subclasse deverá, inevitavelmente, interagir com o Estado e Métodos protegidos da 
Classe ancestral. Em outras palavras, a Subclasse deverá conhecer como a Classe ancestral trabalha (internamente) para
poder adicionar funcionalidade, o que é uma clara **Violação de Encapsulamento**.

<blockquote>
  A implementação de uma Subclasse, dessa forma, torna-se tão amarrada à implementação da sua classe-mãe que qualquer mudança
  na implementação desta forçará uma mudança naquela.
  <footer><cite title="Padrões de Projetos">Padrões de Projetos, 2002 — p.34</cite></footer>
</blockquote>

A Herança de Classe é simples de usar e entender, mas no longo prazo é provado que essa não é a melhor escolha
ao projetar seu diagrama de Classes. Ao invés de Herança a melhor escolha é a
[Composição de Objetos](https://en.wikipedia.org/wiki/Composition_over_inheritance). Esse é um
dos princípios proposto no livro Padrões de Projetos:

*Favoreça a composição de objetos em relação à herança de classe*.

##Implementando a Violação do Encapsulamento

A teoria foi explicada. Agora vou provar os conceitos apresentados, implementando no código.

<code>
begin ... end
</code>

Espero que você tenha entendido. Herança viola o Encapsulamento. Não é apenas um conceito, mas um fato.

##No próximo artigo...

No próximo artigo irei falar sobre **Duplicação de Código** ao utilizarmos Herança.

Caso você tenha alguma dúvida ou quiser compartilhar seus pensamentos sobre essa série, utilize a área 
abaixo para comentários.
  
Até logo.