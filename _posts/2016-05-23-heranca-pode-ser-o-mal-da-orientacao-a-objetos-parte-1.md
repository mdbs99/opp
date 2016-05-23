---
layout: post
title: "Herança pode ser o Mal da Orientação a Objetos — Parte 1"
date: 2016-05-23
description: Não faça da Herança a sua primeira escolha para reutilizar código.
summary: Não faça da Herança a sua primeira escolha para reutilizar código.
image: /images/photo-1444212477490-ca407925329e.jpg
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

Se você utiliza Herança em seu código, especificamente **Herança de Classe**, pode estar
cometendo um **erro** terrível.

Com o uso da Herança você **viola o encapsulamento** de suas Classes, deixa seu código menos
**flexível** e mais **complexo**.

Herança de Classe deve ser **evitada** a todo custo.

<!--more-->

![Imagem]({{ page.image }})

##Indrodução

Se você já programa utilizando **Orientação a Objetos** a muito tempo, utilizando **Classes e 
Subclasses** em seu código para "reutilizar" funcionalidades, eu realmente **não** espero que você 
entenda e concorde comigo tão rapidamente. 

Será necessário **tempo**.

Tempo para digerir as informações. Tempo para uma **releitura**. Tempo para **pesquisas** feitas por
você mesmo. Tempo para ler sobre **outras linguagens**. Tempo para ler outros **artigos e livros**.

Depois você poderá formar sua opinião própria. Mas faça isso baseado em **fatos** e não baseado
em **preconceitos** ou ignorância de informações.

No fim eu espero veementemente que você junte tudo isso e chegue a mesma conclusão que eu cheguei: 
**Herança é o Mal**.

E, claro, é importante saber que grandes empresas e desenvolvedores ao redor do mundo já 
chegaram a mesma conclusão a muito tempo.

Mas porfavor, não pense que estou **abolindo** completamente a Herança. Estou lhe dizendo que ela é má,
mas mesmo assim haverá momentos em que poderá ser mais **simples** ou eficiente utitilizá-la do que outra 
abordagem. Por exemplo, um médico pode tomar a decisão de amputar a perna de um paciente para mantê-lo vivo. 
Não é algo "bom" para se fazer se pensarmos somente na amputação, no entanto pode ser a melhor decisão se 
você pensar no todo — a sobrevivência do paciente.

É o mesmo com a Herança. Você tem essa possibilidade, mas só deverá utilizar em último recurso.

##Herança

A Herança é um dos pilares da Orientação a Objetos, assim como **Encapsulamento** e **Polimorfismo**.

Se uma Classe B herda de A, então B **é-um** A. Herança é isso. As Subclasses serão substitutas mais especializadas
de suas Super Classes.

Quando comecei a estudar Orientação a Objetos — 20 anos atrás — foi a primeira coisa que li. Esse era o 
mantra na época: **Herança, Encapsulamento e Polimorfismo**.

Para alguém como eu que só tinha visto **código procedural** em C ou Pascal, declarar uma Classe e Subclasses para
herdar o comportamento reutilizando o código era... **mágico**.

Acho que Herança é o que mais **impressiona** quem está aprendendo — ou mesmo utilizando a muito tempo — a
programar Orientado a Objetos.

Com Herança o programador consegue extender uma Classe com novos
métodos ou mesmo alterar o comportamento da Hierarquia com pouco código. Mágica.

E isso não é bom?!

Quase nunca.

Eu não entendi logo na primeira vez que li sobre "Herança ser ruim". Eu já me considerava um desenvolvedor
**experiente** em Orientação a Objetos, no entanto eu tive que mudar meus conceitos. Na verdade eu tive que
**reaprender** o que era Orientação a Objetos. Não só por causa do uso da Herança, mas por causa de uma série
de "novas" ideias que venho coletando, estududando e divulgando aqui mesmo, nesse blog.

E é por isso que eu acho que o mesmo deve acontecer à você, ou seja, mais cedo ou mais tarde você terá que 
mudar/atualizar seus conceitos sobre Orientação a Objetos. É inevitável.

Mas se você for uma pessoa mais **sábia** do que eu fui na época quando li sobre **Herança ser ruim e que deve
ser evitada**, você poderá obter o benefício de ler sobre isso agora e não ter que perder tempo em busca 
de uma resposta para a **dor** que eu sei que você tem hoje, quando seu código começa a não fazer mais sentido; 
quando sua **Hierarquia de Classes** se torna um peso e atrapalha mais do que ajuda; quando você tem que sobrescrever
métodos para não "fazer nada", desabilitando funcionalidades; ou quando você tem que **duplicar código** porque sua
Classe foi codificada numa "Hierarquia errada".

##Os Males

Se você nunca ouviu dizer que Herança é ruim, deve estar pensando que eu sou louco. Bem, saiba que eu não inventei
tal afirmação. Gostaria de citar um artigo que encontrei na Internet a muito tempo e que serviu
de inspiração para o título desse artigo que você está lendo agora.

O [artigo](http://www.javaworld.com/article/2073649/core-java/why-extends-is-evil.html) foi publicado na JavaWorld em
**1 de Agosto de 2003**, intitulado: "*Why extends is evil*".

Abaixo um pequeno trecho desse artigo. (Tradução Livre)

<blockquote>
  Uma vez fui a uma reunião do grupo de usuários Java, onde James Gosling (inventor do Java) foi o orador de destaque. 
  Durante o memorável Q&A sessão, alguém lhe perguntou: "Se você pudesse fazer Java novamente, o que você mudaria?" 
  "Eu deixaria de fora as Classes", ele respondeu. Após sessarem os risos, ele explicou que o verdadeiro problema 
  não era as Classes em si, mas sim a Herança de Implementação (extends). Herança de Interface 
  (implements) é preferível. Você deve evitar a Herança de Implementação, sempre que possível.
  <footer><cite title="JavaWorld">Why extends is evil — JavaWorld</cite></footer>
</blockquote>

Nessa série de artigos vou tentar explicar os motivos porque eu — e milhares de desenvolvedores ao redor do mundo — 
considero Herança algo ruim e que deve ser evitada a todo custo.

Os principais motivos são:

  1. Viola o Encapsulamento
  2. Duplicação de Código
  2. Forte Acoplamento
  3. Hierarquias Complexas

##No próximo artigo...

No próximo artigo irei explicar em detalhes porque a Herança Viola o Encapsulamento, com exemplos de código.

Caso você tenha alguma dúvida ou quiser compartilhar seus pensamentos sobre essa série, utilize a área 
abaixo para comentários.
  
Até o próximo artigo!

