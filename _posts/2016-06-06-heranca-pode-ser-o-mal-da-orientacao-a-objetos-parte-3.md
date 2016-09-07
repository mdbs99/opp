---
layout: post
title: "Herança pode ser o Mal da Orientação a Objetos — Parte 3"
date: 2016-06-06
description: Não faça da Herança a sua primeira escolha para reutilizar código.
summary: Não faça da Herança a sua primeira escolha para reutilizar código.
image: /images/photo-90abc28jsj8383.jpg
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

No artigo anterior falei sobre Violação de Encapsulamento.
Nesse artigo irei falar sobre a **Duplicação de Código** ao utilizarmos a Herança de Classe.

<!--more-->

![Duplicação]({{ page.image }})

[Clique aqui]({% post_url 2016-05-30-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-2 %}) para ler a **Parte #2** 
dessa série, caso ainda não tenha lido.

##Introdução {#introducao}

Duplicação de Código utilizando Herança. Essa afirmação pode parecer **heresia** porque a maioria dos programadores
utilizam Herança de Classe justamente para **não** duplicar o código.

Em uma hierarquia de classes onde `B` e `C` herdam de `A`, basta adicionar a nova funcionalidade em `A` para termos 
disponível em `B` e `C`. Simples, rápido e sem duplicar código.

Mas será esse o melhor *design* para **reaproveitar** e não duplicar código?

>A Herança de Classe é simples de usar e entender, mas no longo prazo é provado que essa não é a melhor escolha
>ao projetar seu diagrama de Classes. Ao invés de Herança a melhor escolha é a
>[Composição de Objetos](https://en.wikipedia.org/wiki/Composition_over_inheritance).

Quando estamos começando um novo projeto — ou um novo módulo — sim, essa **parece** ser ser a "melhor maneira"
para reaproveitar código.

Parece.

Com certeza a Herança é **simples** e **intuitiva** no entanto não é o melhor *design* no médio e longo prazo.

Herança de Classe leva a um código altamente acoplado e sem flexibilidade.

##Duplicando o código {#duplicando-o-codigo}

Então, como é possível duplicar o código utilizando Herança?

Utilizando **Hierarquias de Classes**.

É contra intuitivo pensar nisso, mas é verdade. Pense por um minuto. Se você tem um comportamento/funcionalidade
espalhado entre Classes e Subclasses dentro de uma Hierarquia, como **reutilizar** esse código em outras Classes 
ou Hierarquias diferentes? Difícil.

Não estou dizendo que é impossível. Estou dizendo que **dificulta** muito a reutilização devido o comportamento não
ter sido codificado de maneira **coesa**, utilizando **Classes pequenas** com 
[apenas uma responsabilidade]({% post_url 2016-03-07-classes-devem-implementar-apenas-uma-responsabilidade %}).

Quando utilizamos Herança somos seduzidos pelo **fácil reaproveitamento** do código e por isso vamos agregando mais e 
mais funcionalidades nas Classes e Subclasses, tornando-as "inchadas" e dificultando o reaproveitamento entre módulos
ou Projetos distintos.

Na maioria das vezes exigirá uma refatoração massiva no código para haver reutilização, devido as Classes e Subclasses
estarem muito acopladas umas as outras.

Esse é o motivo da **Duplicação de Código**. É muito mais fácil e confortável — para a maioria dos programadores 
com prazos curtos e chefes estressados — duplicar o código do que refatorar, talvez, toda a Hierarquia.

##Exemplo {#exemplo}

Vejamos agora um exemplo utilizando Diagramas de Classes.

Antes eu gostaria de lembrá-lo que é fácil olhar um exemplo simples e pensar em inúmeras maneiras de fazê-lo melhor. 
Mas pense que esses são apenas exemplos para lhe mostrar o conceito e não a melhor maneira de implementar uma solução.

###O Mundo Animal {#exemplo-1}

Vou começar com o bom e velho "Exemplo de Animais". O exemplo é antigo, sim, porém você ainda tem dúvidas sobre
utilizar ou não herança — senão não estaria lendo esse artigo — então acho que o exemplo continua sendo válido.

Precisamos implementar um sistema que lida com Animais e resolvemos utilizar Herança de Classes para implementar
uma Hierarquia e "reutilizar o código".

Primeiramente o sistema só irá trabalhar com Cachorros e Gatos então temos o primeiro modelo:

![Diagram1](/images/Diagram01.png)

No diagrama acima temos 3 Classes simples: `Animal`, `Cachorro` e `Gato`.

Apenas a Classe `Animal` tem um método `Caminhar()` que é herdado para as Classes `Cachorro` e `Gato`. Queremos 
reaproveitar o código então basta implementar na Classe mais acima da Hierarquia, ou seja, `Animal`.

Então vamos imaginar como seria a implementação do método `Caminhar()`.

    function TAnimal.Caminhar: TAnimal;
    begin
      Result := Self;
      MovePata1;
      MovePata2;
      MovePata3;
      MovePata4;
    end;

Como estamos trabalhando, a princípio, com tetrápodes ou quadrúpedes, precisamos mover as 4 patas para caminhar.
Tudo funciona perfeitamente.

Então precisamos acrescentar mais alguns animais: Pato, Tubarão, Sapo, Morcego e Crocodilo.

Como somos espertos, vamos acrescentar mais abstrações para deixar o modelo mais "legal" e "sofisticado".

![Diagram1](/images/Diagram02.png)

Um `Pato` também caminha, no entanto ele caminha utilizando 2 membros. O `Sapo` e `Crocodilo` caminham com 4 patas, assim
como o `Cachorro` e `Gato`. O `Morcego` tem pernas, mas ele realmente caminha? Não sei. O `Tubarão`... complicou.

Veja que pelo simples fato de termos acrescentado outros animais, **toda a hierarquia deverá ser verificada** afim 
de saber se os métodos de Classes ancestrais continuam fazendo sentido para a nova hierarquia.

O que fazer? Há inúmeras possibilidades. Algumas mais certas, outras mais erradas. Mas o que é que a **maioria** 
dos programadores fazem — 
inclusive em [grandes empresas]({% post_url 2016-05-30-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-2 %}#exemplo-3) —
quando a hierarquia não reflete o mundo real? 

Eles sobrescrevem métodos, **ignorando-os** (métodos em branco) ou **lançando uma exceção** (métodos que existem, mas não devem ser
utilizados... WTF!).

Nesse ponto emerge a Duplicação de Código.

Como assim?

Dependendo da quantidade de Classes numa hierarquia, duplicar a implementação será o método escolhido.
No caso do nosso exemplo o método `Caminhar()` seria modificado para utilizar apenas
2 patas em algumas Classes. Em outras levantaríamos uma exceção naqueles Animais que não deveriam caminhar, como no caso da Classe `Tubarão`.

Mas isso é uma **solução alternativa** que está longe de ser a ideal.

##Solução {#solucao}

A Hierarquia está errada? Qual seria a implementação "certa"? Como reaproveitar o código sem usar Herança?

Quando os cientistas separam os animais em "Classes", eles fizeram-na baseado no que eles são (identidade) e não no que eles fazem (comportamento).

Um pato nada, assim como um crocodilo. Mas o ato de nadar **não pertence** ao pato nem ao crocodilo, pois nadar é um **comportamento** "implementado" em vários outros Animais (Classes).

Vou lhe contar como resolver o problema da implementação da Hierarquia Animal.

**Atenção**. Aqui está:

Um Animal não caminha. Quem caminha é um Cachorro, Gato ou Pato. Animal é uma abstração, uma classificação, 
assim como Mamífero ou Anfíbio. Animal não existe.

Tetrápodes, Bípedes, Mamíferos, Anfíbios, Aves, Peixes, Pássaros... **eles não existem**!

Que?!

É isso aí.

Não entendeu? Aqui vai outro exemplo.

Você já viu uma Árvore? Tenho certeza que não! Você viu, talvez, uma goiabeira, bananeira, macieira... Árvore é uma abstração, uma classificação. Árvore não existe.

Se Mamíferos, Anfíbios, Aves, Peixes...e Árvores não existem, o que são? 

São apenas [Interfaces]({% post_url 2016-01-18-interfaces-em-todo-lugar %}) que definem uma classificação para algum comportamento.

Então não há — ou não deveria haver — Hierarquia de Classes, apenas **Hierarquias de Interfaces**!

Cientistas não tem problemas em classificar novas espécieis porque a inclusão na hierarquia existente não "quebra" nenhuma "funcionalidade"
nas "Classes" abaixo desta. São apenas Interfaces.

Então, se não há Hierarquia de Classes, como reaproveitar código sem duplicar? 

Resposta: **Composição de Objetos**.

Objetos tem comportamento, Interfaces não. Um cachorro é um Objeto; um pato é um Objeto; o ato de caminhar é um comportamento de 
"algum objeto" que os pássaros sabem utilizar... ou será que **Alguém** "Duplicou o Código" em cada DNA porque era mais fácil de implementar? :)

##No próximo artigo...

No próximo artigo irei falar sobre **Forte Acoplamento** entre Classes que utilizam Herança.

[Clique aqui]({% post_url 2016-06-13-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-4 %}) para ler a **Parte #4** dessa série.

Caso você tenha alguma dúvida ou quiser compartilhar seus pensamentos sobre essa série, utilize a área 
abaixo para comentários.
  
Até logo.
