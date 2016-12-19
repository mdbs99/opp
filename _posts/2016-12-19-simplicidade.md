---
layout: post
title: "Simplicidade"
date: 2016-12-19
description:
  Concentre-se no essencial, nos fundamentos, e mantenha simples.
image: /images/photo-hdnslau35ky-jeffrey-wegrzyn.jpg
categories: 
  - Codificando
tags:
  - simplicidade
keywords:
  - simplicidade
  - simplicity
  - simples
  - simple
  - simple code
--- 

Hoje em dia temos acesso a dezenas de linguagens com
inúmeros recursos como Orientação a Objetos, Generics, Classes 
anônimas, paradigma Funcional, Lambdas, Paralelismo...

Mas nós já dominamos o básico antes de começar a usar tais
*features*?

<!--more-->

![Unsplash image]({{ page.image }})  

> "When there is freedom from mechanical conditioning, there is
simplicity. The classical man is just a bundle of routine, ideas 
and tradition. If you follow the classical pattern, you are 
understanding the routine, the tradition, the shadow — you are 
not understanding yourself" — Bruce Lee

## Introdução {#introducao}

Me considero um programador *old-school*. Já desenvolvi/estudei com
linguagens como C, C++, COBOL, Pascal, *Object Pascal*, Visual Basic,
JavaScript, Java, PHP, C#, Lua e talvez algumas outras que tenha 
esquecido agora.

Apesar de ter estudado várias linguagens e utilizado algumas delas em 
projetos reais, o que tenho aprendido ao longo desses anos é que eu
poderia fazer (quase) tudo com apenas algumas linguagens, poucas
ferramentas.

Então escolhi *Object Pascal* para meus projetos. É uma linguagem
simples, elegante, fácil de ler/escrever, fortemente tipada e
possui (possuía) poucas features. Com ela eu consigo fazer desde um
simples projeto de fim de semana até grandes sistemas com centenas de
linhas de código.

Mas a "linguagem morta" — Object Pascal — cresce a cada dia, adicionando
mais e mais *features*, tentanto seguir outras linguagens.
E eu não gosto disso.

## Uma Ferramenta para cada tipo de Problema {#ferramenta}

É eu também já ouvi isso. Devemos usar a ferramenta apropriada
para cada tipo de problema. É claro que todos concordam.
Mas se você consegue ter 80% de eficácia investindo apenas 20%
do seu tempo e energia em algumas poucas ferramentas, por que acha
necessário aprender outras dezenas de linguagens e ferramentas complexas,
investindo mais tempo e dinheiro para conseguir produzir "apenas" 
20% a mais?
Lembre-se que segundo
[Paretto](https://pt.wikipedia.org/wiki/Princ%C3%ADpio_de_Pareto), 
você irá precisar investir 80% a mais de recursos para conseguir
aumentar a produtividade em 20%. Vale a pena?

Bem, talvez sim, talvez não. Depende.

Que tipos de projetos você faz hoje?

Ou melhor: Os tipos de projetos que você fazia a 3 anos ou mais são
os mesmos tipos de projetos em que trabalha hoje?
Se sim, quais são as grandes diferenças que uma mudança de linguagem
faria para toda a equipe?

Muitas mudanças positivas? Pode ser.
Mas será que o problema é a linguagem ou o problema é com a equipe
que A) não domina a linguagem que utiliza e B) não domina os requisitos
do sistema?

Talvez você diga que precisa de novas *features* para facilitar
o desenvolvimento. Sim, algumas *features* melhoraram muito o
desenvolvimento — a Orientação a Objetos, por exemplo — no entanto
outras não são tão essenciais assim.

O que eu vejo, na verdade, é que muitos programadores utilizam
novas *features* apenas porque são
[legais ou "*sexy*"](https://medium.freecodecamp.com/constant-confusion-why-i-still-use-javascript-function-statements-984ece0b72fd#.8b7hytmfk)
mas isso, na minha opinião, não deveria ser motivo para utilizá-las.

## Dia a Dia {#dia-a-dia}

No meu trabalho diário vejo problemas que seriam evitados se o código
tivesse um *design* mais simples.

Os programadores desejam mais e mais utilizar as novas *features*
sem pensar se uma adição de complexidade irá beneficiar o projeto 
no longo prazo.

> "It’s not the daily increase but daily decrease.
> Hack away at the unessential." — Bruce Lee

Alguns projetos em Java, por exemplo, são pura magia pra mim. São tantos 
artefatos que devem se interconectar que você perde a noção do que é
seu e do que faz parte das dezenas de *libs* e *frameworks*. Você
não codifica, apenas dá à esses *frameworks* o que eles querem.

É ótimo quando tudo funciona, mas quando há um problema...

Objetos são criados por "pura magia", pois foram "injetados" por
alguma entidade; registros não são salvos porque o *Session* não 
for marcado com "atributos especiais"; classes são "mapeadas" em
tabelas utilizando-se de dezenas de "atributos" que parece haver
mais código do que o próprio código da Classe! Mas a maioria acha
tudo isso bem legal. Programadores que não codificam. Esse é o mundo
hoje...

Você já parou pra pensar que talvez seja mais simples utilizar
[menos]({% post_url 2016-11-28-menos-e-mais %}) artefatos do que 
tentar aprender e aplicar todas as novidades que nos apresentam
diariamente?

Saint-Exupéry disse: *"Você sabe que alcançou a perfeição em design,
não quando não há mais nada para adicionar, mas quando já não há
mais nada que se possa retirar"*.

Ao invés de aumentar a complexidade e uso de artefatos, tente
diminuí-los.

Busque a simplicidade, não o contrário.

## Simplicidade {#simplicidade}

Eu vejo a Simplicidade como a [chave](http://www.extremeprogramming.org/rules/simple.html)
para minimizar 80% de todos os problemas existentes em projetos
de *software*. Bem, na verdade em qualquer tipo de projeto,
não apenas *software*.

Eu tento aplicar a simplicidade em tudo que eu faço. Diariamente.

Mas o que é simples para uns, pode ser complexo para outros.
Por exemplo. Um sistema pode ser altamente complexo, mas seus
desenvolvedores o consideram simples. Isso acontece porque eles
já estão acostumados e conhecem todos os módulos e problemas.

> "Simplicity is complex. It’s never simple to keep things simple.
Simple solutions require the most advanced thinking"
― Richie Norton

Então como saber se algo é realmente simples? Há alguma medida?

Em minhas pesquisas por essa definição, encontrei uma palestra
intitulada "Rumo a Ciência da Simplicidade" por George Whitesides.
Eu a tenho arquivada desde 2012 e agora compartilho o link com você.

<iframe src="https://embed.ted.com/talks/lang/pt-br/george_whitesides_toward_a_science_of_simplicity" width="640" height="360" frameborder="0" scrolling="no" webkitAllowFullScreen mozallowfullscreen allowFullScreen></iframe>

Para um Objeto ser considerável simples deve ter uma função 
*previsível* e *confiável*, com *baixo custo* e característica de servir,
ou ter o potencial de servir, como *blocos modulares*.

Não deixe de ver o vídeo. Pois essa é a melhor definição de 
Simplicidade que eu já ouvi. Perfeito.

Desde então eu tento seguir essa linha de raciocínio.

## Conclusão {#conclusao}

Abrace a simplicidade em todas as parte do projeto.
Faça com que suas Classes e Objetos sejam previsíveis, confiáveis,
de baixo custo computacional e que ajam como blocos modulares
que possam ser conectados entre si.

> "Simplicity is the ultimate sophistication" — Leonardo da Vinci

Quanto mais simples, menos *bugs*, menos *debug*, menos curva de
aprendizado, menos releitura do código.

Meu objetivo principal nos meus projetos é *qualidade*. 
Quero construí-los, entregá-los e esquecê-los. 
Não quero ficar fazendo remendos.
Quero fazê-los certo logo na primeira vez. 
Pra isso sua base de código deve ser simples.

Esse é o caminho.

Ou talvez eu seja apenas um "velho" programador,
avesso as "novidades"... você decide.

Até logo.