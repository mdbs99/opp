---
layout: post
title: "Pascal Deveria ser Modernizado?"
date: 2018-11-19
permalink: /:title
description:
  Você acha que estamos modernizando a linguagem Object Pascal?
image: /images/2018/hayes-potter-1054894-unsplash.jpg
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
  - inline
  - var-inline
  - generics
---

Ao longo dos anos, desde Turbo Pascal até as versões mais atuais do Delphi e Lazarus, temos visto muitas mudanças na linguagem.

Você acha que estamos modernizando a linguagem Object Pascal?

<!--more-->

![Unsplash image]({{ page.image }})
<br><span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Hayes Potter on Unsplash</span>

## Introdução {#introducao}

É incrível como até hoje eu ainda tenho que responder perguntas sobre os motivos de continuar utilizando a linguagem Object Pascal.

Vou desenvolver utilizando Object Pascal — eu digo a um possível cliente.

- Object Pas... Ah, aquela linguagem da Apple? (Objective C)
- Acho que você quer dizer Delphi, não é?
- Por quê você continua utilizando "isso"?
- Por quê não utilizar C# ou Java?
- Você ainda consegue achar emprego?

Eu já escrevi meus [motivos]({% post_url 2017-08-28-porque-eu-escolhi-delphi-e-object-pascal %}#motivos) mas entendo como pode ser difícil para a maioria das pessoas acreditarem.

Talvez eles pensem que Object Pascal não é tão [moderno](https://castle-engine.io/modern_pascal_introduction.html) quanto as linguagens *mainstream*.

## Quando o Concorrente é Prioridade {#prioridade}

Existem linguagens muito mais "modernas" hoje em dia e escolher uma que "está morta" não parece uma opção razoável <del>para aqueles que só sabem seguir cegamente o que a maioria diz</del>.

> Se você quer chegar onde a maioria não chega, faça o que a maioria não faz — Bill Gates

Talvez esse sentimento de modernização está abalando até mesmo os diretores e desenvolvedores do atual Delphi.

A "modernização" a que me refiro pode ser simplificada dessa forma:

- Se meu concorrente tem tal feature, eu preciso ter também.

E isso pode matar a linguagem.

### Inline Variable Declarations {#inline}

A alguns dias recebemos a notícia que a linguagem Delphi irá ter *Inline Variables*.

Marco Cantu — respeitado e conceituado no mundo Delphi — já começou a "matar" a sintaxe Pascal clássica de declaração de variáveis quando inicia seu [artigo](http://blog.marcocantu.com/blog/2018-october-inline-variables-delphi.html) escrevendo "Old Style Var Blocks".

Então é oficial, estamos *defasados*?

Na linguagem Object Pascal, declaramos as variáveis em blocos bem definidos e organizados, diferentemente de outras linguagens onde pode-se declarar onde quiser. Entretanto, agora isso parece ser ruim, já que a maioria faz diferente.

Solução? Ora, vamos copiar o concorrente!

O foco parece ser mais no que o concorrente anda fazendo nos compiladores deles do que nos seus fieis usuários e desenvolvedores Pascal.

### Type Inference for Inline Variables {#inference}

Na linguagem Pascal, tudo deve ser previamente declarado.

Tudo. 

Isso é algo intrínseco a linguagem desde o seu nascimento e é um dos pontos fortes dela, pois minimiza qualquer ambiguidade na declaração de tipos.

Tem quem goste, tem quem odeie. Não importa. Ela foi feita dessa forma e deve ser respeitada.

Mas, agora temos a *inferência de tipo*.

Isso existe em outras linguagens mas é provável que você conheça através do C#.

E as pessoas "aplaudiram"...

Sim, pode ser considerada uma facilidade. Você irá escrever menos. E 99% dos programadores Object Pascal — especialmente Delphi — adoram uma facilidade sem se preocupar com o custo. Sim, o custo, por quê na engenharia nada vem de graça. 

A compilação pode ficar mais lenta; pode haver erros de conversão obscuros; o código pode ficar menos legível... além de ir contra a própria filosofia da linguagem, que é declarar tudo antes.

Mesmo assim, se é fácil para escrever e o concorrente tem... por quê não?

## Conclusão {#conclusao}

Não há problemas com a declaração de variáveis atual. Esse é o estilo Pascal. Entretanto, parecem que estão mais preocupados em adicionar mais features a linguagem do que corrigir *bugs*. Bem, cada um sabe o que é melhor para sua empresa.

Trabalho com sistemas de alta performance que ainda utilizam a sintaxe clássica do Delphi 7. Então, posso afirmar que não é isso que irá deixar a linguagem melhor que seus concorrentes.

Não me entenda mal. Não estou dizendo que não podemos alterar a linguagem. Existem algumas features ou acertos que que seriam muito bem vindos como, por exemplo, a correção da sintaxe [WITH-DO]({% post_url 2018-04-02-a-declaracao-with-do-perfeita %}) ou uma nova sintaxe para para determinar um [alias para uma Unit]({% post_url 2018-06-25-fpc-macros %}#alias). Esses seriam melhorias que iriam corrigir problemas no design da linguagem, melhorando a leitura do código e tudo de acordo com a filosofia e estilo Pascal.

Não quero um [Frankenstein](https://en.wikipedia.org/wiki/Frankenstein) de várias linguagens. Quero um design limpo, [simples]({% post_url 2016-12-19-simplicidade %}) e eficiente.

Não quero copiar o concorrente. Quero escrever código Object Pascal.

Não faz sentido levar seu BMW clássico ao mecânico dizendo para deixá-lo mais parecido com uma Mercedes-Benz mais moderna. Você acabará ficando com  *nenhum* dos dois, apenas sucata.

Até logo.