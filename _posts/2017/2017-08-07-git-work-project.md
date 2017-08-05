---
layout: post
title: Git-work Project
date: 2017-08-05
permalink: /:title
description:
  Git-work são extensões minimalistas para Git, fornecendo operações de repositório de alto nível.
image: /images/2017/photo-zachary-nelson-192289.jpg
tags:
  - git
  - git-work
keywords:
  - freepascal
  - fpc
  - delphi
  - lazarus
  - object-oriented
  - oop
  - mdbs99
  - git
  - git-work
  - git-api
  - git-flow
  - github
  - git-hub
---

Git-work são extensões minimalistas para Git, fornecendo operações de repositório de alto nível.

<!--more-->

![Unsplash image]({{ page.image }}) 
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Zachary Nelson on Unsplash</span>

## Introdução {#introducao}

***Git***, segundo a [Wikipedia](https://pt.wikipedia.org/wiki/Git),
é um sistema de controle de versão distribuído e um sistema de gerenciamento de código fonte, com ênfase em velocidade. O Git foi inicialmente projetado e desenvolvido por Linus Torvalds para o desenvolvimento do kernel Linux, mas foi adotado por muitos outros projetos.

Ele é o padrão atual para o controle de versão de softwares. Não há nenhum outro concorrente que seja tão simples e eficaz.

Apesar de ser um magnífico sistema, por muito tempo não havia um padrão definido em como utilizá-lo num trabalho em equipe — ou mesmo sozinho — ou seja, como enviar uma alteração feita por você; como ser eficaz ao trabalhar com *branches*; se devemos trabalhar diretamente no `master` ou em *branches* secundários; como iniciar uma alteração quando o projeto recebe uma *issue*, etc.

Então, agora, temos um padrão chamado `git flow`. Ele vem instalado nas últimas versões do Git e você pode configurá-lo digitando `git flow`.

No entanto, ainda que o `git flow` não seja um padrão complicado de se utilizar, ele não nos deixa customizá-lo de forma simples num único lugar (são alguns arquivos *bash* que devem ser editados), é verboso e não define um padrão para as mensagens dos *commits*.

Nesse artigo eu irei lhe apresentar o meu mais novo "*pet project*" chamado **git-work**, que é uma alternativa minimalista ao `git flow`.

## O que é Git-work {#git-work}

Assim como o `git flow`, o [**git-work**](https://github.com/mdbs99/git-work) é uma coleção de extensões para a linha-de-comando do Git.

O nome escolhido é devido a semântica ao digitar os comandos como, por exemplo, `git work done` para concluir uma `issue`. Legal, não?

É um projeto extremamente novo (apenas alguns dias) porém com um grande potencial, na minha humilde opinião.

São extensões minimalistas para Git, fornecendo operações de repositório de alto nível além de padronizar, um *fluxo de trabalho* com os *branches*, que poderá ser totalmente customizável.

## Motivação {#motivacao}

Eu tentei (tento) utilizar alguns *clients* para Git como [Github Desktop](https://desktop.github.com/), [SmartGit](http://www.syntevo.com/smartgit/) ou [GitKraken](https://www.gitkraken.com/). Apesar desses e dezenas de outras IDE's serem ótimos produtos, podem ter restrições de uso, além da curva de aprendizado para utilizá-los.

Além disso, no meu caso, eu utilizo Git em alguns computadores e não posso me dar ao luxo de ter que ficar atualizando e reconfigurando IDE's em todos esses computadores caso eu queria mudar meu modo de trabalho.

Eu sou um cara *"old-school"*.

Gosto das coisas mais [simples]({% post_url 2016-12-19-simplicidade %}) e manuais. Gosto de saber o que está acontecendo nos "bastidores" quando clico num botão. Gosto de aprender a utilizar algo reduzindo-o à sua interface mais básica. 

Então voltei para a linha-de-commando, onde posso fazer literalmente tudo com o Git. [Menos é mais]({% post_url 2016-11-28-menos-e-mais %}), certo?

Mas a linha-de-comando pode ser assutadora quando você não conhece os comandos. E, mesmo depois de aprendê-los, pode ser ineficaz se você tiver que fazer *tudo* de forma manual.

Então como eu poderia utilizar a linha-de-comando, mas automatizar os comandos mais utilizados e, ao mesmo tempo, definir um fluxo de trabalho padronizado?

A ideia de construir o [**git-work**](https://github.com/mdbs99/git-work) me ocorreu após uma tarde de intenso trabalho e uso do Git em um projeto particular.

Trabalhei em algumas *issues* >>>>>>>>

## Características {#features}

Inicialmente eu pensei em codificar o [**git-work**](https://github.com/mdbs99/git-work) em *Object Pascal*. Sério. Porém vi que o mais simples a fazer seria utilizar *Bash scripts*.

O Git é bem integrado com *Bash* então... Por quê não?

Então criei o projeto no Github, codifiquei as primeiras características e subi os fontes.

Então meu amigo [Fabrício Cabral](https://github.com/fabriciofx/) se interessou pelo projeto e começou a trabalhar e melhorar o código.

No momento da escrita desse artigo já temos algumas funcionalidades.

Após "instalar", digite `git work` para ver os comandos:

![git-work](/images/2017/git-work-1.jpg)

Esses comandos ainda serão aperfeiçoados, pois estão em constante desenvolvimento.

Por exemplo, para terminar um trabalho num determinado *branch* chamado "123" precisamos digitar: `git work done 123`. Mas em versões futuras não será necessário digitar o *branch* atual, facilitando ainda mais a digitação. O mesmo para o commando `release`.

### Fluxo de Trabalho {#flow}

Após um usuário registrar a *issue* #41 no seu sistema de *tickets*, você inicia um fluxo:

1. Digite `git work issue 41`: git-work irá criar uma nova *branch* com o nome "41" a partir da `master` e fazer o *checkout*;
2. Enquanto você vai alterando os fontes, poderá digitar `git work commit "mensagem"` para ir comitando seu trabalho;
3. Quando você tiver terminado o trabalho, digite `git work done 41`: git-work irá para o `master` e irá fazer o *merge* com a *branch* 41.
4. Então você pode enviar suas alterações para o servidor digitando: `git work push master`.
5. Quando tiver uma versão de *release* o comando `git work release 1.0` poderá ser utilizado. O git-work irá criar uma nova *tag* e enviar ao servidor.

Pode não parecer muito agora. Os comandos são muito similares aos comandos do próprio Git, porém tudo ainda está em desenvolvimento.

Almejamos deixar os comandos mais simples, com [menos parâmetros](https://github.com/mdbs99/git-work/issues/3). Então, num futuro breve, os comandos acima serão resumidos em apenas isso:

<script src="https://gist.github.com/mdbs99/0148935b556fd4892f260057945ac329.js"></script>

E, claro, os scripts irão verificar, na medida do possível, se você está utilizando os comandos certos, nos *branches* corretos.

O *branch* padrão poderá ser [configurado](https://github.com/mdbs99/git-work/issues/9) (padrão `master`) para que os comandos `done`, `push` e `release` saibam onde pegar os fontes atuais.

As mensagens do `commit` poderão ser [padronizadas](https://github.com/mdbs99/git-work/issues/8) com o número da *issue* no início. Exemplo: *"#41 this fix...".*

E se todos os (futuros) parâmetros customizáveis ainda não sejam suficientes, basta você alterar apenas o [único arquivo](https://github.com/mdbs99/git-work/blob/master/git-work.sh) desse projeto. 

## Conclusão {#conclusao}

Essa é apenas a versão `0.1` desse projeto que só tem poucos dias de vida.

Padronização, eficiência e simplicidade. É a proposta desse projeto.

Aqui está o link do projeto: [https://github.com/mdbs99/git-work](https://github.com/mdbs99/git-work).

Até logo.
