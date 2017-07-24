---
layout: post
title: Diretivas de Compilação
date: 2017-07-24
permalink: /:title
description:
  Diretivas de Compilação podem lhe ajudar a tornar seu código multi-plataform ou até mesmo cross-compiled.
image: /images/2017/photo-mathyas-kurmann-102977.jpg
tags:
  - pascal
  - language
  - cross
keywords:
  - freepascal
  - fpc
  - delphi
  - lazarus
  - pascal
  - object-pascal
  - object-oriented
  - oop
  - mdbs99
  - diretiva
  - diretiva de compilação
  - compiler directive
---

Diretivas de Compilação podem lhe ajudar a tornar seu código *multi-plataform* ou até mesmo *cross-compiled*.

<!--more-->

![Unsplash image]({{ page.image }})
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Mathyas Kurmann on Unsplash</span>

## Introdução {#introducao}

[Diretivas de Compilação](https://www.freepascal.org/docs-html/prog/progch1.html) são comandos poderosos que o desenvolvedor pode utilizar para customizar a compilação.

Essas Diretivas passam parâmetros para o compilador, informando os *argumentos* da compilação, *como* deve ser compilado e *o que* deve ser compilado.

Existem basicamente [3 tipos](http://docwiki.embarcadero.com/RADStudio/Tokyo/en/Delphi_compiler_directives) de Diretivas de Compilação:

* *Switch directive*
* *Parameter directive*
* *Conditional compilation directive*

Os dois primeiros tipos alteram os parâmetros de compilação, enquanto o último altera o que o compilador irá executar.

Nesse artigo iremos tratar do último tipo: *Condicionais*.

## Condicionais {#condicionais}

Apesar de serem comandos poderosos, *não* devem ser utilizados levianamente.

Com apenas alguns comandos condicionais, seu código [Free Pascal](https://freepascal.org/) ou [Delphi](https://www.embarcadero.com/products/delphi) pode ser compilável em várias plataformas.

Entretanto, a medida que vamos adicionando mais e mais diretivas, o código irá ficar mais complexo.

Vejamos o exemplo abaixo:
<script src="https://gist.github.com/mdbs99/dffca8326e924f4e036485a5674ea147.js"></script>

No código acima apenas a 1º e 3º chamada da função *Writeln* serão executadas.

Todas as diretivas e também a 2º chamada de *Writeln* não irão fazer parte do executável final.

Bem legal.

Entretanto, veja que o código ficou bem "poluído" e também temos um [acoplamento temporal]({% post_url 2016-07-04-objetos-imutaveis %}#acoplamento-temporal), pois as  constantes precisam ser definidas numa ordem específica.

Diretivas e Definições de constantes que serão utilizadas em apenas numa única [Unidade]({% post_url 2017-07-17-declarando-unidades %}) pode até ser gerenciável, mas e se tivermos trabalhando com dezenas ou até centenas de Unidades que irão utilizar as mesmas diretivas e definições, ainda acha que essa abordagem é a melhor escolha para a arquitetura do seu projeto com a finalidade de construí-lo como *multi-plataform* ou *cross-compiled*?

Eu acho que *não* e é por isso que no [Projeto James](https://github.com/mdbs99/james) estamos utilizando uma abordagem diferente.

## Encapsulando Diretivas {#encapsulando}

No James estamos codificando num estilo que chamo de *Encapsulamento de Diretivas*.

Um dos objetivos desse projeto é ser *cross-compiled*, ou seja, ele irá compilar em Free Pascal e Delphi. No entanto não queremos que nem os usuários (desenvolvedores) ou os autores desse projeto se preocupem com diretivas de compilação.

Atualmente não há diretivas condicionais no código de implementação dos métodos — com exceção da classe `TXMLComponent` que deverá ser [refatorada](https://github.com/mdbs99/james/issues/65) em breve.

O motivo dessa abordagem é que não queremos nos preocupar em  quebrar um código que é utilizado por 2 compiladores diferentes.

Não utilizamos arquivos *"include"* com pedaços de código.

Não utilizamos definições globais de condicionais.

Diretiva condicional é uma técnica *procedural*. Não gostamos.

Ao invés disso, utilizamos apenas [Objetos]({% post_url 2016-03-14-objetos-pensam-e-tomam-decisoes %}).

## Implementação {#implementacao}

Imagine uma Unidade que contenha classes para [representar]({% post_url 2016-02-29-objetos-representam-entidades %}) a criptografia [MD5](https://en.wikipedia.org/wiki/MD5).

No Free Pascal já temos uma Unidade `md5` que tem funções que fazem esse trabalho — e é claro que temos que fazer Objetos para encapsular essas funções.

No Delphi a Unidade que faz o mesmo trabalho é denominada `hash`.

Não queremos "reinventar a roda". Queremos utilizar o que já está pronto em ambas as plataformas.

Então, como fazer essa implementação sem utilizar diretivas condicionais no código de implementação ou arquivos de inclusão?

Bem, no James temos a Unidade [James.Crypto.MD5.Clss](https://github.com/mdbs99/james/blob/master/src/james.crypto.md5.clss.pas) com algumas classes que representam MD5.

Essa é a única Unidade (até a data desse artigo) que os usuários devem utilizar para trabalhar com MD5.

Nós, autores do projeto, poderíamos separar alguns desenvolvedores para trabalhar na implementação Free Pascal e outros para trabalhar na implementação Delphi, se assim o desejarmos, pois as implementações estão separadas em Unidades distintas.

Primeiro criamos mais duas Unidades que serão utilizadas pela `James.Crypto.MD5.Clss`:
Uma para Free Pascal e outra para o Delphi.

São elas [James.Crypto.MD5.FPC](https://github.com/mdbs99/james/blob/master/src/james.crypto.md5.fpc.pas) e [James.Crypto.MD5.Delphi](https://github.com/mdbs99/james/blob/master/src/james.crypto.md5.delphi.pas), respectivamente.

Veja abaixo como implementamos isso:

<script src="https://gist.github.com/mdbs99/1bc867bb6303223496c44d221eae9044.js"></script>

Ambas as Unidades possuem a definição da Classe `TMD5Hash` (sim, mesmo nome em ambas). Então bastou criar um *alias* (novamente, com o mesmo nome) direcionando `TMD5Hash` para a classe correta (dependente da plataforma) e, *voilá!*, temos uma Unidade "limpa" e sem condicionais na implementação dos métodos.

Agora temos duas Classes distintas, em Unidades diferentes, que podem evoluir independentemente sem receio de quebrar o código entre plataformas.

A clase `TMD5Stream` não tem nenhuma diferença entre os compiladores, então é implementada diretamente na Unidade `James.Crypto.MD5.Clss`.

## Conclusão {#conclusao}

Diretiva de Compilação é uma boa ferramenta para customização do código, porém deve ser utilizada com parcimônia.

No código [Orientado a Objetos]({% post_url 2016-03-14-objetos-pensam-e-tomam-decisoes %}), dê preferência aos Objetos para resolver seus problemas.

Para cada diretiva condicional que você queira adicionar no código de implementação, sugiro implementar um novo Objeto que encapsule a diretiva.

Seu código ficará mais limpo e sustentável.

Até logo.
