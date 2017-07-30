---
layout: post
title: Imutabilidade do Estado
date: 2017-07-31
permalink: /:title
description:
  Na Orientação a Objetos, um Objeto pode ser considerável Imutável mesmo que ele... mude.
image: /images/2017/photo-morgan-harper-nichols-157838.jpg
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
  - imutabilidade
  - immutability
  - imutable
---

Na Orientação a Objetos, um Objeto pode ser considerável Imutável mesmo que ele... mude. 

<!--more-->

![Unsplash image]({{ page.image }})
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Morgan Harper Nichols on Unsplash</span>

## Introdução {#introducao}

O aprendizado de um conceito complexo, tende a ser melhor absorvido se dividirmos o conceito em pequenas partes.

Por exemplo, quando começamos a aprender Inglês, nos é ensinado que, para nos referirmos a algo que ocorreu no passado, devemos utilizar os verbos no passado para fazer afirmações ou utilizamos o verbo modal *did* ou *did not* para fazermos perguntas ou negações. Esse é o tempo verbal *Simple Past*.

Exemplo: *I saw this movie* (tradução: eu vi esse filme).

Então, quando você começa a estudar mais a língua, descobre que existe muitas outras maneiras de expressar algo que ocorreu no passado. Por exemplo, o uso do tempo verbal *Present Perfect*.

Exemplo: *I have seen this movie* (tradução: eu vi esse filme).

Aqui vemos o uso do verbo modal *have* e o uso do *Past Participle* do verbo *to see*.

Ambas as frases dizem o mesmo quando traduzimos para o Português, porém a segunda frase pode ser considerada mais *correta* do que a primeira — você só descobre isso depois que aprende o *Present Perfect*.
Mas isso não é um blog de Inglês, vamos voltar para a Orientação a Objetos.

Essa introdução é para lhe mostrar que primeiro devemos aprender um conceito na sua forma mais simples; depois aprimoramos.

## Conceitos da Imutabilidade {#conceitos}

Quando você aprendeu sobre [Imutabilidade]({% post_url 2016-07-04-objetos-imutaveis %}) pode ter pensado que um Objeto é imutável se, depois de criado, nada é alterado dentro dele ou que o Objeto retorna sempre a mesma informação quando um método é chamado.

Bem, não é tão simples assim.

Diferentemente das linguagens funcionais, onde tudo é imutável por padrão, na Orientação a Objetos esse conceito pode ser mais amplo.

Vamos ver alguns conceitos.

### Conteúdo Externo {#ex1}

Uma classe que representa um arquivo pode ser imútável de duas maneiras:

<script src="https://gist.github.com/mdbs99/48237c902271b6f2c3a314d3b6f5b8c0.js"></script>

No código acima, o método `TFile.Stream: IDataStream` sempre irá retornar o mesmo valor lido na primeira execução.

O Objeto é imutável e *constante*.

Mas, e se alterarmos o método confome abaixo, a classe continuaria sendo imutável?

<script src="https://gist.github.com/mdbs99/0aea75fefaf8612d0bc84d5fc9ce85a0.js"></script>

Sim, com certeza.

Apesar do retorno do método poder ser diferente em cada chamada — o conteúdo do arquivo pode ser alterado por outro processo — o Objeto continuaria sendo imutável pois seu [Estado]({% post_url 2017-06-05-estado-do-objeto %}) (`FFilePath`) não foi alterado.

Ele é imutável, porém *não* é constante.

O mesmo conceito se aplica para um conteúdo vindo de um Banco de Dados, site na Web, etc.

### Conteúdo em Memória {#ex2}

Uma lista de Objetos é considerada mutável se adicionarmos itens após ela ter sido criada?

Vejamos no código:

<script src="https://gist.github.com/mdbs99/af8769161d1b111128349b05b74bccb9.js"></script>
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Copyright (c) [James Project](https://github.com/mdbs99/james)</span>

A Classe `TDataParams` encapsula uma lista do tipo `TInterfaceList`.

A cada inclusão de um novo item, é delegada à essa lista interna, a persistência em memória dos Objetos.

Você acha que, assim, estamos alterando o Estado (`FList`) do Objeto?

Não estamos.

O motivo é que não estamos redefinindo `FList`. Não estamos recriando uma nova lista.

Uma lista de itens está sendo criada em memória — blocos de memória — mas o endereço inicial de `FList` continua intacto.

Além disso, `FList` é um [Atributo Secundário]({% post_url 2017-06-12-atributos-primarios-secundarios %}#secundarios), então nós até poderíamos redefinir esse atributo sem estar em desacordo com o princípio. Mas, se a instância da lista fosse passada no construtor, então não poderíamos redefinir o Objeto, pois esse seria considerado como um [Atributo Primário]({% post_url 2017-06-12-atributos-primarios-secundarios %}#primarios).

## Imutabilidade do Estado {#estado}

Se mesmo após algumas mudanças internas ou externas ao Objeto, ele continua sendo considerado imutável, como saber se não estamos violando o princípio da Imutabilidade?

**Simples**: Se o [Estado do Objeto]({% post_url 2017-06-05-estado-do-objeto %}), ou seja, seus [Atributos Primários]({% post_url 2017-06-12-atributos-primarios-secundarios %}#primarios), não for alterado/redefinindo, então ele é imutável.

Uma vez que um atributo é instânciado, ele não poderá ter seu *endereço de memória* alterado. Esses atributos serão inicializados no [construtor]({% post_url 2016-03-21-construtores-da-classe-primario-secundarios %}) da classe e jamais poderão ser reinicializados.

Seu Objeto deve ser *fiél* aos argumentos passados no construtor da Classe, no entanto ele é *livre* para trabalhar e responder o que quiser em seus métodos.

## Conclusão {#conclusao}

Imutabilidade, na Orientação a Objetos, não é só sobre as não-mudanças internas ou externas ao Objeto, mas sim sobre não alterar o Estado Primário do Objeto.

Não confunda [dados]({% post_url 2016-08-01-classes-de-dados %}) com o Estado do Objeto.

Infelizmente nenhum compilador *Object Pascal* que eu conheça possui uma sintaxe para que essas regras não sejam quebradas. Java, por exemplo, tem atributos *final* onde, uma vez inicializados, não podem ter seu endereço de memória substituído.

Sven Barth, um integrante do FPC team, [me falou](http://lists.freepascal.org/pipermail/fpc-pascal/2017-July/051941.html) sobre um `{$modeswitch
finalfields}` que foi implementado para a interoperabilidade do FPC para a JVM plataforma, porém ainda não disponível.

Então temos que trabalhar apenas com o conceito ou utilizar ferramentas de verificação de código para que essa regra não seja quebrada.

Até logo.
