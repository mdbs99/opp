---
layout: post
title: "Objetos Agregados"
date: 2016-12-05
description:
  Existe uma quantidade ideal para a quantidade de argumentos, 
  métodos, atributos e classes dentro de uma unidade.
image: /images/icfxslgjq50-james-garcia.jpg
categories: 
  - Objetos
tags:
  - aggregated
keywords:
  - aggregated object
  - objetos agregados
  - taggregatedobject
  - implements
  - delegation
  - delegação
--- 

Objetos Agregados é uma *feature* da linguagem *Object Pascal*. 
Ela nos permite delegar uma implementação
de uma Interface para outro Objeto, utilizando uma sintaxe especial.

No entanto existe um grande problema
no *design* dessa *feature* que não permite o programador utilizá-la em
sua plenitude. Estou falando dos terríveis vazamentos de memória.

Muitos programadores desistem de utilizar Objetos Agregados devido
a esses problemas... 

Bem, não mais. 

<!--more-->

![Unsplash image]({{ page.image }})

Já faz algum tempo que venho pesquisando uma maneira de utilizar Objetos Agregados
sem comprometer o *software* com vazamentos de memória.

Mas como minhas Interfaces possuem [poucos métodos]({% post_url 2016-11-28-menos-e-mais %}#metodos),
é fácil implementar os métodos (as assinaturas) e delegar para um Objeto privado,
ou seja, aquele que realmente irá fazer o trabalho.

Infelizmente essa abordagem é verbosa, ineficiente e nos leva a duplicação de código.

## Você lembra? {#voce-lembra}

A primeira vez que falei sobre esse assunto foi nesse
[artigo]({% post_url 2016-10-03-delegacao-de-implementacao-de-interfaces%}), no entanto
a implementação original possui vazamentos de memória.

Enviei um [email](https://www.mail-archive.com/fpc-pascal@lists.freepascal.org/msg43835.html)
sobre esse problema **crítico** na lista oficial do FreePascal e ninguém deu alguma 
solução plausível.

Utilizando o mesmo código de exemplo que enviei a lista, fiz algumas considerações
e publiquei um novo [artigo]({% post_url 2016-10-10-interfaces-delegacao-problemas-solucoes %})
com uma possível solução.

A solução para corrigir o código do exemplo foi implementar a classe `TDelegatedIntegerValue`.

Mas eu estava **errado**.

O motivo era simples, a *duplicação* de código continuava.

A Classe `TDelegatedIntegerValue` tem sua própria implementação.
Não há nenhuma reutilização da `TIntegerValue` original.

Eu poderia utilizar `TIntegerValue` como um atributo privado de `TDelegatedIntegerValue` e 
repassar as mensagens. Estaria correto. É o que todos fazem. Trivial.

Mas mesmo assim não ficaria satisfeito.

Para cada Classe que faz o real trabalho, eu teria
que ter uma outra Classe, herdada de `TAggregatedObject` que seria utilizada nas agregações.
Sempre teria um par de Classes: uma normal e outra para agregação.

Eu não queria ter *pares* de Classes para fazer o mesmo trabalho.

Eu não queria ter Classes que só poderiam ser utilizadas como *agregadas*.

Eu não queria ter Classes que só poderiam ser utilizadas *diretamente*.

O que eu queria era ter apenas uma **única classe** que pudesse ser utilizada *diretamente* ou
*agregada* a outro Objeto. E, claro, sem vazamentos de memória.

## Em busca do "código perfeito" {#codigo-perfeito}

Então eu comecei a implementação de um método que permitisse que qualquer Classe
fosse utilizada *diretamente* ou na forma *agregada*.

Comecei essa implementação no Delphi 7.

Tenho alguns (grandes) projetos que ainda
utilizam essa versão do Delphi. Nesses projetos você encontra de tudo: 

  * Código Procedural
  * Código que *parece* Orientado a Objetos
  * Código que *realmente* é Orientado a Objetos
  * Código RAD
  * e muito mais.

Então quando tenho que fazer alguma manutenção neles, é uma boa hora
para fazer uma refatoração.

Tive uma ideia que, a princípio, pareceu brilhante:

Iria unir as Classes `TInterfacedObject` e `TAggregatedObject` numa única nova Classe 
e herdaria todas as minhas Classes a partir daí.

Perfeito!

Certo? Err...

Bem, não tão rápido.

Se você abrir o código da RTL e localizar ambas as Classes, verão que elas divergem
muito na implementação. Para unir as duas eu tive que fazer várias condições — IF's —
em várias chamadas de métodos.

Exemplo. Se o argumento `Controller` que `TAggregatedObject` utiliza não é `nil`, então o
Objeto iria agir de um jeito, do contrário agiria de outra maneira.

Tive que copiar/colar o código de ambas as Classes;
estava trabalhando com o [anti-padrão]({% post_url 2016-04-11-nao-utilize-nil-ou-null%}) `nil/NULL`;
a Classe estava complexa e não estava mais implementando apenas uma
[única responsabilidade]({% post_url 2016-03-07-classes-devem-implementar-apenas-uma-responsabilidade %}).

A "solução" estava criando um monstro.

Isso estava indo contra muitas ideias que eu *defendo* como *corretas*.

Então...

Joguei tudo fora, esqueci do assunto e continuei minha vida.

Nenhuma outra linguagem tem isso — eu pensei — então por que continuar nessa busca?

Isso não funciona. Desisto.

## Uma solução absurdamente simples

O tempo passou.

Então num belo dia tive uma *inspiração*.

Todo esse tempo eu queria uma solução simples, mas estava pensando de forma errada... novamente!

Não tenho que ter apenas uma única Classe. Tenho que utilizar a
[decoração]({% post_url 2016-05-02-decorator-pattern %}) e composição de Objetos.

Voltei o exemplo original do
[artigo anterior]({% post_url 2016-10-10-interfaces-delegacao-problemas-solucoes %}#exemplo-2)
e, ao invés de implementar `TDelegatedIntegerValue`, implementei a Classe `TAggregateValue`.

Pouquíssimas linhas de código. Sem copiar/colar. Sem complexidade. Simples.

E isso fez **toda** diferença.

Veja o código completo do programa anterior, reescrito:

<script src="https://gist.github.com/mdbs99/a37d69af39bf859c0c9da77ce48f6a3b.js"></script>

A Classe `TAggregateValue` implementa `IValue`, porém ela não tem uma própria implementação. 
Essa Classe apenas repassa as mensagens a instância `FOrigin` — argumento passado no construtor —
que também implementa `IValue`.

Em outras palavras, para cada Interface no seu código você terá uma, e apenas uma, implementação agregada 
que poderá ser utilizada com qualquer outra "classe normal" que implemente a mesma Interface.

A Classe `TMyApp` continua implementando `IValue`, mas seu atributo privado que realmente
implementa `IValue` através da delegação, é 
[inicializado](https://gist.github.com/mdbs99/a37d69af39bf859c0c9da77ce48f6a3b#file-project-lpr-L75-L83)
através de uma composição/decoração entre `TAggregateValue` e `TIntegerValue`.

Apesar de continuarmos com 2 Classes como no exemplo anterior, veja que é possível codificar 
infinitas Classes que implementem `IValue`, mas utilizar apenas 1 Classe agregadora em todos 
os casos.

E tão bom quanto isso tudo é não ter vazamentos de memória:

    IntegerValue:
    TIntegerValue.Create
    Number is 5
    TIntegerValue.Destroy

    MyApp:
    TIntegerValue.Create
    TMyApp.Create
    Number is 10
    TMyApp.Destroy
    TIntegerValue.Destroy

    MyAppAsInterface:
    TIntegerValue.Create
    TMyApp.Create
    Number is 20
    TMyApp.Destroy
    TIntegerValue.Destroy

    Heap dump by heaptrc unit
    85 memory blocks allocated : 2049/2232
    85 memory blocks freed     : 2049/2232
    0 unfreed memory blocks : 0
    True heap size : 196608 (128 used in System startup)
    True free heap : 196480

## Conclusão {#conclusao}

Todas as minhas soluções anteriores iriam, de alguma forma, mudar a implementação
original das Classes que já estavam funcionando por todo o sistema. Esse é um dos
indícios que **A)** suas classes foram mal implementadas ou **B)** sua abordagem atual está 
errada porque você não deveria alterar Classes que já funcionam para adaptá-las a
novas exigências.

Eu estava insistindo na opção B, enquanto a resposta sempre esteve comigo.

**Decoração e Composição de Objetos**.

Até logo.