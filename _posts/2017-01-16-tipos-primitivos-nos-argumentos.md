---
layout: post
title: Tipos Primitivos nos Argumentos
date: 2017-01-14
description:
  Utilizar tipos primitivos em argumentos de Métodos
  é uma prática que deve ser evitada quando programamos
  Orientado a Objetos.
image: /images/photo-3_i4nvi9d1k-randall-bruder.jpg
categories: 
  - Code
tags:
  - code
  - object pascal
keywords:
  - code
  - código
  - object pascal
  - duplication
  - duplicação
--- 

Se os Objetos conversam entre si utilizando mensagens, 
ou seja, através dos seus Métodos, por que utilizaríamos
Tipos Primitivos nos argumentos desses Métodos ao invés
de utilizarmos Objetos?

<!--more-->

![Unsplash image]({{ page.image }})  

## Introdução {#introducao}

Os Objetos conectam-se uns aos outros através de seus 
[construtores]({% post_url 2016-03-21-construtores-da-classe-primario-secundarios %})
e Métodos, enviando argumentos uns aos outros.

No paradigma da Orientação a Objetos, tudo são Objetos.
Inclusive os argumentos.
Bem, deveriam ser, pelo menos na maioria dos casos.

Imagine um um formulário onde o usuário precisa digitar
seu *login*. Esse formulário, que contém Objetos visuais
para capturar a digitação, irá enviar o *login* digitado
à outro Objeto para validar a identificação.

Seria o *login* um Objeto ou apenas texto?

Você pode pensar que o *login* é apenas uma informação
simples, um tipo *string*, apenas texto puro, não havendo
necessidade de um Objeto.

Mas, não seria esse *"texto puro"* também um Objeto?

Tudo são Objetos na Orientação a Objetos.

Confuso?

## Argumentos {#argumentos}

Tudo são Objetos, mas nem sempre os requisitos são
implementados como tal.
Os motivos são diversos, mas *simplicidade* e *performance*
sempre são citados como motivos plausíveis para o uso
de tipos primitivos nos argumentos.

Utilizar tipos primitivos nos argumentos é um anti-padrão chamado
[*"Primitive Obsession"*](http://enterprisecraftsmanship.com/2015/03/07/functional-c-primitive-obsession/).

Então vamos ver esses motivos e tentar dismitificá-los.

### Performance {#performance}

É claro que Objetos são "mais lentos" para trabalhar e
consomem "mais recursos" que os tipos primitivos.
No entanto a Orientação a Objetos é sobre a arquitetura
do sistema, deixando a performance por conta do compilador
e código de máquina gerado.

A diminuição da performance — mesmo que mínima —
em sistemas Orientado a Objetos é um preço justo a se 
pagar devido a melhoria na arquitetura que o paradigma
da Orientação a Objetos nos traz.

Na minha experiência, utilizando Objetos pequenos e simples,
que implementam apenas
[uma única responsabilidade]({% post_url 2016-03-07-classes-devem-implementar-apenas-uma-responsabilidade%}),
não há perda de performance significativa.

Mas, ainda assim, se seu maior requisito for performance,
talvez a Orientação a Objetos não sirva para seu projeto...
talvez.

### Simplicidade {#simplicidade}

Seriam os tipos primitivos mais [simples]({% post_url 2016-12-19-simplicidade %})
de usar do que Objetos?

De acordo com a minha experiência, na maioria das vezes, não.

Pense bem: Quantas vezes você teve que passar uma *"Data de Nascimento"*
como argumento de um Método e lá dentro ter que convertê-la
em *String*, por exemplo, para exibir ao usuário?

Quantas vezes passou um *"Nome Completo"* por parâmetro como
sendo apenas uma *String* mas depois teve que usar uma função
`Copy` porque só queria exibir o *"Primeiro Nome"* ou a primeira
parte do nome?

Sem falar na [repetição de código]({% post_url 2017-01-09-codigo-duplicado-talvez-nao %})
que poderia ocorrer com o uso dessas funções auxiliares em 
várias partes do código.

Com Objetos seria **muito mais simples**.

Veja. Um Objeto *"Data de Nascimento"* seria responsável por
algum cálculo de idade, além de retornar a si mesmo no formato
*String*.

Um Objeto *"Nome Completo"* seria responsável por retornar
o primeiro nome e sobrenome; talvez retornar tudo em maiúsculo
seria outro Método.

Quer outro exemplo?

Trabalho bastante com *GUID's*. Elas são utilizadas em meus
sistemas como chaves-primárias de registros.

Se não me engano, o tipo `TGuid` no *Object Pascal* é um *record*.
Não lembro, pois eu não o utilizo mais, não diretamente.

Eu tenho minha própria Classe `TDataGuid` que implementa uma
Interface `IDataGuid`.

Essa Classe tem alguns construtores com argumentos diferentes
para inicializar o [estado]({% post_url 2016-04-04-objetos-sem-estado %})
do Objeto, que internamente é um `TGuid`.
Posso receber uma *String* com ou sem "{...}", posso receber um `TGuid`
ou mesmo não receber nada e inicializar o estado internamente.

Por quê um Objeto ao invés de um "simples" *record*?

Nos meus sistemas, um *GUID* tem [comportamento]({% post_url 2016-03-14-objetos-pensam-e-tomam-decisoes %}).
Ele não é apenas um [dado]({% post_url 2016-11-07-pensando-em-dados %}).

Por exemplo. Para exibição de um *GUID* para o usuário basta
utilizar o Método `AsString`.
Se eu quiser exibir apenas o "formato de digitação" — os 8
primeiros caracteres — basta utilizar o Método `AsShortString`.
Se eu quiser o próprio `TGuid` basta chamar `Value`.

Seria mais simples ficar fazendo conversões por todo o código
utilizando funções auxiliares ou utilizar um Objeto desses?

Acho que não há dúvidas aqui.

## Conclusão {#conclusao}

Quando você implementa Objetos da forma correta, o código
é simplificado.

Essa ideia vai contra o pensamento da maioria dos programadores
procedurais que acham que utilizando tipos primitivos e funções
deixaria o código mais simples de entender e alterar.

Ledo engano.

É claro que podemos e devemos utilizar os tipos primitivos, mas
utilíze-os apenas como argumentos de Métodos privados ou em 
variáveis locais.

Para a comunicação entre Objetos, utilize Objetos.

Até logo.