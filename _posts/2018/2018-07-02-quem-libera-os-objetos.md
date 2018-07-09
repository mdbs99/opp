---
layout: post
title: "Quem é o Responsável por Liberar os Objetos?"
date: 2018-07-02
permalink: /:title
description:
  Você consideraria um bom design para seu projeto, ter métodos ou funções que criam objetos que serão liberados em outro local, por outro objeto ou função?
image: /images/2018/w-a-t-a-r-i-535375-unsplash.jpg
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
  - free-and-nil
  - destrutor
---

Você consideraria um bom design para seu projeto, ter métodos ou funções que criam objetos que serão liberados em outro local, por outro objeto ou função?

<!--more-->

![Unsplash image]({{ page.image }})
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by W A T A R I on Unsplash</span>

Em linguagens onde a liberação de memória e recursos é algo manual que depende do desenvolvedor, ter funções ou métodos (vamos nos referir apenas a métodos de agora em diante) que criam novos objetos para que "alguém", em algum lugar, faça a limpeza da memória é uma má ideia.

Ao longo dos anos tenho visto código como esse:

    function TFoo.Build(const Name: string; Value: Integer): TBar;
    begin
      Result := TBar.Create;
      Result.Name := Name;
      Result.Value := Value;
    end;

Uma classe `TFoo` que, através do método `Build`, retorna uma nova instância de `TBar` sem manter uma referência para o objeto criado.

Acredito que esses tipos de métodos são criados com a intenção de "facilitar" a codificação. Ao invés de criar um objeto `TBar` e ter que iniciar suas propriedades em muitos lugares no código, cria-se um método que [encapsula]({% post_url 2018-04-16-encapsulamento %}) a criação do objeto sem ter que [duplicar o código]({% post_url 2017-01-09-codigo-duplicado-talvez-nao %}) em outros lugares.

Parece bom?

Eu considero essa abordagem *péssima*, pois é o início para ocorrer eventuais vazamentos de memória.

A linguagem Object Pascal não obriga o desenvolvedor a armazenar o retorno de um método em alguma variável — assim como Java e C#. Então, se não tivermos uma referência através de um variável, o objeto não seria liberado, gerando um vazamento de memória.

Mas você poderia dizer que não há sentido em criar um objeto e não fazer nada com ele, então o desenvolvedor iria ter uma variável... bem, nem sempre. Mas no caso acima, eu concordo.

Então, digamos que o desenvolvedor armazena o objeto em uma variável.

Infelizmente, isso também não garante que ele irá liberar a memória após seu uso.

OK. Esse não é um problema. Sei que isso é intrínseco ao design da linguagem: O desenvolvedor deve liberar os objetos chamando os destrutores.

No entanto, há uma grande diferença.

Por exemplo: Quando temos objetos que possuem objetos, naturalmente não nos preocupamos quem irá liberar a memória alocada. Sabemos que alguém irá liberá-los. O motivo é simples: *não fomos nós que os criamos*.

Então, temos uma regra aqui:

> Aquele que cria é também o responsável por destruir a instância e/ou liberar o recurso.

Mas, quando utilizamos métodos que retornam objetos, como saber se devemos ou não liberar os recursos manualmente?

Será que o próprio objeto irá fazer o trabalho?

Como o desenvolvedor iria saber?

Só lendo a documentação ou código da classe, o que é um péssimo design de projeto.

Ao voltarmos para o nosso primeiro exemplo, a instância da classe `TFoo` seria, então, responsável por liberar quaisquer objetos que porventura tenha criado para estar de acordo com a regra acima. O que não é o caso.

O que fazer?

Existe alguma técnica que nos permite criar e retornar novos objetos sem ter de nos preocupar (muito) quem irá liberar a memória?

*Sim*, contanto que você utilize [interfaces]({% post_url 2016-01-18-interfaces-em-todo-lugar %}) ao invés de instâncias de classes. Para esses casos eu proponho a técnica do [método estático New]({% post_url 2016-01-10-interfaces-e-o-metodo-estatico-new %}).

Até logo.