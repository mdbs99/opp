---
layout: post
title: "Tipo object Continua Vivo"
date: 2019-03-18
permalink: /:title
description:
  Existe uma estrutura no Object Pascal que foi o precursor das classes.
image: /images/2019/namroud-gorguis-253765-unsplash.jpg
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
  - object
---

Existe uma estrutura no Object Pascal que foi o precursor das classes.

Você conhece o tipo `object`?

<!--more-->

![Unsplash image]({{ page.image }})
<br><span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Namroud Gorguis on Unsplash</span>

Em um [artigo]({% post_url 2018-12-17-formularios-e-widgets %}) anterior eu expliquei com dividir o código de um formulário em diferentes objetos, organizando os widgets em diferentes classes para dividir as responsabilidades em diferentes objetos.

Separar partes do código em classes distintas melhoram a legibilidade e manutenção do código. Mas você pode implementar o mesmo conceito sem utilizar classes.

Muitas vezes eu utilizo o tipo `object` como um *helper* para um widget ou conjunto deles.

De fato, essa técnica pode ser utilizada não só na implementação de formulários, mas em *qualquer* parte do código.

Não confunda com os tipos Helpers que *adiciona* comportamento a tipos já existentes. Existe uma sintaxe especial para declara-los, estendendo outros tipos sem utilizar herança... mas não é deles que estou falando.

Se você não gosta de nomenclatura *helper*, poderíamos chamá-los de *ferramentas*, pois eles [não representam]({% post_url 2016-02-29-objetos-representam-entidades %}) uma entidade e não implementam uma [interface]({% post_url 2016-01-18-interfaces-em-todo-lugar %}). São apenas ferramentas que serão utilizadas pelos verdadeiros objetos.

Não representar uma entidade é um bom motivo para utilizar o tipo `object`, mas há outras vantagens.

**Performance**, por utilizar o stack ou invés do heap na alocação de memória.

**Utiliza menos memória**, pois são estruturas mais simples, comparadas às classes.

**Uso de herança** é possível.

**Determinar um escopo** para um conjunto de funções utilitárias, genéricas e reutilizáveis, ao invés de declarar funções e procedimentos diretamente no escopo da unit.

**Simplicidade**, sempre será um ótimo motivo para tomar decisões na arquitetura. Por exemplo: não é necessário o uso de `try..finally`, tornando o código menos verboso.

Uma **desvantagem**, entretanto, é não existir um [construtor] e por isso não tem como garantir uma inicialização adequada dos campos privados. Diferentemente de uma classe, onde é *obrigatório* o uso de seu construtor, um tipo `object` pode ser utilizado diretamente como qualquer outra variável primitiva. 

Além disso, a Embarcadero descontinuou o uso do tipo `object` desde o Delphi 2010, sugerindo o uso de "records com métodos" em seu lugar—uma decisão equivocada, na minha opinião, e [não estou sozinho](http://blog.synopse.info/post/2013/10/09/Good-old-object-is-not-to-be-deprecated-it-is-the-future) nesse pensamento. Além de perder a compatibilidade com o código legado, introduzir métodos em records é, no mínimo, duvidoso.

Já tínhamos o tipo `object` e ninguém solicitou pela sua descontinuidade, [por quê não mantê-lo](http://blog.synopse.info/post/2010/08/06/Save-object%2C-stop-class-hegemony%21)?

E para aqueles que não conhecem a sintaxe do seu uso, aqui está um simples exemplo:

    type
      TMsg = object
        procedure Show(const aText: string);
      end;
    {...}
    var
      msg: TMsg;
    begin
      msg.Show;
    end;

Simples, direto, sem `try..finally`.

Felizmente o tipo `object` continua vivo no Free Pascal.

Até logo.
