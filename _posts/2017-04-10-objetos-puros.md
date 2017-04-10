---
layout: post
title: Objetos Puros
date: 2017-04-10
description:
  Da mesma forma que temos as Funções Puras no Paradigma Funcional, podemos ter os Objetos Puros no Paradigma Orientado a Objetos.
image: /images/photo-florian-klauer-489.jpg
categories: 
  - OOP
tags:
  - conceito
keywords:
  - freepascal
  - fpc
  - delphi
  - lazarus
  - c#
  - csharp
  - java
  - object-oriented
  - oop
  - mdbs99
  - pure-object
  - objetos puros
---

Mesmo que você tenha utilizado linguagens imperativas por toda a sua vida, você já deve ter ouvido falar em linguagens funcionais, assim como o termo *Funções Puras*.

E sobre *Objetos Puros*, você já ouviu algo a respeito?

<!--more-->

![Unsplash image]({{ page.image }})  

## Introdução {#introducao}

Se você não conhece o termo [Funções Puras](https://en.wikipedia.org/wiki/Pure_function) ou linguagens funcionais, bem, você deveria.

A verdadeira [Orientação a Objetos]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}) tem muito em comum com a Programação Funcional.

Por exemplo:

A Programação Funcional é declarativa. Você não deve dizer ao compilador o que fazer linha-a-linha. Assim como na Orientação a Objetos.

A Programação Funcional trabalha com estruturas [imutáveis]({% post_url 2016-07-04-objetos-imutaveis %}), que é um ótimo conceito para simplificar a interperabilidade entre Objetos na Programação Orientada a Objetos.

A Programação Funcional também tem o conceito de Funções Puras ou Impuras.

Será que esse mesmo conceito tem algo em comum com a Orientação a Objetos?

## Funções Puras {#funcoes-puras}

Segundo a [Wikipedia](https://en.wikipedia.org/wiki/Pure_function), para uma função ser considerada *Pura*, as seguintes afirmações abaixo devem ser verdadeiras (tradução livre):

1. A função sempre avalia o mesmo valor de resultado dado o mesmo valor do(s) argumento(s). O valor do resultado da função não pode depender de nenhuma informação oculta ou estado que possa mudar enquanto a execução do programa prossegue ou entre diferentes execuções do programa, nem pode depender de qualquer entrada externa dos dispositivos de E/S.
2. A avaliação do resultado não causa qualquer efeito ou saída secundária observável semanticamente, como a mutação de objetos mutáveis ​​ou a saída para dispositivos de E/S.

Se ambas as afirmações não forem verdadeiras, a função é considerada *Impura*.

Eu acho que essas afirmações são cabíveis na Orientação a Objetos, pois é bem possível criar um Objeto que cumpre ambas as afirmações.

Mas Objetos podem se mais complexos que funções. Os Objetos tem *estado* encapsulado. Podem ser mutáveis ou imutáveis.

Então, é possível considerarmos um Objeto como puro?

## Objetos Puros {#objetos-puros}

Eu não encontrei esse termo na Web então, será que posso dizer que eu cunhei o termo?

Na verdade eu não me importo, visto que não é uma ideia 100% original. Eu me inspirei nas Funções Puras e apenas ampliei o conceito para a Programação Orientada a Objetos.

Em primeiro lugar, um Objeto Puro deve ter as mesmas características que uma Função Pura.

Um Objeto Puro também não deve criar outro Objeto internamente.

Em outras palavras, para um Objeto ser considerado Puro, toda dependência deve ser injetada pelo [construtor]({% post_url 2016-03-21-construtores-da-classe-primario-secundarios %}) da Classe.

Vamos ver alguns exemplos.

#### Exemplo 1 — Objeto Impuro {#exemplo-1}

Abaixo temos um exemplo de uma Classe `TBMWCar` que implementa a Interface `ICar`.

Então instanciamos um `Car` e depois mostramos na tela o valor do sensor de velocidade do motor — acho que tem sensores mais antigos que calculam a partir das rodas, mas isso é irrelevante.

    var
      Car: ICar;
    begin
      Car := TBMWCar.New;
      Print(Car.Engine.Sensors['speed'].Value);
    end;

Eu diria que essa Classe `TBMWCar` é impura.

O motivo é que temos um método `Engine` (Método ou Propriedade, dá no mesmo) que retorna um Objeto interno que [representa]({% post_url 2016-02-29-objetos-representam-entidades %}) o motor do carro.

Como essa instância de motor foi criada?

Olhando apenas o exemplo, podemos concluir que a instância foi criada no construtor da Classe ou mesmo diretamente na chamada do Método `Engine`.

E sobre os `Sensors`? Também não sabemos como foram criados.

Não temos o controle do que será instanciado dentro da Classe `TBMWCar` se quisermos utilizá-la.

O Objeto tem o controle e isso está *correto*.

Você compra um carro e, normalmente, apenas utiliza sua interface (volante, marcha, pedais, painel, etc). Todo ele já vem montado e perfeito.

Talvez você não saiba o tipo de motor, tecnologia ou fornecedores que fabricaram suas peças.

O carro apenas existe, pronto para sua função.

Mas... Isso retrata a vida real ou estamos pulando algumas etapas?

Na verdade, antes do carro chegar até suas mãos, ele foi *montado na fábrica* utilizando centenas ou milhares de peças.

Quando olhamos para o carro montado, concluído, perfeito, não pensamos nessa etapa de montagem, queremos apenas utilizá-lo. Não queremos *montar* o carro toda vez que tivermos que utilizá-lo, certo?

Ele já deve vir concluído e pronto para uso.

O problema dessa abordagem, no entanto, é que não podemos *testar* o carro — a Classe `TBMWCar` — apropriadamente ou isoladamente, utilizando *Unit Tests*, sem que o "motor original" seja "ativado".

Para fazer esses testes, temos que voltar algumas etapas para ser possível desmontar ou montar o carro com "outras peças".

Temos que voltar à "fábrica" para podermos escolher quais peças irão compor o carro. 

#### Exemplo 2 — Objeto Puro {#exemplo-2}

Vamos refazer o exemplo anterior, *injetando* o motor que quisermos quando formos criar a instância do nosso BMW.

    Car := 
      TBMWCar.New(
        TV8Engine.New(
          // more arguments
        )
      );

No código atualizado, sabemos que tipo de motor está sendo utilizado. Trata-se de um modelo V8. Legal!

Estamos utilizando [Injeção de Dependência](https://en.wikipedia.org/wiki/Dependency_injection) aqui.

Essa técnica, em conjunto com a Composição de Objetos, é crucial para um desenvolvimento elegante e eficaz na Programação Orientada a Objetos.

E por quê é "melhor" fazer dessa forma?

O motivo é que agora podemos testar o carro sem "ligar o motor original" se assim o desejarmos. Vejamos:

    Car := 
      TBMWCar.New(
        TFakeEngine.New
      );

Podemos testar o carro, mas ele estará utilizando um "motor falso" ou pense em sensores falsos (utilize sua imaginação).

Esse Objeto é *puro*.

Ele não cria nada internamente. Todas as suas dependências são injetadas através do construtor da Classe.
      
## Conclusão {#conclusao}

Para um Objeto ser considerado puro ele deve ter o comportamento de uma Função Pura e também não deve criar nada internamente.

Da mesma forma que temos as Funções Puras no Paradigma Funcional, podemos ter os Objetos Puros no Paradigma Orientado a Objetos. Para isto, precisamos que estes Objetos sejam imutáveis e que as suas dependências sejam injetadas explicitamente, passadas por meio do construtor do Objeto.

A vantagem de termos Objetos Puros é que eles tem baixo acoplamento e por isso são mais fáceis de serem testados.

Mas se tudo são Objetos, onde as instâncias serão criadas? 

Em apenas 2 lugares: Dentro de Construtores [Secundários]({% post_url 2016-03-21-construtores-da-classe-primario-secundarios %}) ou através de Métodos de Objetos Impuros.

Mas esse é um assunto para outro artigo.

Até logo.