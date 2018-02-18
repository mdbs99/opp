---
layout: post
title: Declarando Unidades
date: 2017-07-17
permalink: /:title
description:
  Determinar uma ordem para declarar as Unidades no código poderá facilitar a manutenção do código no futuro.
image: /images/2017/photo-annie-spratt-303942.jpg
tags:
  - pascal
  - language
  - unit
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
  - unit
  - using
---

Determinar uma ordem para declarar as Unidades no código poderá facilitar a manutenção do código no futuro.

<!--more-->

![Unsplash image]({{ page.image }})

## Introdução {#introducao}

Quando organizamos nossas coisas, temos a tendência de manter mais perto todos os objetos que são mais utilizados no dia-a-dia, certo?

Assim também deveria ser com as [Unidades](http://castle-engine.io/modern_pascal_introduction.html#_units) de um Projeto.

A ordem em que aparecem as Unidades no código pode facilitar ou dificultar a manutenção.

Nesse artigo irei lhe mostrar qual a ordem que utilizo ao declarar as Unidades nos meus projetos.

## Compilador {#compilador}

As Unidades, também conhecidas como módulos, compõe os projetos.

Todo projeto deve ser dividido em Unidades lógicas e fazer essa divisão nem sempre é fácil, pois envolve um nível de pensamento abstrato e lógico.

Após dividir as Unidades do projeto, também temos que nos preocupar com as Unidades de terceiros.

Hoje em dia é quase mandatório utilizarmos *libs* e *frameworks* de terceiros. Na maioria das vezes são projetos *OpenSource* como, por exemplo, o [James](https://github.com/mdbs99/james).

Tais Unidades facilitam o nosso trabalho, pois já possuem artefatos prontos que agilizam muito a conclusão do projeto.

Além dessas Unidades de terceiros, também temos as Unidades padrão da linguagem localizadas na *Run-time Library* (RTL) ou VCL/LCL, possuindo muitas interfaces, classes, funções e artefatos prontos para uso.

Então, qual ordem devemos declarar essas Unidades quando utilizamos em uma das nossas próprias Unidades?

Aqui está a ordem que eu proponho:

    unit MyUnit;
    uses
      // 1. fpc/lazarus/delphi units,
      // 2. 3rd units,
      // 3. my open source units,
      // 4. project units
      
Quando o compilador começa a fazer o *parser* do código do seu projeto e ele chega na `MyUnit`, ele irá ler as Unidades na sequência na qual elas foram declaradas — pode haver alguma otimização, como leitura paralela, porém o que importa é o resultado final, no qual depende da ordem acima — e os identificadores (classes, interfaces, funções/procedimentos, variáveis e constantes) serão "armazenados em ordem" na qual eles foram declarados.

Por isso eu proponho declarar, inicialmente, todas as Unidades que são padrão ao ambiente. Depois, todas as Unidades de terceiros como *libs* e *frameworks*. Então, todas as Unidades referentes a seus projetos públicos, sejam eles *OpenSource* ou compartilhados dentro de setores de uma empresa. Finalmente, declaramos as Unidades do projeto em si, ou seja, no projeto no qual estamos trabalhando no momento.

E por quê isso é importante?

O motivo é porque o compilador precisa saber em qual Unidade um determinado artefato (classe, função, etc) está localizado.

A regra é a seguinte:

Se você tem 3 Unidades (`Unit1`, `Unit2` e `Unit3`) e, em cada uma delas, tem uma classe com o nome `TFoo`, o compilador irá "informar" ao seu código que `TFoo` está localizada na última Unidade declarada.

Exemplo:

    uses
      Unit1, Unit3, Unit2;

Reparem que eu inverti a ordem numérica. Primeiro é a 1, depois 3 e por último a 2.

Onde está localizada a classe `TFoo` quando você utilizá-la na sua Unidade, a `MyUnit`? 

A resposta é: Na `Unit2`.

Motivo: Ela foi declarada por *último*.

Imagine que o compilador vai montando, em memória, uma tabela de referência para todos os identificadores que ele encontra. Primeiro ele encontra `TFoo` quando lê a `Unit1`, depois encontra novamente quando lê a `Unit3`. Nesse momento ele *substitui* a referência que dizia que `TFoo` está na `Unit1`, pois agora está na `Unit3`. Por fim, quando ele lê a `Unit2` o identificador é reposicionado novamente.

Se nessa mesma `MyUnit` eu precisar utilizar `TFoo` da `Unit1`, eu sou obrigado a qualificar a classe com o nome da Unidade ao invés de apenas utilizar o identificador:

    Foo := Unit1.TFoo.Create...

Vocês podem ver outro exemplo [aqui](http://castle-engine.io/modern_pascal_introduction.html#_qualifying_identifiers_with_unit_name).
    
## Seções {#secoes}

Na linguagem *Object Pascal* temos dois lugares (seções) onde podemos declarar as Unidades que serão utilizadas.

Podemos declarar na seção de interface ou na seção de implementação.

Vale a pena declarar Unidades na seção de implementação?

Na minha opinião sincera, não.

Ao fazer isso, você terá 2 lugares para organizar a precedência da Unidades, ou seja, terá mais trabalho.

Como a linguagem *Object Pascal* não aceita [referência circular]({% post_url 2016-02-01-interfaces-e-a-referencia-circular-entre-objetos %}) entre Unidades, o motivo de haver opção para declarar Unidades na implementação é para permitir essa referência bi-direcional entre elas. No entanto, isso também indica um mal *design* na divisão das Unidades do projeto.

## Conclusão {#conclusao}

Definir a ordem de declaração das Unidades em cada Unidade de seu projeto pode ser de extrema importância para facilitar a manutenção e organização.

Conforme você for adicionando novos identificadores, você terá a certeza que o compilador dará "prioridade" para uso dos identificadores do seu projeto, sem precisar prefixar com o nome da Unidade.

Caso você adicione algum identificador e este entre em conflito com outra Unidade que possui o mesmo nome, basta prefixar esse identificador com o nome da Unidade, seja ela de terceiros ou do próprio ambiente. Entretanto, esse conflito não existirá nos identificadores das Unidades do seu projeto.

Portanto deixe mais "próximo" do código as Unidades que pertencem ao projeto, ou seja, as Unidades que são mais utilizadas e mais "distantes" as Unidades auxiliares.

Até logo.
