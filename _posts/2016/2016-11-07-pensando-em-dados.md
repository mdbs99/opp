---
layout: post
title: "Pensando em Dados"
date: 2016-11-07
description:
  Não implemente Classes que são apenas recipientes de dados.
image: /images/photo-1460925895917-afdab827c52f.jpg
categories: 
  - Objetos
tags:
  - objetos
keywords:
  - data
  - object
  - data-object
  - report
  - relatório
  - report-data
--- 

Você precisa fazer um relatório financeiro num sistema que 
possui o código Orientado a Objetos.

Quais Classes e Objetos você precisa criar para representar os dados 
que serão exibidos no relatório?

<!--more-->

![Unsplash image]({{ page.image }})

Nenhuma!

Nenhuma Classe que [representa dados]({% post_url 2016-08-01-classes-de-dados %})
é necessária. Use apenas... *dados*.

Muitos programadores (ainda) confundem conceitos tão distintos.
Dados e Objetos são coisas completamente diferentes.

**Enquanto Objetos são criaturas vivas.** Dados são apenas registros do passado.

Relatórios tabulares, por exemplo, consistem apenas de exibição e formatação de 
dados no formato de tabela.
Não precisamos de Objetos, comportamento ou
[encapsulamento]({% post_url 2016-05-30-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-2 %}#encapsulamento)
para fazer essa tarefa — a não ser para obter os dados no lugar onde estão armazenados.

Objetos, no entanto, podem representar dados na forma de uma
[Entidade]({% post_url 2016-02-29-objetos-representam-entidades %})
que encapsula os dados. Poderíamos ter uma Classe `TFinanceReport`, 
por exemplo, que representa um 
[relatório]({% post_url 2016-02-22-datamodule-e-apenas-um-container %}#implementao-de-um-relatrio),
porém onde estariam os dados? Em atributos? Não, não especificamente.
 
Imagine ter um atributo para cada coluna. Listas de objetos. Listas de
listas...

No fim estaríamos trabalhando com *objetos anêmicos* que só possuem 
[Métodos *Getters* e *Setters*]({% post_url 2016-06-27-getters-e-setters %}).

Isso não seria nada eficaz devido ao grande trabalho que iria ser codificar
essas Classes — muitas delas — e fazer a manipulação de seus Objetos. A performance,
também, seria drasticamente afetada.

Além disso essa abordagem é totalmente contra os princípios
da verdadeira [Orientação a Objetos]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}).

**Mas não é assim que codificamos no *Object Pascal*, certo?**

Estou ciente de que utilizar Objetos e listas de Objetos para exibição de
relatórios não é um padrão muito utilizado no mundo *Object Pascal*.
Vemos isso mais no mundo Java ou C#. Nesses ambientes muitos programadores acham
que por estarem utilizando Objetos, eles estariam programando Orientado a Objetos,
o que não é, necessariamente, uma verdade.

Programadores *Object Pascal* — maioria — são orientados a Dados e não a Objetos.
Utilizam a abordagem RAD. Dropam componentes nos formulários, fazem algumas
ligações e isso é o suficiente para a exibição dos dados em uma *Grid*.

Eu sou contra a abordagem RAD para [Regras de Negócio]({% post_url 2017-01-09-codigo-duplicado-talvez-nao %}#regras-de-negocio) ou para qualquer módulo
que exija alguma inteligência, onde o encapsulamento e uso de Objetos iria me
proporcionar um maior *controle* e *manutenibilidade*.

Porém sou completamente a favor da abordagem RAD se estivermos falando sobre 
exibição de dados.

Não tem nada de errado em utilizarmos *queries* para obter os dados de um 
SGBD e exibir o resultado diretamente na tela ou relatório.

**Dados são apenas registros.** Estejam eles gravados em arquivos, Banco de Dados, em *streams*
em memória ou provenientes de uma requisição HTTP via Web, não importa. São
dados, *não* Objetos.

Portanto não trate Objetos como um
[balde de dados]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}#objeto-nao-e-um-balde-de-funcoes-e-dados)
e não trate os Dados como se fossem Objetos que
[pensam]({% post_url 2016-03-14-objetos-pensam-e-tomam-decisoes%}) e tomam decisões.

Até logo.