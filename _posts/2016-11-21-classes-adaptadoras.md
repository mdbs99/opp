---
layout: post
title: "Classes Adaptadoras"
date: 2016-11-20
description:
  Classes Adaptadoras são responsáveis...............................
image: /images/photo-1428954376791-d9ae785dfb2d.jpg
categories: 
  - Objetos
tags:
  - adaptadores
  - about
keywords:
  - data
  - about
  - apdater class
  - classes adaptadoras
--- 

Os dados sempre deverão estar encapsulados em Objetos que conversam
entre si enviando mensagens uns aos outros. No entanto essas mensagens
podem conter dados no formato que só o Objeto emissor conhece.

Como o Objeto receptor irá saber ler esses dados que, outrora, estavam
encapsulados no Objeto emissor?

<!--more-->

![Unsplash image]({{ page.image }})

## Indrodução {#introducao}

Cada Objeto detém o conhecimento sobre seus próprios dados encapsulados.
Ninguém mais.

Para um sistema funcionar esses Objetos devem conversar entre si, enviando
[mensagens]({% post_url 2016-11-14-diga-me-algo-sobre-voce %}#mensagens)
uns aos outros.

Sabemos o que fazer quando queremos saber informações sobre um determinado
Objeto. Basta utilizar seu
[Método *About*]({% post_url 2016-11-14-diga-me-algo-sobre-voce %}#metodo-about).

O problema é que você, programador, sabe o que fazer com esses dados pois
você detém o conhecimento sobre todas as Classes e Objetos. No entanto, são
os Objetos que devem [conversar]({% post_url 2016-03-14-objetos-pensam-e-tomam-decisoes %})
entre eles mesmos. Você está *fora* dessa conversa.

Então deve haver uma maneira do Objeto emissor conversar com o Objeto receptor,
utilizando um mesmo dialeto, sem que você faça conversões explícitas para que
essa conversa aconteça.

Para isso existem as Classes Adaptadoras.

## Dados para Formulários {#dados-para-formularios}

Sempre que utilizamos formulários — ou qualquer tipo de *view* — é porque
queremos exibir alguma informação para que o usuário tome alguma decisão ou 
execute alguma coisa.

Esses formulários podem obter seus dados quando são criados ou posteriormente,
devido a alguma ação do usuário.

Vamos nos concentrar nesses dados inicias.

Imagine que, num sistema Financeiro, há dois formulários:

  1. Faturas a Pagar
  2. Opções de Pagamento
  
No formulário de Faturas a Pagar o usuário irá selecionar as faturas, clicar
em algum botão para pagar e, nesse momento, o outro formulário com as Opções
de Pagamento será exibido.

O que o formulário de Faturas a Pagar deveria enviar para o formulário de 
Opções de Pagamento no seu
[construtor]({% post_url 2016-03-21-construtores-da-classe-primario-secundarios %})?

Bem, isso depende.

Antes precisamos definir nesse pequeno exemplo, o que é uma Fatura a Pagar,
assim como o que são Opções de Pagamento.

Vamos nos concentrar nos dados:

  1. Uma Fatura encapsula um número sequencial, um valor a pagar original, um valor
  a pagar atual (com multa e juros), uma data, uma descrição, itens...
  vamos parar aqui.

  2. Uma Opção de Pagamento encapsula opções (dinheiro, cartão de crédito, etc),
  valor para cada opção, valor total a pagar e... tudo bem, já está bom.

## Oções de Implementação {#opcoes-de-implementacao}

Agora que temos algumas definições, podemos pensar no modelo que iremos
seguir para determinar como será feita a implementação.

Vamos ver as opções.
  
### 1- Banco de dados e registros {#banco-de-dados-e-registros}
  
Não pense em [dados]({% post_url 2016-11-07-pensando-em-dados %}) provenientes de
um banco de dados, *queries* ou *DataSet*. Os dados já estão no formulário, numa 
*grid* listando todas as faturas.

Você já tem tudo que precisa nos *widgets* do formulário.

E agora, ficou mais fácil?

Ainda não.

Se toda Entidade deve ser
[representada]({% post_url 2016-02-29-objetos-representam-entidades %})
por um Objeto, você já sabe que não são apenas 2 Objetos (formulários) que irão
fazer parte dessa conversa.

Mas lembre-se que ainda estamos falando de dados.

### 2- Objetos anêmicos e listas {#objetos-anemicos-e-listas}

Não pense em criar Classes Anêmicas
como `TInvoice` com todos os campos acima, utilizando 
[Getters e Setters]({% post_url 2016-06-27-getters-e-setters %})
e uma lista `TInvoices` para passar ao formulário de Opções de Pagamento.

Pensou nisso mesmo?

Então vamos adicionar um complicador:

  * O formulário de Opções de Pagamento é utilizado
  para pagar *qualquer coisa* dentro do sistema, ou seja, não *apenas* Faturas.

E agora, ainda acha que criar uma lista de Faturas seria a melhor opção?

### 3- Herança e esperança {#heranca-e-esperanca}

Ora, vamos criar uma lista genérica ou Interface, utilizar herança para `TInvoices`
e refatorar o formulário para receber essa lista.

É claro que não.

Já sabemos que
[herança]({% post_url 2016-05-23-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-1 %})
não foi feita para compartilhar código e que ela pode piorar (e muito) a arquitetura do projeto.

Essa seria a pior opção.

### 4- Manualmente {#manual}

Então você, o programador, manipula os dados do formulário de Faturas, criando 
um XML que você sabe que o formulário de Opções de Pagamento espera receber — 
porque você pode ver o código-fonte de todas as Classes — e então repassa 
para o formulário.

É, talvez irá funcionar... mas por quanto tempo?

Sabendo que o formulário de Opções de Pagamento pode ser utilizado por outros módulos,
se houver alguma alteração nos dados que ele espera receber, será muito difícil
procurar em todos esses lugares para fazer alterações. Cada equipe que trabalha em 
módulos distintos — mas que utiliza esse mesmo formulário — criou suas próprias versões
do XML para enviar ao formulário. O código foi duplicado.

### 5- Classes Adaptadoras {#classes-adaptadoras}

Minha sugestão é utilizarmos Classes Adaptadoras.

Elas poderão ser muitas. Vai depender de quantos módulos/classes irão utilizar 
o formulário de Opções de Pagamentos.

Classes Adaptadoras são **conectores**. Elas adaptam as interfaces, adaptam os
[contratos]({% post_url 2016-01-18-interfaces-em-todo-lugar %}#interfaces-sao-contratos).

Elas tornam possível uma *Classe-A* trabalhar em conjunto com a uma *Classe-B*,
mesmo que ambas não se conheçam.

Os programadores só precisam saber que esses conectores existem e onde estão 
guardados, quando eles precisarem.

## Adaptando {#adaptando}



## Conclusão {#conclusao}


Até logo.