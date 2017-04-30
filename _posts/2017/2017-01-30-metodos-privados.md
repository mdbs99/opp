---
layout: post
title: Métodos Privados
date: 2017-01-30
description:
  Existe um pensamento que diz que todo Método Privado deveria ser, na verdade, a implementação de outro Objeto.
image: /images/photo-q6vbepqsojc-rita-morais.jpg
categories: 
  - OOP
tags:
  - oop
  - metodo
keywords:
  - métodos privados
  - private methods
  - orientacao a objetos
  - object-oriented
---

Existe um pensamento que diz que todo Método Privado deveria ser, na verdade, a implementação de outro Objeto.

<!--more-->

![Unsplash image]({{ page.image }})  

## Introdução {#introducao}

Dizemos que todo Objeto deve implementar apenas uma única [responsabilidade]({% post_url 2016-03-07-classes-devem-implementar-apenas-uma-responsabilidade %}).

Então a ideia que todo Método Privado deveria ser outro Objeto faz sentido, pelo menos na teoria.

Se o Objeto deveria ter apenas uma única responsabilidade, por que eu teria mais Métodos (privados) além dos Métodos Públicos?

Não seriam os Métodos Públicos suficientes para implementar apenas uma única responsabilidade?

É o que vamos tentar entender nesse artigo.

## Implementando um Objeto {#implementando-um-objeto}

Um Objeto deve ser a [representação]({% post_url 2016-02-29-objetos-representam-entidades %}) de uma Entidade. Mas não a representação total da Entidade. Isso seria quase impossível ou muito complexo.

Temos que abstrair ao máximo, ou seja, implementar apenas o comportamento relevante para o software.

Por isso é possível termos Objetos pequenos, coesos e que implementam apenas uma única responsabilidade.

Vejamos alguns exemplos para determinarmos se há sentido termos Métodos Privados, mesmo em Objetos simples.

#### Exemplo 1 {#exemplo-1}

No código abaixo temos uma Classe que é inicilizada com uma `string` que representa o nome do empregado.

Há uma Método Privado para "limpar" o nome. Essa limpeza consiste em retirar os espaços em branco do início e do fim do nome. Depois há uma verificação se a `string` não ficou em branco no final. Bem simples.

<script src="https://gist.github.com/mdbs99/bbdc509810890ea5724a94a021f16791.js"></script>

Apesar dessa simplicidade aparente, esse código possui vários erros ou violações de princípios.

O primeiro problema é conceitual: Você não deve fazer [validações no construtor]({% post_url 2016-05-09-validacoes-no-construtor %}), pois segundo Alan Kay — inventor do nome/conceito Orientação a Objetos — [aqui](http://userpage.fu-berlin.de/~ram/pub/pub_jf47ht81Ht/doc_kay_oop_en) nesse email, ele diz: *"Programação Orientada a Objetos (para mim) significa apenas mensagens, encapsular e esconder estado, e **extrema ligação tardia de todas as coisas**"*.

Primeiro você cria os Objetos, depois eles interagem. Se você for validando e/ou executando rotinas a medida que vai criando seus Objetos, você não está programando Orientado a Objetos, isso é Programação Procedural.

Você nem deve chamar nenhum Método no construtor, pois esses Métodos podem gerar algum problema e o Objeto pode não ser criado.

O segundo problema é que um Empregado não deveria "limpar o nome recebido". Não faz sentido.

O Nome deveria ser um [Objeto](https://www.youtube.com/watch?v=nia7UqcpOAc) e ele iria validar a si mesmo; retornar uma string válida, sem espaços, formatada, talvez até com o nome do meio abreviado.

Em outras palavras, o Método `CleanName` não deveria existir, mesmo sendo privado.

#### Exemplo 2 {#exemplo-2}

Então para corrigir o exemplo acima, vamos implementar a Classe `TEmployeeName`.

Agora sabemos que essa Classe deve ser responsável por tudo que envolve `Nome`, então vamos incrementar e por alguns Métodos como `FirstName` e `LastName`. Assim esses Métodos poderão ser reutilizados por toda a aplicação e você não precisará ficar fazendo esses algoritmos em partes do código que não tem nada haver com manipulação de `Nome`.

Mas veja que eu introduzi um Método Privado.

Esse Método tem três responsabilidades — portanto não está 100% correto, mas serve de exemplo — que é 1) implementar o algoritmo para obter o `FirstName`, 2) implementar outro algoritmo para obter o `LastName` e 3) também está fazendo *cache* dos dados.

<script src="https://gist.github.com/mdbs99/a303d0bf83930fcf580aee818ee23a52.js"></script>      

Essa Classe é tão simples que não haveria necessidade de existir esse Método Privado, porém imagine que um Objeto dessa Classe seja muito utilizado e precisa de muita performance.

Nesse caso o Método Privado está fazendo um *cache* dos dados, assim não será necessário "calcular" as partes dos nomes sempre que executarmos esses Métodos.

Essa é das ocasições que um Método Privado ajuda e é bem vindo. O Objeto é criado sem interferências, rápido, sem cálculos. Depois, se um de seus Métodos for realmente executado, o Objeto faz as devidas validações e execuções. Para o mundo externo é indiferente, já que ninguém sabe o que acontece dentro de um Objeto.

Se você está curioso para saber como seria a "implementação pura" Orientada a Objetos, teríamos que implementar outra Classe para [decorar]({% post_url 2016-05-02-decorator-pattern %}) a `TEmployeeName` somente para fazermos o *cache* dos dados. Então cada Método de `TEmployeeName` teria a implementação dos cálculos de partes do nome nos próprios Métodos Público — a Classe ficaria com menos código e mais simples.

A Classe decoradora ficaria mais ou menos assim:

<script src="https://gist.github.com/mdbs99/f9f7aa261bd2e317df503f974429560f.js"></script>

Mas você sempre teria que instanciar esses dois Objetos, um decorando o outro.

Se vale ou não a pena criar essa Classe, é você quem decide ou as Regras de Négócio decidem por você. 

Bom é saber que temos opções. Talvez começar a implementação com Métodos Privados e depois, se for o caso, refatorá-los.

Veja que é possível fazer um design melhor, sem Métodos Privados, mas sempre temos que ver os prós e contras.

## Conclusão {#conclusao}

Em teoria um Objeto não deveria ter Métodos Privados, porém vimos que existem casos de natureza tecnológica no qual os Métodos Privados auxiliam a codificação.

Como eu já disse em outro [artigo]({% post_url 2016-05-16-singleton-e-um-anti-padrao %}): *"Não devemos ser puristas em Orientação a Objetos se não há nenhum benefício"* ou se o benefício for apenas estético, talvez.

Então fique atento sobre os Métodos Privados que você está criando. Veja se eles deveriam estar em outro Objeto. Mas não perca tempo buscando a perfeição, pois um Método Privado sempre poderá ser refatorado no futuro sem nenhum problema.

Até logo.