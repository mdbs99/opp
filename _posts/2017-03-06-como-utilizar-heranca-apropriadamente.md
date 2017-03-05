---
layout: post
title: Como Utilizar Herança Apropriadamente
date: 2017-03-05
description:
  Sempre favoreça a Composição de Objetos ao invés de Herança de Classe, mas se você ainda quiser ou precisar utilizar a Herança, sabia como utilizá-la apropriadamente.
image: /images/photo-joanna-kosinska-37665.jpg
categories: 
  - OO
tags:
  - herança
keywords:
  - herança
  - inheritance
  - orientação a objetos
  - oop
  - poo
  - encapsulamento
  - polimorfismo
  - delphi
  - freepascal
  - lazarus
  - c#
  - csharp
  - java
---

Sempre favoreça a Composição de Objetos ao invés de Herança de Classe, mas se você ainda quiser ou precisar utilizar a Herança, sabia como utilizá-la apropriadamente.

<!--more-->

![Unsplash image]({{ page.image }})  

## Introdução {#introducao}

Eu já escrevi uma [série de 5 partes]({% post_url 2016-05-23-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-1 %}) aqui no blog sobre como a Herança pode ser o mal da Orientação a Objetos.

Meu entendimento sobre esse assunto continua o mesmo: Herança de Classe deve ser *evitada*.

Então qual o motivo para escrever artigo?

A Herança de Classe é uma ferramenta e, como tal, está disponível para ser utilizada. Se você conseguir fazer isso apropriadamente, poderia obter algum benefício prático e, ao mesmo tempo, minimizar os [males]({% post_url 2016-05-23-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-1 %}#os-males) que a Herança de Classe traz consigo.

Então compilei algumas *regras* que, na minha opinião, são essenciais para o uso da Herança de Classe.

## Não utilize Herança (apenas) para Reaproveitamento de Código {#reaproveitamento-de-codigo}

A maioria dos desenvolvedores utilizam Herança pensando em Reaproveitamento de Código. Esse é o início de todos os outros problemas relacionados à Herança de Classe.

Herança de Classe não foi projetada para Reaproveitamento de Código — isso é apenas um *efeito colateral* — mas sim para construir hierarquias do tipo **é-um**. Um gato *é um* animal; cachorro *é um* mamífero; tubarão *é um* peixe; etc.

Assim como os ciêntistas classificam um animal apenas colocando-o numa hierarquia *não* baseada no seu comportamento, mas sim nas suas características físicas, na Orientação a Objetos isso também pode ser feito utilizando Interfaces com um mínimo de esforço na manutenção do código. Por exemplo, se um animal fosse retirado ou movido para outro ponto na hierarquia, bastaria retirar ou reescrever apenas algumas assinaturas de métodos.

O problema está nas Classes. Especialmente naquelas Classes abstratas e/ou métodos abstratos que são codificadas apenas para reutilizar código.

> Quando você estiver projetando seu sistema, é o Domínio que irá sugerir quais Classes deverão ser implementadas.

Se o software que você está desenvolvendo é sobre venda de Carros e Motos, não pense em codificar uma Classe `TAbstractVehicle` para compartilhar código entre `TCarro` e `TMoto`. Se essa abstração não existe no Domínio, você não deveria criá-la.

Mas se no Domínio já existir o conceito de hierarquia como, por exemplo, *Carro Alemão*, *Carro Francês* e *Carro Americano*, então talvez você possa definir uma Hierarquia de Classes onde os carros da marca BMW e Audi **é-um** *Carro Alemão*.

Não porque você *precisa* compartilhar código, mas sim porque seu Domínio *exige* que assim seja, por algum motivo.

Então, antes de implementar alguma hierarquia, verifique se está de acordo com o seu Domínio.

## Classes de Domínio não podem Herdar de Classes de Suporte {#classes-de-suporte}

Um dos grandes erros que você desenvolvedor pode cometer é herdar suas *Classes de Domínio* de *Classes de Suporte*. Estou utilizando essa nomenclatura (Suporte) para todas as Classes *built-in*, ou seja, Classes disponíveis nas *libs* do Compilador (VCL/LCL ou RTL) ou mesmo *libs* de terceiros.

Se o Domínio lhe diz que é necessário uma "Lista de Veículos", por exemplo, o que fazem a maioria dos desenvolvedores?

  1. Procuram uma Classe de Suporte que implementa uma Lista para poderem utilizar herança ou ;
  2. Procuram uma Classe de Suporte que implementa uma Lista utilizando *Generics*.

A segunda opção parece ser a mais "sofisticada" hoje em dia — todos amam *generics*, não é? — mas lhe digo que ambas as opções estão erradas.

Uma "Lista de Veículos" *é-um* `TList`, `TStringList`, `TInterfaceList` ou `sua-classe-de-lista-preferida-aqui`?

Não necessariamente.

As Classes do Domínio não podem ser "contaminadas" pelas Classes de Suporte através da Herança de Classe.

As Classes de Suporte devem ser utilizadas na Composição de Objetos. Em outras palavras, você implementa sua `TVehicleList` e, internamente, utiliza alguma Classe de Suporte que implementa o conceito de lista — nem é necessário o uso de *Generics*.

Seu Domínio não pode depender de implementações de Classes de Suporte de outras *libs*.

Então, não utilize Herança a partir de Classes de Suporte.

## Não pode haver Herança de Classes entre Domínios distintos {#dominios-distintos}

Suas Classes de Domínio e Classes de Suporte pertencem a Domínios ou [Contextos]({% post_url 2016-10-03-delegacao-de-implementacao-de-interfaces %}#contextos) distintos. Somente isso já seria um forte motivo para não utilizar herança entre essas Classes.

A explicação é simples: Um objeto (A) não é, necessariamente, do mesmo *tipo* que um objeto (B) apenas porque eles são *similares*.

Um cachorro é similar a um lobo — pelo menos pra mim — no entanto podemos afirmar que ambos descendem de uma única Classe? Caso a resposta seja sim, alterar qualquer coisa nessa Classe Ancestral iria alterar o comportamento de duas criaturas que são similares na *aparência* porém bem diferentes, em muitos aspectos, no *comportamento*.

Domínios e Contextos distintos podem e devem evoluir independentemente.

Então, não utilize Herança entre Domínios distintos. Se deseja reutilização de Código é melhor utilizar Composição de Objetos ao invés de Herança de Classe.

## Não utilize Atributos ou Métodos Protegidos Apenas para Uso em Subclasses {#metodos-protegidos}

Uma das "vantagens" da Herança de Classe é implementarmos Atributos e Métodos protegidos — muitas vezes eles são abstratos — para serem utilizados ou sobrescritos nas Subclasses.

Não faça isso.

É o mesmo que programar procedural, mas utilizando Classes.

Se você codifica uma Classe abstrata apenas para codificar atributos (variáveis globais?) e métodos abstratos ("pontos de execução"?) para Subclasses é o mesmo que criar Procedimentos que tem outros  Procedimentos (ponteiros) como argumentos. Isso não é Orientação a Objetos.

As Classes devem ser *sólidas*. A implementação delas não deve estar "fatiada" entre outras "Classes" (procedimentos?).

A Herança de Classe faz isso, ou seja, divide o comportamento em várias Classes. Porém, se a Herança for bem feita, uma Classe Ancestral terá comportamento independentemente se há ou não Subclasses para implementar ou sobrescrever quaisquer metódos.

Então, não implemente Classes que dependam de Subclasses para ter algum comportamento útil.

## Sempre Adicione, nunca Altere ou Remova Funcionalidades {#sempre-adicione}

Se você costuma sobrescrever Métodos de Classes ancestrais para "desativá-los" (removendo a implementação) ou não chama a execução do Método Ancestral (herdado) *antes* ou *depois* de adicionar comportamento, é bem provável que sua hierarquia de Classes está *errada*.

Se sua Subclasse deveria ter um comportamento diferente da Classe Ancestral, a ponto de você querer reimplementar todo o Método ou desativá-lo, então esse comportamento não deveria estar na Classe Ancestral ou a Herança entre essas Classes não deveria existir.

Com a Herança de Classe você quer herdar todos os contratos (Interfaces) que a Classe implementa assim como todo o seu comportamento. Não faz sentido, então, querer eliminar ou refazer alguma coisa. É um desperdício.

Um pato pode ter duas pernas, porém ele não *corre* como um ser humano. Não faz sentido ambos terem uma Classe em comum apenas para reaproveitar 2 pernas, mas mudar o comportamento quando ambos estão correndo.

Então, pense na Hierarquia de Classes para determinar tipos em comum, não comportamento em comum. 

## Conclusão {#conclusao}

A Orientação a Objetos implementa o conceito de Composição de Objetos que é o suficiente para implementar Objetos simples, sólidos e que implementam apenas uma única [responsabilidade]({% post_url 2016-03-07-classes-devem-implementar-apenas-uma-responsabilidade %}).

Mas ela também nos dá outra ferramenta: A Herança de Classe.

Você pode ter uma melhor arquitura substituindo Herança por [Decoradores]({% post_url 2016-05-02-decorator-pattern %}), com a única desvantagem de escrever mais.

Vale a pena? Depende.

Pode ter casos que a Herança facilita ou até mesmo é necessária? Talvez.

Resumindo: A melhor ferramenta vai depender do tipo de trabalho a ser feito.

No meu carro, os problemas do dia-a-dia eu resolvo com um par de chaves-de-fenda, um alicate, Silver-tape e WD. Mas tem problemas que só podem ser resolvidos com ferramentas espefícias. Entende?

Até logo.