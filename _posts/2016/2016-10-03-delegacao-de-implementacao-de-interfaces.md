---
layout: post
title: "Delegação de Implementação de Interfaces"
date: 2016-10-03
description: "A beleza da linguagem Object Pascal para implementar delegação entre Objetos"
summary: "A beleza da linguagem Object Pascal para implementar delegação entre Objetos"
image: /images/photo-1451650645557-62193a7bed6a.jpg
categories: 
  - Object Pascal
tags:
  - Language
keywords:
  - delegation
  - delegates
  - delegação
  - implements
  - object delegation
--- 

Existe uma bela *feature* na linguagem *Object Pascal*
que nem todos sabem que existem. Aqueles que sabem pouco
utilizam e, talvez, não tenham percebido o potencial dessa 
*feature* para a implementação da Orientação a Objetos
numa linguagem de programação. 

<!--more-->

![Imagem]({{ page.image }})

## Introdução {#introducao}

A bela *feature* que existe no *Object Pascal* e que — até onde
eu sei — não existe em nenhuma outra linguagem de programação chama-se
[Delegation](http://docwiki.embarcadero.com/RADStudio/Seattle/en/Using_Implements_for_Delegation).

Java não tem.

Ruby não tem.

C# não tem. 

Os engenheiros da linguagem C# utilizaram a mesma nomenclatura 
para implementar o que nós — Programadores *Object Pascal* — 
damos o nome de Eventos. 
Um ponteiro para um Método é uma implementação para um Evento, 
mas em C# eles deram o nome de
[*delegates*](https://msdn.microsoft.com/library/900fyy8e.aspx).

Você verá porque essa *feature* é tão importante para implementarmos
um Objeto da forma mais fiel possível a 
[Entidade real]({% post_url 2016-02-29-objetos-representam-entidades %})
que ele representa.

Verá que é possível escrever menos e ainda deixar o código mais 
flexível para implementações de **comportamentos dinâmicos**, 
utilizando a composição de Objetos.

## Delegation / Implements

*Delegation* no *Object Pascal* também pode ser conhecido pela
palavra *Implements*.

Ambas as nomenclaturas estão corretas.
Sendo *delegation* a técnica e *implements* a palavra-reservada
que a implementa.

Essa *feature* existe na linguagem *Object Pascal* desde sempre e
seu objetivo é a **delegação de implementação de Interfaces, 
utilizando Composição de Objetos**.

A anos atrás eu só codificava utilizando o 
[paradigma Procedural]({% post_url 2016-09-05-procedural-e-facil-orientado-a-objetos-e-dificil %})
então eu apenas ignorava essa *feature*.

Mas antes de começarmos a utilizar *Delegation*, precisamos pensar
em Contextos.

## Contextos {#contextos}

Já falei muito sobre Contextos em [artigos anteriores](/archive/).
Um *software* deve ser codificado utilizando Contextos. Cada Contexto
é um agrupamento de ideias, Classes, Objetos, Regras de Negócio, etc.

No [DDD](https://en.wikipedia.org/wiki/Domain-driven_design) 
dá-se o nome de 
[Bounded Context](http://martinfowler.com/bliki/BoundedContext.html).

> Bounded Context é um conceito muito importante do DDD e pode ser a 
solução para a boa modelagem do seu domínio. Bounded Context é um 
conceito tão importante quanto o entendimento da separação de 
responsabilidades das camadas do DDD. — Google search

Em termos práticos, isso quer dizer o seguinte:

Imagine que você tem uma Entidade `Client`. Um `Client` teria Regras de
Negócio de acessos mas também teria Regras de Negócio sobre suas Finanças,
considerando um sistema hipotético.

Se cada Classe deve
[representar apenas uma responsabilidade]({% post_url 2016-03-07-classes-devem-implementar-apenas-uma-responsabilidade %}),
como construir um Objeto que A) tenha Regras de Acesso e B) tenha Regras de Finanças ao mesmo tempo?

Por agora, a resposta é: Você **não** deve fazer isso.

Por quê não?

Bem, se você tem uma Interface `IFinances` e outra `IAccess`, por exemplo,
e tivesse que implementar os métodos de ambas numa única Classe, você 
estaria quebrando a regra de **Implementar apenas uma Responsabilidade**.

A Classe ficaria "inchada". Métodos demais. Responsabilidades demais.

Se daqui a algumas semanas tivesse que acrescentar outras regras, ou seja,
implementar outra Interface... imagine onde isso iria parar! 
A manutenção ficaria extremamente comprometida.

A maneira mais eficaz e segura é implementar Classes diferentes para
cada Contexto, mas que representem a mesma Entidade.

Utilizando o exemplo acima teríamos que implementar as Classes:

  1. TClientFinances (IFinances)
  2. TClientAccess (IAccess)
  
Ambas as Classes representam um `Client`, porém em Contextos diferentes,
com Métodos diferentes.

E esse tipo de implementação funciona em qualquer linguagem com suporte a
Orientação a Objetos.

Eu gosto disso. Gosto de ter soluções simples que funcionam em (quase)
qualquer linguagem, sem precisar utilizar *features* específicas de
cada linguagem.

No entanto...

Nem todo Objeto é tão simples. Há casos em que Objetos mais complexos
podem simplificar o código, pois se a complexidade está dentro do Objeto
mas seu uso é simples, então vale a pena, certo?

Então digamos que gostaríamos de ter um Objeto que representasse
`Finances` mas também `Access`, por algum motivo, mas sem quebrar a 
regra da responsabilidade única.

## O Problema {#o-problema}

Vou definir alguns Métodos simples para ambas as Interfaces.

    type
      IFinances = interface
        function Current: Currency; // total no banco
        function AsString: string;
      end;
      
      IAccess = interface
        function List: IDataList; // lista de acessos
        function AsString: string;
      end;

Se tivéssemos uma Classe que implementa ambas as Interfaces, 
teríamos algo como:
      
      TSuperClient = class(TInterfacedObject, IFinances, IAccess)
      public
        function Current: Currency;
        function List: IDataList;
        function IFinances.AsString: string;
        function IAccess.AsString: string;
      end;
      
Veja que com apenas duas Interfaces simples já temos 4 métodos
que não são nada coesos numa única Classe. Ainda tem o fato de 
ambas as Classes possuirem Métodos iguais, então, se for preciso
representar as Interfaces como `String`, é necessário definir um
prefixo com o nome da Interface.

## Como utilizar a Delegação {#como-utilizar-a-delegacao}

Para utilizarmos *Delegation*, primeiro precisamos definir os
reais Objetos que representam as Interfaces:

    type
      TSimpleFinances = class(TInterfacedObject, IFinances)
      public
        construtor ...
        function Current: Currency;
        function AsString: string;
      end;
      
      TSimpleAccess = class(TInterfacedObject, IAccess)
      public
        construtor ...
        function List: IDataList;
        function AsString: string;
      end;

Essas são Classes que implementam, de forma genérica, as Interfaces 
acima.

Agora podemos implementar uma Classe que represente um `Client`
dessa forma:

      TTheClient = class(TInterfacedObject, IFinances, IAccess)
      private
        FFinances: IFinances;
        FAccess: IAccess;
        property Finances: IFinances read FFinances implements IFinances;
        property Access: IAccess read FAccess implements IAccess;
      end;

Veja agora que `TTheClient` não implementa mais os Métodos de ambas as
Interfaces, ele **delega** para outras Classes, especialistas no assunto!

No entanto `TTheClient` pode ser 
[contratado]({% post_url 2016-01-18-interfaces-em-todo-lugar %}#interfaces-sao-contratos)
para trabalhar em qualquer especialidade definida pelas Interfaces que ele 
implementa (por delegação).

Veja que ambas as propriedades são privadas. Sim. Você irá trabalhar com 
`TTheClient` instanciando um tipo de Interface. Você não deve trabalhar com 
a Classe diretamente. Sempre trabalhe com o(s) tipo(s) da(s) Interface(s) 
que a Classe representa.

As possibilidades de evolução do código com essa técnica são muitas. Por exemplo.
Agora que temos a Classe genérica (simples) `TSimpleFinances`, ela pode ser 
reutilizada por outras Classes, similares a `TTheClient`, apenas definindo 
uma propriedade. Essas Classes deverão possuir construtores que as inicialize
de acordo com as Classes que as consome. Por exemplo, `TSimpleFinances` poderia
ser inicializa com o `ID` do Client ou algum outro identificador.

Outra possibilidade: A Classe `TTheClient` pode ter em seu construtor um argumento do
tipo `IFinances`, indicando que ela pode ser inicializada com qualquer tipo de
Classe que implemente `IFinances`, inicializando, assim, o atributo `FFinances`.

## Conclusão {#conclusao}

Conseguimos fazer isso em *Object Pascal*, mas parece que a maioria prefere
definir Classes com 30 métodos do tipo Getter/Setter ou adicionam 40 Métodos numa
Classe do tipo `TDataModule` e acham que estão programando Orientado a Objetos e
reutilizando código.

Temos as ferramentas para programar melhor, mas é preciso olhar "fora da caixa".

Até logo.