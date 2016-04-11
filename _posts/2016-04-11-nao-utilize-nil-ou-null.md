---
layout: post
title: "Não Utilize nil ou NULL"
date: 2016-04-11
description: Objetos deveriam ser inicializados somente através dos seus construtores e não através de propriedades.
summary: Objetos deveriam ser inicializados somente através dos seus construtores e não através de propriedades.
image: /images/photo-1415226620463-aedee27159c5.jpg
categories: 
  - Pascal
tags:
  - nil
  - null
keywords:
  - nil
  - null
--- 

O conceito *NULL*, também conhecido como ["O erro de 1 bilhão de dólares"](http://www.infoq.com/presentations/Null-References-The-Billion-Dollar-Mistake-Tony-Hoare),
foi inventado por Charles Antony Richard Hoare em 1965.

Em uma conferência em 2009, ele pediu desculpas por inventar a referência nula.

Mas o estrago já havia sido feito...

<!--more-->

![NULL]({{ page.image }})

A referência *NULL* — o mesmo vale para *nil* do Object Pascal — é antiga e existe, talvez, 
em todas as linguagens de programação. 

O conceito também é valido para SGBD's.

Então por que estou dizendo para você não utilizar *nil* ou *NULL*?

>*NULL* ou nil não pertencem ao mundo Orientado a Objetos.

Pense por um segundo, como seria o mundo sem o *NULL*.

##*NULL* na Orientação a Objetos

De agora em diante quando eu disser *NULL*, pense também em *nil*.

Esse blog fala de Orientação a Objetos com *Object Pascal*, então nós, programadores *Object Pascal*, 
utilizamos o `nil` ao invés de `NULL` quando estamos falando sobre Objetos e referências a Objetos. 
Se estivermos falando de dados, então é *NULL* mesmo.
Mas como a maioria das linguagens e *papers* falam sobre *NULL*, fica mais claro para todos — inclusive 
para programadores que não programam em *Object Pascal* — falarmos sobre *NULL* para nos referirmos a ambos.

Então.

**O que significa *NULL* na Orientação a Objetos?**

Nada. 

Não existe instância, não existe um Objeto. Ninguém para receber uma mensagem (método) e fazer alguma coisa.

Mas um método pode retornar *NULL* se o contexto em execução não tiver um Objeto válido
para o retorno — você poderia afirmar.

Bem, em um código Orientado a Objetos você deve ter Objetos "conversando" entre si e *NULL*
não é uma instância válida.

**Todo método que retorna um tipo de Interface ou Classe deveria retornar uma instância
válida. Sempre.**

Se não for possível, **levante uma exceção**.

Essa é a **teoria**.

Infelizmente essa teoria não funciona na prática. Não em 100% das vezes.

Não há como saber, previamente, se um método que deveria retornar uma instância de algum
Objeto irá retornar uma instância válida ou *NULL*.

Muitos *frameworks* utilizam *NULL*, o SGBD retorna *NULL*, o mundo
utiliza *NULL*...

Esse é o problema. O *NULL* existe e não podemos ignorá-lo.

Felizmente existem técnicas para evitá-lo e proteger nosso código de seu uso.

  1. *Null Object Pattern*
  2. *Optional Pattern*
  3. Listas
  4. Levantar uma Exceção

##1. *Null Object Pattern*

O padrão [*Null Object Pattern*](https://en.wikipedia.org/wiki/Null_Object_pattern) sugere utilizar
um "Objeto Nulo" que implementa a mesma interface do Objeto real, no entanto esse Objeto não 
teria implementação.

Tradução livre:
<blockquote>
  Em vez de utilizar uma referência nula para transmitir ausência de um objeto (por exemplo, um cliente inexistente), 
  utiliza-se um objeto que implementa a interface esperada, mas cujo o corpo dos métodos estarão vazios. 
  <footer><cite title="Wikipedia">— Wikipedia</cite></footer>
</blockquote>

É uma boa ideia. Mas não resolve o problema. Aliás, em alguns casos, pode-se aumentar o problema
introduzindo um Objeto que "não faz nada" e não levanta exceções. Pode ser difícil encontrar *bugs* devido
ao seu uso.

Esse tipo de Objeto pode ser utilizado, na minha opinião, não como um "Objeto Nulo" mas como um "Objeto *Default*",
que contém código real.

Em um Sistema de Pagamentos, cada Documento tem um código. Cada um desses códigos tem uma "Classe de Cálculo"
específica que irá fazer o cálculo do valor a ser pago. O Objeto é criado de acordo com o código, utilizando uma **Fábrica** de 
acordo com o padrão [*Abstract Factory Pattern*](https://en.wikipedia.org/wiki/Abstract_factory_pattern).

O problema ocorre se o usuário criar um novo código para o qual ainda não existe um Cálculo pré-determinado. 
Nesso momento utilizo o "Objeto *Default*". É uma Classe padrão que serve para calcular qualquer novo Documento que
não tenha uma Classe de Cálculo definida.

O cálculo pode não estar 100% correto para o usuário, mas também não considero um erro pois a Classe de Cálculo ainda 
não foi definida.

É uma **solução elegante** para um **problema temporário**.

##2. *Optional Pattern*

*Optional Pattern* ou [*Option Type*](https://en.wikipedia.org/wiki/Option_type) é nada mais, nada menos, que a 
utilização de *Generics* para encapsular o resultado de um método, quando esse resultado pode ou não conter uma 
instância válida.

*Optional* também é conhecido por *Maybe*. É um conceito das linguagens funcionais que foram implementados em linguagens
imperativas como Java e C#.

Basicamente você tem uma Classe genérica que contém os seguintes métodos:

  * IsNull — indica se o valor encapsulado é *NULL*;
  * Value — retorna o valor real, ou seja, o Objeto encapsulado;
  
O nome dos métodos podem variar dependendo da *Lib* utilizada.
  
Este [link](http://enterprisecraftsmanship.com/2015/03/13/functional-c-non-nullable-reference-types/) tem exemplos em C# interessantes.

##3. Listas

Como podemos simular o *Optional Pattern* sem utilizar *Generics*?

Ora, Listas!

*Generics* é muito utilizado, mas não o acho impressindível. *Generics* nos permite escrever menos e, é isso.
O que é muito bom pois menos código, menos *bugs*.

Mas para o problema do *NULL* você não precisa de *Generics*, basta utilizar Listas.

Qual a diferença entre ter uma condicional que verifica o método `IsNull` de um *Optional*  para depois utilizar sua propriedade `Value`
para obter a instância, do que ter uma Lista que também devemos verificar `Empty` e depois obter o primeiro `Item[0]` da lista? :)

Então se está utilizando uma versão antiga do Delphi — eu ainda utilizo Delphi 7 para alguns sistemas — que não tem *Generics*
e não tem certeza se um método poderá retornar *NULL*, pense em retornar uma Lista com itens que contém o tipo da interface que o método
deveria retornar. Se a lista não está vazia, basta pegar o primeiro item.

##4. Levantar uma Exceção

Se tudo der errado, levante e uma exceção. É isso. **Falhe rapidamente**.

Mas como saber se devo utilizar uma das técnicas acima ou levantar uma exceção?

Não há uma regra. Deve-se usar o **bom senso**. 

Se você tem um método que pesquisa pelo `Id`, então quer dizer que o sistema, naquele momento, tem
o `Id` do registro e ele **deveria** existir. Se nada é retornado, então levante uma exceção.

Se a pesquisa é por `Nome`, no entanto, então **não há certeza** de retornar 0-n registros. Nesse caso
levantar uma exceção não faria muito sentido.

##Conclusão

Não há mágica para remover o *NULL*. Mesmo utilizando as técnicas apresentadas acima, deve-se codificar
uma **condicional** para verificar a presença ou não do *NULL*.

Muitos *frameworks*, *API's*, componentes irão utilizar o *NULL*, então tais verificações sempre irão existir no seu código.

Você irá utilizar *NULL* ou *nil*, isso é quase um fato.

Esse artigo propõe a **não utilização do *NULL*** em Objetos que fazem parte do 
**Domínio** do seu sistema, ou seja, das suas **Classes de Negócio**.

Não permita que seus métodos retornem *NULL*.

Não utilize *NULL* em lógicas condicionais de suas Classes.

Você deve estar pensando:

<ins>Testar se uma instância é *NULL* não é o mesmo que testar se um *Optional* ou Lista estão vazios antes de obter seu `Value`?</ins>

O teste é o mesmo, sim. Você terá um `if` no código, é verdade. Mas se o valor retornado no método é uma instância 
de algum Objeto que implementa alguma interface, não tem como saber se a instância é válida ou *NULL*. E **ninguém**, nenhum programador,
**testa todos os retornos de todos os métodos**.

No entanto, *Optional* ou Listas **avisam** ao programador que algo pode dar errado, que *NULL* pode ser retornado. Cabe
ao programador fazer a análise, fazer o teste ou não... mas ele foi avisado.

O código fica **auto-documentado** sobre o possível retorno de valores *NULL* nos métodos.

Até logo.
