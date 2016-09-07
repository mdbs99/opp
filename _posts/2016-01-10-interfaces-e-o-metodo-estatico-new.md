---
layout: post
title: "Interfaces e o Método estático New"
date: 2016-01-10
description: Como implementar seu Garbage Collector no Object Pascal de forma segura
summary: Utilize uma técnica simples para implementar um Garbage Collector sem vazamentos de memória.
image: /images/829d24cf.jpg
categories: 
  - Pascal
tags:
  - object pascal
keywords:
  - oop
  - object pascal
  - orientação a objetos
---

No [post anterior]({% post_url 2016-01-03-pensando-em-objetos %}) eu mostrei um código
Orientado a Objetos onde os Objetos eram instanciados utilizando um Método estático chamado *New*.

Este método *New* não é padrão da linguagem Object Pascal. Ele também não é um `constructor`.
Ele é um **Método de classe** que retorna a mesma Interface que a classe implementa.

A utilização do Método *New* é um padrão que defini para todos os meus projetos.

<!--more-->

![Pensando](/images/829d24cf.jpg)

##Object Pascal e seu "*Garbage Collector*"

Ok. Object Pascal não tem *garbage collector* como no Java ou C# — e isso é ótimo quando precisamos de performance —
porém temos algo parecido quando utilizamos Interfaces.

Toda variável que tem uma Interface como seu tipo, por padrão, será desalocada da memória assim que
sair do escopo de execução. Vamos chamar essas variáveis de **variável-interface**.

No momento que o compilador sabe que a variável-interface não será mais utilizada, ele desaloca a memória
chamando o `destructor` da instância.

Isso é ótimo para a programação Orientada a Objetos.

Significa que, se eu fizer isso corretamente, poderei criar Objetos dentro de Objetos, encadear chamadas de Objetos,
criar novas instâncias `inline` para utilizar em argumentos e muito mais... assim como fazem no Java ou C#. 

##O problema

Existe um grande problema em utilizar Interfaces:

A falta da variável-interface.

Se não implementar da forma correta, você terá algumas violações de acesso (`Access Violation`)
e/ou vazamentos de memória (`memleaks`).

Para que o *garbage colletor* funcione corretamente você precisa ter uma variável-interface para receber a instância, sempre.

Considere o código abaixo:

    function TFoo.Execute(const Name: string): string;
    begin
      Result := TAction.Create(TTask.Create(Name)).Execute.ToString;
    end;

  1. TTask é uma classe que recebe uma `string` no construtor;
  2. TTask implementa ITask, uma Interface;
  3. TAction é uma classe que recebe uma **interface** ITask no construtor;
  4. TAction implementa IAction, uma Interface;
  5. TAction tem um método Execute que retorna uma interface IResult;
  6. IResult tem um método ToString que retorna o resultado no formato `string`;

Quantos vazamentos de memória teremos: 0, 1 ou 2?

A resposta é 1. 

Por quê?

O compilador fará o seguinte (mais ou menos, em termos leigos):

  1. Executar TTask.Create(Name), criando a instância;
  2. A instância criada irá ser passada no argumento, que é do tipo ITask, do construtor de TAction;
  3. TAction irá criar uma instância de IResult quando chamar Execute;
  4. Utilizando a instância de IResult, o método ToString é chamado e uma `string` é retornada;
  
Onde está o erro?

Na chamada `TAction.Create` porque a instância não é referenciada por nenhuma variável-interface!

O compilador não tem uma referência, então ele não sabe se deve desalocar a memória. 

Na chamada `TTask.Create(Name)` não há problema. O argumento de `TAction` recebe uma `ITask`, então o compilador
sabe que deverá desalocar essa instância logo que não precisar mais dela.

Então vamos acertar o código:

    function TFoo.Execute(const Name: string): string;
    var
      A: IAction;
    begin
      A := TAction.Create(TTask.Create(Name));
      Result := A.Execute.ToString;
    end;

Agora não haverá `memleaks` porque agora o compilador tem a referência `A` para `TAction`. No fim do método
`TFoo.Execute` a instância em `A` será liberada automaticamente.

Se não me falha a memória isso foi implementado no Delphi
a partir da versão 3 (me corrijam se eu estiver errado).

Tem muito material sobre isso na Internet se quiserem mais detalhes, mas acredito que você já sabia disso
ou ao menos já leu sobre isso alguma vez.

##O método New

Se você prestou atenção nos códigos acima, vai reparar que está na mão do programador saber quando utilizar
uma variável-interface para "dizer" ao compilador que a instância deverá ser liberada automaticamente...

E é claro que você vai esquecer disso quando estiver programando... várias vezes! Aconteceu comigo. Centenas de vezes.

Nem precisa ser esquecimento, basta o código mudar numa parte para começar a ter `memleaks`, exemplo:

Se o argumento é do tipo Interface, não há problema em criar uma instância `inline`, mas e se o código for
alterado para receber uma classe concreta (mudou de IAction para TAction o argumento)? 
O código pode continuar funcionando, porém haverá `memleaks` porque
o compilador não irá mais desalocar a instância criada no argumento por não ser mais do tipo Interface.

Ora podemos criar `inline`, ora não... isso é muito chato!

### A Solução

Todas as novas instâncias terão uma variável-interface para receber a referência. 

Eu implemento essa "mágica" de forma muito simples utilizando o método *New*.

    type
      TAction = class(TInterfacedObject, IAction)
      public
        constructor Create(Task: ITask);
        class function New(Task: ITask): IAction;
        function Execute: IResult;
      end;
      
    class function TAction.New(Task: ITask): IAction;
    begin
      Result := TAction.Create(Task);
    end;  

    { Implementação de Create e Execute... irrelevantes }  

    { Implementação do método New também para TTask... }

    {...}  
      
    function TFoo.Execute(const Name: string): string;
    begin
      Result := TAction.New(TTask.New(Name)).Execute.ToString;
    end;

Agora nós estamos chamando `TAction.New` ao invés de `TAction.Create`.

O método *New* irá retornar uma instância de `IAction` (interface) diferentemente do construtor
`Create` que iria retornar uma instância de `TAction` (classe).

  * Sem problemas de `memleaks`
  * Código mais limpo, sem variáveis locais
  * Código mais limpo, sem try-finally para desalocar variáveis locais

Utilize o método *New* em todas as suas classes e nunca mais chame o construtor diretamente.
Assim você não terá o problema da falta da variável-interface, seja por esquecimento ou por
mudanças no código.

Tá gostando? Alguma dúvida? Não concorda? Posta aí nos comentários.