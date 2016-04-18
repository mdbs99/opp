---
layout: post
title: "Objetos sem Estado"
date: 2016-04-04
description: Objetos deveriam ser inicializados somente através dos seus construtores e não através de propriedades.
summary: Objetos deveriam ser inicializados somente através dos seus construtores e não através de propriedades.
image: /images/photo-1455641064490-74f5f8dbf598.jpg
categories: 
  - OO
tags:
  - estado
keywords:
  - estado
  - state
  - encapsulamento
--- 

Por que sempre deveríamos criar nossas Classes com pelo menos um argumento no construtor principal afim de 
não deixar nossos Objetos **"vazios de estado"**?

Porque construtores sem argumentos é um anti-padrão.

<!--more-->

![Vazio]({{ page.image }})

Para criamos instância de objetos utilizamos os Construtores de Classes 
[Primários e Secundários]({% post_url 2016-03-21-construtores-da-classe-primario-secundarios %})
ou indiretamente, através do padrão chamado [Método New()]({% post_url 2016-01-10-interfaces-e-o-metodo-estatico-new %}).

Um bom construtor sempre tem argumentos para a inicialização do Objeto.

Não utilizar argumentos é visto como uma má prática na Orientação a Objetos porque um Objeto deve encapsular
atributos privados, ou seja, seu **Estado**.

Se um Objeto não tem Estado então ele pode ser visto como uma instância de uma 
[Classe Utilitária]({% post_url 2016-02-15-nao-utilize-metodos-estaticos %}#classes-utilitarias),
o que é um anti-padrão na programação Orientada a Objetos.

Infelizmente o senso comum da maioria dos programadores Object Pascal — e também de outras linguagens Orientada a Objetos —
é sempre ter apenas um **construtor sem parâmetros**.

Essa prática é comum desde... sempre.

A VCL/LCL, a RTL, todos os descendentes de <code>TComponent</code>, a maioria dos códigos que tenho visto em 
várias empresas e, é claro, em muitos dos meus códigos
— projetos antigos cujo o custo para fazer tais alterações seria muito alto — 
todos utilizam construtores sem argumentos, na maioria das Classes.

Por que codificamos dessa maneira?

Porque é mais **fácil** codificar dessa maneira. 

Após a inicialização do Objeto com um construtor sem argumentos basta utilizar as *properties* ou métodos <code>SetXxx(Value)</code> para inicializar ou
injetar algum valor.

Fácil.

Essa prática é **simples e conveniente**, no entanto dizem que "na Engenharia nada é de graça", ou seja, essa facilidade tem um preço.

Essa **falsa simplicidade** trás consigo:

  * "Código-espaguete": Classes mutáveis, crescentes, são quase sempre o início do caos.
  * Complexidade: Classes gigantes e complexas com dezenas de métodos e propriedades.
  * Desperdícios: De memória devido a alocação desnecessária de "objetos filhos"; 
  de tempo devido ao tempo perdido para entender, implementar ou resolver problemas.

Sério.

Não concorda?

Então me diga quando foi a última vez que você utilizou todas as propriedades e métodos de um componente? Não lembra, quase nunca ou o mais provável, nunca.

Um Objeto deve ser fácil de criar, mas você precisa no mínimo raciocinar sobre o motivo de instânciá-lo e isso é feito 
através dos argumentos passados em seu construtor. Você deve instanciar um Objeto que
[represente uma Entidade]({% post_url 2016-02-29-objetos-representam-entidades %}) para fazer algum trabalho num
momento específico no tempo.

Sem um construtor adequado é fácil adicionar mais e mais propriedades 
e métodos aos Objetos.

O problema é que esses Objetos não param de crescer ou já são grandes o bastante para ninguém mais entendê-los ou não ter a coragem para 
refatorar seu código.

No fim você terá um monstro em suas mãos e é justamente isso que queremos evitar!

O outro motivo desses monstros aparecerem é devido ao fato desses Objetos não serem [imutáveis](https://en.wikipedia.org/wiki/Immutable_object).
Codificar utilizando imutabilidade irá restringí-lo num primeiro momento, no entanto essa restrição irá fazê-lo pensar num
design melhor para suas Classes.

Num mundo perfeito, todo construtor deveria prover informação necessária para um Objeto encapsular algum Estado. Essa informação
é concedida através dos argumentos.

Bem, acho que ainda não é possível criar componentes para a IDE sem que apenas um construtor único e sem argumentos seja utilizado
para instanciar o Objeto. Fizeram a arquitetura dessa forma e não temos como mudar. É assim no Delphi e Lazarus, mas também 
ocorre no Java ou C# onde muitos *frameworks* exigem um construtor sem argumentos.

Então vamos deixar os componentes de lado e focar apenas nas **Classes de Negócio**, porque podemos implementar essas Classes 
da maneira que quisermos sem nos preocuparmos em integrá-las com *frameworks* ou IDE's.

Antes de criar uma Classe, não pense nas funções. Pense no que ela representa e quais os argumentos que seus Objetos irão precisar
para fazer o trabalho.

Até logo.


 


