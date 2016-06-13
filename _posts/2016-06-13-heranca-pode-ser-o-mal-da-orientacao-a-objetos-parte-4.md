---
layout: post
title: "Herança pode ser o Mal da Orientação a Objetos — Parte 4"
date: 2016-06-13
description: Não faça da Herança a sua primeira escolha para reutilizar código.
summary: Não faça da Herança a sua primeira escolha para reutilizar código.
image: /images/photo-1461287159820-04de78c094e9.jpg
categories: 
  - OO
tags:
  - herança
keywords:
  - herança
  - inheritance
  - evil
  - mal
  - orientação a objetos
  - oop
  - poo
  - encapsulamento
  - polimorfismo
--- 

No artigo anterior falei sobre Duplicação de Código.
Nesse artigo irei falar sobre o **Forte Acoplamento** que ocorre ao utilizarmos a Herança de Classe.

<!--more-->

![Acoplamento]({{ page.image }})

[Clique aqui]({% post_url 2016-06-06-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-3 %}) para ler a **Parte #3** 
dessa série, caso ainda não tenha lido.

##Introdução {#introducao}

>Se um gato possui raça e patas, e um cachorro possui raça, patas e tipoDoPelo, logo Cachorro extends Gato? 

O texto acima é emprestado [desse artigo](http://blog.caelum.com.br/como-nao-aprender-orientacao-a-objetos-heranca/) que
fala sobre Herança e Hierarquia de Classes. O artigo fala sobre o erro de *design* dos projetistas da linguagem Java quando
codificaram as Classes `Stack` e `Properties`. Não precisa conhecer Java para entender o artigo. 

No mesmo artigo ele mostra o erro **grotesco** no *design* da Classe `HttpServlet`. 

Se você já utilizou Java e precisou extender essa Classe, deve ter reparado no mesmo problema citado no artigo.

Na época da faculdade eu reparei nesse erro de *design* mas, **ignorei**. Achei que eu estava errado, que não tinha entendido a "maneira correta
de programar utilizando Orientação a Objetos com Java". Eu não sabia muita coisa mesmo. Talvez
ainda não saiba... sempre que leio textos de outrem fico indignado o quanto ainda preciso aprender. Pois é.
 
O fato é que o pensamento comum da maioria das pessoas é: Será que uma grande empresa, projetista do Java por exemplo, 
poderia estar errada?

Sim as empresas erram, muito.

Sim, elas também ganham muito dinheiro. Mas não é porque seus *softwares* são perfeitos, mas sim porque elas se adaptam
rapidamente e dão aos seus clientes o que eles querem — ou o que está na moda; ou algo similar ao concorrente; ou talvez algo
"legal de se ter".

Por isso muitas empresas não se importam em descontinuar aplicativos e *frameworks* se eles não dão lucro ou não estão mais em conformidade
com a visão de longo prazo delas — vide Microsoft — o que é perfeitamente normal. Elas precisam gerar lucro, certo?

Então não pense que as empresas milionárias como a Microsoft, Sun/Oracle, Embarcadero e muitas outras, estão sempre certas "só" porque
elas são milionárias. Bem, elas estão corretíssimas sobre gerar lucro, mas nem sempre estão corretas sobre **arquitetura e desenvolvimento**
de *softwares*.

##Forte Acoplamento {#forte-acoplamento}

O termo [Acoplamento](https://en.wikipedia.org/wiki/Coupling_(computer_programming)) é uma medida de quão duas rotinas, módulos ou 
Classes estão ligados **intimamente**.

<blockquote>
  Em engenharia de software, acoplamento é a maneira e grau de interdependência entre módulos de software.
  <footer><cite title="Coupling">Coupling - Wikipedia</cite></footer>
</blockquote>

Existem várias formas de acoplamento.

O Forte Acoplamento entre Classes se dá quando uma Classe "conhece" uma outra diretamente. 
Por exemplo. Quando você inicializa um atributo de uma Classe utilizando uma Classe concreta, 
você está acoplando as Classes.

{% highlight pascal %}
constructor TCustomer.Create(const Name: string);
begin
  inherited Create;
  FName := Name;
  FAddress := TAddress.Create;
end;
{% endhighlight text %}

No exemplo acima existe um acoplamento entre `TCustomer` e `TAddress`. Conceitualmente isso é ruim. O mais correto seria utilizar
**injeção de dependência** passando a instância de `TAddress` através de um argumento do tipo Interface no construtor de `TCustomer`.

{% highlight pascal %}
constructor TCustomer.Create(const Name: string; 
  Address: IAddress);
begin
  inherited Create;
  FName := Name;
  FAddress := Address;
end;
{% endhighlight text %}

Devemos evitar o acoplamento. O motivo é simples. Toda vez que você alterar uma Classe que está acoplada a outra, haverá grandes
chances de você ter que alterar ambas.

Se utilizar argumentos do tipo [Interface]({% post_url 2016-01-18-interfaces-em-todo-lugar %}) e injeção de dependência, 
o acoplamento será apenas de Interfaces.

O exemplo acima não é sobre Herança, mas sobre **Composição de Objetos**.

Apesar de ser ruim o acoplamento sem utilizar injeção de dependência, é possível refatorar essas Classes (se necessário) 
sem termos que alterar todas as Classes envolvidas.
Poderíamos começar a codificação de `TCustomer` sem injetar `TAddress` e, depois, resolver alterar o código para tornar `TCustomer`
mais desacoplada.

No entanto, se utilizássemos **Herança de Classe**, a mesma refatoração poderia ser **muito mais difícil**. 
Por exemplo. Se `TCustomer` fosse uma Subclasse de `TPerson` e essa Classe inicializasse `FAddress` em seu construtor,
`TCustomer` não poderia — ou não deveria — alterar a instância de `FAddress`, reinicializando-a com um outro tipo de Classe.

{% highlight pascal %}

{ TPerson }

constructor TPerson.Create(const Name: string);
begin
  inherited Create;
  FName := Name;
  FAddress := TAddress.Create;
end;

{ TCustomer }

constructor TCustomer.Create(const Name: string);
begin
  inherited Create(Name);
  FAddress := TAnotherAddress.Create; //<<<
end;
{% endhighlight text %}

Nesse exemplo simples, existem alguns problemas.

Primeiro, para que `TCustomer` consiga alterar `FAddress`, esta deve ter a visibilidade de `protected`, o que é uma
[Violação de Encapsulamento]({% post_url 2016-05-30-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-2 %}), pois
`TCustomer` teria conhecimento interno sobre o estado de `TPerson`.

Segundo, se `TCustomer` tem acesso de alteração dos atributos herdados de `TPerson`, esta mudança poderia causar inúmeros
problemas pois o comportamento de `TPerson` poderia ser alterado ou invalidado para todos as outras Subclasse de `TPerson`.

E terceiro, se `FAddress` não fosse `protected` nenhuma opção estaria disponível.

##A Classe TDataSet

Erros de *design* de Classes não existem só na linguagem Java. No Delphi também temos muitos erros. Um dos piores, na minha opinião, 
é a Classe `TDataSet`.

Por simplicidade, talvez, alguém pensou em fazer uma Classe para manipulação de dados. É o famoso *data-ware*. Componentes mostram
dados em *widgets* (edits, grids, etc) obtendo esses dados de instâncias de Subclasses de `TDataSet`.

Esse é um abuso clássico da Herança: Utilização da Herança para reaproveitar código.

Bem, se a Herança fosse mesmo uma boa escolha, eu estaria utilizando Herança nas minhas Classes. Teria minha própria Hierarquia de 
Classes e Subclasses, certo?

Então eu lhes pergunto: Onde `TDataSet` faria sentido na minha Hierarquia de Classes? Provavelmente nenhum sentido.
No entanto eu sou **obrigado** a herdar de `TDataSet` se eu quiser exibir dados num `TDBGrid`. Esse *design* é muito errado.

E qual seria o *design* correto?

Você já sabe.

Todos os lugares que esperam uma instância de `TDataSet` deveriam esperar uma Interface `IDataSet`. Tão simples quanto
utilizar Herança, porém mais eficiente e desacoplado.

##Conclusão {#conclusao}

A utilização de Herança é a pior forma de acoplamento porque é mais difícil de corrigir e refatorar. 

Utilizar Herança é como usar concreto, após secar você não consegue mover mais nada.

##No próximo artigo...

No próximo artigo irei falar sobre **Hierarquias Complexas** entre Classes que utilizam Herança.

Caso você tenha alguma dúvida ou quiser compartilhar seus pensamentos sobre essa série, utilize a área 
abaixo para comentários.
  
Até logo.
