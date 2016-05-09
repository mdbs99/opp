---
layout: post
title: Validações no Construtor
date: 2016-05-09
description: É possível validar um Objeto antes de criá-lo?
summary: É possível validar um Objeto antes de criá-lo?
image: /images/photo-EnF7DhHROS8OMEp2pCkx_Duferfood.jpg
categories: 
  - OO
tags:
  - validation
keywords:
  - validation
  - validação
  - construtor
---

Dizem que um Objeto não pode ter um **estado inválido** e que 
sua validação deve ser feita no seu **Construtor**, ou seja, se 
os argumentos do Construtor não são suficientes, não deveríamos
criar o Objeto.

Imagine um mundo onde tudo são objetos.

Como você irá verificar se um Objeto é inválido antes de você criá-lo?

<!--more-->

![Imagem]({{ page.image }})

O que significa **estado inválido**?

Se os argumentos do construtor de um Objeto não forem suficientes, e não fizermos uma
validação prévia desses argumentos, poderíamos criar um Objeto com estado inválido.
Quero dizer que seu **Estado** não estaria válido para que ele pudesse fazer o trabalho
para o qual ele foi/será designado.

E porque isso seria um problema?

Bem, eu não acho que seria um problema. A única **exceção** é
a utilização de [Objetos Nulos]({% post_url 2016-04-11-nao-utilize-nil-ou-null %}). Nesse 
caso sim, seria um **erro**. 

Vamos a um exemplo. Vamos definir uma classe `TDbQuery` que irá encapsular um SQL.
Essa classe só terá um método (simplificando) para executar o SQL.

{% highlight pascal %}
type
  TDbQuery = class
  private
    FScript: string;
  public
    construtor Create(const Script: string);
    function Execute: Integer;
  end;
{% endhighlight text %}

O construtor tem um argumento do tipo `string` e o usuário da classe poderia passar qualquer
valor como um SQL válido, mas também letras, números ou até mesmo uma string em branco.

Precisamos saber se o SQL é válido antes da hora de chamar `Execute`? **Não**.

Existem validações simples como verificar se uma `string` está em branco, mas há outras validações
muito mais complexas.

Para validar a classe acima seria muito complexo.

Por que?

Você teria que validar a sintaxe do SQL,
talvez utilizando *Regular Expression* (RegExpr). Se tivéssemos Objetos que implementam RegExpr, então
teríamos muitas instâncias de Objetos para validar a `string`. São Objetos, que chamam outros, que chamam outros...

E, mesmo depois de toda essa validação sintática, a execução do SQL ainda poderia gerar erro! A tabela não existe,
o nome da coluna foi digitado errado, etc.

Pior. Talvez o algoritmo em execução nem precisasse chamar esse Objeto, talvez o usuário tenha cancelado a operação
antes... então você perdeu tempo validando e nem precisou executar o *Script*!

Se tudo são Objetos, eles devem ser criados **antes** de serem validados. Essa operação deve ocorrer o **mais rápido**
possível, no **menor custo** computacional possível, mesmo em "estado inválido".

Em outro momento ele será validado pois não há como validar algo que ainda não existe! 

Ainda não concorda?

Então vamos ver um exemplo no mundo real.

Para fazer um bom prato você precisa ter bons ingredientes. Um *Spaghetti a Lá Carbonara* precisa de uns 4 ovos — eu
gosto de fazer esse prato — bacon, queijo parmesão...
No [Mise en place](https://en.wikipedia.org/wiki/Mise_en_place) você precisa bater os ovos e misturar com parmesão; 
então você vai quebrando os ovos e descobre que um deles está podre (estado inválido). Você não vai utilizá-lo.
Esse Objeto não serve... mas ele existe! Estava lá, junto com os outros "ovos válidos". Ele ocupava espaço, tinha a 
mesma aparência dos demais mas estava "inválido". O mundo é assim. 

>Se existisse um mundo onde todos os Objetos fossem validados antes de serem criados, então seria um mundo perfeito com Objetos perfeitos.

A [Matrix](https://en.wikipedia.org/wiki/The_Matrix) tentou criar um mundo assim — ou foram 3, talvez 4? Não lembro — 
mas não deu muito certo... ao que parece os seres humanos precisam de estado inválido e não lidam bem com perfeição 
— o primeiro filme da série foi o melhor, não é? :) 

Se nosso **mundo real** foi "implementado" assim, contendo "Objetos com estado inválido", porque você acha que 
seus Objetos são — ou deveriam ser — mais **perfeitos** do que os objetos do mundo real, do qual eles representam?

Até logo.
