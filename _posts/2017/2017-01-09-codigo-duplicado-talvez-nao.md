---
layout: post
title: Código Duplicado, Talvez Não
date: 2017-01-09
description:
  É quase impossível não haver duplicação de código num programa.
  O importante é saber se essa duplicação é de Infraestrutura ou de Negócio.
image: /images/photo-ceukkv42o40-aaron-burden.jpg
categories: 
  - Code
tags:
  - code
  - object pascal
keywords:
  - code
  - código
  - object pascal
  - duplication
  - duplicação
--- 

É quase impossível não haver duplicação de código num programa real,
com pelo menos algumas dezenas de linhas.

O importante é saber se essa duplicação é de Infraestrutura ou
de Regras de Negócio.

<!--more-->

![Unsplash image]({{ page.image }})  

## Introdução {#introducao}

Sempre ouvimos falar que não se deve ter código duplicado num
sistema, mas eu tenho certeza que isso é quase impossível de 
conseguir.

O que é uma duplicação de código?

Se você tem duas linhas iguais, elas estão duplicadas?

Bem, depende muito do *ponto de vista*.

Pra mim existem dois tipos de código: *Infraestrutura* e *Regras de Negócio*.

Código de Infraestrutura pode estar duplicado mas você *não*
pode — ou não deveria — duplicar Código de Regras de Negócio.

Simples assim.

Mas num sistema onde tudo é código, como saber a diferença entre
um tipo e outro?

Nesse artigo você aprenderá a dinstinção entre eles.

## Infraestrutura {#infraestrutura}

Dou esse nome para todo código que faz parte da infraestrutura
para criação de Classes, implementação de Interfaces, instanciação
de Objetos, chamadas aos métodos, etc.

Vou lhe dar um exemplo. Imagine que você tem uma Classe com três
argumentos no construtor. Toda vez que você instanciar um Objeto
dessa Classe você terá que *duplicar* a mesma construção:

    Ob1 := TFoo.New('a', 1, 'z');
    Ob2 := TFoo.New('b', 5, 'a');
      
    // in other place...
    Ob3 := TFoo.New('a', 1, 'z');
 
O código acima parece duplicado, não?

Bem os Objetos `Ob1` e `Ob2` tem argumentos diferentes, então essas
linhas não são totalmente iguais, apenas bem similares. Então o que
você acha? Há duplicação aqui?

Não.

Mas e o `Ob3`? Ele foi construído com a mesma assinatura de `Ob1` 
mas em outra parte do programa. Isso é uma duplicação?

Também não.

O fato de haver 2 chamadas iguais num mesmo projeto não quer dizer 
que há uma duplicação de código. Se assim fosse, toda vez que você
codifica um `TStringList.Create` estaria duplicando código.

Quer outro exemplo? Vamos lá.

Você tem uma Interface com dois métodos.
Toda Classe que implementar essa Interface terá que, obrigatoriamente,
*duplicar* os dois métodos dessa Interface na Classe.

Isso é uma duplicação? Não.

Isso é *Código de Infraestrutura*.

Poderia ser menos verboso implementar uma Interface? Talvez. 
Mas se a linguagem *exige* que seja dessa forma,
então não é duplicação.

O código de Infraestrutura são como "tijolos" no seu projeto.
Eles são iguais — ou bem parecidos — e você precisa deles dessa forma.

Você irá vê-los por todo o projeto. As vezes ficará tentado a refatorar,
criando uma função — ou mesmo outro Objeto — para agrupar chamadas iguais
removendo, assim, a "duplicação" de código... mas, espere, essa nova função
também será duplicada toda vez que você utilizá-la, pois sempre será a 
mesma assinatura. Então, talvez...

Impossível.

## Regras de Negócio {#regras-de-negocio}

As Regras de Negócio são todo o tipo de código que não é de Infraestrutura.
São as implementações do Negócio do seu Cliente. São as Regras que os usuários
já utilizam mesmo sem haver um sistema informatizado. São os cálculos, as validações,
as restrições... as regras.

Aqui *não* pode haver duplicação. Se, por exemplo, há um cálculo de desconto
para determinado grupo de produtos, a implementação desse cálculo deverá estar em
apenas uma Classe.

Isso é *Código de Regras de Negócio*.

Seu Cliente poderá mudar essas regras e você deverá alterar a implementação de cada
uma delas apenas uma vez, num único ponto no código, para cada uma das Regras.

Para toda Regra de Negócio deve existir um ou mais testes automatizados.
Esses Objetos que implementam as Regras não devem ter dependências externas que
impeçam os testes. Não podem depender de recursos como Banco de Dados ou arquivos
no disco. Todos os seus argumentos devem ser passados através dos
[construtores]({% post_url 2016-03-21-construtores-da-classe-primario-secundarios %})
de suas Classes e, preferencialmente, todos esses Objetos devem ser
[imutáveis]({% post_url 2016-07-04-objetos-imutaveis%}), garantindo que uma vez
criados eles não podem ser corrompidos de nenhuma forma.

## Conclusão {#conclusao}

Se você leu até aqui deve estar pensando: Ora, isso é óbvio!

Bem, nem tanto, pois ainda tem programadores que desenvolvem suas Classes 
pensando apenas na reutilização e na não-duplicação de código de Infraestrutura.
A [herança de Classes]({% post_url 2016-05-23-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-1 %})
é o caminho mais utilizado para isso... erroneamente, é claro.

Você deveria codificar suas Classes pelo que elas são sem se preocupar com 
reaproveitamento de *Código de Infraestrutura*.

Uma asa-delta pode voar e fazer acrobacias como um pássaro, no entanto eles 
não devem ter uma subclasse em comum para "reutilizar" Código de Infraestrutura
afim de escrever menos.

O *Código de Regras de Negócio* é bem diferente. Não deve haver duplicações. Nunca.
Esse tipo de código é como um Manual de construção, ou seja, as Regras de construção.

Você pode *duplicar* as paredes da sua casa, mas não as *regras* de como
fazer isso.

Saiba distinguir essas diferenças.

Até logo.