---
layout: post
title: "Funções Aninhadas Melhoram a Legibilidade do Código"
date: 2016-10-17
description:
  Funções Aninhadas podem e devem ser utilizadas, pois melhoram
  a legibilidade do Código e sua manutenção.
summary: 
  Funções Aninhadas podem e devem ser utilizadas na Orientação a Objetos
image: /images/photo.jpg
categories: 
  - Object Pascal
tags:
  - language
keywords:
  - funções aninhadas
  - nested functions
  - organização do código
--- 

Se você tem um Método coeso, que trabalha em apenas uma única 
tarefa, mas mesmo assim o código parece complicado, dificultando
o entendimento e a manutenção... já pensou em refatorar o código
utilizando Funções Aninhadas?

<!--more-->

![Imagem]({{ page.image }})

## Introdução {#introducao}

[Funções Aninhadas](https://en.wikipedia.org/wiki/Nested_function)
é algo que não tem em todas as linguagens.
A linguagem Pascal tem e acho que devemos aproveitar essa *feature*.

Funções Aninhadas nada mais são do que funções declaradas dentro de
outras funções ou Métodos.

Esse artigo irá mostrar os motivos e vantagens ao utilizarmos Funções 
Aninhadas e também algumas regras que devemos seguir.

## Motivos {#motivos}

Funções Aninhadas é uma opção bem melhor do que 
[pular linhas]({% post_url 2016-09-19-linhas-em-branco-no-metodo-e-mal-cheiro %})
dentro da implementação de um Método com o intuito de separar blocos
de código.

Cada função já define um bloco, com a vantagem de ser reutilizável em
outra parte do Método.

Funções Aninhadas estão muito próximas da Programação Orientada a 
Objetos (leia esse 
[artigo](http://blog.synopse.info/post/2012/05/20/Recursive-calls-and-private-objects)
).

Cada Método que contenha Funções Aninhadas é como uma Classe Anônima.
As variáveis locais serão como atributos e as Funções Aninhadas serão
como Métodos privados.

Funções Aninhadas facilitam a correta utilização do
[*WITH*]({% post_url 2016-09-12-a-declaracao-with-do-e-do-mal %})
quando a implementação do Método é complexa ou quando o código utilizad
muita composição de Objetos.

Dividir o código em pequenas funções, diminuirá as chances de termos conflitos
entre identificadores que utilizam.

O código fica melhor organizado, fácil de ler e alterar.

## Regras {#regras}

Qualquer *feature* utilizada de forma indiscriminada poderá ser um
potencial problema no futuro, ao invés de uma solução — o mesmo ocorre com 
o uso indiscriminado do *WITH*.

Precisamos de **regras** e **disciplina**.

Abaixo algumas regras que utilizo em meus projetos.

### Regra 1: Não utilize mais do que 3 Funções Aninhadas

É uma regra óbvia.
Se você tem muitas Funções Aninhadas dentro e um único Método, 
é bem provável que ele esteja fazendo coisas demais.
Pense na refatoração e decomposição em outros Métodos ou mesmo
na criação de uma nova Classe.

Existem pouquíssimas exceções a essa regra.

### Regra 2: Evite compartilhar as Variáveis Locais

Falei acima que um Método com Funções Aninhadas é como uma Classe
Anônima, contendo Métodos privados e atributos. 
No entanto, sabemos que não são verdadeiras Classes.

É melhor que você isole cada Função Aninhada em suas próprias 
variáveis e argumentos, ou seja, evite compartilhar as variáveis
locais do Método com as Funções Aninhadas.

Essa disciplina na codificação irá ajudá-lo na extração e 
refatoração das Funções Aninhadas, se for o caso, para criar outros 
Métodos com o mínimo de impacto possível.

Ao invés de utilizar a variável local do Método, passe
a referência como argumento da função, mantendo-as isoladas.

Existem poucas exceções a esta regra. Por exemplo.
Se todas as Funções Aninhadas trabalham sempre com
os mesmos Objetos (variáveis locais) do Método, é mais fácil 
compartilhar as variáveis — ou refatorar criando uma nova Classe —
do que ficar repassando às funções. Utilize o bom senso.

### Regra 3: Apenas 1 Nível de Funções

Não complique. Use apenas um "nível" de Funções Aninhadas. Se
você tiver utilizando mais de um nível, é provável que a função
de "nível 2" deveria ser um Método.

Refatore. 

Após a refatoração do código, o "nível 2" de Funções Aninhadas
passaria a ser o "nível 1" no novo Método criado.

Sem exceções aqui!

## Exemplos {#exemplos}


## Conclusão {#conclusao}

