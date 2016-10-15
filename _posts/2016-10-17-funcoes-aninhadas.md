---
layout: post
title: "Funções Aninhadas"
date: 2016-10-15
description:
  Funções Aninhadas deixam o código melhor organizado, fácil de ler e alterar.
summary: 
  Funções Aninhadas podem e devem ser utilizadas na Orientação a Objetos
image: /images/photo-1476411890462-80309823db3b.jpg
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

![Unsplash image]({{ page.image }})

[Funções Aninhadas](https://en.wikipedia.org/wiki/Nested_function)
é algo que não tem em todas as linguagens.
A linguagem Pascal tem e acho que devemos aproveitar essa *feature*.

Funções Aninhadas nada mais são do que funções declaradas dentro de
outras funções ou Métodos.

Esse artigo irá mostrar os motivos e vantagens ao utilizarmos Funções 
Aninhadas e também algumas regras que devemos seguir.

## Motivos para utilizá-las {#motivos}

Funções Aninhadas é uma opção bem melhor do que 
[pular linhas]({% post_url 2016-09-19-linhas-em-branco-no-metodo-e-mal-cheiro %})
dentro da implementação de um Método com o intuito de separar blocos
de código.

Cada função já define um bloco, com a vantagem de ser **reutilizável** em
outra parte do Método.

Funções Aninhadas tem um conceito muito próximo da Programação Orientada a 
Objetos (leia esse 
[artigo](http://blog.synopse.info/post/2012/05/20/Recursive-calls-and-private-objects)
).
Um Método que contém Funções Aninhadas é como se fosse uma implementação
de uma *Classe Anônima*. As variáveis locais serão como *atributos* e as 
Funções Aninhadas serão como *Métodos privados*.

Funções Aninhadas facilitam a correta utilização do
[*WITH*]({% post_url 2016-09-12-a-declaracao-with-do-e-do-mal %})
quando a implementação do Método é complexa ou quando o código utiliza
composição com muitos Objetos.
Dividir o código em pequenas funções, diminuirá as chances de haver **conflitos**
entre identificadores que utilizam o *WITH*.

Resumindo: Funções Aninhadas deixam o código melhor organizado, fácil de ler 
e alterar.

## Regras de Uso {#regras}

Qualquer *feature* utilizada de forma indiscriminada poderá ser um
potencial problema no futuro, ao invés de uma solução — o mesmo ocorre com 
o uso indiscriminado do *WITH* por programadores que não sabem exatamente o
que estão fazendo.

Precisamos de **regras** e **disciplina**.

Abaixo algumas regras que você deve levar em conta.

### Regra 1: Não utilize mais do que 3 Funções Aninhadas {#regra1}

É uma regra óbvia.
Se você tem muitas Funções Aninhadas dentro e um único Método, 
é bem provável que ele esteja fazendo **coisas demais**.
Pense na refatoração e decomposição em outros Métodos ou mesmo
na criação de uma nova Classe.

Existem pouquíssimas exceções a essa regra.

### Regra 2: Evite compartilhar as Variáveis Locais {#regra2}

Falei acima que um Método com Funções Aninhadas é como uma Classe
Anônima, contendo Métodos privados e atributos. 
No entanto, sabemos que não são verdadeiras Classes.

É melhor que você isole cada Função Aninhada em suas próprias 
variáveis e argumentos, ou seja, **evite** compartilhar as variáveis
locais do Método com as Funções Aninhadas.

Essa disciplina na codificação irá ajudá-lo na extração e 
refatoração das Funções Aninhadas, se for o caso, para criar outros 
Métodos com o mínimo de impacto possível.

Ao invés de utilizar a variável local do Método, passe
a referência como argumento da função, mantendo-as **isoladas**.

Existem poucas exceções a esta regra. Por exemplo.
Se todas as Funções Aninhadas trabalham sempre com
os mesmos Objetos (variáveis locais) do Método, é mais fácil 
compartilhar as variáveis — ou refatorar criando uma nova Classe —
do que ficar repassando-as às funções. Utilize o bom senso.

### Regra 3: Apenas 1 Nível de Funções {#regra3}

Não complique. Use *apenas* um "nível" de Funções Aninhadas. Se
você tiver utilizando mais de um nível, é provável que a função
de "nível 2" deveria ser um Método.

**Refatore**. 

Após a refatoração do código, o "nível 2" de Funções Aninhadas
passaria a ser o "nível 1" no novo Método criado.

Sem exceções aqui!

## Exemplos {#exemplos}

Funções Aninhadas podem ser utilizadas em qualquer tipo de Objeto.

Objetos que lidam com XML, por exemplo, onde é necessário trabalhar
com recursividade e validações sempre são bons candidatos. 

Mas meu uso pessoal de Funções Aninhadas é, na maioria das vezes, 
utilizada no código de *Forms*.
O motivo é simples: É natural para o usuário clicar em apenas um
botão e o sistema fazer *inúmeras* tarefas. Para o usuário é
irrelevante se foi preciso 1 ou 20 Objetos para concluir a tarefa.
O usuário acha que apenas uma tarefa foi executada quando, na 
verdade, **inúmeros Objetos** podem ter tido participação para executar
o serviço. 

Então, abaixo temos alguns exemplos do que eu considero um bom uso
de Funções Aninhadas.

### Exemplo 1: Peça Permissão, Faça o Serviço {#exemplo1}


### Exemplo 2: Clicou no "Botão Mágico" que Faz tudo {#exemplo2}


### Exemplo 3: Mostre-me o que está acontecendo {#exemplo3}


## Conclusão {#conclusao}

