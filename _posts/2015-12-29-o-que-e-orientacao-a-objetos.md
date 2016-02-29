---
layout: post
title: "Você sabe, com certeza, o que é Orientação a Objetos?"
redirect_from:
  - /o-que-e-orientacao-a-objetos/
date: 2015-12-27 15:00:00
description: Orientação a Objetos é muito mais do que Herança, Encapsulamento e Polimorfismo
summary: Não pense que você programa Orientado a Objetos só porque utiliza Classes, Herança, Encapsulamento e, talvez, Polimorfismo.
image: /images/photo-1451340124423-6311db67a5d9.jpg
categories: oop
tags: oop
keywords:
  - orientação a objetos
  - oop
  - poo
  - object oriented
---

Não pense que você programa Orientado a Objetos só porque utiliza Classes, Herança, Encapsulamento e, talvez, Polimorfismo.

<!--more-->

![Rochas](/images/photo-1451340124423-6311db67a5d9.jpg)

##Um pouco de História

Muitos programadores, ao menos uma vez na vida, já devem ter pesquisado 
o que significa __Orientação a Objetos__, quando o termo foi cunhado e por quem.

Há vários sites hoje em dia contendo essas informações, então não vou 
replicá-las aqui… bem, só pra lembrar rapidamente: o criador do termo foi Alan Kay, 
o mesmo criador da linguagem Smalltalk — uma linguagem puramente 
Orientada a Objetos — no entanto, mesmo antes do termo ter sido criado, 
ideias conceituais sobre Orientação a Objetos já estavam sendo aplicadas 
na linguagem Simula 67 (1967).

Linguagens de programação foram desenhadas com base nesse paradigma; centenas ou milhares de livros foram escritos; milhões de sites na Internet falam desse assunto. Então porque, após todos esses anos, a maioria dos programadores ainda não sabem o que é programar Orientado a Objetos?

##Um Objeto não é um “balde” de Funções e Dados {#objeto-nao-e-um-balde-de-funcoes-e-dados}

Ok, <ins>tecnicamente</ins> ele é um “balde” de funções e dados.
Quando um objeto é criado ele possui uma estrutura de dados em memória; cada função recebe um ponteiro implícito para essa estrutura; as funções podem utilizar esses dados como se fossem variáveis locais… basicamente é isso.

Mas se você não for um projetista de compiladores, pouco importa saber como um Objeto é gerenciado internamente pelo compilador — muitos programadores Java nem sabem o que é um ponteiro :)

Então funções e dados ganharam novos nomes: funções são chamadas de Métodos e dados são chamados de Atributos ou Propriedades.

Utilizar as palavras Métodos e Atributos já é um grande passo para entender o que são Objetos, mas eu prefiro dizer que um Objeto tem __Comportamento__ e __Estado__, respectivamente.

E qual a diferença (teórica) entre <ins>Métodos vs Comportamentos</ins> e <ins>Atributos vs Estado</ins>?
Se você “executa sequencialmente os métodos” de um Objeto e/ou “atualiza seus atributos”, você utiliza __Programação Procedural__.

É assunto para outro post explicar as diferenças sutis de nomenclatura, assim como as afirmações que fiz acima, mas se for possível para você apenas aceitar tais definições por enquanto, continue lendo.
<blockquote>
  <p>
    “A orientação a objetos é um modelo de análise,
    projeto e programação de sistemas de software baseado
    na composição e interação entre diversas unidades de
    software chamadas de objetos.” 
  </p>
  <footer><cite title="Wikipedia">— Wikipedia</cite></footer>
</blockquote>

Objetos podem ser __compostos__ por outros Objetos e eles __interagem__ entre si.

Funções são executadas. Objetos interagem entre si.

Nunca pensem em Objetos em termos de funções e dados. Nunca.

Para nós, de agora em diante, um Objeto representa uma entidade, 
criatura ou qualquer coisa fora do contexto do programa.

Alguns dizem, também, que um Objeto representa uma entidade na “vida real”. 
A definição está correta, porém pode gerar confusão, por exemplo:

  1. Um cachorro é uma entidade na vida real? Sim.
  2. Uma pessoa é uma entidade na vida real? Sim.
  3. Um arquivo de computador é uma entidade na vida real? Sim.
  4. Um pixel é uma entidade na vida real? Sim.

Se você não notou diferença entre os exemplos, parabéns. Mas se ficou em dúvida a 
respeito dos itens #3 e #4, tudo bem, muitos tem a mesma dúvida. Por isso eu digo 
que um Objeto é qualquer coisa fora do contexto do programa. Definir como “vida real” 
também está certo e tem a vantagem de ser mais rápido pra escrever 😉

##Os programadores não utilizam Orientação a Objetos

Estou afirmando com base na minha experiência pessoal com o mercado brasileiro, 
livros e inúmeros post lidos por mim no decorrer dos anos.

Você pode entender isso como uma coisa boa ou ruim. Depende do ponto de vista. 
Existe a Programação Funcional e muitas (grandes) empresas a utilizam. 
Programação Funcional não tem nada haver com Orientação a Objetos e, tudo bem.

Mas, atenção. Não pense que você programa Orientado a Objetos só 
porque utiliza __Classes__, __Herança__, __Encapsulamento__ e, talvez, 
__Polimorfismo__. Esse é a base, mas não é tudo.

Não somos diferentes. Eu pensava que programava Orientado a Objetos por 
usar e aplicar — ou achar que estava aplicando — esses conceitos básicos da 
Orientação a Objetos que nos ensinaram no meio acadêmico. Mas não foi o bastante.

Esqueceram de nos ensinar como __Pensar Orientado a Objetos__.

Mas isso é assunto para um próximo post.
