---
layout: post
title: "Herança pode ser o Mal da Orientação a Objetos — Parte Final"
date: 2016-06-20
description: Não faça da Herança a sua primeira escolha para reutilizar código.
summary: Não faça da Herança a sua primeira escolha para reutilizar código.
image: /images/photo-1457473075527-b0db85c08e66.jpg
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

No artigo anterior falei sobre Forte Acoplamento.
Nesse artigo irei falar sobre **Hierarquias Complexas** e concluir a série.

<!--more-->

![Imagem]({{ page.image }})

[Clique aqui]({% post_url 2016-06-13-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-4 %}) para ler a **Parte #4** dessa série, caso ainda não tenha lido.

##Introdução {#introducao}

A maioria dos desenvolvedores esqueceram ou nunca aprenderam que a Herança foi feita para proporcionar o Polimorfismo e não  para reutilizar código. 

O reuso de código é apenas um "efeito colateral" da Herança.

Polimorfismo é importante e em linguagens como C++, por exemplo, a única maneira de implementarmos Polimorfismo é através da Herança de Classes.
A linguagem C++ não tem Interfaces, sendo Herança de Classe uma escolha correta.

Arquitetos de linguagens de programação vem copiando as ideias de outras linguagens a muito tempo. Se uma linguagem nova for muito diferente das mais populares, corre o risco de nunca ter um público que as utilize. Então, por que não copiar caracteristicas de outras linguagens que já fazem sucesso?

Se C++ tem Herança de Classes, porque não ter em Java, Ruby, Object Pascal... os programadores já estão acostumados, então vamos implementar essa característica também.

Imagino que seja assim ao criar uma nova linguagem. Mas nem sempre. As linguagens funcionais são bem diferentes das imperativas. A Go Language, por exemplo, não é funcional mas também não implementa Herança — mas parece uma nova cópia da liguagem C, não?

Então, cada nova linguagem deve ter alguma referência anterior para não ser completamente alienígena. Pra mim, esse é o maior motivo de ainda termos Herança de Classes nas novas linguagens.

Se você programa em Object Pascal, no entanto, já tem o suporte a [Interfaces]({% post_url 2016-01-18-interfaces-em-todo-lugar %}), que é uma melhor opção para implementar Polimorfismo do que utilizar Herança de Classes.
Se podemos implementar o Polimorfismo através de Interfaces e reutilizar código através da Composição de Objetos e delegação, por que iríamos limitar nossas Classes a herdar de uma única hierarquia rígida de Classes?

Não faz sentido.

Por que uma Classe Gato deveria herdar de Animal, Mamífero ou quadrúpede se o sistema só precisa conhecer um Gato?
Essas implementações de Super Classes, muitas vezes, [nem existem]({% post_url 2016-06-06-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-3 %}#solucao) no modelo de negócios.

##Hierarquias Complexas {#hierarquias-complexas}

Eu defino Hierarquias Complexas todos os modelos de Classes que utilizam Herança e que contenham erros conceituais ou reais. E eu sempre encontro esses tipos de erros. Sempre. A não ser que seu modelo seja muito pequeno, ele também conterá esses erros. O motivo é simples. É quase impossível definir uma Hierarquia de Classes perfeita porque o mundo real, o qual modelamos utilizando Classes, não é estruturado.

Isso mesmo, a natureza não é estruturada. Não disse que não ela não é perfeita. Estou dizendo que ela não é estruturada como Classes perfeitamente agrupadas numa Hierarquia bonita e elegante. Na minha opinião, a natureza é um caos.

Na [Parte #3]({% post_url 2016-06-06-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-3 %}#exemplo-1) dessa série eu apresentei um modelo de Classes do Mundo Animal.

Aqui está o Diagrama:

![Diagram1](/images/Diagram02.png)

A princípio, parece simples e correto. Todos os animais no modelo são Animais, obviamente, e todos eles Caminham. Bem, com exceção do Tubarão, pois ele não tem pernas mas... vamos ignorar isso. Imagine um `raise Exception.Create` no método `Caminhar` do Tubarão e vamos em frente.

E se quisermos adicionar mais animais nessa Hierarquia? Talvez um Tigre.

Você pode imaginar que um Tigre herde de Gato — ou seria o contrário? — e tudo irá continuar funcionando. Bonito e elegante.

O mesmo para um Leão, Lobo, Urso... e então precisamos implementar um Ornitorrinco.

O que é um Ornitorrinco?

<blockquote>
Com bico de ave, semelhante a pato, é um mamífero semiaquático natural da Austrália e Tasmânia. É o único representante vivo da família Ornithorhynchidae, e a única espécie do gênero Ornithorhynchus. Juntamente com as equidnas, formam o grupo dos monotremados, os únicos mamíferos ovíparos existentes. A espécie é monotípica, ou seja, não tem subespécies ou variedades reconhecidas.
  <footer><cite title="wikipedia">— Wikipedia</cite></footer>
</blockquote>

É um mamífero, ovíparo, tem bico e nadadeiras e é uma mono espécie... se a natureza fosse estruturada, esse seria um dos seus *bugs*? 

Pense agora no peixe-voador...

Brincadeira a parte, eu acredito que não, que não há nada de errado com o Ornitorrinco ou qualquer outro animal.

Nosso Modelo de Classes, baseado em Herança, é que está errado.

Imagine o modelo acima bem maior, com mais métodos e Subclasses. Para tentar colocar o Ornitorrinco na Hierarquia você teria que fazer uma refatoração possivelmente grande. Criar novas classes abastratas, mover métodos para Classes acima, outros para Classes abaixo, "desabilitar" alguns métodos utilizando `raise` e seguir a vida.

A Hierarquia iria ficar **extremamente complexa**. Iriam existir Classes que só existiram para compartilhar código, e não porque o **modelo de negócios** exige que existam.

Sempre foi assim nos sistema que utilizam Herança, certo? Aquele *Form* que tem um botão que não é necessário na Subclasse, basta setar um `Visible = False` e tudo certo; um botão que faz algo a mais do que é preciso na nova Subclasse, basta "refatorar" o código dividindo em 2 métodos com nomes insípedos e fazer a chamada deles, sobrescrevendo o método original para chamar apenas uma parte do código; sobrescrever métodos da super Classe para "não fazer nada"; copiar código de outra hierarquia de Classes... uma verdadeira bagunça.

Eu conheço esses problemas. Eu passei por muitos deles. E, acredite, em grande parte o motivo dos problemas sempre foram as Hierarquias Complexas que criamos sem pensar, quando estamos tentando "reaproveitar" código utilizando Herança.

Na natureza, eu acredito, não há "reaproveitamento de código", só composição. 

Átomos, elementos químicos. Alguns organismos tem mais disso, outros mais daquilo. Um animal tem 0,0003% desse elemento, mais 0,0006% de outro. A composição desses Objetos determinam o comportamento e a evolução de cada indivíduo.

DNA. 

Lembre-se que [a árvore]({% post_url 2016-06-06-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-3 %}#solucao) não existe. Tetrápodes, Bípedes, Mamíferos, Anfíbios, Aves... nada disso existe. São abstrações que nós criamos para organizar as coisas, mas elas **não existem no mundo real**.

Qualquer Hierarquia de Classes que tente implementar o mundo, terá falhas.

Parece que os biólogos conseguem separar os indivíduos em "hierarquias perfeitas". Eles observam os indivíduos na natureza e classificam. Se descobrem um animal que não se encaixa em nenhuma hierarquia não há problema, basta criar uma nova. Nenhum animal já classificado será afetado. Não há um "fio" que liga os animais às hierarquias. 
O indivíduo determina a hierarquia, ou seja, de baixo pra cima. As hierarquias são apenas nomes e classificações. Ontologia. 

>A ontologia não define o comportamento das amostras individuais.

Nós, desenvolvedores de software, fazemos o inverso. Vamos codificando nossas hierarquias e tentando definir o comportamento dos objetos de cima pra baixo! É como se tentássemos prever o futuro da evolução de nossas Classes separando-as em hierarquias pré-definidas. É praticamente impossível dar certo.

##Solução {#solucao}

A solução é a Composição de Objetos em conjunto com Interfaces. Já falei isso. Falei e repeti muitas vezes. Mas para quem ainda não entendeu, vou revelar mais uma dica através de uma pergunta:

**Se seu sistema trabalha com Tigres, quantas Classes Tigre você teria?**

A grande maioria, acredito, responderia **apenas uma classe** `TTigre`. 

Concorda?

A pergunta é capciosa.

Esse é um dos problemas ao tentarmos modelar sistemas Orientados a Objetos. O desenvolvedor acha que apenas uma Classe deve representar o conceito quando, na verdade, podemos ter **diferentes Classes para representar o mesmo conceito em diferentes contextos**.

As pessoas me perguntaram quais eram as soluções para os problemas apontados nessa série. Eu lhes disse, Composição de Objetos.

Mas elas esperavam ler a solução definitiva: Quais classes criar; quais interfaces; usar ou não herança; em quais Classes por tais métodos.

Não há uma única resposta, porque tudo irá depender do seu modelo de negócios. Uma vez que você saiba o modelo de negócios, deverá haver a definição de contextos — Bounded Context no DDD — e assim você terá "N" [variações de um único conceito]({% post_url 2016-02-29-objetos-representam-entidades %}#o-que-e-uma-entidade).

Lembra do `TTigre`? Se nesse sistema hipotético você precisa analisar na tela a pata de um tigre, não haveria necessidade de instânciar um objeto da Classe `TTigre` que contém tudo de um tigre (olhos, tipo, peso, tamanho, cor, que corre, nada, caça, etc). Não estou dizendo isso pensando em performance ou alocação de memória, não! Estou dizendo isso porque essa Classe seria enorme e complexa. Iria implementar muitas interfaces para poder Correr, Nadar, Pular... num sistema precisamos abstrair o que não é necessário. Esse é um dos motivos de não precisarmos de Hierarquias Complexas. Se você precisa examinar uma pata, então:

{% highlight pascal %}
type
  IPaw = interface
    // function...
  end;
  
  TTigerPaw = class(TInterfacedObject, IPaw)
  public
    // function...
  end;
{% endhighlight text %}

Assim como você poderá utilizar os Objetos da Classe `TTigerPaw` para compor outras abstrações de Tigre em outros contextos.

Sim, você terá muito mais definições de Classes. Mas isso já é previsto na Composição de Objetos. Fazer o certo dá mais trabalho, mas só no início.

##Conclusão Final {#conclusao-final}

Chegamos ao final dessa série. Falei sobre como a Herança Viola o Encapsulamento, pode Duplicar o Código, promove Forte Acoplamento e
Hierarquias Complexas.

Espero ter lhe ajudado a entender mais sobre Herança de Classes e os males que ela pode trazer ao código.

O título dessa série é "Herança **pode** ser o Mal da Orientação a Objetos". Eu não falei que ela **é**, disse que pode ser.
Eu lhe mostrei os problemas que vem com a Herança de Classes. Tenha conhecimento deles e evite-os.

Herança de Classe pode ser benéfica em alguns (poucos) contextos. Mas ao usá-la uma vez, a tentação é grande em continuar utilizando. 
Não faça isso.

Modele suas Classes utilizando Composição de Objetos que implementam Interfaces. Esse é o segredo.

Até logo.
