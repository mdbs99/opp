---
layout: post
title: Nomeando Classes em Libraries
date: 2017-10-02
permalink: /:title
description:
  Veja nesse artigo alguns ideias que você deveria considerar ao nomear suas Classes em projetos do tipo Library.
image: /images/2017/photo-marcel-strauss-334864.jpg
categories:
  - Pascal
tags:
  - naming
keywords:
  - freepascal
  - fpc
  - delphi
  - lazarus
  - object-oriented
  - oop
  - mdbs99
  - lib
  - libs
  - libraries
  - open-source
  - naming
  - nomeando
---

Veja nesse artigo algumas ideias que você deveria considerar ao nomear suas Classes em projetos do tipo Library.

<!--more-->

![Unsplash image]({{ page.image }}) 
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Marcel Strauß on Unsplash</span>

## Introdução {#introducao}

Uma Library ou Lib, para encurtar, é definida na [Wikipedia](https://en.wikipedia.org/wiki/Library_(computing)) como "uma coleção de subprogramas utilizados no desenvolvimento de software".

Eu gosto mais da próxima definição: "Uma Lib é também uma coleção de implementações de [comportamento]({% post_url 2016-03-14-objetos-pensam-e-tomam-decisoes %})".

Não pense numa Lib somente em termos de arquivos dll/so. Vamos aumentar esse conceito. Pense em Libs como dll/so, pacotes de software ou até mesmo um programa. Pense em termos de definição de comportamento especializado.

Uma Lib é — ou deveria ser — um especialista em um assunto, dentro de um contexto bem definido.

Eu diria que é (quase) a mesma definição do Objeto.

No entanto, enquanto há Objetos tão simples com apenas um método, uma Lib parece mais como uma composição de muitos Objetos, trabalhando colaborativamente entre si, fazendo emergir um comportamento especializado.

Os Objetos são como peças intercambiáveis. Podem ser simples ou complexos através do uso da composição.

Uma Lib, eu acredito, é uma personificação de alguma entidade especializada.

E toda entidade precisa ter um nome.

## Nomeando uma Lib {#lib}

Há muitos anos eu já publicava alguns pequenos projetos no [SourceForge](https://sourceforge.net/).

A maioria desses projetos eram pequenos pacotes de componentes para Delphi mas, apesar de ter tido alguns pouquíssimos colaboradores, tais projetos nunca foram relevantes para a comunidade.

Bem, eu sabia quase nada sobre OpenSource e também não tinha muita experiência em desenvolvimento, então utilizei isso apenas como um aprendizado.

Na época eu nomeava meus pacotes de componentes com nomes abstratos. Eles não representavam uma entidade especializada. Eram apenas arquivos agrupados num pacote. Um bom nome não era relevante naquela época — eu pensava.

Anos se passaram.

Então, eu aprendi mais sobre [Orientação a Objetos]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}) e [semântica](https://en.wikipedia.org/wiki/Semantics) e como é importante utilizar bons nomes nos identificadores no código.

Dizem que *nomear coisas e invalidar cache* são as duas coisas mais difíceis de fazer na computação. E é verdade. 

Nomear coisas é difícil. Nunca fica perfeito. Escolhemos um nome pensando ser a melhor opção e daqui a duas horas estamos refatorando e renomeando...

É quase uma *arte*.

Muitas vezes demora mais tempo para descobrirmos que um nome não foi bem escolhido.

Há pouco mais de 5 anos atrás eu publiquei um projeto OpenSource chamado [Greyhound](https://github.com/mdbs99/greyhound), mas muitos não sabem qual o motivo da escolha desse nome. 

Aqui está a explicação: Enquanto a *Cheetah* (símbolo do Free Pascal) é o animal terrestre mais rápido, *Greyhound* é a raça mais rápida entre cães.

E o que esse nome tem haver com um projeto que abstrai entidades de um Banco de Dados? — você pode estar pensando.

É, concordo com você que esse nome não foi uma boa escolha.

Mas ainda acho que é um nome bem interessante. Talvez para uma Lib que trabalhe com *stream* de dados ou um novo sistema operacional. Talvez algo relacionado a velocidade? Não sei.

Pode não ter sido a melhor escolha para o meu projeto, mas por causa dele eu passei a utilizar nomes cada vez mais comuns. 

Hoje em dia todos os meus projetos (privados) ganham nomes próprios comuns. Alguns desses nomes provocam sorrisos em reuniões sempre que são pronunciados. Não devido ao nome em si, mas devido a relação do nome com o sistema que ele representa.

Eu também levei essa nomenclatura para o mundo OpenSource através das minhas Libs mais recentes: [James](https://github.com/mdbs99/james) e [Xavier](https://github.com/mdbs99/xavier).

Se esses são bons nomes ou não, você decide.

O aprendizado aqui é que bons nomes podem fazer a diferença pois o nome não será utilizado apenas na URL do seu projeto, mas sim em (quase) todas as Classes.

## Nomeando Classes {#classes}

Na época do SourceForge minhas Classes tinham minhas iniciais "MD" como prefixo ou qualquer outras "duas letras" abstratas.

Prefixos. Vemos essa prática ainda hoje em dia para diminuir a colisão de nomes entre classes similares. Exemplo: `TAbStream` vs `TCdStream`. 

Essa prática é visto até mesmo em grandes *frameworks* e outros ecosistemas como Java e C#.

Por quê? 

Bem, precisamos diferenciar a classe padrão `TStream` da sua `TMyStream`, por exemplo. 

Poderíamos utilizar a Unidade como prefixo, mas ninguém gosta de escrever o nome totalmente qualificado como `acme.streams.TStream` sempre que tiver que usar a Classe `TStream`.

Então o prefixo `My`, nesse caso, faz diferença.

No entanto, um grande problema é que não controlamos os prefixos que são utilizados em projetos ao redor do mundo. Eu quero dizer, por exemplo, que se você escolhe `Ab` como prefixo de suas Classes, é bem possível que exista alguma outra Lib que já utiliza esse prefixo — e tem, chama-se [Abbrevia](https://github.com/TurboPack/Abbrevia) e eu a utilizo.

O prefixo `Ab` deixa os identificadores pequenos — o que é bom — porém a ambiguidade continua existindo, visto que, por serem apenas 2 letras, a probabilidade é que outro desenvolvedor já tenha escolhido esse mesmo prefixo. 

E esse não é o maior problema.

É comum utilizarmos prefixos em grandes sistemas, compostos de muitos módulos, representando implementações diferentes para um mesmo conceito. Então se um desses prefixos for `Ab` e já estiver sendo utilizado dentro de um grande sistema — imagine que esse prefixo faz todo o sentido para o sistema da empresa, podendo representar uma área, setor ou algo bem importante — a ambiguidade é ainda pior, visto que poderia confundir o desenvolvedor: "Essa é uma Classe da empresa ou da Lib?".

É claro que basta posicionarmos o mouse em cima do identificador para vermos um *hint* com a informação de qual unidade ele pertence. Mas, e se você já utiliza o mesmo nome como `TAbStream` (exemplo dado acima) dentro um grande sistema? Você muda o identificador que faz mais sentido para empresa para não conflitar com o identificador da Lib ou [redefine]({% post_url 2017-08-21-redeclarando-classes %}) todas as Classes da Lib?

> "Nomear coisas e invalidar cache são as duas tarefas mais difíceis na computação."

Acredito que muitos desenvolvedores nem sequer pensam nesses conflitos.

"Ei! O código está funcionando, é o que importa", eles dizem.

Bem, existem bons profissionais, outros nem tanto.

E como resolver esse impasse? 

Como ter identificadores curtos, porém com uma boa semântica? 

Como diminuir ou (quase) acabar com conflitos de nomenclatura?

Minha única resposta é: Não use prefixos abstratos em Classes, use um *nome* de verdade.

O primeiro passo é escolher um bom nome: Simples, memorável e que represente um [contexto]({% post_url 2016-10-03-delegacao-de-implementacao-de-interfaces %}#contextos).

## Contextualizando {#contextualizando}

Um contexto encapsula uma possível implementação para um determinado problema.

Ao invés de utilizar `TAbStream`, você utilizaria `TAbbreviaStream`, por exemplo — eu não sei se tal Classe realmente existe nessa Lib.

Não parece muito, mas essa pequena alteração pode fazer uma grande diferença em médios a grandes sistemas.

Ao invés de termos apenas 2 letras abstratas, temos um nome que identifica um contexto.

Esse contexto é o mesmo que dizer: "Essa é a implementação de Abbrevia para representar um *Stream*" — essa seria a "tradução" ao vermos a Classe `TAbbreviaStream` declarada no código.

É muito menos provável que outra Lib, com a mesma especialidade, utilize o mesmo nome. A probabilidade diminui ao utilizarmos mais letras, concorda?

Além disso, é muito menos provável que você utilize nomes assim para representar contextos (área, setor ou algo bem importante) dentro de um sistema da empresa.

## Conclusão {#conclusao}

Uma Lib é um especialista em determinado assunto. Dar um nome memorável, apropriado e curto a sua Lib irá facilitar tanto a identificação das Classes quando o uso da mesma.

Evite o uso de prefixos abstratos. Eles não eliminam a ambiguidade e podem tornar as coisas ainda mais confusas, caso você tenha outros pacotes que, por ventura, utilize o mesmo prefixo.

Escolha nomes simples, de fácil memorização, curtos o suficiente, que tenham alguma relação ou que personifique a entidade especializada.

Até logo.

