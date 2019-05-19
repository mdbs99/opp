---
layout: post
title: "Implementando Interfaces Utilizando Diferente Assinaturas de Métodos"
date: 2019-05-20
permalink: /:title
description:
  Como implementar em uma única classe, duas ou mais interfaces que tenham métodos com o mesmo nome e argumentos?
image: /images/2019/raka-rachgo-276831-unsplash.jpg
tags:
  - Object Pascal
keywords:
  - freepascal
  - fpc
  - delphi
  - lazarus
  - pascal
  - object pascal
  - object-oriented
  - oop
  - mdbs99
  - interfaces
  - multiple-interfaces
  - inheritance
  - herança múltipla
---

Como implementar em uma única classe, duas ou mais interfaces que tenham métodos com o mesmo nome e argumentos?

Talvez o pensamento mais natural à essa pergunta seria: "por quê eu iria querer fazer isso?".

Se você nunca precisou implementar essa situação antes, pode ser difícil imaginar tal cenário agora. Mas vou lhe mostrar que ele existe e que não é só possível implementá-lo como também é um requisito você conhecê-lo.

<!--more-->

![Unsplash image]({{ page.image }})
<br><span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Raka Rachgo on Unsplash</span>

Estávamos conversando sobre orientação a objetos em um chat privado entre amigos quando uma dúvida pairou no ar.

Não era exatamente uma dúvida, mas mais um desdém sobre como a linguagem C++ consegue implementar herança múltipla sem ser caos total.

Como a maioria dos desenvolvedores lá utilizam Java, e como essa linguagem implementa herança simples — assim como Object Pascal — não conseguiram visualizar um cenário onde seria possível duas classes terem um método com o mesmo nome e argumentos, enquanto uma terceira classe deveria herdar de ambas.

Nesse momento eu me lembrei de alguns casos de implementação em Object Pascal, onde essa situação é totalmente possível quando implementamos múltiplas interfaces.

Object Pascal não tem herança múltipla como C++, mas podemos implementar mais de uma interface na mesma classe e é bem possível que uma ou mais interfaces tenham nomes e argumentos idênticos.

Como você codificaria esse cenário?

Pelo que eu pude entender lá na discussão, assim como também [pesquisando na internet](https://stackoverflow.com/questions/2801878/implementing-two-interfaces-in-a-class-with-same-method-which-interface-method), Java não tem suporte no nível da linguagem para fazer tal construção ou diferenciação de métodos de interface com o mesmo nome e argumentos — e acho incrível ninguém falar sobre isso.

É também interessante que praticamente todas as respostas a essa dúvida, no meio Java, tenham a mesma convicção que poderia ser resumida assim: se duas interfaces tem o mesmo nome, elas *não* deveriam ter implementação diferenciada nas classes que as implementam.

Eles estão enganados.

Nomes de métodos iguais, mesmo com argumentos iguais, podem exigir implementação diferenciada devido a *semântica das interfaces* e não devido a assinatura de seus métodos.

Quando estamos definindo uma interface, deveríamos pensar apenas no contrato que ela representa e não como iremos implementá-la em classes mais tarde. Dessa forma, em teoria, não deveríamos nos preocupar se haveria algum conflito de nomenclatura na implementação das classes, pois isso é apenas um *detalhe da implementação*.

Felizmente, fazer essa diferenciação e implementação utilizando a linguagem Object Pascal não é só possível como também existe uma sintaxe *específica* para a codificação.

A Embarcadero chama essa sintaxe de *[Method Resolution Clause](http://docwiki.embarcadero.com/RADStudio/Rio/en/Implementing_Interfaces#Method_Resolution_Clause)*.

Agora que você sabe a sintaxe, consegue imaginar cenários para sua utilização?

Vamos enumerar alguns possíveis cenários:

**O primeiro cenário** está no próprio exemplo da Embarcadero, e mesmo não tendo nada haver com herança múltipla de interfaces, ainda é um necessário válido.

Lá, a explicação é que a interface `IMalloc`  possui os métodos `Alloc` e `Free` e precisamos implementá-la na classe `TMemoryManager`.

Poderíamos implementar um método `Alloc` — mesma nomenclatura da interface — ao invés do `Allocate` sugerido no exemplo. E tudo bem.

Mas não poderíamos definir `Free` — o mesmo da interface — pois esse é um método existente na classe `TObject`, a classe de onde todas as outras herdam. 

O método `Free` libera o próprio objeto, chamando seu destrutor. Seria esse o objetivo do programador que definiu `IMalloc` em primeiro lugar, ou seja, desalocar a própria instância?

Se a resposta for *sim*, então não teríamos problemas pois `Free` já está implementado em todas as classes. Feito.

Mas se a resposta for *não*, teríamos que fazer o que a documentação da Embarcadero sugere, definindo um novo *método de implementação* para implementar o *método da interface* afim de não chamar o `Free` padrão de `TObject`.

**Um segundo cenário** é sobre reutilização de classes legadas que existiam antes da introdução de alguma interface no código.

Imagine que você tem uma classe que define alguns métodos que implementam mensagens para o usuário em um programa Desktop:

    TUserMessages = class
    public
      procedure MsgWarning(const aText: string);
      procedure MsgError(const aText: string);
      function MsgQuestion(const aText: string): boolean;
    end;

Os métodos `MsgWarning` e `MsgError` apenas exibem uma mensagem ao usuário com os respectivos ícones de aviso ou erro. Já o método `MsgQuestion` retorna `true` ou `false` dependendo da escolha do usuário.

Então o sistema evoluiu. Começaram a pensar em introduzir TDD para codificar os testes de regressões. Seria necessário introduzir uma interface para essas interfaces, pois não iriam querer visualizar tais mensagens ao executar os testes automatizados. Seria necessário criar uma classe *fake* para essas mensagens, implementando uma nova interface:

    IUserMessages = interface
      procedure Warning(const aText: string);
      procedure Error(const aText: string);
      function Question(const aText: string): boolean;
    end;

Repare que os nomes dos métodos ficaram menos redundantes, pois eles não tem mais o prefixo "Msg" como os da classe original.

Enquanto essa nova interface vai sendo introduzida no código, aos poucos, o que antes estava funcionando deve continuar assim. Isso significa que várias partes do código ainda continuarão utilizando a classe `TUserMessages` original até a completa refatoração.

Isso quer dizer que não podemos utilizar a classe original para implementar a nova interface fazendo, assim, que essa classe possa trabalhar tanto no código "antigo" como também no "novo"?

Claro que podemos:

    TUserMessages = class(TInterfacedObject, IUserMessages)
    private
      procedure IUserMessages.Warning = MsgWarning;
      procedure IUserMessages.Error = MsgError;
      function IUserMessages.Question = MsgQuestion;
    public
      procedure MsgWarning(const aText: string);
      procedure MsgError(const aText: string);
      function MsgQuestion(const aText: string): boolean;
    end;

Dessa forma `TUserMessages` pode continuar a ser utilizada para construir instâncias da classe e continuar utilizando os métodos (antigos) com prefixo "Msg", assim como pode ser utilizada no novo código como uma instância da interface `IUserMessages`.

**Um terceiro cenário** é sobre a implementação de múltiplas interfaces. 

A sintaxe para fazer isso você, agora, já sabe. Mas talvez ainda possa lhe faltar a imaginação onde esse cenário seria adequado ou mesmo requerido.

Por quê, você diria, eu iria querer implementar 2 ou mais interfaces utilizando diferentes assinaturas de métodos?

A resposta irá depender da sutil diferença semântica das interfaces que serão implementadas.

Imagine que tenhamos que criar 2 interfaces que são *similares*, porém *diferentes*. Uma interface irá representar um Avião (`IPlane`) e outra um Planador (`IGlider`).

Eu não entendo tanto assim de aviação, porém podemos enumerar algumas diferenças entre ambos:

- o avião tem motor; o planador não;
- o avião é muito mais pesado que o planador;
- o avião pode levantar voo por ele mesmo; o planador precisa de outro avião para rebocá-lo ao ar;

Agora imagine que uma empresa revolucionária acaba de lançar uma máquina que pode ser utilizada tanto como um avião como um planador.

Incrível? Nem tanto, eu acho.

Esses híbridos nascem o tempo todo: carros elétricos mas que também utilizam combustível fóssil; carros que andam na terra, mas num clique de um botão viram uma espécie de lancha para andar na água; até mesmo um [carro/helicóptero](https://www.youtube.com/watch?v=_v1wc0bD_Cg) já foi inventado; skates com motor a combustão ou elétrico; bicicletas elétricas, mas você pode continuar pedalando; etc.

Na minha imaginação, essa nova máquina — será que já existe? — teria alguns requisitos básicos:

- ser feita de fibra de carbono para ser o mais leve e resistente possível;
- possibilidade de acoplar um motor plug-n-play para se transformar num avião;
- ter configurações diferentes para o "modo avião" vs "modo planador";
- no modo avião, levantar voo sozinha sendo auto-propulsionada; 

Pensando no código, vamos retornar as interfaces e defini-las:

    IPlane = interface
      procedure Fly;
    end;

    IGlider = interface
      procedure Fly;
    end;

É isso. Não vamos complicar. São duas interfaces com apenas um método. Ambos são idênticos, ou seja, mesma assinatura.

Nosso novo, incrível e imaginável veículo, representado por uma classe, deveria implementar o método `Fly()` apenas uma única vez?

Minha resposta é sonoro *não*. Mas se você continua achando que sim, como uma única classe iria implementar os dois modos de voos tão distintos?

Como explicado acima, o veículo pode ser tanto um avião como um planador. O usuário pode ou não utilizar um motor e, assim sendo, utilizar configurações de voo completamente diferentes.

Apesar do avião e do planador voarem, ambos fazem de maneira completamente diferentes, ou seja, com *implementações* diferentes.

Precisamos de um nome para esse incrível veículo. Então sugiro batizar essa invenção de "OnePlane".

Então teríamos a implementação da classe:

    TOnePlane = class(TInterfacedObject, IPlane, IGlider)
    private
      // real flying implementation methods
      procedure FlyAsPlane;
      procedure FlyAsGlider;
      // method resolution clause
      procedure IPlane.Fly = FlyAsPlane;
      procedure IGlider.Fly = FlyAsGlider;
    public
      // will initialize the instance as a plane or glider
      constructor Create(asPlane: boolean = true); reintroduce;
    end;

A classe acima exemplifica como implementar 2 métodos idênticos de interfaces distintas em uma mesma classe utilizando implementações completamente diferentes.

A sintaxe é tão simples e explícita que até mesmo desenvolvedores de outras linguagens, como Java, poderão entender essa implementação sem problemas, eu acredito.

Apesar da linguagem Object Pascal ter [outra sintaxe]({% post_url 2016-10-03-delegacao-de-implementacao-de-interfaces%}) ainda mais *limpa* para implementar mais de uma interface em uma mesma classe, ainda assim esse código parece perfeitamente razoável pra mim. Iniciaríamos a implementação utilizando [métodos privados]({% post_url 2017-01-30-metodos-privados %}) que depois poderiam ser convertido em novas classes, se necessário.

Finalmente, essa sintaxe não é novidade no Free Pascal nem no Delphi. Acredito que ela foi implementada quando surgiram as interfaces. Mas tenho certeza que você poderá utilizá-la perfeitamente na versão Delphi 7 de 2002 em diante, como eu continuo utilizando em vários projetos até hoje.

Até logo.

