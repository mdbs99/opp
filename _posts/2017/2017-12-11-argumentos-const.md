---
layout: post
title: "Mais Performance usando Argumentos \"const\" para Interfaces"
date: 2017-12-11
permalink: /argumentos-const
description:
  Se você utiliza instâncias de Interfaces em todos os lugares e quer aumentar um pouco a performance do seu código, leia esse artigo.
image: /images/2017/photo-alex-holyoake-334209.jpg
tags:
  - OOP
keywords:
  - freepascal
  - fpc
  - delphi
  - lazarus
  - object-oriented
  - oop
  - mdbs99
  - argumento const
  - delphi const parameter performance
---

Se você utiliza instâncias de Interfaces em todos os lugares e quer aumentar um pouco a performance do seu código, leia esse artigo.

<!--more-->

![Unsplash image]({{ page.image }})
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Alex Holyoake on Unsplash</span>

## Introdução {#introducao}

Em um código verdadeiramente Orientado a Objetos, as instâncias dos Objetos devem ser do tipo Interface em [todos os lugares]({% post_url 2016-01-18-interfaces-em-todo-lugar %}) do código.

Além de prover um contrato entre os Objetos, as instâncias de Interfaces são auto-gerenciadas, ou seja, a memória é liberada automaticamente pelo compilador.

No Free Pascal, no entanto, podemos ter instâncias de Interfaces que não são auto-gerenciadas. Essas interfaces utilizam o modelo [CORBA](https://www.freepascal.org/docs-html/prog/progsu37.html) com a directiva `{$interfaces corba}`.

Porém, o *default* de uso de Interfaces é o modelo COM, com auto-gerenciamento de memória através de um contador interno de referências à instância.

Esse auto-gerenciamento tem um custo computacional de execução, mas pode ser extremamente minimizado com apenas uma pequena alteração no código.

Entretanto, para você entender como essa pequena alteração irá afetar a performance do seu código, antes você precisa saber como o compilador faz o auto-gerenciamento de memória de instâncias de Interfaces.

## Métodos Especiais {#metodos-especiais}

Todo Objeto que implementa uma Interface COM — `IUnknown` — deve implementar 3 Métodos especiais que não fazem parte da Interface.

Funciona assim:

Quando criamos uma instância do tipo Interface, o compilador irá chamar um método especial do Objeto chamado [_AddRef](https://www.freepascal.org/docs-html/rtl/system/iunknown._addref.html).

Esse método irá aumentar uma contagem de referência, toda vez que uma nova variável receber a mesma instância do Objeto.

Da mesma forma, toda vez que uma variável sair do escopo de execução, o compilador irá chamar o método [_Release](https://www.freepascal.org/docs-html/rtl/system/iunknown._release.html) para decrementar a contagem.

Quando a contagem de referência — o atributo `FRefCount: LongInt` — chegar a zero, o destrutor do Objeto será executado e a memória será liberada.

Esse mecanismo é muito, muito mais simples do que ter um *garbage collector* sendo executado em paralelo, como é feito em outras linguagens.

Existe mais um terceiro e último método chamado [QueryInterface](https://www.freepascal.org/docs-html/rtl/system/iunknown.queryinterface.html).

Esse método é utilizado quando fazemos [*casting*]({% post_url 2016-04-18-nao-utilize-casting %}) do Objeto para Interface utilizando a estrutura `Intf := Obj as IMyInterface;` ou quando utilizamos a função [Supports](https://www.freepascal.org/docs-html/rtl/sysutils/supports.html) para determinar se uma instância implementa ou não uma determinada Interface.

Todas as Classes que implementam uma ou mais Interfaces, devem ter esses 3 métodos implementados.

Esse é o motivo de herdarmos quase todas as nossas Classes de `TInterfacedObject`, pois ela já contém a implementação destes métodos para lidar com a contagem de referência.

Implementamos esses métodos apenas para o compilador e é possível sabermos o momento no qual o compilador irá chamá-los, sobrecarregando o código com execuções além da implementação padrão dos Métodos do Objeto.

Então, se conseguirmos evitar tais chamadas, nosso código terá mais performance pois ciclos de CPU serão poupados.

## Sobrecarga {#sobrecarga}

Devido as chamadas automáticas de `_AddRef` e `_Release` em toda atribuição e saída de escopo das variáveis, muitos desenvolvedores acham que essa é uma *grande* sobrecarga para o código.

Na Engenharia nada é grátis, havendo sempre prós e contras.

Se por um lado temos o auto-gerenciamento de memória para instâncias de Interfaces, por outro temos essa sobrecarga do incremento e decremento da contagem de referência.

Além disso, toda vez que `_Release` for chamado, haverá uma checagem para saber se a contagem chegou a zero para que o destrutor do Objeto também seja chamado.

Eu acredito que essa é uma sobrecarga *mínima*, considerando todas as *vantagens* que temos ao utilizar instâncias de Interfaces.

Mas nem todos os desenvolvedores pensam da mesma forma.

De fato, existem algoritmos que não podem se dar o luxo de perder ciclos de CPU fazendo operações de infraestrutura da linguagem — incremento/decremento da contagem de referência.

Entretanto, a linguagem Object Pascal nos dá uma "saída" bastante elegante para a diminuição de toda essa sobrecarga.

## Argumentos "const" {#argumentos-const}

Sempre que uma instância for passada por parâmetro a outro Objeto através de um argumento, haverá o incremento da contagem de referência.

A menos que você utilize Argumentos "const".

Vamos chamá-los de "*ConstArgs*", para encurtar.

Aqui está a definição, em tradução livre, de [const](https://www.freepascal.org/docs-html/ref/refsu67.html) na documentação do Free Pascal:

*"Especificar um parâmetro como Constante está dando ao compilador uma dica de que o conteúdo do parâmetro não será alterado pela rotina chamada. Isso permite que o compilador execute otimizações que não poderia fazer de outra forma, e também para executar determinadas verificações no código dentro da rotina: ou seja, pode proibir atribuições ao parâmetro. Além disso, um parâmetro const não pode ser transmitido para outra função que requer um parâmetro variável: o compilador pode verificar isso também. O principal uso para isso é reduzir o tamanho da pilha, portanto, melhorar o desempenho e ainda manter a semântica de passagem por valor..."*

Eles não falam nada sobre utilizar "const" para argumentos do tipo Interface, no entanto eu posso lhe afirmar que utilizá-los irá aumentar a performance do seu código.

O motivo é simples: *ConstArgs* não executam `_AddRef` e `_Release`.

Isso quer dizer que o incremento/decremento do atributo de contagem de referência não irão ocorrer.

A checagem se o contador interno chegou a zero também não irá existir no ASSEMBLY final gerado pelo compilador.

Vou demostrar isso, começando com a unidade base de todos os exemplos:

<script src="https://gist.github.com/mdbs99/d7583833ddaf07df796321c95ee8faa8.js"></script>

A `Unit1` contém uma Interface e uma Classe que a implementa.

Os Métodos `_AddRef` e `_Release` foram sobrescritos para termos o controle do que ocorre dentro deles quando o compilador chamá-los.

Então vamos criar o primeiro programa de teste que utiliza a `Unit1`:

<script src="https://gist.github.com/mdbs99/ac9932e2b0e5a3525fdf05223051d5c9.js"></script>

Esse é um programa *CLI* (*command-line*) bem simples.

É necessário setar a utilização do [*heaptrc*](http://wiki.freepascal.org/heaptrc) no Lazarus para mostrar os vazamentos de memória (se houver) no final da execução do programa.

Então, após a execução, no meu laptop a saída foi essa:

    c:\temp>project1.exe
    _AddRef called
    RefCount is 1
    _AddRef called
    RefCount is 2
    Acting...
    _Release called
    _Release called
    RefCount is 0
    Destroing...
    Heap dump by heaptrc unit
    64 memory blocks allocated : 1593/1720
    64 memory blocks freed     : 1593/1720
    0 unfreed memory blocks : 0
    True heap size : 196608 (80 used in System startup)
    True free heap : 196528

A execução foi bem sucedida, e não contém vazamentos de memória.

Porém, veja que `_AddRef` e `_Release` foram, ambos, chamados 2 vezes cada. Isso ocorre por quê, ao criar a instância em `A`, há o incremento e ao passar essa instância para `Execute`, há um novo incremento da contagem.

Agora apenas altere a assinatura do procedimento, adicionando `const`:

    procedure Execute(const A: IAction);
 
Execute novamente.

Esse é o resultado por aqui:

    c:\temp>project1.exe
    _AddRef called
    RefCount is 1
    Acting...
    _Release called
    RefCount is 0
    Destroing...
    Heap dump by heaptrc unit
    64 memory blocks allocated : 1593/1720
    64 memory blocks freed     : 1593/1720
    0 unfreed memory blocks : 0
    True heap size : 196608 (80 used in System startup)
    True free heap : 196528

A mesma memória foi consumida, mas `_AddRef` e `_Release` foram chamados apenas 1 vez cada.    
    
Isso se traduz em *mais* performance ou *menos* sobrecarga, dependendo do seu ponto de vista.

Imagine se esse mesmo procedimento fosse chamado 100 vezes. Sem o "const" haveria mais de 100 chamadas aos Métodos especiais.

Mas, se é tão simples eliminar essa sobrecarga apenas especificando o tipo do argumento, por quê nem todos os desenvolvedores utilizam essa técnica?

Talvez seja apenas falta de conhecimento ou talvez eles tenham lido sobre problemas obscuros de vazamento de memória com o uso de *ConstArgs* e Interfaces.

## Argumentos e Interfaces {#problemas}

Há alguns artigos na Internet dizendo que temos que ter [cuidado](https://pascal.today/2016/12/16/take-care-of-const-and-interface-parameters/) ao utilizarmos *ConstArgs* em conjunto com instâncias de Interfaces.

Sim, *devemos* ter cuidado.

**O código realmente pode ficar intrincado e vazamentos de memória podem ocorrer** em lugares onde tudo parece estar correto.

Para demonstrar vamos a outro exemplo:

<script src="https://gist.github.com/mdbs99/979c82ffe9288ec129b5792cfef0d9b0.js"></script>

É o mesmo programa anterior, porém agora a instância de `TAction` é criada "inline".

Após executar, esse é o resultado por aqui:

    c:\temp>project1.exe
    Acting...
    Heap dump by heaptrc unit
    64 memory blocks allocated : 1593/1720
    63 memory blocks freed     : 1577/1704
    1 unfreed memory blocks : 16
    True heap size : 229376 (80 used in System startup)
    True free heap : 229200
    Should be : 229216
    Call trace for block $01872E88 size 16
      $004015C7  main,  line 14 of C:/temp/project1.lpr

Veja que os Métodos especiais não foram executados e há um vazamento de memória, o que não é nada bom.

Mas se você retirar o "const" do procedimento, o vazamento é corrigido.

Se o argumento não for um *ConstArgs* ele poderá incrementar a contagem e não haverá vazamentos de memória.

Por isso há o mito de que "não devemos utilizar *ConstArgs* com Interfaces" ou que devemos evitar seu uso.

**Felizmente a solução para toda essa complexidade é muito simples**, bastando utilizar uma técnica que eu publiquei a quase dois anos atrás, na qual eu chamo de [Método New]({% post_url 2016-01-10-interfaces-e-o-metodo-estatico-new %}).

Altere `TAction` adicionando o Método `New`:

    TAction = class(TInterfacedObject, IAction)
    public
      class function New: IAction;
      // ...
    end;
    
    implementation
    
    class function TAction.New: IAction;
    begin
      Result := Create;
    end;

Altere também o programa, dessa forma:

    begin
      Execute(TAction.New);
    end.
    
Execute e veja o resultado você mesmo.

## Conclusão {#conclusao}

Se você criar suas instâncias utilizando `New` — um método da Classe — ao invés de chamar diretamente os [construtores]({% post_url 2016-03-21-construtores-da-classe-primario-secundarios %}), eu lhe asseguro que você não terá problemas de vazamento de memória, se a utilizar corretamente.

Vimos nesse artigo que uma simples mudança no código pode gerar um ganho considerável de performance.

Utilizando *ConstArgs* em conjunto com a técnica chamada [Método New]({% post_url 2016-01-10-interfaces-e-o-metodo-estatico-new %}), você poderá deixar seu código mais rápido e sem vazamentos de memória.

Até logo.
