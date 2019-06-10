---
layout: post
title: "Classes Mutáveis vs Objetos Imutáveis"
date: 2019-06-10
permalink: /:title
description:
  Veja como implementar classes mutáveis que criam objetos imutáveis.
image: /images/2019/djim-loic-69263-unsplash.jpg
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
  - objetos mutáveis
  - objetos imutáveis
  - mutable
  - imutable
---

É possível ter a comodidade em utilizar classes mutáveis para construir os objetos aos poucos, configurando propriedades e opções de execução além do uso dos construtores, porém com a vantagem de obter instâncias imutáveis no final do processo.

E eu não estou falando do padrão Builder. Aliás você não deveria precisar construir uma classe para criar uma instância de outra classe...

<!--more-->

![Unsplash image]({{ page.image }})
<br><span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Djim Loic on Unsplash</span>

## Introdução {#introducao}

Os seres na natureza *não* são criados num único instante.

Um ser dentro do ventre de sua progenitora é "construído" aos poucos, parte por parte. 

Imagine todos os órgãos crescendo em um desenvolvimento constante.

Mas mesmo que esse ser ainda não tenha nascido, ele já existe.

Ele ocupa espaço.

Ele interage com o mundo através do corpo de sua mãe...

Porém ele ainda não é uma entidade 100% concluída ou estável.

O mesmo pode acontecer com um objeto no software.

O objeto pode não ter sido totalmente formado, mas ele já existe quando obtemos uma nova instância a partir do construtor de sua classe.

O objeto já existe mas pode não estar pronto para uso.

Faz sentido?

## Mutável vs Imutável {#mutavel-vs-imutavel}

Na internet muito tem se falado sobre utilizar objetos imutáveis considerando seus prós e contras.

Eu mesmo sou um defensor de [objetos imutáveis]({% post_url 2016-07-04-objetos-imutaveis %}).

Porém há casos onde os objetos, apesar de já ter "nascido", ainda não estão 100% pronto para uso.

Você pode estar pensando: "se podemos ter um objeto mutável, e se isso parece até estar de acordo com a natureza e de como o mundo realmente funciona, por quê eu iria querer ter um objeto imutável afinal?"

Objetos imutáveis são muito mais previsíveis, seguros e simples de usar. Isso é um fato.

No entanto, também haverá casos onde nem todos os objetos em um software poderão ser imutáveis. Vide *widgets* de formulários Desktop, por exemplo. Os Edits, Memos, Grids... todos são mutáveis.

Apesar da imutabilidade ser o ideal a seguir, ela não é uma verdade absoluta.

Então, se mutabilidade e imutabilidade tem seus prós e contras, e eventualmente iremos trabalhar com ambos, como decidir entre um e outro quando isso já não for pré-determinado?

Eu diria para utilizarmos classes mutáveis em conjunto com objetos imutáveis.

## Na Prática {#praticando}

Uma classe é a construtora de seus objetos — a progenitora.

Um objeto deveria ser uma entidade 100% pronta para uso.

Entretanto, assim como uma criatura ainda está sendo formada no ventre, um objeto ainda pode estar sendo construído.

Mas o programador pode determinar quando essa construção termina em um momento especial: quando a *instância objeto* se transforma em *instância de interface*.

Se você não tiver nenhum método na interface que promova a mutabilidade do objeto, você poderá construir seus objetos aos poucos utilizando a mutabilidade dos métodos da classe, porém com a vantagem de ter uma instância final imutável que só implementa métodos imutáveis da interface.

É como ter o melhor dos dois mundos.

Imagine uma classe que faz um processamento de Faturas — `TInvoiceMaker`.

Tal classe necessitaria de uma "configuração especial" sobre como fazer o processamento.

Essa configuração poderia depender de muitos fatores, como período do ano, tipos de contrato, tipos de produtos, promoções, se é apenas um teste ou se está executando em produção.

Estamos apenas especulando as opções.

Primeiro, iniciamos com a definição de uma interface que `TInvoiceMaker` deverá implementar:

    IDataMaker = interface
      procedure Make;
    end;

Veja que não há métodos que promovem a mutabilidade, ou seja, não há [Setters].

Uma instância `IDataMaker` seria passada como argumento para algum objeto/rotina "big process" que recebe um "maker", não importando quem:

    BigProcess(const aMaker: IDataMaker);

Internamente `aMaker.Make` será chamado.

## Modelo Imutável {#imutavel}

Então, vamos pensar como poderia ser essa implementação no "modelo imutável":

    TInvoiceMaker = class(TInterfacedObject, IDataMaker)
    public
       constructor Create(aPeriod: TPeriod; 
         aContracts: TContractTypes;
         aProducts: TProductTypes;  
         aPromotions: TPromotionArray); reintroduce;
       procedure Make;
    end;

O construtor da classe deverá ter *todas* as dependências injetadas no construtor.

Esse é um modelo sólido. Não há setters. Após o objeto ter sido construído, ele estará 100% pronto para uso.

Esse seria um modelo de implementação ideal.

A classe é pequena; não há muitos argumentos no construtor; fácil de instanciar; imutável.

Agora imagine essa classe seja utilizada em muitos lugares no código e que, na maioria das vezes, seus argumentos serão sempre os mesmos.

Mesmo que haja poucos argumentos no construtor, a repetição deles torna o código verboso e chato para o programador.

Se os contratos são os mesmos, os produtos os mesmos, promoções... por quê não definir valores padrão para tais atributos?

Então implementamos mais um [construtor secundário] que irá chamar o primário:

    TInvoiceMaker = class(TInterfacedObject, IDataMaker)
    public
       constructor Create(aPeriod: TPeriod; 
         aContracts: TContractTypes;
         aProducts: TProductTypes;
         aPromotions: TPromotionArray); reintroduce; overload;
       constructor Create(aPeriod: TPeriod); overload;
       procedure Make;
    end;

O construtor secundário irá passar valores padrão para os argumentos `aContracts, aProducts, aPromotions`. Apenas o período deverá sempre ser informado, por exemplo.

E, novamente, temos um design limpo e sólido.

Mas sabemos como é a vida de um desenvolvedor: os requisitos mudam; modelos mudam; não há tempo suficiente.

E se, agora, o desenvolvedor precisar informar o período e, também, os contratos? Não há um *overload* de construtor para informar apenas esses dois parâmetros.

Seguindo o método acima, o desenvolvedor tem duas opções: a) criar um novo construtor ou b) utilizar o construtor principal passando todos os parâmetros — o que eu vejo acontecer bastante em códigos de terceiros onde tais parâmetros são dispostos, por exemplo, assim `foo(true, true, '', '', false, '', 1)`, que é difícil de ler/entender.

Há os argumentos com valores padrão no Pascal. Mas eles não ajudariam muito se quisermos passar apenas 1 dos valores "no meio". Nesses casos você terá que digitar os valores padrão até chegar na posição do argumento que você deseja informar.

Voltando ao problema, o mais "correto", seguindo o modelo imutável, seria implementar um novo construtor:

    TInvoiceMaker = class(TInterfacedObject, IDataMaker)
    public
       constructor Create(aPeriod: TPeriod; 
         aContracts: TContractTypes;
         aProducts: TProductTypes;  
         aPromotions: TPromotionArray); reintroduce; overload;
       constructor Create(aPeriod: TPeriod; 
         aContracts: TContractTypes); overload;
       constructor Create(aPeriod: TPeriod); overload;
       procedure Make;
    end;

Infelizmente essa abordagem pode não ser estável no longo prazo, dependendo das mudanças.

Se os argumentos ou a *combinação* deles aumentarem, teria de haver muitos outros construtores com inúmeras combinações.

Esse é uma abordagem que funciona com classes estáveis com poucas mudanças, não sendo o ideal para classes que tem a tendência de mudar muito.

## Modelo Mutável {#mutavel}

Então, vamos pensar como poderia ser essa implementação no "modelo mutável":

    TInvoiceMaker = class(TInterfacedObject, IDataMaker)
    private
       fPeriod: TPeriod;
       fContracts: TContractTypes;
       fProducts: TProductTypes;
       fPromotions: TPromotionArray;
    public
       constructor Create(aPeriod: TPeriod); reintroduce;
       procedure Make;
       property Contracts: TContractTypes 
         read fContracts write fContracts;
       property Products: TProductTypes 
         read fProducts write fProducts;
       property Promotions: TPromotionArray 
         read aPromotions write fPromotions;
    end;

A classe tem um construtor primário que solicita o essencial: um período.

Todos os outros atributos que podem ter um valor padrão, poderiam ser implementados como propriedades.

A vantagem é óbvia: não precisamos construir inúmeros construtores com inúmeras combinações. Além disso, podemos adicionar mais propriedades a qualquer momento, com tanto que cada uma tenha um valor padrão que será inicializado no construtor primário.

Esse também é um um modelo limpo e (de certa forma) sólido.

Muitos desenvolvedores acham que a classe deve ter apenas os mesmos métodos que a interface que implementam. Ledo engano.

Há setters através das propriedades, porém cada uma delas terá um valor padrão definido no construtor. Então, o desenvolvedor não é obrigado a passar cada uma das propriedades.

Após o objeto ter sido construído, ele estará 100% pronto para uso.

Mas, como manter a imutabilidade utilizando essa classe?

Na verdade, a instância será virtualmente imutável quando você transformá-la em instância de interface.

Lembre-se que precisamos de uma instância `IDataMaker` para passar a uma rotina:

    /// imutable version
    procedure Execute(aPeriod: TPeriod);
    var
      maker: IDataMaker;  // interface
    begin
      maker := TInvoiceMaker.Create(
        aPeriod, TDefContracts.Create, 
        TDefProducts.Create, []);
      BigProcess(maker);
    end;

    /// mutable version
    procedure Execute(aPeriod: TPeriod);
    var
      maker: TInvoiceMaker; // class
    begin
      maker := TInvoiceMaker.Create(aPeriod);
      // changing default TDefProducts instance
      maker.Products := TOtherProducts.Create;
      BigProcess(maker);
    end;

Você pode ainda utilizar uma construção sem ter que definir uma variável, utilizando WITH and [REF]({% post_url 2018-08-13-metodo-ref %}):

    /// mutable version using WITH
    procedure Execute(aPeriod: TPeriod);
    begin
      with TInvoiceMaker.Create(aPeriod) do
      begin
        // changing default TDefProducts instance
        Products := TOtherProducts.Create;
        BigProcess(Ref);  // obj.Ref
      end;
    end;

## Conclusão {#conclusao}
    
A rotina `BigProcess` só sabe o que é um `IDataMaker` com apenas o único método `Maker`. É irrelevante para o processo se `aMaker` é mutável com propriedades ou totalmente imutável, implementando somente o único método da interface.

Dessa forma você teria classes mutáveis com outras propriedades e métodos gerando instâncias de objetos mutáveis. 

Porém, após as instâncias serem convertidas para instâncias de interface, você teria instâncias virtualmente imutáveis, provenientes de classes mutáveis.

Combinar classes mutáveis com objetos imutáveis pode ser uma escolha poderosa no design de sua aplicação, trazendo a flexibilidade da construção de instâncias mutáveis com a segurança das instâncias imutáveis, após serem convertidas.
  
Até logo.