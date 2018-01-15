---
layout: post
title: "Objetos Complexos"
date: 2018-01-15
permalink: /:title
description:
  Um Objeto não é apenas uma instância de uma Classe. Ele pode conter instâncias (quase) infinitas de diferentes Classes combinadas.
image: /images/2018/photo-chuttersnap-378918.jpg
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
  - objetos complexos
  - complex objects
  - how implement complex objects
---

Um Objeto não é apenas uma instância de uma Classe. Ele pode conter instâncias (quase) infinitas de diferentes Classes combinadas.

<!--more-->

![Unsplash image]({{ page.image }})
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by chuttersnap on Unsplash</span>

No mundo real convivemos constantemente com Objetos (humanos, animais, plantas, coisas, etc) que possuem todo tipo de [comportamento]({% post_url 2016-03-14-objetos-pensam-e-tomam-decisoes %}) especializado. Alguns desses Objetos — talvez a maioria — fazem coisas demais, ou seja, eles agregam muitas funcionalidades num único "corpo". Eles são *complexos*.

Quando tentamos abstrair e implementar um [Objeto ou Entidade]({% post_url 2016-02-29-objetos-representam-entidades %}) no *software*, utilizando uma linguagem de programação em conjunto com o paradigma da Orientação a Objetos, uma dúvida comum em quase todos os desenvolvedores iniciantes — ou mesmo experientes — é tentar identificar quais métodos irão ser implementados na Classe, baseado apenas no comportamento "final" do Objeto ou Entidade do mundo real.

O comportamento final ao que me refiro é como vemos um Objeto e suas funcionalidades sem considerar as partes que os compõem. Vemos um Objeto como um *todo* e não como uma soma das partes.

Esse é um grande equívoco no desenvolvimento de software.

Vejamos um exemplo. Como representamos um *Carro* utilizando a [Orientação a Objetos]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %})?

O primeiro passo seria determinar o nível de abstração dos comportamentos que precisamos implementar ou descartar. Em outras palavras, é necessário saber os requisitos do sistema para sabermos o que é realmente necessário.  Se a cor do carro é irrelevante para os requisitos do usuário dentro do *software*, então não há necessidade de implementarmos. É necessário saber se a carroceria utiliza fibra de carbono ou alumínio? Se o carro é 4x4, sua velocidade máxima, quantidade de airbags, tipo de roda? Se nada disso for importante, descartamos. Então, o mais fácil a fazer é pensarmos no que é *necessário*, pois é quase certo que qualquer Objeto implementado não irá representar uma entidade completamente, ou seja, com todo comportamento que a entidade possui no mundo real.

Após termos o comportamento necessário já definido, o próximo passo é pensar na estrutura da Classe.

É aqui, neste momento, que a grande maioria dos desenvolvedores utiliza o paradigma procedural, porém pensam que estão programando Orientado a Objetos apenas por quê estão utilizando Classes.

    type
      TCar = class
      public
        procedure Run;
        procedure Brake;
        function CheckBrakeFluid: Boolean;
        function CheckOil: Boolean;
        function CheckWater: Boolean;
        procedure LockDoors;
        procedure UnlockDoors;
        procedure StartEngine;
        procedure StopEngine;
      end;

A Classe acima mostra de forma simples os comportamentos necessários que foram solicitados, hipoteticamente, nos requisitos do usuário. Mas será esse é o design *correto* para representar um carro dentro do *software*?

Apesar das funcionalidades estarem bem evidentes e com [nomes de métodos]({% post_url 2016-07-25-nomeando-variaveis-e-metodos %}) bem "explicativos", essa é uma implementação totalmente equivocada.

Um carro não *corre*. É o motor que gira e, através de mecanismos e engrenagens, passa energia para as rodas fazendo o carro se mover através do atrito dos pneus com o asfalto.

Um carro não *freia*. É o motorista que através de um pedal, em conjunto com cabos ou mesmo eletrônica, envia um comando às pinças de freio. Essas, em conjunto com os discos de freio, fazem o carro parar.

Um carro não faz *checagens* de fluídos. Existem sensores específicos para fazer essa leitura a cada segundo ou microssegundo.

Fica claro aqui, que não devemos ver a entidade como um todo, mas sim pensar nas partes específicas que a compõe. No entanto, não devemos especializar demais para não complicarmos o modelo desnecessariamente.

Vamos reescrever a Classe acima.

    type
      TBrakes = class(TInterfacedObject, IBrakes)
      public
        function Brake(AForce: Double): IBrakes;
      end;
      
      TSensors = class(TInterfacedObject, ISensors)
      public
        function CheckBrakeFluid: Boolean;
        function CheckOil: Boolean;
        function CheckWater: Boolean;
      end;
      
      TDoors = class(TInterfacedObject, IDoors)
      public
        function Lock: IDoors;
        function Unlock: IDoors;
      end;
      
      TEngine = class(TInterfacedObject, IEngine)
      public
        procedure Start;
        procedure Stop;
        function Work: IEngine;
      end;
      
      TCar = class(TInterfacedObject, ICar)
      public
        procedure Run;
        function Brakes: IBrakes;
        function Sensors: ISensors;
        function Doors: IDoors;
        function Engine: IEngine;
      end;

Há várias possibilidade de implementação, mas a opção sugerida acima separa bem o comportamento de um carro através da composição de Objetos. No entanto, não há necessidade de especializar tudo. Por exemplo, o método `Run` continua o mesmo; existe apenas uma Classe `TSensors` para todos os sensores — seria muito melhor se tivéssemos uma Classe por sensor.

O design deve ser o mais simples possível, porém correto, reutilizável (partes independentes) e testável (cada parte em separado).

Na minha opinião, a [simplicidade]({% post_url 2016-12-19-simplicidade %}), a reutilização e a testabilidade devem ser alguns dos maiores objetivos a serem seguidos no design de cada Classe.

Essa foi uma maneira de implementar algo complexo de forma simples.

Um outro padrão de desenvolvimento que podemos utilizar para implementarmos algo complexo é chamado de [Decoração de Objetos]({% post_url 2016-05-02-decorator-pattern %}).

Imagine que você tenha diferentes modelos de motores. Utilizar [herança de Classes]({% post_url 2016-05-23-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-1 %}) pode ser a primeira técnica que vem a sua mente, porém a Decoração de Objetos pode ser bem melhor. Utilizando essa técnica, podemos decorar Objetos em *runtime* de acordo com os requisitos ou escolhas feita pelos usuários num determinado momento.

Então, seguindo nosso exemplo, imagine um motor chamado X13 e uma versão com mais performance chamada X15 que possui um turbo compressor. Ao implementarmos essas Classes, gostaríamos de reutilizar as partes em comum. Sendo o X15 uma melhoria do X13, que tal *decorar* o X13 em *runtime*?

    var
      Engine: IEngine;
    begin
      Engine := 
        TX15Engine.New(
          0.6, // pressure
          TX13Engine.New()
        );
    end

Nesse exemplo, a variável `Engine` será, de certa forma, um Objeto *combinado* de duas instâncias de Classes diferentes, porém complementares.

Para implementarmos Objetos complexos, devemos analisar suas partes, de acordo com o nível de abstração requerido, e implementar cada uma delas, combinando-as entre si, para então termos a implementação de um Objeto maior e mais *complexo*.

*"O todo é maior que a soma das partes".*

Até logo.