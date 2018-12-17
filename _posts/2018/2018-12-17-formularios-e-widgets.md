---
layout: post
title: "Como Dividir e Organizar o Código em Formulários com Muitos Widgets"
date: 2018-12-17
permalink: /:title
description:
  Nesse artigo você poderá aprender algumas técnicas para se livrar dessas classes com dezenas de métodos, difíceis de entender e gerenciar
image: /images/2018/robert-bye-200739-unsplash.jpg
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
  - forms
  - datamodules
---



<!--more-->

![Unsplash image]({{ page.image }})
<br><span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Robert Bye on Unsplash</span>

## Introdução {#introducao}

Em um formulário com muitos widgets, haveria dezenas de métodos para controlá-los. Esses métodos lidariam com o estado visual (visível, checado, cores,...); com os dados (texto, grids, labels,...); mensagens (dialogs, popups,...) e todo tipo de interação entre os componentes visuais e não-visuais.

Definir dezenas de métodos em formulários e DataModules parece ser comum. O programador cria um DataModule com queries e componentes relacionados, acrescenta 50 métodos e considera ter feito um ótimo trabalho na reutilização e organização do código — no entanto ele só criou mais uma classe inchada, seguindo o anti-padrão conhecido como a [síndrome da classe Deus](http://wiki.c2.com/?GodClass).

Nesse artigo você poderá aprender algumas técnicas para se livrar dessas classes com dezenas de métodos, difíceis de entender e gerenciar.

## A culpa é do RAD? {#rad}

Na minha opinião, Delphi sempre foi a melhor IDE para desenvolvimento de aplicações Desktop — até surgirem opções como Lazarus — e grande parte desse sucesso é devido ao conceito e uso do RAD (Rapid Application Development).

Largue um componente no formulário, defina algumas propriedades, implemente alguns eventos e pronto. Fácil. Rápido. Mas talvez, sujo.

Para prototipagem é ótimo. Você consegue mostrar ao cliente algo funcionando em minutos. Mas para código real, para a manutenção, esse tipo de design pode se tornar um pesadelo no médio prazo.

Mas a culpa não é do RAD. Ele é apenas uma ferramenta. Ferramentas sozinhas não fazem mal projetos. Os desenvolvedores fazem.

RAD foi feito para agilizar o desenvolvimento, especialmente no design de formulários. Mas levaram esse conceito tão longe que muitos desenvolvedores só conhecem esse caminho para o desenvolvimento, desde formulários, regras de negócio, até a persistência.

RAD significou o sucesso mas também a "desgraça" do Delphi. É por causa do RAD que o Delphi (até hoje) não é bem visto em círculos de desenvolvimento mais voltados a regras de negócio, padrões de projetos, aplicações em camadas, etc.

Mas, novamente, não é culpa do RAD. Não há nada de errado com RAD. O errado e tentar implementar tudo no modelo RAD, na minha opinião.

É mais fácil largar componentes no formulário e setar propriedades e eventos, do que pensar em padrões de projeto, orientação a objetos e desenvolvimento em camadas.

RAD funciona, mas não é sustentável no longo prazo.

## Formulários sem Métodos {#forms}

O uso indiscriminado do RAD pode deixar a aplicação tão acoplada que seria muito difícil ter camadas separadas entre GUI, regras de negócio e persistência.

Acredito que para remover o conceito de "RAD para tudo", temos que começar com o básico, por exemplo, como desenvolver formulários com uma quantidade mínima de métodos.

A quantidade de métodos não está relacionado diretamente ao RAD — você pode ter uma classe com 100 métodos em qualquer linguagem, utilizando RAD ou não — no entanto, eu acredito que o RAD "estimula" essa prática, pois se todos os widgets e eventos estão declarados no formulário, onde mais eu iria implementar os métodos para lidar com esses componentes, certo?

Essa prática e muito comum, porém equivocada. Uma classe deveria ser pequena — menor quantidade de métodos possível — e coesa — todos os seus atributos são utilizados na maior quantidade de métodos possível. E fazer isso em um formulário e praticamente impossível.

A tendência é que cada novo método trabalhe com poucos widgets; os métodos terão uma nomenclatura tão verbosa quanto uma frase, pois todos irão fazer parte de uma mesma classe e não pode haver ambiguidade ou colisão de nomes.

Quantas vezes isso já aconteceu com você?

Seria possível termos formulários com o [mínimo de métodos](https://objectpascalprogramming.com/posts/menos-e-mais/#metodos) ou até nenhum e, mesmo assim, capazes de controlar as interações entre todos os widgets e componentes visuais e não-visuais?

A primeira prática que eu recomendo é separar widgets em frames — disponíveis no Delphi desde as primeiras versões através da classe `TFrame`.

Muitos formulários possuem *tabs* e cada tab pode ser um frame que desconhece completamente o formulário onde ele está contido.

Cada frame irá ter seu próprio conjunto de widgets, com nomenclatura simplificada, visto que não há colisões de nomes entre outros widgets se tudo estivesse em um mesmo formulário.

Esse é o *primeiro* nível da refatoração.

Mas isso não elimina a quantidade enorme de métodos que poderão existir, pois o desenvolvedor precisa por ação nos formulários e frames.

O segundo nível consiste em dividir para conquistar, eliminando quase que totalmente os métodos de um formulário.

## Dividir para Conquistar {#dividir}

Independente de ser um formulário, frame ou DataModule, considere todos como sendo [containers](2016-02-22-datamodule-e-apenas-um-container) de componentes (visuais ou não).

O objetivo é dividir os métodos em pequenos objetos que irão fazer uso dos componentes do container.

Mesmo que esse artigo se refira a formulários complexos, vou lhe dar um exemplo simples, mostrando que com apenas 1 widget, poderíamos adicionar vários métodos ao formulário, erroneamente.

Considere uma instância de um `TStatusBar` para exibir informações da aplicação.

O usuário precisaria ver a versão da aplicação; o database em uso; o status atual de algum processamento.

Um StatusBar possui a propriedade `Panels[]`que é como um array de objetos de texto — dado um índice, você altera o texto naquela posição.

Um programador iniciante talvez iria utilizá-lo dessa forma:

    StatusBar1.Panels[0].Text := version;
    StatusBar1.Panels[1].Text := database;
    StatusBar1.Panels[2].Text := 'Loading...';

Um programador mais experiente saberia que não se deve utilizar índices arbitrários pelo código — se quisermos mudar a posição de alguma informação, teríamos que percorrer todo o código para mudar os índices para novas posições. Esse programador provavelmente iria criar constantes para cada índice, o que resolveria a manutenção futura, no entanto teríamos constantes (verbosas) no código que estão "desacopladas" do objeto no qual elas atuam, ou seja, o StatusBar.

O código ficaria dessa forma:

    const
      STATUSBAR_VERSION = 0;
      STATUSBAR_DATABASE = 1;
      STATUSBAR_STATUS = 2;

    StatusBar1.Panels[STATUSBAR_VERSION].Text := version;
    StatusBar1.Panels[STATUSBAR_DATABASE].Text := database;
    StatusBar1.Panels[STATUSBAR_STATUS].Text := 'Loading...';

Veja o quão é verboso cada constante, pois é (quase) obrigatório utilizar prefixos para deixar claro em qual componente(s) esse  "grupo de constantes" trabalham.

Outro tipo de desenvolvedor iria utilizar métodos no formulário. O exemplo abaixo pode ser meio extremo, mas considere que eu não poderia dar um exemplo real pois não caberia nesse artigo. Portanto, utilizar tais métodos não estaria tão longe da realidade:

      procedure SetStatusBarVersion(const aValue: string);
      procedure SetStatusBarDatabase(const aValue: string);
      procedure SetStatusBarStatus(const aValue: string);

Criando métodos no formulário elimina as constantes e índices, porém incha a classe do formulário.

Veja, também, o quão é verboso cada método, novamente utilizando prefixos longos e entediantes — nomes longos em métodos e variáveis são sinais de que eles podem não fazer parte da classe onde estão.

Foram adicionados 3 métodos para lidar com apenas 1 componente que exibe texto.

Finalmente, um arquiteto de software teria objetivos maiores como: reutilizar os métodos em outro formulário ou projeto; simplificar o formulário, removendo métodos que lidam apenas com poucos componentes; retirar índices e constantes arbitrárias; trabalhar com o StatusBar sem pensar que ele é um StatusBar, abstraindo-o.

    TStatusBarWidget = class
    private
      fOrigin: TStatusBar;
      fVersion: string;
      fDatabase: string;
      fStatus: string;
    public
      constructor Create(aOrigin: TStatusBar); reintroduce;
      property Version: string read fVersion;
      property Database: string read fDatabase;
      property Status: string read fStatus write fStatus;
    end;

Essa classe iria receber a instância do StatusBar real no construtor, inicializada no construtor do formulário; iria obter os dados da versão e database automaticamente, disponibilizando propriedades somente leitura; o status poderia ser atualizado e lido dentro dos eventos do formulário.

    begin
       fStatusBar.Status := 'Loading...';
    end;

Toda a lógica referente ao StatusBar estaria encapsulada dentro da classe `TStatusBarWidget`, podendo ser reutilizada em outro lugar.

Outros programadores que porventura trabalhem no código, não iriam precisar lembrar de índices ou constantes arbitrárias. Eles iriam utilizar apenas um único objeto com métodos bem definidos para cada atividade, abstraindo sobre o que esta acontecendo internamente ao objeto — a atualização do StatusBar.

O formulário não teria nenhum método (relacionado a StatusBar), contendo apenas um atributo privado do tipo `TStatusBarWidget`.

O exemplo é simples, porém acredito que você tenha visualizado que poderá utilizar essa técnica não apenas para encapsular um componente, mas também para todo um conjunto deles, implementando toda a interação entre eles num único objeto.

## Conclusão {#conclusao}

Toda vez que você nomear um método com nomenclatura composta, pare e pense se esse método deveria estar em outra classe.

Não implemente vários métodos numa única classe. Separe-os em objetos menores e coesos, mesmo que esses objetos trabalhem diretamente com os componentes do container.

É melhor ter vários objetos pequenos, mesmo que fortemente acoplados, do que por tudo num único objeto onde a manutenção irá se tornar um caos em pouco tempo.

Até logo.

