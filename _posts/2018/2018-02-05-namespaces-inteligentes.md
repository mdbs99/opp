---
layout: post
title: "Namespaces Inteligentes"
date: 2018-02-05
permalink: /:title
description:
  Na minha opinião, a implementação atual de Namespaces no Delphi poderia ser muito mais inteligente, simples e sem ambiguidades.
image: /images/2018/photo-fervent-jan-121249.jpg
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
  - namespaces
  - unit
---

Na minha opinião, a implementação atual de Namespaces no Delphi poderia ser muito mais inteligente, simples e sem ambiguidades.

<!--more-->

![Unsplash image]({{ page.image }})
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Fervent Jan on Unsplash</span>

## Introdução {#introducao}

A linguagem Pascal foi criada com o conceito de Namespace desde o início chamado *Units*. As Unidades agrupam os identificadores, variáveis, tipos, classes...tudo que faz parte de um mesmo contexto.

No entanto, hoje em dia nós, desenvolvedores Object Pascal, ganhamos uma definição diferente para Unit e Namespace.

Talvez, devido a influência de outras linguagens "mais modernas", ter apenas Unidades não parece ser mais suficiente para a modularização de sistemas.

Concordo que ainda há espaço para melhorias no conceito das Unidades, porém a implementação proposta pela Embarcadero é suficiente para, agora, dizermos que a linguagem foi atualizada e que Namespaces é uma evolução?

#Namespaces Atual {#atual}

A partir do Delphi 2007~2009 a Embarcadero implementou o "novo" conceito chamado [Namespace](http://docwiki.embarcadero.com/RADStudio/Tokyo/en/Using_Namespaces_with_Delphi), onde é possível termos nomes de Unidades pontilhadas, como já existem em algumas outras linguagens.

Além da questão estética para termos [nomes de arquivos]({% post_url 2016-07-18-nomeando-unidades %}) mais legíveis, esse conceito permite que o compilador localize as Unidades declaradas no código não apenas por seu nome, mas também levando em consideração o Namespace (prefixo) da Unidade.

Segundo a Embarcadero, um Namespace é um [container]({% post_url 2016-02-22-datamodule-e-apenas-um-container %}) de Unidades.

Nesse [link](http://docwiki.embarcadero.com/RADStudio/Tokyo/en/Using_Namespaces_with_Delphi)
oficial diz que *"Namespaces fornece uma maneira de organizar identificadores e tipos, e são utilizados para desambiguar tipos com o mesmo nome"*.

Mas já temos isso com as Unidades então, qual é a grande atualização?

O documento segue dizendo: *"Desde que eles são um container para Unidades Delphi, os Namespaces também podem ser utilizados para diferenciação de unidades com o mesmo nome, que residem em pacotes diferentes"*.

Aqui parece haver uma vantagem. O texto afirma que podemos ter pacotes distintos porém cada um deles contendo Unidades com o mesmo nome e mesmo assim poderíamos utilizar tais pacotes em um mesmo projeto — algo que não seria possível sem essa *feature*.

Enquanto eu escrevo esse artigo, tomando uma xícara quente de café na Starbucks, não tenho disponível uma versão atual do Delphi para fazer esse teste, mas eu acredito que funcione desde que eu utilize o Namespace do Pacote para referenciar todas as Unidades dele no meu projeto.

Meu entendimento é esse:

  1. Existe um Pacote chamado `Acme.Xpto.MyPack`;
  2. Dentro desse pacote existe as Unidades `Foo.pas` e `Bar.pas`;
  3. No meu projeto eu desejo utilizar as Unidades do Pacote, então eu preciso declara-las como `Acme.Xpto.Foo` e `Acme.Xpto.Bar` respectivamente.

Se eu tenho que escrever as Unidades de forma totalmente qualificada, qual é a grande vantagem? Não seria mais simples todas as Unidades do Pacote já serem totalmente qualificadas?

De fato, é possível não escrever os Namespaces, em alguns casos.

É possível, por exemplo, declarar a Unidade `Dialogs` no código do seu projeto mas a referência real será `Vcl.Dialogs` ou `FMX.Dialogs`, bastando definir um parâmetro para o compilador. Parece ser uma *feature* interessante mas, e se eu quisesse utilizar ambas as Unidades no mesmo projeto, como o compilador iria saber qual `Dialogs` utilizar? Será que uma das Unidades deveria ser totalmente qualificada enquanto a outra não? Se assim for, é uma solução *inconsistente*.

Talvez, o Objetivo da implementação padrão serviu apenas aos interesses da Embarcadero para o uso da VCL/FMX ao invés de ser realmente uma nova *feature* para o desenvolvedor.

Namespaces, na minha opinião, deveriam ser implementados de outra forma.

## O Problema {#problema}

Uma Unidade já agrupa, de forma inequívoca, um identificador. Sempre tivemos isso na linguagem Object Pascal mas muitos acham que esse conceito só apareceu em linguagens mais recentes.

Porém, hoje em dia eu posso ver uma referência a `Dialogs` no código e não ter certeza se é da VCL ou qualquer outra Lib que utiliza Namespaces. É necessário verificar as configurações do compilador.

Olhar "só" o código não é mais suficiente.

Outro problema existe com o uso de Libs externas. Não sabemos, previamente, quais nomes de Unidades serão utilizadas pelos programadores ao redor do mundo. Esse é o motivo pelo qual sempre utilizamos prefixos (2~3 letras) nos nomes da Unidades/Classes para simular um Namespace único.

Incrivelmente, essa pratica tem sido suficiente para a maioria dos casos. Pelo menos, para a maioria dos projetos Open Source mais conhecidos como Zeos(Z), RX, Indy(Id), JEDI(Jc) ou Synopse mORMot(Syn). Parece haver um certo "cavalheirismo" entre programadores em não utilizar os prefixos "já em uso" na comunidade Open Source.

No Java, eles resolveram esse problema de forma mais simples: Utilize a URL da empresa/projeto como um Namespace para cada projeto.

E é só isso. Não há ambiguidades, pois não há URL iguais.

Então, bastaria fazermos o mesmo em Object Pascal. O mesmo que a comunidade de Java fez. Basta utilizar o maior nome possível para uma Unidade. Não haverá ambiguidade. Não haverá conflito. Não é necessário implementar nada no compilador.

Resolvido?

Em teoria sim. No entanto a implementação atual do Namespaces clama em dizer que é muito mais quem um nome único (veremos mais sobre isso abaixo).

Além disso, Object Pascal não é assim, tão verboso. Gostamos de nomes simples e de fácil memorização. Podemos programar utilizando um editor simples de texto. Em Java, por exemplo, é necessário haver uma IDE inteligente para ter o mínimo de eficiência na codificação, visto que a IDE incluí automaticamente as "unidades" que serão importadas, além de fazer uma tonelada de checagens.

Bem, se vamos alterar ou melhorar o conceito de Unidades, por quê não fazer isso de forma inteligente, ainda mais [simples]({% post_url 2016-12-19-simplicidade %}), eficaz e sem os problemas de ambiguidades na utilização dos Namespaces atuais?

Podemos sugerir algumas ideias.

## Namespaces Desejável {#desejavel}

Então, qual seria o *nirvana* da implementação de Namespaces no Delphi e Lazarus?

Eu posso lhe dizer minhas ideias (atuais) e no fim veremos se você concorda.

**Namespaces deveriam ser únicos e explícitos no código**, sem ambiguidades.

É *errado* e anti-Pascal ter uma Unidade que não está 100% explícita no código sobre o que ela representa ou se há ambiguidades sobre o que ela representa.

Isso quer dizer que `Dialogs` *não* pode representar, ora `FMX.Dialogs`, ora `Vcl.Dialogs`.

Mesmo que eles possam ser configuráveis, eles deveriam ser únicos no código para não haver ambiguidades.

**Namespaces deveriam ser dinâmicos**, porém explícitos no código.

Ser dinâmico implica em configuração. Então, a ideia de definirmos Namespaces como argumentos do compilador é boa, porém sua implementação atual não foi bem executada.

Sim, Namespaces deveriam ser definidos *por* Pacote, porém o *erro* é defini-los utilizando os nomes dos Pacotes.

A *configuração* deveria ser local.

**Namespaces deveriam ser locais**, ou seja, cada projeto implementa os Namespaces de acordo com a nomenclatura que desejar.

Na minha opinião, ao adicionar um Pacote ao meu projeto, eu poderia (ou deveria) definir um Namespace para ele. É o mesmo que dizer que todas as Unidades do Pacotes seriam "renomeadas" com o novo prefixo — muito similar com a solução atual — porém sem ambiguidades.

Imagine 2 Pacotes: `FooPack` e `BarPack`.

Os desenvolvedores de ambos não deveriam se importar sobre Namespaces. Cada Pacote está bem definido em seus próprios contextos locais, sem se preocupar com o mundo afora. Não importa se os projetos que irão utilizá-los serão pequenos ou gigantescos com inúmeras dependências de vários outros Pacotes. Isso deveria ser irrelevante para os autores de Pacotes.

Cabe a cada Projeto definir o Namespace para cada Pacote, se assim o desejar.

Por exemplo, se ambos os Pacotes tem uma Unidade chamada `Utils`, nos iríamos referenciá-las como `Foo.Utils` e `Bar.Utils` dentro do código do Projeto.

Essa foram escolhas óbvias. Mas ou invés de `Foo.Utils` e `Bar.Utils` poderiam ser `FoPack.Utils` e `BrPack.Utils`, respectivamente. O mais importante aqui é que estas são *escolhas do Projeto*, do usuário do Pacote, não daqueles que criaram os Pacotes.

## Conclusão {#conclusao}

É necessário uma implementação mais moderna do conceito de Namespace, porém sem perder as características originais do Object Pascal.

Tudo na linguagem é previamente declarado, exatamente para não haver ambiguidades. Unidades devem ser únicas em todo o projeto. Não importa se elas estão na VCL/LCL, RTL, Libs, etc. Não deveriam haver ambiguidades.

Tudo é ou deveria ser explícito no código. Deixar o compilador decidir se um identificador corresponde a X ou a Y em tempo de execução, não me parece ser algo tão explícito.

Finalmente, as ideias propostas nesse artigo tem a finalidade e o desejo de melhorar a linguagem Object Pascal e não depreciar as escolhas de *design* dos desenvolvedores da linguagem e do compilador.

Esses são meus pensamentos... Você concorda?

Até logo.
