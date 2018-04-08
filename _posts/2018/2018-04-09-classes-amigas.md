---
layout: post
title: "Classes Amigas"
date: 2018-04-09
permalink: /:title
description:
  Classes Amigas tem acesso ao estado (privado) de seus objetos. Apesar dessa prática ser entendida por muitos desenvolvedores como uma quebra do encapsulamento, na verdade ela pode até aprimorá-lo.
image: /images/2018/photo-ben-white-178537-unsplash.jpg
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
  - classes-amigas
  - friends classes
---

Classes Amigas tem acesso ao estado (privado) de seus objetos. Apesar dessa prática ser entendida por muitos desenvolvedores como uma quebra do encapsulamento, na verdade ela pode até aprimorá-lo.

<!--more-->

![Unsplash image]({{ page.image }})
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Ben White on Unsplash</span>

Classes Amigas existem na linguagem C++ a muito tempo, assim como na linguagem Object Pascal.

Enquanto na linguagem C++ é necessária uma sintaxe específica, em Object Pascal basta declararmos as classes na mesma unit para que elas sejam "amigas". E não importa se elas fazem parte ou não de uma hierarquia de herança.

Eu conheço alguns desenvolvedores que não lidam muito bem com isso. Para eles, o que é privado nunca deveria ser acessado por outra instância além do próprio objeto.

Eles estão certos.

Mas essa é uma regra geral. Precisamos saber quando "quebrar" as regras, se isso for aumentar a <i>qualidade</i> do software.

Eu também fui contra a essa <i>feature by design</i> por muito tempo. A muitos anos atrás eu declarava as classes em units separadas, mesmo que seus objetos existissem apenas para "conversar" entre si, ou seja, mesmo que tais objetos fossem bem "íntimos".

A fim de seguir a "regra de ouro" do encapsulamento, eu declarava tais classes em units separadas pensando que eu estava desenvolvendo mais Orientado a Objetos <i>protegendo</i> seus estados.

Mas, ao mesmo tempo, eu declarava as propriedades de acesso — também conhecidos com [Get/Set]({% post_url 2016-06-27-getters-e-setters %}) — para que o estado desses objetos pudessem ser acessados por alguma instância de <i>fora</i>!

E isso não faz sentido.

Eu estava escrevendo e complicando mais o código, sem ganhar nenhum benefício, já que toda essa "proteção", na verdade, não existia.

Então, eu voltei a rever esse conceito das Classes Amigas.

Eu diria que Classes Amigas enfraquecem o encapsulamento local (na unit) afim de <i>fortalecer</i> o encapsulamento global (por todo o projeto).

Ainda assim, é claro, isso irá depender de alguns fatores. Utilizá-las é mais uma exceção à regra e deve ser muito bem pensado afim de obter mais prós do que contras.

Vamos a um exemplo: Imagine que você tem uma classe que representa um livro. Na interface da classe desse livro nós só queremos ter o básico que, no nosso exemplo, irá conter métodos para retornar o título, autor e ISBN.

    type
      IBook = interface
        function Title: string;
        function Author: string;
        function ISBN: string;
      end;
      
      TBook = class(TInterfacedObject, IBook)
      private
        FDoc: IXMLDocument;
      public
        constructor Create(const Doc: IXMLDocument);
        function Title: string;
        function Author: string;
        function ISBN: string;
      end;
 
Temos uma interface <code>IBook</code> e também uma classe que a implementa.

O código é bem simples e de fácil entendimento.

A classe <code>TBook</code> recebe uma instância de <code>IXMLDocument</code> proveniente de algum lugar. Não importa. 

Esse objeto XML possui toda a informação referente a um livro.

Hoje, apenas os 3 atributos estariam contidos no XML mas tenha em mente que ele pode ser alterado. Poderíamos adicionar mais nós de informação futuramente, se assim o desejarmos.

Internamente a classe <code>TBook</code> irá fazer o parser do XML, retornando suas informações através dos respectivos métodos.

Imaginemos que tudo funciona bem por meses e muitas outras classes podem ter implementado a mesma interface <code>IBook</code>. Por exemplo, <code>TDbBook</code>, <code>TEmptyBook</code>, <code>TNullBook</code>, <code>TJSONBook</code>, <code>TXMLBook</code>, etc.

Então, uma nova regra de negócio é solicitada: é necessário saber o <i>título original</i> e o <i>ano de publicação</i>.

Se alterarmos a interface atual, teremos que implementar os novos métodos em todas as classes que a implementam. Isso é bastante trabalho e pode não ser uma boa ideia.

Outra opção é implementar uma nova classe, especializando <code>IXMLDocument</code> como uma interface para ser implementada por uma [classe de dados]({% post_url 2016-08-01-classes-de-dados %}). Por exemplo, <code>IBookData</code>. Todo o código de parser que antes estava contido em <code>TBook</code>, deverá ser migrado para a nova classe, refatorando não só <code>TBook</code> mas também todas as outras implementações de <code>IBook</code> que, porventura, tenham o construtor parecido. Isso também é bastante trabalho, talvez mais trabalho do que a primeira opção.

A outra opção (mas pode haver muitas) é utilizar Classes Amigas criando apenas o necessário sem alterar nenhuma outra classe, escrevendo menos e de forma mais simples.

    type
      ICompletedBook = interface(IBook)
        function OriginalTitle: string;
        function Year: Integer;
      end;
      
      TCompletedBook = class(TBook, ICompletedBook)
      public
        function OriginalTitle: string;
        function Year: Integer;
      end;

Agora temos uma nova interface e uma nova classe que a implementa. Ambos utilizando [herança]({% post_url 2016-05-23-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-1 %}), pois ambas são muito íntimas dos seus predecessores.

Dentro dos métodos da classe <code>TCompletedBook</code>, será necessário ter acesso ao atributo privado de <code>TBook</code> denominado <code>FDoc</code>.

Entretanto, se ambas as classes são amigas, ou seja, declaradas na mesma unit, não haverá problemas para fazer isso:

      function TCompletedBook.OriginalTitle: string;
      begin
        Result := FDoc.Node('original-title').AsString;
      end;

      function TCompletedBook.Year: Integer;
      begin
        Result := FDoc.Node('year').AsInteger;
      end;

O atributo <code>FDoc</code> é acessível, mesmo sendo privado.

Você poderia pensar em alterar a visibilidade desse atributo para <code>protected</code> e deixar que até mesmo o código de outros usuários possam herdar de <code>TBook</code>, mas isso seria um erro.

Ao tornar os atributos acessíveis para qualquer classe, você <i>enfraquece</i> o encapsulamento global e pode perder o controle do código. Seria muito mais difícil fazer alguma alteração em <code>TBook</code> pois não há como saber, com certeza, quem está utilizando os atributos privados.

Finalmente, ao utilizar o modelo de Classes Amigas, você sabe quais classes deverão ser alteradas se houver alguma refatoração, pois todas elas estarão declaradas na mesma unit, obrigatoriamente.

Suas classes serão [menores]({% post_url 2016-11-28-menos-e-mais %}) e mais simples.

Há seres humanos que são mais ligados intimamente a uns do que outros — mesmo todos sendo provenientes da mesma "classe" — que sabem seus gostos, desejos e segredos mais íntimos.

A mesma lógica pode também ser aplicada a apenas algumas classes onde seus objetos são mais amigos, concedendo acesso irrestrito uns aos outros, mas bloqueando esse acesso ao mundo exterior.

Até logo.