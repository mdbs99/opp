---
layout: post
title: "Records - Antiga Nova Tecnologia"
date: 2018-10-22
permalink: /:title
description:
  O tipo Record pode ter métodos e até mesmo campos privados. Seria isso considerado uma abominação para um código Orientado a Objetos?
image: /images/2018/alvaro-reyes-517391-unsplash.jpg
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
  - records
---

O tipo Record pode ter métodos e até mesmo campos privados. Seria isso considerado uma abominação para um código Orientado a Objetos?

<!--more-->

![Unsplash image]({{ page.image }})
<br><span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Alvaro Reyes on Unsplash</span>

Na programação Orientada a Objetos, utilizamos objetos.

Se você precisasse de um objeto para representar os *dados* de um usuário, apenas login, password e nome, como seria a definição da classe?

Vamos tentar:

    TUserData = class
    private
      fLogin: string;
      fPassword: string;
      fName: string;
    public
      property Login: string read fLogin write fLogin;
      property Password: string read fPassword write fPassword;
      property Name: string read fName write fName;
    end;

Esse seria um estilo de implementação que podemos ver em muitos sistemas que se dizem ser Orientados a Objetos.

No entanto, essa classe irá gerar "objetos" que são mais conhecidos como DTO (Data Transfer Object) o que, por definição, é um conceito errado.

Objetos não são ["baldes de dados"]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}#objeto-nao-e-um-balde-de-funcoes-e-dados).

Objetos verdadeiros possuem comportamento e [encapsulam]({% post_url 2018-04-16-encapsulamento %}) dados e/ou outros objetos em seu interior. O que está encapsulado não deveria ser visível ou acessado diretamente por outro objeto externo.

Essa é a *teoria*.

Mas, e se no contexto do seu sistema você realmente não necessitasse de nenhum método para representar os dados de um usuário?

Por muito tempo eu tenho utilizado objetos para tudo, até mesmo para representar dados, mas sem utilizar objetos anêmicos como esse, no entanto.

Para representar os dados acima, eu utilizaria a técnica do [Método About()]({% post_url 2016-11-14-diga-me-algo-sobre-voce %}#metodo-about), que iria retornar um XML ou JSON, conforme explicado em 2016...

Há vantagens nessa técnica como ter uma única interface que qualquer classe possa implementar para retornar um XML/JSON. Mas também há desvantagens, como não haver checagem estática do compilador e o *overhead* em "montar/desmontar" o *stream* se houver a necessidade de repassar a informação por uma cadeia de objetos.

Nesses ~2 anos, eu diria que muita coisa mudou.

Estou trabalhando em alguns projetos que necessitam de extrema velocidade de processamento, mas nem por isso eles são menos Orientados a Objetos.

Como?

Redescobrindo o tipo Record.

Mas antes eu quero lhe contar por quê eu neglicenciei os Records por muito tempo.

Bem, Records existem desde o Turbo Pascal. Eles representam uma estrutura de dados. Você define uma estrutura e depois define *procedures* e *functions* para trabalhar com as estruturas — como ainda fazemos na linguagem C.

Na Orientação a Objetos, nós unimos comportamento aos dados num único artefato chamado objeto.

Então, a ideia de ter um Record com todos os seus dados públicos e funções "soltas" que o utilizam, implementadas muitas vezes em Unidades diferentes, me soa muito "anti-objeto", muito anti-encapsulamento.

Records, dessa forma, são estruturas para programas procedurais.

Mas, já faz uns anos que os arquitetos da linguagem Object Pascal introduziram métodos em Records.

Sim, métodos, em Records.

Quando eu li isso pela primeira vez, achei que os arquitetos tinham ficado loucos. Métodos em Records? Qual o sentido? Já temos objetos, não precisamos de Records com métodos. Isso não faz o menor sentido! — eu pensava.

Entretanto, hoje em dia, Records podem ter métodos e eles podem ser até mesmo privados, assim como ter *fields* privados!

Ora, não é isso um objeto?

Não.

Então, não seria melhor utilizar um objeto?

Depende.

Records são estruturas de dados — mesmo que eles tenham métodos — e deveria ser utilizados como tal. 

Records são alocados automaticamente pelo compilador na área de memória *stack*, ao invés da área de memória *heap*, como fazem os objetos. Utilizar o *stack* é mais rápido e mais simples para o compilador gerenciar a memória.

Então, se você necessita representar *apenas* dados, Record é a melhor opção.

Records não podem utilizar herança e muito menos implementar uma interface, então não há polimorfismo — lembre-se, são apenas dados.

Então, se você necessita de polimorfismo e comportamento, objeto é a melhor opção.

Dito isso, por quê métodos em Records pode mudar a maneira em como utilizamos Records em programas Orientados a Objetos?

Quando declaramos um Record, seus campos não são inicializados, como acontece com os atributos de um objeto. Então, podemos definir um método para iniciá-lo, sem ter que fazer isso externamente — mesmo que seus campos sejam públicos — encapsulando a inicialização dos campos na própria estrutura de dados:

    TUserData = record
      login: string;
      password: string;
      name: string;
      procedure Init;
    end;
    
    procedure TUserData.Init;
    begin
      login := '';
      password := '';
      name := '';
    end;

    var
      u: TUserData;
    begin
      u.Init;
    end;

Outra opção é utilizar a função [Default()](https://www.freepascal.org/docs-html/rtl/system/default.html) mas o importante é que você tem a opção de fazer o que quiser no procedimento que será como um construtor:

    procedure TUserData.Init;
    begin
      Default(self);
    end;

Se o *password* precisar ser criptografado/descriptografado, você não precisa de funções externas para isso:

    function TUserData.EncryptedPassword: string;
    begin
      result := '<using some algorithm to encrypt>';
    end;
    
    function TUserData.DecryptedPassword: string;
    begin
      result := '<using some algorithm to decrypt>';
    end;

Existem algumas regras a seguir:

1. Não implemente métodos que se comportem como objetos, ou seja, trabalhe *apenas* com os dados do Record, transformando-os;
1. Não retorne instâncias de objetos em seus métodos, com exceção de instâncias de interfaces, mas evite a todo custo;
1. Só utilize campos com tipos primitivos ou gerenciados (ex: strings, Variant, etc), nunca instâncias de objetos;

Faça isso e você estará seguro.

Seu código não será considerado menos Orientado a Objetos por estar utilizando estruturas de dados que representam dados. Records foram criados exatamente para isso e a implementação de métodos apenas facilitaram seu uso.

Até logo.