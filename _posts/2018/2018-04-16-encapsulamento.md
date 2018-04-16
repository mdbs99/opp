---
layout: post
title: "Encapsulamento"
date: 2018-04-16
permalink: /:title
description:
  O Encapsulamento é um dos pilares da Orientação a Objetos. Mas você sabe o que realmente significa esse conceito?
image: /images/2018/photo-erwan-hesry-166245-unsplash.jpg
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
  - encapsulamento
  - encapsulation
---

O Encapsulamento é um dos pilares da Orientação a Objetos. Mas você sabe o que realmente significa esse conceito?

<!--more-->

![Unsplash image]({{ page.image }})
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Erwan Hesry on Unsplash</span>

## Introdução {#introducao}

De acordo com o dicionário, Encapsulamento significa: colocar ou encerrar em cápsula; capsular.

Uma cápsula não pode — ou não deveria — ser quebrada. O mundo externo ao objeto não pode saber o que há em seu interior a menos que o objeto queira lhes dizer “com suas próprias palavras”, ou seja, através de métodos que retornam informações mas não necessariamente seu [Estado]({% post_url 2017-06-05-estado-do-objeto %}).

O mundo externo só deve conhecer os métodos públicos de um objeto. Seus métodos públicos representam sua Interface para fazer o que ele deve fazer. Qualquer método ou atributo interno ao objeto é de propriedade dele e de mais ninguém.

Atributos e [métodos privados]({% post_url 2017-11-20-eliminando-metodos-privados %}) estão encapsulados no objeto e nunca deveriam ser acessados externamente.

Essa é a teoria.

## Encapsulamento pelo Código {#codigo}

Toda a [Orientação a Objetos]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}) é sobre encapsulamento e envio de mensagens.

Infelizmente, ambos os conceitos não são implementados perfeitamente em Object Pascal ou em nenhuma outra linguagem <i>mainstream</i> do mercado como Java e C#.

E isso gera muitas dúvidas.

Vamos falar sobre Encapsulamento e tentar expandir nossas mentes além do código, além dos conceitos pré-concebidos e do básico.

O que é estar encapsulado? Essa pergunta parece tola, mas veremos que depende do ponto de vista do observador para respondê-la.

Por exemplo. Um atributo privado está encapsulado no objeto e não pode ser acessado pelo mundo externo, certo?

    type
      TFoo = class
      private
         FValue: Integer;
      public
         function GetValue: Integer;
      end;

    function TFoo.GetValue: Integer;
    begin
      Result := FValue;
    end;

    procedure TMainForm.ExecButton(Sender: TObject);
    var
      F: TFoo;
    begin
      F := TFoo.Create;
      ShowMessage(F.GetValue.ToString);
      F.Free;
    end;

No exemplo acima, o mundo externo representado pela instância de  <code>TMainForm</code> consegue acessar o atributo privado da instância da classe <code>TFoo</code>?

Não, você diria.

O atributo privado está sendo acessado por um método público que o "protege". Além disso, como o atributo é [primitivo]({% post_url 2017-01-16-tipos-primitivos-nos-argumentos %}), ele não é passado por referência. Então, o mundo externo não pode nem mesmo alterá-lo.

Está correto.

Então, vamos alterar um pouco o código.

    type
      TFoo = class
      private
         FValue: TDataValue;
      public
         function GetValue: TDataValue;
      end;

    function TFoo.GetValue: TDataValue;
    begin
      Result := FValue;
    end;

    procedure TMainForm.ExecButton(Sender: TObject);
    var
      F: TFoo;
    begin
      F := TFoo.Create;
      F.GetValue.Free;
      F.Free;
    end;

Agora <code>GetValue</code> retorna uma instância de objeto.

É possível criar o objeto <code>F</code>, acessar (indiretamente) seu atributo privado e destruí-lo chamando <code>Free</code>, pois<code>GetValue</code> retornar a referência direta do atributo.

Ora, o atributo não é público! Não temos um [setter]({% post_url 2016-06-27-getters-e-setters %}) para ele e, mesmo assim, o "mundo externo" consegue destruí-lo? Ele não deveria ser intocável externamente?

Vejamos agora:

    type
      TFoo = class
      public
         Value: TDataValue;
      end;

Nessa versão a classe <code>TFoo</code> tem um atributo público <code>Value</code>.

Se o mundo externo acessar esse atributo, estaríamos quebrando o encapsulamento do objeto?

Na verdade, não.

Não há nada encapsulado no objeto e, portanto, por definição não há nenhuma quebra de encapsulamento!

Talvez a linguagem não devesse permitir que atributos fossem públicos, mas esse seria um debate para outro artigo.

Quer outro exemplo? Imagine que <code>GetValue</code> agora é <i>privado</i>. Imagine uma outra classe chamada <code>TBar</code> com essa definição:

    type
      TDataValueFunc = function: TDataValue of object; 

      TBar = class
      private
         FFunc: TDataValueFunc;
      public
         constructor Create(Func: TDataValueFunc);
         function Data: TDataValue;
      end;

    function TBar.Data: TDataValue;
    begin
      Result := FFunc;
    end;

E o (novo) construtor de <code>TFoo</code> seria assim:

    constructor TFoo.Create;
    begin
      FBar := TBar.Create(@GetValue);
      FBar.Data;
    end;

Não estaria <code>TBar.Data</code> executando um código privado de <code>TFoo</code>, mesmo que indiretamente? Um objeto "acessando" um método privado de outro objeto. Seria essa uma quebra de encapsulamento?

O que eu estou tentando lhe mostrar é que não é apenas o código puramente técnico que irá definir um bom nível de encapsulamento no seu projeto.

É necessário que você pense em encapsulamento no nível de design do projeto e não apenas na implementação técnica das classes.

## Conclusão {#conclusao}

Vimos que a linguagem Object Pascal permite-nos acessar atributos e 
métodos privados, mesmo que indiretamente.

O mesmo pode ser feito em outras linguagens consideradas "mais" Orientadas a Objetos.

Então, discutir sobre encapsulamento no nível puramente técnico da linguagem é perda de tempo. Seria muito difícil — mas não impossível — implementar um código onde todos os atributos privados não fossem acessados, de alguma maneira, pelo mundo externo sem perder performance ou complicar o modelo de classes apenas para afirmar que os atributos estão protegidos e intocados.

No mundo real, sistemas devem ter boa performance e serem simples de manutenção.

É melhor quebrar algumas regras e deixar as coisas [simples]({% post_url 2016-12-19-simplicidade %}) do que ser purista e não ter nenhum benefício real.

Finalmente, o conceito de Encapsulamento na Orientação Objetos é mais sobre como projetar a comunicação entre os objetos e módulos utilizando o mínimo necessário ao invés de se preocupar unicamente com a sintaxe utilizada.

Até logo.


