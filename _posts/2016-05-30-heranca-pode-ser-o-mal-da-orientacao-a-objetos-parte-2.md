---
layout: post
title: "Herança pode ser o Mal da Orientação a Objetos — Parte 2"
date: 2016-05-26
description: Não faça da Herança a sua primeira escolha para reutilizar código.
summary: Não faça da Herança a sua primeira escolha para reutilizar código.
image: /images/photo-1456087468887-17b7d7b076e0.jpg
categories: 
  - OO
tags:
  - herança
keywords:
  - herança
  - inheritance
  - evil
  - mal
  - orientação a objetos
  - oop
  - poo
  - encapsulamento
  - polimorfismo
--- 

No artigo anterior citei alguns males ao utilizarmos Herança de Classe. Nesse artigo irei falar sobre
um deles, a **Violação de Encapsulamento** ao utilizarmos Subclasses.

<!--more-->

![Imagem]({{ page.image }})

[Clique aqui]({% post_url 2016-05-23-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-1 %}) para ler a **Parte #1** 
dessa série, caso ainda não tenha lido.

##Indrodução {#introducao}

Há muitos artigos na internet que falam sobre esse assunto, mas não encontrei nenhum que tenha apresentado esse conceito,
de forma satisfatória, para programadores *Object Pascal* e, aqui estamos.

Então, como um dos pilares da Orientação a Objetos, a Herança, pode se **contrapor** a outro princípio, o **Encapsulamento**?

<blockquote>
  ...a herança de classe permite definir a implementação de uma classe em termos da implemetação de outra.
  A reutilização por meio de sublcasses é frequentemente chamada de reutilização de caixa branca (ou aberta).
  O termo "caixa branca" se refere à visibilidade: com herança, os interiores das classes ancestrais são frequentemente
  visíveis para subclasses.
  <footer><cite title="Padrões de Projetos">Padrões de Projetos, 2002 — p.34 — Erich Gamma e outros — ISBN 85-7307-610-0</cite></footer>
</blockquote>

O texto é claro.

O autor escreveu que **"...os interiores das classes ancestrais são frequentemente visíveis para subclasses"**.
Ele não escreveu, especificamente, que a Herança Viola o Encapsulamento mas você percebeu que é a mesma coisa?

O interior de uma Classe não pode ser visível, mesmo para Subclasses, porque isso significaria **violação de 
encapsulamento**. Ponto.

<blockquote>
  Porque a herança expõe para uma subclasse os detalhes da implementação dos seus ancestrais, frequentemente
   é dito que "a herança viola a encapsulação"[Sny86]
  <footer><cite title="Padrões de Projetos">Padrões de Projetos, 2002 — p.34</cite></footer>
</blockquote>

Quando li isso a primeira vez, pouco tempo depois que comecei a codificar software "Orientado a Objetos", tive o 
sentimento de **negação** que você pode estar tendo agora:

— "Ora, isso não faz o menor sentido!"

Mas faz todo o sentido. Vou repetir: A Herança Viola o Encapsulamento.

Isso é um **fato lógico**. Pode não parecer lógico se é a primeira vez que você lê essa afirmação, mas você entenderá.

##Encapsulamento {#encapsulamento}

Precisamos definir o que é Encapsulamento que, de acordo com o dicionário, significa:
**colocar ou encerrar em cápsula; capsular**.

Uma cápsula não pode — ou não deveria — ser quebrada. O mundo externo ao Objeto não pode saber o que há em seu
interior a menos que o Objeto queira lhes dizer "com suas próprias palavras", ou seja, através de métodos que 
retornam informações mas não necessariamente seu [Estado]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}#objeto-nao-e-um-balde-de-funcoes-e-dados).

O mundo externo só deve conhecer os métodos públicos de um Objeto. Seus métodos públicos representam sua
[Interface]({% post_url 2016-01-18-interfaces-em-todo-lugar %}) para fazer o que ele deve fazer. Qualquer método ou
atributo interno ao Objeto é de propriedade dele e de mais ninguém.

Um Objeto pode ter um único método em sua Interface
porém pode haver 10 outros métodos privados apenas para resolver um único problema. Mas o mundo externo não sabe e não deve
saber como o Objeto trabalha internamente.

Um subtipo de Classe viola esse conceito.

##Herança de Classe

O motivo de termos Subclasses é **acrescentar** mais funcionalidade às Classes ancestrais, criando um novo tipo.
Ao acrescentar funcionalidades a nova Subclasse deverá, inevitavelmente, interagir com o Estado e Métodos protegidos da 
Classe ancestral. Em outras palavras, a Subclasse deverá conhecer como a Classe ancestral trabalha (internamente) para
poder adicionar funcionalidade, o que é uma clara **Violação de Encapsulamento**.

<blockquote>
  A implementação de uma Subclasse, dessa forma, torna-se tão amarrada à implementação da sua classe-mãe que qualquer mudança
  na implementação desta forçará uma mudança naquela.
  <footer><cite title="Padrões de Projetos">Padrões de Projetos, 2002 — p.34</cite></footer>
</blockquote>

A Herança de Classe é simples de usar e entender, mas no longo prazo é provado que essa não é a melhor escolha
ao projetar seu diagrama de Classes. Ao invés de Herança a melhor escolha é a
[Composição de Objetos](https://en.wikipedia.org/wiki/Composition_over_inheritance). Esse é um
dos princípios proposto no livro Padrões de Projetos:

*Favoreça a composição de objetos em relação à herança de classe*.

Foi o que fizeram os engenheiros do Google ao projetar a [Go language](https://golang.org/). 
A linguagem [não tem nenhuma forma](https://talks.golang.org/2012/splash.article#TOC_15) de Herança de Classes,
ou seja, Herança baseada em subtipos. Não há hierarquia de tipos!

##Me mostre o Código

A teoria foi explicada. Agora vou lhe mostrar alguns exemplos de código.

Não posso colocar exemplos completos e reais senão o artigo iria virar um "repositório de código" e não um
artigo. Então veja os exemplos e imagine como seria no código real.

###Exemplo 1 — *Stack Overflow*

É um exemplo simples que poderia ser evitado pelo programador que fez a Classe A, a classe ancestral, 
no entanto veja que para descobrir o problema o programador da Classe B, a Subclasse, deve ver como a Classe A
foi implementada, Violando o Encapsulamento, pois ele deverá fazer ajustes em função da implementão interna da 
Classe A: 

{% highlight pascal %}
type
  TClasseA = class
  protected
    procedure Exec; virtual;
  public
    procedure Proc;
  end;

  TClasseB = class(TClasseA)
  protected
    procedure Exec; override;
  end;

{ TClasseA }

procedure TClasseA.Exec;
begin
  ShowMessage('Exec')
end;

procedure TClasseA.Proc;
begin
  Exec;
end;

{ TClasseB }

procedure TClasseB.Exec;
begin
  Proc;
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  with TClasseA.Create do
  try
    Proc;
  finally
    Free;
  end;

  with TClasseB.Create do
  try
    Proc;
  finally
    Free;
  end;
end;
{% endhighlight text %}

A Classe B sobrescreve um método da Classe A. Simples. Fazemos isso todo tempo.
Então, qual é o problema desse código?

Se você copiar/colar o código na sua IDE e executar, verá uma mensagem de erro na segunda execução de `Proc`.
Essa mensagem pode variar de IDE mas basicamente é um *Stack Overflow*. Acontece que ao sobrescrever `Exec` 
na Classe B, o programador chamou `Proc` internamente. O problema é que na Classe A, `Proc` já está chamando `Exec` e,
dessa forma, o programa entra em um *loop* infinito.

###Exemplo 2 — Utilizar `inherited` ou não?

Quando sobrescrevemos um método há a possibilidade de chamar o código do mesmo método da Classe ancestral. 
Para isso utilizamos a palavra reservada `inherited`.

Mas como saber se devo ou não chamar o código da Classe ancestral?

Somente olhando a implementação privada da Classe para termos certeza se **devemos ou não** chamar o código.
Novamente, temos uma Violação de Encapsulamento.

{% highlight pascal %}
type
  TClasseA = class
  protected
    FCount: Integer;
  public
    procedure Exec; virtual;
  end;

  TClasseB = class(TClasseA)
  public
    procedure Exec; override;
  end;

{ TClasseA }

procedure TClasseA.Exec;
begin
  FCount := FCount + 1;
  ShowMessage('Count=' + IntToStr(FCount)); //=1
  ShowMessage('Exec A');
end;

{ TClasseB }

procedure TClasseB.Exec;
begin
  inherited;
  FCount := FCount + 1;
  ShowMessage('Count=' + IntToStr(FCount)); //=2
  ShowMessage('Exec B');
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  with TClasseB.Create do
  try
    Exec;
  finally
    Free;
  end;
end;
{% endhighlight text %}

Quando executado teremos as mensagens: 

  1. "Count=1"
  2. "Exec A"
  3. "Count=2"
  4. "Exec B"
  
Nesse exemplo o programador utilizar `inherited` porque ele sabe — olhando para a implementação de `TClasseA.Exec` —
que a execução da Classe ancestral iria incrementar o atributo `FCount` antes que ele fosse incrementado novamente no 
método sobrescrito.

É um exemplo idiota, mas serve para o que eu quero lhe mostrar.

Como eu disse, tente imaginar um código em produção, pensando quantas vezes você já teve que
ver o código da Classe ancestral (ou uma hierarquia de classes inteira) para saber se podia ou não chamar um método; se devia ou não
chamar o código ancestral do método; se um determinado atributo já havia sido inicializado, etc.

Antigamente, nos compiladores mais antigos, era possível fazer uma chamada a um método abstrato — sei que o compilador FreePascal
não permite, nem compila — mas não sei se o compilador atual do Delphi permite. Bem, em compiladores antigos esse é mais um problema:
"Será que posso utilizar `inherited` nesse método? Como saber se é abstrato?"

Se não me engano, acho que o Java permite a chamada a métodos abstratos — me corrijam se eu estiver errado.

###Exemplo 3 — Herança pode ser o Mal em qualquer lugar

O próximo exemplo não é sobre *Object Pascal*, mas sim Java. Muitas pessoas pensam que algo está correto só
porque foi feito por uma grande empresa. Não. Pode ser uma grande empresa lucrativa, porém seus programadores
não são robôs, eles falham.

<blockquote>
  Quando então usar herança? Essa é uma questão difícil. Na minha visão particular, a resposta seria um enfático “quase nunca”.
  <footer><cite title="Paulo Silveira">Como não aprender orientação a objetos: Herança — Paulo Silveira</cite></footer>
</blockquote>

[Clique aqui](http://blog.caelum.com.br/como-nao-aprender-orientacao-a-objetos-heranca/) para ler o artigo na íntegra e
ver como os arquitetos do Java erraram incrivelmente no *design* da arquitetura de *Servlets*.

A solução proposta pelo autor do artigo é a mesma que proponho em quase todos os posts aqui no blog:
[Utilize Interfaces]({% post_url 2016-01-18-interfaces-em-todo-lugar %}).

##No próximo artigo...

Espero que você tenha entendido. Herança viola o Encapsulamento. Não é apenas um conceito, mas um fato.

No próximo artigo irei falar sobre **Duplicação de Código** ao utilizarmos Herança.

Caso você tenha alguma dúvida ou quiser compartilhar seus pensamentos sobre essa série, utilize a área 
abaixo para comentários.
  
Até logo.