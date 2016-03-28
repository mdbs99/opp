---
layout: post
title: "Não utilize Métodos Estáticos"
date: 2016-02-15
description: Não utilize Métodos Estáticos ou Classes Utilitárias
summary: Utilizando Métodos Estáticos, qual Objeto irá executar a tarefa? Nenhum.
image: /images/photo-1444208393177-b2a88904ed8d.jpg
categories: 
  - Pascal
tags:
  - método estático
keywords:
  - método estático
  - static methods
---

Objetos são representações de Entidades vivas. Quando solicitamos uma tarefa a um Objeto, 
ele mesmo irá executar ou repassar a tarefa à outro Objeto.

Utilizando Métodos Estáticos, qual Objeto irá executar a tarefa? Nenhum. A tarefa será
executada pelo Método Estático da Classe e nenhum Objeto estará envolvido.

Método Estático é uma herança da programação Procedural.

<!--more-->

![Circular Reference]({{ page.image }})

##Introdução

Há pequenas exceções na utilização de Métodos Estáticos, mas todos eles são em benefício
da performance ou algum [recurso tecnológico]({% post_url 2016-01-10-interfaces-e-o-metodo-estatico-new %}).
Métodos Estáticos simplesmente não existem no mundo Orientado a Objetos.

Linguagens populares como Java, C# ou Object Pascal utilizam Métodos Estáticos, muitas vezes em
Classes Utilitárias. São classes onde todos os seus métodos são estáticos.

Vou lhe mostrar porque não devemos utilizar Métodos Estáticos – na maioria dos casos – e qual seria
a alternativa Orientada a Objetos.

##Métodos Estáticos ou Procedural disfarçado

Os Métodos Estáticos executam uma tarefa sem estarem vinculados a um Objeto. Se não existe
Objeto vinculado, tais métodos não trabalham com atributos de uma instância. Eles são imperativos, procedurais e
não Orientados a Objetos.

Mas a verdade é que ainda precisamos deles em pouquíssimos casos. 

Vejamos alguns exemplos:

  * O padrão do [Método New]({% post_url 2016-01-10-interfaces-e-o-metodo-estatico-new %}) é um caso de necessidade, 
mais do que estilo de codificação. Ele não é um método de negócio. É utilizado como um construtor.
  * Se necessitarmos de extrema performance, utilizar um Método Estático – ou mesmo uma *procedure* ou *function* – terá 
uma performance melhor do que ter o *overhead* da criação de um Objeto porém você não estará perdendo todos os benefícios 
da Programação Orientada a Objetos. Será que vale a pena?
  * Várias linguagens tem disponíveis classes – ou funções – para trabalhar no nível básico e genérico que todo programa
poderia querer utilizar. Classes para cálculos Matemáticos, manipulação de Arquivos, API do Sistema Operacional, etc. Se 
você precisa lidar com esses problemas não tem que reinventar a roda, apenas utilize o que já existe. 

O Método Estático em si não é o (grande) problema. O real problema é você utilizá-lo para modelar seus Objetos de Negócios.

Então a Regra de Ouro que deixo à você é:

>Não implemente Métodos de Negócio utilizando Métodos Estáticos.

Exemplo:

A) Se você precisa apenas gravar um arquivo de Log, pode utilizar alguma rotina que trabalhe com arquivos e tudo bem.

B) Mas se fizer parte das **Regras de Negócio** do sistema, por exemplo, ter que gerar um XML e compactar antes de enviar a um serviço REST,
você **NÃO** deve utilizar rotinas que manipulam esses problemas diretamente. É necessário que você crie classes [adaptadoras ou invólucros](https://en.wikipedia.org/wiki/Adapter_pattern)
para encapsular as regras de negócio que irão utilizar essas rotinas.

Faz sentido? 

São coisas diferentes. Gerar um arquivo de Log não é algo intrínsico das Regras de Negócio – ao menos no meu exemplo –
porém se o seu sistema foi feito para integrar dados entre outros sistemas, faz parte das Regras de Negócio trabalhar com 
arquivos. Então você precisa de Objetos reais e não de rotinas ou Métodos Estáticos.

##Classes Utilitárias {#classes-utilitarias}

Enquanto Métodos Estáticos são considerados apenas uma má prática, as Classes Utilitárias representam o **mal** para 
a Orientação a Objetos.

Sim, várias linguagens disponibilizam essas Classes Utilitárias, como falei acima, mas elas servem para serem utilizadas **dentro** 
das suas Classes de Negócio.

De acordo com Nick Malik nesse [artigo](http://blogs.msdn.com/b/nickmalik/archive/2005/09/06/461404.aspx), na maioria das 
vezes a definição e implementação de Classes Utilitárias é apenas preguiça. Eu concordo.

Essas classes já existem. Você pode utilizá-las mas não precisa replicá-las ou copiá-las. Não aumente o problema!
Crie Objetos, não rotinas procedurais.

Objetos tem [Comportamento e Estado]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}). Os métodos representam
o comportamento de um Objeto. É valido dizer que todo método de um Objeto deverá fazer uso de pelo menos um atributo
dele mesmo, do contrário tal método pode estar na classe errada.

Um canditado a Método Estático é um desses métodos que não faz uso de atributos.

Por exemplo:

{% highlight pascal %}
type
  TMath = class
    class function Max(A, B: Integer): Integer;
  end;

implementation

class function TMath.Max(A, B: Integer): Integer;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;
{% endhighlight text %}

A classe *TMath* é bem simples. Possui um método que verifica qual é o maior número dentre 2 possíveis.

Um exemplo de uso:

{% highlight pascal %}
var
  V: Integer;
begin
  V := TMath.Max(10, 15);
  ShowMessage(IntToStr(V));
end;
{% endhighlight text %}

Você consegue ver algum problema no *design* desse código?

Se você está acostumado a utilizar Classes Utilitárias como essa, provavelmente não vê problema algum. O
código é simples de entender, é rápido, não cria "objetos desnecessários" e... funciona.

Então qual é o problema?

Bem, vamos incrementar com um exemplo um pouco mais complexo:

{% highlight pascal %}
class function TMathA.Compute(A, B: Double): Double;
begin
  Result := TMathC.Sum(TMathB.Max(A, B), A*2, B*B);
end;

{...}

var
  V: Double;
begin
  V := TMathA.Max(10, 15);
  ShowMessage(IntToStr(V));
end;
{% endhighlight text %}

O código não está completo mas dá para entender perfeitamente:

  1. Existem as classes *TMathA*, *TMathB* e *TMathC*.
  2. *TMathA* tem uma dependência física com as outras classes.
  3. Todas as classes possuem Métodos Estáticos.

O cálculo é irrelevante. Pura invenção. Escrevi o que veio a minha mente.

Tente enxergar os **problemas** dessa abordagem, considerando o paradigma da Orientação a Objetos.

Aqui estão alguns problemas:

  1. Não dá para substituir nenhuma classe no código, pois elas não [implementam uma *Interface*]({% post_url 2016-01-18-interfaces-em-todo-lugar %}).
  2. Se não consigo **substituir** nenhuma implementação, então não consigo testar essas classes isoladamente.
  3. Se não consigo **testar** cada classe isoladamente, o sistema de testes terá que processar todos os Métodos Estáticos.
  Se esses métodos demoram muito para calcular, seria uma perda de tempo toda vez que executasse os testes. E também
  teríamos que lidar com outros acoplamentos que tais classes poderiam ter se fosse um exemplo mais complexo.
  4. Temos um **acoplamento forte** entre nosso código e todas essas classes estáticas, além do acoplamento entre elas.
  
##Solução?

Vou tentar reescrever o mesmo exemplo acima utilizando uma abordagem mais Orientada a Objetos.

Primeiramente sempre temos que ter uma [Interface que represente uma abstração]({% post_url 2016-01-18-interfaces-em-todo-lugar %}),
que neste exemplo aqui é *Math*.

{% highlight pascal %}
type 
  IMath = interface
    function Compute(A, B: Double): Double
    function Sum(A, B, C: Double): Double;
    function Max(A, B: Double): Double;
  end;
{% endhighlight text %}

Então vamos codificar *TMathA*, *TMathB* e *TMathC*.

Para simplificação do código, só irei implementar os métodos relevantes para o entendimento:

{% highlight pascal %}
type
  TMathA = class(TInterfacedObject, IMath)
  private
    FMC: IMath;
    FMB: IMath;
  public
    constructor Create(MC, MB: IMath);
    class function New(MC, MB: IMath): IMath;
    function Compute(A, B: Double): Double;
    //...function Sum(A, B, C: Double): Double;
    //...function Max(A, B: Double): Double;
  end;
  
  TMathB = class(TInterfacedObject, IMath)
  public
    //...function Compute(A, B: Double): Double
    //...function Sum(A, B, C: Double): Double;
    //...function Max(A, B: Double): Double;
  end;
  
  TMathC = class(TInterfacedObject, IMath)
  public
    //...function Compute(A, B: Double): Double
    //...function Sum(A, B, C: Double): Double;
    //...function Max(A, B: Double): Double;
  end;
  
implementation

constructor TMathA.Create(MC, MB: IMath);
begin
  inherited Create;
  FMC := MC;
  FMB := MB;
end;

class function TMathA.New(MC, MB: IMath): IMath;
begin
  Result := TMathA.Create(MC, MB);
end;

function TMathA.Compute(A, B: Double): Double;
begin
  Result := FMC.Sum(FMB.Max(A, B), A*2, B*B);
end;

end;  
{% endhighlight text %}

E então reescrevo o mesmo teste, mas agora utilizando instâncias em vez de Métodos Estáticos:

{% highlight pascal %}
var
  V: Double;
begin
  V := TMathA.New(
    TMathC.New(),
    TMathB.New()
  ).Max(10, 15);
  ShowMessage(IntToStr(V));
end;
{% endhighlight text %}

No código acima utilizei [Injeção de Dependência](https://en.wikipedia.org/wiki/Dependency_injection)
na inicialização de *TMathA*. Por esse motivo *TMathA* não tem conhecimento da existencia
de *TMathB* ou *TMathC*. O código fica desacoplado e ainda ganhamos nos testes.

Por exemplo:

Imagine que *TMathB* e *TMathC* são classes que tem um processamento muito demorado. Mas nós queremos apenas
testar a funcionalidade de *TMathA*. O que podemos fazer?

Criar Objetos Falsos.
 
{% highlight pascal %}
var
  V: Double;
begin
  V := TMathA.New(
    TFakeMathC.New(),
    TFakeMathB.New()
  ).Max(10, 15);
  ShowMessage(IntToStr(V));
end;
{% endhighlight text %}

As classes *TFakeMathC* e *TFakeMathB* não tem código de produção. A implementação nem precisa calcular nada,
apenas retornar números válidos para passar nos testes automatizados.

Viu as vantagens?

##Conclusão

Métodos Estáticos não fazem parte do paradigma da Orientação a Objetos. Podem ainda ser necessários devido a
natureza e herança procedural das linguagens. No entanto devem ser evitados.

Para Classes de Negócio é altamente desencorajado utilizar Métodos Estáticos para evitar todos os problemas
relatados acima.

Classes Utilitárias... esqueça-as. É um conceito errado. Não permita que nenhuma Classe Utilitária seja criada
no seu código.
  
Até logo.