---
layout: post
title: "Construtores da Classe, Primário e Secundários"
date: 2016-03-21
description: Como Utilizar Construtores Primários e Secundários.
summary: Como Utilizar Construtores Primários e Secundários.
image: /images/photo-1457803097035-3ace37af34a7.jpg
categories: 
  - oo
tags:
  - construtor
keywords:
  - construtores
  - primário
  - primary
  - secundário
  - secondary
  - constructor
  - create
  - builder
--- 

Você pode implementar dezenas de Construtores para uma única Classe, porém apenas um deles 
deverá ser o **Primário** enquanto os demais deverão ser apenas **Secundários**.

Todos os construtores Secundários deverão fazer uma chamada ao construtor Primário. 

<!--more-->

![Imagem]({{ page.image }})

Os [Construtores de Classes](https://en.wikipedia.org/wiki/Constructor_(object-oriented_programming)) são 
utilizados para criar instâncias de Objetos.

Uma Classe pode ter vários construtores, mas somente o **Primário** deverá retornar uma instância de um Objeto.

Os demais construtores são apenas **Secundários**, pois irão fazer uso do construtor Primário.

O construtor Primário, na maioria das vezes, sempre será aquele com mais parâmetros.

Vamos a um exemplo.

{% highlight pascal %}
type
  TUser = class
  private
    FLogin: string;
    FPassword: string;
  public
    constructor Create(const Login, Password: string);
    constructor Create(const Login: string); 
    function Login: string;
    function Password: string;
  end;
  
implementation

constructor TUser.Create(const Login, Password: string);
begin
  inherited Create;
  FLogin := Login;
  FPassword := Password;
end;

constructor TUser.Create(const Login: string);
begin
  Create(Login, '123456');
end;

function TUser.Login: string;
begin
  Result := FLogin;
end;

function TUser.Password: string;
begin
  Result := FPassword;
end;

{% endhighlight text %}

A Classe <code>TUser</code> tem 2 construtores.

O segundo construtor chama o primeiro, passando um valor *default* para o argumento *Password*.

Qualquer inicialização de atributos, sejam eles passados em argumentos pelo construtor ou não, deverá
ser executado em apenas um local, ou seja, o construtor Primário.

Este é um padrão que eu sigo em meus projetos.

Esse padrão simplifica muito quando quero saber sobre tudo que um Objeto utiliza, inicializa ou encapsula, 
bastando olhar no seu construtor Primário. Esse construtor sempre será o primeiro da lista de métodos.

##Argumentos *Default* {#argumentos-default}

A linguagem Pascal permite que funções e métodos tenham argumentos *default*. O exemplo acima poderia ser
reescrito com apenas 1 construtor, desse jeito:

{% highlight pascal %}
constructor Create(const Login, Password: string = '123456');
{% endhighlight text %}

Apesar de escrever menos, eu não recomendo utilizar argumentos *default*.

O primeiro motivo é que não podemos nomear os argumentos. Quando temos mais de um argumento *default*, 
essa "vantagem de escrever menos" não faz muito sentido.

O segundo motivo é que eu prefiro gerar os valores *default* em *runtime*. Se eu quisesse
gerar uma senha aleatória, por exemplo, bastaria criar um método <code>RandomPassword</code> e chamá-lo no construtor 
secundário, ou seja, a implementação do construtor Secundário poderia ser reescrita assim:

{% highlight pascal %}
constructor TUser.Create(const Login: string);
begin
  Create(Login, RandomPassword);
end;
{% endhighlight text %}

##Construtores Secundários e Método New() 

O uso do [Método New()]({% post_url 2016-01-10-interfaces-e-o-metodo-estatico-new %}) pode ser considerado um substituto de 
construtores Secundários.
Se sua Classe implementa uma ou mais [Interfaces]({% post_url 2016-01-18-interfaces-em-todo-lugar %}), não haveria motivos de 
implementar construtores Secundários e Métodos New() com a mesma assinatura. 

Ao invés de criar novos construtores Secundários, basta criar novas variações de Métodos <code>New()</code>.

Exemplo.

{% highlight pascal %}
type
  IUser = interface
    function Login: string;
    function Password: string;
  end;

  TUser = class(TInterfacedObject, IUser)
  private
    FLogin: string;
    FPassword: string;
  public
    constructor Create(const Login, Password: string);
    class function New(const Login, Password: string): IUser;
    class function New(const Login: string): IUser;
    function Login: string;
    function Password: string;
  end;
{% endhighlight text %}

Assim a implementação de suas Classes ficam ainda mais simples, pois teriam apenas 1 construtor
onde todos os argumentos são inicializados. 

E você, como implementa seus construtores?
