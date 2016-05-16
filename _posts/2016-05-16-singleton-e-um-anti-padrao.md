---
layout: post
title: Singleton é um anti-padrão?
date: 2016-05-16
description: Como utilizar o padrão Singleton da maneira correta, sem acoplamento de classes.
summary: Como utilizar o padrão Singleton da maneira correta, sem acoplamento de classes.
image: /images/photo-1462826743322-63f0fbb29f87.jpg
categories: 
  - OO
tags:
  - singleton
  - pattern
keywords:
  - singleton
  - pattern
  - design pattern
  - padrão de projetos
  - padrão
---

O padrão *Singleton* garante a existência de **apenas uma instância** de **uma classe**, 
mantendo **um ponto global** de acesso ao seu Objeto.

O **conceito** do *Singleton* não está errado, mas sua **implementação** clássica está.

<!--more-->

![Imagem]({{ page.image }})

Ter um **ponto global de acesso** é bom.

Garantir uma **única instância** para um Objeto, pode ser bom.

O **erro** é ter uma única instância de uma **classe**.

Por que?

Simples. Você não deve utilizar pontos globais com **Referência Direta** em seu código.

Quando digo Referência Direta quero dizer:

  * Variáveis globais
  * Métodos estáticos que não trabalham com *Interfaces*

##Variáveis globais {#variaveis-globais}

Você já deve saber que não devemos utilizar variáveis globais. 

Seu uso é considerado um anti-padrão porque seu conteúdo pode ser modificado/acessado de 
qualquer parte do seu código. Haverá uma dependência direta dessa variável aumentando a 
complexidade do código. Qualquer alteração no valor dessa variável poderá ocasionar problemas
em qualquer parte do código que a utilize.

##Métodos estáticos {#metodos-estaticos}

Um [método estático]({% post_url 2016-02-15-nao-utilize-metodos-estaticos %}) é acessado
diretamente na Classe. Assim como uma variável global, o método estático é uma dependência 
direta e isso não é bom. No entanto o valor retornado 
por um método estático pode ser dinâmico. Pode ser um Objeto. Pode ser uma instância
de uma *Interface*.

O [Método New()]({% post_url 2016-01-10-interfaces-e-o-metodo-estatico-new %}) é estático, 
mas seu retorno é dinâmico. Ele também retorna uma instância de uma *Interface* ao invés de 
um tipo primitivo ou Classe.

##O Padrão *Singleton* {#padrao-singleton}

Um [*Singleton*](https://en.wikipedia.org/wiki/Singleton_pattern) utiliza uma variável global, 
que está encapsulada dentro da própria classe. Também utiliza um método estático, que irá 
retornar a instância única encapsulada na Classe. Essa instância é do tipo da própria Classe.

Precisamos utilizar esse Padrão?

Sim, por simplicidade.

Explico.

É possível nunca utilizar o *Singleton*. Basta você utilizar Injeção de Dependência nos
[construtores]({% post_url 2016-03-21-construtores-da-classe-primario-secundarios %}) das Classes.

Mas para cada dependência teríamos que definir um argumento em todos os construtores, de todas as 
Classes, que necessitam utilizar a dependência.

Isso irá gerar **Complexidade** no código.

Você precisa de uma única instância de Objetos para:

  * Conexão com um SGBD
  * Log de execução/*debugging*
  * Configurações da Aplicação
  * Etc  
  
São exemplos. Na verdade eu posso querer utilizar um *pool* de conexões para o SGBD; ter várias Classes
que representam as Configurações, etc. Mas, você entendeu.

Então garantir a existência de **apenas uma instância** mantendo **um ponto global** de acesso ao Objeto 
é importante. Isso minimiza a complexidade.
Ao invés de ter sempre um parâmetro a mais em todos os construtores das Classes — ou da maioria delas — eu
só preciso ter um único ponto de acesso para "algum" Objeto.

O *Singleton* clássico parece resolver isso. Sim, mas ele nos traz um problema maior do que o benefício.

###O Problema {#o-problema}

Não há como implementar um *Singleton* sem haver uma **variável** para retornar **uma única instância**.

O problema não é a variável, pois ela estará encapsulada na Classe. Isso quer dizer que
ela estará protegida. Bem, talvez não tão protegida mas ainda melhor que uma variável global.

O problema também não é o **método estático** — mesmo tendo seu uso desencorajado na maioria dos casos —
pois podemos retornar qualquer valor.

O **real problema** do Padrão *Singleton*, em sua implementação clássica, é o **tipo de retorno** do método estático.

###Implementação Clássica {#implementacao-classica}

Existem variações de implementação do Padrão *Singleton* que utilizam algumas técnicas para inicializar 
a variável encapsulada, ou seja, a instância que será retornada pelo método estático que, na maioria das 
vezes, é denominado `GetInstance`.

Na implementação mais básica, que pode ser feita em praticamente qualquer linguagem Orientada a Objetos,
implementamos construtores da Classe 
como privados. Assim o utilizador da Classe não conseguirá instanciar Objetos dessa Classe diretamente. Depois
é definido um (ou mais) método estático — `GetInstance` — para retornar uma instância da Classe... do mesmo tipo da Classe!

Aí está o problema.

Um **grave** problema.

Se você retornar uma instância do mesmo tipo da Classe, seu código terá uma dependência direta com essa Classe.
Só isso pode arruinar qualquer tentativa de testes automatizados no código.

Veja um exemplo:

{% highlight pascal %}
procedure TUserAction.ChangePassword(
  User: IUser; const NewPassword: string);
begin
  if NewPassword = '' then
    raise Exception.Create('Invalid Password');
  if Length(NewPassword) < 8 then
    raise Exception.Create('Must have 8 characters or more');
  User.Password := NewPassword;
  TMSSQL.GetInstance.Save(User);
end;
{% endhighlight text %}

É um código idiota, mas serve ao propósito.

Vejamos. 

Existe um Classe `TUserAction` que tem um método para alterar o *password* do usuário logado. Esse método tem 
algumas validações simples.

Alteramos seu *password* e depois essa alteração será persistida no SGBD.

A chamada `TMSSQL.GetInstance` é um *Singleton*. O método irá retornar uma conexão `TMSSQLConnection` para o MSSQL.

Utilizamos então o método `Save(User)` para persistir as alterações.

Não importa como `Save(User)` sabe como persistir um usuário. Isso não é relevante. O problema aqui é
como iremos testar `ChangePassword` sem fazer a integração/conexão real com o SGBD (MSSQL).

Eu só quero testar o método utilizando um **teste automatizado**. Eu não quero ter que conectar num SGBD real,
utilizar *user/password* reais e muito menos alterar algum dado.

Quero fazer **testes de unidade**, não **testes de integração**.

Bem, se `TMSSQL.GetInstance` me retorna uma instância de `TMSSQLConnection`, uma Classe concreta, **não é possível
fazer o teste automatizado**.

É um exemplo simples. Vejo isso na maioria dos códigos "*Object Pascal*" — que não tem nada de Orientação a Objetos.

Na verdade, a maioria iria utilizar uma instância de algum
[DataModule]({% post_url 2016-02-22-datamodule-e-apenas-um-container %}) e iria substituir a chamada `TMSSQL.GetInstance`
por apenas `DM.conMSSQL` ou seja, duas variáveis. Triste.

Entendeu por que não devemos utilizar uma instância de Classe concreta?

E qual a solução?

Retornar uma instância de [Interface]({% post_url 2016-01-18-interfaces-em-todo-lugar %}) ao invés
de uma instância do tipo Classe.

###Implementação Sugerida {#implementacao-sugerida}

O que irei sugerir para a implementação de um *Singleton* é tão simples quanto parece, mas com um ganho
incontestável: **Desacoplamento**.

Utilize *Interfaces*.

Ao invés de `TMSSQL.GetInstance` retornar um `TMSSQLConnection`, o método irá retornar um `IConnection` ou seja,
uma *Interface*.

Se agora temos um retorno que é uma *Interface*, podemos ter qualquer Classe que implemente essa *Interface*. 

A Classe `TMSSQLConnection` deverá implementar `IConnection`.

E onde está desacoplamento?

Você poderá retornar qualquer instância que implemente `IConnection` e isso pode ser feito de várias formas.

Se quiser escrever menos, utilize **diretivas de compilação**.

{% highlight pascal %}
class function TMSSQL.GetInstance: IConnection;
begin
  if not Assigned(FInstance) then
  begin
    {$IFDEF TEST}
      FInstance := TFakeConnection.Create;
    {$ELSE}
      FInstance := TMSSQLConnection.Create('user', 'password');
    {$ENDIF}
  end;
  Result := FInstance;
end;
{% endhighlight text %}

Essa é a maneira "Rápido e sujo". Você pode começar por aí caso nunca tenha pensado nisso. Quando estiver
em "modo de teste", ative a diretiva `TEST`, do contrário a Classe de produção será utilizada.

A Classe `TFakeConnection` não se conecta em nenhum SGBD. Talvez você possa utilizá-la para fazer 
um log das instruções SQL ou **não fazer nada** quando chamar o método `Save`. 

Se você está testando as **Regras de Negócio**, a persistência é irrelevante.

Depois você pode **refatorar** esse código e deixá-lo mais Orientado a Objetos, utilizando 
Injeção de Dependência em conjunto com [*Abstract Factory Pattern*](https://en.wikipedia.org/wiki/Abstract_factory_pattern).

Como?

Bem, em algum lugar no seu código você deverá ter uma chamada para "inicializar" a Classe `TMSSQL`.

{% highlight pascal %}
class function TMSSQL.Initialize(Factory: IConnectionFactory);
begin
  FFactory := Factory;
end;

initialization
  TMSSQL.Initialize(TDbConnectionFactory.New('mssql'));

end.
{% endhighlight text %}

Mais um vez estaremos utilizando métodos estáticos. Mas estamos em busca de simplicidade.
Não devemos ser **puristas** em Orientação a Objetos se não há nenhum benefício. Sempre haverá partes do código
que poderiam melhorar, refatorar, eliminar, etc. Enfim.

A Classe `TMSSQL` deverá ser bem pequena e sem complexidade.

O atributo `FFactory` é um `class var` assim como `FInstance`.

Então você poderá ter novas Classes que implementam `IConnectionFactory`.

No exemplo temos `TDbConnectionFactory`. Optei por utilizar um parâmetro que me diz qual "tipo" de Classe será
utilizada. Se eu quiser testar o código bastaria chamar `TDbConnectionFactory.New('test')`.

Utilize seu estilo.

Após essa alteração o método `GetInstance` pode ser refatorado:

{% highlight pascal %}
class function TMSSQL.GetInstance: IConnection;
begin
  if not Assigned(FInstance) then
    FInstance := FFactory.NewConnection(FUser, FPassword);
  Result := FInstance;
end;
{% endhighlight text %}

A chamada a `FFactory.NewConnection` irá gerar uma nova instância de `IConnection`. Qual a Classe que 
estará sendo utilizada é irrelevante para o código que utiliza `TMSSQL.GetInstance` em todos os lugares,
como deve ser. O retorno poderia ser `TMSSQLConnection`, `TFakeConnection` ou qualquer outra classe que
implemente `IConnection`.

Esse é um *Singleton* **simples**, **desacoplado** e **Orientado a Objetos**.

Até logo.
