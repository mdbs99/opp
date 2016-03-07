---
layout: post
title: "Objetos devem representar Entidades reais"
date: 2016-02-29
description: Um Objeto representa uma entidade, criatura ou qualquer coisa fora do contexto do programa.
summary: Tudo que existe fora do Contexto do software é uma Entidade que existe na vida real e deve ser representada por um Objeto dentro do programa.
image: /images/matrix20.jpg
categories: 
  - oo
tags:
  - entidade
  - objeto
keywords:
  - entidade
  - entidade real
  - entity
  - real entity
  - criatura
  - contexto
---

Tudo que existe fora do Contexto do software é uma Entidade que existe na vida 
real e deve ser representada por um Objeto dentro do programa.

Defina Classes que implementem abstrações de Entidades reais.

<!--more-->

![Matrix®]({{ page.image }})

<blockquote>
  <p>
    "O que é real? Como você define o 'real'? Se você está falando
    sobre o que você pode sentir, o que você pode cheirar, o que você
    pode saborear e ver, o real são simplesmente sinais elétricos
    interpretados pelo seu cérebro"
  </p>
  <footer><cite title="Morpheus, Matrix">— Morpheus, Matrix</cite></footer>
</blockquote>

##O que é uma Entidade? {#o-que-e-uma-entidade}

Não pense em Entidade somente como criaturas vivas ou objetos palpáveis ao nosso redor. Um
arquivo de computador, um pixel no monitor, um carro, uma pessoa, uma molécula... tudo
que existe fora do **Contexto do Software** é uma Entidade que existe na vida real.

No código uma Entidade é **representada ou abstraída** por Interfaces e implementada por Classes. Sim, no plural.

Uma única Interface pode ser suficiente para abstrair uma Entidade, mas há muitos casos que uma mesma 
Entidade pode ter dezenas de Interfaces representando-a em dezenas de Contextos diferentes. Ainda mais comum
é termos dezenas de Classes que implementam uma única Interface.

Gostaria de ver um simples exemplo? Vamos lá.

Bem, imagine um sistema que tem a Entidade Cliente. 

Nesse **simples** sistema eu preciso saber:

  1. Seus dados pessoais básicos;
  2. Seu endereço;
  3. Seu *status* (ativo, cancelado ou suspenso);
  4. Seu Cartão de Crédito;

Como implementar Cliente?
  
###Exemplo 1: Clássica implementação "Tudo em um"

A maioria das pessoas iriam definir **todos** os campos (atributos) 
em uma única Classe, mais ou menos assim:

{% highlight pascal %}
type
  TCustomer = class
  private
    FName: string;
    FBirthday: TDateTime;
    FEmail: string;
    FAddress: TAddress;
    FStatus: TStatus;
    FCreditCard: TCreditCard;
  public
    constructor Create;
    destructor Destroy; override;
    property Name: string read FName write FName; 
    property Birthday: TDateTime read FBirthday write FBirthday; 
    property Email: string read FEmail write FEmail; 
    property Address: TAddress read FAddress; 
    property Status: TStatus read FStatus write FStatus; 
    property CreditCard: TCreditCard read FCreditCard; 
  end;
{% endhighlight text %}

Num sistema real de médio ou grande porte sabemos que um
Cliente poderia ter muitos outros atributos, certo? Essa Classe iria crescer e crescer... 

O que há de "errado"?

A resposta simples é: A Classe é **complexa** demais.

Nesta implementação, a Classe expõe publicamente 4 atributos – lembre-se que poderia ser muito mais! – que não faz parte
da **abstração mais simples** de um Cliente.

Não concorda? Não achou complexo?

Acho que faltou definir o que é um Cliente:

>"Um Cliente representa uma pessoa para qual vendemos serviços/produtos através do nosso site". 

Por definição geral todo cliente tem um Nome e, talvez, ele queira nos dizer sua data de nascimento.
Mas não citei *Email*, *Address*, *Status* ou *CreditCard*.

Não adianta implementar uma Interface *ICustomer* nessa Classe e dizer que está programando 
Orientado a Objetos. Uma Interface que cresce a cada novo requisito não é uma Interface muito útil.

Essa Classe foi definida pensando mais nas tabelas – que
porventura esse sistema venha a ter – do que na real abstração de um Cliente.

Será que todo Cliente é igual, em todos os sistemas? Tenho certeza que não.

Então como reutilizamos essa Classe?
Não reutilizamos. Ela foi **codificada rigidamente**, utilizando atributos e conceitos para apenas 1 sistema e mesmo assim, 
está errada!

Errada?!

Bem... respire por um momento.

Espero que você já tenha lido meus outros posts. Espero que você já tenha entendido que quando digo ERRADO ou CERTO é porque 
eu quero que você raciocine porque estou dizendo isso.

Não há CERTO ou ERRADO **absolutamente** na programação. Orientação a Objetos não é uma ciência exata – na verdade programação, 
seja qual for o tipo, não é uma ciência exata. Mas temos nossas convicções, nossa experiência, o que deu certo ou errado no passado... 

![Free your mind](/images/e3c71ca4993ce03a8e48256fcfa3343b.jpg)

Quando estou afirmando alguma coisa, gostaria que você deixasse de lado, por um momento, suas convicções pessoais mantendo sua mente aberta,
para tentar entender meus motivos de escrever tais afirmações.

Eu estou lhe mostrando uma porta, mas só você poderá abrí-la.

###Exemplo 2: Pensamento Objeto

Pense.

**Contexto #1:**
Você quer fazer uma assinatura online de uma revista "Object Pascal Programming". Precisa preencher um cadastro contendo
seus dados pessoais, seu endereço, além do seu cartão de crédito.

**Contexto #2:**
O setor financeiro do site no qual você fez a assinatura precisa saber se o seu cartão de crédito é válido.

**Contexto #3:**
O setor de *marketing* precisa enviar, por e-mail, qual serão as próximas matérias da próxima edição.

Percebeu que cada contexto necessita de informações distintas?

No Contexto #1 é necessário o Endereço e Cartão de Crédito. No #2 somente o cartão é necessário. No #3 o e-mail e
o *Status* são necessários para enviar e-mails somente aos clientes ativos.

Então temos, de uma forma bem sucinta, a definição das Interfaces:

{% highlight pascal %}
type
  ICustomer = interface
    function Name: string;
    function Birthday: TDateTime;
  end;
  
  IEmail = interface
    function Value: string;
  end;
  
  IAddress = interface
    function Address: string;
    function Number: Integer;
  end;
  
  IStatus = interface
    function Value: Integer
  end;
  
  ICreditCard = interface
    function Number: string;
    function Name: string;
    function ExpiresIn: TDateTime;
  end;
{% endhighlight text %}

Abaixo as possíveis implementações para cada contexto relacionado com Cliente:

{% highlight pascal %}
// Contexto #1
type 
  TCustomer = class(TInterfacedObject, ICustomer)
  public  
    constructor Create(Id: Integer);
    {...}
  end;
  
  TCustomerAddress = class(TInterfacedObject, IAddress)
  public
    constructor Create(Customer: ICustomer);
    {...}
  end;

// Contexto #1 e #2
type
  TCustomerCreditCard = class(TInterfacedObject, ICreditCard)
  public
    constructor Create(Customer: ICustomer);
    {...}
  end;
  
// Contexto #3
type
  TCustomerStatus = class(TInterfacedObject, IStatus)
  public
    constructor Create(Customer: ICustomer);
    {...}
  end;
{% endhighlight text %}

Então se queremos saber o cartão de crédito de um determinado Cliente, basta codificar:

{% highlight pascal %}
begin
  ShowMessage(
    TCustomerCreditCard.New(
      TCustomer.New(
        123
      )
    ).Number
  );
end;
{% endhighlight text %}

Dica: Veja mais sobre o **Método New** [aqui]({% post_url 2016-01-10-interfaces-e-o-metodo-estatico-new %}).

No Exemplo #2 eu tenho:

  1. Possível **reutilização de código** em vários sistemas;
  2. Interfaces e Classes **pequenas, simples e coesas**;
  3. **Manutenção fácil**;
  4. Interfaces e Implementações para cada Entidade distinta.

Se você ainda está pensando:

  1. Escreve demais
  2. É mais complexo que o primeiro exemplo
  3. Dá mais trabalho
  4. [Escolha sua desculpa]

Bem... sugiro pensar novamente.

Você tem que [pensar em Objetos]({% post_url 2016-01-03-pensando-em-objetos %}). Tem que pensar no longo prazo. Tem que pensar na manutenção. Tem que pensar na
verdadeira reutilização de código. Tem que pensar em Contextos.

  * Se você vai a praia, não precisa levar um comprovante de endereço de sua residência com você;
  * Se você vai ao mercado, precisa levar dinhero ou cartão, caso queira levar alguma coisa de lá;
  * Se você vai ao trabalho precisa ter algum cartão que o identifique – na maioria dos casos;
  * Se está em casa, preparando um jantar romântico para você e sua esposa, vocês só precisam de si mesmos;
  
São **contextos diferentes** de você mesmo!

Você não precisa de tudo que é relacionado com você em todos os momentos, em todos os contextos. Assim são com os Objetos.

Aplique essa "regra" para não ter Classes gigantescas em seu código, crescendo a cada dia.

##Conclusão

Cada Entidade deverá ter uma Interface ou Interfaces que a definiam, assim como poderá ter várias implementações. 

Cada Objeto deverá representar uma Entidade num determinado momento para um determinado Contexto.

Esse é o segredo de um bom código Orientado a Objetos. Mantenha suas Interfaces e Classes pequenas, coesas e simples. 
  
Até logo.