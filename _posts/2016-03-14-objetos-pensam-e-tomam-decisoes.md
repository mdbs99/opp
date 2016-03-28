---
layout: post
title: "Objetos pensam e tomam decisões"
date: 2016-03-14
description: Objetos pensam e tomam decisões. Eles não precisam de um controlador.
summary: Objetos pensam e tomam decisões. Eles não precisam de um controlador.
image: /images/photo-1456406644174-8ddd4cd52a06.jpg
categories: 
  - OO
tags:
  - oo
  - interfaces
keywords:
  - entidade
  - entidade real
  - entity
  - real entity
  - criatura
  - contexto
---

Se você [Pensar em Objetos]({% post_url 2016-01-03-pensando-em-objetos %}) ou invés de pensar em procedimentos e funções
conseguirá implementar um código onde os Objetos conversam entre si ao invés de utilizar Programação Procedural que instrui o 
compilador sobre o que fazer linha-a-linha.

Não fazemos isso na Orientação a Objetos. Seu Objeto sabe o que fazer. 
Ele não precisa de um "controlador" (você) para lhe dizer o que fazer. Ele sabe.
Ele foi [contratado]({% post_url 2016-01-18-interfaces-em-todo-lugar %}#interfaces-sao-contratos) pra isso. Deixe-o trabalhar.

<!--more-->

![Pense]({{ page.image }})

##Pense em Objetos, não em procedimentos {#pense-em-objetos}

Um Objeto deve ter uma identidade bem definida.
Sua Classe deve [implementar apenas uma responsabilidade]({% post_url 2016-03-07-classes-devem-implementar-apenas-uma-responsabilidade %}).
Essa responsabilidade é determinada por um [contrato]({% post_url 2016-01-18-interfaces-em-todo-lugar %}#interfaces-sao-contratos) que 
representa uma possível abstração de alguma Entidade.

Mas como saber se seu Objeto representa uma Entidade ou se ele é apenas um 
[balde de funções e dados]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}#objeto-nao-e-um-balde-de-funcoes-e-dados)?

Algumas classes são mais difíceis de perceber enquanto outras são tão claras como a água. Veja: 

{% highlight pascal %}
type
  TDataModule1 = class(TDataModule)
    { fields, components...}
  public
    function AddNewCustomer(Customer: TCustomer): Integer;
    function AddNewUser(User: TUser): Integer;
    function GetCustomer(Id: Integer): TCustomer;
    function GetEmployeeSalary(EmployeeId: Integer): Currency;
    { more...}
  end;
{% endhighlight text %}

O exemplo acima, apesar de ser codificado como uma Classe, ter a sintaxe de uma Classe, se comportar como uma Classe etc, na verdade,
não representa nenhuma abstração de uma Entidade sendo, portanto, um **código procedural**, ou seja, um 
[balde de funções e dados]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}#objeto-nao-e-um-balde-de-funcoes-e-dados).

Quando [postei sobre *DataModules*]({% post_url 2016-02-22-datamodule-e-apenas-um-container %}) imaginei que muitos programadores
poderiam não entender — num primeiro momento — meu motivo em dizer que *DataModules* são apenas recipientes e que não devem conter regras de negócio
e muito menos serem utilizados como instâncias de **Classes de Negócio**. Para minha surpresa o post foi muito bem recebido pela maioria dos leitores.

Acho que o exemplo de código acima deixa bem claro o que eu quis dizer.

Ora, um *DataModule*, na grande maioria das vezes, não representa uma Entidade
por não ter uma identidade bem definida. Após 10 minutos e 5 componentes adicionados, 
ele vira um [balde de funções e dados]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}#objeto-nao-e-um-balde-de-funcoes-e-dados), ou seja,
uma Classe utilitária.

##Objetos pensam... como?

Você deve fazer com que as decisões de negócio ou sistema devam ser tomadas pelos próprios objetos e não por um "controlador".

Vamos inventar um exemplo.

Imagine um *Form* onde temos um *TButton* para mostrar ao usuário qual o valor da comissão, neste mês, de um determinado empregado selecionado.
Cada funcionário tem um salário fixo e, talvez, uma comissão que é determinada pelo tipo de registro em seu cadastro. Os tipos 1 e 2 tem um cálculo completamente
diferente e para todos os outros a comissão será de 2%.

###A Versão Procedural: {#versao-procedural}

Nessa versão do código tudo é feito num único lugar, no evento do botão.

É claro que você poderá me dizer como reescrever o código. Dividir em partes. Criar outros "métodos", etc. Mas o ponto aqui é comparar um Código
Procedural com um Orientado a Objetos de verdade.

{% highlight pascal %}
procedure TForm1.Button1Click(Sender: TObject);
var
  Id: Integer;
  EmployeeType: Integer;
  Salary: Currency;
begin
  Id := SelectedEmployeeId;
  Query.Close;
  Query.Params.ParamByName('id').Value := Id;
  Query.Open;
  
  Salary := Query.FieldByName('salary').AsCurrency;  
  
  EmployeeType := Query.FieldByName('etype').AsInteger;
  case EmployeeType of
    1: Salary := Salary + CalcCommission1(Id, {parameters});
    2: Salary := Salary + CalcCommission2(Id, {parameters});
  else
    Salary := Salary + (Salary * 0.02);
  end;

  ShowMessage('Total Salary = ' + CurrToStr(Salary));
end;
{% endhighlight text %}
 
Quem nunca fez um código parecido com esse em todos esses anos de *Object Pascal*? Eu já. Muito mais do que eu gostaria.

No clique de um único botão:

  1. Utilizei uma *query* global;
  2. Utilizei 3 variáveis locais;
  3. Utilizei 2 funções para cálculo de comissão;
  4. Fiz um cálculo de comissão diretamente no código do evento(!)
  5. Apresentei o valor calculado ao usuário.
  
Esse código é Procedural, **muito acoplado** e impossível de ser utilizado em *UnitTests*.

Melhor vermos uma outra opção.

###A Versão Orientada a Objetos: {#versao-orientada-a-objetos}

Para essa versão, é claro, temos que ter as Interfaces que representam cada Entidade do nosso domínio.
Mesmo um Valor ou Salário [também são Entidades]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %})!

{% highlight pascal %}
type
  ICurrencyValue = interface
    function Value: Currency;
    function AsString: string;
  end;

  ISalary = interface
    function Value: ICurencyValue;
  end;
  
  IEmployee = interface
    { ... }
  end;
{% endhighlight text %}

Com as Interfaces definidas, não fica difícil implementar as Classes utilizando o 
[Decorator Design Pattern](https://en.wikipedia.org/wiki/Decorator_pattern),
o mesmo utilizado [aqui]({% post_url 2016-03-07-classes-devem-implementar-apenas-uma-responsabilidade %}#implementacao-oo). Mas o código
das Classes são irrelevantes para o entendimento do problema. E é assim mesmo que deve ser! Não precisamos saber o código interno de uma 
Classe para podermos utilizá-la. Apenas a sua Interface é suficiente.

Então o código do evento no botão fica assim:

{% highlight pascal %}
procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage(
    'Total Salary = ' + 
      TEmployeeSalaryWithCommission.New(
        TEmployeeSalary.New(
          TEmployee.New(SelectedEmployeeId)
        )
      )
      .Value.AsString
  );
end;
{% endhighlight text %}

Mesmo sem o código das Classes — apenas para não "poluir" demais o post — acredito que dá pra entender perfeitamente o que seria implementado:

  1. A classe *TEmployee* implementa *IEmployee*;
  2. A classe *TEmployeeSalary* e *TEmployeeSalaryWithCommission* implementam *ISalary*;
  3. A classe *TEmployeeSalaryWithCommission* é um *decorator* para *TEmployeeSalary*;

Diferentemente do primeiro exemplo, no clique de um único botão:

  1. Instanciei *TEmployee*;
  2. Instanciei *TEmployeeSalary*;
  3. Decorei a instancia de *TEmployeeSalary* com *TEmployeeSalaryWithCommission*;
  4. Chamada para *Value* — uma instancia de *ISalary* — e depois para o método *AsString*;
  
Não há condicionais, cálculos ou Regras de Negócio "espalhadas" no código. Apenas objetos "conversando".

Claro que uma *query* será executada em algum lugar — talvez encapsulado dentro de *TEmployee* — assim como os cálculos de comissões terão que 
ser implementados em *TEmployeeSalary* ou em algum outro objeto. Mas isso não é mover o problema para outro lugar... não! Isso é encapsular
o problema em objetos e deixá-los trabalhar livremente. 

Nessa versão você não vê as regras, os cálculos. Você, como utilizador dessas Classes, não precisa
pensar em procedimentos e na ordem de chamada entre eles. **Deixe que os Objetos pensem por você.**

Até logo.
