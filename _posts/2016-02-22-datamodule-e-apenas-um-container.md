---
layout: post
title: "DataModule é apenas um recipiente, e só isso"
date: 2016-02-22
description: Não utilize DataModules para implementar regras de negócio
summary: DataModule é apenas um recipiente. Não utilize-o para implementar regras de negócio.
image: /images/photo-1455487890814-f11ab4eaec4b.jpg
categories: 
  - oop
  - object pascal
tags:
  - datamodule
keywords:
  - datamodule
  - container
---

Não utilize um *DataModule* como uma Classe de Negócio, implementando métodos públicos e utilizando-o como um
*Singleton*. Isso é errado. Você deixará seu sistema acoplado a esta única implementação e não será possível
testá-lo da maneira correta.

Um *DataModule* não deve conter Regras de Negócio.

<!--more-->

![Container]({{ page.image }})

Utilize *DataModules* apenas com recipientes de componentes e nada mais.

Um *DataModule* deverá conter, na maioria das vezes, componentes de relatório e *queries*, todos relacionados
a um contexto bem específico e delimitado.

Quanto menos componentes adicionar, melhor, mais simples e com menos gasto de memória do computador.

Componentes de relatório foram concebidos para utilizar dados tabulados. Aqui a Orientação a Objetos não ajuda muito.
A maneira mais eficiente para gerar um relatório é trabalhar com *queries*, retornando conjuntos de dados relacionados
entre si.

Vou mostrar um exemplo real – sistema em produção – que demostra como eu utilizo *DataModules*.

##Implementação de um Relatório

Para a implementação de um relatório, [a primeira coisa a fazer]({% post_url 2016-01-18-interfaces-em-todo-lugar %})
é definirmos uma abstração para a definição de um Relatório, ou seja, uma Interface.

Para a Interface apenas 3 métodos são necessários:

  1. *Show*: Para pré-visualizar o relatório na tela.
  2. *Print*: Para mostrar somente o *PrintDialog* para o usuário para impressão direta.
  3. *PDF*: Para gerar um PDF no disco.
  
{% highlight pascal %}
type
  IReport = interface
  ['{29414A64-69BA-40A1-970B-2CD61C633508}']
    function Show: IReport;
    function Print: IReport;
    function PDF(const FileName: string): IReport;
  end;
{% endhighlight text %}

O sistema que utiliza esse código foi codificado em Delphi 7, utilizando ADO e *ReportBuilder®*(RB), no 
entanto essa é uma Interface genérica o bastante para ser utilizado por qualquer versão do Delphi/Lazarus,
com a possibilidade de implementar e utilizar qualquer gerador de relatório.

O próximo passo é codificar uma implementação de <code>IReport</code> para o RB.

Abaixo contém apenas a Interface da classe. Não é necessária a implementação para o 
entendimento dos conceitos que quero mostrar.

{% highlight pascal %}
type
  TRBuilderReport = class(TInterfacedObject, IReport)
  private
    FReport: TppReport;
  public
    constructor Create(Report: TppReport);
    class function New(Report: TppReport): IReport;
    function Show: IReport;
    function Print: IReport;
    function PDF(const FileName: string): IReport;
  end;
{% endhighlight text %}

O motivo de termos um [Método New]({% post_url 2016-01-10-interfaces-e-o-metodo-estatico-new %}) já foi
explicado anteriormente.

Bem, nesse momento eu tenho uma Interface para qualquer tipo de relatório e uma implementação genérica
para utilizar o *design* do RB.

Repare que na implementação do RB eu tenho que injetar a dependência <code>Report: TppReport</code>. 
A instância desse componente estará num *DataModule*.

Aqui está o *design* da classe <code>TDividaAtivaMedicoModule</code>, que é o *DataModule* que vou 
utilizar nesse exemplo.

![DataModule](/images/divativa-datamodule-1.jpg)

Todo o *design* dos relatórios, assim como o SQL é propriedade privada e não será mostrado aqui.

Garanto que neste *DataModule* não há métodos públicos ou privados. Apenas eventos dos componentes do
relatório são implementados, pois essa é a maneira de customizar a impressão do relatório.

Para o contexto do problema, e de acordo com as regras de negócio, são necessários 3 relatórios como
pode-se ver na imagem, mais irei demostrar a implementação de apenas um deles.

###Um *DataModule* é um *Singleton*

Quando você instancia um *DataModule* ele se torna um *Singleton* global para todo projeto. Não importa se
você está utilizando a variável global que a IDE adiciona automaticamente ou qualquer outra variável. Isso quer dizer
que se eu tenho 2 *Forms* que tem componentes "apontando" para componentes num *DataModule*, ambos os *Forms* 
estarão utilizando a mesma instância do *DataModule*, compartilhando os mesmos componentes.

Eu não quero isso.

---

**Dica:** [*Singleton*](https://goo.gl/2zkz3U) é um anti-padrão. 

---

A maneira para fazer uma instância do *DataModule* não ser global é chamar <code>RemoveDataModule</code> no seu construtor:

{% highlight pascal %}
procedure TDataModule1.DataModuleCreate(Sender: TComponent);
begin
  inherited;
  RemoveDataModule(Self);
end;
{% endhighlight text %}

Por que fazer isso?

Bem, utilizar variáveis globais nunca é recomendado. A abordagem proposta aqui é mais *thread-safe*.
Poderia apresentar o relatório ao usuário e gerar um PDF ao mesmo tempo, utilizando *thread*.
Também poderia gerar relatórios diferentes ao mesmo tempo passando parâmetros diferentes às *queries*.

###A Classe de Negócio

Instanciar um *DataModule*, atribuir parâmetros às *queries*, chamar o relatório... o passo-a-passo
é mais ou menos assim, certo? No entanto eu espero que você **não** tenha pensado em fazer isso diretamente 
no código de um *Form*, por exemplo.

Precisamos ter uma – ou muitas outras – **Classe de Negócio** para encapsular e implementar o relatório.

Para o único relatório que iremos implementar – *AvisosReport* – temos uma classe chamada <code>TDividaAtivaMedicoAvisosReport</code>.

{% highlight pascal %}
type
  TDividaAtivaMedicoAvisosReport = class(TInterfacedObject, IReport)
  private
    FModule: TDividaAtivaMedicoModule;
    FReport: IReport;
  public
    constructor Create(NotificacaoId: Integer);
    class function New(NotificacaoId: Integer): IReport;
    destructor Destroy; override;
    function Show: IReport;
    function Print: IReport;
    function PDF(const FileName: string): IReport;
  end;

implementation  
  
{ TDividaAtivaMedicoAvisosReport }

constructor TDividaAtivaMedicoAvisosReport.Create(NotificacaoId: Integer);
begin
  inherited Create;
  FModule := TDividaAtivaMedicoModule.Create(nil);
  FReport := TRBuilderReport.Create(FModule.AvisosReport);
  with FModule.NotificacoesQuery.Parameters do
  begin
    ParamByName('notificacao_id').Value := NotificacaoId;
    ParamByName('fase').Value := CDividaAtivaFases.AvisoId;
  end;
end;

class function TDividaAtivaMedicoAvisosReport.New(NotificacaoId: Integer): IReport;
begin
  Result := Create(NotificacaoId);
end;

destructor TDividaAtivaMedicoAvisosReport.Destroy;
begin
  FModule.Free;
  inherited;
end;

function TDividaAtivaMedicoAvisosReport.Show: IReport;
begin
  Result := FReport.Show;
end;

function TDividaAtivaMedicoAvisosReport.Print: IReport;
begin
  Result := FReport.Print;
end;

function TDividaAtivaMedicoAvisosReport.PDF(const FileName: string): IReport;
begin
  Result := FReport.PDF(FileName);
end;
{% endhighlight text %}

Essa implementação é real e está em produção. O código poderia ser melhorado, é claro, mas isso é
irrelevante agora.

Essa classe, a <code>TDividaAtivaMedicoAvisosReport</code>, funciona como um decorador para <code>IReport</code>,
e tem uma implementação implícita de <code>TRBuilderReport</code>. Como eu disse, pode melhorar. Enfim.

O mais importante são essas 2 linhas:

{% highlight pascal %}
  FModule := TDividaAtivaMedicoModule.Create(nil);
  FReport := TRBuilderReport.Create(FModule.AvisosReport);
{% endhighlight text %}

Um *DataModule* é instanciado em <code>FModule</code>. Esse *DataModule* é retirado do "estado global" utilizando
<code>RemoveDataModule</code> em seu construtor.

Então temos uma instância de <code>TRBuilderReport</code> em <code>FReport</code> que é inicializado com um
relatório específico: <code>FModule.AvisosReport</code>.

A implementação da classe, a partir daí, apenas repassa as solicitações para <code>FReport</code> utilizando os
métodos da Interface <code>IReport</code>.

Dessa maneira o restante do código não precisa se preocupar em inicializar variáveis, campos calculados, cores,
abrir *queries*, etc pois tudo que o sistema precisa é utilizar uma instância de <code>TDividaAtivaMedicoAvisosReport</code>.

Essa classe poderia ter sua implementação alterada, no futuro, sem termos que reescrever código que a utilize, pois o
relatório, dados e parâmetros estão encapsulados dentro dela.

##Conclusão

Quando o sistema precisa de emitir um relatório, a Interface <code>IReport</code> é utilizada e Classes de Negócio
são implementadas para encapsular cada relatório. O sistema não sabe qual gerador de relatórios está sendo utilizado, apenas
as Classes de Negócio sabem. É *thread-safe*, simples, extensível e de fácil manutenção.

Não é um método perfeito, mas demostra o uso eficiente de um componente *RAD* sendo utilizado num contexto mais 
Orientado a Objetos.

Para mim esse é o uso correto de um *DataModule*, ou seja, ser apenas um recipiente de componentes e nada mais. 

Até logo.
