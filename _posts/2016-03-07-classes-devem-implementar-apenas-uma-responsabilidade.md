---
layout: post
title: "Classes devem Implementar apenas uma Responsabilidade"
date: 2016-03-07
description: Uma Classe com apenas uma Responsabilidade é simples para usar, modificar e reutilizar.
summary: Uma Classe com apenas uma Responsabilidade é simples para usar, modificar e reutilizar.
image: /images/photo-1445783426434-670f9b34e183.jpg
categories: 
  - OO
tags:
  - oo
  - responsabilidade
keywords:
  - responsability
---

O Princípio da Responsabilidade Única (Single Responsibility Principle) afirma que, se tivermos 
mais de uma razão para mudar a implementação de uma Classe, devemos dividir o comportamento em 
duas ou mais Classes.

<!--more-->

![Funcionalidades]({{ page.image }})

##Uma única responsabilidade {#unica-responsabilidade}

A implementação de uma Classe deve ser o mais Simples possível.
Classes com mais de uma Responsabilidade são Complexas.

Não confunda Objeto com Classe. Objetos podem ser **Complexos** e ter mais de uma
Responsabilidade devido ao uso da **Composição e Decoração de Objetos**, no
entanto as Classes que compõem esses Objetos devem implementar apenas uma única
Responsabilidade.

>"Uma Classe com apenas uma Responsabilidade é Simples para usar, modificar e reutilizar"

Mas como é possível ter Objetos que são mais complexos enquanto suas Classes são simples?

Bem, vamos implementar um módulo simples de *Log* para responder a esta pergunta. 
Primeiro vamos utilizar a **Programação Procedural** com Classes do tipo "tudo em um" e depois a **Orientada a Objetos**, 
com Classes simples utilizando [Decorator Design Pattern](https://en.wikipedia.org/wiki/Decorator_pattern),
dividindo as funcionalidades em Classes menores e mais reutilizáveis.

Um sistema hipotético necessita salvar um *Log* com tudo o que foi processado.

Este *Log* deverá implementar os requisitos a seguir:

  1. Possibilidade de salvar a informação em Arquivo Texto
  2. Possibilidade de salvar a informação em Banco de Dados
  3. Possibilidade de enviar o *Log* por e-mail


###1-Implementação "Tradicional" e Procedural {#implementacao-tradicional}
  
![Procedural e antiga](/images/photo-1453560814059-d646dffd32fd.jpg)

De acordo com os requisitos acima, **parece muito simples** pensar numa única Classe com "apenas" 4 métodos.

    type
      TLog = class
      private
        FInfo: TStrings;
        FMail: TMail;
      public
        constructor Create;
        destructor Destroy; override;
        function Add(const S: string): TLog;
        function SaveToFile(const FileName: string): TLog;
        function SaveToDB(DB: IDatabase): TLog;
        function SendMail(const From: string): TLog;
      end;
      
    implementation

    constructor TLog.Create;
    begin
      inherited Create;
      FInfo := TStringList.Create;
      FMail := TMail.Create;
    end;

    destructor TLog.Destroy;
    begin
      FInfo.Free;
      FMail.Free;
      inherited;
    end;

    function TLog.Add(const S: string): TLog;
    begin
      Result := Self;
      FInfo.Add(S);
    end;

    function TLog.SaveToFile(const FileName: string): TLog;
    begin
      Result := Self;
      FInfo.SaveToFile(FileName);
      FInfo.Clear;
    end;

    function TLog.SaveToDB(DB: IDatabase): TLog;
    begin
      Result := Self;
      DB.Query(
        'insert into Log (info) values (:info)',
        TSQLParams.New
          .Add('info', ftString, FInfo.Text)
      )
      .Execute; 
      FInfo.Clear;
    end;

    function TLog.SendMail(const From: string): TLog;
    begin
      Result := Self;
      FMail.Send(From, FInfo.Text);
      FInfo.Clear;
    end;

    end.

**Observações**: 

  1. Considere <code>DB: IDatabase</code> como uma instância de uma conexão com o Banco de Dados. 
  2. Considere <code>FMail.Send(From, FInfo.Text)</code> uma instrução que envia para um destinatário a informação no corpo do e-mail.

Para utilizar a Classe não há dúvidas. Faz-se uma instância de <code>TLog</code> para chamar cada método. Seu uso é simples, mas...
  
Quais são os problemas dessa implementação?

Muitos problemas:

  1. A Classe é "complexa" demais porque lida com muitas responsabilidades e consequentemente muito mais propensa a alterações e bugs.
  2. A classe tem uma instância concreta de *TMail*, não sendo possível utilizarmos *libs* diferentes para o envio de e-mail.
  3. Sua interface possui muitos métodos com contextos diferentes.
  4. Cada novo requisito irá gerar um novo método. A Classe não para de crescer.
  
Já vi implementações como a Classe acima, mas também já encontrei outras muito piores.

Trabalhei num sistema onde existia uma Classe de *Log* que tinha +20 métodos! Método para salvar em arquivo e no Banco de Dados; 
gerar XML; enviar por e-mail; excluir arquivos antigos; e muito mais. Era uma grande **Classe Utilitária** composta somente 
de [Métodos Estáticos]({% post_url 2016-02-15-nao-utilize-metodos-estaticos %}).
  
###2-Implementação Orientada a Objetos {#implementacao-oo}

![Orientada a Objetos](/images/photo-1457257495536-67f31bc9773d.jpg)

É claro que a [primeira coisa a fazer]({% post_url 2016-01-18-interfaces-em-todo-lugar %}) é definirmos uma abstração para *Log*. Precisamos de uma Interface.

    type
      ILog = interface
        function Add(const S: string): ILog;
        function Text: string;
        function Save: ILog;
      end;

Apenas isso.

Um *Log* precisa registrar a informação, informar e salvar em **algum lugar**.

Agora podemos implementar cada Classe, cada uma com uma responsabilidade bem distinta e única.

Primeiro uma Classe para implementar o *Log* apenas em memória — pode ser utilizado em *UnitTests*.

    type
      TLogInMemory = class(TInterfacedObject, ILog)
      private
        FInfo: TStrings;  
      public
        constructor Create(const S: string);
        //class function New...
        destructor Destroy; override;
        function Add(const S: string): ILog;
        function Text: string;
        function Save: ILog;
      end;
      
    implementation

    constructor TLogInMemory.Create(const S: string);
    begin
      inherited Create;
      FInfo := TStringList.Create;
      FInfo.Add(S);
    end;

    destructor TLogInMemory.Destroy;
    begin
      FInfo.Free;
      inherited Destroy;
    end;

    function TLogInMemory.Add(const S: string): ILog;
    begin
      Result := Self;
      FInfo.Add(S);
    end;

    function TLogInMemory.Text: string;
    begin
      Result := FInfo.Text;
    end;

    function TLogInMemory.Save: ILog;
    begin
      Result := Self;
      FInfo.Clear;
    end;

No método <code>Save</code> não há nada para registrar pois o *Log* só trabalha em memória.

Teremos outra Classe para fazer o *Log* num Arquivo Texto:

    type
      TLogToFile = class(TInterfacedObject, ILog)
      private
        FOrigin: ILog;
        FFileStream: TFileStream;
      public
        constructor Create(Origin: ILog; const FileName: string);
        //class function New...
        destructor Destroy; override;
        function Add(const S: string): ILog;
        function Text: string;
        function Save: ILog;
      end;
      
    implementation

    constructor TLogToFile.Create(Origin: ILog; 
      const FileName: string);
    begin
      inherited Create;
      FOrigin := Origin;
      FFileStream := TFileStream.Create(FileName, fmOpenWrite);
    end;

    destructor TLogToFile.Destroy;
    begin
      FFileStream.Free;
      inherited Destroy;
    end;

    function TLogToFile.Add(const S: string): ILog;
    begin
      Result := Self;
      FOrigin.Add(S);
      FFileStream.Write(S);
    end;

    function TLogToFile.Text: string;
    begin
      Result := FOrigin.Text;
    end;

    function TLogToFile.Save: ILog;
    begin
      Result := Self;
      FInfo.SaveToFile(FFileName);
      FOrigin.Save;
    end;

Repare que há um parâmetro <code>Origin</code>. Isso significa que essa classe
irá gerar Objetos que trabalham "decorando" outros Objetos que se originaram em outro momento.

Da mesma forma teremos outra Classe para fazer o *Log* no Banco de Dados:

    type
      TLogToDB = class(TInterfacedObject, ILog)
      private
        FOrigin: ILog;
        FDB: IDatabase;  
      public
        constructor Create(Origin: ILog; DB: IDatabase);
        //class function New...
        function Add(const S: string): ILog;
        function Text: string;
        function Save: ILog;
      end;
      
    implementation

    constructor TLogToDB.Create(Origin: ILog; 
      DB: IDatabase);
    begin
      inherited Create;
      FOrigin := Origin; 
      FDB := DB;
    end;

    function TLogToDB.Add(const S: string): ILog;
    begin
      Result := Self;
      FOrigin.Add(S);
    end;

    function TLogToDB.Text: string;
    begin
      Result := FOrigin.Text;
    end;

    function TLogToDB.Save: ILog;
    begin
      Result := Self;
      DB.Query(
        'insert into Log (info) values (:info)',
        TSQLParams.New
          .Add('info', ftString, Self.Text)
      )
      .Execute; 
      FOrigin.Save;
    end;

**Observação**:

Você pode pensar: Por que <code>TLogToFile</code> não **herda** de <code>TLogInMemory</code> para reaproveitar
o código que trabalha com a instância de *FInfo*?

Eu poderia, mas não devo. Falarei sobre herança em próximos posts. Por enquanto vamos deixar as coisas simples 
e prosseguir com o exemplo.

Falta a Classe de *Log* que envia e-mails, sem esquecer de Injetar a Dependência de <code>Mail</code> no *construtor*:
    type
      TLogToMail = class(TInterfacedObject, ILog)
      private
        FOrigin: ILog; 
        FMail: IMail;  
      public
        constructor Create(Origin: ILog; Mail: IMail);
        //class function New...
        function Add(const S: string): ILog;
        function Text: string;
        function Save: ILog;
      end;
      
    implementation

    constructor TLogToMail.Create(Origin: ILog; 
      Mail: IMail);
    begin
      inherited Create;
      FOrigin := Origin; 
      FMail := Mail;
    end;

    function TLogToMail.Add(const S: string): ILog;
    begin
      Result := Self;
      FOrigin.Add(S);
    end;

    function TLogToMail.Text: string;
    begin
      Result := FOrigin.Text;
    end;

    function TLogToMail.Save: ILog;
    begin
      Result := Self;
      FMail.Send(From, Self.Text);
      FOrigin.Save;
    end;

Cada Classe tem apenas uma única responsabilidade e todas as Classes seguem o [contrato]({% post_url 2016-01-18-interfaces-em-todo-lugar %}#interfaces-sao-contratos)
de uma única Interface, simples e coesa.

E agora, e o mais legal! Podemos ter um único Objeto que implementa tudo, mas o código que irá utilizar esse Objeto
ignora completamente sua implementação, ou seja, o código não irá chamar um procedimento específico para salvar a informação em determinado
lugar, como na Implementação Procedural.

Não entendeu? Vamos lá.

Por exemplo. Se os requisitos mudarem e seu cliente pedir para gravar o *Log* num Arquivo Texto e também no Banco de Dados, o que seria preciso fazer?

Bem, na implementação Procedural do primeiro exemplo, o programador teria que <ins>criar um novo método</ins> porque ele não consegue
reutilizar os métodos existentes para fazer algo que seria simples!

Por que ele não consegue? Repare que em cada método <code>SaveXxx</code> há uma chamada de <code>FInfo.Clear</code>. Então ele não pode executar
os métodos assim:

    begin
      Log.SaveToFile('/path/file.log');
      Log.SaveToDB(DB);
    end;

Na primeira chamada a informação do *Log* será perdida.

Sei que você pode pensar em inúmeras outras implementações para que isso não aconteça, mas o **fato** é que você precisa
alterar a Classe para fazer algo que seria simples. Talvez você tenha lido o código da primeira vez e não viu nada de errado. Isso acontece
todo tempo com Classes complexas e muito mais com aquelas que utilizam herança.

Mas, continuando.

Então vou escolher uma das possíveis implementações: Adicionar um novo método.

    function TLog.SaveToFileAndDB(const FileName: string; 
      DB: IDatabase): TLog;
    begin
      Result := Self;
      FInfo.SaveToFile(FileName);
      SaveToDB(DB);
    end;

UAU! HORRÍVEL!

Está claro que essa não é uma solução elegante...

Esqueçam isso e vamos para a implementação **correta**, Orientada a Objetos, **simples** e **reutilizável**.

Para um novo requisito você não precisa alterar um código que já estava funcionando. Não precisa quebrar nada, nem sobrescrever nada.
Basta criar um <ins>novo Objeto</ins> que encapsule os Objetos que já sabem fazer o serviço.

Com apenas uma única Interface e várias implementações, é possível criar um único Objeto que contenha todo tipo de comportamento
referente a *Log*.

    begin
      TLogToMail.New(
        TLogToDB.New(
          TLogToFile.New(
            TLogInMemory.New('First info'),
            '/path/file.log'
          ),
          DB
        ),
        Mail
      )
      .Add('Second info')
      .Add('Final info')
      .Save
    end;

Para o código-geral não há diferença se o *Log* está sendo gravado num Arquivo, Banco de Dados ou sendo enviado por e-mail.

Só há uma Interface *ILog* com apenas alguns poucos métodos. Cada Classe tem sua própria implementação, simples e limpa.

No fim temos uma única instância de um Objeto mais Complexo que é capaz de fazer todo o tipo de *Log*, mas sua Interface
não cresce junto com os requisitos. 
 
Até logo.