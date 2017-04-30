---
layout: post
title: A declaração WITH-DO é do Mal?
date: 2016-09-12
description: Reflexões sobre o uso do WITH-DO no código Object Pascal.
summary: Reflexões sobre o uso do WITH-DO no código Object Pascal.
image: /images/photo-8bc72ed7.jpg
categories: 
  - Pascal
tags:
  - language
keywords:
  - with-do
  - language
--- 

Muita gente odeia utilizar a construção WITH-DO do Pascal.
Dizem que é difícil de ler o código, introduz *bugs*... 
esqueçam que WITH-DO existe! Eles dizem.

Mas será que WITH-DO é mesmo do mal ou esses programadores é 
que não sabem utilizá-lo no momento certo?

<!--more-->

![Imagem]({{ page.image }})

## Introdução {#introducao}

Se você não sabe o que é WITH-DO, 
[aqui](http://www.freepascal.org/docs-html/ref/refsu63.html)
está uma boa explicação técnica.

Então você define um **bloco** ou **contexto** e pode acessar os 
métodos e atributos de um Objeto sem a referência explícita à uma 
variável.

A primeira vista parece que o objetivo é escrever menos e,
como bem sabemos, a maioria dos programadores são preguiçosos.

Eu utilizo WITH-DO. Muito. E é claro, também quero escrever menos. 

Mas tento escrever menos da maneira correta. Não para poupar *bytes*,
mas para deixar o código **mais legível**, **menos verboso** e 
**menos acoplado**.

Ao longo de muitos anos utilizando WITH-DO eu posso contar 
nos dedos de apenas uma mão a quantidade de problemas que já 
tive ao utilizar essa construção, ou seja, acho que seu uso vale
a pena.

Nesse artigo você aprenderá a utilizar a declaração WITH-DO.

## Imperativo {#imperativo}

Creio que o **mal** que WITH-DO nos traz é devido ao seu uso 100%
[imperativo](https://pt.wikipedia.org/wiki/Programa%C3%A7%C3%A3o_imperativa),
sem nenhum contexto determinando onde começa ou termina seu uso.

Veja [aqui](http://forum.lazarus.freepascal.org/index.php?topic=32889.0) 
um exemplo de mal uso da declaração WITH-DO, postado no fórum 
do Lazarus.

Vou replicar o código abaixo, caso o *link* não esteja mais
disponível:

    try
      btnOK.Enabled:=False;
      with datamodule.qryRegister do begin
        datamodule.connection.AutoCommit:=True;
        datamodule.connection.StartTransaction;
        SQL.Clear;
        SQL.Text:=
          'INSERT INTO table VALUES (:name, :sex, :dateofbirth, :maritalstatus)';
        ParamByName('name').AsString:=teName.Text;
        ParamByName('sex').AsInteger:=cbSex.ItemIndex;
        ParamByName('dateofbirth').AsDate:=StrToDate(teDateOfBirth.Text);
        ParamByName('maritalstatus').AsInteger:=cbMaritalStatus.ItemIndex;          
        SQL.Clear;
        SQL.Text:='SELECT LAST_INSERT_ID() AS iddb FROM table';
        Open;
        idUser:=FieldByName('iddb').AsInteger;
        ExecSQL;
        Close;        
        try
          datamodule.connection.Commit;
          clearFields;
        except
          datamodule.connection.Rollback;
          ShowMessage('Error');
        end;
    end; //with ends statement
    except
      ShowMessage('Error: no connection to the database');
    end;
    bntOK.Enabled:=True;

O que é questionado nesse *post* é irrelevante. 

Apenas analisem o código e vejam como o autor está utilizando
o WITH-DO:

  1. Ele inicia uma transação;
  2. Configura uma cláusula SQL;
  3. Passa parâmetros;
  4. Limpa a *query*(?)
  5. Executa a *query*;
  6. Commit... ClearFields... 
  
É um código totalmente procedural, imperativo e confuso!

É muito difícil saber o que está sendo utilizado pelo 
WITH-DO e o que não está, concorda?

Nesses casos, quando há ambiguidade entre métodos utilizados
pelo WITH-DO que não deveriam ser utilizados, alguns programadores
culpam o WITH-DO quando, na verdade, deveriam culpar a si mesmos
por escrever um código dessa maneira.

Essa é uma maneira imperativa de codificação. Procedural.
Um código altamente acoplado que, realmente, não deveria utilizar
WITH-DO porque só piora as coisas.

## Declarativo {#declarativo}

A codificação declarativa é utilizada nas linguagens funcionais.
Também é utilizada, por exemplo, quando codificamos SQL.

Na codificação declarativa você define **o que fazer**, mas não se
importa **como será feito**.

    SELECT * FROM Clients ORDER BY name;
    
Quando escrevemos o código SQL acima, não nos preocupamos como o
SGBD irá trazer o resultado; quais índices; quais tuplas. Apenas
queremos o resultado o mais rápido possível.

Assim deve ser a codificação 
[Orientada a Objetos]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}),
onde os Objetos
[pensam e tomam as próprias decisões]({% post_url 2016-03-14-objetos-pensam-e-tomam-decisoes %}),
sem que tenha um "controlador" informando **como fazer** linha a linha.

E o que isso tem haver com o uso do WITH-DO?

Bem, o WITH-DO nos ajuda a transformar um código procedural em algo mais
declarativo, mais Orientado a Objetos.

Um código mais Orientado a Objetos é mais **legível**.

Um código com menos declarações de variáveis, é menos **verboso**.

E se você não declara variáveis de tipos específicos, automaticamente
seu código é **menos acoplado**, com menos usos de Units para determinar
os tipos das variáveis.

Vou lhe mostrar como.

Vamos pegar o mesmo código procedural acima e reescrever utilizando
WITH-DO.

Esse código será apenas **um pouco mais** Orientado a Objetos, com
uma melhor legibilidade que o primeiro:

    try
      btnOK.Enabled := False;
      with TDatabase.Instance.StartTransaction do
      try
        with 
          TQuery.New(
            'INSERT INTO table VALUES (' +
            '  :name, :sex, :dateofbirth, :maritalstatus)'
          )
        do
        begin
          Params
            .Add('name', ftString, teName.Text)
            .Add('sex', ftInteger, cbSex.ItemIndex)
            .Add('dateofbirth', ftDateTime, StrToDate(teDateOfBirth.Text))
            .Add('maritalstatus', ftInteger, cbMaritalStatus.ItemIndex)
          Execute;
        end;
        idUser := 
          TQuery.New('SELECT LAST_INSERT_ID() AS iddb FROM table')
            .Open
            .Field('iddb').AsInteger;
        Commit;
      except
        Rollback;
        raise;
      end;
    except
      ShowMessage('Error: no connection to the database');
    end;
    bntOK.Enabled:=True;

**O código continua ruim, porém melhor.**

Há muita coisa acontecendo num único método.

Não deveríamos parar a refatoração agora.
Cada execução deveria estar num método a parte...

No entanto o objetivo é mostrar o uso do WITH-DO e 
acho que dá pra ver a diferença, certo?

Ao todo são apenas 2 declarações de WITH-DO. Cada declaração tem 
um **contexto** bem definido por *TRY-EXCEPT* ou *BEGIN-END*. 
Não há ambiguidade.

A declaração `TDatabase.Instance.StartTransaction` irá retornar um
Objeto do tipo `ITransaction`.

A declaração `TQuery.New` irá retornar um Objeto do tipo `IQuery`, 
através do [Método New]({% post_url 2016-01-10-interfaces-e-o-metodo-estatico-new %}).

O código ficou mais legível.

Não há declarações de variáveis.

Não há variáveis globais como existe no código procedural — 
`datamodule.qryRegister` é um Objeto global que, possivelmente, é 
reutilizado em toda a aplicação, sendo um *design* muito errado, 
não *thread-safe* e gerador de problemas.

## Orientado a Objetos {#orientado-a-objetos}

A declaração WITH-DO nos ajuda a escrever um código mais simples e elegante.
Mas sejamos sinceros. Utilizamos WITH-DO para produzirmos código
procedural. E não tem nada de errado nisso, visto que é quase
impossível codificar 100% Orientado a Objetos em linguagens que
são imperativas.

Uma vez que WITH-DO define uma variável implícita, o programador
pode chamar os **métodos na sequência**, linha a linha.

O uso de WITH-DO deixa o código melhor, mas se dá pra codificar
Orientado a Objetos, é o que devemos fazer.

Então vou reescrever o código.

Não há espaço aqui para a definição das Classes utilizadas
no exemplo.

Qualquer dúvida, basta postar nos comentários.

Abaixo um hipotético código **declarativo** e **Orientado a Objetos**:

    btnOK.Enabled := False;
    try
      idUser :=  
        TDatabase.Instance.Transaction(
          TTasks.New
            .Add(
              TQueryTask.New(
                'INSERT INTO table VALUES (' +
                '  :name, :sex, :dateofbirth, :maritalstatus)',
                TDataParams.New
                  .Add('name', ftString, teName.Text)
                  .Add('sex', ftInteger, cbSex.ItemIndex)
                  .Add('dateofbirth', ftDateTime, teDateOfBirth.Text)
                  .Add('maritalstatus', ftInteger, cbMaritalStatus.ItemIndex)
              )
            )
            .Add(
              TQueryTask.New('SELECT LAST_INSERT_ID() AS iddb FROM table')            
            )
        )
        .Results
        .Get('iddb').AsInteger;
    finally      
      bntOK.Enabled := True;
    end;

O que está ocorrendo aqui?

Bem, temos `TQueryTask` que implementa `ITask`.

Temos uma lista de *tasks*.

A lista é passada para `Transaction` que, internamente, irá executar
cada *task* — chamando o método `Execute` que não está explícito no
código — dentro de uma transação.

Por fim `Transaction` tem um método `Results` que pode ser do tipo
`IDataParams` e através de `Get` é retornado um valor.

O que acontece dentro desses Objetos é de responsabilidade deles.
O programador é apenas o **orquestrador** dessa comunicação.
    
## Conclusão {#conclusao}

Na minha opinião WITH-DO pode e deve ser utilizado.

Muita gente deseja cada vez mais *features* para a linguagem
Object Pascal. Mas será que já sabemos utilizar, de forma eficaz,
tudo o que já temos nessa **elegante linguagem** chamada Object Pascal?

Até onde eu sei, essa *feature* não existe em nenhuma outra linguagem.

Apenas nós temos o WITH-DO!

Mas será que sou o único que usa e aprecia essa *feature*? :)
