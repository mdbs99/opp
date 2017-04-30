---
layout: post
title: "Funções Aninhadas"
date: 2016-10-17
description:
  Funções Aninhadas deixam o código melhor organizado, fácil de ler e alterar.
summary: 
  Funções Aninhadas podem e devem ser utilizadas na Orientação a Objetos
image: /images/photo-1476411890462-80309823db3b.jpg
categories: 
  - Object Pascal
tags:
  - language
keywords:
  - funções aninhadas
  - nested functions
  - organização do código
--- 

Se você tem um Método coeso, que trabalha em apenas uma única 
tarefa, mas mesmo assim o código parece complicado, dificultando
o entendimento e a manutenção... já pensou em refatorar o código
utilizando Funções Aninhadas?

<!--more-->

![Unsplash image]({{ page.image }})

[Funções Aninhadas](https://en.wikipedia.org/wiki/Nested_function)
é algo que não existe em todas as linguagens.
A linguagem Pascal tem e acho que devemos aproveitar essa *feature*.

Funções Aninhadas nada mais são do que funções declaradas dentro de
outras funções ou Métodos.

Esse artigo irá mostrar os motivos e vantagens ao utilizarmos Funções 
Aninhadas, assim como algumas regras que devemos seguir ao utilizá-las.

## Motivos {#motivos}

Funções Aninhadas é uma opção bem melhor do que 
[pular linhas]({% post_url 2016-09-19-linhas-em-branco-no-metodo-e-mal-cheiro %})
dentro da implementação de um Método com o intuito de separar blocos
de código.

Cada função já define um bloco, com a vantagem de ser **reutilizável** em
outra parte do Método.

Funções Aninhadas tem um conceito muito próximo da Programação Orientada a 
Objetos (leia esse 
[artigo](http://blog.synopse.info/post/2012/05/20/Recursive-calls-and-private-objects)
).
Um Método que contém Funções Aninhadas é como se fosse uma implementação
de uma *Classe Anônima*. As variáveis locais serão como *atributos* e as 
Funções Aninhadas serão como *Métodos privados*.

Funções Aninhadas facilitam a correta utilização do
[*WITH*]({% post_url 2016-09-12-a-declaracao-with-do-e-do-mal %})
quando a implementação do Método é complexa ou quando o código utiliza
composição com muitos Objetos.
Dividir o código em pequenas funções, diminuirá as chances de haver **conflitos**
entre identificadores que utilizam o *WITH*.

Resumindo: Funções Aninhadas deixam o código melhor organizado, fácil de ler 
e alterar.

## Regras de Uso {#regras}

Qualquer *feature* utilizada de forma indiscriminada poderá ser um
potencial problema no futuro, ao invés de uma solução — o mesmo ocorre com 
o uso indiscriminado do *WITH* por programadores que não sabem exatamente o
que estão fazendo.

Precisamos de **regras** e **disciplina**.

Abaixo algumas regras que você deve levar em conta.

### Regra 1: Não utilize mais do que 3 Funções Aninhadas {#regra1}

É uma regra óbvia.
Se você tem muitas Funções Aninhadas dentro e um único Método, 
é bem provável que ele esteja fazendo **coisas demais**.
Pense na refatoração e decomposição em outros Métodos ou mesmo
na criação de uma nova Classe.

Existem pouquíssimas exceções a essa regra.

### Regra 2: Evite compartilhar as Variáveis Locais {#regra2}

Falei acima que um Método com Funções Aninhadas é como uma Classe
Anônima, contendo Métodos privados e atributos. 
No entanto, sabemos que não são verdadeiras Classes.

É melhor que você isole cada Função Aninhada com suas próprias 
variáveis e argumentos, ou seja, **evite** compartilhar as variáveis
locais do Método com as Funções Aninhadas.

Essa disciplina na codificação irá ajudá-lo na *extração* e 
*refatoração* das Funções Aninhadas, se for o caso, para criar outros 
Métodos com o mínimo de impacto possível.

Ao invés de utilizar a variável local do Método, passe
a referência como argumento da função, mantendo-as **isoladas**.

Existem poucas exceções a esta regra. Por exemplo.
Se todas as Funções Aninhadas trabalham sempre com
os mesmos Objetos (variáveis locais) do Método, é mais fácil 
compartilhar as variáveis — ou refatorar criando uma nova Classe —
do que ficar repassando-as às funções. Utilize o bom senso.

### Regra 3: Apenas 1 Nível de Funções {#regra3}

Não complique. Use *apenas* um "nível" de Funções Aninhadas. Se
você tiver utilizando mais de um nível, é provável que a função
de "nível 2" deveria ser um Método.

*Refatore*. 

Após a refatoração do código, o "nível 2" de Funções Aninhadas
passaria a ser o "nível 1" no novo Método criado.

Sem exceções aqui!

### Regra 4: Menos é Mais {#regra4}

Cada função deve implementar apenas uma **única** responsabilidade.

Essa é uma regra geral para codificar Métodos e Funções.

Sem exceções.

## Exemplos {#exemplos}

Funções Aninhadas podem ser utilizadas em qualquer tipo de Objeto.

Objetos que lidam com XML, por exemplo, onde é necessário trabalhar
com recursividade e validações sempre são bons candidatos. 

Mas meu uso pessoal de Funções Aninhadas é, na maioria das vezes, 
utilizada no código de *Forms*.
O motivo é simples: É natural para o usuário clicar em apenas um
botão e o sistema fazer *inúmeras* tarefas. Para o usuário é
irrelevante se foi preciso 1 ou 20 Objetos para concluir a tarefa.
O usuário acha que apenas uma tarefa foi executada quando, na 
verdade, **inúmeros Objetos** podem ter tido participação para executar
o serviço. 

Então, abaixo temos alguns exemplos do que eu considero um *bom* uso
de Funções Aninhadas.

Observação: Podem haver erros de sintaxe, visto que eu estou escrevendo
o código diretamente no editor do artigo sem compilar.

### Exemplo 1: Pergunte, Faça o Serviço {#exemplo1}

Em alguns formulários temos que questionar o usuário sobre
qual caminho o sistema deve tomar.
Esses questionamentos, com mensagens *strings*, podem 
diminuir a legibilidade do código.

Somado isso com o uso *errado* do *WITH*, o código *não* fica 
elegante. Veja:

    procedure TMainForm.DeleteButtonClick(Sender: TObject);
    begin
      with TUserQuestion.New(
        Format(
          'Você tem %d registros para excluir.' + #13
          'Esse processo pode demorar.' + #13
          'Confirma a execução?', [
            DataSet.RecordCount
          ]
        )
      ) do
      begin
        Show;
        if Confirmed then
        begin
          ExecuteProcessOne;
          ExecuteProcessTwo;
          TUserInformation.New('Processo concluído.').Show;
        end;
      end;
    end;
    
Nesse exemplo eu utilizei Classes de Mensagens ao usuário.
Não precisamos de sua implementação para entender o que está 
ocorrendo aqui. No entanto o código pode parecer um pouco confuso
para alguns programadores "não iniciados" no uso do *WITH*.
Por exemplo. O Método `Show` após o primeiro `begin`, pertence ao
Formulário ou a instância de `TUserQuestion`?

Também temos a mensagem em texto com quebra de linha e parâmetros
concatenados. As vezes essas mensagens são maiores. O código fica 
uma bagunça.

Vamos refatorar esse código utilizando Funções Aninhadas:

    procedure TMainForm.DeleteButtonClick(Sender: TObject);
    
      function Question(Total: Integer): IUserQuestion;
      begin
        Result :=
          TUserQuestion.New(
            Format(
              'Você tem %d registros para excluir.' + #13
              'Esse processo pode demorar.' + #13
              'Confirma a execução?', [
                Total
              ]
            )
          ).Show;
      end;
    
    begin
      if Question(DataSet.RecordCount).Confirmed then
      begin
        ExecuteProcessOne;
        ExecuteProcessTwo;
        TUserInformation.New('Processo concluído.').Show;
      end;
    end;

Mais simples?

Sim, optei por retirar o *WITH*, porque é mais simples dessa forma,
nesse exemplo.
Mas ele poderia ser utilizado caso o programador necessitasse de 
mais alguma informação da instância `IUserQuestion`, criada e retornada
através da função `Question`.

Exemplo. Você pode fazer uma pergunta ao usuário onde ele deve digitar 
um valor. Nesse caso você precisa validar o retorno (`Confirmed`) e 
mostrar o valor digitado (`Value`):

    with TUserInput.New('Digite o valor') do
    begin
      if Confirmed then
        TUserInformation.New(
          'Valor digitado: ' + Value.AsString
        )
        .Show;
    end;

Repare, também, que a função `Question` não tem mais dependência
com o Objeto que vem do Formulário, `DataSet.RecordCount`. A função
agora está **isolada**. Isso quer dizer que se você precisar dessa
função em mais de um lugar, pode extraí-la, criar um novo Método e 
passar o argumento na sua chamada.

### Exemplo 2: Clicou no "Botão Mágico" que Faz tudo {#exemplo2}

Imagine um Formulário que, ao clique de um botão, o sistema:

  1. Faz validações das informações desejadas
  2. Pergunta ao usuário se deve continuar
  3. Salva as informações no banco de dados
  4. Envia um e-mail notificando alguém

Claro que você terá uma Classe para cada uma dessas funções (certo?).
Você não deve implementar tudo isso dentro de um evento num botão.

Por outro lado, a implementação mais **correta** seria separar essas ações
em outras camadas, outras Classes, sem instanciar tais Classes específicas
dentro do Formulário.

Infelizmente nem sempre temos tempo para fazer o 100% correto. Mas que 
tal fazer 80% correto para depois, quando tivermos mais tempo, refatorar
com tranquilidade o código?

Aqui as Funções Aninhadas nos ajudam novamente:

    procedure TMainForm.ConfirmButtonClick(Sender: TObject);
    
      function Checked: Boolean;
      begin
        Result := False;
        //if EmailEdit.Text <> '' then
        // Exit;
        //...
        Result := True;
      end;
      
      function Question: IUserQuestion;
      begin
        Result := 
          TUserQuestion.New('Confirma a operação?').Show;
      end;
      
      procedure Save;
      begin
        // persistência...
      end;
      
      procedure SendEmail;
      begin
        // envia um e-mail...
      end;
    
    begin
      if Checked then
        if Question.Confirmed then
        begin
          Save;
          SendMail;
        end;
    end;

Tenha em mente que isso é apenas um exemplo.

Todas as Funções Aninhadas devem seguir o bom senso e ter 
**poucas linhas**. Caso contrário, *refatore*.

Haverá muitas dependências entre o Formulário e todas essas Classes 
especialistas. Mas nem sempre
precisamos abstrair em camadas. Pode ser um pequeno Formulário de um
pequeno sistema, onde não haveria problemas em ter essas dependências.

Mesmo assim o código está *limpo* e *elegante*, com fácil manutenção.

## Conclusão {#conclusao}

Funções Aninhadas nos ajudam a codificar de forma mais simples e elegante.

É um dos caminhos para "seguir em frente" sem muitas preocupações com 
o **purismo** da Orientação a Objetos — devido a falta de tempo — mas sem deixar
cair a qualidade do código no longo prazo.

Utilize-as com sabedoria.

