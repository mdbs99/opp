---
layout: post
title: "Pensando em Objetos"
date: 2016-01-03
description: 
summary: Pensar em Objetos é uma mudança de paradigma enorme para quem começou a programar em linguagens procedurais como C, ASM ou Pascal.
image: /images/photo-1446511437394-36cdff3ae1b3.jpg
categories: 
  - oo
tags:
  - oop
  - object pascal
keywords:
  - oop
  - object pascal
  - orientação a objetos
---

Como Pensar em Objetos?

Aqui vai uma resposta simples e direta:

> Pensar em Objetos significa **não** implementar uma tarefa na forma de instruções passo-a-passo para o computador.

<!--more-->

![Pensando](/images/photo-1446511437394-36cdff3ae1b3.jpg)

Pensar em Objetos é uma mudança de paradigma enorme para quem começou a programar em 
linguagens procedurais como C, ASM ou Pascal e não conseguiu — ou nunca quis — mudar o *mindset*
para entender como programar Orientado a Objetos.

A tempos aprendemos sobre Herança, Encapsulamento e Polimorfismo, mas eu acho que todos nós — inclusive eu, no passado —
não entendemos a transição do Procedural para o paradigma Orientado a Objetos.

## Programando Orientado a Objetos

Nada melhor do que um exemplo prático para começarmos a entender as diferenças entre programação Procedural e Orientada a Objetos.

Um cliente nos contratou. Um WebService está disponível e precisamos construir um *Client* para consumi-lo.

Eu não quero utilizar um gerador automatizado de código para ler o WSDL e gerar uma classe
— existe essa opção no Delphi e no Lazarus instalando um `package` específico.

Então, hipoteticamente falando, temos um WebService simples com 5 métodos. Cada método tem os mesmos argumentos: usuário e senha.

As configurações do WebService como Usuário, Senha, URL, métodos do WebService que deverão ser executados, horário de execução, etc.
estão num arquivo de configurações. Porque isso? Bem, esse WebService será executado em `batch` automaticamente em horários pré-programados.
E em cada horário alguns métodos serão executados, em outros não.

Como é automatizado, precisamos de um `Log`. Em texto mesmo. Um `Log` que grave a execução, método, horário, usuário/senha, exeções, etc.

O cliente também quer testar o WebService utilizando um Form (GUI) para ver se está tudo funcionando, mostrando na tela o XML recebido
de acordo com o método a ser executado, escolhido numa `ComboBox`.
Quando o Form é utilizado o sistema não precisa criar `Log`, mas precisa mostrar exceções na tela. 
Mas quando em execução em `batch`, ele precisa gravar o `Log` de execução e de exceções.

Vejam que esse *Client* é para o pessoal de Infra então é sem frescura mesmo: `Log` em texto, roda em `batch` e não tem tela bonita.

Para cada requisição, o *Client* precisa:
  
  1. Trazer o XML;
  2. Fazer um *parser* nos dados;
  3. Fazer uma persistência com os dados obtidos;  

Aqui está o código **Procedural** para consumir o WebService:

**Atenção**: Eu digitei a `procedure` diretamente no post, sem teste de compilação, então podem haver erros de sintaxe.

{% highlight pascal %}
procedure TDpvAction.Act(const WebMethodName: string);
var
  Client: TDpvService;
  Mapping: TDpvXMLMappingInTable;
  XML: string;
  XMLIterable: TXMLIterable; 
begin
  // FFileConfig é um atributo da classe
  
  Client := TDpvService.Create;
  Mapping := TDpvXMLMappingInTable.Create;
  XMLIterable := TXMLIterable.Create; 
  try
    // Configura a URL do WebService
    Client.SetURL(FFileConfig.ReadString('WebService', 'URL'));
   
    // Configura com verificação de exceção
    Client.WithCheck(True);

    // Configura para gerar Log
    Client.WithLog(True);

    // Client executa o método
    XML := Client.ExecuteMethod(
      FFileConfig.ReadString('WebService', 'User'),
      FFileConfig.ReadString('WebService', 'Password'),
      WebMethodName
    );

    // Obtém as configurações necessárias
    XMLIterable.SetXMLNodes(FFileConfig.ReadString('XMLNodes', WebMethodName));

    // Configura o XML gerado
    XMLIterable.SetXML(XML);

    // Executa e gera outro XML (sim, reutilizei a mesma variável)  :)
    XML := XMLIterable.Execute;

    // Executa a persistência
    Mapping.SetXML(XML);
    Mapping.Execute;
  finally
    // Não pode esquecer de desalocar tudo
    Client.Free;
    Mapping.Free;
    XMLIterable.Free;
  end;
end;
{% endhighlight text %}

É um código Procedural e funciona (teoricamente).

Parece fácil de ler, mas o código tem complexidade onde não deveria.
As classes tem funções demais — mesmo separando em classes, continua procedural.

Reparou nas **linhas em branco** para "facilitar a leitura"? Fiz isso propositalmente por 2 motivos:

  1. Queria destacar cada linha para você ter mais contexto do problema;
  2. Ainda não sei como alterar as configurações do `highlight` para deixar o código menos poluído quando eu usar comentários :( 
  2. Todo mundo faz isso para facilitar a leitura... certo? :)

Mas quando vocês programarem Orientado a Objetos, não haverá mais motivos para pular linhas dentro de um método, Ok?  
  
---  
**Dica:** Pular linhas dentro de um método é um **"mal cheiro"** no código.
Pode significar que você está fazendo coisas demais num único método.

---
Nesse tipo de código, a cada manutenção, você se preocupa em quebrar alguma coisa.

Para não quebrar nada você precisa:

  * Chamar os métodos na sequência correta
  * Chamar os `SetXXX` na sequência correta
  * Ter cuidado com as variáveis, suas inicializações e reutilização
  * Instanciar os objetos na sequência correta
  * Etc.

Não tome cuidado e... BUMM! Quebrou o código. Debugg. Checagem das atribuições na sequência exata. Perda de tempo.

Não? Você sabe que é verdade :)

O código apresentado é 100% Procedural, mesmo utilizando classes!
Você poderia usar mais classes e até mesmo Herança entre elas,
mas o código continuaria Procedural. Você não tem objetos, você tem **funções agrupadas**
em blocos (classes) para fazer ações para o **orquestrador**, você!

E como estes mesmos requisitos poderiam ser codificados utilizando Orientação a Objetos,
sem um orquestrador, utilizando "objetos vivos" com **encapsulamento** perfeito, minimizando a quebra de código
em manutenções futuras e com um código muito mais **elegante**?

Aqui está o código **Orientado a Objetos** para consumir o mesmo WebService:

{% highlight objectpascal %}
function TDpvAction.Act(const WebMethodName: string): IActionResult;
begin
  Result := TDpvXMLMappingInTable.New(
    FFileConfig.ReadString('MethodsVsTables', WebMethodName),
    TDpvXMLIterable.New(
      FFileConfig.ReadString('XMLNodes', WebMethodName),
      TDpvServiceWithLogging.New(
        TDpvServiceWithChecking.New(
          TDpvService.New(
            FFileConfig.ReadString('WebService', 'URL')
          )
        )
      )
      .Call(
        TDpvMethod.New(
          WebMethodName,
          TWebAuthorization.New(
            FFileConfig.ReadString('WebService', 'User'),
            FFileConfig.ReadString('WebService', 'Password')
          )
        )
      ).XML
    )
  ).Act;
end;
{% endhighlight text %}

Esse é um código verdadeiramente Orientado a Objetos. Não dizemos ao computador linha-a-linha sobre o que fazer.
Os Objetos sabem o que fazer! Eles não precisam de um **orquestrador** lhes dizendo o que fazer a cada momento.
Crie-os já configurados e deixe-os conversarem entre si. Eles sabem os métodos a chamar e **quando** chamá-los.
Eles podem criar objetos internamente ou não e isso é irrelevante para o código chamador. Eles são como uma
caixa preta, ninguém sabe o que há dentro deles. Você, o programador, só precisa saber o **contrato** que o
objeto deverá cumprir. Esse contrato chamamos de `interface`.

Não dá pra ver no código por motivos de simplificação, mas cada instância é do tipo de uma `interface`, por isso temos os métodos `New` — um padrão
que eu defini para meus projetos para contornar uma `feature` do compilador mas só irei falar a respeito em outros posts.

Ao invés de uma `procedure` agora temos uma `function` que retorna um `IActionResult` e o fato de ter apenas um `Result` é um ótimo sinal de que a função
não faz coisas demais. Essa função é como um grande objeto ou uma composição deles — vou falar mais sobre isso em futuros posts.

Se todas as instâncias são do tipo `interface`, então não preciso desalocar nada manualmente
deixando o código mais `clean` sem try-finally e sem `memleaks` (caso esqueça algum .Free).

Todos os objetos são **imutáveis**, ou seja, após criados não há nenhum `SetXXX` que altere seu **estado interno**.
Então não configuro meu *Client*, e nenhum outro objeto, utilizando métodos `SetXXX` para ele trabalhar com `Log` 
ou com checagem de exceções.

---
**Dica:** Toda classe deveria ser imutável, a não ser que tenha uma ótima razão para não ser.

---
Eu uso `Decorators` ao invés de `properties` ou `SetXXX` para ligar/desligar `flags` (atributos).
Assim não preciso **completar** a(s) classe(s) numa sequência correta de chamadas dos métodos.

Se eu não preciso do `Log`, não utilizo o `decorator` do `Service` de `Log` que chamei de `TDpvServiceWithLogging`;
o mesmo para a checagem de Exceções, que chamei de `TDpvServiceWithChecking`.

O método de chamada no WebService é uma classe separada, que chamei de `TDpvMethod`.
Cada autorização (usuário/senha) também é uma classe, que chamei de `TWebAuthorization`... enfim.

Viu as diferenças?

Espero que você tenha visto as vantagens de programar Orientado a Objetos, mesmo com esse exemplo simples.

Tá gostando? Alguma dúvida? Não concorda? Posta aí nos comentários.

Vejo você no próximo post.

---
**Dica:** Sugiro a leitura do livro Object Thinking de David West. Leia-o com a mente aberta e começará a entender mais sobre o **Pensamento Objeto**.
<a href="http://www.amazon.com/Object-Thinking-Developer-Reference-David/dp/0735619654">![Object Thinking](/images/object-thinking-book.jpg "Object Thinking by David West")</a>

