---
layout: post
title: "FreeAndNil... Esqueça"
date: 2016-01-25
description: Não utilize FreeAndNil na programação Orientada a Objetos 
summary: Não utilize FreeAndNil na programação Orientada a Objetos.
image: /images/photo-1452942000102-9c4c7aaeac81.jpg
categories: 
  - Pascal
tags:
  - compilador
keywords:
  - freeandnil
---

Se você utiliza o paradigma da Orientação a Objetos corretamente, você não precisa utilizar
*FreeAndNil*... você não precisa nem mesmo utilizar o método *Free*!

<!--more-->

![Antigo pensamento]({{ page.image }})

##Introdução

Já existe muitas controvérsias e discussões na Internet em torno do uso do *FreeAndNil*, se é "certo" ou
"errado" utilizá-lo.

Dizem que o código fica mais seguro, previne vazamentos de memória... será mesmo?

As coisas mudam. Nem sempre o que aprendemos no passado será verdade no futuro.

Por exemplo. Muitos programadores aprenderam Pascal utilizando variáveis globais — a IDE Delphi/Lazarus adiciona 
automaticamente uma variável global na *unit* do *Form* — e alguns mantém esse aprendizado até hoje como uma
verdade, como correto. Outro exemplo do antigo aprendizado é o uso do *FreeAndNil*. Para muitos programadores 
seu uso é incontestável.

Então não quero tornar esse post mais um debate sem fim, mas apenas mostrar porque **eu** não
utilizo *FreeAndNil* em 99.9% dos casos e explicar porque considero errado utilizá-lo — mesmo quando sou 
obrigado a utilizá-lo — mas deixo à você leitor, o veredicto final.

##Programação Imperativa ou Estruturada

A Linguagem Pascal é realmente surpreendente. Até hoje seus desenvolvedores conseguem suportar
vários tipos de paradigmas de desenvolvimento dentro da mesma linguagem.

Infelizmente (meu ponto de vista) o paradigma mais utilizado pelas comunidades Pascal — e também Java, 
C#, PHP... sim, eles também — é o Imperativo ou Estruturado. Para mim ambos são equivalentes e se complementam.
O paradigma Estruturado só melhorou o Imperativo, adicionando estruturas de sequência, decisão e iteração.

Eu me refiro a ambos como Imperativo.

<blockquote>
  <p>
    Na Ciência da Computação, programação imperativa é um paradigma de programação que descreve 
    a computação como ações, enunciados ou comandos que mudam o estado (variáveis) de um programa.
    Muito parecido com o comportamento imperativo das linguagens naturais que expressam ordens, 
    programas imperativos são uma sequência de comandos para o computador executar.
  </p>
  <footer><cite title="Wikipedia">— Wikipedia</cite></footer>
</blockquote>

As diferenças entre o paradigma Imperativo comparado ao paradigma Orientado a Objetos podem ser bem
sutis ou muito explícitos, dependendo do caso.

No paradigma Imperativo o programador diz ao computador para seguir uma sequência de ações passo-a-passo,
enquanto que no paradigma Orientado a Objetos são **os objetos que determinam a sequência de ações**.

![??!](/images/photo-1442458370899-ae20e367c5d8.jpg)

Pode parecer muito estranho afirmar isso, pois mesmo objetos tem códigos sequênciais em seus métodos, mas essa
[diferença sutil de mindset]({% post_url 2016-01-03-pensando-em-objetos %}) pode determinar se uma aplicação
é mais ou menos Orientada a Objetos.

O uso de *FreeAndNil* remete ao código Imperativo. Significa que você tem uma variável global — ou um atributo
num objeto — que está sendo recriada várias vezes. Isso é o que eu penso quando encontro um *FreeAndNil* no código.
E isso é péssimo. Primeiro que instâncias deveriam ser **imutáveis**, pois a mutabilidade é um dos fatores que
trazem complexidade para o código e segundo, se você está reutilizando uma variável, o *design* do seu código
está errado, na maioria dos casos.

Sobre o uso do *FreeAndNil*, Nick Hodges [escreveu](http://www.nickhodges.com/post/Using-FreeAndNil.aspx):

>“When should I use FreeAndNil?” is “never”, or at least “Almost never, and if you must use it, make sure that there is a really, really good reason to do so and that you clearly document that reason”.

A opinião dele é **nunca**, ou quase nunca, utilizar *FreeAndNil* a não ser que tenha uma ótima razão para fazê-lo.
Seu post data de Dezembro de 2011. É um post antigo que fala de um assunto mais antigo ainda... mas que 
continuamos a falar sobre isso em 2016. Triste.

Para quem não sabe, Nick Hodges é programador Delphi de longa data e autor de [Coding in Delphi](http://www.amazon.com/gp/product/1941266037/ref=as_li_ss_il?ie=UTF8&camp=1789&creative=390957&creativeASIN=1941266037&linkCode=as2&tag=nickhodgeshomepa).

##Tentando achar motivos para usar *FreeAndNil*

Vou tentar dar alguns exemplos do uso de *FreeAndNil* para depois mostrar o que eu considero ser
a forma correta, utilizando o paradigma Orientado a Objetos.

Vamos codificar um acesso hipotético ao serviço da Amazon S3 para obter um arquivo e salvá-lo no HD.

###Exemplo 1

    procedure TForm1.SaveButtonClick(Sender: TObject);
    var
      Credentials: TAWSCredentials;
      Client: TAWSClient;
      Rgn: TS3Region;
      Bucket: TS3Bucket;
      Obj: TS3Object;
    begin
      Credentials := TAWSCredentials.Create('access_key', 'secret_key', True);
      Client := TAWSClient.Create(Credentials);
      Rgn := TS3Region.Create(Client);
      Bucket := nil;
      Obj := nil;
      try
        Bucket := Rgn.GetBucket('mybucket');
        Obj := Bucket.GetObject('foo.txt');
        Obj.SaveToFile('./foo.txt');
      finally
        Obj.Free;
        Bucket.Free;
        Rgn.Free;
        Client.Free;
        Credentials.Free;
      end;  
    end;

O programador teve que informar a sequência de ações passo-a-passo para o compilador. Mas o código parece bem "simples", não?

Criamos uma credencial, um client, uma região, inicializamos *Bucket* e *Obj* com `nil` (?!!).

Depois *Rgn* chama *GetBucket* para obter uma nova instância de *TS3Bucket* e este chama *GetObject* para
obtermos uma nova instância de *TS3Object*.

Por que, neste exemplo, **precisamos** inicializar *Bucket* e *Obj* com `nil`? O motivo é porque dentro 
do *try-finally*, pode ocorrer uma *Exception* (acesso a Internet, HTTP, login, etc) nestas linhas
    Bucket := Rgn.GetBucket('mybucket');
    Obj := Bucket.GetObject('foo.txt');
e se ocorrer uma exceção a variável *Bucket* e/ou *Obj* nunca serão inicializadas e no fim ocorrerá uma 
violação de acesso quando o compilador tentar liberar a memória utilizando *.Free*.

Bem, na minha opinião, esse código é muito errado. Tem mais código de infraestrutura para o computador do que código para 
resolver meu requisito, que é salvar o arquivo no meu computador. 

Mas, talvez, o *FreeAndNil* possa nos ajudar com esse problema... não é?

---
**Dica:** Você não deve criar um objeto em A (*Rgn.GetBucket*) e destruí-lo B (*TForm1.SaveButtonClick*).
Isso é errado para linguagens com liberação manual de memória. Sempre crie e libere seus objetos dentro de um mesmo contexto.
Não siga o *design* do exemplo acima, é apenas um exemplo.

---
Continuando...

Escrevemos demais num único método (evento, na verdade). Melhor seria refatorar esse código para a reutilização dos objetos,
para podermos chamar os métodos de *Bucket* ou *Obj* em outros botões ou formulários.

####Refatorando

    type
      TForm1 = class(TForm)
      private
        FCredentials: TAWSCredentials;
        FClient: TAWSClient;
        FRgn: TS3Region;
        FBucket: TS3Bucket; 
        FObj: TS3Object;
        // ...
      end;

    procedure TForm1.FormCreate(Sender: TObject);
    begin
      FCredentials := TAWSCredentials.Create('access_key', 'secret_key', True);
      FClient := TAWSClient.Create(Credentials);
      FRgn := TS3Region.Create(Client);
    end;

    procedure TForm1.FormDestroy(Sender: TObject);
    begin
      FCredentials.Free;
      FClient.Free;
      FRgn.Free;
      FBucket.Free;
      FObj.Free;
    end;

    procedure TForm1.SaveButtonClick(Sender: TObject);
    begin
      try
        FBucket := FRgn.GetBucket('mybucket');
        FObj := FBucket.GetObject('foo.txt');
        FObj.SaveToFile('./foo.txt');
      finally
        FreeAndNil(Obj);
        FreeAndNil(FBucket);
      end;  
    end;

Agora o código esta menor e continua "seguro". Se houver uma exceção, as variáveis *FBucket* e *FObj* ainda
serão liberadas no fim do programa, no envento *FormDestroy*, sem violação de acesso porque o *FreeAndNil*
irá garantir isso.

Agora ficou muito melhor? Eu acho que não:

  1. Apenas movemos o problema para outro lugar;
  2. *Form1* não deveria saber quem é *Bucket*, *Region*, *Credentials*, etc apenas porque queremos reutilizar suas instâncias;
  3. Se *FBucket* e *FObj* são recriados toda hora, qual a vantagem de termos eles como atributos de *Form1*?
  4. *Form1* agora é um objeto inchado porque "sabe demais";
  5. Você ainda precisa se preocupar com a liberação de memória no *FormDestroy* e utilizando *try-finally*.

###Exemplo 2  
  
Se o exemplo anterior foi simples demais, veja este outro [aqui](https://github.com/mdbs99/Greyhound/blob/0524b1f61e9ef267d17544ae1fc9dfa82683d64c/src/ghsql.pas#L1170).

Este código faz parte do [Greyhound](https://github.com/mdbs99/Greyhound/), um projeto OpenSource codificado em FreePascal por mim.

Isso mesmo, estou lhe dizendo que usei *FreeAndNil* em um dos meus projetos.

Eu utilizo *FreeAndNil* 3 vezes em todo o projeto (nesta mesma *unit*, pode procurar). E sim, continuo achando errado. Mas esse
projeto tem uma complexidade muito maior do que o simples exemplo anterior.

####Explicando o uso do *FreeAndNil* no Projeto Greyhound
*FData* é um atributo do tipo *TDataSet*. A instância de *TghSQLTable*, que contem *FData*, não sabe como o *DataSet* é criado.
Ela depende de outro objeto para criar o *DataSet*, que depende dos filtros setados e também
do tipo de *Lib* utilizada para conexão ao SGBD. Então, neste caso, foi **mais fácil** destruir e pedir uma nova instância.
 
Mas esse design <ins>não está correto</ins> assim como eu não estou 100% satisfeito com ele. Motivos esses que me farão
reescrever toda a Lib — na verdade já estou reescrevendo a algum tempo, porém privadamente.
  
##*FreeAndNil* na Programação Orientada a Objetos

É claro que *FreeAndNil* não existe aqui. Nenhuma *procedure* ou *function* deveriam existir.

Apenas Objetos com Estado e Comportamento, interagindo entre si e manipulando dados.

No início do post eu falei que não há necessidade de usar *FreeAndNil* ou mesmo *Free*. É claro que estou falando do uso de 
*Interfaces* em conjunto com a técnica que expliquei [aqui sobre o Método *New*]({% post_url 2016-01-10-interfaces-e-o-metodo-estatico-new %}).

Utilizando *Interfaces* para usufruir do [*"Garbage Collector"*]({% post_url 2016-01-10-interfaces-e-o-metodo-estatico-new %}) do compilador, 
não precisaremos utilizar *Free* e muito menos *FreeAndNil*.

Então como seria a codificação do exemplo acima utilizando Orientação a Objetos?

    procedure TForm1.SaveButtonClick(Sender: TObject);
    begin
      TS3Region.New(
        TAWSClient.New(
          TAWSCredentials.New('access_key', 'secret_key')
        )
      )
      .Buckets
      .Get('mybucket')
      .Objects
      .Get('foo.txt', '/')
      .Stream
      .SaveToFile('./foo.txt');
    end.

Não é a melhor *design*, mas melhorou 100% comparado aos exemplos anteriores.

Não tenho variáveis, nem mutabilidade, nem código de infraestrutura como *try-finally* para desalocação de memória. É
menor, mais simples, mais direto.

Mas ainda haverá duplicação de código toda vez que for utilizar os métodos de um *S3Bucket* ou *S3Object* em outros lugares.
Então como faríamos se quiséssemos reutilizar o código sem ter que especificar a *Credentials* e *Client* todas as vezes?

Uma das opções é codificar sua própria implementação de *IS3Region*, a *interface* que *TS3Region* implementa.
No construtor da nova implementação (ex: *TMyRegion*) você poderá inicializar *Credentials* e *Client*.

####Refatorando
    type
      TMyRegion = class(TInterfacedObject, IS3Region)
      private
        FOrigin: IS3Region;
      public
        constructor Create(Origin: IS3Region);
        class function New: IS3Region;    
        // IS3Region methods...
      end;
      
    constructor TMyRegion.Create(Origin: IS3Region);
    begin
      inherited Create;
      FOrigin := Origin;
    end;

    class function TMyRegion.New: IS3Region;
    begin
      Result := TMyRegion.Create(
        TS3Region.New(
          TAWSClient.New(
            TAWSCredentials.New('access_key', 'secret_key')
          )
        )  
      );
    end;    

    //....
      
    procedure TForm1.SaveButtonClick(Sender: TObject);
    begin
      TMyRegion.New
      .Buckets
      .Get('mybucket')
      .Objects
      .Get('foo.txt', '/')
      .Stream
      .SaveToFile('./foo.txt');
    end.

A instância *FOrigin* será utilizada dentro de cada implementação para cada método de *IS3Region*.
A implementação *TMyRegion* é um *Decorator* implícito. 

O código ficou ainda menor e mais robusto, pois agora qualquer modificação em *Credentials* e/ou *Client* serão
modificados em apenas um lugar, a classe *TMyRegion*.

É claro que o código das *classes* e *interfaces* somados podem ser maiores que a quantidade de código do modelo Imperativo.
Mas isso é irrelevante quando você consegue codificar de forma mais segura e consistente, após implementar as *interfaces*
e *classes*. Se implementá-los bem, você só precisa fazer isso uma vez, enquanto que no modelo Imperativo a qualquer momento
você pode quebrar o código devido a sua mutabilidade e **acoplamento temporal** — assunto para outro post.

##Conclusão

Para dizer se é "certo" ou "errado" utilizar *FreeAndNil* é necessário saber o contexto e o paradigma utilizado para
codificar o sistema.

Sistemas antigos com muito código Imperativo o utilizam. Sem problemas.

Mesmo em sistemas Orientado a Objetos poderemos encontrar *FreeAndNil* devido a alguns motivos bem particulares como, 
por exemplo, utilização de código legado, uso de DLL/SO, uso de *lib* de terceiros ou até mesmo por simplicidade em alguns casos.

O importante, no entanto, é estarmos conscientes do seu uso e do **motivo** de estarmos utilizando-o. Não tome como uma regra 
quando dizem que é melhor utilizar *FreeAndNil* em todos os casos ou que "não faz mal utilizar"... *FreeAndNil* pode não
fazer mal diretamente, mas tem potencial para fazer mal indiretamente quando ajuda a deixar seu código com um péssimo *design*.

Sempre pesquise sobre o real motivo de utilizar qualquer função, classe, estrutura e todo o resto... e tire suas próprias conclusões.

Até logo.
