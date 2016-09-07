---
layout: post
title: "Interfaces e a Referência Circular entre Objetos"
date: 2016-02-01
description: Interfaces e a Referência Circular entre Objetos
summary: Um Objeto A aponta para um Objeto B e este aponta para o Objeto A... e nunca serão desalocados da memória automaticamente pelo compilador.
image: /images/photo-1447600514716-ca6f3974c346.jpg
categories: 
  - Pascal
tags:
  - object pascal
  - interfaces
  - compilador
keywords:
  - interfaces
  - referencia circular
  - circular reference
  - weak reference
  - compiler
  - compilador
---

Um Autor escreveu um Livro. Um Livro tem um Autor... que escreveu um Livro, que tem um Autor...

Um **Objeto A** aponta para um **Objeto B** e este aponta para o **Objeto A**. Esses Objetos,
baseados em *Interfaces* com contagem de referência, nunca serão desalocados da memória automaticamente pelo compilador.

O nome desse problema é denominado **Referência Circular** e ele não ocorre apenas no *Object Pascal*.

<!--more-->

![Circular Reference]({{ page.image }})

##Introdução

Sabemos que o *Object Pascal* não possui *Garbage Collector*, mas temos *Interfaces* com contagem de referência
que diz ao compilador quando desalocar um objeto da memória quando não há mais referências para ele.

Esse método de gerenciamento de memória é simples e eficiente, porém não é perfeito.

Funciona assim. Toda vez que uma variável aponta para uma instância, o contador de referência é incrementado.
Toda vez que uma variável sai do escopo ou lhe é atribuída *nil*, o contador de referência é decrementado.

Mas quando Objetos apontam um para o outro (associação bidirecional) como no exemplo entre os Objetos A e B no início desse post, nenhuma 
variável sai do escopo porque os Objetos "seguram uns aos outros" na memória, pois ambos estão numa Referência Circular.

<blockquote>
  <p>
    ...bidirectional associations are problematic in a design.
  </p>
  <footer><cite title="Eric Evans">— Domain-Driven Design Tackling Complexity in the Heart of Software by Eric Evans</cite></footer>
</blockquote>

Eu diria para **evitar** a associação bidirecional sempre que puder. Mas sabemos que em muitos casos ela é necessária.

Nesses casos a solução para a **Referência Circular** é usar **Referência Fraca**.

##Referência Fraca

<blockquote>
  <p>
    In computer programming, a weak reference is a reference that does not protect the referenced object from collection by 
    a garbage collector, unlike a strong reference. An object referenced only by weak references – meaning "every chain of 
    references that reaches the object includes at least one weak reference as a link" – is considered weakly reachable, and 
    can be treated as unreachable and so may be collected at any time. Some garbage-collected languages feature or support 
    various levels of weak references, such as Java, C#, Python, Perl and Lisp.
  </p>
  <footer><cite title="Wikipedia">— Wikipedia</cite></footer>
</blockquote>

Em *Object Pascal* se você tem um ponteiro para uma instância, mas não houve incremento na contagem de referência, você tem uma
Referência Fraca.

Aqueles que programam em linguagens que fazem o uso de Virtual Machines quase nunca pensam nesse problema porque acham 
que estão seguros e que a Virtual Machine cuidará de tudo.

##Virtual Machines

Interessante dizer que o problema da Referência Circular também ocorre em Virtual Machines(VM) com *Garbage Collector* mas, 
devido a toda inteligência que e as VM's atuais possuem, elas dão um jeito de não haver vazamentos de memória (será?).

No entanto, mesmo que a VM cuide dos vazamentos de memória, você deveria estar ciente deste problema pois seu sistema poderá acabar
com a memória RAM disponível rapidamente.

E a solução aqui é a mesma. Utilizar Referências Fracas.

A classe [WeakReference de C#](https://msdn.microsoft.com/en-us/library/system.weakreference(v=vs.110).aspx), assim como a 
classe [WeakReference de Java](https://docs.oracle.com/javase/7/docs/api/java/lang/ref/WeakReference.html), foram criadas com esse objetivo.

Essas classes devem ser utilizadas internamente ou por *frameworks*.

Vejam que não há mágica.  

##Object Pascal e a Referência Fraca

Felizmente não temos o *overhead* de uma VM deixando nossos sistemas lentos a cada passada do *Garbage Collector*, mas também 
significa dizer que não temos ajuda para resolver o problema.

A boa notícia é que já existem algumas implementações com possíveis soluções.

A melhor fonte de explicação e resolução para esse problema foi [postado no Blog Synopse](http://blog.synopse.info/post/2012/06/18/Circular-reference-and-zeroing-weak-pointers).

Eles mostram uma implementação própria e também apresentam três outras propostas de outros autores, cada uma com seus prós e contras.

Escolho a solução da Synopse, a solução mais simples na minha opinião:

    procedure SetWeak(aInterfaceField: PIInterface; 
      const aValue: IInterface);
    begin
      PPointer(aInterfaceField)^ := Pointer(aValue);
    end;

Apenas uma *procedure* que é utilizada para atribuição de instância a uma variável.

Funciona perfeitamente mas eu vejo um problema: Quando saber se devo utilizar a Referência Fraca?

Por exemplo. Considere o seguinte:
  
  1. *Book* tem um *Author*;
  2. *Author* tem uma lista de livros;

    var
      Book: IBook;
    begin
      Book := TBook.New(
        'Object Pascal',     // name
        'About Interfaces',  // text
        TAuthor.New('Jeff')   // author
      );
      ShowMessage(Book.Author.Books.Get(0).Name);
    end;

Para que o código acima funcione, no construtor de *TBook* haverá uma chamada para *Author.Books.Add(Self)*, adicionando
a nova instância de *Book* na lista de livros do *Author*.

Então teremos uma Referência Circular.

  1. *Book* -> *Author*
  2. *Author.Books* -> *Book*  
  
O *Author* nunca será liberado enquanto houver *Books* assim como o *Book* nunca será liberado enquanto houver o *Author* com 
sua lista de livros.

Onde deveria estar a Referência Fraca?

A resposta é, depende.

Bem, se um *Book* nunca pudesse ser instânciado diretamente no código pelo programador, mas somente internamente através da lista 
de livros, não haveria problema. Bastava utilizar *SetWeak* dentro do construtor de *Book* para utilizar uma Referência Fraca entre
ele e a lista de livros do *Author*.

Mas eu quero ter a possibilidade de instanciar somente *Book*. Nesse caso a referência para *Author* no construtor de
*Book* não deveria ser fraca porque, se assim fosse, tão logo a instância de *Author* fosse passada no construtor de *Book*, ela
seria desalocada pelo compilador por não haver ninguém referenciando o *Author*. Tendo *Book* uma Referência Fraca para *Author*, não
haveria incremento no contador de referência da *Interface*.

Não existe uma solução única e nenhuma delas será perfeita. A *procedure SetWeak* resolve a Referência Circular mas o código não 
pode depender do programador saber qual o momento certo para utilizar a *SetWeak*. Caso a *procedure* seja utilizada num momento errado
alguns ponteiros poderão ficar órfãos antes do tempo e violações de acesso irão ocorrer.

Esse problema foi explicado no mesmo [post](http://blog.synopse.info/post/2012/06/18/Circular-reference-and-zeroing-weak-pointers):
<blockquote>
  <p>
    But there are still some cases where it is not enough. Under normal circumstances, a class instance should not be deallocated 
    if there are still outstanding references to it. But since weak references don't contribute to an interface reference count, 
    a class instance can be released when there are outstanding weak references to it. Some memory leak or even random access 
    violations could occur. A debugging nightmare...
  </p>
</blockquote>

Complicado?

Então eles implementaram uma outra *procedure*, muito mais complexa, denominada *SetWeakZero*.

Basicamente essa *procedure* irá setar *nil* nas Referências Fracas quando o Objeto referenciado for liberado. Assim o programador
poderá testar se a variável está associada a alguma instância válida.

Mas *nil* ou *NULL* é um **grande problema** para a Orientação a Objetos (lembra?). Então, pra mim, também não é uma solução perfeita.

##Implementando a Referência Fraca utilizando Composição e Imutabilidade

A solução que irei apresentar agora pode parecer loucura num primeiro momento, pois ainda não falei sobre Imutabilidade neste Blog. Se
você não está familiarizado com o termo pode ser difícil de aceitar. Sugiro pesquisar sobre o assunto enquanto não falo sobre Imutabilidade
por aqui.

Mantenha a mente aberta.

Abaixo eu implemento uma solução para a Referência Fraca utilizando uma abordagem mais
Orientada a Objetos sem que o programador tenha que se preocupar quando deverá ou não utilizar a Referência Fraca. Em todo
o código as mesmas classes com os mesmos argumentos podem ser utilizados sem nenhuma preocupação.

O código implementa o exemplo do início do post, sobre *Author*, *Book* e *Books*.

    unit Unit1;

    {$mode objfpc}{$H+}

    interface

    uses
      Classes, SysUtils, Forms, Controls, StdCtrls, Dialogs;

    type
      IAuthor = interface;
      IBook = interface;
      IBooks = interface;

      IAuthor = interface
      ['{5F7AC8EF-4C81-4F88-8915-8319B506291F}']
        function Name: string;
        function Books: IBooks;
      end;

      IBook = interface
      ['{02582B1D-F608-4800-9702-2E5B75CD1264}']
        function Name: string;
        function Author: IAuthor;
        function Write(const Text: string): IBook;
        function Text: string;
      end;

      IBooks = interface
      ['{55E4D43B-8DC8-4A01-AA90-97F08A6F3F1A}']
        function Add(Book: IBook): IBooks;
        function Get(Index: Integer): IBook;
        function Count: Integer;
      end;

    { Classes }

      TAuthor = class(TInterfacedObject, IAuthor)
      private
        FName: string;
        FBooks: IBooks;
      public
        constructor Create(const Name: string);
        class function New(const Name: string): IAuthor;
        function Name: string;
        function Books: IBooks;
      end;

      TBook = class(TInterfacedObject, IBook)
      private
        FName: string;
        FText: string;
        FAuthor: IAuthor;
      public
        constructor Create(const Name: string; 
          const Text: string; Author: IAuthor);
        class function New(const Name: string; 
          const Text: string; Author: IAuthor): IBook; overload;
        class function New(const Name: string; 
          const Text: string): IBook; overload;
        function Name: string;
        function Author: IAuthor;
        function Write(const Text: string): IBook;
        function Text: string;
      end;

      TBooks = class(TInterfacedObject, IBooks)
      private
        FList: TInterfaceList;
        FAuthor: IAuthor;
      public
        constructor Create(Author: IAuthor);
        class function New(Author: IAuthor): IBooks;
        destructor Destroy; override;
        function Add(Book: IBook): IBooks;
        function Get(Index: Integer): IBook;
        function Count: Integer;
      end;

      TForm1 = class(TForm)
        Button1: TButton;
        procedure Button1Click(Sender: TObject);
      end;

    var
      Form1: TForm1;

    implementation

    {$R *.lfm}

    procedure SetWeak(aInterfaceField: PInterface; 
      const aValue: IInterface);
    begin
      PPointer(aInterfaceField)^ := Pointer(aValue);
    end;

    { TAuthor }

    constructor TAuthor.Create(const Name: string);
    begin
      inherited Create;
      FName := Name;
      FBooks := TBooks.New(Self);
    end;

    class function TAuthor.New(const Name: string): IAuthor;
    begin
      Result := TAuthor.Create(Name);
    end;

    function TAuthor.Name: string;
    begin
      Result := FName;
    end;

    function TAuthor.Books: IBooks;
    begin
      Result := FBooks;
    end;

    { TBook }

    constructor TBook.Create(const Name: string; 
      const Text: string; Author: IAuthor);
    begin
      inherited Create;
      FName := Name;
      FText := Text;
      if Assigned(Author) then
        FAuthor := TAuthor.New(Author.Name)
      else
        FAuthor := TAuthor.New('Undefined');
    end;

    class function TBook.New(const Name: string; 
      const Text: string; Author: IAuthor): IBook;
    begin
      Result := TBook.Create(Name, Text, Author);
    end;

    class function TBook.New(const Name: string; 
      const Text: string): IBook;
    begin
      Result := New(Name, Text, nil);
    end;

    function TBook.Name: string;
    begin
      Result := FName;
    end;

    function TBook.Author: IAuthor;
    begin
      Result := FAuthor;
    end;

    function TBook.Write(const Text: string): IBook;
    begin
      Result := Self;
      FText += Text + #13;
    end;

    function TBook.Text: string;
    begin
      Result := FText;
    end;

    { TBooks }

    constructor TBooks.Create(Author: IAuthor);
    begin
      inherited Create;
      FList := TInterfaceList.Create;
      SetWeak(@FAuthor, Author);
    end;

    class function TBooks.New(Author: IAuthor): IBooks;
    begin
      Result := TBooks.Create(Author);
    end;

    destructor TBooks.Destroy;
    begin
      FList.Free;
      inherited Destroy;
    end;

    function TBooks.Add(Book: IBook): IBooks;
    begin
      Result := Self;
      FList.Add(
        TBook.New(
          Book.Name,
          Book.Text,
          TAuthor.New(FAuthor.Name)
        )
      );
    end;

    function TBooks.Get(Index: Integer): IBook;
    begin
      Result := FList.Items[Index] as IBook;
    end;

    function TBooks.Count: Integer;
    begin
      Result := FList.Count;
    end;

    { TForm1 }

    procedure TForm1.Button1Click(Sender: TObject);
    begin
      ShowMessage(
        TBook.New(
          'Object Pascal',
          'Strong Reference',
          TAuthor.New('Jeff')
        )
        .Author.Name
      );

      ShowMessage(
        TAuthor.New('Marcos')
          .Books.Add(
            TBook.New(
              'Object Pascal',
              'Weak Reference'
            )
          )
          .Get(0)
          .Author.Name
      );
    end;

    end.

A *procedure SetWeak* é utilizada somente uma vez, na chamada do construtor de *Books*. O motivo é que o *Author* cria, internamente, 
uma instância de *Books* e esta mantém a referência do seu criador, o *Author*.

No código de execução *TForm1.Button1Click* há dois exemplos:

  1. Um *Book* é criado já com um *Author*, que é acessado através da *Interface* de *Book*;
  2. Um *Author* é criado, um *Book* é adicionado e depois acessado através de *Books*;
  
Não há vazamentos de memória nem violações de acessos. Mas se você prestar atenção no código verá que Objetos de *Author* e *Book* são 
recriados em alguns momentos.
Isso quer dizer que os "filhos" não mantém a referência real para os "pais", mas sim para *clones* deles.

Essa abordagem não funcionaria com Objetos Mutáveis, pois se você alterasse uma propriedade do "pai" a referência no "filho" iria
estar apontando para o Objeto antigo, com valores de propriedades diferentes antes das alterações.

No entanto, se utilizarmos **Objetos Imutáveis**, não teríamos esse problema pois suas propriedades nunca irão mudar após eles 
terem sido criados. Poderíamos utilizar *clones* de *clones* de *clones* e sempre iríamos ver as mesmas propriedades.

A classe *Books* é mutável por motivo de simplicidade, mas isso não afeta o exemplo.

Sim, haverá um *overhead* devido a recriação dos Objetos porém o benefício será muito maior.

Basta mantermos nossas classes tão pequenas quanto possível; não fazer nenhum processamento nos construtores a não ser atribuições;
implementar o princípio da responsabilidade única; e você verá que esse *overhead* praticamente será imperceptível.

##Conclusão

Infelizmente não podemos codificar apenas [Pensando em Objetos]({% post_url 2016-01-03-pensando-em-objetos %}), pois sempre teremos
problemas técnicos à resolver.

Nenhuma linguagem ou plataforma serão perfeitos. Mas podemos evitar ou contornar esses problemas.

Evite a Referência Circular e fique longe dos problemas relacionados a alocação e desalocação de memória pelo compilador.
Só utilize-a se for absolutamente necessário e tenha certeza de ter codificado **Testes Automatizados** para 
todas as formas de contruir esses Objetos. Só assim você terá plena certeza de que nenhum vazamento de memória foi introduzido 
quando fizer manutenção no código.

Até logo.

