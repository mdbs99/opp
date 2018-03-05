---
layout: post
title: "Assertions é uma boa prática"
date: 2018-03-05
permalink: /:title
description:
  Assertions auxiliam o programador no desenvolvimento e depuração do código, sendo a primeira linha de defesa contra bugs.
image: /images/2018/photo-andrew-ruiz-394426-unsplash.jpg
tags:
  - Object Pascal
keywords:
  - freepascal
  - fpc
  - delphi
  - lazarus
  - pascal
  - object pascal
  - object-oriented
  - oop
  - mdbs99
  - assertion
---

Assertions auxiliam o programador no desenvolvimento e depuração do código, sendo a primeira linha de defesa contra bugs.

<!--more-->

![Unsplash image]({{ page.image }})
<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Andrew Ruiz on Unsplash</span>

Quando estamos desenvolvendo, é comum executar o programa várias vezes, digitar alguma entrada de dados "falsa", e seguir adiante, afim de testar outras funcionalidades.

Assim, vamos construindo um protótipo.

Um protótipo pode ser codificado muito rapidamente, sendo comum codificarmos várias funcionalidades apenas para visualizarmos como seria o sistema real.

Como um protótipo pode ser alterado inúmeras vezes, vários bugs podem aparecer.

Aqui entram as [Assertions](https://www.freepascal.org/docs-html/3.0.2/rtl/system/assert.html).

De acordo com a [Wikipedia](https://en.wikipedia.org/wiki/Assertion_(software_development)):

> In computer programming, an assertion is a statement that a predicate (Boolean-valued function, i.e. a true–false expression) is expected to always be true at that point in the code. If an assertion evaluates to false at run time, an assertion failure results, which typically causes the program to crash, or to throw an assertion exception.

Então, uma Assertion sempre deve ser verdadeira ou uma exceção <code>EAssertionFailed</code> será gerada e o sistema será abortado, retornando o código <code>227</code>

No entanto, se a unit <code>SysUtils</code> for declarada em algum lugar do código, é possível verificar mais informações da exceção como a linha e a unit onde o erro foi gerado, além da <i>mensagem</i> opcional que o desenvolvedor pode utilizar para cada Assertion.

Uma Assertion funciona dessa forma:

    procedure TForm1.Button1Click(Sender: TObject);
    var
      I: Integer;
    begin
      I := 0;
      Assert(I = 1, 'The I variable is not 1');
      ShowMessage(I.ToString);
    end;

No código acima, temos um <code>Form</code> com um botão. No evento desse botão o Assert irá verificar se a variável <code>I</code> é igual a <code>1</code>.

O resultado será falso e uma exceção será gerada.

Na minha IDE eu vejo essa mensagem:

    The I variable is not 1 (unit1.pas, line 34).

Eu compilei o projeto <i>sem</i> informações de <i>debugger</i>. Mesmo assim, é possível ver a linha onde a validação ocorreu, tornando muito útil o uso de Assert para identificar onde e por quê um erro ocorreu.

Entretanto, as Assertions não deve ser utilizadas em produção. Suas informações técnicas só deveriam ser vistas por programadores, não usuários finais.

Por esse motivo nós <b>devemos</b> desabilitar as Assertions quando fizermos o <i>deploy</i> do sistema para o usuário final.

Para desabilitar Assertions, o compilador tem uma diretiva de compilação global.

Há duas opções:

    $ASSERTIONS ON/OFF (long form)
    $C +/- (short form)

Mas isso pode ser definido diretamente na IDE, na sessão de debugger nas  opções do projeto, sem haver necessidade de escrever uma linha de código.

Quando as Assertions estão desabilitadas, as verificações não são executadas. Isso libera o sistema de todo <i>overhead</i> das verificações no produto final.

Finalmente, Assertions não substituem Testes de Unidade ou Teste de Integração, sendo apenas mais um tipo de ferramenta para testar o código na fase de desenvolvimento.

Até logo.
