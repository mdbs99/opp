---
layout: post
title: "Pensando em Objetos"
date: 2016-01-01
categories: oop
description:
summary: 
tags:
  - oop
  - object pascal
keywords:
---

Como Pensar em Objetos ao invés de pensamento procedural?

Aqui vai uma resposta simples e direta:

> Pensar em Objetos significa **não** implementar uma tarefa na forma de instruções passo-a-passo para o computador.

<!--more-->

![Pensando](/images/photo-thinking.jpg)

Os ambientes Pascal — Delphi, Lazarus e outros — sempre tiveram uma pegada mais RAD.
Nesses ambientes a prioridade sempre foi programar rapidamente. Prototipar rapidamente.
Entregar rapidamente. E isso é ótimo... mas não no longo prazo, especialmente para sistemas
médios ou grandes.

<blockquote>
  <p>
    “Rapid Application Development (RAD) ou Desenvolvimento Rápido de Aplicação (em português), 
    é um modelo de processo de desenvolvimento de software interativo e incremental que enfatiza 
    um ciclo de desenvolvimento extremamente curto (entre 60 e 90 dias).”
  </p>
  <footer><cite title="Wikipedia">— Wikipedia</cite></footer>
</blockquote>

Digo isso porque a maioria dos sistemas codificados em Pascal que eu já vi até hoje — com 
exceção de poucos — não tinham uma boa manutenalibidade.

E porque isso ocorre? Na minha opinião, tem muito haver com o RAD e sua __filosofia__.

Lembram como o pessoal do Delphi — desde a Borland até a Embarcadero — sempre pregaram sua filosofia RAD em todas as conferências?
Eu ia nesses encontros sempre que tinha aqui no Rio de Janeiro — época da Borland! — e sempre tinha um exemplo do tipo:

  1. Novo Form
  2. Nova Query (na época era Table!)
  3. Acrescenta os Fields
  4. Arrasta os Fields para o Form
  
Boom! Um cadastro, com tudo funcionando, sem uma única linha de código.

Ok, isso era só pra mostrar a compatibilidade com versões anteriores... mas não é assim que a maioria programa até hoje? :)

Isso era apenas um cadastro (CRUD), mas onde ficam regras de negócio? Nos __Eventos__, é claro.

  * Se quiser fazer alguma coisa antes de trazer os dados, evento BeforeOpen;
  * Se quiser fazer alguma coisa depois de inserir os dados, evento AfterPost;
  * Para qualquer coisa, evento OnXxxx;
  
Aí você tem que usar essa mesma lógica em outros lugares. O que fazer?

Inventaram o DataModule. Basicamente um container de componentes. 
Transferimos a Query pra lá e pronto, estamos reutilizando a "lógica de negócio" em vários formulários.

Então as regras de negócio ficam nos eventos da Query, no DataModule?

Se você precisa da mesma Query, mas com "regras de negócio" diferentes no(s) evento(s), o que você faz?

  1. Adiciona IF's no Evento?
  2. Copia e cola a mesma Query e codifica outros eventos?
  3. [Deixe um comentário com outras possibilidades] :)

Se você utiliza IF's, opção #1, quer dizer que não sabe o que é **Polimorfismo** e está adicionando **complexidade** ao sistema
porque está "completando" código, introduzindo IF's para situações diferentes em momentos diferentes. 

Usar **Herança** incorretamente também introduz complexidade porque faz o programador **completar código** nas subclasses.

<blockquote>
  <p>
    "Completing things is the source of complexity"
  </p>
  <footer><cite title="Wikipedia"><a href="http://www.infoq.com/presentations/Simple-Made-Easy">— Rich Hickey</a></cite></footer>
</blockquote>

**Dica:** Toda classe que não for `abstrata`, deve ser `sealed` e não permitir nenhuma herança.

Se você escolhe a opção #2, então estará duplicando código, o que é pior. Aumenta a complexidade e o sistema irá quebrar em algum momento,
porque você pode esquecer de mudar/adicionar algum campo/evento em todas as cópias de Querys que utilizam a mesma lógica.

Ah, e o problema não é o DataModule (mais sobre ele em novos posts).

Então, como seria **Pensar em Objetos** para um simples CRUD?

Bem, você precisa separar **tudo** em objetos bem definidos.

Imaginem um CRUD de Clientes. Quando o usuário clicar no botão `Save` haverá a persistência: 

{% highlight objectpascal %}
procedure TClientForm.SaveButtonClick(Sender: TObject);
begin
  TUIMessage.New(
    TSQLCRUD.New(
      Connection, // conexão global
      'client',
      TValidations.New
        .Add(TNameValidated.New(NameEdit, 'Nome'))
        .Add(TCPFValidated.New(CPFEdit, 'CPF'))
        .Add(TAddressValidated.New(AddressEdit, 'Endereço')),
      TSQLColumns.New
        .Add('name', NameEdit.Text)
        .Add('cpf', CPFEdit.Text)
        .Add('address', AddressEdit.Text)
    )
    .Post
    .Text
  ).Show;  
end;
{% endhighlight text %}