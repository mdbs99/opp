---
layout: post
title: Objetos Validadores
date: 2017-07-03
permalink: /:title
description:
  Veja como fazer validações utilizando Objetos ao invés de utilizar programação procedural.
image: /images/2017/photo-scott-webb-59043.jpg
categories:
  - OO
tags:
  - validação
keywords:
  - freepascal
  - fpc
  - delphi
  - lazarus
  - pascal
  - object-pascal
  - object-oriented
  - oop
  - mdbs99
  - validation
  - validação
  - teste
  - testing
  - validando
---

Veja como fazer validações utilizando Objetos ao invés de utilizar programação procedural.

<!--more-->

![Unsplash image]({{ page.image }})

## Introdução {#introducao}

Na Orientação a Objetos a codificação deve ser declarativa. Isso quer dizer que, num mundo ideal, iríamos criar os Objetos agrupando-os entre si e, com uma única [mensagem]({% post_url 2016-11-14-diga-me-algo-sobre-voce %}#mensagens), o trabalho a ser realizado seria iniciado e cada Objeto iria realizar parte desse trabalho. Tudo em perfeita harmonia.

No entanto, não vivemos num mundo ideal e problemas podem ocorrer.

Se pudermos validar o *input* dos dados no nosso sistema antes de iniciar um processo mais elaborado, isso tornaria o processamento menos custoso, menos demorado e menos propenso a erros.

No entanto, se devemos codificar de forma declarativa, ou seja, sem condicionais que validem passo-a-passo o que está sendo processado — o *modus operandis* da programação procedural — como seria possível fazer isso utilizando Objetos para deixar o código mais seguro e fazer um tratamento mais adequado para cada problema ou decisão, antes que uma exceção possa ocorrer?

## Validações {#validacoes}

Imagine um Formulário onde há diversos campos para o usuário preencher antes de clicar em algum botão que irá fazer algo com os dados preenchidos nos *widgets*.

Sempre temos que validar o *input* antes de processá-lo, certo?

Tenho trabalhado com desenvolvimento de *software* a muitos anos. Grande parte desse tempo eu codifiquei a validação de campos utilizando o mesmo "padrão" que até hoje é utilizado, independentemente da linguagem utilizada.

Vejamos como é esse padrão:

<script src="https://gist.github.com/mdbs99/1d990d474d0a15a7543c54c5c02b370a.js"></script>

O exemplo acima é sobre um Formulário que contém alguns campos, dentre esses campos temos `Name` e `Birthday`. O primeiro é *string* e não pode estar em branco. Já o segundo deveria ser uma data válida, então o código utiliza a função padrão `SysUtils.TryStrToDate` que verifica se é uma data válida e retorna o valor na variável `MyDate`.

Quem nunca fez isso?

Pois é.

Há problemas demais com essa abordagem:

1. Não podemos reutilizar as validações. Em cada Formulário haverá uma possível cópia do mesmo código;
2. As variáveis locais podem aumentar consideravelmente caso haja mais testes que necessitem de variáveis;
3. O código é totalmente procedural;
4. Não posso utilizar as informações de aviso ao usuário em outra aplicação Web, por exemplo, visto que as *strings* estão codificadas dentro de funções `ShowMessage` (ou qualquer outra função de mensagem para Desktop);
5. O Formulário ficou complexo, visto que há muito código num único evento — e não adianta apenas criar vários métodos privados para cada teste, pois o Formulário irá continuar fazendo coisas demais.

Há variantes dessa abordagem acima, porém acredito que todos nós já vimos algo assim ou mesmo estamos codificando dessa maneira ainda hoje.

O que podemos fazer para simplificar o código, obter a reutilização das validações e ainda codificar utilizando Orientação a Objetos?

## Constraints {#constraints}

*Constraints* são Objetos que tem por função a validação de algum dado ou mesmo a validação de outro Objeto.

Cada *constraint* valida apenas 1 artefato. Assim podemos reutilizar a validação em muitos outros lugares.

Vamos definir algumas [Interfaces]({% post_url 2016-01-18-interfaces-em-todo-lugar %}):

<script src="https://gist.github.com/mdbs99/c668747123d651298e9f40d0e10af5b4.js"></script>

Vamos entender cada Interface:

1. `[guids]`: São necessários para *casting* de Interfaces. 
2. `IDataInformation`: Representa uma infomação.
3. `IDataInformations`: Representa uma lista de informações.
4. `IDataResult`: Representa um resultado de uma restrição.
5. `IDataConstraint`: Representa uma restrição.
6. `IDataConstraints`: Representa uma lista de restrições.

Não estou utilizando *Generics*. Apenas Classes que podem ser reescritas em praticamente qualquer versão do Lazarus ou Delphi.

Bem simples.

Agora veremos a implementação das Interfaces — por questões de breviedade, vou apresentar somente as assinaturas das Classes:

<script src="https://gist.github.com/mdbs99/b254ff882ce27ce9fa4e8219c63e3e96.js"></script>
          
Essas são Classes utilizadas em projetos reais. <del>Em breve</del> todo o código <del>estará</del> já está disponível no [Projeto James](https://github.com/mdbs99/james). Você poderá obter esse código acompanhando a [Issue #17](https://github.com/mdbs99/james/issues/17) do mesmo projeto.

Na implementação acima não tem nenhuma Classe que implemente a Interface `IDataConstraint`. O motivo disso é que você, programador, irá criar suas próprias *constraints*.

Vejamos um exemplo de como reescrever o código procedural do primeiro exemplo.

Precisamos criar duas Classes que implementam `IDataConstraint`. 

Como só há apenas 1 método nessa Interface, e para não deixar esse artigo ainda maior, vou mostrar o código de apenas uma implementação:

<script src="https://gist.github.com/mdbs99/62040307d41fbc45c0f15605acc541b5.js"></script>

O código acima mostra como seria a implementação para a *constraint* `TNameConstraint`.

O código está *procedural* e ainda pode *melhorar* muito. As variáveis locais poderiam ser retiradas, bastando adicionar mais um *overload* do método `New` na Classe `TDataResult` — você consegue ver essa possibilidade? Conseguiria implementá-la?

Abaixo o código de como utilizar todos esses Objetos em conjunto:

<script src="https://gist.github.com/mdbs99/5dc64c57916d86d01de561077d831aaf.js"></script>

Se nas validações acima o nome estivesse *em branco* mas a data de aniverário tivesse sido digitada *corretamente* — o resultado do método `OK` será verdadeiro se apenas todas as validações passarem no teste — o resultado do `ShowMessage` poderia ser:

    - Name: Name is empty
    - Birthday: OK
    
Essa seria apenas uma versão da implementação de como mostrar as informações. Poderia haver muitos outros *decoradores* para mostrar a informação em outros formatos como, por exemplo, HTML numa aplicação Web.

## Conclusão {#conclusao}

O código não está completo, mas acredito que posso ter aberto sua mente a novas possibilidades quando se trata de validações.

O resultado final de `SaveButtonClick`, mesmo utilizando a Classe `TDataConstraints`, também não foi implementada complemente seguindo o paradigma da Orientação a Objetos — para deixar o código mais sucinto — pois tem um `IF` lá que não deixa o código tão elegante quanto deveria, mas eu acho que dá para você visualizar as possibilidades de uso.

A instância de `TDataConstraints` e seus itens poderia ser utilizada em muitos outros lugares do código.

A combinação de tais Objetos é virtualmente infinita e seu será o mesmo em *todo* código.

O código é *reutilizável* em qualquer tipo de aplicação.

Nenhuma informação ou mensagem ao usuário seria duplicada no código. Haverá apenas um *único* lugar, uma única Classe, para fazer a manutenção de cada validação.

E a exibição da mensagem poderia ser em qualquer formato, bastando utilizar outros Objetos *decoradores* para ler as *informations*, formatando como quiser. 

Até logo.
