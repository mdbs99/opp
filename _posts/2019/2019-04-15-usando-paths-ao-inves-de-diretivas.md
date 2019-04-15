---
layout: post
title: "Usando Paths ao invés de Diretivas de Compilação"
date: 2019-04-15
permalink: /:title
description:
  Já pensou em utilizar paths, ao invés de diretivas de compilação, para compilar seu projeto em diferentes plataformas?
image: /images/2019/claudio-hirschberger-1326178-unsplash.jpg
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
  - paths
  - diretiva de compilação
  - directive
---

Já pensou em utilizar paths, ao invés de diretivas de compilação, para compilar seu projeto em diferentes plataformas?

<!--more-->

![Unsplash image]({{ page.image }})
<br><span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Nathan Dumlao on Unsplash</span>

Em meados de 2017 eu escrevi um [artigo]({% post_url 2017-07-24-diretivas-de-compilacao %}) sobre Diretivas de Compilação, explicando como poderíamos encapsulá-las para ajudar a tornar o código *multiplatform*. 

Basicamente teríamos uma unidade comum a todas as plataformas (FPC e Delphi) e outra(s) unidade(s) específica(s) para cada uma delas.

Por exemplo: uma unidade `FooFuncs` seria a principal enquanto `FooFuncsFPC` e `FooFuncsDelphi` seriam as especializadas.

A unidade `FooFuncs` utilizaria as unidades especializadas e seus identificadores—interfaces e classes—seriam [redefinidos]({% post_url 2017-08-21-redeclarando-classes %}) utilizando uma nomenclatura mais genérica para toda a lib ou projeto e sem dependências com qualquer plataforma.

Após quase 2 anos, eu mudei esse conceito para um modelo que considero ainda mais simples e sem utilizar diretivas de compilação: utilização de apenas *paths* para cada plataforma.

<center><span style="font-size: 1.0em;">* * *</span></center>

Veja o projeto [Xavier lib](https://github.com/mdbs99/xavier), por exemplo.

Dentro do diretório `xavier/src/` podemos ver dois subdiretórios: `fpc` e `delphi`.

Cada diretório possui 1 unidade com o mesmo nome: `XavierCorePlatform`.

Sabemos que um projeto não pode ter unidades duplicadas, então como isso funciona?

Basta aplicar o path por plataforma, ou seja, apenas 1 unidade será utilizada por projeto. Se você está utilizando o Delphi, irá utilizar apenas o path `src\delphi`, por exemplo.

E é só isso.

As *vantagens* são visíveis em relação a abordagem anterior que utilizava diretivas:

- sem necessidade de diretivas de compilação
- não é necessário redeclarar identificadores de unidades especializadas
- não é necessário haver uma unidade genérica—ex: `FooFuncs`
- basta 1 unidade por plataforma
- menos código

No entanto, você poderia considerar a (possível) [duplicação de código]({% post_url 2017-01-09-codigo-duplicado-talvez-nao %}) como uma desvantagem, visto que haverá implementações de classes, com o mesmo nome, em unidades por plataforma. No projeto Xavier, por exemplo, vemos que há algumas linhas muito similares... mas isso não é um problema real.

A implementação por plataforma pode ser muito diferente uma da outra. Existir algumas linhas similares—ou mesmo iguais—é um preço mínimo a se pagar, na minha humilde opinião.

Finalmente, repare que todo código comum entre as plataformas devem estar em outras unidades—no caso do Xavier, tais códigos compartilhados estão em `XavierCore` e `XavierAdapters`, utilizados pelas respectivas `XavierCorePlatform` em cada plataforma.

Até logo.




