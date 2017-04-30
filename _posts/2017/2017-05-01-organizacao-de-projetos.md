---
layout: post
title: Organização de Projetos
date: 2017-05-01
permalink: /:title
description:
  Como você organiza a parte física e lógica dos seus Projetos?
image: /images/2017/photo-jeff-hopper-17306.jpg
categories: 
  - Projeto
tags:
  - projeto
keywords:
  - freepascal
  - fpc
  - delphi
  - lazarus
  - c#
  - csharp
  - java
  - object-oriented
  - oop
  - mdbs99
  - project
  - organização
  - organize
  - organization
---

Todo novo Projeto é uma boa oportunidade para melhorarmos nossa habilidade de organização.

Como você organiza a parte física e lógica dos seus Projetos?

<!--more-->

![Unsplash image]({{ page.image }})  

## Introdução {#introducao}

A organização do Projeto é uma etapa muito importante no desenvolvimento do *software*.

Trabalhar num Projeto organizado é muito mais satisfatório e eficaz. 

No entanto, muitos desenvolvedores ainda acham que é perda de tempo organizar e querem logo iniciar a codificação, sem pensar na modularização e nomenclaturas de todas as partes que irão compor o Projeto.

Utilizar uma boa nomenclatura, padronizando os nomes dos pacotes e arquivos é de suma importância para que o desenvolvimento flua de forma eficaz entre a equipe.

Uma boa nomenclatura também ajuda na modularização, como veremos a seguir.

## Nome do Projeto {#projeto}

Todo Projeto é construído para alguma empresa e por isso toda empresa deve ter sua própria árvore de Projetos. Caso seu Projeto seja feito para uma pessoa e não empresa, utilize o nome da pessoa.

Então vem o nome do Projeto, que deve ser curto, de fácil memorização e escrita.

Por quê?

Primeiro, o nome do Projeto irá fazer parte do nome das *units* como um prefixo, evitando a colisão de nomes com outros Projetos ou *Libs*.

Segundo, o nome do Projeto será utilizado por outras ferramentas de análise e acompanhamento das tarefas (tickets) e prazos. Todo nome deve ser de fácil memorização e como uma boa identidade.

Não use nomes indecifráveis com *SisXpto10* ou nomes grandes e sem personalidade como *Sistema de Tickets ABC*.

Nomes de Projetos devem ser curtos, fáceis de lembrar e... *sexys*, por que não?

Tenho Projetos chamados Sophia, [James](https://github.com/mdbs99/james), Odin, [Greyhound](https://github.com/mdbs99/greyhound), Dog, Nicole, e muitos outros.

Tente encontrar um nome que tenha haver com o Projeto. Pode ser por causa da definição do nome em si, o que ele representa, uma sátira, apelido, pela "sonoridade" ou porque é *legal*.

## Diretórios e Arquivos {#diretorios-e-arquivos}

Nos meus Projetos, sejam eles individuais ou em equipe, eles tem basicamente essa estrutura logo abaixo.

### Diretórios {#diretorios}

A começar pela nomenclatura dos diretórios, utilizo a seguinte divisão:

    empresa
      |
      projeto
        |
        bin  : todos os arquivos binários e libs
        |
        doc  : toda a documentação
        |
        pkg  : todos os pacotes
        |
        src  : fontes de produção
        |
        test : fontes de testes
        
Obviamente eu não inventei essa estrutura que é bem simples e lógica. Muitos Projetos *Open Source* já a utilizam a muito tempo.

O nome dos diretórios poderiam ser `/binaries`, `/documentation`, `/packages`, `/source` e `/tests` porém para nomes de diretórios, quanto menor, melhor. Você irá lembrar disso quando tiver que digitar no console, digitar os *paths* na IDE, etc.

Considero uma árvore simples e limpa. Mas nem todos os projetos a utilizam.

Por exemplo. A maioria dos projetos Java são codificados com base na URL da empresa em conjunto com algum padrão estranho.

Exemplo: `empresa/projeto/src/main/java/com/acme/projeto`.

Na minha opinião, esta é uma nomenclatura muito deselegante e repetitiva.

Por quê o nome do projeto se repete nos diretórios? Não sei. Talvez a IDE faça isso e ninguém dá a mínima.

O que o `/com` significa no contexto do Projeto? Nada. Apenas um padrão que seguem para evitar a colisão de nomes, talvez.

E por quê `/java` deve fazer parte da árvore de diretórios?!

Realmente não sei.

### Arquivos {#arquivos}

Já o nome dos arquivos, ou seja, das *units*, devem seguir o seguinte padrão:

`<projeto>.<grupo>.<subgrupo>.<nome>`

Apenas `<projeto>` e `<grupo>` são obrigatórios.

Por quê?

Bem, cada *unit* no *Object Pascal* pode conter mais de uma Classe. Então não é necessário um `<nome>` de Classe, por exemplo, como é o caso do Java. 

Apenas para Formulários e [*DataModules*]({% post_url 2016-02-22-datamodule-e-apenas-um-container %}), o nome será obrigatório.

Vejamos alguns exemplos:

  1. [james.data](https://github.com/mdbs99/james/blob/master/src/james.data.pas) contém `<projeto>` e `<grupo>`;
  2. [james.data.clss](https://github.com/mdbs99/james/blob/master/src/james.data.clss.pas) contém `<projeto>`, `<grupo>` e `<subgrupo>`;
  3. [james.data.stream.clss](https://github.com/mdbs99/james/blob/master/src/james.data.stream.clss.pas) contém `<projeto>`, `<grupo>` e dois `<subgrupo>`;

Existem alguns *sufixos* de arquivos que utilizo bastante:

  1. Clss : conterá classes que implementam interfaces;
  2. Tests : contém as classes de testes;
  3. Form : utilizado para todos os formulários;
  4. Module : utilizado para todos os *DataModules*;
  
Para o nome das Classes, veja [esse artigo]({% post_url 2016-04-25-nomeando-classes %}).
  
## Dividir para Conquistar {#dividir}

Todo projeto de tamanho médio ou grande, deve ser dividido em módulos lógicos, ou seja, subdiretórios dentro de `/src`.

Essa divisão irá depender exclusivamente do seu projeto e contextos.

> "Dividir para conquistar" — Napoleão

Essa divisão em subdiretórios é importante para termos a reutilização dos fontes por outros Projetos e ao mesmo tempo a restrição de visibilidade.

Se os fontes de um Projeto ficasse em apenas um diretório, outros Projetos poderiam ter acesso a tudo. Dividindo em subdiretórios você pode restringir a visibilidade e acesso para esses outros Projetos.

Outra possibilidade bem interessante é dividir os módulos em [Pacotes]({% post_url 2017-04-24-james-e-testes-de-unidade %}#pacote-de-testes) reutilizáveis entre Projetos distintos.
Nesse padrão a divisão dos fontes não é apenas por subdiretórios, mas sim por árvores completamente diferentes.

É normal criarmos Pacotes para componentes e *Libs* reutilizáveis, mas criar Pacotes entre Projetos também pode ser muito eficaz para o desenvolvimento.

No Lazarus, basta abrir um Pacote uma única vez que ele já fica registrado na IDE. Depois basta adicionar o Pacote no(s) Projeto(s) para reutilizar seus fontes. Caso queira mudar o *path* do Pacote, basta fazer num único lugar (XML de configuração da IDE) para que todos os outros Projetos sejam atualizados. Isso é muito melhor do que ter que alterar *paths* de diretório reutilizáveis em cada Projeto.

## Conclusão {#conclusao}

Um Projeto de *software* não é apenas código-fonte. Devemos fazer um estudo desde o nome a ser utilizado, passando pela nomenclatura, diretórios e divisões de cada módulo.

Seu Projeto poderá ter uma vida mais longa se você souber onde encontrar cada peça facilmente.

A divisão por módulos irá facilitar a manutenção no longo prazo e a nomenclatura correta irá deixar tudo mais fácil.

E você, como organiza seus Projetos?

Até logo.