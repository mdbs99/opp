---
layout: post
title: Pacotes e Versões no Lazarus
date: 2017-04-03
description:
  Veja como utilizar versões diferentes do mesmo Package no Lazarus.
image: /images/photo-samuel-zeller-118195.jpg
categories: 
  - Lazarus
tags:
  - lazarus
  - fpc
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
  - package
  - packages
  - version
  - versioning
---

Se você já precisou trabalhar com versões diferentes de um mesmo Pacote de componentes no Lazarus, esse artigo pode ser útil.

<!--more-->

![Unsplash image]({{ page.image }})  

## Introdução {#introducao}

Os *frameworks* e *libs* sempre são disponibilizados no formato de Pacotes.

No Delphi são os arquivos \*.dpk e no Lazarus são os arquivos \*.lpk.

Os Pacotes são ótimos. Ajudam na separação lógica do código e reutilização, mesmo dentro de um único projeto.

Os Pacotes também servem para empacotar componentes que serão instalados na IDE.

Mas se você precisar utilizar uma versão diferente (mais atualizada) de um Pacote num projeto, mas não quer alterar o código de outros projetos mais antigos que também utilizam esse Pacote, o que fazer?

## Problemas {#problemas}

Pode ser sorte, mas a anos eu trabalho com componentes, frameworks ou libs (vou utilizar a nomenclatura *Libs* agora) de terceiros e quase nunca tive problemas em atualizar os fontes.

Talvez minha sorte tenha haver com o fato de que eu quase não atualizo as *Libs* que estão funcionando.

Por outro lado não posso ficar "parado no tempo" pois quando atulizar o código muita coisa pode ter mudado e levará um tempo até fazer todas as correções.

Então, precisamos atualizar alguma hora.

As vezes tenho que fazer pequenas correções no meu código ou mesmo envio *patches* para outros autores de componentes *Open Source*, mas nunca tive um problema grave.

Por isso sempre mantive apenas 1 source para cada *Lib*. Para projetos mais organizados que mantém *branches* imutáveis (*labels*) de versões, era ainda mais seguro.

Bem, não mais.

Parece que minha sorte acabou e venho tendo alguns problemas de incompatibilidade entre esses Pacotes.

São alguns problemas, não muitos.

As vezes uma atualização de algum Pacote não compila com a versão mais nova do Lazarus; outras vezes não compila com a versão mais nova do FPC; outras vezes o código foi tão alterado que fico receoso em atualizar, pois tenho projetos que não tem *Unit Tests* apropriados.

Então vi que era hora de me preocupar com isso e vou lhe explicar como estou fazendo atualmente.

## Versionando {#versionando}

Primeiramente eu entrei em contato com a [Lista Oficial do Lazarus](http://lists.lazarus.freepascal.org/pipermail/lazarus/) e (resumindo) [perguntei](http://lists.lazarus.freepascal.org/pipermail/lazarus/2017-March/231269.html) como eu poderia manter mais de uma versão de um mesmo Pacote.

Ao que parece o Lazarus IDE é bem *esperto* e consegue fazer o carregamento do Pacote correspondende a versão que você sinalizou que gostaria de utilizar quando adicionou o Pacote como dependência no seu Projeto.

No exemplo abaixo eu estou adicinando o Pacote AWS a partir da versão 0.4:

![Lazarus New Requirement](/images/photo-new-requirement-8287.jpg) 

Significa que se um Projeto estiver utilizando a versão 0.3 é esse Pacote que está disponível, mas se você abrir um novo projeto que utiliza a versão 0.4, por exemplo, a IDE irá recarregar a nova versão do Pacote.

Legal, mas não resolve 100%.

## Definindo Pacotes Preferenciais {#preferencial}

Muitas vezes o autor não versiona o Pacote, ou seja, ele não define os números de *Min* e *Max*. Nesse caso, não há como a IDE carregar o Pacote correto, pois todos teriam o mesmo número de versão.

Então mais uma vez o Lazarus me surpreendeu me dando a opção de marcar um [Pacote como preferencial](http://wiki.freepascal.org/IDE_Window:_Project_Inspector#Store_file_name_as_preferred_for_this_dependency), apontando diretamente para o *path* do *.lpk.

Quase perfeito, mas...

Não há opção na IDE para configurar o *path*. Ao utilizar a oção de *preferencial* você vai salvar o *path* nas configurações do seu projeto, mas ele vai salvar o *path* do Pacote que já está carregado.

Eu [sugeri](http://lists.lazarus.freepascal.org/pipermail/lazarus/2017-April/231287.html) que deveria haver essa opção lá na lista, porém não sei se não entenderam ou talvez eu não tenha entendido como o mecanismo funciona — ou talvez os autores estão curtindo o fim de semana merecido e irão responder outra hora — enfim.

De qualquer forma estou pensando em fazer esse *patch* para o Lazarus, ou seja, poder adicionar uma dependência apontando para um *path* relativo.

Enquanto não há essa opção, sigo em frente.

Estou utilizando no mínimo 3 diretórios com a cópia dos fontes de diferentes versões para o mesmo Pacote. 

São eles:

1. master — diretório com a versão de produção;
2. develop — diretório com a versão modificada, testes, patchs, etc;
3. snapshot — diretório com a versão original do autor;

Felizmente esses diretórios são **facilmente** alterados no XML \*.lpi de configuração do Projeto. É manual, porém bem fácil (como a maioria das coisas são no *Object Pascal*).

Caso eu queria mudar a versão de um Pacote, basta mudar o diretório com a versão correspondente e dar um *build* no Projeto.

Simples assim.

## Conclusão {#conclusao}

Os Pacotes são uma ótima maneira de modularizar seus Projetos em unidades lógicas que podem ser compartilhadas entre outros Projetos.

No entanto, manter a organização de várias versões não é um trabalho fácil. A IDE ajuda com o carregamento das versões corretas, porém nem sempre isso irá funcionar.

Eu prefiro as coisas mais [simples]({% post_url 2016-12-19-simplicidade %}), mais minimalista. Não quero ficar "dependente" da IDE para gerenciar meu trabalho. Então eu estou versionando meus Pacotes utilizando os diretórios e toda vez que adiciono uma nova dependência, marco como *preferencial* para que a IDE sempre abra a versão configurada.

Até logo.