---
layout: post
title: "AWS Lib: A História e o novo suporte a SES"
date: 2016-03-28
description: Como tudo começou e o novo Suporte a SES.
summary: Como tudo começou e o novo Suporte a SES.
image: /images/photo-awslist.png
categories: 
  - AWS Lib
tags:
  - aws
keywords:
  - awslib
  - aws
  - aws ses
  - api
  - amazon
--- 

Você já conhece o projeto OpenSource [AWS Lib](https://github.com/mdbs99/aws)?

**AWS Lib** é uma implementação minimalista para **Amazon Web Services (AWS)**. Inicialmente
foi implementada somente a API para a utilização de apenas um dos serviços, o **Amazon S3**,
até agora...

<!--more-->

![Amazon Web Services List]({{ page.image }})

##Como Tudo Começou

A três anos atrás fui contratado para a fazer a análise, arquitetura e implementação de alguns módulos 
de um sistema.

Apesar desse sistema utilizar outras linguagens, implementei os módulos utilizando a linguagem **Object Pascal**.

Esses módulos eram, na verdade, **ferramentas** para um grande sistema muito maior.

Um dos requisitos desse grande sistema era a utilização do serviço Amazon S3. Os motivos ou 
como eles utilizam o S3 são propriedade privada e não são relevantes aqui neste blog.

Esse requisito chegou até mim por último. Os módulos ou ferramentas já estávam quase terminados, 
mas agora tinha um novo requisito e eu não sabia nada sobre os serviços da Amazon.

Eu tinha apenas mais 1 semana para entregar o sistema.

Então fiz uma rápida pesquisa na Internet sobre possíveis implementações em Object Pascal para a 
**API Amazon S3** e não encontrei nada que pudesse utilizar. Lembro que existia alguma coisa em **Delphi**
mas o código era confuso e eu não estava utilizando Delphi mas sim, **FreePascal** (FPC) e **Lazarus**.

Então, mesmo que funcionasse em Delphi, eu não iria copiar o código alheio, converter para FPC e
depois cobrar por isso. É claro que não.

Então só havia uma coisa a fazer. Teria que codificar minha própria implementação.

Descobri que não era tão difícil mas tive dificuldades para implementar a parte de autenticação do serviço.

Quase terminando o prazo consegui resolver o problema fazendo uma classe **simples**, **suja** e 
totalmente **procedural**. Mas funcionava. O projeto foi entregue e foi um sucesso.

Esse módulo que utilizava o Amazon S3 era apenas 5% de tudo que tinha que entregar.

Depois de alguns dias resolvi publicar no [Github](https://github.com/mdbs99/aws) apenas essa classe
como projeto OpenSource, pois sabia que haveria outras pessoas com o mesmo problema que tive, ou seja, não
havia nenhuma implementação dos serviços AWS para Object Pascal — e pelo que sei ainda não há — especificamente
para FPC.

O tempo passou e o projeto ficou esquecido. Até que um dia um amigo me lembrou:
<pre>
— Você não tem vergonha daquele código publicado no Github? — ele disse.
— Que código? — perguntei.
— A implementação do Amazon S3...
— Err, sei... tá ruim aquele código e...
— Tá horrível! Você fala de Orientação a Objetos, é aquilo lá?
— Está bem, vou dar uma olhada no código.
— Acho melhor refazer tudo — ele disse.
</pre>

E ele estava certo.

Então eu refiz tudo. Trabalhei no código até que as prioridades mudarem novamente, ou seja, novos Projetos.

Mas ficou 100% melhor do que estava antes.

Defini alguns princípios para o projeto:

  1. todas as classes são *sealed*
  2. todos os métodos retornam uma interface ou um tipo primitivo
  3. todos os métodos públicos são implementações de métodos de uma interface
  4. todas as instâncias são imutáveis
  5. a alocação de memória é liberada automaticamente

Nada mal, eu acho.

##Amazon SES, um Novo Serviço
 
Pela imagem acima tem-se uma ideia de que a Amazon, para quem não conhece, tem dezenas de serviços. Um deles
é o [Amazon Simple Email Service](https://aws.amazon.com/pt/ses/).

<blockquote>
  <p>
    O Amazon Simple Email Service (Amazon SES)  é um serviço de e-mails econômico criado na infraestrutura 
    confiável e escalável que a Amazon.com desenvolveu para atender à sua própria base de clientes. 
  </p>
  <footer><cite title="Amazon SES">— Amazon SES</cite></footer>
</blockquote>

Na semana passada retornei ao projeto.

Comecei refatorando todos os construtores para utilizar 
o padrão do [Método New()]({% post_url 2016-01-10-interfaces-e-o-metodo-estatico-new %}). O motivo
é que estou preparando o código para a implementação de um novo serviço, o **Amazon Simple Email Service (SES)** e,
felizmente, não estou sozinho nessa empreitada.

[André Medeiros](https://www.facebook.com/andre.medeiros.5458?fref=ts) entrou em contato comigo em 2015 para 
falar sobre meu outro projeto, o [Greyhound](https://github.com/mdbs99/Greyhound), e assim nos tornamos amigos.

O tempo passou e em 12 de Março recebo um e-mail do André que falava, dentre outras coisas, o seguinte:

>Nos últimos dias precisei fazer uma integração com a API do SES, e pensei em usar a sua base para isso, já que ela esta muito bem feita. 

Mais uma vez, não sabia que serviço era esse.

O próprio André resumiu o que é o SES e como ele quer utilizar esse serviço na empresa dele. Achei bem interessante.

Alguém, além de mim, estava utilizando o AWS Lib em projetos reais e agora estava querendo extender seu uso para outros
serviços... Legal!

O AWS Lib foi feito com esse pensamento. A princípio seria só a implementação do S3, porém seu nome não é "AWS S3 Lib" 
mas sim, **AWS Lib** que é um nome genérico para compor todos as implementações possíveis de fazer com Object Pascal.

>...nosso desejo é fazer com que a sua AWS Lib possa nos acompanhar, 
>e tenho certeza que com nosso conhecimento em AWS podemos contribuir bastante para seu projeto

Então vamos lá, falei, vamos fazer isso!

##Problema na Autenticação

A autenticação padrão do AWS Lib não estava funcionando com o novo serviço, o SES.

Novamente problemas com a autenticação, pensei...

Ora, fiz a unit *aws_client* separada justamente pensando nessa reutilização de autenticação em todas as outras 
implementações de outros serviços... mas não estava funcionando.

Felizmente o André apresentou a solução 2 dias depois:

>O problema não era a forma de assinar, e sim a forma de declarar o tipo de assinatura para AWS.
>As autenticações se dividem em versões, e cada serviço da AWS suporta 1 ou mais tipos de autenticação

O SES suporta as autenticações das versões 3 e 4. Ele implementou a versão 3 e funcionou!

Mas...

##Refatorando o código, Você vai Gostar disso

Ontem eu atualizei os fontes do projeto no Github utilizando a implementação do André, ou seja, a unit *aws_ses*
assim como a pequena modificação na unit *aws_client* que ele precisou fazer para tudo funcionar.

>Acredito que você vai melhorar esta codificação, mas já é possível enviar email pelo SES e interagir com o S3 usando o AWS Lib.

André é leitor do blog. Ele sabe... bem, ambos sabemos que o código não está no "padrão". Mas mesmo assim
eu atualizei os fontes.

O motivo é simples: Vou utilizar esse *Case* para fazer refatorações e lhe explicar — você leitor — os motivos de tais
alterações. E, é claro, tais refatorações serão feitas tendo sempre em mente a verdadeira **Orientação a Objetos**.

Neste [*commit*](https://github.com/mdbs99/aws/commit/c5d990f4d55b4565035327705e2e33999d4f9126) você poderá ver o código novo. 
Funciona, mas não está como gostaríamos.

Vou utilizar os ensinamentos aqui expostos para direcionar a refatoração do código. O objetivo final é termos uma suíte
de componentes 100% codificado em Object Pascal que implementa a maioria dos serviços da Amazon.

Fique ligado!

Até logo.




