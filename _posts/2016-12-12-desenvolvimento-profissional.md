---
layout: post
title: "Desenvolvimento Profissional"
date: 2016-12-11
description:
  Comece a mudar o seu mindset sobre como desenvolver software profissionalmente.
image: /images/photo-gitlab86186324.jpg
categories: 
  - Professional
tags:
  - gitlab
keywords:
  - gitlab
  - github
  - bitbucket
  - professional
  - software professional
  - development
--- 

Como você controla o desenvolvimento dos seus projetos?

Eu quero dizer, os requisitos, fontes, 
tickets, *releases*, documentação... ou seja, todo os
artefatos do software.

Eu utilizo Git, *Tickets*, *Wikis* e GitLab para o controle
de todos os meus softwares privados.

<!--more-->

![Unsplash image]({{ page.image }})

## Introdução {#introducao}

Codificar um software hoje em dia não é tão difícil.
São tantas ferramentas automatizadas, vídeos com tutoriais
completos ou IDE's fantásticas, que praticamente qualquer
pessoa que realmente se interesse pelo assunto iria conseguir 
fazer um software.

Isso é incrível.

Mas existe uma *grande diferença* entre fazer um simples programa
e desenvolver um projeto *profissional*.

## Controle {#controle}

Como desenvolvedor, arquiteto e gerente de projetos pessoais ou 
de clientes, preciso ter o controle de todos os artefatos que compõe
todos os softwares para qual eu trabalho.

Sendo o software um trabalho de engenharia, mas também arte e trabalho
manual, ter o controle 100% de todos os detalhes é impossível.

No entanto precisamos ter o controle da *qualidade*, *prazo* e *orçamento*.

É necessário haver ferramentas para controlar tudo, desde o levantamento
de requisitos, codificação, testes, acompanhamento de problemas, lançamento
de versões e documentação.

As ferramentas para esse tipo de controle existem e estão a disposição para
qualquer desenvolvedor, mas nem todos as utilizam.

O uso dessas ferramentas separam os *amadores* dos verdadeiros *profissionais*.

## Ferramentas {#ferramentas}

Qualquer ferramenta que realmente ajude no controle do desenvolvimento de
software é válida. No entanto algumas dessas ferramentas já foram e são utilizadas
diariamente e, por serem tão eficazes, considero-as como um padrão
no mundo do desenvolvimento de software. E é sobre elas que irei falar.

***Git***, segundo a [Wikipedia](https://pt.wikipedia.org/wiki/Git),
é um sistema de controle de versão distribuído e um sistema de gerenciamento de 
código fonte, com ênfase em velocidade. O Git foi inicialmente projetado e desenvolvido 
por Linus Torvalds para o desenvolvimento do kernel Linux, mas foi adotado por muitos 
outros projetos.

Ele é o padrão atual para o controle de versão de softwares. Mas, não apenas por isso,
você *deve* utilizá-lo porque não há nenhum outro concorrente que seja tão *simples* 
e *eficaz*.

***Ticket* ou *Issue*** é a ferramenta mais utilizada por um profissional.
Tudo sobre o software deverá estar registrado em *tickets*. Desde uma 
dúvida do usuário sobre uma documentação, até a resolução de um *bug* complicado.
Tudo, *absolutamente tudo*, deverá ser registrado *antes* do desenvolvimento
ou execução da solicitação.

Eles mantém todo o histórico de solicitações e *decisões* desde o início do desenvolvimento.

***Wikis*** são sites que permitem a edição colaborativa de seu conteúdo e 
estrutura pelos usuários do sistema. Os usuários são os arquitetos, desenvolvedores
e *stakeholders* (partes interessadas).

Esses sites fazem parte da documentação do projeto. 
Todas as Regras de Negócio, configurações, manuais e informações gerais deverão estar
preservados nas páginas desses sites.
No entanto algumas poucas informações não deverão estar nesse formato, como 
configurações de usuário/senha, orçamentos e contratos.

***Branches*** são ramificações do código indo em outras direções do *branch* principal,
o *master*. O objetivo é manter o *branch* principal *master* sempre com a última versão
liberada para o usuário, enquanto o desenvolvimento vai sendo feito em outros *branches*.

Todo projeto deve ter no *mínimo* dois *branches*: *master* e *develop*.

Enquanto o *master* é "intocável" pela maioria dos desenvolvedores, o *develop* está em
constante desenvolvimento.
Você pode até utilizar outros nomes, mas é isso aí.

Cada *ticket* poderá gerar um novo *branch*, porém temporario. Após mesclar o
*branch* temporário no *develop*, ele poderá ser apagado. Isso melhora a rastreabilidade
das modificações do código.

No momento que o *develop* está pronto para ser liberado como uma nova versão do sistema,
uma mesclagem de todo o desenvolvimento deve ser feito no *master*.

***Tags*** são como *branches* imutáveis. A cada *release* do software devemos criar uma
*tag* correspondente a versão. Uma vez criada uma *tag*, seu código não poderá ser mais
alterado. Elas devem contar a história de todas as versões do software.

## Serviços {#servicos}

Existem vários serviços para o controle do desenvolvimento de softwares, no entanto vou
escrever apenas sobre os três maiores, que são os mesmos no qual tenho maior experiência.

**[Github](https://github.com/) é um portal de projetos** OpenSource incrível.
Como o próprio nome diz, ele oferece repositórios *Git*. Todo mundo utiliza.
Desde programadores amadores até grandes corporações privadas.

Eu o [utilizo](https://github.com/mdbs99) diariamente.

Essa [página](https://github.com/mdbs99/opp) que você está lendo agora está hospedado lá,
assim com alguns dos meus [projetos](https://github.com/mdbs99?tab=repositories) OpenSource.
Também contribuo em outros projetos e utilizo bastante
*libs* e *frameworks*. É fácil encontrar outros projetos. Fácil de usar. Fantástico.

Mas se você quiser ter um projeto privado lá, terá que pagar. Não é caro, 
na verdade, porém nem todo mundo quer ou pode pagar. Talvez você apenas queira
trabalhar num projeto acadêmico ou talvez só armazenar alguns exemplos de código...
não importa o motivo, você não quer pagar um valor mensal.

**[BitBucket](https://bitbucket.org/) é outro serviço** um pouco diferente do Github.
Ele também oferece repositórios *Git* e outros serviços que são "instalados" se 
você quiser. No entanto sua interface e usabilidade não são tão *simples* como no Github.

A boa notícia é que ele dá direito a repositórios privados e gratuitos. Esse foi um 
dos fatores que me fez escolher o BitBucket como meu serviço para projetos privados
desde 2012. Mas esses repositórios privados, somados todos,
[só podem ter](https://bitbucket.org/product/pricing/) até 5 desenvolvedores.
Então quando seus  projetos começarem a crescer e você ainda não quiser pagar ou 
não estiver satisfeito com o funcionamento geral do sistema, você terá que procurar 
outra solução.

**[GitLab](https://gitlab.com/) é o melhor** dentre eles pois une o que ambos
acima tem de melhor: projetos privados, gratuitos, ótima interface e usabilidade.

Quem me indicou o GitLab foi meu amigo [Fabricio Cabral](https://github.com/fabriciofx),
que também estava em busca de algo parecido com o Github.

O GitLab tem a interface parecida com o Github. Simples e elegante. No entanto o Github
ainda consegue ser "perfeito" nesse quesito.

Mas o GitLab tem outras features que, até o momento — ou até onde eu saiba —, não existem
no Github:

  1. *Tickets* com [anexo](https://help.github.com/articles/file-attachments-on-issues-and-pull-requests/)
  2. [Time tracking](https://about.gitlab.com/2016/11/03/track-your-time-in-the-same-tool-you-do-your-work/)

Dar a possibilidade ao usuário para anexar um relatório, documento, *prinscreen*
ou um esboço de tela sempre foi imprescindível pra mim.

*Time tracking* é uma *feature* nova que estou começando a utilizar. É aquele tipo de coisa
que falamos: "Como ninguém havia pensando nisso antes?"

E você ainda tem a possibilidade de ajudar no desenvolvimento do projeto e propor melhorias.
É tudo OpenSource — mas eles comercializam uma versão paga — e codificado em Ruby.

A companhia responsável pelo desenvolvimento do GitLab não para de crescer.

<blockquote class="twitter-tweet" data-lang="pt"><p lang="en" dir="ltr">Thanks <a href="https://twitter.com/businessinsider">@businessinsider</a> for including us on your list of &quot;enterprise <a href="https://twitter.com/hashtag/startups?src=hash">#startups</a> to bet your career on&quot;<a href="https://t.co/YFuLfGFheS">https://t.co/YFuLfGFheS</a> <a href="https://t.co/4DmHdbAcZs">pic.twitter.com/4DmHdbAcZs</a></p>&mdash; GitLab (@gitlab) <a href="https://twitter.com/gitlab/status/806489296261812224">7 de dezembro de 2016</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

Por isso tudo acho que este é ótimo serviço de uma grande companhia.
Não teria restrições se tivesse que pagar por ele no futuro, caso meus 
projetos ficassem maiores do que a versão gratuita é capaz de suportar.

Eu posso começar pequeno, com projetos privados e gratuitos, testar
todo o sistema e ver se me adapto à ele antes de começar a pagar. Perfeito.

O único problema que tive até agora no uso do GitLab é encontrar o serviço temporariamente 
indisponível por estar sendo atualizado. Eles utilizam o GitLab para fazer o GitLab.
Então a cada nova versão liberada o serviço pode ficar intermitente.

<blockquote class="twitter-tweet" data-lang="pt"><p lang="en" dir="ltr">We&#39;ll be deploying 8.14.4 shortly. No downtime is expected but you may see intermittent errors during this time.</p>&mdash; GitLab.com Status (@gitlabstatus) <a href="https://twitter.com/gitlabstatus/status/806931030674460673">8 de dezembro de 2016</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

Mas quando você não está pagamento nada e ainda é avisado — por Twitter — antes
do serviço ficar indisponível bem, você não teria o direito de reclamar, teria?

A verdade é que estou utilizando o GitLab a apenas 1 mês. Mas como tenho bastante
conhecimento dos outros serviços, posso dizer que o GitLab superou minhas expectativas.

Atualmente estou migrando todos os meus projetos privados do BitBucket para o GitLab — ele tem
um *importador de projetos* que facilita muito esse trabalho.

Mesmo em tão pouco tempo posso afirmar que o GitLab é um grande concorrente, senão o melhor.
E, até agora, estou bastante satisfeito.

## Conclusão {#conclusao}

Projetos profissionais não são feitos apenas com código e boa intensão. Temos que ter
o controle sobre todos os artefatos.

Esse controle é feito utilizando ferramentas especializadas.
Acima você pode ver quais foram as minhas escolhas.

Mas, antes de tudo, é necessário ter disciplina e conhecimento. Do contrário nenhuma ferramenta 
será suficiente.

Até logo.