---
layout: post
title: "Nomeando Unidades"
date: 2016-07-18
description: Veja porque devemos utilizar nomes hierárquicos nas Unidades do projeto.
summary: Veja porque devemos utilizar nomes hierárquicos nas Unidades do projeto.
image: /images/photo-1434030216411-0b793f4b4173.jpg
categories: 
  - Pascal
tags:
  - naming
keywords:
  - naming
  - nomeando
  - unit
--- 

Acredito que todos os nomes de Unidades em um Projeto *Object Pascal*, quiça em qualquer outra linguagem, deveriam ser hierárquicos do mais genérico para o mais específico.

Neste artigo irei escrever sobre a lógica em utilizar uma nomenclatura hieraquizada para deixar seu projeto mais coeso e elegante.

<!--more-->

![Imagem]({{ page.image }})

##Introdução {#introducao}

Foi-se o tempo quando programávamos utilizando nomes de Unidades como: `uUtils`, `uPerson`, `frmClient`, etc. 

Em projetos pequenos tais nomes de Unidades ainda são utilizados (será?), mas essa nomenclatura não é nada profissional.

Por quê?

O motivo é que uma Unidade deve encapsular um Contexto **bem definido**. Um Contexto pode ser genérico ou ser uma especialização.

Um nome de Unidade como esses acima não transmitem um Contexto bem definido.

Para que a idéia de Contexto fique clara, uma Unidade deve ter um nome sem ambiguidades.

Veja esse exemplo de nomes de Unidades:

  1. frmClient
  2. AcmeClientEditForm
  
Qual dos nomes é mais claro, passando imediatamente a ideia do que essa Unidade faz?

O item #2, é claro.

  1. `Acme` é o nome do Projeto ou nome da Empresa.
  2. `Client` é o Contexto mais genérico.
  3. `Edit` é o Contexto mais específico.
  4. `Form` determina hierarquia da Classe ou o seu propósito.

A medida que um projeto cresce e novas Unidades são inseridas, colisões de nomes podem ocorrer. Ter bons nomes de Unidades no projeto fará muita diferença no longo prazo. Será mais fácil determinar o impacto de alguma alteração apenas olhando a hierarquia no nome da Unidade.

Mas existem regras ou dicas para escolhermos bons nomes de Unidades?

É o que iremos ver a seguir.

##Regra 1: Prefixos {#regra-1-prefixos}

Um nome de Unidade hieraquizado nada mais é do que utilizar **prefixos**. A cada prefixo adicionado, damos o nome de Contexto. O nome para cada Contexto ou prefixo utilizado vai depender de cada projeto. 

Um Contexto pode identificar uma tecnologia, protocolo, setor da empresa, entidade ou até mesmo um outro sistema.

Para sistemas comerciais, o próprio usuário irá lhe dar os nomes desses Contextos. Os nomes dos setores na empresa serão o ponto de partida.

##Regra 2: Nome do Projeto ou Empresa {#regra-2-nome-do-projeto-ou-empresa}

Toda Unidade deve ser precedida com o nome do Projeto ou nome da Empresa.

Se teus projetos são feitos somente dentro de tua empresa para seu próprios clientes, basta utilizar o nome do Projeto como prefixo para todas as Unidades.

Caso sua empresa faça *frameworks* ou projetos OpenSource, ou seja, se o projeto terá um contexto mais global, é uma boa ideia utilizar o nome da Empresa como prefixo.

Qual dos dois será utilizado dependerá do propósito geral do Projeto.

##Regra 3: Genérico para o mais Especializado {#regra-3-generico-para-especializado}

O nome de uma Unidade deve ser hierarquizado do mais genérico para o mais especializado.

No artigo sobre [Nomenclatura de Classes]({% post_url 2016-04-25-nomeando-classes %}) eu lhes dei um exemplo de nomenclatura (errada) para uma hipotética Classe do meu [Projeto AWS](https://github.com/mdbs99/aws) que foi `TAWSNetHTTPClient`.

O nome de uma Classe não deve ser uma hierarquia.

Classes nem deveriam fazer parte de uma [hieraquia de herança]({% post_url 2016-05-23-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-1 %}) na maioria das vezes.

A hierarquia deve estar no nome da Unidade.

Então como ficariam os nomes, da Classe e da Unidade, considerando o exemplo acima?

  1. Unidade: AWSNetHttpClient
  2. Classe: THttpClient

O `Http` se repete no nome da Classe. O motivo é porque o nome da Classe deve ser constituído de **Contexto + Nome**, conforme já explicado [aqui]({% post_url 2016-04-25-nomeando-classes %}).

Outros exemplos de bons nomes para Unidades:

  * AcmeData
  * AcmeDataXml
  * AcmeDataJson
  
  * AcmeReport
  * AcmeReportRBuilder
  
  * AcmeWeb
  * AcmeWebHttp
  * AcmeWebFtp
  
  * AcmeFinances
  * AcmeFinancesClient
  * AcmeFinancesClientReports
  
##Regra 4: Interfaces numa Unidade, Classes noutra {#regra-4-interfaces-e-classes}

Toda Classe deveria [implementar ao menos uma Interface]({% post_url 2016-01-18-interfaces-em-todo-lugar %}) e todo o Projeto deveria trabalhar com variáveis e argumentos do tipo Interface. Dessa forma um argumento poderia receber qualquer instância de Classe que implementa o tipo da Interface.

Então temos que definir as Interfaces numa Unidade e suas implementações *default* em outra Unidade. Caso o programador queira codificar sua própria implementação da Interface, ele só precisa adicionar no *uses* apenas a Unidade que contém as Interfaces.

Como nomear ambas as Unidades?

Bem, eu utilizo uma nomenclatura que a princípio você não verá muita lógica, mas ficou como um costume pra mim. Talvez você pense numa nomenclatura melhor e nos diga na área de comentários.

Vamos lá. Para Unidades sobre Finanças, por exemplo, eu teria o seguinte:

  1. Unidade das Interfaces: AcmeFinances
  2. Unidade das Classes: AcmeFinancesA
  
Isso aí. Apenas a letra `A` como sufixo.

O motivo é que as Unidades são exibidas em ordem alfabética, então a Unidade de Interfaces virá antes da Unidade de Classes.

Caso eu tenha outro sub prefixo que também começe com `A`, exemplo `AcmeFinancesAction`, ainda assim a ordem visual seria mantida, do mais genérico para o mais especializado.

##Regra 5: Uma Unidade especializada utiliza a mais genérica, nunca o contrário {#regra-5-unidade-especializada}

Essa regra é clara, mas sempre é bom lembrar que uma Unidade genérica **nunca** poderá ter um *uses* para uma mais especializada dentro da mesma hierarquia.

Veja o esquema abaixo:

  1. AcmeFinances
  2. AcmeFinancesClient >> uses AcmeFinances
  3. AcmeFinancesClientReports >> uses AcmeFinancesClient

##Regra 6: Se uma Unidade está sendo utilizada em muitas outras hierarquias, ela não pertence ao Projeto {#regra-6-unidade-nao-pertence-ao-projeto}

Se uma Unidade ou um pequeno conjunto delas, sempre aparecem no *uses* de várias hierarquias diferentes, isso pode indicar que essa(s) Unidade(s) não fazem parte do *core* do projeto devem ser refatoradas/extraídas para outro projeto ou *package*.

Se essas Unidades podem ser reaproveitadas em outros projetos por serem muito genéricas, é também outro grande indício para refatorar e extraí-las do projeto.
  
##Bônus

###Nomes de Unidades Pontilhadas {#unidades-pontilhadas}

Hoje e dia o Delphi e FreePascal/Lazarus tem suporte para "Nomes de Unidades Pontilhadas", ou seja, podemos nomear uma Unidade utilizando pontos que separam contextos. No Java sempre foi assim e o .NET fez igual.

E em que isso muda as regras acima?

Nada.

Sem considerar os pontos, não há nenhuma diferença técnica ou de utilização para utilizar `Acme.Finances.Client.Reports` ao invés de `AcmeFinancesClientReports`.

A "vantagem" em utilizar "Nomes de Unidades Pontilhadas", na minha opinião, é apenas visual.

Veja um exemplo:

  1. AcmeWebHTTPClient
  2. Acme.Web.HTTP.Client
  
A opção #2 é mais *clean* que a opção #1 devido ao Contexto `HTTP` ter sido escrito em maiúsculo.

Obs.: Já ouvi falar que o compilador do Delphi atualmente tem parâmetros que podem selecionar um *namespace* específico para compilar Unidades específicas e isso pode ter haver com esse tipo de nomenclatura que utiliza pontos. Caso você saiba mais sobre isso, porfavor deixei seu comentário.

###Abreviações em Pascal-case ou tudo em maiúsculas {#abreviacoes-maiusculas}

Qual nomenclatura utilizar para abreviações, por exemplo, `HTTP`?

Sendo `HTTP` uma abreviação de *Hypertext Transfer Protocol*, o mais correto na escrita seria `HTTP` e não `Http`. Mas visualmente, quando uma abreviação faz parte do nome de uma Classe, fica mais elegante ou mais fácil de ler utilizando Pascal-case.

Teoricamente `THttpClient` é mais fácil de ler do que `THTTPClient`.

Mas nesse quesito eu mesmo ainda não tomei uma decisão. Algumas vezes eu utilizo tudo em maiúsculo, outras vezes em Pascal-case. Ainda tenho um conflito interior entre o "certo na escrita" vs. "visual elegante".

##Conclusão {#conclusao}

Escolher bons nomes para Unidades, Classes, Interfaces e métodos sempre será uma arte. Não existe o **certo absoluto**. Cada projeto nos dará seus requisitos e os nomes irão aparecer.

Essas Regras podem ajudar, mas cabe ao arquiteto ter bom senso, conhecer bem o domínio e pensar bem antes de nomear suas Unidades.

Até logo.
