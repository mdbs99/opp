---
layout: post
title: Versionando e Organizando seus Pacotes
date: 2017-09-25
permalink: /:title
description:
  Veja nesse artigo como trabalhar com múltiplas versões de um mesmo Pacote.
image: /images/2017/photo-neonbrand-345038.jpg
categories:
  - Xavier
tags:
  - package
  - open-source
  - lib
keywords:
  - freepascal
  - fpc
  - delphi
  - lazarus
  - object-oriented
  - oop
  - mdbs99
  - package
  - packages
  - version
  - versioning
---

O desenvolvimento de Pacotes reutilizáveis pode se tornar complicado quando cada projeto está utilizando diferentes versões do mesmo pacote.

<!--more-->

![Unsplash image]({{ page.image }}) 

## Introdução {#introducao}

A modularização de sistemas é um conceito muito abrangente. Podemos modularizar um projeto desde o nível de arquivos (unidades) até o uso de WebServices e Microservices executando em servidores completamente distintos.

Nesse meio, temos a modularização por Pacotes.

Acredito que o uso de Pacotes seja o meio mais eficaz e simples para modularizar sistemas pequenos a médios.

Até mesmo grandes sistemas podem (e devem) se beneficiar do uso de Pacotes, porém como a interoperabilidade com outras aplicações é quase mandatória em sistemas maiores, outras técnicas de modularização devem ser utilizadas em complemento. 

Mas essa eficácia tem um (baixo) custo que é <del>gastar</del> investir parte do tempo do desenvolvimento na organização e versionamento desses Pacotes.

Eu já escrevi sobre esse assunto [aqui]({% post_url 2017-04-03-pacotes-e-versoes-no-lazarus %}), porém esse artigo é um aprimoramento sobre como versionar e consequentemente organizar seus Pacotes no [Lazarus](http://www.lazarus-ide.org/).

Talvez as ideias a seguir possam funcionar também no Delphi, mas eu não sei. Eu não encontrei nenhum artigo sobre como ter mais de uma versão de um mesmo (*runtime*) Pacote no Delphi. Na verdade encontrei *workarounds* estranhas como [essa resposta](https://stackoverflow.com/a/998096) no *StackOverflow* sobre como instalar múltiplas versões.

## Pacotes Registrados {#pacotes-registrados}

No Lazarus, todo Pacote *runtime* é registrado automaticamente na IDE quando você o compila. Simples assim. Abra o Pacote. Compile. Pronto.

Após o "registro" do Pacote, ele estará disponível para ser adicionado em qualquer projeto através da janela do *"Project Inspector -> Required Packages"*.

Essa funcionalidade é muito, muito útil para a modularização de qualquer projeto.

Imagine você poder criar um Pacote a qualquer momento, adicionar algumas Unidades e tê-lo disponível para uso sem preocupações com *paths*.

Ao abrirmos a janela *Required Packages*, todos os pacotes registrados aparecem numa *combo box* com auto-completar ao digitar.

Quase perfeito.

No entanto, é comum um Pacote ser utilizado por mais de um projeto. Então, se modificarmos algo nesse Pacote compartilhado, talvez todos os projetos que o utilizam deverão sofrer alterações... o que pode ser um problema caso você tenha dezenas de projetos ou mais.

Mas, e se tivermos múltiplas versões de um mesmo Pacote, sendo que cada projeto poderá escolher qual versão utilizar?

Isso resolveria o problema.

## Versionamento {#versionamento}

Quando desenvolvemos um Pacote, na maioria das vezes, temos 2 objetivos primários: Modularização e  Compartilhamento. 

Modularizar o sistema em pequenas partes mais gerenciáveis, encapsulando contextos menores e bem definidos, facilita o desenvolvimento e a manutenção de todo o sistema. A modularização por Pacotes já nos dá um grande benefício.

No entanto, acredito que a principal razão para a maioria dos desenvolvedores criarem pacotes é devido ao compartilhamento de Unidades entre projetos. Esse é um pensamento lógico e razoável. Mas se algo é compartilhado entre mais de um projeto, como ter o controle de qualidade ao fazer uma mudança no código do Pacote que é utilizado em muitos lugares?

Por exemplo, se você alterar a interface de uma Classe, acrescentar mais um método numa Interface, remover algo que não é mais necessário, etc, como saber quais projetos serão afetados?

Não há como saber.

Então você segue em frente e faz as alterações necessárias no seu Pacote e também no seu projeto atual, sem pensar nos projetos mais antigos.

Poderia haver outros projetos que utilizam o mesmo Pacote e que, ao tentar compilá-los futuramente, eles não irão compilar e você teria que lembrar o que fez anteriormente e ir alterando cada parte do projeto para torná-lo compatível com a nova versão do Pacote, novamente.

É claro, você terá que fazer isso em algum momento. O problema é que abrimos projetos antigos, na maioria das vezes, apenas para acrescentar ou consertar algo. E o que em teoria seria uma tarefa simples, poderia demorar horas ou dias apenas por que você, primeiro, terá que compatibilizar o código do projeto com o código do seu Pacote recém alterado.

A *solução* é o versionamento dos Pacotes.

Todo Pacote (compartilhado entre projetos) deve ser versionado. Isso quer dizer que você terá que ter N diretórios do mesmo Pacote, ou seja, cada diretório irá tratar de uma específica versão do Pacote. A versão é definida pelos números Mínimo e Máximo para cada versão do Pacote.

Tendo essas versões definidas, basta [escolhermos](http://wiki.freepascal.org/Lazarus_Packages#Using_minimum.2C_maximum_version) uma versão mínima para cada projeto quando o Pacote for incluído como uma dependência.

As versões de dependência do(s) Pacote(s), que ficam gravadas nos arquivos do projeto, é tudo que o Lazarus precisa saber para carregar a versão correta quando um projeto for aberto na IDE.

E, *voilá*! Você não precisa mas alterar todos os projetos que utilizam determinado Pacote. Você pode evoluir a versão mais atual, mas mantendo as versões antigas totalmente funcionais. Então, ao abrir projetos antigos, você só iria fazer modificações para utilizar a versão do Pacote mais atual se, e somente se, fosse estritamente necessário. Do contrário, continue trabalhando com a versão antiga que o projeto já utilizava anteriormente, pois ela não deixou de funcionar.

Perfeito agora?

## Fluxo, Organização e Desenvolvimento {#solucao}

Se quisermos ter um sistema simples, eficaz e organizado, precisamos dedicar algum tempo à sua manutenção ao invés de somente codificar as [Regras de Negócio]({% post_url 2017-01-09-codigo-duplicado-talvez-nao %}#regras-de-negocio) do projeto.

Ao criamos esses Pacotes, precisamos definir um fluxo de trabalho para que tudo dê certo não só agora, mas principalmente no longo prazo.

Hoje em dia, com o acesso quase irrestrito a informação, qualquer programador iniciante consegue criar um pequeno ou médio sistema totalmente funcional. Mas o que separa programadores iniciantes dos verdadeiros profissionais, é a arquitetura bem estruturada dos projetos, Pacotes, e todo o resto... no longo prazo.

### Passo a Passo {#passo-a-passo}

Essa é a maneira que utilizo para organizar meus Pacotes ou Libs:

A primeira coisa a fazer, é criar o diretório do Pacote e depois um subdiretório para o primeiro *release* — normalmente a versão 1.0.

Diferentemente do que eu disse [aqui]({% post_url 2017-04-03-pacotes-e-versoes-no-lazarus %}#preferencial) sobre a nomenclatura, considere nomear os diretórios de Pacotes com o número da versão correspondente — para projetos, no entanto, a nomenclatura sugerida continua válida — mas isso não faz diferença para o compilador, sendo apenas uma questão de organização visual.

Exemplo:

    yourpack
      |
      1.0
        |
        bin  : todos os arquivos binários e libs
        |
        pkg  : pacotes e configurações
        |
        src  : fontes de produção
        |
        test : fontes de testes

Segundo, você irá trabalhar nos fontes do seu Pacote. Irá criar Unidades, implementar testes, projeto com os testes, etc. Não esqueça de definir a versão atual real, por exemplo, 0.8 (lembre-se que ainda não chegamos na versão 1.0.

Então, digamos que ele já pode ser utilizado em algum dos seus projetos, mesmo que a versão atual não seja a 1.0.

Terceiro, você vai adicionar ao seu projeto uma nova dependência para o seu pacote, mas ao adicioná-lo no projeto, irá definir a versão *mínima* de uso que, atualmente, é a 0.8.

A versão *máxima* de uso é opcional (nesse caso) pois mesmo que você atualize a versão do Pacote para, digamos 0.9.2, seu projeto continuará funcionando pois apenas a versão mínima foi definida.

O Lazarus procura os Pacotes pelo seu nome, versão e *path*, no arquivo `packagesfiles.xml`, que é gerenciado pela IDE.

Então, ambos os projetos (seu projeto e o Pacote) vão sendo atualizados, até que você resolve publicar a versão 1.0 do Pacote — determinado pelo mesmo nome do diretório.

Nesse momento, você irá fazer um cópia do diretório `/1.0` para gerar um novo diretório, por exemplo o `/2.0`.

    yourpack
      |
      2.0
        |
        bin  : todos os arquivos binários e libs
        |
        doc  : toda a documentação
        |
        pkg  : pacotes e configurações
        |
        src  : fontes de produção
        |
        test : fontes de testes

Veja que, hipoteticamente, somente na versão 2.0 foi criado um diretório `/doc` com a documentação do projeto. Está evoluindo...

A partir do diretório 2.0 você irá continuar seu trabalho com as próximas versões: 1.1, 1.2... até chegar na 2.0 quando o ciclo recomeça.

No meio desse ciclo, imagine que seu cliente, dono do primeiro projeto de exemplo, abre um *ticket*  relatando um problema. Você então abre o antigo projeto e "magicamente" a IDE faz o *load* da versão correta do Pacote, ou seja, a versão 1.0 que esse projeto ainda utiliza. Você não precisa compatibilizar esse projeto com a versão 2.0 do seu Pacote agora. Você pode (deve) fazer isso mais tarde, pois a prioridade agora é resolver o *ticket* do seu cliente.

## Conclusão {#conclusao}

Na conclusão do [artigo]({% post_url 2017-04-03-pacotes-e-versoes-no-lazarus %}) anterior sobre esse assunto, eu disse que [marcava](http://wiki.freepascal.org/Lazarus_Packages#Different_versions_of_a_package) as dependências como *preferencial* — opção que utiliza o *path* do pacote, não a versão. Porém, percebi que utilizando o fluxo descrito acima eu teria *liberdade* na configuração das dependências do meu projeto. O *path* seria gerenciado pela IDE e para utilizar novas versões dos Pacotes bastaria mudar os números mínimo e máximo.

Para a maioria dos desenvolvedores, Pacotes são sinônimos de Componentes instalados na IDE. Mas eles são muito mais que isso.

A modularização de sistemas utilizando Pacotes sempre foi muito negligenciada e espero que esse artigo tenha aberto seus olhos. 

Até logo.


