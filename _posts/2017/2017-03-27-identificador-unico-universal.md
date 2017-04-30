---
layout: post
title: Identificador Único Universal
date: 2017-03-27
description:
  O uso do Identificador Único Universal (UUID) simplifica o desenvolvimento Orientado a Objetos e também o relacionamento entre as Tabelas do Sistema.
image: /images/photo-greg-rakozy-38802.jpg
categories: 
  - Database
tags:
  - Database
keywords:
  - freepascal
  - delphi
  - lazarus
  - c#
  - csharp
  - java
  - object-oriented
  - oop
  - mdbs99
  - uuid
  - guid
  - oid
---

Como identificar unicamente seus Objetos?

Como fazer migração de dados sem se preocupar com campos auto-incremento e *foreign key* entre as tabelas?

Como diminuir o número de tabelas?

<!--more-->

![Unsplash image]({{ page.image }})  

## Introdução {#introducao}

O [*Universally Unique Identifier*](https://en.wikipedia.org/wiki/Universally_unique_identifier) (UUID) é um número de 128-bit utilizado para identificar uma informação unicamente.

A Microsoft utiliza o termo *Globally Unique Identifier* (GUID), mas na prática é a mesma coisa.

Nesse artigo você vai aprender como utilizá-lo.

## Tabelas e Relacionamentos {#tabelas-e-relacionamentos}

A muitos anos atrás eu estava fazendo um curso de Programação de Computadores onde uma das matérias era sobre Sistemas Gerenciadores de Banco de Dados (SGBD's).

Nesse curso ensinavam que tabelas são relacionadas entre si através das *Foreign Keys* (FK's). Um ou mais campos na tabela `A` deveria ter sua contra-parte na tabela `B`, ou seja, os mesmos campos com os mesmos valores. 

Executando um comando no SGBD para criar uma FK entre essas tabelas, você garante que ambas estão relacionadas entre si e que esse vínculo não pode ser quebrado (normalmente) alterando ou excluindo registros em apenas um dos lados.

E isso não mudou. 

O conceito continua o mesmo e ainda é muito utilizado hoje em dia para manter a integridade dos dados.

Um campo "chave", na época que eu estava fazendo o curso, era o Cadastro de Pessoa Física (CPF). Por ser "único", esse era um dos campos escolhidos quando trabalhávamos com Pessoas Físicas.

Mas anos atrás era comum uma mulher casada apresentar e utilizar o CPF do marido. Eu não sei quem teve essa ideia idiota, mas o fato é que era *aceitável* naqueles dias.

Então muitos desenvolvedores construíam seu Modelo de Dados com *Chaves Estrangeiras Compostas*. Poderia ser o CPF e Data de Nascimento, por exemplo.

Mas e se marido e mulher nasceram no mesmo dia? Nesse casos a chave composta ficaria duplicada.

Ah, então a chave será CPF, Data de Nascimento e Nome. É impossível eles terem o mesmo nome. Agora sim, problema resolvido?!

Bem, a FK poderia estar "correta", no entanto agora você está *duplicando* as informações de 3 campos entre essas tabelas.

Toda vez que você atualizar o nome, teria que atualizar em ambas as tabelas. Claro, você poderia fazer isso através da FK (ON UPDATE CASCADE) ou através de *triggers*, mas o fato é que os dados continuariam duplicados.

Multiplicando esses mesmos campos em dezenas de tabelas com milhões de dados, é um grande desperdício. Desperdício de armazenamento em disco e de transferencia de dados entre o servidor e a aplicação.

Então criaram o "conceito" de "Identificador Numérico".

## Identificador Numérico {#identificador-numerico}

Eu não sei se há realmente um conceito ou *paper* por trás dessa ideia, muito menos o autor ou autores originais — se você sabe, posta nos comentários por favor.

Então alguém — ou muitos ao mesmo tempo — pensou que seria mais fácil ter apenas um único campo para representar a FK entre as tabelas.

Esse tipo de campo é conhecido hoje em dia por todos os desenvolvedores como o Identificador, Identificador Numérico ou apenas ID.

O ID é um número.

Um exemplo seria termos uma tabela `PEDIDO` e outra `PEDIDO_ITENS`. Se eu quero relacionar essas tabelas (1..N, respectivamente) basta fazer uma FK dessa forma:

`PEDIDO.id <-> PEDIDO_ITENS.pedido_id`

Agora a duplicação de valor é de apenas um campo numérico que é bem mais simples e eficaz, ocupando menos espaço em disco, gerando menos transferência de dados rede.

Frameworks foram e são baseados nesse conceito (RubyOnRails por exemplo). 

Até mesmo alguns *Widgets* tinham propriedades do "tipo ID" para identificação de registros na tabela.

Parecia estar tudo resolvido, até você ter que modelar algo assim:

Imagine que no seu Modelo de Dados você tem uma tabela para `PESSOA_FISICA` e outra de `PESSOA_JURIDICA`. São, digamos, os "tipos de clientes" que você tem. São tabelas diferentes porque os campos são bem diferentes.

Então você tem uma tabela de `FATURAS` onde quer concentrar todas as faturas de todos os Clientes.

Como fazer esse relacionamento?

Temos algumas opções.

#### 1- Tabela FATURAS com 2 campos ID's {#ex-1}

A tabela `FATURAS` iria ter 2 ID's (`pessoa_fisica_id` e `pessoa_juridica_id`).

Nesse tipo de *design* um dos campos sempre estaria [NULO]({% post_url 2016-04-11-nao-utilize-nil-ou-null %}).

Se houver mais "tipos de pessoa", você teria que ir acrescentando mais e mais campos.

É um tipo de *design* muito deselegante, no *mínimo*. 

#### 2- Tabela FATURAS com 1 campo ID e 1 campo Tipo {#ex-2}

Nessa opção a tabela `FATURAS` iria ter 2 campos identificadores: 

  1. Um campo `pessoa_id int` que identifica o valor numérico; 
  
  2. Um campo `pessoa_tipo char(1)` que identifica o "tipo de pessoa" ("F" = Física; "J" = Jurídica);

Nesse tipo de *design*, você não precisa acrescentar mais campos se 
houverem mais "tipos de pessoa" no futuro. Basta identificar por um 
`char` o tipo. Enquanto o campo `pessoa_id` iria se relacionar com o 
registros específico da tabela específica do "tipo de pessoa".

Mas continua bem deselegante. Eu diria, pior.

Além disso, a FK é apenas "virtual", pois você não pode ter um único campo que é relacionado com mais de 1 tabela.

#### 3- Tabela FATURAS com FK apenas para PESSOA {#ex-3}

Uma terceira opção seria refatorar ambas as tabelas de Pessoa, criando uma terceira tabela `PESSOA` e fazendo com que as tabelas "herdem" os campos comuns dessa nova tabela.

                  PESSOA ------- FATURAS (pessoa_id)
                    ^
            --------|--------
            |               |
      PESSOA_FISICA     PESSOA_JURIDICA

Então, `FATURAS` iria ter um campo `pessoa_id` somente. Teria uma FK "forte" pois realmente iria estar relacionada com `PESSOA` através do campo `pessoa_id`.

Tudo certo?

De fato esse *design* parece mais *limpo*, mas profissional e até mais "Orientado a Objetos", pois agora temos até o conceito de "herança" em tabelas(!)

Porém, essa abordagem tem os mesmos problemas do uso da [Herança de Classe]({% post_url 2016-05-23-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-1 %}):

  1. É difícil de fazer uma hierarquia desde o início, quando os requisitos ainda não estão bem definidos;

  2. Uma Tabela pode ter sido criada com a "herança" errada e agora será difícil refatorar todas as *queries* já em produção;

  3. É mais verboso escrever *queries*, visto que agora você sempre terá mais uma tabela `PESSOA` para fazer o relacionamento;
  
  4. A performance tende a cair, visto que haverá mais tabelas envolvidas;

Interessante, porém na prática é ineficiente.
  
#### 4- Novas Tabelas de FATURAS {#ex-4}

Uma opção mais voltada para "conjuntos de dados" é criamos outras tabelas de Faturas: `FATURAS_FISICA` e `FATURAS_JURIDICA`

Então, teríamos esses relacionamentos:

  1. `PESSOA_FISICA.id <-> FATURAS_FISICA.pessoa_id`
  
  2. `PESSOA_JURIDICA.id <-> FATURAS_JURIDICA.pessoa_id`

Todas as FK funcionam.

Não haveria problema de performance, visto que você só iria utilizar as tabelas do mesmo "contexto de dados".

O grande problema é que ambas as tabelas são iguais e se houver alguma manutenção relacionado a Faturas, teria que ser feito em ambas as tabelas. 

Se houver mais um "tipo de pessoa" futuramente, teríamos que criar uma nova tabela, o que não é nada bom.

#### 5- Combinar as Opções 3 e 4 {#ex-5}

Repare que você pode combinar as Opções #3 e #4.

Poderia continuar existindo a Tabela `FATURAS` que estaria relacionada com as novas Tabelas `FATURAS_FISICA` e `FATURAS_JURIDICA`.

No entanto, essa é uma opção "virtual" que só existe se você utilizar as oções #3 e #4 num mesmo Modelo.

#### Sua escolha é?

Pois bem. Considerando os exemplos acima, qual das opções você diria que tem a "melhor" abordagem?

Pense um pouco sobre isso antes de continuar lendo o artigo — e se você tiver outra opção não mencionada aqui, eu gostaria de ler lá nos comentários, Ok?

Pense nos prós e contras de cada uma das opções... escolheu?

Certo.

Quer saber qual é a minha escolha?

Tendo eu utilizado todas as opções acima — e variações destas no decorrer de anos — tenho o prazer de lhe dizer que a minha escolha é: *Nenhuma das opções acima*.

## UUID é a Solução {#uuid}

E se eu lhe dissesse que podemos unir (quase) tudo de bom das opções acima num único conceito mais simples, com menos tabelas, apenas 1 campo para FK e com performance igual ao uso de um ID numérico?

> O uso UUID simplifica o desenvolvimento Orientado a Objetos e também o relacionamento entre as Tabelas do Sistema.

Essa é a nova velha opção que a maioria dos desenvolvedores (atuais) não utilizam em seus Modelos de Dados: 

*Utilizar o UUID como identificador de registros.*

Você entenderá agora porque o UUID é tão importante para o desenvolvimento Orientados a Objetos e como ele irá simplificar seu código e *excluir* tabelas em seu modelo. 

Vamos continuar com o mesmo exemplo básico acima, mas reescrito para demostrar todo o conceito.

Com UUID eu consigo implementar o mesmo exemplo dessa forma:

                    FATURAS (pessoa_uid)
                       ^
            -----------|-----------
            |                     |
      PESSOA_FISICA (uid)   PESSOA_JURIDICA (uid)

Não há "tipos" e há apenas 1 campo de identificação de registro por tabela. Para todas e quaisquer tabelas, na verdade.

Como isso é possível?

Todo UUID é **único** em toda a base de dados. Não importa a tabela ou contexto no qual o UUID existe. Ele é único.

Ok, mas como você sabe qual tabela de Pessoa está relacionada com o valor do campo `pessoa_uid` na tabela de `FATURAS`, na linha 5043?

Você não sabe.

E a beleza disso é que você não precisa saber!

Essa é uma informação [encapsulada]({% post_url 2016-05-30-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-2 %}#encapsulamento) no Modelo de Dados.

Ainda confuso? 

Vou lhe mostrar então como obtemos todas as Faturas somente de Pessoas Físicas:

    SELECT 
      f.*
    FROM FATURAS f
    INNER JOIN PESSOA_FISICA p
      ON p.id = f.pessoa_id
      
E de Pessoas Jurídicas:

    SELECT 
      f.*
    FROM FATURAS f
    INNER JOIN PESSOA_JURIDICA p
      ON p.id = f.pessoa_id

A *query* só irá trazer os dados relacionados entre as tabelas no *JOIN*, todo o resto será ignorado. 

Nesse modelo os "tipos" são determinados pelas tabelas que fazem parte da *query* e não por campos "fantasmas" ou *strings* como "F" ou "J".

Se houver mais "tipos de clientes" no futuro, nada será alterado nas tabelas já existentes.

A performance será excelente, visto que haverá poucas tabelas a serem relacionadas.

Só há uma desvantagem considerável: Não há FK's reais entre as tabelas, apenas índices. Não seria possível o campo `pessoa_uid` de `FATURAS` ter uma restrição em mais de uma tabela Pessoa. Mas antes que você comece a "atirar pedras" em mim, dá uma olhada em como o pessoal do NoSQL (muito utilizado hoje em dia) mantém a integridade dos dados, Ok?

No entanto você poderá utilizar *triggers* para manter a integridade dos registros ou não.

Pensa bem: não atualizamos `ID's` ou `UID's`. O único problema seria deletarmos um registro da `PESSOA_FISICA`, por exemplo, sem deletar suas `FATURAS`, certo?

Mas porque você ainda está excluindo registros da sua base de dados?! Registros devem ser preservados como uma linha do tempo. Então, apenas desative-os, sem exclusões, sem problemas.

## Conclusão {#conclusao}

Não há modelo perfeito. Sempre haverá prós e contras. Mas é importante trabalharmos com Modelos mais próximos da realidade e dos requisitos do Cliente quando utilizamos Orientação a Objetos.

A utilização do UUID torna isso possível.

O UUID simplifica o modelo diminuindo a quantidade e relacionamentos de tabelas. E isso facilita a nomenclatura pois haverá menos tabelas a serem nomeadas. Facilita, também, a construção de *queries* com mais performance por utilizar menos tabelas.

O UUID proteje a identificação de registros na web, dificultando a vida de *bots* que escaneiam sites fazendo requisições *GET/POST* apenas incrementando o ID inicial.

Enfim, o UUID não é perfeito, mas é a solução mais *eficaz*, na minha opinião, para construírmos sistemas mais *simples* e *robustos*.

Até logo.