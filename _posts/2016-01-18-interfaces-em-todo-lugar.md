---
layout: post
title: "Interfaces em Todo Lugar"
date: 2016-01-18
description: Todas as variáveis devem ser do tipo Interface
summary: Utilize Interfaces para definir o tipo de todas as Variáveis, Atributos e Argumentos de Métodos.
image: /images/photo-1452776145041-517a74be1f14.jpg
categories: oop
tags:
  - oop
keywords:
  - oop
  - object pascal
  - orientação a objetos
---

Variáveis não devem ser do tipo Classe.

Utilize Interfaces para definir o tipo de todas as Variáveis, Atributos e Argumentos de Métodos.

Por que?

<!--more-->

![Pensando](/images/photo-1452776145041-517a74be1f14.jpg)

##Interfaces são Contratos {#interfaces-sao-contratos}

Vou contar uma história. 

Carlos é um empreendedor. Ele tem um Café chamado "Instância do Café" :)

Seus negócios estão indo muito bem mas seus clientes estão ficando cada vez mais exigentes... ele
precisa contratar um barista mais experiente.

Ele paga por um anúncio para achar o melhor barista que ele puder pagar. Ele não sabe quem será o novo
contratado, ele só espera que o pretendente tenha as qualificações necessárias para fazer o trabalho que
lhe for designado.

Carlos não está procurando especificamente um João, Amanda ou um Xavier... suas exigências são apenas
sobre as qualificações de barista. Ele não quer saber se o novo pretendente será homem ou mulher. Não importa
a cor do cabelo e muito menos sobre seu gosto musical. Ele precisa de um profissional, e só.

O pretendente com as melhores qualificações terá um **contrato** com Carlos, seja ele/ela quem for.

Na Orientação a Objetos, a **Interface** é o contrato firmado entre dois ou mais Objetos.

##Interfaces definem o trabalho, não as Implementações

Assim como Carlos, você não deve ser "preconceituoso" ao definir trabalho para Classes específicas.

Defina o contrato do trabalho ou função utilizando Interfaces.

Toda classe que implementar esse contrato será uma pretendente em potencial.

Por isso que toda Variável deve ser do tipo Interface — essa é a resposta ao "Porque" no início do post.

Se você utilizar tipos de Classes em vez de tipos de Interfaces,
você estará restringindo quem fará o trabalho. Isso é preconceito. Seu sistema ficará dependende dessas Classes e 
não poderá deixá-las nunca mais. Não faça isso!

Exemplo. Eu utilizo [Synapse](http://synapse.ararat.cz/doku.php/download) para protocolos de rede como `HTTP`, `FTP`, etc. 
Mas meus sistemas não conhecem a implementação do Synapse, com exeção de um único módulo onde as classes específicas de Synapse são instânciadas.
Todos os outros módulos/classes só conhecem os contratos, ou seja, as Interfaces para os protocolos de rede, 
mas não se importam em saber quem irá executar o trabalho.
Isso é Polimorfismo. Se eu quiser mudar de Synapse para [lNet](https://lnet.wordpress.com/) ou [Indy](http://www.indyproject.org/index.en.aspx), 
poderia fazê-lo em apenas um lugar e tudo continuaria funcionando perfeitamente.

Eu tenho um projeto OpenSource codificado em FreePascal chamado [AWS Lib](https://github.com/mdbs99/AWS).
Ele é um *client* para o  serviço [Amazon S3](http://docs.aws.amazon.com/AmazonS3/latest/API/Welcome.html) que também utiliza Synapse
para as chamadas `HTTP`.
Este projeto mostra de forma bem simples o que eu quero dizer. Sugiro "passear" pelos fontes e descobrir você mesmo
onde é instanciado o(s) Objeto(s) do Synapse.  

A [AWS Lib](https://github.com/mdbs99/AWS) também mostra conceitos sobre **imutabilidade** que irei
explicar em futuros posts.

Argumentos e atributos também são variáveis. Então vamos nos referir aos três apenas como **instância** de agora em diante,
utilizando a nomenclatura correta para o paradigma Orientado a Objetos.

Utilizando Interfaces em todas as suas instâncias você ganha o benefício do **Polimorfismo** pois qualquer classe — mesmo
aquelas fora do seu sistema — poderão executar o trabalho de acordo com o contrato. Favorece baixo acoplamento
porque as classes não estarão conectadas entre si, mas sim por Interfaces. Outra vantagem é facilitar a codificação de testes
automatizados, pois podemos criar implementações *fake* para qualquer Interface... assunto para outro post.

---
**Dica:** A utilização de tipos primitivos como Integer, TDateTime ou `string` em variáveis sempre será em
função da performance em detrimento ao melhor design. Priorize as Interfaces. Só utilize tipos 
primitivos em métodos locais — você poderá alterar quando quiser sem complicações — ou em casos bem específicos onde o 
ganho de performance é muito maior do que o benefício do Polimorfismo ao utilizar Interfaces.  

---

