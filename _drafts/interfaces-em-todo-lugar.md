---
layout: post
title: "Interfaces em todo lugar"
date: 2016-01-16
categories: oop
description:
image: /images/photo-1452776145041-517a74be1f14.jpg
summary:
tags:
  - oop
  - object pascal
keywords:
  - oop
  - object pascal
  - orientação a objetos
---

Nunca defina o tipo de uma variável como uma Classe.

Utilize Interfaces para definir o tipo de todas as Variáveis, Atributos e Argumentos de Métodos.

Porque?

<!--more-->

![Pensando](/images/photo-1452776145041-517a74be1f14.jpg)

##Interfaces são Contratos

Carlos é um empreendedor. Ele tem um Café chamado "Instância do Café" :)

Seus negócios estão indo muito bem mas seus clientes estão ficando cada vez mais exigentes... ele
precisa contratar um barista mais experiente.

Ele paga por um anúncio para achar o melhor barista que ele puder pagar. Ele não sabe quem será o novo
contratado, ele só espera que o pretendente tenha as qualificações necessárias para fazer o trabalho que
lhe for designado.

Carlos não está procurando especificamente um João, Amanda ou um Xavier... suas exigências são apenas
sobre as qualificações de barista. Ele não quer saber se o novo pretendente será homem ou mulher. Não importa
a cor do cabelo e muito menos sobre seu gosto musical. Ele precisa de um profissional barista, e só.

O pretendente que conseguir o cargo terá um **contrato** com Carlos.

O contrato, na Orientação a Objetos, é a **Interface** entre Carlos e o pretendente.

##Interfaces definem o trabalho, não as Implementações

Assim como Carlos, você não deve ser "preconceituoso" ao definir trabalho para Classes específicas.

Defina o contrato do trabalho ou função utilizando Interfaces.

Toda classe que implementar esse contrato será uma pretendente em potencial.

Por isso toda Variável deve ser do tipo Interface. Se você utilizar tipos de Classes em vez de tipos de Interfaces,
você estará restringindo quem fará o trabalho. Isso é preconceito. Seu sistema ficará dependende dessas Classes e 
não poderá deixá-las nunca mais. Não faça isso!

Exemplo. Eu utilizo [Synapse](http://synapse.ararat.cz/doku.php/download) para protocolos de rede como `HTTP`, `FTP`, etc. 
Mas meus sistemas não conhecem a implementação do Synapse, com exeção de um único módulo onde as classes específicas de Synapse são instânciadas.
Todos os outros módulos/classes só conhecem os contratos, ou seja, as Interfaces para os protocolos de rede, 
mas não se importam em saber quem irá executar o trabalho.
Isso é Polimorfismo. Se eu quiser mudar de Synapse para [lNet](https://lnet.wordpress.com/) ou [Indy](http://www.indyproject.org/index.en.aspx), 
 poderia fazê-lo em apenas um lugar e tudo continuariafuncionando perfeitamente.

Eu tenho um projeto OpenSource codificado em FreePascal chamado [AWS Lib](https://github.com/mdbs99/AWS).
Ele é um `client` para o  serviço [Amazon S3](http://docs.aws.amazon.com/AmazonS3/latest/API/Welcome.html).
Este projeto mostra de forma bem simples o que eu quero dizer. Sugiro "passear" pelos fontes e descobrir você mesmo.  

---
**Dica:** [AWS Lib](https://github.com/mdbs99/AWS) também mostra conceitos sobre **imutabilidade** que irei
explicar melhor em futuros posts.

---

Argumentos e atributos também são variáveis. Então vamos nos referir aos três apenas como **instância** de agora em diante,
utilizando a nomenclatura correta para o paradigma Orientado a Objetos.

Utilizando Interfaces em todas as suas instâncias você ganha o benefício do **Polimorfismo** pois qualquer classe — mesmo
aquelas fora do seu sistema — poderão executar o trabalho de acordo com o contrato. Favorece baixo acoplamento
porque as classes não estarão conectadas entre si, mas sim por Interfaces. Outra vantagem é a codificação de testes
automatizados, pois podemos criar implementações `fake` para qualquer contrato... assunto para outro post.

---
**Dica:** A utilização de tipos primitivos como Integer, TDateTime ou `string` em variáveis sempre será em
função da performance em detrimento ao melhor design. Priorize as Interfaces. Só utilize tipos 
primitivos em métodos locais — você poderá alterar quando quiser sem complicações — ou em casos bem específicos onde o 
ganho de performace é muito maior do que o benefício do Polimorfismo ao utilizar Interfaces.  

---

