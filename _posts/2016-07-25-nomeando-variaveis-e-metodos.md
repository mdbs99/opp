---
layout: post
title: "Nomeando Variáveis e Métodos"
date: 2016-07-25
description: A nomenclatura correta de Variáveis e Métodos simplifica o código.
summary: A nomenclatura correta de Variáveis e Métodos simplifica o código.
image: /images/photo-08208oju93773-4746gf33.jpg
categories: 
  - Pascal
tags:
  - naming
keywords:
  - naming
  - nomeando
  - variaveis
  - métodos
  - variable
  - method
--- 

A nomenclatura correta de Variáveis e Métodos simplifica e melhora o entendimento do código.

Basicamente há uma única Regra para nomear ambos: **Não utilize prefixos**.

<!--more-->

![Imagem]({{ page.image }})

##Introdução {#introducao}

A regra de [nomenclatura para Classes]({% post_url 2016-04-25-nomeando-classes %}) é sobre utilizar um prefixo (contexto) além do seu nome.

A primeira regra de [nomenclatura para Unidades]({% post_url 2016-07-18-nomeando-unidades %}) é sobre utilizar prefixos.

Ambas as regras falam sobre prefixos. O maior objetivo para os utilizarmos é evitar colisões de nomes. Unidades e Classes podem ser utilizadas globalmente em todo o projeto e por isso precisam ter nomes que minimizam ao máximo ambiguidades. 

No entanto, ao nomear Variáveis ou Métodos na programação [Orientada a Objetos]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}), você deve esquecer os prefixos.

Prefixos de qualquer tipo, esqueça-os.

##Nomeando Variáveis {#variaveis}

Acho que não preciso lembrá-lo que utilizar [variáveis globais]({% post_url 2016-05-16-singleton-e-um-anti-padrao %}#variaveis-globais) é um anti-padrão.

Então toda variável é, ou deveria ser, local.

Se variáveis são locais, não deveria haver colisões de nomes. Se não há colisões de nomes, por que utilizar prefixos?

Mas o problema — você deve estar pensando — é que há colisões de nomes de variáveis até mesmo dentro de um único método. Pode não ser exatamente uma colisão de nome onde o compilador não conseguir ir adiante. Talvez haja só uma confusão de nomes, daquelas que você olha o código e pensa: 

"Essa é uma variável-atributo, uma variável local ou argumento do método?

Acontece, certo?

Bem, continue lendo.

##Nomeando Métodos {#metodos}

Métodos de uma Classe definem a implementação de um [contrato]({% post_url 2016-01-18-interfaces-em-todo-lugar %}#interfaces-sao-contratos) de uma Interface.

Toda Classe deveria implementar apenas uma [única responsabilidade]({% post_url 2016-03-07-classes-devem-implementar-apenas-uma-responsabilidade %}#unica-responsabilidade). Se a Classe tem apenas uma única responsabilidade, dificilmente você irá utilizar prefixos nos métodos.

Veja nesse [exemplo de código]({% post_url 2016-03-07-classes-devem-implementar-apenas-uma-responsabilidade %}#implementacao-tradicional) os métodos `SaveXxx`. 

A palavra `Save` é um prefixo — nem sempre um nome composto será um prefixo, mas nesse caso ele é. 

Compare agora com a [versão]({% post_url 2016-03-07-classes-devem-implementar-apenas-uma-responsabilidade %}#implementacao-oo) Orientada a Objetos e verá que as Classes ficaram muito mais simples, utilizando nomes de métodos simples. Cada Classe só implementa apenas uma única responsabilidade. Por isso seus Métodos são simples e diretos.

Consegue ver a diferença?

##O que há de errado com Prefixos? {#o-que-esta-errado}

Os prefixos podem ser o mais diversos. 

Quando comecei a programar eu utilizava a [notação húngara](https://en.wikipedia.org/wiki/Hungarian_notation). Também já utilizei outros prefixos, dependendo da empresa/projeto. 

Meu último "padrão pessoal de prefixo" foi utilizar um "l" (L minúsculo) para qualquer variável local...

Nada disso era ou é necessário.

Utilizamos prefixos em Métodos e Variáveis com o mesmo objetivo de quando nomeamos Classes e Unidades: evitar a colisão de nomes.

É isso, não é?

Bem, se você está fazendo isso — como eu mesmo fiz durante muito tempo — eu tenho que lhe dizer: 

Seu código **não** tem um bom *design*. Ele está acoplado e não está modularizado o sufiente.

Se você está utilizando prefixos em Variáveis dentro de um método, quer dizer que seu Método está fazendo coisas demais. Você precisa identificar "quem é quem" dentro de um código possivelmente grande. Isso é errado. Refatore.

Alias, aqui vai uma **dica**: Refatore o Método que tiver utilizando mais de 5 variáveis.

Se você está utilizando prefixos em Métodos, sua Classe está fazendo coisas demais. Esses Métodos com prefixos estão "gritando" à você que eles pertencem a outra Classe e que não deveriam estar ali! Isso é muito mais errado. Refatore imediatamente.

Variáveis e Métodos devem ter o nome mais simples possível. Isso deveria ser um requisito de *design*, uma restrição. Toda restrição impõe limites, que leva a regras, que leva a uma melhor organização de todo o código.

##Argumentos dos Métodos {#argumentos-dos-metodos}

E para Argumentos dos Métodos, é necessário algum prefixo?

Você continua não precisando de prefixos, mas Argumentos podem ter nomes mais verbosos a fim de facilitar o uso do Método, sem que o programador tenha que ler alguma documentação.

A linguagem *Object Pascal* tem mais uma particularidade: Não é *case-sensitive*.

Por isso utilizamos prefixos como "T" para Classes, "I" para Interfaces, "P" para tipos ponteiro, etc. Nessa mesma linha de pensamento, alguns programadores utilizam o prefixo "A" para Argumentos.

Não vejo problema nessa prática. No entanto se você codificar Classes e Métodos pequenos, talvez não será necessário utilizar nenhum prefixo nos argumentos, pois haverá pouco código para existência de conflitos de nomes. 

##Conclusão {#conclusao}

Escolher bons nomes para Métodos e Variáveis é bem mais simples que escolher nomes para Unidades, Classes e Interfaces, mas ainda assim é uma arte.

Novamente lhe digo que não existe o **certo absoluto**. Talvez o padrão de nomenclatura que você já utiliza seja melhor pra você. Tudo bem. Mas o que tenho observado ao longo de alguns anos é que a minha "obrigação" de utilizar nomes mais simples deixa o código melhor. Não apenas por causa dos nomes em si, mas devido as Classes implementarem apena uma única responsabilidade, sendo os nomes uma espécie de métrica para verificar essa prática.

Após anos de desenvolvimento, utilizando dezenas de padrões de nomenclatura em diversas empresas e projetos distintos, cheguei a conclusão que o "não-padrão" — sem prefixos e sem burocracia — é o melhor. É como estar organizado, sem organizar. Faz sentido?

Até logo.
