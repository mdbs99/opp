---
layout: post
title: "Unidades Limítrofes"
date: 2016-10-24
description:
  Unidades Limítrofes promovem o desacoplamento entre Componentes e Libraries de terceiros.
summary: 
  Unidades Limítrofes promovem o desacoplamento entre componentes e Libraries de terceiros.
image: /images/photo-1465447142348-e9952c393450.jpg
categories: 
  - Object Pascal
tags:
  - language
  - units
keywords:
  - unidades limítrofes
  - units
--- 

A maioria dos sistemas utilizam componentes ou *libraries*
de terceiros. Seria perda de tempo codificar cada peça de 
código específica e necessária ao total funcionamento do 
sistema, já que existem ótimas opções mundo afora, que já fazem 
o trabalho específico que queremos implementar.

Mas é prudente utilizar tais *peças de código* sem nos 
preocuparmos sobre a manutenibilidade, dependências e 
encapsulamento?

<!--more-->

![Unsplash image]({{ page.image }})

Utilizar grupos de *componentes* ou *libraries* de terceiros 
com funcionalidades diversas é ótimo, pois nos fazem ganhar 
tempo na codificação.

Vou me referir a ambos os grupos como **peças de código**.

Muitas peças já estão prontas para uso. Sejam elas *Open Source*
ou comercial, não importa.

Cada uma dessas peças foram projetadas e codificadas utilizando
estilos e paradigmas diferentes. Cada projeto utilizou um *caminho*
diferente para implementar e resolver o problema que motivou o
desenvolvimento da peça.

Não há um único caminho certo a percorrer, porém uns são menos 
difíceis que outros.

Seu projeto tem seu próprio caminho e *estilo* — ou o estilo da
empresa na qual você trabalha. Isso *precisa* ser respeitado.

Todas as peças intercambiáveis no seu projeto, que são utilizadas em 
muitas camadas comuns, devem seguir o mesmo estilo de codificação.

Esse artigo irá ressaltar os *cuidados* que devemos tomar ao utilizarmos
peças que não foram "feitas em casa" dentro dos nossos projetos.

## Encapsulamento {#encapsulamento}
Um dos pilares da Orientação a Objetos é o
[Encapsulamento]({% post_url 2016-05-30-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-2%}#encapsulamento).

Não estou me referindo apenas ao Encapsulamento que as Classes podem 
promover, mas sim a qualquer tipo de *cápsula*.

Uma Função ou Método encapsula variáveis.

Classes encasulam Estado e Comportamento.

Unidades (módulos) encapsulam um conjunto de Classes que 
implementam as [Regras de Negócio]({% post_url 2017-01-09-codigo-duplicado-talvez-nao %}#regras-de-negocio) do nosso sistema.

Unidades também podem encapsular todo um conjunto de peças de 
terceiros, para facilitar ou *adaptar* o uso das mesmas dentro 
do nosso projeto.

Essas Unidades são chamadas de *Unidades Limítrofes*.

Exemplo. Imagine um sistema para controlar um Hotel. Eu não conheço
tais Regras de Negócio, mas você pode imaginar cadastros, controle de
preços, quartos vagos, quartos ocupados e muitas outras.

Nesse mesmo sistema, no entanto, você necessita de uma conexão HTTP, para
enviar arquivos, baixar uma nova versão do próprio sistema, atualizar
dados, etc.

O protocolo HTTP não faz parte das Regras de Negócio, concorda?

Não precisamos desenvolver Classes para fazer essa comunicação. Já temos
isso pronto na Internet, sendo a maioria *Open Source*.

Ótimo, vamos baixar um componente ou *library* e pronto!

Aqui eu alerto para um problema: **Dependência**.

Quando você adiciona um código de terceiro ao seu projeto, automaticamente
você fica *dependende* desse código.

## Componentes de terceiros {#componentes-de-terceiros}

Existem algumas ótimas opções de implementação do protocolo HTTP 
em Object Pascal. Mas, mesmo que a opção seja ótima e muitos usuários
já utilizam, não quer dizer que o componente terá um ótimo suporte, 
que sempre estará sendo atualizado ou que o mantenedor nunca irá desistir
do projeto.

Pelo contrário. Você já deve ter visto componentes "morrerem" porque
o mantenedor não quer mais atualizar ou porque não faz mais sentido
em continuar o projeto, caso tenha outras opções melhores.

E aí, o que fazer?

Se o seu código utiliza tal componente em inúmeros lugares, você está 
com um grande problema nas mãos.

Você não encapsulou essa tecnologia (HTTP) e agora há Unidades de
terceiros por todo o seu projeto, utilizando dezenas de Classes,
constantes e variáveis.

Se você, no entanto, utilizasse apropriadamente as **Unidades Limítrofes**,
encapsulando tudo relacionado a HTTP em Módulos a parte, não teria esse 
tipo de problema.

## Unidades Limítrofes {#unidades-limitrofes}

Límitrofe [significa](https://pt.wiktionary.org/wiki/lim%C3%ADtrofe)
"*que se situa ou que vive nos limites de uma extensão, 
de uma região etc.; que tem limites comuns*".

A fronteira entre dois sistemas ou módulos. O limite entre Contextos.

Sempre que possível, temos que encapsular *todas* as peças que
não tem relação *direta* com as Regras de Negócio do seu sistema.

A maneira de fazer isso é implementar novas Unidades no seu projeto
apenas para adaptar as peças de terceiros que serão utilizadas.

Há duas opções quando você for implementar essas novas Unidades. Essas
opções também podem trabalhar em conjunto. São elas:

  1. Redefinição de tipos
  2. Definição de novas Interfaces e Classes
  
## Redefinição de Tipos {#redefinicao-de-tipos}

Redefinir ou renomear um tipo é algo trivial no Object Pascal.

Imagine que você baixou um componente para comunicação HTTP. 
A Unidade que contém a Classe que você quer utilizar chama-se
`FastHTTP.pas` e a Classe foi implementada como `TftHttpClient`.

Queremos encapsular essa Classe numa Unidade Limítrofe, para que 
nosso sistema só enxergue a nossa Unidade, que daremos o nome de `AcmeWebHttp.pas`, 
e a nova Classe terá o nome `TWebHttp`.

Então temos:

    unit AcmeWebHttp;
    
    interface
    
    uses
      FastHTTP;
      
    type
      TWebHttp = FastHTTP.TftHttpClient;
      
    implementation
    
    end;

Agora `TWebHttp` é apenas um atalho — *alias* — para a verdadeira
Classe que implementa o protocolo HTTP.
    
Todo o restante do sistema irá utilizar apenas `TWebHttp` como se
essa Classe fosse a responsável por fazer o trabalho.

Esse é um encapsulamento **simples**. Ele só deve ser utilizado para 
coisas simples, pontuais, que não são utilizados em muitas partes do 
sistema.

Essa técnica também pode ser utilizada para "concentrar" Classes e
constantes numa mesma Unidade. Mas, por algum motivo, não funciona
com tipos enumerados.

## Definição de novas Interfaces e Classes {#novas-definicoes}

Para componentes mais complexos ou que são utilizados por todo o
sistema, o correto é implementarmos um encapsulamento através 
de [Interfaces]({% post_url 2016-01-18-interfaces-em-todo-lugar %}).

Componentes costumam ter *inúmeros* Métodos e Propriedades mas, 
na maioria das vezes, você não necessita de todas essas opções.

Implemente Interfaces apenas com os Métodos que fazem sentido
dentro do seu projeto e crie uma ou mais Classes que irão implementar
essa Interface.

Pelo menos uma dessas novas Classes terá encapsulado
o componente real, ou seja, a Classe `TftHttpClient`.

Exemplo. Se você só necessita de um Método para apontar para uma
URL e retornar seu conteúdo, você poderia implementar:

    unit AcmeWebHttp;
    
    interface
    
    uses
      FastHTTP,
      AcmeData;
      
    type
      IWebHttp = interface
        function Get(const URL: string): IDataStream;
      end;
      
      TWebHttp = class sealed(TInterfacedObject, IWebHttp)
      private
        FOrigin: TftHttpClient;
      public
        function Get(const URL: string): IDataStream;
      end;    

Por todo o sistema seu uso será simples, utilizando somente a Interface
`IWebHttp` que, nesse exemplo, só tem um único Método.

Essa é a forma que eu utilizo e que *recomendo* que você faça.
  
## Conclusão {#conclusao}

A utilização de Unidades Limítrofes nos dá a capacidade de substituir
*todas* as peças de terceiros em nosso sistema de forma simples, num *único
lugar* e que irá refletir globalmente em todo o código. Isso facilita a
*manutenibilidade* do código.

Outro ganho com essa técnica é que mantemos o mesmo estilo de nomenclatura
de Classes e Unidades por todo o sistema, visto que podemos renomear ou
redefinir qualquer peça externa ao projeto.

Não é algo difícil de se fazer. Dá trabalho, mas os *benefícios* como diminuição
de dependências externas, encapsulamento e estilo de codificação, valem 
muito a pena no longo prazo.

Até logo.
