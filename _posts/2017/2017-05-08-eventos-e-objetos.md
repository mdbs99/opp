---
layout: post
title: Eventos e Objetos
date: 2017-05-08
permalink: /:title
description:
  Como utilizar Eventos dentro do paradigma da Orientação a Objetos.
image: /images/2017/photo-ashim-d-silva-95242.jpg
categories: 
  - Projeto
tags:
  - projeto
keywords:
  - freepascal
  - fpc
  - delphi
  - lazarus
  - object-oriented
  - oop
  - mdbs99
  - eventos
  - events
---

Quando o semáforo de trânsito muda de vermelho para verde, isso é considerado um Evento ou uma Mensagem entre Objetos (motorista e semáforo)?

<!--more-->

![Unsplash image]({{ page.image }})  

## Introdução {#introducao}

O mundo é constituído de Eventos.

Uma sirene que toca ao longe; o semáforo que passou de amarelo para vermelho; as notificaçõess do celular; a luz que acende num painel; o atendente do Starbucks chamando seu nome quando o café está pronto.

Um [Evento](https://en.wikipedia.org/wiki/Event_(computing)) no mundo computacional é uma ação ou ocorrência gerado no software e reconhecido e tratado pelo software.

Entre Objetos computacionais, existem 2 formas de interação: Mensagens e Eventos.

## Mensagens {#mensagens}

Dois Objetos comunicam-se entre si através de [mensagens]({% post_url 2016-11-14-diga-me-algo-sobre-voce %}#mensagens) ou Métodos.

Podemos chamar essa troca de mensagens de diálogo pois os Objetos que estão participando da "conversa" estão todos "cientes" do trabalho que estão desenvolvendo juntos (composição).

Cada Objeto está fazendo o trabalho para o qual foi [contrato]({% post_url 2016-01-18-interfaces-em-todo-lugar %}#interfaces-sao-contratos) e repassando seu resultado para o próximo Objeto afim de terminar um trabalho maior.

Para um Objeto enviar uma Mensagem à outro, ele deve saber como passar a mensagem na forma que o receptor já conhece, para que as instruções sejam entendidas.

Em outras palavras, o transmissor deve conhecer a Interface do receptor.

Mas num diálogo entre alguns Objetos, podem haver interrupções externas que quebra o fluxo normal, podendo mudar o comportamento em execução.

Imaginem dois Objetos trabalhando, fazendo um *parser* de arquivos PDF, o usuário clica no botão "cancelar" para notificar, a quem quiser saber, que a operação em andamento deve ser cancelada.

Isso é um Evento.

## Eventos {#eventos}

Um Evento, diferente de um diálogo, é como um interruptor. Ele desvia o "fluxo normal" do diálogo entre Objetos para notificar outros Objetos — que ele desconhece ou ignora — que podem ou não estar inseridos no mesmo contexto ou trabalho em andamento.

Por exemplo:
Dois mecânicos estão fazendo um belo trabalho na montagem de um motor recem retificado, a sirene sinalizando a hora do almoço toca. Eles largam suas ferramentas e vão almoçar.

Podemos dizer que eles estavam *aguardando* esse Evento? Sim. Deviam estar anciosos para o almoço...

Então todo evento é *aguardado* pelos Objetos?

Creio que sim. Pelo menos no mundo computacional.

Os Objetos em sistemas computacionais não tem *sentidos* como os seres humanos. Os 5 sentidos humanos são como interpretadores de todo tipo de Evento que pode ocorrer ao seu redor. Então, sim, aguardamos por qualquer tipo de Evento que possa ocorrer, pois temos a capacidade de interpretá-los e tomar uma decisão sobre o que fazer.

Já os Objetos dentro de um software são apenas abstrações de [entidades]({% post_url 2016-02-29-objetos-representam-entidades %}) do mundo real. Sendo abstrações, eles são construídos de forma minimalista — ou deveriam ser — para representar apenas uma faceta da entidade na qual eles representam. Então temos que implementar apenas os Eventos essenciais que essa abstração deveria aguardar. Caso ocorram.

Um evento é uma notificação que um Objeto faz a quem estiver interessado. O Objeto (emissor) não sabe a quem está enviando a notificação.

Em outras palavras, é o receptor que deve ter conhecimento sobre como será emitido o evento no transmissor, ou seja, sua Interface.

## Implementação {#implementacao} 

A implementação de um Evento, em qualquer linguagem de programação, pode ser definida basicamente como um *ponteiro para uma função*.

Essa foi a maneira que o Delphi implementou os Eventos dos *widgets* de um Formulário.

Um clique num botão, por exemplo, é um ponteiro para uma função que está definida fora da instância do botão (Classe TButton e/ou derivados).

A implementação dessa função normalmente está localizada no Formulário onde o botão está inserido.

    procedure TForm1.Button1Click(Sender: TObject);
    begin
      ShowMessage('Clicked');
    end;

No código acima `Button1Click` é um método da Classe `TForm1`.

No *design* do Formulário (arquivo *.dfm ou *.lfm) haverá um código como abaixo:

    object Button1: TButton
      Left = 102
      Height = 25
      Top = 72
      Width = 75
      Caption = 'Button1'
      OnClick = Button1Click
      TabOrder = 0
    end

Em *runtime* o compilador irá configurar o evento `OnClick` — uma propriedade do tipo `TNotifyEvent` — da instância `Button1` para utilizar a "função" `Button1Click`.

Essa é a "maneira clássica" de utilizarmos Eventos.

É uma maneira *simples* e *eficaz*, porém primitiva — mas eu não estou dizendo isso de maneira perjorativa.

> Primitivo: que é o primeiro a existir; que coincide com a origem de algo; inicial, primevo, original.

Vamos ver outra maneira de utilizarmos Eventos, apenas com Classes sem a necessidade de propriedades com [*Getters/Setters*]({% post_url 2016-06-27-getters-e-setters %}).

Vou utilizar esse exemplo de Classe Log [aqui]({% post_url 2016-03-07-classes-devem-implementar-apenas-uma-responsabilidade %}#implementacao-oo) como base:

<script src="https://gist.github.com/mdbs99/c5da3c1d04894b7ee949aa1f7735aade.js"></script>

As mensagens desse programa serão:

1. "log start"
2. "log finish"
3. "clicked"

Temos algumas diferenças entre o primeiro exemplo (Button1) e este último (Log).

Ao invés da Classe `TLogInMemory` possuir 2 propriedades do tipo `TLogNotifyEvent` para implementar os eventos, ela recebe os "ponteiros" no construtor e isso simplifica muita coisa.

*Primeiro*, não precisamos definir as propriedades na Interface `ILog`, mantendo-a limpa. Caso essas propriedades fossem definidas na Interface, obrigatoriamente teríamos que definir 3 linhas para cada propriedade:

    property OnStart: TLogNotifyEvent 
                 read GetOnStart 
                write SetOnStart;

1. A propriedade em si
2. O método GetOnStart
3. O método SetOnStart

Como Interfaces não possuem atributos, os Métodos de leitura e escrita teríam que ser definidos na Interface e implementados em todas as Classes.

Bem *verboso*, não?

*Segundo*, não precisamos definir nenhuma propridade em `TLogInMemory` mesmo que não houvessem os *Getters/Setters*.

*Terceiro*, uma vez definido os eventos, não podemos modificá-los deixando a Classe [imutável]({% post_url 2016-07-04-objetos-imutaveis %}).

Simples e elegante.
 
## Conclusão {#conclusao}

Os Eventos fazem parte de qualquer sistema. Podemos tirar proveito dessa técnica primária para deixar nossos Objetos com comportamento mais dinâmico.

O acoplamento entre os Objetos é reduzido. O transmissor do Evento não sabe quem irá recebê-los, o que é ótimo.

Essa é a forma mais básica e limpa para utilizarmos Eventos no *Object Pascal*, mas não é a única.

E se quisermos que mais de um Objeto seja notificado? 

Não seria melhor utilizarmos Objetos ao invés de simples ponteiros para função?

Bem, essas são perguntas que poderão ser respondidas em outro artigo. 

Até logo.