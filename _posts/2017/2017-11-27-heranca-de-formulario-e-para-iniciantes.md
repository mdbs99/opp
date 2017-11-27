---
layout: post
title: "Herança de Formulário é para Iniciantes"
date: 2017-11-27
permalink: /:title
description:
  Depois que você aprendeu como funciona a Herança de Formulários, pode esquecê-la.
image: /images/2017/photo-ken-treloar-346065.jpg
tags:
  - OOP
keywords:
  - freepascal
  - fpc
  - delphi
  - lazarus
  - object-oriented
  - oop
  - mdbs99
  - herança
  - inheritance
  - form
  - formulário
---

Depois que você aprendeu como funciona a Herança de Formulários, pode esquecê-la.

<!--more-->

![Unsplash image]({{ page.image }})

<span style="font-family: 'Bebas Neue'; font-size: 0.7em;">Photo by Ken Treloar on Unsplash</span>

## Introdução {#introducao}

No início da minha carreira, a muitos anos atrás, alguém me mostrou a Herança de Formulário no Delphi.

Mesmo em *design time*, dois Formulários herdados estavam "sincronizados".

Ao alterar um componente de lugar (*Top* / *Left*) no Formulário 1, automaticamente também era alterado no outro Formulário 2 devido a Herança entre eles.

E na época eu pensei: UAU!

Como era rápido fazer um Formulário (simplório) de CRUD. Basta herdar e setar algumas propriedades e sobrescrever alguns Métodos.

Acredito que essa é uma das *features* mais utilizadas hoje em dia.

Mas, o que tem de errado com isso?

## Problemas {#problemas}

Utilizar Herança de Formulário é bom apenas para os *iniciantes* na programação Orientada a Objetos e para o uso *superficial* do Delphi ou Lazarus.

Pode ser considerado "Ok" para criar aplicações de exemplo, ensinar sobre <del>como não utilizar</del> a Herança simplificada e... talvez, apenas isso.

Para aplicações realmente importantes, sugiro esquecer a Herança de Formulário por completo.

Enquanto a [Herança de Classes é um mal]({% post_url 2016-05-23-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-1 %}) que pode ser gerenciado ou mesmo utilizado de forma mais [apropriada]({% post_url 2017-03-06-como-utilizar-heranca-apropriadamente %}), a Herança de Formulário acaba com todos os conceitos da verdadeira [Orientação a Objetos]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}).

**O [encapsulamento]({% post_url 2016-05-30-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-2 %}#encapsulamento) é perdido** quando utilizamos Herança de Formulários, pois o desenvolvedor precisa ter conhecimento do comportamento interno do formulário-pai afim de conseguir fazer algo de útil no formulário-filho.

Muitas vezes encontramos formulários contendo apenas Métodos protegidos ou [privados]({% post_url 2017-01-30-metodos-privados %}) sem nenhum comportamento público; eles vem do formulário-pai. E isso é muito estranho. Por quê teríamos uma Classe (Form) que, a primeira vista, não tem nenhum comportamento para o mundo externo?

**A ordem de execução dos Métodos protegidos** que devem ser sobrescritos, devem ser de conhecimento do desenvolvedor, mais uma vez quebrando o encapsulamento.

É necessário visualizar o código do formulário-pai afim de saber onde e em que momento tais métodos abstratos e protegidos são utilizados.

É comum ver métodos sobrescritos que não chamam o Método herdado, "matando" o comportamento padrão da Classe ancestral quando o Método não é abstrato.

Pedaços de código serão duplicados pois estão em métodos não acessíveis nos formulários-filhos.

Uma bagunça.

**Componentes herdados não podem ser excluídos**, o que resulta em mais bagunça e "código espaguete".

Como não é possível excluir componentes em formulários-filho, o desenvolvedor tem que habilitar/desabilitar ou torná-los visível/não-visível. No entanto tais componentes continuarão a serem criados no formulário...

Criar formulários com componentes que não serão utilizados é realmente um desperdício de memória RAM.

**Qualquer alteração no formulário-pai irá refletir nos formulários-filhos**, e isso quer dizer que você pode quebrar o sistema inteiro com pequenas modificações.

Você poderia argumentar que mesmo se não utilizasse Herança, mas sim Composição de Objetos, um Objeto que fosse reaproveitado por todo o sistema também poderia quebrá-lo caso algo fosse indevidamente alterado no seu código.

Correto. Mas a grande diferença é que é muito mais fácil alterar um código de apenas uma Classe [pequena]({% post_url 2016-03-07-classes-devem-implementar-apenas-uma-responsabilidade %}) e sem dependências externas ou heranças de Classes-filhas, do que um código de Formulário onde toda lógica está contido nele além de ter a preocupação se alguma alteração irá impactar negativamente nos nos formulários-filhos.

## Soluções {#solucoes}

Vejamos então algumas opções para a completa substituição da Herança de Formulário, utilizando outras técnicas mais Orientadas a Objetos, [simples]({% post_url 2016-12-19-simplicidade %}) e limpas.

**A Composição e [Decoração]({% post_url 2016-05-02-decorator-pattern %}) de Objetos** são as opções mais simples e corretas para substituir praticamente qualquer tipo de Herança.

Você pode criar Classes intermediárias ou [adaptadoras]({% post_url 2016-11-21-classes-adaptadoras %}) que se comunicam entre si para instanciar Formulários complexos.

Com o uso de pequenas Classes utilizadas como "interface de acesso" aos Formulários, você consegue instanciá-los, desacoplá-los entre si, instanciar *Frames*, ler e escrever dados em componentes. Tudo em *runtime*.

**O uso de *Frames* é uma ótima opção** para o uso de composição visual, visto que são mini-formulários embutidos em outros Formulários.

Pense em como são construídas as páginas de um website. Não há herança, apenas *includes* de "pedaços de código": *Header*, *Footer*, *JavaScript*, etc.

Você pode utilizar o mesmo pensamento para o uso de *Frames*. Pode criar um *Frame* apenas com os botões de CRUD; outro com *Grids*; outro para o *footer* de todo formulário.

Eu utilizo *Frames* até mesmo sem pensar na reutilização...

Explico. Imagine um Formulário com algumas abas utilizando um [TPageControl](http://wiki.lazarus.freepascal.org/TPageControl).

Bem, não podemos ter nomes de componentes iguais num único Formulário, ou seja, se você definiu que um `TEdit` se chama `NameEdit` na aba 1, você não pode nomear outro `TEdit` com o mesmo nome na aba 2, pois ambos pertencem ao mesmo Formulário.

Você precisa acrescentar mais prefixos/sufixos na [nomenclatura]({% post_url 2016-07-25-nomeando-variaveis-e-metodos %}) e isso torna o código mais verboso e confuso, além do fato de que seu Formulário terá que lidar com todos os componentes de uma só vez.

Em meus projetos, uma aba pode conter a instância de um *Frame*, caso haja ações diferentes em cada aba.

Por exemplo, se eu precisar adicionar uma barra de botões (ações) na aba 2, então essa aba será um *Frame* com sua própria implementação, componentes de nomenclatura simples e ações específicas.

Dentro de um *Frame* eu não preciso me preocupar em lidar com um único código inchado que lida com todos os *widgets* dentro de um Formulário.

E se um *Frame* precisar notificar o Formulário principal, isso pode ser feito através de [Eventos]({% post_url 2017-05-08-eventos-e-objetos %}).

**Criar Componentes no Delphi/Lazarus é tão fácil** que se você precisar utilizar muito um comportamento, pode ser uma boa ideia criar um novo componente e disponibilizá-lo na IDE.

Já vi muito isso em empresas, onde existem componentes genéricos que podem ser utilizados por qualquer sistema, mas também componentes específicos por projeto.

Entretanto, apesar de manter um padrão de uso de componentes por toda a equipe, pode ser difícil manter todas as IDE's do Delphi atualizadas após um novo *release* desses componentes — no Lazarus essa tarefa seria mais fácil devido sua "não-instalação" e componentes "registrados" em apenas 1 arquivo XML, no entanto.

## Conclusão {#conclusao}

O objetivo do artigo não é depreciar desenvolvedores que ainda utilizam Herança de Formulários, mas sim apresentar técnicas muito mais eficazes de construção, visando a manutenibilidade do código no longo prazo.

Eu mesmo ainda trabalho em alguns projetos totalmente baseado em Herança de Formulários. Nesses projetos eu sou *obrigado* a utilizar herança pois todo o *framework* funciona a partir de alguns formulários base. No entanto, esses projetos estão longe de ter um código ideal.

Mantenha seus projetos atuais, mas repense o uso de Herança de Formulários em projetos futuros.

Até logo.

