---
layout: post
title: "Diga me algo Sobre você"
date: 2016-11-14
description:
  Envie uma mensagem a um Objeto para que ele lhe diga ou não os
  dados que ele está encapsulando.
image: /images/photo-1453738773917-9c3eff1db985.jpg
categories: 
  - Objetos
tags:
  - objetos
  - about
keywords:
  - data
  - about
  - object data
--- 

Quando aprendemos sobre Orientação a Objetos, anos atrás,
ouvimos dizer que Objetos enviam *mensagens* uns aos outros.
No entanto o que realmente vemos na maioria dos códigos de 
hoje não são verdadeiros Objetos, são apenas estrutura 
de dados com funções...

Esquecemos o que significa enviar uma mensagem a um Objeto?

<!--more-->

![Unsplash image]({{ page.image }})

## Indrodução {#introducao}

Fazendo a análise de um novo sistema, o arquiteto vê a 
necessidade de implementar uma Classe para representar um 
*Cliente*.

O analista de negócio diz que um Cliente precisa ter 
*Login*, Senha e seu Nome.

Você começa a implementação da Classe.

Após 2 horas o analista retorna e diz
que também precisa ter mais "alguns campos". Então ele 
também acrescenta Endereço de correspondência; a Data de
Nascimento para dar descontos; separa o nome em 2 campos
(Nome e Sobrenome); e o Cartão de Crédito que
será a forma de pagamento ao comprar os produtos da empresa.

Como implementamos a nova Classe `TCustomer` que irá representar
um Cliente?

Aqui está uma sugestão de implementação bastante utilizada
pelos desenvolvedores:

    type
      TCustomer = class
      private
        FLogin: string;
        FPassword: string;
        FFirstName: string;
        FLastName: string;
        FAddress: string;
        FBirthday: TDateTime;
        FCreditCard: string;
      public
        constructor Create;
        property Login: string read FLogin write FLogin;
        property Password: string read FPassword write FPassword;
        property FirstName: string read FFirstName write FFirstName;
        property LastName: string read FLastName write FLastName;
        property Address: string read FAddress write FAddress;
        property Birthday: TDateTime read FBirthday write FBirthday;
        property CreditCard: string read FCreditCard write FCreditCard;
      end;

Utilizei *properties*. No *Object Pascal*, assim como no C# fica até 
bonito e escrevemos menos. No Java seria o dobro de linhas, pois
teríamos 7 *Getters* e 7 *Setters* para cada atributo.

Bem, tudo certo nessa implementação?

Não.

Infelizmente essa Classe não tem nada a ver com o real uso da 
[Orientação a Objetos]({% post_url 2015-12-29-o-que-e-orientacao-a-objetos %}).
Essa Classe não está representando um Cliente, mas sim uma
tabela do Banco de Dados.

Além disso, o uso de *properties* não elimina o fato que, na verdade, 
essa Classe só possui
[*Getters* e *Setters*]({% post_url 2016-06-27-getters-e-setters %}),
que é um anti-padrão.

## Mensagens {#mensagens}

Uma Classe deve representar uma
[Entidade]({% post_url 2016-02-29-objetos-representam-entidades %}),
*nunca* um registro de tabela no Banco de Dados.

Se você tem uma Classe com *Getters* e *Setters*, seu pensamento não
está na Orientação a Objetos. Você está pensando em
[Dados]({% post_url 2016-11-07-pensando-em-dados %}), em tabelas, em
persistência.

Você não deveria *pegar* e muito menos *alterar* um dado de um Objeto
diretamente!

Pense no mundo real. Quando você pergunta o nome de alguém,
ele/ela pode lhe responder de diversas maneiras:

  1. Diz apenas o primeiro nome;
  2. Diz apenas o sobrenome;
  3. Diz o nome completo;
  4. Diz o apelido;
  5. Inventa um nome;
  6. Não diz nada;
  
Veja que uma pergunta *simples* pode ter inúmeras respostas.
Tudo irá depender do [contexto]({% post_url 2016-10-03-delegacao-de-implementacao-de-interfaces %}#contextos).

Todo Objeto deve ser *respeitado*. Então, o máximo que fazemos é enviar
uma **mensagem** ao Objeto e aguardar uma resposta.

Vamos a um exemplo.

Compare os dois diálogos abaixo:

### Diálogo 1:

Carlos quer alugar um carro. Ele entra numa loja e vai falar com o vendedor.

    — Bom dia. Gostaria de alugar um carro. — disse Carlos.
    — Bom dia! Qual modelo o senhor está procurando?
    — Procuro um de menor custo, bem simples.
    — Ok. Temos esses 3 modelos...
    — Legal. Quanto custa o hatch?
    — Bem, o senhor está com sorte!
    — Estou? — disse Carlos
    — Sim, hoje é dia de promoção para esse modelo.
    O vendedor vai buscar um catálogo e diz o preço.
    — E vou lhe dar 20% de desconto!
    Carlos fica feliz com a escolha do modelo e 
    preço e resolve alugar.
    — Ok! Vou levar esse!
    — Certo. Preciso que o senhor preencha esse formulário.

### Diálogo 2:

Um *Registro* quer um carro.

    O Controlador pega uma lista de carros;
    O Controlador ordena a lista de forma crescente;
    O Controlador pega o Carro com o menor valor;
    O Controlador verifica a data atual e concede um desconto;
    O Controlador altera o Registro, apontando o carro;
    O Controlador pega os dados do Registro e salva 
    em outra tabela;

No primeiro diálogo temos duas pessoas (Objetos no software)
conversando.

No segundo... bem, na verdade não houve um segundo diálogo.
Havia apenas um Controlador (você) dando **ordens** e alterando 
dados. Pegando o que quer, onde quer e alterando dados sempre
que queria.

Para codificar o primeiro diálogo, precisaremos utilizar mensagens
entre os Objetos.

Mensagens também são Métodos. Tecnicamente essa é a maneira que temos
para adicionar comportamento a uma Classe.

Mas diferentemente de um Método `Get`, onde o dado é *tomado a força*
do Objeto, uma mensagem dá o controle ao Objeto para que ele retorne
as informações que ele desejar.

Repare a diferença entre as 2 últimas linhas dos diálogos:

  1. "Preciso que o senhor preencha esse formulário"
  2. "O Controlador pega os dados do Registro e salva 
  em outra tabela"

Na mensagem #1 o vendedor solicita o preenchimento de um formulário
com os dados pessoais de Carlos.

No procedimento #2 o Controlador apenas pega os dados do Registro.

O correto é utilizarmos mensagens e dar o controle da resposta ao
Objeto.

## O Método About {#metodo-about}

Todos os Objetos deveriam responder a uma mensagem padrão quando
outros Objetos quiserem saber sobre seus dados encapsulados.

Eu nomeei esse Método de `About`, ou seja, perguntamos ao Objeto
se ele tem **algo a dizer sobre si mesmo**.

O Método `About` é utilizado em todos os Objetos que preciam
informar ao mundo as informações que eles encapsulam.

Essa é uma alternativa viável para darmos o controle
de volta ao Objeto ao invés de tomar o dado dele.

O Objeto tem o total controle sobre quais dados retornar. Dessa
forma não há uma quebra de
[Encapsulamento]({% post_url 2016-05-30-heranca-pode-ser-o-mal-da-orientacao-a-objetos-parte-2 %}#encapsulamento) 

Objetos devem ser capaz de apresentar a si mesmo.
De dar informações "pessoais" que só eles tem conhecimento.

Essa ideia não é nova. No livro
[Object Thinking](https://www.amazon.com/dp/0735619654/?tag=stackoverfl08-20) o autor aborda esse assunto.

Existem outros artigos na Internet que falam sobre esse tema.
Dentre esses artigos tem [esse](http://www.yegor256.com/2016/04/05/printers-instead-of-getters.html)
em particular que capta bem essa ideia. No entanto a abordagem do autor
para implementar esse conceito é diferente da minha abordagem.

Então, o que retorna o Método `About`?

Retorna um *Stream* (`IDataStream`). E, basicamente é isso.

Para implementar o `About` precisamos definir a Interface:

    type
      IDataStream = interface
        function Save(Stream: TStream): IDataStream;
        function Save(Strings: TStrings): IDataStream;
        function Save(const FileName: string): IDataStream;
        function AsString: string;
        function Size: Int64;
      end;
      
Veja [aqui](http://objectpascalprogramming.com/posts/microservices-delphi-parte-1/#comment-2982613320)
a implementação da Classe que implementa `IDataStream`.

Aqui há uma grande diferença. Antes o Objeto era passivo. Ele
estava disponível para dar e receber dados através de seus Métodos,
o que é *extremamente* errado se estivermos utilizando Orientação a Objetos.

Agora, com a implementação do Método `About`, os dados do Objeto continuam encapsulados.
O Objeto não é mais passivo. Ele não é mais um "balde de dados". Ele está vivo
e tem seu próprio comportamento.

Se Objetos externos querem obter os dados de outrem, deverão enviar uma mensagem
solicitando-os. Cabe ao receptor responder com os dados que *ele* deseja
compartilhar.

## Refatorando {#refatorando}

Nos diálogos acima, Carlos e o "Registro" obtém seus dados da mesma
tabela, chamada CUSTOMER. Seus campos são os mesmos definidos
na primeira versão da Classe `TCustomer`.

O que vamos fazer é refatorar `TCustomer`, implementando seu Método
`About` ao invés de ter todos os seus dados **expostos** em *Getters* e *Setters*.

    type
      ICustomer = interface
        function About: IDataStream;
      end;
      
      TCustomer = class(TInterfacedObject, ICustomer)
      private
        FId: IDataGuid;
      public
        constructor Create(Id: IDataGuid);
        class function New(Id: IDataGuid): ICustomer;
        function About: IDataStream;
      end;
      
      implementation
      
      constructor TCustomer.Create(Id: IDataGuid);
      begin
        inherited Create;
        FId := Id;
      end;
      
      class function TCustomer.New(Id: IDataGuid): ICustomer;
      begin
        Result := Create(Id);
      end;
      
      function TCustomer.About: IDataStream;
      begin
        // 1. get record from Database using Id
        // 2. choose data
        // 3. convert to XML
        Result := TDataStream.New({XML});
      end;

Quando o vendedor diz *“Preciso que o senhor preencha esse formulário”*
na verdade é o Formulário — `TRentACar` — enviando uma mensagem ao Carlos:

    TRentACar.New(
      TCustomer.New(
        TDataGuid.New('{guid}') // Carlos' guid
      ).About
    ).Rent
    
O Formulário necessita de alguns dados como Nome Completo,
Endereço e Cartão de Crédito. Esses dados são passados
no construtor de `TRentACar`, provenientes da chamada
ao Método `About` de `TCustomer`.

Mas o retorno de `About` será parecido com isso:

    <customer>
      <Id>{guid}</Id>
      <FirstName>Carlos</FirstName>
      <LastName>Almeida</LastName>
      <Address>Rua 1...</Address>
    </customer>

Veja que *Login* e *Password* não foram retornados, assim como
a informação do *CreditCard*.

E aí temos um problema. Tudo bem que o Formulário não necessita
do *Login* e *Password* — essas informações serão salvas caso
o Cliente opte por fazer um registro online — mas o Cartão de Crédito
é necessário para concluir a operação.

O que fazer?

Temos 2 opções:

  1. Retornar todos os dados no XML;
  2. Criar Classes especialistas de `Customer`;
  
Nesse exemplo é mais fácil retornar todos os dados, é claro. Tudo
está numa única tabela, num único registro. Mas imagine um sistema
mais complexo onde um `Customer` poderia ter várias tabelas (outros 
endereços, registros financeiros, histórico de aluguel, tickets de
reclamações...). É possível retornar tudo, num único XML, porém não 
é performático. Exigiria a consulta em muitas tabelas.

Para esses casos, criamos Classes especialistas. Exemplo:

    TCustomerFinances = class(TInterfacedObject, ICustomer)
    
Essa Classe é especializada em Finanças. Ela pode retornar o Cartão
de Crédito e também todos os registros financeiros. O mais interessante,
no entanto, é que todas as Classes especialistas deverão implementar
`ICustomer`, pois todas representam a mesma Entidade, porém cada uma
na sua especialidade e em contextos diferentes.

Então, caso você opte por separar as responsabilidades, o registro
do aluguel do carro poderia ser feita dessa maneira:
    
    TRentACar.New(
      TCustomerFinances.New(
        TCustomer.New(
          TDataGuid.New('{guid}') // Carlos' guid
        )
      ).About
    ).Rent
    
A Classe `TCustomerFinances` deverá receber uma instância de
`TCustomer` no construtor.
Na chamada ao `About` ela irá chamar o `About` padrão, incluir os 
dados financeiros e retornar a resposta.

Isso é [decoração]({% post_url 2016-05-02-decorator-pattern %}) de Objetos.

Para a Classe `TRentACar` é transparente. Ela está conversando
com um único Objeto que retorna um `IDataStream` quando enviar uma
mensagem `About`.

## Conclusão {#conclusao}

Não podemos mais confundir Dados com Objetos.

Vamos parar de criar Objetos anêmicos que mapeiam tabelas no 
Banco de Dados. 

Devemos dar o controle aos Objetos. Eles sabem o que devem ou não
responder quando estão se comunicando.

Você é apenas o *diretor* do teatro. Você pode até fazer o roteiro
e escolher quem irá atuar em cada cena. Mas você *não* atua.

Quando os *atores* estiverem atuando, não tente controlá-los,
deixe-os trabalhar.

Até logo.