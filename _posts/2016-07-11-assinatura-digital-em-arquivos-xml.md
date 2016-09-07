---
layout: post
title: "Assinatura Digital em Arquivos XML"
date: 2016-07-11
description: Veja como assinar digitalmente arquivos XML utilizando Object Pascal.
summary: Veja como assinar digitalmente arquivos XML utilizando Object Pascal.
image: /images/photo-1430760903787-4d91bbf15384.jpg
categories: 
  - Pascal
tags:
  - digital signature
keywords:
  - digital signature
--- 

A Assinatura Digital é cada vez mais utilizada devido a segurança de integridade que ela proporciona.
Nesse artigo vou demostrar como Assinar Digitalmente um arquivo XML utilizando o *framework* ACBr.

<!--more-->

![Imagem]({{ page.image }})

##Introdução {#introducao}

A Assinatura Digital nos dá mais segurança no envio de arquivos entre servidores, utilizando WebServices e/ou Serviços REST.

No mundo Java e .NET tem-se muitos *frameworks* para trabalhar com Assinatura Digital... mas e em *Object Pascal*?

Eu conhecia o [Projeto ACBr](http://www.projetoacbr.com.br/) a muito tempo. Eu utilizei esse projeto, anos atrás, para fazer a integração de NF-e para um cliente que, na época, utilizava Delphi 6.

A integração foi feita em apenas 1 semana — conte com as madrugadas — e tudo funcionou perfeitamente.

Anos se passaram e eu não tive mais projetos que envolvessem NF-e e/ou Assinatura Digital. Pelo menos não que eu mesmo tivesse que implementá-los.

Este ano eu tenho um Novo Projeto.

Nesse projeto tenho que utilizar Assinatura Digital em arquivos XML. Não são Notas Fiscais Eletrônicas (NF-e). São arquivos XML que precisam ser assinados antes do envio e verificados na outra ponta. A segurança é levado muito a sério nesse projeto.

Lembrei do Projeto ACBr, é claro.

##Utilizando o ACBr {#utilizando-acbr}

O ACBr trabalha com dois tipos de Assinadores: CAPICOM (Microsoft) e OpenSSL (OpenSource e crossplataforma).

Meu cliente só utiliza Windows, então eu escolhi a opção mais óbvia.

O ACBr compila em Delphi e FreePascal perfeitamente.

Para instalar no Delphi é mais fácil. O projeto dispõe de um Instalador *next-next-finish*.

Para instalar no FreePascal eu não sei se é o mesmo instalador. O que fiz foi instalar os pacotes manualmente, pela IDE do Lazarus.

Então fiz um programa simples para assinar um XML e tentar validar o resultado em um dos WebServices do cliente.

Precisei criar o XML Template, que é o XML que tenho que enviar ao WebService:

    <?xml version="1.0" encoding="iso-8859-1"?>
    <Principal>
      <Solicitacao Id="123"><Parametros Codigo="Todos" /></Solicitacao>
      <Assinatura>
        <Signature xmlns="http://www.w3.org/2000/09/xmldsig#">
          <SignedInfo>
            <CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" />
            <SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1" />
            <Reference URI="#123">
              <Transforms>
                <Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature" />
                <Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/>
              </Transforms>
              <DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1" />
              <DigestValue></DigestValue>
            </Reference>
          </SignedInfo>
        <SignatureValue/>
        </Signature>
      </Assinatura>
    </Principal>

Veja que já existe os nós da assinatura (Signature), mas estão em branco ou incompletos. Esse é o template que o ACBr irá utilizar para realmente assinar e gerar um novo XML.

Então codifiquei um pequeno programa em Object Pascal para fazer os testes.

    procedure TMainForm.Assinar;
    var
      A: TDFeSSL;
      I: Integer;
    begin
      A := TDFeSSL.Create;
      A.SSLLib := libCapicom;
      A.SelecionarCertificado;
      with TStringList.Create do
      try
        // carrega o XML de template
        LoadFromFile('template.xml');
        // obtém o XML assinado no Text do StringList
        Text := '<?xml version="1.0" encoding="iso-8859-1"?>'
              + A.Assinar(Text, 'Assinatura', '');
        // salva o XML em disco
        SaveToFile('assinado.xml');
      finally
        Free;
      end;
      A.Free;
    end;

A tela para escolher o certificado — previamente instalado no computador — aparece e o usuário, que pode selecionar qual ele deseja.

O XML é assinado (*node* Assinatura) e tudo parece perfeito... mas o WebService do cliente não valida a Assinatura.

Algo estava errado.

#Solução {#solucao}

Após ajuda do pessoal do ACBr e amigos, chegamos a conclusão que os espaços em branco no Template eram o problema.

Modifiquei o projeto de teste, intruduzindo algumas linhas de código para retirar os espaços e quebras de linha. Funcionou.

    procedure TMainForm.Assinar;
    var
      A: TDFeSSL;
      S: AnsiString;
      I: Integer;
    begin
      A := TDFeSSL.Create;
      A.SSLLib := libCapicom;
      A.SelecionarCertificado;
      with TStringList.Create do
      try
        // carrega o XML de template
        LoadFromFile('template.xml');
        S := '';
        // retira dos os espaços em braco e quebras de linha
        for I := 0 to Count-1 do
        begin
          S := S + Trim(
            StringReplace(
              StringReplace(Strings[I], #13, '', [rfReplaceAll]),
              #10, '', [rfReplaceAll]
            )
          )
        end;
        // obtém o XML assinado no Text do StringList
        Text := '<?xml version="1.0" encoding="iso-8859-1"?>'
              + A.Assinar(S, 'Assinatura', '');
        // salva o XML em disco
        SaveToFile('assinado.xml');
      finally
        Free;
      end;
      A.Free;
    end;

##Conclusão {#conclusao}

O Projeto ACBr me ajudou bastante me poupando muito tempo de desenvolvimento.

Há alguns detalhes que não mencionei. Foram necessários pequenas modificações no código do ACBr devido a alguns *nodes* customizados que eu precisa fazer no XML mas que o ACBr original não suportava. Essas modificações foram enviadas aos programadores do ACBr.

É isso.

Agradeço a todos os desenvolvedores e amigos que ajudaram na solução.

Obrigado e até logo.
