***
# TOTVS Developer Studio para Eclipse (BETA)
***

## Apresentação

O adicional **TOTVS Developer Studio**, é um conjunto de aplicações e ferramentas que interagem entre si, disponibilizando aos desenvolvedores na plataforma **Protheus**, um ambiente de desenvolvimento ágil, confortável e poderoso.

## Público

Desenvolvedores de produtos na plataforma **Protheus**, que utilizem uma ou mais das linguagens de programação suportadas pela plataforma.

* Adv/PL, Adv/PL-asp e suas variantes
* 4GL (em planejamento)

## Requisitos

### Conhecimento

* Sistema operacional adotado
* Conhecimentos da linguagem a ser utilizada
* Configuração e execução da plataforma **Protheus** (desejável)
* Ambiente Eclipse (desejável)

### Sistemas Operacionais

O **TDS** pode ser utilizado nos sistemas operacionais:

* Microsoft Windows
* Linux
* MAC/OS 

> Recomenda-se o uso de arquitetura 64.

### Plataforma Protheus

O **TDS** pode ser utilizado nas plataformas **Protheus** com suporte a:

* Adv/PL, versão 7.00.101202A ou superior, de acordo com o sistema operacional em uso
* 4GL, versão 7.00.101202A ou superior, de acordo com o sistema operacional em uso

> Verifique a lista de [Plataformas Homologadas](http://tdn.totvs.com/x/MQW-Ag) para servidores.
    
## Principais Funcionalidades e Características

* Comunicação baseada nos protocolos _LSP_ e _DAP_
* Sintaxe destacada
* Auto complemento
* Amostras de código
* Formatação de fontes
* Compilação de fontes, pastas e área de trabalho (_workspace_)
* Depuração de fontes (local e _webApp_).
* Geração e aplicação de Pacotes de Atualização
* Exclusão de fontes do _RPO_
* Desfragmentação do _RPO_
* Inspetor de objetos do _RPO_
* Inspetor de funções do _RPO_
* Geração de Serviços Web (_webservice_) Protheus
* Monitoramento de servidores
* Assistentes para geração de códigos
* Integração com o dicionário de dados
* E mais aquilo que você invertar e compartlhar com a comunidade

## Guia Ultra Rápido e Simplificado 

### Usuários

> A lista de servidores de área de trabalhos com versões anteriores a 11.4 não são compatíveis.
Processo de compatibilização planejado (chamados [#11](https://github.com/totvs/tds-eclipse/issues/11) e [#12](https://github.com/totvs/tds-eclipse/issues/12)).

> Use o instalador (baseado no Oomph) somente se tiver experiência no seu uso.

1. Obtenha e instale a plataforma [Eclipse](https://www.eclipse.org/downloads/packages/), dando preferência a sua versão mais recente.
2. Inicie o Eclipse e instale os adicionais **TDS** a partir do endereço do sítio de [distribuição do **TDS**](http://ds.totvs.com/updates/tds11.4). 
3. Inicie (ou abra) uma área de trabalho.
4. Registre um servidor **Protheus** na visão _Servidores_.
5. Conecte-se ao servidor.
6. Inicie (ou selecione) um projeto **TOTVS**.
7. Inicie (ou abra) um arquivo fonte.
8. Acione menu de contexto do editor e compile.
9. Inicie (ou selecione) um executor para depuração/execução.

### Colaborador

> Para ser um colaborador, faz-se necessário:
> - Ter uma conta no [GitHub](https://github.com)
> - Conhecimentos em Java
> - Uso da plataforma Eclipse
> - Conhecimentos no desenvolvimento de adicionais para o Eclipse.
> - Conhecimentos básicos no uso da aplicação Maven
> - Conhecimentos no uso de repositórios GIT
> - Conhecimentos no uso das ferramentas de teste JUnit e SWTBot

1. Obtenha e instale a plataforma [Eclipse IDE for RCP and RAP Developers](https://www.eclipse.org/downloads/packages/release/2019-09/r/eclipse-ide-rcp-and-rap-developers-includes-incubating-components).
2. Inicie o Eclipse e instale os adicionais:
  - ???????????????????????
3. Efetue um _fork_ do [TDS-Eclipse](https://github.com/totvs/tds-eclipse)
   **ATENÇÃO:** Faça o _fork_ a partir do ramo no qual vai efetuar a correção ou do ramo `desenv`, se for uma nova funcionalidade.
   Fazendo assim, você agiliza o processo de revisão e incorporação.
4. Inicie (ou abra) uma área de trabalho.
5. Baixe o projeto a partir do endereço do seu _fork_.
6. Selecione o projeto `br.com.totvs.tds.tycho`.
7. Acione o menu de contexto do projeto, item de menu `Configure | Configure and Detect Nested Project...`.
8. No diálogo apresentado, selecione todos os projetos e confirme.
9. Após o passo anterior, pode fechar os projetos:
  - br.com.totvs.tds.tycho
  - br.com.totvs.tds.tycho.configurations
  - bundles
  - features
  - tests
10. Acesse [Abertura e acompanhamento de chamados](https://github.com/totvs/tds-eclipse/issues), localize (ou crie) um e...
11. DIVIRTA-SE programando :D
12. Ao terminar a sua colaboração, solicite a incorporação.

> Veja outras formas de colaborar no tópico _Colaboração_ abaixo.

## Conceitos

Se você nunca usou o **TDS** ou o _Eclipse_, recomendamos os tópicos:

* [[Conceitos]]
* [[Operações Básicas]]
* [[Preferências|Preferências]]

## Suporte

No caso de dúvidas ou ocorrências, abra um chamado em [Abertura e acompanhamento de chamados](https://github.com/totvs/tds-eclipse/issues).

> Leitura complementar: [[Ajude-nos a ajudá-lo]]

## Colaboração

Você pode contribuir com o **TDS** de diversas formas. Escolha uma.

### Erros, falhas ou sugestões

Caso encontre erros, comportamentos inesperados ou tenha sugestões, entre em contato através de um chamado.
Isso vale tanto para ocorrências nos adicionais do **TDS**, assim como na sua documentação.

> [Abertura e acompanhamento de chamados](https://github.com/totvs/tds-eclipse/issues)

Leitura complementar: [[Ajude-nos a ajudá-lo]]

### Documentação

1. Efetue um _fork_ do repositório (Wiki)[https://github.com/totvs/tds-eclipse/wiki]
2. Faça as edições que desejar
3. Requisite a incorporação de suas edições

> Recomenda-se que a edição seja efetuada localmente. 
> Leitura complementar: [[Fazendo fork]], [[Edição local da documentação]], [[Diretrizes de Documentação]]

### Tradução (nova ou revisão)

1. Execute os mesmos passos do tópico _Guid ultra-rápido e simplificado | Colaborador_
2. Faça as edições que desejar
3. Requisite a incorporação de suas edições

> Recomenda-se que a edição seja efetuada localmente. 
> Leitura complementar: [[Fazendo fork]], [[Edição local da documentação]], [[Diretrizes de Documentação]]

### Desenvolvimento

1. Procure um incidente com que possa colaborar.
  Caso a sua colaboração não se enquadre em nenhum chamado, abra um descrevendo o que irá fazer.
2. Efetue um _fork_ do repositório [TDS Eclipse](https://github.com/totvs/tds-eclipse)
3. Faça o desenvolvimento da nova funcionalidade ou a correção de um chamado
4. Requisite a incorporação de suas edições

> ATENÇÃO: Faça o _fork_ a partir do ramo no qual vai efetuar a correção ou do ramo `Desenv`, se for uma nova funcionalidade.
> Fazendo assim, você agiliza o processo de revisão e incorporação.

Leitura complementar: [[Fazendo fork]], [[Estrutura do TDS-Eclipse]], [[Diretrizes de Desenvolvimento]]

## Problemas Conhecidos

1. Incompatibilidade da lista de servidores registrados no [TDS-VSCode](https://github.com/totvs/tds-vscode) ou em **TDS** anteriores a versão 11.4. Assim como, a lista de servidores do **TDS 11.4** também não é compatível com essas aplicações.
Chamados #11 e #12.
