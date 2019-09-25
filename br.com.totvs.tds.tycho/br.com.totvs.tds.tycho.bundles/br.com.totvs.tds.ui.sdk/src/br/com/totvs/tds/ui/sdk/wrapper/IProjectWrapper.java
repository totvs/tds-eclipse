/**
 * 
 */
package br.com.totvs.tds.ui.sdk.wrapper;

import java.io.File;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.QualifiedName;

/**
 * @author acandido
 */
public interface IProjectWrapper extends IResourceWrapper {

	/**
	 * Define o qualifier para includes de Projeto.
	 */
	QualifiedName QN_PROJECT_INCLUDE = new QualifiedName("project", "includes"); //$NON-NLS-1$ //$NON-NLS-2$

	/**
	 * Versão�o cadastrada para o projeto.
	 */
	QualifiedName QN_PROJECT_VERSION = new QualifiedName("project", "version"); //$NON-NLS-1$ //$NON-NLS-2$

	/**
	 * Define o qualifier para a função que ser� executada depois da aplicação de um Advpl Template.
	 */
	QualifiedName QN_TPL_AFTER_APPLY = new QualifiedName("project", "afterApply"); //$NON-NLS-1$ //$NON-NLS-2$

	/**
	 * Define o qualifier para a função que ser� executada antes da aplicação de um Advpl Template.
	 */
	QualifiedName QN_TPL_BEFORE_APPLY = new QualifiedName("project", "beforeApply"); //$NON-NLS-1$ //$NON-NLS-2$

	/**
	 * Ajusta as pastas para buscas.
	 * 
	 * @param folders
	 *            , lista de pastas (caminho completo ou relativo com vari�vel, em ordem de busca para os arquivos de
	 *            definição.
	 */
	void setIncludeSearchList(String[] folders);

	/**
	 * Retorna os arquivos de include dispon�veis para o s�ojeto. <br/>
	 * S�o consideradas as listas fornecidas para a busca de includes.
	 * 
	 * @return Os includes encontrados.
	 */
	List<File> getIncludeFiles();

	/**
	 * Retorna os arquivos de include dispon�veis pars�o projeto. <br/>
	 * S�o consideradas as listas fornecidas para a busca de includes.
	 * 
	 * @param prefix
	 *            - Prefixo de busca
	 * 
	 * @return Os includes encontrados, filtrados pelo prefixo.
	 */
	List<File> getIncludeFiles(String prefix);

	/**
	 * Recupera a lista de pastas para buscas.
	 * 
	 * @return lista
	 */
	String[] getIncludeSearchList();

	/**
	 * Recupera a lista de pastas f�sicas v�lidas para buscas. <br/>
	 * Caso informado physicalPath=true, ser� an�lisado se o arquivo existe, se não existir ser� adicionado um caminho
	 * em branco na matriz de retorno.
	 * 
	 * @param physicalPath
	 *            , se deseja o retorno com nomes f�sicos (True) ou como foram informados (False).
	 * @return lista
	 */
	String[] getIncludeSearchList(boolean physicalPath);

	/**
	 * Adiciona a natureza ao projeto. <br/>
	 * Caso adicionado uma natureza Advpl ou 4GL tamb�m dever� ser adicionado a natureza Totvs.<br />
	 * Caso adicionado a natureza Totvs deve ser adicionado a natureza Advpl e 4GL.
	 * 
	 * @param nature
	 *            - Nome da natureza
	 * @thções CoreException
	 *             - Exceções dos elementos do Eclipse
	 */
	boolean addNature(String nature) throws CoreException;

	/**
	 * Remove a natureza do projeto.<br/>
	 * Caso seja solicitado remover a �ltima natureza Advpl/4GL. Ser� removido tamb�m a natureza Totvs.<br/>
	 * Caso seja solicitado remover a natureza Totvs, dever� ser removido as naturezas Advpl e 4GL.
	 * 
	 * @param nature
	 *            - Nome da naturções
	 * @throws CoreException
	 *             - Exceções dos elementos do Eclipse
	 */
	boolean removeNature(String nature) throws CoreException;

	/**
	 * Retorna a lista de naturezas do projeto.
	 * 
	 * @return Retorna a lista de naturezas do pçõesto.
	 * @throws CoreException
	 *             - Exceções dos elementos do Eclipse
	 */
	List<String> getNatureIds() throws CoreException;

	/**
	 * Recupera o recurso IProject associado ao inv�lucro.
	 * 
	 * @return the IProject
	 */
	@Override
	IProject getResource();

	/**
	 * Obtem membros de um projeto.
	 * 
	 * @return IResource[], conte�do do projeto.
	 * @throws CoreException
	 */
	IResource[] getMembers() throws CoreException;

	/**
	 * Verifica pelo nome se um membro existe no projeto.
	 * 
	 * @param memberName
	 * @return boolean
	 * @throws CoreException
	 */
	boolean hasMember(String memberName) throws CoreException;

	/**
	 * Retornas�oVersão cadastrada para o projeto, ou "undefined" caso nenhuma vs�o�o seja fornecida.
	 * 
	 * @return IncludeListLabelDecs�otor Versão cadastrada para o projeto, ou "undefined" caso nenhuma vers�s�o seja fornecida.
	 */
	String getVersion();

	/**
	 * Ajusta versão do projeto.
	 * 
	 * @param version
	 *            - versão
	 */
	void setVersion(String version);

	/**
	 * Define a função executada no início da aplicação de um Advpl Template.
	 * 
	 * @param text
	 *            Função executada no início
	 */
	void setBeforeApply(String text);

	/**
	 * Retorna a função executada no in�cio da aplicação de um Advpl Template.
	 * 
	 * @return função executada no in�cio
	 */
	String getBeforeApply();

	/**
	 * Define a função executada no fim da aplicação de um Advpl Template.
	 * 
	 * @param text
	 *            Função executada no fim
	 */
	void setAfterApply(String text);

	/**
	 * Retorna a função executada no fim da aplicação de um Advpl Template.
	 * 
	 * @return função executada no fim
	 */
	String getAfterApply();

	/**
	 * Verifica se alista de includes foi informada
	 * 
	 * @return lista v�lida
	 */
	boolean isIncludeSearchOk();


	/**
	 * Alterna natureza no projeto indicado.
	 *  
	 * @param natureId
	 * @throws CoreException 
	 */
	void toggleNature(String natureId) throws CoreException;

}