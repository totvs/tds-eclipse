package br.com.totvs.tds.server.interfaces;

import java.net.URI;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import org.eclipse.debug.core.ILaunchesListener;

import br.com.totvs.tds.lsp.server.ILanguageServerService;
import br.com.totvs.tds.lsp.server.model.protocol.CompileOptions;
import br.com.totvs.tds.server.jobs.ApplyPatchReturn;
import br.com.totvs.tds.server.jobs.CleanUpType;
import br.com.totvs.tds.server.jobs.ValidationPatchReturn;
import br.com.totvs.tds.server.model.RPOTypeElement;
import br.com.totvs.tds.server.model.SourceInformation;

/**
 * Interface AppServerInfo.
 *
 * @author leo.watanabe
 *
 */
public interface IAppServerInfo extends IServerInfo {

	/**
	 * Adiciona um ambiente.
	 *
	 * @param environment Item a ser adicionado como filho.
	 * @throws RuntimeException Indica o erro durante o processo.
	 * @throws Exception
	 */
	void addEnvironment(IEnvironmentInfo environment) throws RuntimeException;

	/**
	 * Verifica a existência do elemento, baseado no nome.
	 *
	 * @param environment nome do elemento-alvo.
	 * @return boolean
	 */

	boolean containsEnvironment(String environment);

	Map<String, Object> getConnectionMap();

	/**
	 * Recupera a empresa corrente.
	 *
	 * @return current company
	 */
	IOrganization getCurrentOrganization();

	/**
	 * Retorna o ambiente selecionado atualmente.
	 *
	 * @return ambiente selecionado
	 */
	String getCurrentEnvironment();

	/**
	 * Recupera os elementos filho.
	 *
	 * @return the children
	 */
	List<IEnvironmentInfo> getEnvironments();

	/**
	 * Retorna a seleção de múltiplos Ambientes.
	 *
	 * @return
	 */
	List<String> getMultiEnvironmentSelection();

	/**
	 * Hub de 'slaves' utilizados no 'load balance'.
	 *
	 * @return the slave hub info
	 */
	IServerSlaveHubInfo getSlaveLoadBalance();

	/**
	 * @return Token de�aeguran�a.
	 */
	String getToken();

	/**
	 * Remove um ambiente.
	 *
	 * @param child elemento filho.
	 */
	void removeEnvironment(IEnvironmentInfo child);

	/**
	 * Busca por um elemento, baseado no nome.
	 *
	 * @param environment nome do elemento-alvo.
	 * @return the target element ou null
	 */
	IEnvironmentInfo searchEnvironment(String environment);

	/**
	 * Ajusta a empresa corrente. <br>
	 * Nota: Devido a ocorr�ncia de referencia serviçor do servi�o de dicion�rio ao
	 * utilizar IOrganization, este método recebe-o como um Object.
	 *
	 * @param companySelected
	 */
	void setCurrentCompany(IOrganization companySelected);

	/**
	 * Define o ambiente selecionado atualmente.
	 *
	 * @param newEnvironment ambiente selecionado
	 */
	void setCurrentEnvironment(String newEnvironment);

	/**
	 * Lista de ambientes.
	 *
	 * @param environments
	 * @throws RuntimeException
	 */
	void setEnvironments(List<String> environments) throws RuntimeException;

	/**
	 * Define a seleção de m�ltiplos Ambientes.
	 *
	 * @param multiEnvironment
	 */
	void setMultiEnvironmentSelection(List<String> multiEnvironment);

	/**
	 * Efetua a descarga dos ambientes.
	 *
	 * @throws Exception
	 */
	void unloadEnvironments();

	/**
	 * Efetua a carga de informações de configuração de slave
	 *
	 * @param lsService
	 */
	void loadSlaves(ILanguageServerService lsService);

	/**
	 * @return Lista de organizações
	 */

	List<IOrganization> getOrganizations();

	/**
	 * @return lista de permissões
	 */
	List<String> getPermissions();

	/**
	 *
	 * @return nome do usuário usado no login
	 */
	String getUsername();

	/**
	 *
	 * @param string permissão a ser verificada
	 * @return se tem ou permissão
	 */
	boolean canPermission(String string);

	void setAppServerPath(String appServerPath);

	String getAppServerPath();

	boolean isAppServerLocal();

	boolean isRunning();

	void setLauncher(ILaunchesListener launcher);

	ILaunchesListener getLauncher();

	RPOTypeElement getRPOTypeElement(String fullNameOrExtension);

	void setLocalServer(boolean localServer);

	boolean isLocalServer();

	boolean authentication(Map<String, Object> connectionMap);

	String[] getDirectory(String environment, String absolutPath, boolean b);

	IServerReturn _getPatchIntegrity(String env, List<URI> patchFile, boolean local);

	List<SourceInformation> getPatchInfo(String environment, Path serverPatch);

	ApplyPatchReturn applyPatch(String environment, String serverPatch, boolean local, boolean oldPrograms);

	void cleanUp(String environment, CleanUpType cleanupPatch);

	ValidationPatchReturn validPatch(String environment, final List<URI> patchFiles, boolean local);

	void buidlFile(List<String> files, CompileOptions compileOptions, List<String> includePaths);
}
