package br.com.totvs.tds.server.interfaces;

import java.net.URI;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import org.eclipse.debug.core.ILaunchesListener;

import br.com.totvs.tds.lsp.server.model.protocol.CompileOptions;
import br.com.totvs.tds.server.ServerOsType;
import br.com.totvs.tds.server.jobs.ValidationPatchReturn;
import br.com.totvs.tds.server.jobs.applyPatch.ApplyPatchReturn;
import br.com.totvs.tds.server.model.RpoTypeElement;
import br.com.totvs.tds.server.model.SourceInformation;

/**
 * Interface AppServerInfo.
 *
 * @author leo.watanabe
 *
 */
public interface IAppServerInfo extends IItemInfo {

	/**
	 * Recupera o endereço.
	 *
	 * @return the address
	 */
	URI getAddress();

	/**
	 * Retorna a porta de conexão.
	 *
	 * @return porta de conexão çãoplicação servidora.
	 */
	int getAppServerPort();

	/**
	 * Recupera o nome do computador.
	 *
	 * @return
	 */
	String getComputerName();

	/**
	 * @return estado da conexão.
	 */
	boolean isConnected();

	/**
	 * Retorna o tipo de OS do elemento.
	 *
	 * @return the type element.
	 */
	ServerOsType getServerOsType();

	/**
	 * Tipo de servidor
	 *
	 * @return then server type
	 */
	ServerType getServerType();

	/**
	 * s�oeturn Versão do servidor.
	 */
	String getVersion();

	/**
	 * @return caminho smart client
	 */
	String getSmartClientPath();

	/**
	 * @return estado de bloqueio ou não.
	 */
	boolean isBlockedToConnection();

	/**
	 * @return indica se o console de servidor local deve ser apresentado ou não.
	 */
	boolean isConsoleLog();

	/**
	 * @return estado de monitoramento
	 */
	boolean isMonitoring();

	/**
	 * @return çãoado de apresemyação do console do servidor.
	 *
	 */
	boolean isShowConsole();

	/**
	 * Ajusta o endereço.
	 *
	 * @param address endereço URI �nico do elemento.
	 */
	void setAddress(URI address);

	/**
	 * Ajustção porta de conexão a aplicação servidora.
	 *
	 * @param port porta do servidor
	 */
	void setAppServerPort(int port);

	/**
	 * Ajusta o nome do computador.
	 *
	 * @param computerName
	 */
	void setComputerName(String computerName);

	/**
	 * Status da conexão.
	 *
	 * @param connected
	 */
	void setConnected(boolean connected);

	/**
	 * Ajusta a apresentação ou não do log de console do servidor local.
	 *
	 * @param show apresenta ou não o log de console.
	 */
	void setConsoleLog(boolean show);

	/**
	 * Estado de monitoramento.
	 *
	 * @param monitoring
	 */
	void setMonitoring(boolean monitoring);

	/**
	 * Ajusta o tipo de SO do elemento.
	 *
	 * @param osType the OS type element.
	 */
	void setServerOsType(ServerOsType osType);

	/**
	 * Ajusta o tipo de servidor do elemento.
	 *
	 * @param serverType type of element.
	 */
	void setServerType(ServerType serverType);

	/**
	 * Estado de apresentação do console do servidor.
	 *
	 * @param showConsole
	 */
	void setShowConsole(boolean showConsole);

	/**
	 * Ajusta a Versão do servidor.
	 *
	 * @param version Versão do servidor.
	 */
	void setVersion(String version);

	/**
	 * Ajusta caminho do SmartCLient.
	 *
	 * @param smartClientPath, caminho smart client
	 */
	void setSmartClientPath(String smartClientPath);

	/**
	 * @param environment
	 * @param typeElement
	 * @param includeTRes
	 * @return mapa de RPO do tipo indicado, com ou sem arquivos *.TRES.
	 */
	List<IRpoElement> getRpoMap(String environment, RpoTypeElement typeElement, boolean includeTRes);

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

	void setLocalServer(boolean localServer);

	boolean isLocalServer();

	boolean authentication(Map<String, Object> connectionMap);

	String[] getDirectory(String environment, String absolutPath, boolean b);

	List<SourceInformation> getPatchInfo(String environment, Path serverPatch);

	ValidationPatchReturn validPatch(String environment, final URI patchFile, boolean local);

	ApplyPatchReturn applyPatch(String environment, URI serverPatch, boolean local, boolean oldPrograms);

	void buidlFile(List<String> files, CompileOptions compileOptions, List<String> includePaths);

	void defragRPO(String environment, boolean clearPatchLog);

}
