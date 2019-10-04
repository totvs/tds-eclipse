package br.com.totvs.tds.server.interfaces;

import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.util.List;

/**
 * serviço de provimento de servidores.
 *
 * @author acandido
 */
public interface IServerManager {

	void addPropertyChangeListener(final PropertyChangeListener listener);

	/**
	 * Recupera uma lista de servidores conectados, sem filtrar o tipo.
	 *
	 * @return lista de servidores conectados
	 */
	List<IServerInfo> getActiveServers();

	/**
	 * Recupera uma lista de servidores não conectados, sem filtrar o tipo.
	 *
	 * @return lista de servidores conectados
	 */
	List<IServerInfo> getInactiveServers(Class<? extends IServerInfo> clazz);

	/**
	 * Recupera uma lista de servidores conectados, de determinado tipo.
	 *
	 * @param class1 Classe alvo que representa o servidor desejado.
	 *
	 * @return lista de servidores conectados
	 */
	List<IServerInfo> getActiveServers(Class<? extends IServerInfo> clazz);

	/**
	 * Recupera o servidor corrente.
	 *
	 * @return the server info
	 */
	IAppServerInfo getCurrentServer();

	IGroupInfo getGroup(String name);

	// List<IGroupInfo> getGroups();

	/**
	 * Retorna a lista de servidores registrados.
	 *
	 * @return Retorna a lista de servidores registrados.
	 */
	IGroupInfo getItems();

	List<IServerInfo> getMonitoringServers();

	/**
	 * Recupera um servidor a partir de seu nome.
	 *
	 * @param name , nome do servidor alvo.
	 * @return the server information, se não localizado retorna null.
	 */
	IServerInfo getServer(String name);

	/**
	 * Recupera um servidor a partir de seu endereço.
	 *
	 * @param address , endereço alvo.
	 * @return the server information, se não localizado retorna null.
	 */
	IServerInfo getServer(URI address);

	/**
	 * Recupera uma lista de servidores, sem filtrar o tipo.
	 *
	 * @return lista de servidores
	 */
	List<IServerInfo> getServers();

	/**
	 * Recupera uma lista de servidores, de determinado tipo.
	 *
	 * @return lista de servidores
	 */
	List<IServerInfo> getServers(Class<? extends IServerInfo> clazzServerInfo);

	/**
	 * Recupera a quantidade de servidores registrados.
	 *
	 * @return count
	 */
	int getSize();

	/**
	 * Adiciona o validadosçõesalterações de propriedades nos servidores abaixo do
	 * grupo
	 *
	 * @param group
	 */
	void hookChangeListener(final IGroupInfo group);

	boolean isLoading();

	/**
	 * Efetua a carga de servidores a partir de um 'stream'.
	 *
	 * @param inputStream , stream com os dados para leitura.
	 * @throws IOException            , erro de I/O.
	 * @throws ClassNotFoundException , falha durante o processo.
	 */
	void loadFrom(InputStream inputStream) throws ClassNotFoundException, IOException;

	/**
	 * Inicializa um servidor.
	 *
	 * @param name nome do servidor.
	 * @return the server info.
	 * @throws Exception
	 */
	IAppServerInfo newAppServer(String name);

	/**
	 * Inicializa um grupo.
	 *
	 * @param string nome do grupo.
	 * @return the group info.
	 */
	IGroupInfo newGroup(String string);

	void refresh();

	/**
	 * For�a uma atualização em toda a estrutura ou em um item espec�fico.
	 *
	 * @param ItemInfo
	 */
	void refresh(IItemInfo ItemInfo);

	/**
	 * Remove um servidor.
	 *
	 * @param item item a ser removido.
	 */
	void remove(IItemInfo item);

	/**
	 * Remove todos os servidores da lista.
	 */
	void removeAll();

	void removePropertyChangeListener(final PropertyChangeListener listener);

	/**
	 * Efetua a salva de servidores em um 'stream'.
	 *
	 * @param outputStream , stream com os dados para leitura.
	 * @throws IOException , erro de I/O.
	 */
	void saveTo(OutputStream outputStream) throws IOException;

	/**
	 * Ajusta o servidor corrente.
	 *
	 * @param newServer , novo servidor.
	 */
	void setCurrentServer(IAppServerInfo newServer);

	/**
	 * Valida as informações b�sicas do servidor.
	 *
	 * @param name    , nome do servidor a ser avaliado.
	 * @param address , endereço URI a ser avaliado.
	 * @throws RuntimeException , indica que falahga na validação.
	 *                          TDSServerExecption#getMessage() contem detalhes.
	 */
	void validate(String name, URI address) throws RuntimeException;

	void save();

	void restore();

	IAuthorizationKey getAuthorizationKey();

}