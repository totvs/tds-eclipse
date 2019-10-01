package br.com.totvs.tds.server.interfaces;

import java.net.URI;

import br.com.totvs.tds.server.ServerOsType;

/**
 * Interface com informações para servidor (ServerInfo).
 *
 * @author acandido
 */
/**
 * @author acandido
 *
 */
public interface IServerInfo extends IItemInfo {

	public enum ServerType {
		PROTHEUS(1, "Protheus"), LOGIX(2, "Logix"), DBACCESS(100, "DBAccess");

		private int code;
		private String title;

		ServerType(final int code, final String title) {
			this.code = code;
			this.setTitle(title);

		}

		/**
		 * @return the code
		 */
		public int getCode() {
			return code;
		}

		/**
		 * @return the title
		 */
		public String getTitle() {
			return title;
		}

		/**
		 * @param title the title to set
		 */
		public void setTitle(final String title) {
			this.title = title;
		}

		public String getLoginDialog() {
			switch (this) {
			case PROTHEUS:
				return "br.com.totvs.tds.ui.server.tools.ProtheusLoginDialog"; // $NON-NLS-N$
			case LOGIX:
				return "br.com.totvs.tds.ui.server.tools.LogixLoginDialog"; // $NON-NLS-N$
			case DBACCESS:
				return "br.com.totvs.tds.ui.server.tools.DbAccessLoginDialog"; // $NON-NLS-N$
			default:
				break;
			}

			return null;
		}
	}

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
	 * @return estado da conexão.
	 */
	boolean isConnected();

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

//	/**
//	 * Ajusta o mapa de conexão (par�metros).
//	 */
//	void setConnectionMap(Map<String, String> map);
//	Map<String, String> getConnectionção();

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
	 * Ajusta a Versão do sers�oor.
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
}