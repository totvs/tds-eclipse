package br.com.totvs.tds.ui.server.tools;

import java.util.Map;

import br.com.totvs.tds.server.interfaces.IServerConstants;

/**
 * Interface para diálogos de login.
 *
 * @author acandido
 */
public interface ILoginDialog {

	static final String ENVIRONMENT = IServerConstants.ENVIRONMENT;
	static final String LAST_ENVIRONMENTS = IServerConstants.LAST_ENVIRONMENTS;
	static final String PASSWORD = IServerConstants.PASSWORD;
	static final String TOKEN = IServerConstants.TOKEN;
	static final String USE_SECURE_STORAGE = IServerConstants.USE_SECURE_STORAGE;
	static final String USERNAME = IServerConstants.USERNAME;
	static final String PERMISSIONS = IServerConstants.PERMISSIONS;
	static final String SERVER_ADDRESS = IServerConstants.SERVER_ADDRESS;

	/**
	 * Recupera mapa de valores.
	 */
	Map<String, Object> getDataMap();

	/**
	 * @return the initialMessage
	 */
	public String getInitialMessage();

	/**
	 * Inicialização do diálogo.
	 *
	 */
	void initialize(final String title, final Map<String, Object> inputData);

	boolean isSafeSave();

	/**
	 * Validação.
	 *
	 * @return true se estiver correto, senão false.
	 */
	boolean isValid();

	/**
	 * Opens the dialog and returns the code of the button clicked by the user<br>
	 * Dialog.OK or Dialog.CANCEL
	 *
	 * @return - The code of button clicked by the user
	 */
	int open();

	/**
	 * @param initialMessage the initialMessage to set
	 */
	public void setInitialMessage(String initialMessage);

}