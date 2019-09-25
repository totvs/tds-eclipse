package br.com.totvs.tds.ui.sdk.wrapper;

/**
 * Interface IIncludeDataModel.
 * 
 * @author leo.watanabe
 *
 */
public interface IIncludeDataModel {

	String GLOBAL = "$(GLOBAL)"; //$NON-NLS-1$
	String WORKSPACE = "$(WORKSPACE)"; //$NON-NLS-1$
	String GLOBAL_INCLUDE = "org.osgi.tds.global_includes"; //$NON-NLS-1$

	/**
	 * @return the folder
	 */
	String getFolder();

	/**
	 * @return the message
	 */
	String getMessage();

	/**
	 * @return the warning
	 */
	boolean isWarning();

	/**
	 * @return Indica se � configuração global ou não.
	 */
	boolean isGlobal();

	/**
	 * @return Indica se � pasta a partir da �rea de trabalho.
	 */
	boolean isWorkspace();

}