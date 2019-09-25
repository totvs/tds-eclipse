package br.com.totvs.tds.ui;

import org.eclipse.core.runtime.QualifiedName;

/**
 * Interface para configurar constantes.
 * 
 * @author Audrin
 * 
 */
public interface TDSConstants {

	String _NATURE_ID_TOTVS = "br.com.totvs.tds.ui.sdk.protheusNature"; //$NON-NLS-1$
	
	// --------------------

	String _LAUCHER_SMARTCLIENT_DIRECTORY = "SmartClientDirectory"; //$NON-NLS-1$
	String _LAUCHER_SMARTCLIENT_PARAMETERS = "SmartClientParameters"; //$NON-NLS-1$

	String _LAUCHER_START_PROGRAM = "StartProgram"; //$NON-NLS-1$
	String _LAUCHER_START_PROGRAM_PARAMETERS = "ProgramParameters"; //$NON-NLS-1$

	String _LAUCHER_PROFILE = "Profile"; //$NON-NLS-1$

	String _LAUNCHER_REMEMBER_FLAG = "RememberFlag"; //$NON-NLS-1$
	String _LAUNCHER_COMPILE_FONT_FLAG = "CompileFlag"; //$NON-NLS-1$
	String _LAUCHER_INTERNAL_SERVER = "InternalServer"; //$NON-NLS-1$

	String _FALSE = "FALSE"; //$NON-NLS-1$
	String _TRUE = "TRUE"; //$NON-NLS-1$

	/**
	 * Preferencia do launcher tab do coverage.
	 */
	/**
	 * Flag para habilitar ou não o coverage.
	 */
	String _LAUNCHER_ENABLE_COVERAGE = "enableCoverage"; //$NON-NLS-1$

	/**
	 * Flag para recuperar os fontes que ser�o utilizados pelo coverage.
	 */
	String _LAUNCHER_SOURCE_LIST_COVERAGE = "listSourceCoverage"; //$NON-NLS-1$

	/**
	 * Define o qualifierName de arquivos WSDL ADVPL.
	 */
	public static final QualifiedName WSDL_URL_PROPERTY = new QualifiedName("br.com.totvs.tds.sdk.advpl", //$NON-NLS-1$
			"wsdl_url_property"); //$NON-NLS-1$

	public static final String _WORKSPACE_PATH_VARIABLE = "${workspace_loc}"; //$NON-NLS-1$

	public static final String _PREFERENCE_STORE_ROOT = "br.com.totvs.tds.ui"; //$NON-NLS-1$
	public static final String _PREFERENCE_STORE_WORKAREA = "br.com.totvs.tds.ui/workarea"; //$NON-NLS-1$

	/**
	 * Define ID de problemas AdvPL.
	 */
	public static final String _ADVPL_MARKER = "br.com.totvs.tds.sdk.advplmarker"; //$NON-NLS-1$

	/**
	 * Define ID de problemas 4GL.
	 */
	public static final String _FOURGL_MARKER = "br.com.totvs.tds.sdk.fourglmarker"; //$NON-NLS-1$

	/**
	 * Define ID de problemas PDOC.
	 */
	public static final String _PDOC_MARKER = ""; //$NON-NLS-1$

	/**
	 * Define ID de problemas com projeto.
	 */
	public static final String _PROJECT_MARKER = "br.com.totvs.tds.sdk.projectmarker"; //$NON-NLS-1$

	/**
	 * Define ID de problemas com validadores.
	 */
	public static final String _VALIDADOR_MARKER = "br.com.totvs.tds.sdk.validatormarker"; //$NON-NLS-1$

}
