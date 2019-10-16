package br.com.totvs.tds.ui.debug.launcher;

/**
 *
 * Interface respons�vel por prover constantes que representam campos de
 * configuração do SmartClient e Coverage.
 *
 * @author AUDRIN
 *
 */
public interface IDebugLauncherAttributes {

	/**
	 * The Enum to define the language and it's code that must be passed to the
	 * SmartClient.
	 *
	 * @author acandido
	 *
	 *
	 */
	enum SCLanguages {
		PORTUGUESE(1, SCLanguages.getPTString()), SPANISH(2, SCLanguages.getESString()),
		ENGLISH(3, SCLanguages.getENString());

		private int code;
		private String languageName;

		public static final String PT = "Portugu�s"; //$NON-NLS-1$
		public static final String EN = "Ingl�s"; //$NON-NLS-1$
		public static final String ES = "Espanhol"; //$NON-NLS-1$

		SCLanguages(final int code, final String languageName) {
			this.code = code;
			this.languageName = languageName;
		}

		@Override
		public String toString() {
			return this.languageName;
		}

		public int getCode() {
			return this.code;
		}

		private static String getPTString() {
			return PT;
		}

		private static String getENString() {
			return EN;
		}

		private static String getESString() {
			return ES;
		}

		/**
		 * Return the enum associated with the String _informed. <br>
		 * If this String _is not existent, it defaults to PORTUGUESE.
		 *
		 * @param languageString
		 * @return
		 */
		public static SCLanguages getEnum(final String languageString) {
			SCLanguages language = null;
			switch (languageString) {
			case SCLanguages.PT:
				language = SCLanguages.PORTUGUESE;
				break;
			case SCLanguages.EN:
				language = SCLanguages.ENGLISH;
				break;
			case SCLanguages.ES:
				language = SCLanguages.SPANISH;
				break;
			default:
				language = SCLanguages.PORTUGUESE;
				break;
			}
			return language;
		}
	}

	/**
	 * The coverage execution mode.
	 */
	String _COVERAGE_MODE = "coverage"; //$NON-NLS-1$

	/**
	 * The coverage launch group id.
	 */
	String _ID_COVERAGE_LAUNCH_GROUP = "br.com.totvs.tds.debug.ui.launchGroup.coverage"; //$NON-NLS-1$
	/**
	 * A relaunch flag.
	 */
	String _IS_RELAUNCH = "isRelaunch"; //$NON-NLS-1$
	/**
	 * The id to store informationa about the program to be relaunched.
	 */
	String _LAST_PROGRAM_EXECUTED = "lastProgramExecuted"; //$NON-NLS-1$
	/**
	 * The id to store information abou the program parameters to be relaunched.
	 */
	String _LAST_PROGRAM_EXECUTED_PARAMS = "lastProgramExecutedParams"; //$NON-NLS-1$
	/**
	 * The id of the last mode executed attribute.
	 */
	String _LAST_MODE_EXECUTED = "lastModeExecuted"; //$NON-NLS-1$

}
