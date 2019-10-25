package br.com.totvs.tds.server.interfaces;

public enum ServerType {
	PROTHEUS(1, "Protheus"), //$NON-NLS-1$
	LOGIX(2, "Logix") //$NON-NLS-1$
	/* , DBACCESS(100, "DBAccess") */;

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
			return "br.com.totvs.tds.ui.server.tools.ProtheusLoginDialog"; // $NON-NLS-N$ //$NON-NLS-1$
		case LOGIX:
			return "br.com.totvs.tds.ui.server.tools.LogixLoginDialog"; // $NON-NLS-N$ //$NON-NLS-1$
//		case DBACCESS:
//			return "br.com.totvs.tds.ui.server.tools.DbAccessLoginDialog"; // $NON-NLS-N$ //$NON-NLS-1$
		default:
			break;
		}

		return null;
	}
}
