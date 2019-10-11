package br.com.totvs.tds.server;

/**
 * ServerOsType map.
 * 
 * @author leo.watanabe
 *
 */
public enum ServerOsType {

	OS_AIX(9, "AIX"), //$NON-NLS-1$
	OS_ALPHA(8, "Alpha"), //$NON-NLS-1$
	OS_AS400(11, "AS400"), //$NON-NLS-1$
	OS_HP(7, "HP"), //$NON-NLS-1$
	OS_HPIA64(10, "HP IA64"), //$NON-NLS-1$
	OS_INVALID(0, "Invalid"), //$NON-NLS-1$
	OS_LINUX(5, "Linux"), //$NON-NLS-1$
	OS_NOT_RECOGNIZED(99, "(not recognized)"), //$NON-NLS-1$
	OS_NT40(3, "NT 4.0"), //$NON-NLS-1$
	OS_NT50(4, "NT 5.0"), //$NON-NLS-1$
	OS_PPC(14, "PPC"), //$NON-NLS-1$
	OS_S390(12, "S390"), //$NON-NLS-1$
	OS_SOLARIS(6, "Solaris"), //$NON-NLS-1$
	OS_WIN2KORSUP(13, "Windows 2000"), //$NON-NLS-1$
	OS_WINDOWS10(26, "Windows 10"), //$NON-NLS-1$
	OS_WINDOWS7(20, "Windows 7"), //$NON-NLS-1$
	OS_WINDOWS8(22, "Windows 8"), //$NON-NLS-1$
	OS_WINDOWS81(24, "Windows 8.1"), //$NON-NLS-1$
	OS_WINDOWS95(1, "Windows 95"), //$NON-NLS-1$
	OS_WINDOWS98(2, "Windows 98"), //$NON-NLS-1$
	OS_WINME(15, "Windows ME"), //$NON-NLS-1$
	OS_WINRT(27, "Windows Rumtime"), //$NON-NLS-1$
	OS_WINSRV2003(17, "Windows Server 2003"), //$NON-NLS-1$
	OS_WINSRV2008(19, "Windows Server 2008"), //$NON-NLS-1$
	OS_WINSRV2008R2(21, "Windows Server 2008 R2"), //$NON-NLS-1$
	OS_WINSRV2012(23, "Windows Server 2012"), //$NON-NLS-1$
	OS_WINSRV2012R2(25, "Windows Server 2012 R2"), //$NON-NLS-1$
	OS_WINSRV2016(28, "Windows Server 2016"), //$NON-NLS-1$
	OS_WINVISTA(18, "Windows Vista"), //$NON-NLS-1$
	OS_WINXP(16, "Windows XP"); //$NON-NLS-1$

	public static ServerOsType valueOf(final int osTypeCode) {
		ServerOsType result = OS_INVALID;
		//
		ServerOsType[] values = values();
		for (ServerOsType serverOsType : values) {
			if (serverOsType.getOsCode() == osTypeCode) {
				result = serverOsType;
				break;
			}
		}
		//
		return result;
	}

	private final int osCode;

	private final String osName;

	ServerOsType(final int osCode, final String osName) {
		this.osCode = osCode;
		this.osName = osName;
	}

	/**
	 * @return the osCode
	 */
	public int getOsCode() {
		return osCode;
	}

	/**
	 * @return the osName
	 */
	public String getOsName() {
		return osName;
	}

}
