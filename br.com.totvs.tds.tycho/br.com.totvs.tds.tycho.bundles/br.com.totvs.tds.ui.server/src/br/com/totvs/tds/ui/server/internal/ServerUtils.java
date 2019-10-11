package br.com.totvs.tds.ui.server.internal;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;

import br.com.totvs.tds.ui.TDSUtil;
import br.com.totvs.tds.ui.server.nl.Messages;
import br.com.totvs.tds.ui.server.wizards.server.INIFile;
import br.com.totvs.tds.ui.server.wizards.server.INIFile.INIProperty;
import br.com.totvs.tds.ui.server.wizards.server.INIFile.INISection;

public class ServerUtils {

	private static String[] buildVariants(String[] strings) {
		List<String> stringList = Arrays.asList(strings);

		for (String string : strings) {
			stringList.add(string.toLowerCase());
			stringList.add(string.substring(0, 1).toLowerCase() + string.substring(1).toLowerCase());
		}

		stringList.add("*.*"); //$NON-NLS-1$

		return stringList.toArray(new String[stringList.size()]);
	}

	public static String[] doProcessSmartClientIni(String smartClientPath) {
		IPath scPath = Path.fromOSString(smartClientPath);
		File iniFile = scPath.removeFileExtension().addFileExtension("ini").toFile(); //$NON-NLS-1$

		String serverAddress = Messages.EMPTY_STRING;
		String port = Messages.EMPTY_STRING;

		if (iniFile.exists()) {
			INIFile ini = new INIFile(iniFile.getAbsolutePath());

			INISection drivers = ini.getSection("DRIVERS"); //$NON-NLS-1$
			INIProperty active = drivers.getProperty("ACTIVE"); //$NON-NLS-1$
			INISection driver = ini.getSection(active.getPropValue().toUpperCase());

			serverAddress = driver.getProperty("SERVER").getPropValue(); //$NON-NLS-1$
			port = (driver.getProperty("PORT").getPropValue()); //$NON-NLS-1$
		}

		String[] result = new String[] { serverAddress, port };

		return result;
	}

	public static String doSelectAppServer(Shell shell) {
//		if (TDSUtil.isLinux()) {
//			dialog.setFilterExtensions(buildVariants(IServerConstants.APP_SERVER_EXECUTABLES_LINUX));
//		} else if (TDSUtil.isWindows()) {
//			dialog.setFilterExtensions(IServerConstants.APP_SERVER_EXECUTABLES_WIN);
//		} else {
//			dialog.setFilterExtensions(buildVariants(IServerConstants.APP_SERVER_EXECUTABLES_MAC));
//		}

		return TDSUtil.fileDialog(shell);
	}

	public static String doSelectSmartClient(Shell shell) {
		FileDialog dialog = new FileDialog(shell);

//		if (TDSUtil.isLinux()) {
//			dialog.setFilterExtensions(buildVariants(IServerConstants.SMARTCLIENT_EXECUTABLES_LINUX));
//		} else if (TDSUtil.isWindows()) {
//			dialog.setFilterExtensions(IServerConstants.SMARTCLIENT_EXECUTABLES_WIN);
//		} else {
//			dialog.setFilterExtensions(buildVariants(IServerConstants.SMARTCLIENT_EXECUTABLES_MAC));
//		}

		String result = dialog.open();

		return result == null ? Messages.EMPTY_STRING : result;
	}
}
