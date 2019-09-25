package br.com.totvs.tds.server;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OptionalDataException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.HashMap;

import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.services.IServiceLocator;
import org.osgi.framework.BundleContext;

import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.ui.TDSMessageHandler;

/**
 * Ativaação do Bundle.
 * 
 * @author acandido
 */
public final class ServerActivator extends Plugin {

	private static final String PLUGIN_ID = "br.com.totvs.tds.server"; //$NON-NLS-1$
	private static final String FILENAME_TDS113 = "br.com.totvs.tds.server.internal.ServerManager.servers";

	private static final String FILENAME_TDS114 = "servers.data";

	/** Instância do ativador. */
	private static ServerActivator plugin;

	private static HashMap<String, Object> property = new HashMap<String, Object>();

	/**
	 * Retorna a inst��ncia do adicional.
	 * 
	 * @return
	 * @return the bundle activator
	 */
	public static ServerActivator getDefault() {
		return plugin;
	}

	private IServerManager serverManagerService;

	/** 
	 * Retorna a propriedade como uma String
	 * 
	 * @param key
	 * @return
	 */
	public Object getProperty2(final String key) {
		return property.get(key);
	}

	/**
	 * Retorna a propriedade como uma String
	 * 
	 * @param key
	 * @return
	 */
	public String getPropertyString(final String key) {
		return String.valueOf(property.get(key));
	}

	public void setProperty(final String key, final int timeOut) {
		property.put(key, String.valueOf(timeOut));
	}

	public void setProperty(String key, Object value) {
		property.put(key, value);
	}

	public void setProperty(final String key, String value) {
		property.put(key, value);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext)
	 */
	@Override
	public void start(final BundleContext context) throws Exception {
		super.start(context);

		plugin = this;

		try {
			loadServerList();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
	 */
	@Override
	public void stop(final BundleContext context) throws Exception {
		saveServerList();
		plugin = null;

		super.stop(context);
	}

	private File getServerListFile() {
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		File rootWs = workspace.getRoot().getLocation().toFile();
		File file = new File(rootWs, FILENAME_TDS114); // $NON-NLS-1$

		return file;
	}

	/**
	 * Carga da lista de servidores registrados na �rea de trabalho
	 */
	private void loadServerList() {
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		File rootWs = workspace.getRoot().getLocation().toFile();

		// faz compatibilização com 11.4
		File file = new File(rootWs, FILENAME_TDS113); // $NON-NLS-1$
		if (file.exists()) {
			logStatus(IStatus.INFO, "Visão Servidor", "Migrando lista de servidores.");
			Path target = new File(rootWs, FILENAME_TDS114).toPath();
			try {
				Files.copy(file.toPath(), target, StandardCopyOption.REPLACE_EXISTING);
				file.deleteOnExit();
				logStatus(IStatus.INFO, "Visão Servidor", "Lista de servidores migrada.");
			} catch (IOException e) {
				logStatus(IStatus.ERROR, "Visão Servidor", e.getMessage(), e);
			}
		}
		// fim da compatibilização

		file = getServerListFile();

		if (file.exists()) {
			logStatus(IStatus.INFO, "Visão Servidor", "Lendo lista de servidores registrados.\n\tArquivo: %s",
					file.getAbsolutePath());

			InputStream is = null;
			try {
				is = new FileInputStream(file);
				getServerManager().loadFrom(is);
			} catch (OptionalDataException e) {
				showStatus(IStatus.ERROR, "Visão Servidor",
						"Falha na carga do arquivo de servidores. Provavelmente o arquivo esta danificado.", e);
			} catch (ClassNotFoundException | IOException e) {
				logStatus(IStatus.ERROR, "Visão Servidor", e.getMessage(), e);
			} finally {
				try {
					if (is != null) {
						is.close();
					}
				} catch (IOException e) {
				}
			}

			logStatus(IStatus.INFO, "Visão Servidor", "Lista de servidores registrados carregada.");
		}
	}

	public IServerManager getServerManager() {
		if (serverManagerService == null) {
			IServiceLocator serviceLocator = PlatformUI.getWorkbench();
			serverManagerService = serviceLocator.getService(IServerManager.class);
		}
		
		return serverManagerService;
	}

	/**
	 * Salva a lista de servidores registrados.
	 */
	private void saveServerList() {
		File file = getServerListFile();
		OutputStream os = null;

		logStatus(IStatus.INFO, "Visão Servidor", "Salvando lista de servidores registrados.\n\tArquivo: %s",
				file.getAbsolutePath());

		try {
			os = new FileOutputStream(file);
			IServerManager serverManager = ServerActivator.getDefault().getServerManager();
			serverManager.saveTo(os);
			logStatus(IStatus.INFO, "Visão Servidor", "Lista de servidores registrados salvo.");
		} catch (IOException e) {
			logStatus(IStatus.ERROR, "Visão Servidor", e.getMessage(), e);
		} finally {
			if (os != null) {
				try {
					os.close();
				} catch (IOException e) {
				}
			}
		}
	}

	/**
	 * Utility method to create status.
	 * 
	 * @param level
	 * @param message
	 * @param thr
	 *
	 * @return status
	 */
	public static IStatus createStatus(int level, String message, Throwable thr) {

		return new Status(level, PLUGIN_ID, 0, message, thr);
	}

	/**
	 * Utility method to create status.
	 * 
	 * @param level
	 * @param message
	 * @param thr
	 * @return status
	 */
	public static IStatus showStatus(int level, String title, String message, Object... args) {
		IStatus status = TDSMessageHandler.createStatus(level, PLUGIN_ID, title, message, args);
		TDSMessageHandler.showMessage(title, status);

		getDefault().getLog().log(status);

		return status;
	}

	public static IStatus logStatus(int level, String title, String message, Object... args) {
		IStatus status = TDSMessageHandler.createStatus(level, PLUGIN_ID, title, message, args);
		TDSMessageHandler.logMessage(title, status);

		getDefault().getLog().log(status);

		return status;
	}

	public static IStatus logStatus(int level, String title, String message) {
		return logStatus(level, title, message, TDSMessageHandler._EMPTY_ARGS);
	}

}
