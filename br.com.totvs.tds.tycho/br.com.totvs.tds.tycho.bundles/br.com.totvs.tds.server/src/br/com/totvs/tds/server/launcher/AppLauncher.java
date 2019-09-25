package br.com.totvs.tds.server.launcher;

import java.io.File;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.ILaunchesListener;
import org.eclipse.debug.core.model.IProcess;

import br.com.totvs.tds.server.ServerActivator;

public class AppLauncher implements ILaunchesListener {

	private ILaunch launch;
	private String appName;
	private String appFilename;
	private String[] appParams;
	private String configName;

	public AppLauncher(final String serverName, final String serverFilename, final String[] appServerParams) {
		this.appName = serverName;
		this.appFilename = serverFilename;
		this.appParams = appServerParams;
		this.configName = String.format("_app_%s", appName.toLowerCase());
	}

	@Override
	public void launchesAdded(final ILaunch[] launches) {

	}

	@Override
	public void launchesChanged(final ILaunch[] launches) {

	}

	@Override
	public void launchesRemoved(final ILaunch[] launches) {

	}

	public void start() {
		ServerActivator.logStatus(IStatus.INFO, appName, "Iniciando servidor [%s].\n\tAplicação: %s", appName,
				appFilename);

		final ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
		manager.addLaunchListener(this);
		final ILaunchConfigurationType type = manager
				.getLaunchConfigurationType("org.eclipse.ui.externaltools.ProgramLaunchConfigurationType");
		ILaunchConfiguration[] configurations;

		try {
			configurations = manager.getLaunchConfigurations(type);
			for (int i = 0; i < configurations.length; i++) {
				final ILaunchConfiguration configuration = configurations[i];
				if (configuration.getName().equals(configName)) {
					configuration.delete();
					break;
				}
			}

			final File wkPath = new File(appFilename).getParentFile();
			final ILaunchConfigurationWorkingCopy workingCopy = type.newInstance(null, configName);

			workingCopy.setAttribute("org.eclipse.ui.externaltools.ATTR_LOCATION", appFilename);
			workingCopy.setAttribute("org.eclipse.ui.externaltools.ATTR_TOOL_ARGUMENTS", String.join(" ", appParams));
			workingCopy.setAttribute("org.eclipse.ui.externaltools.ATTR_WORKING_DIRECTORY", wkPath.getCanonicalPath());
			launch = workingCopy.launch(ILaunchManager.RUN_MODE, new NullProgressMonitor());

			ServerActivator.logStatus(IStatus.INFO, appName, "Servidor [%s] pronto.", appName);
		} catch (final Exception e) {
			ServerActivator.logStatus(IStatus.ERROR, appName, e.getMessage(), e);
		}

	}

	public void stop() {
		ServerActivator.logStatus(IStatus.INFO, appName, "Solicitada finalização do servidor [%s].", appName);

		final ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
		manager.removeLaunchListener(this);

		if (launch != null) {
			try {
				launch.terminate();
				try {
					Thread.sleep(3000); // aguarda a finalização do processo propriamente dito
				} catch (final InterruptedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}

				final IProcess[] processes = launch.getProcesses();
				for (final IProcess process : processes) {
					if (!process.isTerminated()) {
						ServerActivator.logStatus(IStatus.WARNING, appName,
								"Aplicação não finalizada corretamente.\n\tAplicação: %s",
								process.getAttribute("org.eclipse.debug.core.ATTR_CMDLINE").trim());
					} else if (process.getExitValue() != 0) {
						ServerActivator.logStatus(IStatus.WARNING, appName,
								"Processo finalizado com código de saída [%d].\n\tAplicação: %s",
								process.getExitValue(),
								process.getAttribute("org.eclipse.debug.core.ATTR_CMDLINE").trim());
					}
				}
				ServerActivator.logStatus(IStatus.INFO, appName, "Aplicação finalizada.\n\tAplicação: %s", appFilename);
//				}
			} catch (final DebugException e) {
				e.printStackTrace();
			}
		}

	}
}
