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
import org.eclipse.debug.core.model.IProcess;

import br.com.totvs.tds.server.ServerActivator;

public class AppLauncher {

	private ILaunch launch;
	protected String appName;
	private String appFilename;
	private String[] appParams;
	final private String configName;

	public AppLauncher(final String serverName, final String serverFilename, final String[] appServerParams) {
		this.appName = serverName;
		this.appFilename = serverFilename;
		this.appParams = appServerParams;
		this.configName = String.format("_app_%s", appName.toLowerCase()); //$NON-NLS-1$
	}

	public boolean isRunning() {

		return (launch != null) && (!launch.isTerminated());
	}

	public void start() {
		ServerActivator.logStatus(IStatus.INFO, appName, Messages.AppLauncher_Starting_server, appName, appFilename);

		final ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
		final ILaunchConfigurationType type = manager
				.getLaunchConfigurationType("org.eclipse.ui.externaltools.ProgramLaunchConfigurationType"); //$NON-NLS-1$
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

			workingCopy.setAttribute("org.eclipse.ui.externaltools.ATTR_LOCATION", appFilename); //$NON-NLS-1$
			workingCopy.setAttribute("org.eclipse.ui.externaltools.ATTR_TOOL_ARGUMENTS", String.join(" ", appParams)); //$NON-NLS-1$ //$NON-NLS-2$
			workingCopy.setAttribute("org.eclipse.ui.externaltools.ATTR_WORKING_DIRECTORY", wkPath.getCanonicalPath()); //$NON-NLS-1$
			launch = workingCopy.launch(ILaunchManager.RUN_MODE, new NullProgressMonitor());

			ServerActivator.logStatus(IStatus.INFO, Messages.AppLauncher_Server_ready, appName);
		} catch (final Exception e) {
			ServerActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
		}

	}

	public void stop() {
		ServerActivator.logStatus(IStatus.INFO, Messages.AppLauncher_Requested_server_termination, appName);

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
					ServerActivator.logStatus(IStatus.WARNING,
							Messages.AppLauncher_Application_not_terminated_correctly,
							process.getAttribute("org.eclipse.debug.core.ATTR_CMDLINE").trim()); //$NON-NLS-1$
				} else if (process.getExitValue() != 0) {
					ServerActivator.logStatus(IStatus.WARNING, appName,
							Messages.AppLauncher_Process_terminated_exit_code, process.getExitValue(),
							process.getAttribute("org.eclipse.debug.core.ATTR_CMDLINE").trim()); //$NON-NLS-1$
				}
			}

			ServerActivator.logStatus(IStatus.INFO, Messages.AppLauncher_Application_terminated, appFilename);
		} catch (final DebugException e) {
			e.printStackTrace();
		}
	}
}
