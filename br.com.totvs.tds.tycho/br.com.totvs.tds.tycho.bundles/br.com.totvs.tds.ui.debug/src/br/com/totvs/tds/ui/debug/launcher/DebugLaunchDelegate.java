package br.com.totvs.tds.ui.debug.launcher;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.StringJoiner;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.lsp4e.debug.DSPPlugin;
import org.eclipse.lsp4e.debug.debugmodel.DSPDebugTarget;
import org.eclipse.lsp4e.debug.launcher.DSPLaunchDelegate;

import com.google.gson.Gson;

import br.com.totvs.tds.lsp.server.ActivatorServer;
import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IServerConstants;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.ui.debug.DebugUIActivator;
import br.com.totvs.tds.ui.debug.helper.LaunchParameters;
import br.com.totvs.tds.ui.debug.model.DAPDebugTarget;

/**
 * The Class that defines the launcher.
 *
 * @author acandido
 *
 */
@SuppressWarnings("restriction")
public class DebugLaunchDelegate extends DSPLaunchDelegate {

	private LaunchParameters lp;
	private IAppServerInfo server;
	private String environment;
	private String modeTitle;

	@Override
	public void launch(final ILaunchConfiguration configuration, final String mode, final ILaunch launch,
			final IProgressMonitor monitor) throws CoreException {
		final ILaunchConfigurationWorkingCopy wk = configuration.getWorkingCopy();

		if (mode.equals("run")) { //$NON-NLS-1$
			modeTitle = Messages.DebugLaunchDelegate_Run;
		} else if (mode.equals("debug")) { //$NON-NLS-1$
			modeTitle = Messages.DebugLaunchDelegate_Debug;
		} else {
			modeTitle = Messages.DebugLaunchDelegate_Coverage;
		}
		monitor.setTaskName(String.format("%s TOTVS SmartClient", modeTitle)); //$NON-NLS-1$

		DebugUIActivator.logStatus(IStatus.INFO, modeTitle, Messages.DebugLaunchDelegate_Validating_configuration);
		final boolean ok = verifyLaunchConditions(wk);
		if (!ok) {
			return;
		}

		DebugUIActivator.logStatus(IStatus.INFO, modeTitle, Messages.DebugLaunchDelegate_Starting_SmartClient);

		if (monitor.isCanceled()) {
			DebugUIActivator.logStatus(IStatus.CANCEL, modeTitle,
					Messages.DebugLaunchDelegate_SmartClient_execution_canceled);
			return;
		}

		final List<String> commandList = new ArrayList<String>();
		final String[] commandLine = createSmartClientCommandLine(server, commandList);

		DebugUIActivator.logStatus(IStatus.INFO, modeTitle, Messages.DebugLaunchDelegate_Starting_external_process);

		if (lp.isShowCommandLine()) {
			final StringJoiner sj = new StringJoiner("]\n\t[", "[", "]"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

			sj.add(Messages.DebugLaunchDelegate_COMMAND_LINE_execution);
			for (final String value : commandLine) {
				sj.add(value);
			}

			DebugUIActivator.showStatus(IStatus.INFO, sj.toString());
		}

		final List<String> daargs = ActivatorServer.getInstance().getDAArgs();
		// ao iniciar, aguarda 30 segudos para que possa fazer um "attach"
		// daargs.add("--wait-for-attach");
		// daargs.add("30000"); // milisegundos
		//////////////////////////////////////////////

		wk.setAttribute(DSPPlugin.ATTR_DSP_MODE, DSPPlugin.DSP_MODE_LAUNCH);
		wk.setAttribute(DSPPlugin.ATTR_DSP_MONITOR_DEBUG_ADAPTER, false);
		wk.setAttribute(DSPPlugin.ATTR_DSP_ARGS, daargs);
		wk.setAttribute(DSPPlugin.ATTR_DSP_PARAM, getDspParam(mode, lp));
		wk.setAttribute(DSPPlugin.ATTR_DSP_PARAM, getDspParam(mode, lp));

		try {
			wk.setAttribute(DSPPlugin.ATTR_DSP_CMD, ActivatorServer.getInstance().getDACommand());
		} catch (final IOException e) {
			throw new CoreException(DebugUIActivator.showStatus(IStatus.ERROR, e.getMessage(), e));
		}

		super.launch(wk, mode, launch, monitor);
	}

	private String getDspParam(final String mode, final LaunchParameters lp) throws CoreException {
		final IServerManager serverManager = ServerActivator.getDefault().getServerManager();

		final IAppServerInfo server = serverManager.getCurrentServer();
		final String environment = server.getCurrentEnvironment();

		Map<String, Object> params;
		try {
			params = lp.toMap();
		} catch (final IllegalArgumentException e) {
			throw new CoreException(DebugUIActivator.logStatus(IStatus.ERROR, e.getMessage(), e));
		} catch (final IllegalAccessException e) {
			throw new CoreException(DebugUIActivator.logStatus(IStatus.ERROR, e.getMessage(), e));
		}

		params.put("type", "totvs_language_debug"); //$NON-NLS-1$ //$NON-NLS-2$
		params.put("serverAddress", server.getAddress().getHost()); //$NON-NLS-1$
		params.put("serverPort", server.getAddress().getPort()); //$NON-NLS-1$
		params.put("buildVersion", server.getVersion()); //$NON-NLS-1$
		params.put("environment", environment); //$NON-NLS-1$
		params.put("serverName", server.getName()); //$NON-NLS-1$
		params.put("authToken", server.getToken()); //$NON-NLS-1$
		// params.put("publicKey", server.getToken());
		params.put("noDebug", !mode.equals(ILaunchManager.DEBUG_MODE)); //$NON-NLS-1$
		params.put("smartclientBin", server.getSmartClientPath()); //$NON-NLS-1$

		final Gson gson = new Gson();
		return gson.toJson(params);
	}

	/**
	 * For extenders/consumers of {@link DSPLaunchDelegate} who want to provide
	 * customization of the IDebugTarget, this method allows extenders to hook in a
	 * custom debug target implementation. The debug target is normally a subclass
	 * of {@link DSPDebugTarget}, but does not have to be. The arguments to this
	 * method are normally just passed to {@link DSPDebugTarget} constructor.
	 */
	@Override
	protected IDebugTarget createDebugTarget(final SubMonitor subMonitor, final Runnable cleanup,
			final InputStream inputStream, final OutputStream outputStream, final ILaunch launch,
			final Map<String, Object> dspParameters) throws CoreException {
		final DAPDebugTarget target = new DAPDebugTarget(launch, cleanup, inputStream, outputStream, dspParameters);
		target.initialize(subMonitor.split(80));
		return target;
	}

	/**
	 * Run a set of verifications to confirm whether the execution can continue.
	 *
	 * @param configuration
	 * @param smartClientDirectory
	 * @return
	 * @throws CoreException
	 */
	@SuppressWarnings("unchecked")
	private boolean verifyLaunchConditions(final ILaunchConfiguration configuration) throws CoreException {
		final Map<String, Object> params = (Map<String, Object>) configuration.getAttributes()
				.getOrDefault(IServerConstants.LAUNCH_PARAMETERS, null);
		this.lp = new LaunchParameters(params);

		if (lp == null) {
			throw new CoreException(DebugUIActivator.showStatus(IStatus.ERROR,
					Messages.DebugLaunchDelegate_Executor_parameters_invalid));
		}

		final IServerManager serverManager = ServerActivator.getDefault().getServerManager();
		server = serverManager.getCurrentServer();
		if (server == null) {
			throw new CoreException(
					DebugUIActivator.showStatus(IStatus.ERROR, Messages.DebugLaunchDelegate_No_server_selected));
		}

		if (!server.isConnected()) {
			throw new CoreException(DebugUIActivator.showStatus(IStatus.ERROR, server.getName()));
		}

		environment = server.getCurrentEnvironment();
		if ((environment == null) || (environment.isEmpty())) {
			throw new CoreException(
					DebugUIActivator.showStatus(IStatus.ERROR, Messages.DebugLaunchDelegate_No_environment_selected));
		}

		final File smartClientFile = new File(server.getSmartClientPath());
		if (!(smartClientFile.exists() && smartClientFile.canExecute())) {
			throw new CoreException(DebugUIActivator.showStatus(IStatus.ERROR, smartClientFile.getPath()));
		}

		if (lp.getMainProgram().isEmpty()) {
			throw new CoreException(
					DebugUIActivator.showStatus(IStatus.ERROR, Messages.DebugLaunchDelegate_Main_function_required));
		}

		if (lp.getMainProgram().equalsIgnoreCase(IServerConstants.CANCELED)) {
			DebugUIActivator.logStatus(IStatus.CANCEL, modeTitle, Messages.DebugLaunchDelegate_Process_canceled);
			return false;
		}

		return true;
	}

	/**
	 * Creates the smartClient parameters to be executed.<br>
	 * It is necessary that the parameter commandList already contains in the first
	 * position, the directory SmartClient.
	 *
	 * @param commandList
	 * @param configuration
	 * @return
	 * @throws CoreException
	 */
	private String[] createSmartClientCommandLine(final IAppServerInfo server, final List<String> commandList)
			throws CoreException {
		commandList.add(server.getSmartClientPath()); // $NON-NLS-1$

		commandList.add("-Y=" + String.valueOf(server.getAppServerPort())); //$NON-NLS-1$
		commandList.add("-Z=" + (server.getAddress().getHost())); //$NON-NLS-1$
		commandList.add("-E=" + server.getCurrentEnvironment()); //$NON-NLS-1$

		commandList.addAll(lp.getLineArguments());

		final String[] commandLine = commandList.toArray(new String[commandList.size()]);

		return commandLine;
	}

}