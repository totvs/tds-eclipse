package br.com.totvs.tds.ui.debug.model;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.lsp4e.debug.debugmodel.DSPDebugTarget;
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification;

import br.com.totvs.tds.ui.debug.DebugUIActivator;

@SuppressWarnings("restriction")
public class DAPDebugTarget extends DSPDebugTarget {

	public DAPDebugTarget(final ILaunch launch, final Runnable processCleanup, final InputStream in,
			final OutputStream out, final Map<String, Object> dspParameters) {

		super(launch, processCleanup, in, out, dspParameters);
	}

	@Override
	public void terminate() throws DebugException {
		super.terminate();
	}

	@JsonNotification("TDA/log")
	public final void log(final LogData log) {

		if (log.isNotify()) {
			if (log.getLevel().equals("INFO")) {
				DebugUIActivator.showStatus(IStatus.INFO, "Depuração", "[%s] %s", log.getTime(), log.getMessage());
			} else {
				DebugUIActivator.showStatus(IStatus.OK, "*****", "[%s] %s", log.getTime(), log.getMessage());
			}
		} else {
			if (log.getLevel().equals("INFO")) {
				DebugUIActivator.logStatus(IStatus.INFO, "Depuração", "[%s] %s", log.getTime(), log.getMessage());
			} else {
				DebugUIActivator.logStatus(IStatus.OK, "*****", "[%s] %s", log.getTime(), log.getMessage());
			}
		}
	}

}
