package br.com.totvs.tds.ui.debug.model;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.lsp4e.debug.debugmodel.DSPDebugTarget;
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification;

import br.com.totvs.tds.ui.debug.DebugUIActivator;

@SuppressWarnings("restriction")
public class TDSDebugTarget extends DSPDebugTarget {

	public TDSDebugTarget(ILaunch launch, Runnable processCleanup, InputStream in, OutputStream out,
			Map<String, Object> dspParameters) {

		super(launch, processCleanup, in, out, dspParameters);
	}
	
	@JsonNotification("TDA/log")
	public final void log(LogData log) {
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
