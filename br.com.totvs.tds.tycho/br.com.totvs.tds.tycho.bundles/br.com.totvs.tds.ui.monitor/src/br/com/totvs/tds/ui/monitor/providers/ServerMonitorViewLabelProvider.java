package br.com.totvs.tds.ui.monitor.providers;

import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.swt.graphics.Image;

import br.com.totvs.tds.ui.monitor.MonitorUIIcons;
import br.com.totvs.tds.ui.monitor.model.IServerMonitor;

/**
 * Label provider da visão "Monitor Sevidores".
 *
 * @author Matheus.Sales
 *
 */
public class ServerMonitorViewLabelProvider implements ITableLabelProvider {

	// Imagens do monitor
	private static final Image SERVIDOR = MonitorUIIcons.getServer().createImage(true);
	private static final Image SERVIDOR_BLOQUEADO = MonitorUIIcons.getBlockedServer().createImage(true);
	private static final Image USUARIO = MonitorUIIcons.getUser().createImage(true);

	// Colunas para server Protheus e Logix
	private final int SERVER_NAME = 0;
	private final int ENVIRONMENT = 1;
	private final int COMPUTER_NAME = 2;
	private final int THREAD_ID = 3;
	private final int USER_SERVER = 4;
	private final int PROGRAM = 5;
	private final int CONECTION = 6;
	private final int TIME_ELAPSED = 7;
	private final int INSTRUCTION = 8;
	private final int INSTRUCTION_PER_SECONDS = 9;
	private final int OBSERVATION = 10;
	private final int MEMORY = 11;
	private final int SID = 12;
	private final int RPO = 13;
	private final int INACTIVITY = 14;
	private final int TYPE_CONNECTION = 15;

	// Colunas para server DBACCESS
	// private final int SERVER_NAME = 0;
	// private final int ENVIRONMENT = 1;
//	private final int ID = 2;
//	private final int USER = 3;
//	private final int IOS_TOTALS = 4;
//	private final int TABLES = 5;
//	private final int COMMENTS = 6;
//	private final int PROCEDURE = 7;
//	private final int DBTHREAD = 8;
//	private final int START = 9;
//	private final int IP = 10;

	@Override
	public Image getColumnImage(final Object element, final int columnIndex) {
		if (columnIndex == 0) {
			if (element instanceof IServerMonitor) {
				final IServerMonitor itemMonitor = (IServerMonitor) element;

				if (itemMonitor.isBlockedToConnection()) {
					return SERVIDOR_BLOQUEADO;
				} else {
					return SERVIDOR;
				}
			} else {
				return USUARIO;
			}
		}

		return null;

	}

	@Override
	public String getColumnText(final Object obj, final int columnIndex) {
		String returnValue = null;
		if (obj instanceof IServerMonitor) {
			final IServerMonitor itemMonitor = (IServerMonitor) obj;

			switch (columnIndex) {
			case SERVER_NAME:
				returnValue = itemMonitor.getServerName();
				// returnValue = itemMonitor.getUser();
				break;
			case ENVIRONMENT:
				returnValue = String.format("%d threads", itemMonitor.getChildren().size());
				// returnValue = itemMonitor.getEnvironment();
				break;
			case COMPUTER_NAME:
				if (!itemMonitor.getStateString().isEmpty()) {
					returnValue = String.format("(%s)", itemMonitor.getStateString().toLowerCase());
				}
				// returnValue = itemMonitor.getMachine();
				break;
			case THREAD_ID:
				returnValue = itemMonitor.getThreadId();
				break;
			case USER_SERVER:
				returnValue = itemMonitor.getUserServer();
				break;
			case PROGRAM:
				returnValue = itemMonitor.getProgram();
				break;
			case CONECTION:
				returnValue = itemMonitor.getConection();
				break;
			case TIME_ELAPSED:
				returnValue = itemMonitor.getTimeElapsed();
				break;
			case INSTRUCTION:
				returnValue = itemMonitor.getInstructions();
				break;
			case INSTRUCTION_PER_SECONDS:
				returnValue = itemMonitor.getInstructionsXSeconds();
				break;
			case OBSERVATION:
				returnValue = itemMonitor.getObservations();
				break;
			case MEMORY:
				returnValue = itemMonitor.getMemory();
				break;
			case SID:
				returnValue = itemMonitor.getSID();
				break;
			case RPO:
				returnValue = itemMonitor.getRPO();
				break;
			case INACTIVITY:
				returnValue = itemMonitor.getTimeInactivity();
				break;
			case TYPE_CONNECTION:
				returnValue = itemMonitor.getTypeConnection();
				break;
			default:
				break;
			}
		} else {
//			switch (columnIndex) {
//			case SERVER_NAME:
//				returnValue = itemMonitor.getServerName();
//				break;
//			case ENVIRONMENT:
//				returnValue = itemMonitor.getEnvironment();
//				break;
//			case ID:
//				returnValue = itemMonitor.getThreadId();
//				break;
//			case USER:
//				returnValue = itemMonitor.getUser();
//				break;
//			case IOS_TOTALS:
//				returnValue = itemMonitor.getTotalIos();
//				break;
//			case TABLES:
//				returnValue = itemMonitor.getTables();
//				break;
//			case COMMENTS:
//				returnValue = itemMonitor.getComments();
//				break;
//			case PROCEDURE:
//				returnValue = itemMonitor.getProcedure();
//				break;
//			case DBTHREAD:
//				returnValue = itemMonitor.getDbThread();
//				break;
//			case START:
//				returnValue = itemMonitor.getStart();
//				break;
//			case IP:
//				returnValue = itemMonitor.getIP();
//				break;
			// }
		}

		return returnValue;
	}

	@Override
	public void addListener(final ILabelProviderListener listener) {
	}

	@Override
	public void dispose() {
	}

	@Override
	public boolean isLabelProperty(final Object element, final String property) {
		return false;
	}

	@Override
	public void removeListener(final ILabelProviderListener listener) {
	}

}
