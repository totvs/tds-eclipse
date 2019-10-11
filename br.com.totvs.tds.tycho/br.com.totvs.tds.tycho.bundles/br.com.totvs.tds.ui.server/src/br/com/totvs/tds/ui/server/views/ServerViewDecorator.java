package br.com.totvs.tds.ui.server.views;

import java.util.List;

import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.jface.viewers.IFontDecorator;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ILightweightLabelDecorator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.widgets.Display;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IAppServerSlaveInfo;
import br.com.totvs.tds.server.interfaces.IEnvironmentInfo;
import br.com.totvs.tds.server.interfaces.IServerInfo;
import br.com.totvs.tds.server.interfaces.IServerInfo.ServerType;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.server.interfaces.IServerSlaveHubInfo;
import br.com.totvs.tds.ui.server.ServerUIIcons;

/**
 * @author acandido
 */
public class ServerViewDecorator implements ILightweightLabelDecorator, IFontDecorator {

	public static final String SERVER_DECORATOR_ID = "br.com.totvs.tds.ui.server.serverViewDecorator"; //$NON-NLS-1$

	private static final Font ACTIVE_FONT;
	private static final Font CURRENT_FONT;

	static {
		FontData fd = new FontData();
		fd.setStyle(SWT.BOLD);
		fd.setHeight(9);
		CURRENT_FONT = new Font(Display.getDefault(), fd);
		//
		fd = new FontData();
		fd.setStyle(SWT.ITALIC);
		fd.setHeight(9);
		ACTIVE_FONT = new Font(Display.getDefault(), fd);
	}

	private final IServerManager serverManager;

	/**
	 * Construtor b�sico.
	 */
	public ServerViewDecorator() {
		serverManager = ServerActivator.getDefault().getServerManager();
	}

	@Override
	public void addListener(final ILabelProviderListener listener) {

	}

	/**
	 * @param element
	 * @param decoration
	 */
	private void decorate(final IAppServerSlaveInfo element, final IDecoration decoration) {
		decoration.addOverlay(ServerUIIcons.getSlave(), IDecoration.BOTTOM_RIGHT);

		if (ServerType.PROTHEUS.equals(element.getServerType())) {
			decoration.addOverlay(ServerUIIcons.getProtheus(), IDecoration.TOP_LEFT);
		} else if (ServerType.LOGIX.equals(element.getServerType())) {
			decoration.addOverlay(ServerUIIcons.getLogix(), IDecoration.TOP_LEFT);
		}

	}

	private void decorate(final IEnvironmentInfo element, final IDecoration decoration) {
		IEnvironmentInfo envInfo = element;
		IAppServerInfo serverInfo = (IAppServerInfo) element.getParent();
		String currentEnvironment = serverInfo.getCurrentEnvironment();

		if (element.isCredentialValidated() && envInfo.getName().equals(currentEnvironment.toUpperCase())) {
			decoration.addOverlay(ServerUIIcons.getSelectedEnvironment(), IDecoration.TOP_RIGHT);
		}

		List<String> multiEnvironmentSelection = serverInfo.getMultiEnvironmentSelection();
		if (multiEnvironmentSelection.contains(envInfo.getName())) {
			decoration.addOverlay(ServerUIIcons.getMultiEnvironment(), IDecoration.BOTTOM_LEFT);
		}
		decoration.setFont(decorateFont(element));
	}

	/**
	 * @param element
	 * @param decoration
	 */
	private void decorate(final IServerInfo element, final IDecoration decoration) {
		element.getPersistentPropertyBoolean("isDuplicated"); //$NON-NLS-1$

		if (element.isConnected()) {
			decoration.addOverlay(ServerUIIcons.getConnected(), IDecoration.BOTTOM_RIGHT);
			decoration.setFont(decorateFont(element));
			if (element.isBlockedToConnection()) {
				decoration.addOverlay(ServerUIIcons.getBlocked(), IDecoration.TOP_RIGHT);
			}
		}

		if (element instanceof IAppServerInfo) {
			IAppServerInfo appServerInfo = (IAppServerInfo) element;

			if (ServerType.PROTHEUS.equals(element.getServerType())) {
				decoration.addOverlay(ServerUIIcons.getProtheus(), IDecoration.TOP_LEFT);
			} else if (ServerType.LOGIX.equals(element.getServerType())) {
				decoration.addOverlay(ServerUIIcons.getLogix(), IDecoration.TOP_LEFT);
			}

			if (appServerInfo.isRunning()) {
				decoration.addOverlay(ServerUIIcons.getRunning());
			}
		}

	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.eclipse.jface.viewers.ILightweightLabelDecorator#decorate(java.lang.
	 * Object, org.eclipse.jface.viewers.IDecoration)
	 */
	@Override
	public void decorate(final Object element, final IDecoration decoration) {
		if (element instanceof IServerSlaveHubInfo) {
			// doNothing
		} else if (element instanceof IServerInfo) {
			decorate((IServerInfo) element, decoration);
		} else if (element instanceof IEnvironmentInfo) {
			decorate((IEnvironmentInfo) element, decoration);
		} else if (element instanceof IAppServerSlaveInfo) {
			decorate((IAppServerSlaveInfo) element, decoration);
		}

	}

	@Override
	public Font decorateFont(final Object element) {
		Font font = null;
		//
		IServerInfo currentServer = serverManager.getCurrentServer();
		if (element instanceof IServerInfo) {
			if (element.equals(currentServer)) {
				font = CURRENT_FONT;
			}
		} else if (element instanceof IEnvironmentInfo) {
			IEnvironmentInfo envInfo = (IEnvironmentInfo) element;
			IAppServerInfo serverInfo = (IAppServerInfo) envInfo.getParent();
			String currentEnvironment = serverInfo.getCurrentEnvironment();
			List<String> multiEnvironmentSelection = serverInfo.getMultiEnvironmentSelection();
			if (currentServer != null && currentServer.equals(serverInfo)
					&& envInfo.getName().equalsIgnoreCase(currentEnvironment)) {
				font = CURRENT_FONT;
			} else if (multiEnvironmentSelection.contains(envInfo.getName())) {
				font = ACTIVE_FONT;
			}
		}
		//
		return font;
	}

	@Override
	public void dispose() {
		// nothing to do
	}

	@Override
	public boolean isLabelProperty(final Object element, final String property) {
		return false;
	}

	@Override
	public void removeListener(final ILabelProviderListener listener) {
	}

}
