package br.com.totvs.tds.ui.server.status;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.rmi.ServerException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringJoiner;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.NotEnabledException;
import org.eclipse.core.commands.NotHandledException;
import org.eclipse.core.commands.ParameterizedCommand;
import org.eclipse.core.commands.common.NotDefinedException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.menus.WorkbenchWindowControlContribution;
import org.eclipse.ui.services.IServiceLocator;
import org.eclipse.wb.swt.ResourceManager;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IEnvironmentInfo;
import br.com.totvs.tds.server.interfaces.IOrganization;
import br.com.totvs.tds.server.interfaces.IServerInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.server.interfaces.ISubsidiary;
import br.com.totvs.tds.server.interfaces.IServerInfo.ServerType;
import br.com.totvs.tds.ui.server.ServerUIActivator;
import br.com.totvs.tds.ui.server.ServerUIIcons;
import br.com.totvs.tds.ui.server.nl.Messages;

/**
 * Contribuições para a barra de Status.
 *
 * @author acandido
 */
public class StatusLineControl extends WorkbenchWindowControlContribution implements PropertyChangeListener {
	/**
	 * Item de menu para seleção de ambiente.
	 *
	 * @author acandido
	 */
	private class EnvironmentMenuItemAction implements SelectionListener {

		@Override
		public void widgetDefaultSelected(final SelectionEvent e) {
		}

		@Override
		public void widgetSelected(final SelectionEvent e) {
			final MenuItem menuItem = ((MenuItem) e.getSource());
			final MenuItem parentItem = menuItem.getParent().getParentItem();
			final IAppServerInfo server = (IAppServerInfo) parentItem.getData("server"); //$NON-NLS-1$

			Display.getCurrent().asyncExec(new Runnable() {
				@Override
				public void run() {
					server.setCurrentEnvironment(menuItem.getText());
					serverManager.setCurrentServer(server);
				}
			});
		}
	}

	/**
	 * Item de menu para seleção de ambiente.
	 *
	 * @author acandido
	 */
	private class ServerMenuItemAction implements SelectionListener {

		@Override
		public void widgetDefaultSelected(final SelectionEvent e) {
		}

		@Override
		public void widgetSelected(final SelectionEvent e) {
			final MenuItem menuItem = ((MenuItem) e.getSource());
			final IAppServerInfo server = (IAppServerInfo) menuItem.getData("server"); //$NON-NLS-1$

			IServiceLocator serviceLocator = PlatformUI.getWorkbench();
			ICommandService commandService = serviceLocator.getService(ICommandService.class);

			Command command = null;
			if (server.isConnected()) {
				command = commandService.getCommand("br.com.totvs.tds.ui.server.commands.disconnectCommand"); //$NON-NLS-1$
			} else {
				command = commandService.getCommand("br.com.totvs.tds.ui.server.commands.connectCommand"); //$NON-NLS-1$
			}

			command.setEnabled(server);
			Map<String, Object> parameters = new HashMap<String, Object>();
			parameters.put("server", server.getName()); //$NON-NLS-1$

			ParameterizedCommand pc = ParameterizedCommand.generateCommand(command, parameters);
			IHandlerService handlerService = serviceLocator.getService(IHandlerService.class);

			try {
				handlerService.executeCommand(pc, null);
			} catch (ExecutionException e1) {
				ServerActivator.logStatus(IStatus.ERROR, "Seleção", e1.getMessage(), e1);
			} catch (NotDefinedException e1) {
				ServerActivator.logStatus(IStatus.ERROR, "Seleção", e1.getMessage(), e1);
			} catch (NotEnabledException e1) {
				ServerActivator.logStatus(IStatus.ERROR, "Seleção", e1.getMessage(), e1);
			} catch (NotHandledException e1) {
				ServerActivator.logStatus(IStatus.ERROR, "Seleção", e1.getMessage(), e1);
			}

		}
	}

	private Composite composite;
	private Menu popupMenu;

	private final IServerManager serverManager;
	private ToolBar toolBar;
	private ToolItem tbUserItem;

	private ToolItem tbCompileKeyItem;
	private ToolItem tbServerItem;
	private ToolItem tbOrganizationItem;
	private SelectionListener serverMenuItemAction = new ServerMenuItemAction();
	private SelectionListener environmentMenuItemAction = new EnvironmentMenuItemAction();

	/**
	 * @wbp.parser.constructor
	 */
	public StatusLineControl() {
		super();

		serverManager = ServerActivator.getDefault().getServerManager();

		hookNotifications();
	}

	private void addServerIntoMenuList(final IAppServerInfo server) {
		final String activeEnvironment = server.getCurrentEnvironment();
		final IServerInfo activeServer = serverManager.getCurrentServer();
		final MenuItem menuItem = new MenuItem(popupMenu, SWT.CASCADE);

		menuItem.setText(" " + server.getName()); //$NON-NLS-1$
		menuItem.setData("server", server); //$NON-NLS-1$

		if (activeServer != null && server.getName().equals(activeServer.getName())) {
			menuItem.setImage(ServerUIIcons.getOk().createImage());
		} else if (server.isConnected()) {
			menuItem.setImage(ServerUIIcons.getConnected().createImage());
		} else {
			menuItem.setImage(ServerUIIcons.getServer().createImage());
		}

		try {
			if (server.isConnected()) {

				final Menu menuEnvironment = new Menu(menuItem);
				final List<String> listEnvironments = getEnvironmentsNameList(server.getEnvironments());
				final MenuItem disconnectMenuItem = new MenuItem(menuEnvironment, SWT.PUSH);
				disconnectMenuItem.setText("Desconectar");
				disconnectMenuItem.setMenu(menuEnvironment);
				disconnectMenuItem.addSelectionListener(serverMenuItemAction);

				Collections.sort(listEnvironments);
				for (final String environment : listEnvironments) {
					final MenuItem itemEnvironment = new MenuItem(menuEnvironment, SWT.PUSH);
					itemEnvironment.setText(environment);
					menuItem.setMenu(menuEnvironment);

					itemEnvironment.addSelectionListener(environmentMenuItemAction);
					if (activeServer != null && server.getName().equals(activeServer.getName()) && environment != null
							&& environment.equals(activeEnvironment)) {
						itemEnvironment.setImage(ServerUIIcons.getOk().createImage());
					}
				}
			} else {
				menuItem.addSelectionListener(serverMenuItemAction);
			}
		} catch (final Exception e) {
			ServerUIActivator.logStatus(IStatus.ERROR, Messages.StatusLineControl_status, e.getMessage(), e);
		}
	}

	/**
	 * @wbp.parser.entryPoint
	 */
	@Override
	protected Control createControl(final Composite parent) {
		composite = new Composite(parent, SWT.FILL);
		composite.setLayout(new GridLayout(1, false));

		this.toolBar = new ToolBar(this.composite, SWT.FLAT | SWT.RIGHT);
		GridData gd_toolBar = new GridData();
		gd_toolBar.horizontalAlignment = SWT.FILL;
		toolBar.setLayoutData(gd_toolBar);

		this.tbServerItem = new ToolItem(this.toolBar, SWT.NONE);
		this.tbServerItem.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				fillMenuWithServers();
				popupMenu.setVisible(true);
			}
		});
		this.tbServerItem.setImage(ResourceManager.getPluginImage("br.com.totvs.tds.ui.server", "icons/server.png"));

		this.tbUserItem = new ToolItem(this.toolBar, SWT.NONE);
		this.tbUserItem.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				System.out.println("StatusLineControl.fill(...).new SelectionAdapter() {...}.widgetSelected()"); //$NON-NLS-1$
			}
		});
		this.tbUserItem.setImage(ResourceManager.getPluginImage("br.com.totvs.tds.ui.server", "icons/user.png"));

		this.tbCompileKeyItem = new ToolItem(this.toolBar, SWT.NONE);
		this.tbCompileKeyItem.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				PreferencesUtil.createPreferenceDialogOn(Display.getCurrent().getActiveShell(),
						"br.com.totvs.tds.ui.sdk.preference.compilation.compileKeyPreferencePage",
						new String[] { "br.com.totvs.tds.ui.sdk.preference.compilation.compileKeyPreferencePage" },
						null).open();
			}
		});
		this.tbCompileKeyItem.setImage(ServerUIIcons.getCompileKeyUnlock().createImage());
		this.tbCompileKeyItem.setDisabledImage(ServerUIIcons.getCompileKeyLock().createImage());

		this.tbOrganizationItem = new ToolItem(this.toolBar, SWT.NONE);
		this.tbOrganizationItem.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				openSelectOrganization();
			}
		});
		this.tbOrganizationItem
				.setImage(ResourceManager.getPluginImage("br.com.totvs.tds.ui.server", "icons/organizations.png"));

		updateStatusPanel();

		composite.pack(true);

		return composite;
	}

	@Override
	public void dispose() {
		unhookNotifications();
		super.dispose();
	}

	/**
	 * Preenche o menu com as opções de servidores e ambiente dispon�veis
	 *
	 * @throws ServerException
	 */
	private void fillMenuWithServers() {
		popupMenu = new Menu(tbServerItem.getParent());
		tbServerItem.getParent().setMenu(popupMenu);

		final IAppServerInfo activerServer = serverManager.getCurrentServer();
		if ((activerServer != null) && activerServer.isConnected()) {
			final MenuItem serverItems = new MenuItem(popupMenu, SWT.PUSH);
			serverItems.setEnabled(false);
			serverItems.setText(Messages.StatusLineControl_active);
			addServerIntoMenuList(activerServer);
		}

		List<IServerInfo> servers = serverManager.getActiveServers(IAppServerInfo.class);
		servers.remove(activerServer);

		if (!servers.isEmpty()) {
			getOrderedActiveServers(servers);
			final MenuItem serverItems = new MenuItem(popupMenu, SWT.PUSH);
			serverItems.setText(Messages.StatusLineControl_connecteds);
			serverItems.setEnabled(false);
			for (final IServerInfo server : servers) {
				if (server instanceof IAppServerInfo)
					addServerIntoMenuList((IAppServerInfo) server);
			}
		}

		servers = serverManager.getInactiveServers(IAppServerInfo.class);
		if (!servers.isEmpty()) {
			getOrderedActiveServers(servers);
			final MenuItem connectedServersTitle = new MenuItem(popupMenu, SWT.PUSH);
			connectedServersTitle.setEnabled(false);
			connectedServersTitle.setText(Messages.StatusLineControl_not_connecteds);
			for (final IServerInfo server : servers) {
				if (server instanceof IAppServerInfo)
					addServerIntoMenuList((IAppServerInfo) server);
			}
		}

	}

	private List<String> getEnvironmentsNameList(final List<IEnvironmentInfo> environments) {
		final List<String> envNameList = new ArrayList<String>();

		for (final IEnvironmentInfo envInfo : environments) {
			envNameList.add(envInfo.getName());
		}
		return envNameList;
	}

	/**
	 * Organiza os servidores em ordem alfabética.
	 *
	 * @param unorderedList Lista que será organizada
	 */
	private void getOrderedActiveServers(final List<IServerInfo> unorderedList) {
		Collections.sort(unorderedList, new Comparator<IServerInfo>() {
			@Override
			public int compare(final IServerInfo o1, final IServerInfo o2) {
				return o1.getName().compareTo(o2.getName());
			}
		});
	}

	private void hookNotifications() {
		serverManager.addPropertyChangeListener(this);
	}

	// Necessário para realizar o redimensionamento do status bar
	@Override
	public boolean isDynamic() {
		return false;
	}

	@Override
	public boolean isGroupMarker() {

		return true;
	}

	protected void openSelectOrganization() {
		final IAppServerInfo server = serverManager.getCurrentServer();
		if (server != null) {
			final ServerType serverType = server.getServerType();

			if (ServerType.PROTHEUS.equals(serverType)) { // $NON-NLS-1$
				IAppServerInfo currentServer = serverManager.getCurrentServer();
				List<IOrganization> organizations = currentServer.getOrganizations();

				final SelectOrganizationDialog dialog = new SelectOrganizationDialog(composite.getShell(),
						server.getName(), server.getCurrentEnvironment(), organizations,
						server.getCurrentOrganization());
				if (dialog.open() == IDialogConstants.OK_ID) {
					final IOrganization companySelected = dialog.getSelection();
					server.setCurrentCompany(companySelected);
				}
			} else {
				ServerUIActivator.logStatus(IStatus.ERROR, Messages.StatusLineControl_status,
						Messages.StatusLineControl_invalid_operation);
			}
		} else {
			ServerUIActivator.logStatus(IStatus.ERROR, Messages.StatusLineControl_status,
					Messages.StatusLineControl_select_server_and_environment);
		}
	}

	@Override
	public void propertyChange(PropertyChangeEvent evt) {
		if (evt.getPropertyName().equals("currentServer")) { //$NON-NLS-1$
			updateStatusPanel();
		} else if (evt.getPropertyName().equals("currentCompany")) { //$NON-NLS-1$
			updateStatusPanel();
		} else if (evt.getPropertyName().equals("connected")) { //$NON-NLS-1$
			updateStatusPanel();
		} else if (evt.getPropertyName().equals("environments")) { //$NON-NLS-1$
			updateStatusPanel();
		} else if (evt.getPropertyName().equals("currentOrganization")) { //$NON-NLS-1$
			updateStatusPanel();
		}

	}

	private void unhookNotifications() {
		serverManager.removePropertyChangeListener(this);
	}

	private void updateStatusPanel() {
		final IAppServerInfo server = serverManager.getCurrentServer();
		final String environment = (server == null) ? "" : server.getCurrentEnvironment(); //$NON-NLS-1$

		tbServerItem.setText(Messages.StatusLineControl_no_selected);

		tbOrganizationItem.setText(Messages.StatusLineControl_organization);
		tbUserItem.setText(Messages.StatusLineControl_unknow);
		tbCompileKeyItem.setText(Messages.EMPTY_STRING);

		updateTextServer(server, environment);
		updateTextUser(server, environment);
		updateTextOrganization(server, environment);

		composite.pack(true);
	}

	/*
	 * Valida o label de seleção de empresa/filial.
	 */
	private void updateTextOrganization(final IAppServerInfo server, final String environment) {
		if (server != null && server.isConnected()) {
			final IOrganization organization = server.getCurrentOrganization();
			if (organization != null) {
				final ISubsidiary subsidiary = organization.getCurrentSubsidiary();
				if (subsidiary != null) {
					tbOrganizationItem
							.setText(String.format("[%s/%s] %s/%s", organization.getCode(), subsidiary.getCode(), //$NON-NLS-1$
									organization.getName(), subsidiary.getName()));
//					tbOrganizationItem.setToolTipText(String.format("[%s/%s] %s/%s", organization.getCode(), //$NON-NLS-1$
//							subsidiary.getCode().trim(), organization.getName().trim(), subsidiary.getName().trim()));
				}
			}
		}
	}

	/*
	 * Valida o label de seleção de servidores.
	 */
	private void updateTextServer(final IAppServerInfo server, String environment) {
		String serverName = ""; //$NON-NLS-1$
		String serverNameToolTip = Messages.EMPTY_STRING;
		String environmentToolTip = Messages.EMPTY_STRING;
		boolean enabled = false;

		if ((server != null) && (environment != null)) {
			enabled = ServerType.PROTHEUS.equals(server.getServerType());
			serverName = server.getName().toLowerCase();
			serverNameToolTip = serverName.toUpperCase();
			environment = environment.toLowerCase();
			environmentToolTip = environment.toUpperCase();

			if (serverName.length() >= 15) {
				serverName = serverName.substring(0, 12);
				serverName += "..."; //$NON-NLS-1$
			}

			if (environment.length() >= 15) {
				environment = environment.substring(0, 12);
				environment += "..."; //$NON-NLS-1$
			}

			tbServerItem.setText(String.format("%s [%s]", serverName, environment)); //$NON-NLS-1$
			tbServerItem.setToolTipText(String.format("%s [%s]", serverNameToolTip, environmentToolTip)); // $NON-NLS-1$
		} else {
			tbServerItem.setToolTipText(""); // $NON-NLS-1$
		}

		tbOrganizationItem.setEnabled(enabled);
	}

	/*
	 * Atualiza informações sobre usuário.
	 */
	private void updateTextUser(final IAppServerInfo server, final String environment) {
		boolean enabledKey = false;
		boolean enabled = false;

		if (server != null && server.isConnected()) {
			enabled = ServerType.PROTHEUS.equals(server.getServerType());
			enabledKey = enabled && server.isConnected();

			// String compileKey = server.getPermimissionToken();
			final List<String> permissions = server.getPermissions();
			final String user = server.getUsername();

			StringJoiner sb = new StringJoiner("|", "[", "]");
			for (String permission : permissions) {
				sb.add(permission);

				enabledKey |= permission.equals("COMPILE");
			}

			tbUserItem.setText(user);
			tbUserItem.setToolTipText(String.format(Messages.StatusLineControl_permissions, sb.toString()));
		} else {
			tbUserItem.setToolTipText("");
		}

		tbUserItem.setEnabled(enabled);
		tbCompileKeyItem.setEnabled(enabledKey);
		tbCompileKeyItem.setToolTipText(tbUserItem.getToolTipText());
	}

//
//	@Override
//	public void selectionChanged(IWorkbenchPart part, ISelection selection) {
//		// TODO Auto-generated method stub
//		System.out.println("StatusLineControl.selectionChanged()"); //$NON-NLS-1$
//		System.out.println(part.getTitle());
//		System.out.println(selection.toString());
//
//	}

}
