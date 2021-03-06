package br.com.totvs.tds.ui.monitor.model;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.services.IServiceLocator;

import br.com.totvs.tds.lsp.server.ILanguageServerService;
import br.com.totvs.tds.lsp.server.model.node.KillUserNode;
import br.com.totvs.tds.lsp.server.model.node.SendMessageToUserNode;
import br.com.totvs.tds.lsp.server.model.node.UsersInfoDataNode;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IServerPermissions;
import br.com.totvs.tds.ui.monitor.MonitorUIActivator;

public class ServerMonitor implements IServerMonitor {

	private IAppServerInfo server;
	private List<IUserMonitor> users = Collections.emptyList();
	private String stateString = ""; // $NON-NLS-1$

	public ServerMonitor(final IAppServerInfo server, final Map<String, IServerMonitor> parent) {
		this.server = server;
	}

	@Override
	public IAppServerInfo getServerInfo() {
		return server;
	}

	@Override
	public IUserMonitor createChildren(final List<String> user) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<IUserMonitor> getChildren() {

		return users;
	}

	@Override
	public void setChildren(final List<IUserMonitor> children) {
		if ((children == null) || (children.isEmpty())) {
			users = Collections.emptyList();
		} else {
			users = children;
		}
	}

	@Override
	public boolean isBlockedToConnection() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public String getServerName() {

		return server.getName();
	}

	@Override
	public String getEnvironment() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getMachine() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getThreadId() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getUserServer() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getProgram() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getConection() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getTimeElapsed() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getInstructions() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getInstructionsXSeconds() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getObservations() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getMemory() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getSID() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getRPO() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getTimeInactivity() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getTypeConnection() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setStateString(final String state) {
		stateString = state;
	}

	@Override
	public String getStateString() {
		return stateString;
	}

	@Override
	public List<IUserMonitor> getUsers() {
		final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
		final ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);
		final UsersInfoDataNode[] userList = lsService.getUsersInfo(getToken());
		final List<IUserMonitor> result = new ArrayList<IUserMonitor>();

		for (final UsersInfoDataNode usersInfo : userList) {
			final IUserMonitor userMonitor = new UserMonitor(this);

			userMonitor.setUsername(usersInfo.getUsername());
			userMonitor.setComputerName(usersInfo.getComputerName());
			userMonitor.setThreadId(usersInfo.getThreadId());
			userMonitor.setServer(usersInfo.getServer());
			userMonitor.setMainName(usersInfo.getMainName());
			userMonitor.setEnvironment(usersInfo.getEnvironment());
			userMonitor.setLoginTime(usersInfo.getLoginTime());
			userMonitor.setElapsedTime(usersInfo.getElapsedTime());
			userMonitor.setTotalInstrCount(usersInfo.getTotalInstrCount());
			userMonitor.setInstrCountPerSec(usersInfo.getInstrCountPerSec());
			userMonitor.setRemark(usersInfo.getRemark());
			userMonitor.setMemUsed(usersInfo.getMemUsed());
			userMonitor.setSid(usersInfo.getSid());
			userMonitor.setCtreeTaskId(usersInfo.getCtreeTaskId());
			userMonitor.setClientType(usersInfo.getClientType());
			userMonitor.setInactiveTime(usersInfo.getInactiveTime());

			result.add(userMonitor);
		}

		return result;
	}

	private String getToken() {
		return getServerInfo().getToken();
	}

	@Override
	public boolean isConnected() {

		return server.isConnected();
	}

	@Override
	public boolean isSendMessageEnabled() {

		return server.canDoOperation(IServerPermissions.OPER_SEND_MESSAGE);
	}

	@Override
	public boolean isDisconnectUserEnabled() {

		return server.canDoOperation(IServerPermissions.OPER_DISCONNECT_USER);
	}

	@Override
	public IStatus sendMessageUser(final String username, final String computerName, final long threadId,
			final String server, final String messageText) {

		final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
		final ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);
		final SendMessageToUserNode result = lsService.sendMessageUser(getToken(), username, computerName, threadId,
				server, messageText);

		if (result == null) {
			return MonitorUIActivator.logStatus(IStatus.ERROR,
					"Erro fatal ao enviar mensagem.\n\tServidor: %s\n\tUsu�rio: %s", server, username);
		}

		return MonitorUIActivator.logStatus(IStatus.INFO, "%s.\n\tServidor: %s\n\tUsu�rio: %s", result, server,
				username);
	}

	@Override
	public IStatus killUser(final boolean immediately, final String username, final String computerName,
			final long threadId, final String server) {

		final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
		final ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);
		final KillUserNode result = lsService.killUser(immediately, getToken(), username, computerName, threadId,
				server);

		if (result == null) {
			return MonitorUIActivator.logStatus(IStatus.ERROR,
					"Erro fatal ao desconectar usu�rio.\n\tServidor: %s\n\tUsu�rio: %s", server, username);
		}

		return MonitorUIActivator.logStatus(IStatus.INFO, "%s.\n\tServidor: %s\n\tUsu�rio: %s", result, server,
				username);
	}

}
