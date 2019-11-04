package br.com.totvs.tds.ui.monitor.model;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.services.IServiceLocator;

import br.com.totvs.tds.lsp.server.ILanguageServerService;
import br.com.totvs.tds.lsp.server.model.node.UsersInfoNode;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;

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
		// TODO Auto-generated method stub
		return users;
	}

	@Override
	public void setChildren(final List<IUserMonitor> children) {
		// TODO Auto-generated method stub

	}

	@Override
	public void addChildren(final IUserMonitor itemUser) {
		// TODO Auto-generated method stub

	}

	@Override
	public void removeChildren(final IUserMonitor itemMonitor) {
		// TODO Auto-generated method stub

	}

	@Override
	public void removeChildrenAll() {
		// TODO Auto-generated method stub

	}

	@Override
	public void setParent(final IItemMonitor parent) {
		// TODO Auto-generated method stub

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
		final UsersInfoNode[] userList = lsService.getUsersInfo(getToken());
		final List<IUserMonitor> result = new ArrayList<IUserMonitor>();

		for (final UsersInfoNode usersInfo : userList) {
			System.out.println(usersInfo);
		}

		return result;
	}

	private String getToken() {
		return getServerInfo().getToken();
	}

}
