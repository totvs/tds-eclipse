package br.com.totvs.tds.ui.monitor.model;

import java.util.List;

import org.eclipse.core.runtime.IStatus;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;

public interface IServerMonitor extends IItemMonitor {

	IAppServerInfo getServerInfo();

	IUserMonitor createChildren(List<String> user);

	List<IUserMonitor> getChildren();

	void setChildren(List<IUserMonitor> children);

	boolean isBlockedToConnection();

	String getServerName();

	String getEnvironment();

	String getMachine();

	String getThreadId();

	String getUserServer();

	String getProgram();

	String getConection();

	String getTimeElapsed();

	String getInstructions();

	String getInstructionsXSeconds();

	String getObservations();

	String getMemory();

	String getSID();

	String getRPO();

	String getTimeInactivity();

	String getTypeConnection();

	void setStateString(String state);

	String getStateString();

	List<IUserMonitor> getUsers();

	boolean isConnected();

	boolean isSendMessageEnabled();

	IStatus sendMessageUser(String username, String computerName, long threadId, String server, String messageText);

	IStatus killUser(boolean immediately, String username, String computerName, long threadId, String server);

	boolean isDisconnectUserEnabled();

}