package br.com.totvs.tds.ui.monitor.model;

public interface IUserMonitor extends IItemMonitor {

	void setInactiveTime(String inactiveTime);

	String getInactiveTime();

	void setClientType(String clientType);

	String getClientType();

	void setCtreeTaskId(int ctreeTaskId);

	int getCtreeTaskId();

	void setSid(String sid);

	String getSid();

	void setMemUsed(int memUsed);

	int getMemUsed();

	void setRemark(String remark);

	String getRemark();

	void setInstrCountPerSec(int instrCountPerSec);

	int getInstrCountPerSec();

	void setTotalInstrCount(int totalInstrCount);

	int getTotalInstrCount();

	void setElapsedTime(String elapsedTime);

	String getElapsedTime();

	void setLoginTime(String loginTime);

	String getLoginTime();

	void setEnvironment(String environment);

	String getEnvironment();

	void setMainName(String mainName);

	String getMainName();

	void setServer(String server);

	String getServer();

	void setThreadId(long threadId);

	long getThreadId();

	void setComputerName(String computerName);

	String getComputerName();

	void setUsername(String username);

	String getUsername();

	IServerMonitor getParent();

}
