/**
 *
 */
package br.com.totvs.tds.ui.monitor.model;

/**
 * @author acandido
 *
 */
public class UserMonitor implements IUserMonitor {

	private String username;
	private String computerName;
	private long threadId; // unsigned
	private String server;
	private String mainName;
	private String environment;
	private String loginTime;
	private String elapsedTime;
	private int totalInstrCount;
	private int instrCountPerSec;
	private String remark;
	//
	private int memUsed;
	private String sid; // sid
	private int ctreeTaskId; // ctree task sid
	private String clientType;
	private String inactiveTime;
	private ServerMonitor parent;

	public UserMonitor(final ServerMonitor parent) {
		this.parent = parent;
	}

	@Override
	public IServerMonitor getParent() {

		return parent;
	}

	/**
	 * @return the username
	 */
	@Override
	public String getUsername() {
		return username;
	}

	/**
	 * @param username the username to set
	 */
	@Override
	public void setUsername(final String username) {
		this.username = username;
	}

	/**
	 * @return the computerName
	 */
	@Override
	public String getComputerName() {
		return computerName;
	}

	/**
	 * @param computerName the computerName to set
	 */
	@Override
	public void setComputerName(final String computerName) {
		this.computerName = computerName;
	}

	/**
	 * @return the threadId
	 */
	@Override
	public long getThreadId() {
		return threadId;
	}

	/**
	 * @param threadId the threadId to set
	 */
	@Override
	public void setThreadId(final long threadId) {
		this.threadId = threadId;
	}

	/**
	 * @return the server
	 */
	@Override
	public String getServer() {
		return server;
	}

	/**
	 * @param server the server to set
	 */
	@Override
	public void setServer(final String server) {
		this.server = server;
	}

	/**
	 * @return the mainName
	 */
	@Override
	public String getMainName() {
		return mainName;
	}

	/**
	 * @param mainName the mainName to set
	 */
	@Override
	public void setMainName(final String mainName) {
		this.mainName = mainName;
	}

	/**
	 * @return the environment
	 */
	@Override
	public String getEnvironment() {
		return environment;
	}

	/**
	 * @param environment the environment to set
	 */
	@Override
	public void setEnvironment(final String environment) {
		this.environment = environment;
	}

	/**
	 * @return the loginTime
	 */
	@Override
	public String getLoginTime() {
		return loginTime;
	}

	/**
	 * @param loginTime the loginTime to set
	 */
	@Override
	public void setLoginTime(final String loginTime) {
		this.loginTime = loginTime;
	}

	/**
	 * @return the elapsedTime
	 */
	@Override
	public String getElapsedTime() {
		return elapsedTime;
	}

	/**
	 * @param elapsedTime the elapsedTime to set
	 */
	@Override
	public void setElapsedTime(final String elapsedTime) {
		this.elapsedTime = elapsedTime;
	}

	/**
	 * @return the totalInstrCount
	 */
	@Override
	public int getTotalInstrCount() {
		return totalInstrCount;
	}

	/**
	 * @param totalInstrCount the totalInstrCount to set
	 */
	@Override
	public void setTotalInstrCount(final int totalInstrCount) {
		this.totalInstrCount = totalInstrCount;
	}

	/**
	 * @return the instrCountPerSec
	 */
	@Override
	public int getInstrCountPerSec() {
		return instrCountPerSec;
	}

	/**
	 * @param instrCountPerSec the instrCountPerSec to set
	 */
	@Override
	public void setInstrCountPerSec(final int instrCountPerSec) {
		this.instrCountPerSec = instrCountPerSec;
	}

	/**
	 * @return the remark
	 */
	@Override
	public String getRemark() {
		return remark;
	}

	/**
	 * @param remark the remark to set
	 */
	@Override
	public void setRemark(final String remark) {
		this.remark = remark;
	}

	/**
	 * @return the memUsed
	 */
	@Override
	public int getMemUsed() {
		return memUsed;
	}

	/**
	 * @param memUsed the memUsed to set
	 */
	@Override
	public void setMemUsed(final int memUsed) {
		this.memUsed = memUsed;
	}

	/**
	 * @return the sid
	 */
	@Override
	public String getSid() {
		return sid;
	}

	/**
	 * @param sid the sid to set
	 */
	@Override
	public void setSid(final String sid) {
		this.sid = sid;
	}

	/**
	 * @return the ctreeTaskId
	 */
	@Override
	public int getCtreeTaskId() {
		return ctreeTaskId;
	}

	/**
	 * @param ctreeTaskId the ctreeTaskId to set
	 */
	@Override
	public void setCtreeTaskId(final int ctreeTaskId) {
		this.ctreeTaskId = ctreeTaskId;
	}

	/**
	 * @return the clientType
	 */
	@Override
	public String getClientType() {
		return clientType;
	}

	/**
	 * @param clientType the clientType to set
	 */
	@Override
	public void setClientType(final String clientType) {
		this.clientType = clientType;
	}

	/**
	 * @return the inactiveTime
	 */
	@Override
	public String getInactiveTime() {
		return inactiveTime;
	}

	/**
	 * @param inactiveTime the inactiveTime to set
	 */
	@Override
	public void setInactiveTime(final String inactiveTime) {
		this.inactiveTime = inactiveTime;
	}

}
