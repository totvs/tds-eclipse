package br.com.totvs.tds.lsp.server.model.node;

public class UsersInfoDataNode {
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

	/**
	 * @return the username
	 */
	public String getUsername() {
		return username;
	}

	/**
	 * @param username the username to set
	 */
	public void setUsername(final String username) {
		this.username = username;
	}

	/**
	 * @return the computerName
	 */
	public String getComputerName() {
		return computerName;
	}

	/**
	 * @param computerName the computerName to set
	 */
	public void setComputerName(final String computerName) {
		this.computerName = computerName;
	}

	/**
	 * @return the threadId
	 */
	public long getThreadId() {
		return threadId;
	}

	/**
	 * @param threadId the threadId to set
	 */
	public void setThreadId(final long threadId) {
		this.threadId = threadId;
	}

	/**
	 * @return the server
	 */
	public String getServer() {
		return server;
	}

	/**
	 * @param server the server to set
	 */
	public void setServer(final String server) {
		this.server = server;
	}

	/**
	 * @return the mainName
	 */
	public String getMainName() {
		return mainName;
	}

	/**
	 * @param mainName the mainName to set
	 */
	public void setMainName(final String mainName) {
		this.mainName = mainName;
	}

	/**
	 * @return the environment
	 */
	public String getEnvironment() {
		return environment;
	}

	/**
	 * @param environment the environment to set
	 */
	public void setEnvironment(final String environment) {
		this.environment = environment;
	}

	/**
	 * @return the loginTime
	 */
	public String getLoginTime() {
		return loginTime;
	}

	/**
	 * @param loginTime the loginTime to set
	 */
	public void setLoginTime(final String loginTime) {
		this.loginTime = loginTime;
	}

	/**
	 * @return the elapsedTime
	 */
	public String getElapsedTime() {
		return elapsedTime;
	}

	/**
	 * @param elapsedTime the elapsedTime to set
	 */
	public void setElapsedTime(final String elapsedTime) {
		this.elapsedTime = elapsedTime;
	}

	/**
	 * @return the totalInstrCount
	 */
	public int getTotalInstrCount() {
		return totalInstrCount;
	}

	/**
	 * @param totalInstrCount the totalInstrCount to set
	 */
	public void setTotalInstrCount(final int totalInstrCount) {
		this.totalInstrCount = totalInstrCount;
	}

	/**
	 * @return the instrCountPerSec
	 */
	public int getInstrCountPerSec() {
		return instrCountPerSec;
	}

	/**
	 * @param instrCountPerSec the instrCountPerSec to set
	 */
	public void setInstrCountPerSec(final int instrCountPerSec) {
		this.instrCountPerSec = instrCountPerSec;
	}

	/**
	 * @return the remark
	 */
	public String getRemark() {
		return remark;
	}

	/**
	 * @param remark the remark to set
	 */
	public void setRemark(final String remark) {
		this.remark = remark;
	}

	/**
	 * @return the memUsed
	 */
	public int getMemUsed() {
		return memUsed;
	}

	/**
	 * @param memUsed the memUsed to set
	 */
	public void setMemUsed(final int memUsed) {
		this.memUsed = memUsed;
	}

	/**
	 * @return the sid
	 */
	public String getSid() {
		return sid;
	}

	/**
	 * @param sid the sid to set
	 */
	public void setSid(final String sid) {
		this.sid = sid;
	}

	/**
	 * @return the ctreeTaskId
	 */
	public int getCtreeTaskId() {
		return ctreeTaskId;
	}

	/**
	 * @param ctreeTaskId the ctreeTaskId to set
	 */
	public void setCtreeTaskId(final int ctreeTaskId) {
		this.ctreeTaskId = ctreeTaskId;
	}

	/**
	 * @return the clientType
	 */
	public String getClientType() {
		return clientType;
	}

	/**
	 * @param clientType the clientType to set
	 */
	public void setClientType(final String clientType) {
		this.clientType = clientType;
	}

	/**
	 * @return the inactiveTime
	 */
	public String getInactiveTime() {
		return inactiveTime;
	}

	/**
	 * @param inactiveTime the inactiveTime to set
	 */
	public void setInactiveTime(final String inactiveTime) {
		this.inactiveTime = inactiveTime;
	}

}
