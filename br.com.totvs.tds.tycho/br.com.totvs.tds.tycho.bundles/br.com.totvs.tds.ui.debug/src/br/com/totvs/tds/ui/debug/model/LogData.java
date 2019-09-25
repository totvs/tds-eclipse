package br.com.totvs.tds.ui.debug.model;

public class LogData {
	private String level;
	private String message;
	private boolean notify;
	private String time;
	/**
	 * @return the level
	 */
	public String getLevel() {
		return level;
	}
	/**
	 * @param level the level to set
	 */
	public void setLevel(String level) {
		this.level = level;
	}
	/**
	 * @return the message
	 */
	public String getMessage() {
		return message;
	}
	/**
	 * @param message the message to set
	 */
	public void setMessage(String message) {
		this.message = message;
	}
	/**
	 * @return the notify
	 */
	public boolean isNotify() {
		return notify;
	}
	/**
	 * @param notify the notify to set
	 */
	public void setNotify(boolean notify) {
		this.notify = notify;
	}
	/**
	 * @return the time
	 */
	public String getTime() {
		return time;
	}
	/**
	 * @param time the time to set
	 */
	public void setTime(String time) {
		this.time = time;
	}
}
