package br.com.totvs.tds.lsp.server.model.protocol;

public class SendMessageToUserData {

	private SendMessageToUserInfo sendUserMessageInfo;

	public SendMessageToUserData(final SendMessageToUserInfo sendMessageToUser) {
		this.sendUserMessageInfo = sendMessageToUser;
	}

	/**
	 * @return the sendMessageToUser
	 */
	public SendMessageToUserInfo getSendMessageToUser() {
		return sendUserMessageInfo;
	}

	/**
	 * @param sendMessageToUser the sendMessageToUser to set
	 */
	public void setSendMessageToUser(final SendMessageToUserInfo sendMessageToUser) {
		this.sendUserMessageInfo = sendMessageToUser;
	}

}
